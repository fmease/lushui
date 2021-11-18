//! The type checker.

pub mod interpreter;

use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::{Health, Result},
    format::{pluralize, DisplayWith, QuoteExt},
    hir::{self, expr, Declaration, Expression},
    package::{
        session::{IntrinsicType, KnownBinding},
        BuildSession,
    },
    resolver::{Crate, Identifier},
    span::Span,
    syntax::{
        ast::{Explicitness, ParameterAspect},
        lowered_ast::{AttributeKeys, Attributes},
    },
    typer::interpreter::scope::BindingRegistrationKind,
};
use interpreter::{
    scope::{BindingRegistration, FunctionScope},
    Form, Interpreter,
};
use joinery::JoinableIterator;
use std::default::default;

// @Beacon @Task introduce an Error variant to hir::{Expression, Declaration}
// to be able to continue type checking on errors

/// The state of the typer.
// @Task add recursion depth field
pub struct Typer<'a> {
    pub crate_: &'a mut Crate,
    pub session: &'a mut BuildSession,
    reporter: &'a Reporter,
    health: Health,
}

impl<'a> Typer<'a> {
    pub fn new(
        crate_: &'a mut Crate,
        session: &'a mut BuildSession,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            crate_,
            session,
            reporter,
            health: Health::Untainted,
        }
    }

    pub fn interpreter(&mut self) -> Interpreter<'_> {
        Interpreter::new(self.crate_, self.session, self.reporter)
    }

    pub fn infer_types_in_declaration(&mut self, declaration: &Declaration) -> Result {
        let first_pass = self.start_infer_types_in_declaration(declaration, Context::default());
        self.health &= first_pass;
        first_pass.and(self.infer_types_of_out_of_order_bindings())
    }

    // @Task documentation
    fn start_infer_types_in_declaration(
        &mut self,
        declaration: &Declaration,
        mut context: Context,
    ) -> Result {
        use hir::DeclarationKind::*;

        match &declaration.data {
            Value(value) => {
                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: if declaration.attributes.has(AttributeKeys::INTRINSIC) {
                        BindingRegistrationKind::IntrinsicFunction {
                            binder: value.binder.clone(),
                            type_: value.type_annotation.clone(),
                        }
                    } else {
                        BindingRegistrationKind::Value {
                            binder: value.binder.clone(),
                            type_: value.type_annotation.clone(),
                            value: Some(value.expression.clone().unwrap()),
                        }
                    },
                })?;
            }
            Data(data) => {
                // @Question don't return early??
                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: BindingRegistrationKind::Data {
                        binder: data.binder.clone(),
                        type_: data.type_annotation.clone(),
                    },
                })?;

                if declaration.attributes.has(AttributeKeys::INTRINSIC) {
                    self.evaluate_registration(BindingRegistration {
                        attributes: declaration.attributes.clone(),
                        kind: BindingRegistrationKind::IntrinsicType {
                            binder: data.binder.clone(),
                        },
                    })?;
                } else {
                    let constructors = data.constructors.as_ref().unwrap();

                    // @Task @Beacon move to resolver
                    if let Some(known) = declaration.attributes.filter(AttributeKeys::KNOWN).next()
                    {
                        self.session.register_known_type(
                            &data.binder,
                            constructors
                                .iter()
                                .map(|declaration| declaration.constructor().unwrap())
                                .collect(),
                            known,
                            self.reporter,
                        )?;
                    }

                    return constructors
                        .iter()
                        .fold(Health::Untainted, |health, constructor| {
                            health
                                & self.start_infer_types_in_declaration(
                                    constructor,
                                    Context {
                                        parent_data_binding: Some(data.binder.clone()),
                                    },
                                )
                        })
                        .into();
                }
            }
            Constructor(constructor) => {
                let data = context.parent_data_binding.take().unwrap();

                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: BindingRegistrationKind::Constructor {
                        binder: constructor.binder.clone(),
                        type_: constructor.type_annotation.clone(),
                        data,
                    },
                })?;

                // @Beacon @Task register field projections
            }
            Module(module) => {
                return module
                    .declarations
                    .iter()
                    .fold(Health::Untainted, |health, declaration| {
                        health
                            & self.start_infer_types_in_declaration(declaration, Context::default())
                    })
                    .into();
            }
            Use(_) | Error => {}
        }

        Ok(())
    }

    // @Note very strange API going on here
    // @Note we might want to store the evaluated types into the scopes instead of the
    // unevaluated ones. This dependends on how we'd like to normalize (WeakHead|Normal)
    // @Task @Beacon @Beacon somehow (*somehow*!) restructure this code so it is not DRY.
    // it is DRY even though we use an ugly macro..how sad is that??
    // we need to design the error handling here, it's super difficult, fragile, …
    fn evaluate_registration(&mut self, registration: BindingRegistration) -> Result {
        use BindingRegistrationKind::*;

        match registration.clone().kind {
            Value {
                binder,
                type_,
                value,
            } => {
                let value = value.unwrap();

                recover_error!(
                    self.crate_,
                    self.session,
                    self.reporter,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::Crate),
                    actual = type_
                );
                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context {
                        scope: &FunctionScope::Crate,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                let infered_type =
                    match self.infer_type_of_expression(value.clone(), &FunctionScope::Crate) {
                        Ok(expression) => expression,
                        Err(error) => {
                            return match error {
                                Unrecoverable => Err(()),
                                OutOfOrderBinding => {
                                    self.crate_.out_of_order_bindings.push(registration.clone());
                                    self.crate_.carry_out(
                                        BindingRegistration {
                                            attributes: registration.attributes,
                                            kind: Value {
                                                binder,
                                                type_,
                                                value: None,
                                            },
                                        },
                                        self.session,
                                        self.reporter,
                                    )
                                }
                                // @Task abstract over explicit diagnostic building
                                TypeMismatch { expected, actual } => {
                                    Diagnostic::error()
                                        .code(Code::E032)
                                        .message(format!(
                                            "expected type `{}`, got type `{}`",
                                            expected.with((self.crate_, self.session)),
                                            actual.with((self.crate_, self.session))
                                        ))
                                        .labeled_primary_span(&actual, "has the wrong type")
                                        .labeled_secondary_span(&expected, "expected due to this")
                                        .report(self.reporter);
                                    Err(())
                                }
                            };
                        }
                    };

                recover_error!(
                    self.crate_,
                    self.session,
                    self.reporter,
                    registration;
                    self.it_is_actual(type_.clone(), infered_type.clone(), &FunctionScope::Crate),
                    actual = value,
                    expected = type_
                );
                self.crate_.carry_out(
                    BindingRegistration {
                        attributes: registration.attributes,
                        kind: Value {
                            binder,
                            type_: infered_type,
                            value: Some(value),
                        },
                    },
                    self.session,
                    self.reporter,
                )?;
            }
            Data { binder, type_ } => {
                recover_error!(
                    self.crate_,
                    self.session,
                    self.reporter,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::Crate),
                    actual = type_
                );
                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context {
                        scope: &FunctionScope::Crate,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    expr! { Type { Attributes::default(), Span::default() } },
                )?;

                self.crate_.carry_out(
                    BindingRegistration {
                        attributes: registration.attributes,
                        kind: Data { binder, type_ },
                    },
                    self.session,
                    self.reporter,
                )?;
            }
            Constructor {
                binder,
                type_,
                data,
            } => {
                recover_error!(
                    self.crate_,
                    self.session,
                    self.reporter,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::Crate),
                    actual = type_
                );
                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context {
                        scope: &FunctionScope::Crate,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    data.clone().to_expression(),
                )?;

                self.crate_.carry_out(
                    BindingRegistration {
                        attributes: registration.attributes,
                        kind: Constructor {
                            binder,
                            type_,
                            data,
                        },
                    },
                    self.session,
                    self.reporter,
                )?;
            }
            IntrinsicFunction { binder, type_ } => {
                recover_error!(
                    self.crate_,
                    self.session,
                    self.reporter,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::Crate),
                    actual = type_
                );
                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context {
                        scope: &FunctionScope::Crate,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                self.crate_.carry_out(
                    BindingRegistration {
                        attributes: registration.attributes,
                        kind: IntrinsicFunction { binder, type_ },
                    },
                    self.session,
                    self.reporter,
                )?;
            }
            IntrinsicType { binder } => {
                self.crate_.carry_out(
                    BindingRegistration {
                        attributes: registration.attributes,
                        kind: IntrinsicType { binder },
                    },
                    self.session,
                    self.reporter,
                )?;
            }
        }

        // @Task replace if possible
        macro recover_error(
            $scope:expr,
            $session:expr,
            $reporter:expr,
            $registration:expr;
            $check:expr,
            actual = $actual_value:expr
            $(, expected = $expected_reason:expr )?
        ) {
            match $check {
                Ok(expression) => expression,
                Err(error) => {
                    return match error {
                        Unrecoverable => Err(()),
                        OutOfOrderBinding => {
                            $scope.out_of_order_bindings.push($registration);
                            Ok(())
                        }
                        TypeMismatch { expected, actual } => {
                            Err(Diagnostic::error()
                                .code(Code::E032)
                                .message(format!(
                                    "expected type `{}`, got type `{}`",
                                    expected.with(($scope, $session)), actual.with(($scope, $session))
                                ))
                                .labeled_primary_span(
                                    &$actual_value,
                                    "has the wrong type",
                                )
                                $( .labeled_secondary_span(&$expected_reason, "expected due to this") )?
                                .report(&$reporter))
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn infer_types_of_out_of_order_bindings(&mut self) -> Result {
        while !self.crate_.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.crate_.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                self.evaluate_registration(binding)?;
            }

            if previous_amount == self.crate_.out_of_order_bindings.len() {
                if self.health.is_tainted() {
                    return Ok(());
                }

                Diagnostic::bug()
                    .message(format!(
                        "found unresolvable {} during type checking",
                        pluralize!(previous_amount, "binding")
                    ))
                    .note(format!(
                        "namely {}",
                        self.crate_
                            .out_of_order_bindings
                            .iter()
                            .map(|binding| binding.with((self.crate_, self.session)).quote())
                            .join_with(", ")
                    ))
                    .report(self.reporter);
                return Err(());
            }
        }

        Ok(())
    }

    /// Try to infer the type of an expression.
    // @Beacon @Beacon @Task verify and implement that all it_is_a_type and it_is_actual lead to good error messages
    // and keep it DRY (try to abstract over error handling, find a good API)
    // @Task make independent (^^) type errors non-fatal in respect to each other, i.e. return more than one
    // type error in possible cases
    fn infer_type_of_expression(
        // @Task change to &mut self (for warnings), this also means
        // changing the definition of FunctionScope... it's FunctionScope::Crate now, no payload
        // this means more boilerplate methods (either duplication or as in resolver: every FunctionScope method takes
        // a crate_: &Crate parameter)
        &mut self,
        expression: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<Expression, Error> {
        use hir::ExpressionKind::*;
        use interpreter::Substitution::*;

        Ok(match expression.data {
            Binding(binding) => self
                .interpreter()
                .look_up_type(&binding.binder, scope)
                .ok_or(OutOfOrderBinding)?,
            Type => expr! { Type { Attributes::default(), Span::default() } },
            Number(number) => self.session.look_up_intrinsic_type(
                IntrinsicType::numeric(&number),
                Some(expression.span),
                self.reporter,
            )?,
            Text(_) => self.session.look_up_intrinsic_type(
                IntrinsicType::Text,
                Some(expression.span),
                self.reporter,
            )?,
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type_of_expression`) has been normalized?
                self.it_is_a_type(literal.domain.clone(), scope)?;

                if literal.parameter.is_some() {
                    self.it_is_a_type(
                        literal.codomain.clone(),
                        &scope.extend_with_parameter(literal.domain.clone()),
                    )?;
                } else {
                    self.it_is_a_type(literal.codomain.clone(), scope)?;
                }

                expr! { Type { Attributes::default(), Span::default() } }
            }
            Lambda(lambda) => {
                let parameter_type: Expression = lambda
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(|| Diagnostic::missing_annotation().report(self.reporter))?;

                self.it_is_a_type(parameter_type.clone(), scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let infered_body_type =
                    self.infer_type_of_expression(lambda.body.clone(), &scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    self.it_is_a_type(body_type_annotation.clone(), &scope)?;
                    self.it_is_actual(body_type_annotation, infered_body_type.clone(), &scope)?;
                }

                let aspect = ParameterAspect {
                    laziness: lambda.laziness,
                    ..default()
                };

                expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        explicitness: Explicitness::Explicit,
                        // @Temporary
                        // aspect: ParameterAspect { laziness: lambda.laziness, ..default() },
                        aspect,
                        parameter: Some(lambda.parameter.clone()),
                        domain: parameter_type,
                        codomain: infered_body_type,
                    }
                }
            }
            Application(application) => {
                // @Note this is an example where we normalize after an infer_type_of_expression which means infer_type_of_expression
                // returns possibly non-normalized expressions, can we do better?
                let type_of_callee =
                    self.infer_type_of_expression(application.callee.clone(), scope)?;
                let type_of_callee = self.interpreter().evaluate_expression(
                    type_of_callee,
                    interpreter::Context {
                        scope,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                if let PiType(pi) = &type_of_callee.data {
                    let argument_type =
                        self.infer_type_of_expression(application.argument.clone(), scope)?;

                    let argument_type = if pi.aspect.laziness.is_some() {
                        expr! {
                            PiType {
                                Attributes::default(), argument_type.span;
                                explicitness: Explicitness::Explicit,
                                aspect: default(),
                                parameter: None,
                                domain: self.session.look_up_known_type(KnownBinding::Unit, application.callee.span, self.reporter)?,
                                codomain: argument_type,
                            }
                        }
                    } else {
                        argument_type
                    };

                    self.it_is_actual(pi.domain.clone(), argument_type, scope)
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        .map_err(|error| match error {
                            Unrecoverable => {}
                            TypeMismatch { expected, actual } => Diagnostic::error()
                                .code(Code::E032)
                                .message(format!(
                                    "expected type `{}`, got type `{}`",
                                    expected.with((self.crate_, self.session)),
                                    actual.with((self.crate_, self.session))
                                ))
                                .labeled_primary_span(&application.argument, "has the wrong type")
                                .labeled_secondary_span(&expected, "expected due to this")
                                .report(self.reporter),
                            OutOfOrderBinding => unreachable!(),
                        })?;

                    match pi.parameter.clone() {
                        Some(_) => expr! {
                            Substitution {
                                Attributes::default(), Span::default();
                                substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                                expression: pi.codomain.clone(),
                            }
                        },
                        None => pi.codomain.clone(),
                    }
                } else {
                    Diagnostic::error()
                        .code(Code::E031)
                        .message(format!(
                            "expected type `_ -> _`, got type `{}`",
                            type_of_callee.with((self.crate_, self.session))
                        ))
                        .labeled_primary_span(&application.callee, "has wrong type")
                        .labeled_secondary_span(&application.argument, "applied to this")
                        .report(self.reporter);
                    return Err(Unrecoverable);
                }
            }
            Substitution(substitution) => {
                let expression = self.interpreter().substitute_expression(
                    substitution.expression.clone(),
                    substitution.substitution.clone(),
                );
                self.infer_type_of_expression(expression, scope)?
            }
            UseIn => todo!("1stP infer type of use/in"),
            CaseAnalysis(analysis) => {
                let subject_type =
                    self.infer_type_of_expression(analysis.subject.clone(), scope)?;
                // to get rid of Substitutions
                let subject_type = self
                    .interpreter()
                    .evaluate_expression(subject_type, interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. type_ is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all analysis.cases>>.expressions are of the same type

                match &subject_type.data {
                    Binding(_) => {}
                    Application(_application) => todo!("polymorphic types in patterns"),
                    _ if self.is_a_type(subject_type.clone(), scope)? => {
                        Diagnostic::error()
                            .code(Code::E035)
                            .message("attempt to analyze a type")
                            .primary_span(expression.span)
                            .note("forbidden to uphold parametricity and type erasure")
                            .report(self.reporter);
                        return Err(Unrecoverable);
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        subject_type.with((self.crate_, self.session))
                    ),
                };

                let mut type_of_previous_body = None::<Expression>;

                for case in &analysis.cases {
                    use hir::PatternKind::*;

                    let mut binder_types = Vec::new();

                    // @Question make more elegant w/ the new error handling system?
                    let handle_type_mismatch = |error, context, reporter| match error {
                        TypeMismatch { expected, actual } => {
                            Diagnostic::error()
                                .code(Code::E032)
                                .message(format!(
                                    "expected type `{}`, got type `{}`",
                                    expected.with(context),
                                    actual.with(context)
                                ))
                                .labeled_primary_span(&case.pattern, "has the wrong type")
                                .labeled_secondary_span(&analysis.subject, "expected due to this")
                                .report(reporter);
                            Unrecoverable
                        }
                        error => error,
                    };

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pattern.data {
                        Number(number) => {
                            let number_type = self.session.look_up_intrinsic_type(
                                IntrinsicType::numeric(number),
                                Some(case.pattern.span),
                                self.reporter,
                            )?;
                            self.it_is_actual(subject_type.clone(), number_type, scope)
                                .map_err(|error| {
                                    handle_type_mismatch(
                                        error,
                                        (self.crate_, self.session),
                                        self.reporter,
                                    )
                                })?;
                        }
                        Text(_) => {
                            let text_type = self.session.look_up_intrinsic_type(
                                IntrinsicType::Text,
                                Some(case.pattern.span),
                                self.reporter,
                            )?;
                            self.it_is_actual(subject_type.clone(), text_type, scope)
                                .map_err(|error| {
                                    handle_type_mismatch(
                                        error,
                                        (self.crate_, self.session),
                                        self.reporter,
                                    )
                                })?;
                        }
                        Binding(binding) => {
                            let constructor_type = self
                                .interpreter()
                                .look_up_type(&binding.binder, scope)
                                .unwrap();

                            self.it_is_actual(
                                subject_type.clone(),
                                constructor_type.clone(),
                                scope,
                            )
                            .map_err(|error| {
                                handle_type_mismatch(
                                    error,
                                    (self.crate_, self.session),
                                    self.reporter,
                                )
                            })?;
                        }
                        Binder(_) => {
                            // @Temporary @Beacon @Bug error prone (once we try to impl deappl)
                            // @Update @Note don't push the type of subject but the type of the binder
                            binder_types.push(subject_type.clone());
                        }
                        // @Task
                        Deapplication(deapplication) => {
                            // @Beacon @Task check that subject type is a pi type

                            match (&deapplication.callee.data, &deapplication.argument.data) {
                                // @Note should be an error obviously but does this need to be special-cased
                                // or can we defer this to an it_is_actual call??
                                (Number(_) | Text(_), _argument) => todo!(),
                                (Binding(binding), _argument) => {
                                    let constructor_type = self
                                        .interpreter()
                                        .look_up_type(&binding.binder, scope)
                                        .unwrap();

                                    dbg!(
                                        &subject_type.with((self.crate_, self.session)),
                                        deapplication.callee.with((self.crate_, self.session)),
                                        &constructor_type.with((self.crate_, self.session))
                                    );

                                    todo!();
                                }
                                // @Task make error less fatal (keep processing next cases (match arms))
                                (Binder(binder), _) => {
                                    Diagnostic::error()
                                        .code(Code::E034)
                                        .message(format!(
                                            "binder `{}` used in callee position inside pattern",
                                            binder.binder
                                        ))
                                        .primary_span(&binder.binder)
                                        .help("consider refering to a concrete binding")
                                        .report(self.reporter);
                                    return Err(Unrecoverable);
                                }
                                (Deapplication(_), _argument) => todo!(),
                                (Error, _) => unreachable!(),
                            };
                        }
                        Error => unreachable!(),
                    }

                    let body_type = self.infer_type_of_expression(
                        case.body.clone(),
                        &scope.extend_with_pattern_binders(binder_types),
                    )?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            self.it_is_actual(previous_type.clone(), body_type, scope)?;
                        }
                        None => {
                            type_of_previous_body = Some(body_type);
                        }
                    }
                }

                //  @Temporary unhandled case
                type_of_previous_body.expect("caseless case analyses")
            }
            // @Beacon @Task
            IntrinsicApplication(_) | Projection(_) => todo!(),
            IO(_) => self.session.look_up_intrinsic_type(
                IntrinsicType::IO,
                Some(expression.span),
                self.reporter,
            )?,
            Error => expression,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn it_is_a_type(
        &mut self,
        expression: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<(), Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.it_is_actual(
            expr! { Type { Attributes::default(), Span::default() } },
            type_,
            scope,
        )
    }

    fn is_a_type(
        &mut self,
        expression: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool, Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.is_actual(
            expr! { Type { Attributes::default(), Span::default() } },
            type_,
            scope,
        )
        .map_err(Into::into)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn it_is_actual(
        &mut self,
        expected: Expression,
        actual: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<(), Error> {
        let expected = self.interpreter().evaluate_expression(
            expected,
            interpreter::Context {
                scope,
                form: Form::Normal, /* Form::WeakHeadNormal */
            },
        )?;
        let actual = self.interpreter().evaluate_expression(
            actual,
            interpreter::Context {
                scope,
                form: Form::Normal, /* Form::WeakHeadNormal */
            },
        )?;

        if !self
            .interpreter()
            .equals(expected.clone(), actual.clone(), scope)?
        {
            return Err(TypeMismatch { expected, actual });
        }

        Ok(())
    }

    fn is_actual(
        &mut self,
        expected: Expression,
        actual: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        let expected = self.interpreter().evaluate_expression(
            expected,
            interpreter::Context {
                scope,
                form: Form::Normal, /* Form::WeakHeadNormal */
            },
        )?;
        let actual = self.interpreter().evaluate_expression(
            actual,
            interpreter::Context {
                scope,
                form: Form::Normal, /* Form::WeakHeadNormal */
            },
        )?;

        self.interpreter().equals(expected, actual, scope)
    }

    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(&mut self, expression: Expression, scope: &FunctionScope<'_>) -> Expression {
        use hir::ExpressionKind::*;

        match expression.data {
            PiType(literal) => {
                if literal.parameter.is_some() {
                    let scope = scope.extend_with_parameter(literal.domain.clone());
                    self.result_type(literal.codomain.clone(), &scope)
                } else {
                    self.result_type(literal.codomain.clone(), scope)
                }
            }
            _ => expression,
        }
    }

    /// Instance checking.
    ///
    /// I.e. does a constructor of an algebraïc data type return a valid
    /// instance of the respective type?
    ///
    /// Note: Currently, the checker does allow existential type parameters
    /// and specialized instances. This will complicate the implementation
    /// of case analysis. Of course, feature-complete Lushui shall support
    /// existentials and specialized instances but we first might want to
    /// feature-gate them.
    fn assert_constructor_is_instance_of_type(
        &mut self,
        constructor: Expression,
        type_: Expression,
    ) -> Result {
        let result_type = self.result_type(constructor, &FunctionScope::Crate);
        let callee = result_type.clone().callee();

        if self
            .interpreter()
            .equals(type_.clone(), callee, &FunctionScope::Crate)?
        {
            Ok(())
        } else {
            Diagnostic::error()
                .code(Code::E033)
                .message(format!(
                    "`{}` is not an instance of `{}`",
                    result_type.with((self.crate_, self.session)),
                    type_.with((self.crate_, self.session))
                ))
                .primary_span(result_type.span)
                .report(self.reporter);
            Err(())
        }
    }
}

impl Expression {
    /// Returns the callee of an expression.
    ///
    /// Example: Returns the `f` in `f a b c`.
    fn callee(mut self) -> Expression {
        loop {
            self = match self.data {
                hir::ExpressionKind::Application(application) => application.callee.clone(),
                _ => return self,
            }
        }
    }
}

#[derive(Default)]
struct Context {
    parent_data_binding: Option<Identifier>,
}

impl Diagnostic {
    fn missing_annotation() -> Self {
        // @Task add span
        Self::bug()
            .code(Code::E030)
            .message("currently lambda literal parameters and patterns must be type-annotated")
    }
}

// @Note maybe we should redesign this as a trait (object) looking at those
// methods mirroring the variants
pub enum Error {
    Unrecoverable,
    OutOfOrderBinding,
    TypeMismatch {
        expected: Expression,
        actual: Expression,
    },
}

use Error::*;

impl From<()> for Error {
    fn from((): ()) -> Self {
        Self::Unrecoverable
    }
}
