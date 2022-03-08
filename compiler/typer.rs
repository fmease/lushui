//! The type checker.

// @Beacon @Task introduce an Error variant to hir::{Expression, Declaration}
// to be able to continue type checking on errors

use crate::{
    component::Component,
    diagnostics::{reporter::ErrorReported, Code, Diagnostic},
    entity::EntityKind,
    error::{Health, Result, Stain},
    hir::{self, Declaration, Expression, Identifier},
    session::{BuildSession, IntrinsicType, KnownBinding},
    syntax::{ast::Explicitness, lowered_ast::AttributeName},
    utility::{pluralize, DisplayWith, QuoteExt},
};
use interpreter::{BindingRegistration, BindingRegistrationKind, FunctionScope, Interpreter};
use joinery::JoinableIterator;
use std::default::default;

pub mod interpreter;

pub fn check(
    declaration: &Declaration,
    component: &mut Component,
    session: &mut BuildSession,
) -> Result {
    let mut typer = Typer::new(component, session);

    typer
        .start_infer_types_in_declaration(declaration, Context::default())
        .stain(&mut typer.health);
    typer
        .infer_types_of_out_of_order_bindings()
        .stain(&mut typer.health);

    typer.health.into()
}

/// The state of the typer.
struct Typer<'a> {
    // @Task add recursion depth field
    component: &'a mut Component,
    session: &'a mut BuildSession,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: ComponentIndex, expression: TypeAnnotation|Value|Both|... }
    out_of_order_bindings: Vec<BindingRegistration>,
    health: Health,
}

impl<'a> Typer<'a> {
    fn new(component: &'a mut Component, session: &'a mut BuildSession) -> Self {
        Self {
            component,
            session,
            out_of_order_bindings: Vec::new(),
            health: Health::Untainted,
        }
    }

    fn interpreter(&self) -> Interpreter<'_> {
        Interpreter::new(self.component, self.session)
    }

    // @Task documentation
    fn start_infer_types_in_declaration(
        &mut self,
        declaration: &Declaration,
        mut context: Context,
    ) -> Result {
        use hir::DeclarationKind::*;

        match &declaration.value {
            Function(function) => {
                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: if declaration.attributes.contains(AttributeName::Intrinsic) {
                        BindingRegistrationKind::IntrinsicFunction {
                            binder: function.binder.clone(),
                            type_: function.type_annotation.clone(),
                        }
                    } else {
                        BindingRegistrationKind::Function {
                            binder: function.binder.clone(),
                            type_: function.type_annotation.clone(),
                            value: Some(function.expression.clone().unwrap()),
                        }
                    },
                })?;
            }
            Data(type_) => {
                // @Question don't return early??
                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: BindingRegistrationKind::Data {
                        binder: type_.binder.clone(),
                        type_: type_.type_annotation.clone(),
                    },
                })?;

                if let Some(constructors) = &type_.constructors {
                    let health = &mut Health::Untainted;

                    for constructor in constructors {
                        self.start_infer_types_in_declaration(
                            constructor,
                            Context {
                                owning_data_type: Some(type_.binder.clone()),
                            },
                        )
                        .stain(health);
                    }

                    return Result::from(*health);
                }
            }
            Constructor(constructor) => {
                let owner_data_type = context.owning_data_type.take().unwrap();

                self.evaluate_registration(BindingRegistration {
                    attributes: declaration.attributes.clone(),
                    kind: BindingRegistrationKind::Constructor {
                        binder: constructor.binder.clone(),
                        type_: constructor.type_annotation.clone(),
                        owner_data_type,
                    },
                })?;
            }
            Module(module) => {
                let health = &mut Health::Untainted;

                for declaration in &module.declarations {
                    self.start_infer_types_in_declaration(declaration, Context::default())
                        .stain(health);
                }

                return Result::from(*health);
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
            Function {
                binder,
                type_,
                value,
            } => {
                let value = value.unwrap();

                if let Err(error) = self.it_is_a_type(type_.clone(), &FunctionScope::Module) {
                    return self.handle_registration_error(
                        error,
                        &type_,
                        None,
                        registration,
                        |_| Ok(()),
                    );
                };

                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                let inferred_type =
                    match self.infer_type_of_expression(value.clone(), &FunctionScope::Module) {
                        Ok(type_) => type_,
                        Err(error) => {
                            let attributes = registration.attributes.clone();

                            return self.handle_registration_error(
                                error,
                                &value,
                                // @Question is this correct?
                                None,
                                registration,
                                |typer| {
                                    typer.carry_out_registration(BindingRegistration {
                                        attributes,
                                        kind: Function {
                                            binder,
                                            type_,
                                            value: None,
                                        },
                                    })
                                },
                            );
                        }
                    };

                if let Err(error) =
                    self.it_is_actual(type_.clone(), inferred_type.clone(), &FunctionScope::Module)
                {
                    return self.handle_registration_error(
                        error,
                        &value,
                        // @Question is this correct?
                        Some(&type_),
                        registration,
                        |_| Ok(()),
                    );
                };

                self.carry_out_registration(BindingRegistration {
                    attributes: registration.attributes,
                    kind: Function {
                        binder,
                        type_: inferred_type,
                        value: Some(value),
                    },
                })?;
            }
            Data { binder, type_ } => {
                if let Err(error) = self.it_is_a_type(type_.clone(), &FunctionScope::Module) {
                    return self.handle_registration_error(
                        error,
                        &type_,
                        None,
                        registration,
                        |_| Ok(()),
                    );
                };

                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    Expression::new(default(), default(), hir::ExpressionKind::Type),
                )?;

                self.carry_out_registration(BindingRegistration {
                    attributes: registration.attributes,
                    kind: Data { binder, type_ },
                })?;
            }
            Constructor {
                binder,
                type_,
                owner_data_type: data,
            } => {
                if let Err(error) = self.it_is_a_type(type_.clone(), &FunctionScope::Module) {
                    return self.handle_registration_error(
                        error,
                        &type_,
                        None,
                        registration,
                        |_| Ok(()),
                    );
                };

                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    data.clone().into_expression(),
                )?;

                self.carry_out_registration(BindingRegistration {
                    attributes: registration.attributes,
                    kind: Constructor {
                        binder,
                        type_,
                        owner_data_type: data,
                    },
                })?;
            }
            IntrinsicFunction { binder, type_ } => {
                if let Err(error) = self.it_is_a_type(type_.clone(), &FunctionScope::Module) {
                    return self.handle_registration_error(
                        error,
                        &type_,
                        None,
                        registration,
                        |_| Ok(()),
                    );
                };

                let type_ = self.interpreter().evaluate_expression(
                    type_,
                    interpreter::Context::new(&FunctionScope::Module),
                )?;

                self.carry_out_registration(BindingRegistration {
                    attributes: registration.attributes,
                    kind: IntrinsicFunction { binder, type_ },
                })?;
            }
        }

        Ok(())
    }

    // @Bug does not understand non-local binders
    pub(crate) fn carry_out_registration(&mut self, registration: BindingRegistration) -> Result {
        use BindingRegistrationKind::*;

        match registration.kind {
            Function {
                binder,
                type_,
                value,
            } => {
                // @Bug may be non-local thus panic
                let index = binder.local_declaration_index(self.component).unwrap();
                let entity = &mut self.component[index];
                // @Question can't we just remove the bodiless check as intrinsic functions
                // (the only legal bodiless functions) are already handled separately via
                // IntrinsicFunction?
                debug_assert!(entity.is_untyped() || entity.is_bodiless_function());

                entity.kind = EntityKind::Function {
                    type_,
                    expression: value,
                };
            }
            Data { binder, type_ } => {
                // @Bug may be non-local thus panic
                let index = binder.local_declaration_index(self.component).unwrap();
                let entity = &mut self.component[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::DataType {
                    namespace: std::mem::take(entity.namespace_mut().unwrap()),
                    type_,
                    constructors: Vec::new(),
                };
            }
            Constructor {
                binder,
                type_,
                owner_data_type: data,
            } => {
                // @Bug may be non-local thus panic
                let index = binder.local_declaration_index(self.component).unwrap();
                let entity = &mut self.component[index];
                debug_assert!(entity.is_untyped());

                entity.kind = EntityKind::Constructor { type_ };

                // @Bug may be non-local thus panic
                let data_index = data.local_declaration_index(self.component).unwrap();

                match &mut self.component[data_index].kind {
                    EntityKind::DataType { constructors, .. } => constructors.push(binder),
                    _ => unreachable!(),
                }
            }
            IntrinsicFunction { binder, type_ } => {
                // @Bug may be non-local thus panic
                let index = binder.local_declaration_index(self.component).unwrap();
                debug_assert!(self.component[index].is_untyped());

                self.component[index].kind = self.session.register_intrinsic_function(
                    binder,
                    type_,
                    registration
                        .attributes
                        .span(AttributeName::Intrinsic)
                        .unwrap(),
                )?;
            }
        }
        Ok(())
    }

    // @Task heavily improve API / architecture
    fn handle_registration_error(
        &mut self,
        error: Error,
        actual_value: &Expression,
        expectation_cause: Option<&Expression>,
        registration: BindingRegistration,
        out_of_order_handler: impl FnOnce(&mut Self) -> Result,
    ) -> Result {
        match error {
            Unrecoverable(token) => Err(token),
            OutOfOrderBinding => {
                self.out_of_order_bindings.push(registration);
                out_of_order_handler(self)
            }
            TypeMismatch { expected, actual } => Err(Diagnostic::error()
                .code(Code::E032)
                // @Task put back some more information into the message: use `_`s to shorten the type
                .message("type mismatch")
                .labeled_primary_span(&actual_value, "has the wrong type")
                .if_present(expectation_cause, |this, cause| {
                    this.labeled_secondary_span(cause, "expected due to this")
                })
                .note(format!(
                    "\
expected type `{}`
 but got type `{}`",
                    expected.with((self.component, self.session)),
                    actual.with((self.component, self.session))
                ))
                .report(self.session.reporter())),
        }
    }

    fn infer_types_of_out_of_order_bindings(&mut self) -> Result {
        while !self.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                self.evaluate_registration(binding)?;
            }

            if previous_amount == self.out_of_order_bindings.len() {
                if self.health.is_tainted() {
                    return Ok(());
                }

                return Err(Diagnostic::bug()
                    .message(format!(
                        "found unresolvable {} during type checking",
                        pluralize!(previous_amount, "binding")
                    ))
                    .note(format!(
                        "namely {}",
                        self.out_of_order_bindings
                            .iter()
                            .map(|binding| binding.with((self.component, self.session)).quote())
                            .join_with(", ")
                    ))
                    .report(self.session.reporter()));
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
        // changing the definition of FunctionScope... it's FunctionScope::Component now, no payload
        // this means more boilerplate methods (either duplication or as in resolver: every FunctionScope method takes
        // a component: &Component parameter)
        &self,
        expression: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<Expression, Error> {
        use hir::ExpressionKind::*;
        use interpreter::Substitution::*;

        Ok(match expression.value {
            Binding(binding) => self
                .interpreter()
                .look_up_type(&binding.0, scope)
                .ok_or(OutOfOrderBinding)?,
            Type => Expression::new(default(), default(), hir::ExpressionKind::Type),
            Number(number) => self
                .session
                .look_up_intrinsic_type(number.type_().into(), Some(expression.span))?,
            Text(_) => self
                .session
                .look_up_intrinsic_type(IntrinsicType::Text, Some(expression.span))?,
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

                Expression::new(default(), default(), hir::ExpressionKind::Type)
            }
            Lambda(lambda) => {
                let parameter_type: Expression =
                    lambda.parameter_type_annotation.clone().ok_or_else(|| {
                        Diagnostic::missing_annotation().report(self.session.reporter())
                    })?;

                self.it_is_a_type(parameter_type.clone(), scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let inferred_body_type =
                    self.infer_type_of_expression(lambda.body.clone(), &scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    self.it_is_a_type(body_type_annotation.clone(), &scope)?;
                    self.it_is_actual(body_type_annotation, inferred_body_type.clone(), &scope)?;
                }

                Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::PiType {
                        explicitness: Explicitness::Explicit,
                        laziness: lambda.laziness,
                        parameter: Some(lambda.parameter.clone()),
                        domain: parameter_type,
                        codomain: inferred_body_type,
                    }
                    .into(),
                )
            }
            Application(application) => {
                // @Note this is an example where we normalize after an infer_type_of_expression which means infer_type_of_expression
                // returns possibly non-normalized expressions, can we do better?
                let type_of_callee =
                    self.infer_type_of_expression(application.callee.clone(), scope)?;
                let type_of_callee = self
                    .interpreter()
                    .evaluate_expression(type_of_callee, interpreter::Context::new(scope))?;

                if let PiType(pi) = &type_of_callee.value {
                    let argument_type =
                        self.infer_type_of_expression(application.argument.clone(), scope)?;

                    let argument_type = if pi.laziness.is_some() {
                        Expression::new(
                            default(),
                            argument_type.span,
                            hir::PiType {
                                explicitness: Explicitness::Explicit,
                                laziness: None,
                                parameter: None,
                                domain: self.session.look_up_known_type(
                                    KnownBinding::Unit,
                                    application.callee.span,
                                )?,
                                codomain: argument_type,
                            }
                            .into(),
                        )
                    } else {
                        argument_type
                    };

                    self.it_is_actual(pi.domain.clone(), argument_type, scope)
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        .map_err(|error| match error {
                            Unrecoverable(token) => token,
                            TypeMismatch { expected, actual } => Diagnostic::error()
                                .code(Code::E032)
                                // @Task put back some more information into the message: use `_`s to shorten the type
                                .message("type mismatch")
                                .labeled_primary_span(&application.argument, "has the wrong type")
                                .labeled_secondary_span(&expected, "expected due to this")
                                .note(format!(
                                    "\
expected type `{}`
 but got type `{}`",
                                    expected.with((self.component, self.session)),
                                    actual.with((self.component, self.session))
                                ))
                                .report(self.session.reporter()),
                            OutOfOrderBinding => unreachable!(),
                        })?;

                    match pi.parameter.clone() {
                        Some(_) => Expression::new(
                            default(),
                            default(),
                            hir::Substitution {
                                substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                                expression: pi.codomain.clone(),
                            }
                            .into(),
                        ),
                        None => pi.codomain.clone(),
                    }
                } else {
                    return Err(Diagnostic::error()
                        .code(Code::E031)
                        // @Task put back some more information into the message: use `_`s to shorten the type
                        .message("type mismatch")
                        .labeled_primary_span(&application.callee, "has wrong type")
                        .labeled_secondary_span(&application.argument, "applied to this")
                        .note(format!(
                            "\
expected type `_ -> _`
 but got type `{}`",
                            type_of_callee.with((self.component, self.session))
                        ))
                        .report(self.session.reporter())
                        .into());
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
                    self.infer_type_of_expression(analysis.scrutinee.clone(), scope)?;
                // to get rid of Substitutions
                let subject_type = self
                    .interpreter()
                    .evaluate_expression(subject_type, interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. type_ is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all analysis.cases>>.expressions are of the same type

                match &subject_type.value {
                    Binding(_) => {}
                    Application(_application) => todo!("polymorphic types in patterns"),
                    _ if self.is_a_type(subject_type.clone(), scope)? => {
                        return Err(Diagnostic::error()
                            .code(Code::E035)
                            .message("attempt to analyze a type")
                            .primary_span(expression.span)
                            .note("forbidden to uphold parametricity and type erasure")
                            .report(self.session.reporter())
                            .into());
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        subject_type.with((self.component, self.session))
                    ),
                };

                let mut type_of_previous_body = None::<Expression>;

                for case in &analysis.cases {
                    use hir::PatternKind::*;

                    let mut binder_types = Vec::new();

                    // @Question make more elegant w/ the new error handling system?
                    let handle_type_mismatch = |error, context, reporter| match error {
                        TypeMismatch { expected, actual } => Diagnostic::error()
                            .code(Code::E032)
                            // @Task put back some more information into the message: use `_`s to shorten the type
                            .message("type mismatch")
                            .labeled_primary_span(&case.pattern, "has the wrong type")
                            .labeled_secondary_span(&analysis.scrutinee, "expected due to this")
                            .note(format!(
                                "\
expected type `{}`
 but got type `{}`",
                                expected.with(context),
                                actual.with(context)
                            ))
                            .report(reporter)
                            .into(),
                        error => error,
                    };

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pattern.value {
                        Number(number) => {
                            let number_type = self.session.look_up_intrinsic_type(
                                number.type_().into(),
                                Some(case.pattern.span),
                            )?;
                            self.it_is_actual(subject_type.clone(), number_type, scope)
                                .map_err(|error| {
                                    handle_type_mismatch(
                                        error,
                                        (self.component, self.session),
                                        self.session.reporter(),
                                    )
                                })?;
                        }
                        Text(_) => {
                            let text_type = self.session.look_up_intrinsic_type(
                                IntrinsicType::Text,
                                Some(case.pattern.span),
                            )?;
                            self.it_is_actual(subject_type.clone(), text_type, scope)
                                .map_err(|error| {
                                    handle_type_mismatch(
                                        error,
                                        (self.component, self.session),
                                        self.session.reporter(),
                                    )
                                })?;
                        }
                        Binding(binding) => {
                            let constructor_type =
                                self.interpreter().look_up_type(&binding.0, scope).unwrap();

                            self.it_is_actual(
                                subject_type.clone(),
                                constructor_type.clone(),
                                scope,
                            )
                            .map_err(|error| {
                                handle_type_mismatch(
                                    error,
                                    (self.component, self.session),
                                    self.session.reporter(),
                                )
                            })?;
                        }
                        Binder(_) => {
                            // @Temporary @Beacon @Bug error prone (once we try to impl deappl)
                            // @Update @Note don't push the type of subject but the type of the binder
                            binder_types.push(subject_type.clone());
                        }
                        // @Task
                        Application(application) => {
                            // @Beacon @Task check that subject type is a pi type

                            match (&application.callee.value, &application.argument.value) {
                                // @Note should be an error obviously but does this need to be special-cased
                                // or can we defer this to an it_is_actual call??
                                (Number(_) | Text(_), _argument) => todo!(),
                                (Binding(binding), _argument) => {
                                    let _constructor_type =
                                        self.interpreter().look_up_type(&binding.0, scope).unwrap();

                                    todo!();
                                }
                                // @Task make error less fatal (keep processing next cases (match arms))
                                (Binder(binder), _) => {
                                    return Err(Diagnostic::error()
                                        .code(Code::E034)
                                        .message(format!(
                                            "binder `{}` used in callee position inside pattern",
                                            binder.0
                                        ))
                                        .primary_span(&binder.0)
                                        .help("consider refering to a concrete binding")
                                        .report(self.session.reporter())
                                        .into());
                                }
                                (Application(_), _argument) => todo!(),
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
            IO(_) => self
                .session
                .look_up_intrinsic_type(IntrinsicType::IO, Some(expression.span))?,
            Error => expression,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn it_is_a_type(&self, expression: Expression, scope: &FunctionScope<'_>) -> Result<(), Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.it_is_actual(
            Expression::new(default(), default(), hir::ExpressionKind::Type),
            type_,
            scope,
        )
    }

    fn is_a_type(&self, expression: Expression, scope: &FunctionScope<'_>) -> Result<bool, Error> {
        let type_ = self.infer_type_of_expression(expression, scope)?;
        self.is_actual(
            Expression::new(default(), default(), hir::ExpressionKind::Type),
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
        &self,
        expected: Expression,
        actual: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<(), Error> {
        let context = interpreter::Context::new(scope);
        let expected = self.interpreter().evaluate_expression(expected, context)?;
        let actual = self.interpreter().evaluate_expression(actual, context)?;

        if !self.interpreter().equals(&expected, &actual, scope)? {
            return Err(TypeMismatch { expected, actual });
        }

        Ok(())
    }

    fn is_actual(
        &self,
        expected: Expression,
        actual: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        let context = interpreter::Context::new(scope);
        let expected = self.interpreter().evaluate_expression(expected, context)?;
        let actual = self.interpreter().evaluate_expression(actual, context)?;

        self.interpreter().equals(&expected, &actual, scope)
    }

    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(&self, expression: Expression, scope: &FunctionScope<'_>) -> Expression {
        use hir::ExpressionKind::*;

        match expression.value {
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
        &self,
        constructor: Expression,
        type_: Expression,
    ) -> Result {
        let result_type = self.result_type(constructor, &FunctionScope::Module);
        let callee = result_type.clone().callee();

        if self
            .interpreter()
            .equals(&type_, &callee, &FunctionScope::Module)?
        {
            Ok(())
        } else {
            Err(Diagnostic::error()
                .code(Code::E033)
                .message(format!(
                    "`{}` is not an instance of `{}`",
                    result_type.with((self.component, self.session)),
                    type_.with((self.component, self.session))
                ))
                .primary_span(result_type.span)
                .report(self.session.reporter()))
        }
    }
}

impl Expression {
    /// The callee of an expression.
    ///
    /// # Example
    ///
    /// The `f` in `f a b c`.
    fn callee(mut self) -> Expression {
        loop {
            self = match self.value {
                hir::ExpressionKind::Application(application) => application.callee.clone(),
                _ => return self,
            }
        }
    }
}

#[derive(Default)]
struct Context {
    owning_data_type: Option<Identifier>,
}

impl Diagnostic {
    fn missing_annotation() -> Self {
        // @Task add span
        Self::error()
            .code(Code::E030)
            .message("currently lambda literal parameters and patterns must be type-annotated")
    }
}

// @Note maybe we should redesign this as a trait (object) looking at those
// methods mirroring the variants
pub(crate) enum Error {
    Unrecoverable(ErrorReported),
    OutOfOrderBinding,
    TypeMismatch {
        expected: Expression,
        actual: Expression,
    },
}

use Error::*;

impl From<ErrorReported> for Error {
    fn from(token: ErrorReported) -> Self {
        Self::Unrecoverable(token)
    }
}