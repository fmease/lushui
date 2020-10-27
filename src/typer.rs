//! The type checker.

pub mod interpreter;

use crate::{
    ast::Explicit,
    diagnostic::{Code, Diagnostic, Diagnostics, Result, Results},
    lowered_ast::{AttributeKeys, Attributes},
    resolver::{
        hir::{self, expr, Declaration, Expression},
        CrateScope, Identifier,
    },
    span::Span,
    support::{accumulate_errors, DisplayWith, ManyErrExt, TransposeExt},
};
use interpreter::{
    ffi,
    scope::{FunctionScope, Registration},
    Form, Interpreter,
};

// @Temporary @Note we need to update the macros
// const expr! { Type[] }: Expression = expr! { Type[] };

pub(crate) fn missing_annotation() -> Diagnostic {
    // @Task add span
    Diagnostic::bug()
        .with_code(Code::E030)
        .with_message("currently lambda literal parameters and patterns must be type-annotated")
}

/// The state of the typer.
// @Task add recursion depth field
pub struct Typer<'a> {
    pub scope: &'a mut CrateScope,
    warnings: &'a mut Diagnostics,
    parent_data_binding: Option<Identifier>,
    /// Whether the first type inference pass failed.
    poisoned: bool,
}

impl<'a> Typer<'a> {
    pub fn new(scope: &'a mut CrateScope, warnings: &'a mut Diagnostics) -> Self {
        Self {
            scope,
            warnings,
            parent_data_binding: None,
            poisoned: false,
        }
    }

    #[allow(dead_code)]
    fn warn(&mut self, warning: Diagnostic) {
        self.warnings.insert(warning);
    }

    pub fn interpreter(&mut self) -> Interpreter<'_> {
        Interpreter::new(&mut self.scope, &mut self.warnings)
    }

    pub fn infer_types_in_declaration(&mut self, declaration: &Declaration) -> Results<()> {
        let results_of_first_pass = self.start_infer_types_in_declaration(declaration);
        self.poisoned = results_of_first_pass.is_err();

        accumulate_errors!(
            results_of_first_pass,
            self.infer_types_of_out_of_order_bindings(),
        )?;

        Ok(())
    }

    fn start_infer_types_in_declaration(&mut self, declaration: &Declaration) -> Results<()> {
        use hir::DeclarationKind::*;

        match &declaration.kind {
            Value(value) => {
                self.evaluate_registration(
                    if declaration.attributes.has(AttributeKeys::FOREIGN) {
                        Registration::ForeignValueBinding {
                            binder: value.binder.clone(),
                            type_: value.type_annotation.clone(),
                        }
                    } else {
                        Registration::ValueBinding {
                            binder: value.binder.clone(),
                            type_: value.type_annotation.clone(),
                            value: Some(value.expression.clone().unwrap()),
                        }
                    },
                )?;
            }
            Data(data) => {
                // @Question don't return early??
                self.evaluate_registration(Registration::DataBinding {
                    binder: data.binder.clone(),
                    type_: data.type_annotation.clone(),
                })?;

                if declaration.attributes.has(AttributeKeys::FOREIGN) {
                    self.evaluate_registration(Registration::ForeignDataBinding {
                        binder: data.binder.clone(),
                    })?;
                } else {
                    let constructors = data.constructors.as_ref().unwrap();

                    // @Task @Beacon move to resolver
                    if let Some(inherent) =
                        declaration.attributes.get(AttributeKeys::INHERENT).next()
                    {
                        ffi::register_inherent_bindings(
                            &data.binder,
                            constructors
                                .iter()
                                .map(|constructor| constructor.unwrap_constructor()),
                            declaration,
                            inherent,
                            self.scope,
                        )
                        .many_err()?;
                    }

                    constructors
                        .iter()
                        .map(|constructor| {
                            self.parent_data_binding = Some(data.binder.clone());
                            self.start_infer_types_in_declaration(constructor)?;
                            self.parent_data_binding = None;
                            Ok(())
                        })
                        .collect::<Vec<_>>()
                        .transpose()?;
                }
            }
            Constructor(constructor) => {
                let data = self.parent_data_binding.take().unwrap();

                self.evaluate_registration(Registration::ConstructorBinding {
                    binder: constructor.binder.clone(),
                    type_: constructor.type_annotation.clone(),
                    data,
                })?;
            }
            Module(module) => {
                module
                    .declarations
                    .iter()
                    .map(|declaration| self.start_infer_types_in_declaration(declaration))
                    .collect::<Vec<_>>()
                    .transpose()?;
            }
            Use(_) | Invalid => {}
        }

        Ok(())
    }

    // @Note very strange API going on here
    // @Note we might want to store the evaluated types into the scopes instead of the
    // unevaluated ones. This dependends on how we'd like to normalize (WeakHead|Normal)
    // @Task @Beacon @Beacon somehow (*somehow*!) restructure this code so it is not DRY.
    // it is DRY even though we use an ugly macro..how sad is that??
    // we need to design the error handling here, it's super difficult, fragile, …
    fn evaluate_registration(&mut self, registration: Registration) -> Results<()> {
        use Registration::*;

        match registration.clone() {
            ValueBinding {
                binder,
                type_,
                value,
            } => {
                let value = value.clone().unwrap();

                recover_error!(
                    self.scope,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::CrateScope),
                    actual = type_
                );
                let type_ = self
                    .interpreter()
                    .evaluate_expression(
                        type_,
                        interpreter::Context {
                            scope: &FunctionScope::CrateScope,
                            form: Form::Normal, /* Form::WeakHeadNormal */
                        },
                    )
                    .many_err()?;

                let infered_type = match self
                    .infer_type_of_expression(value.clone(), &FunctionScope::CrateScope)
                {
                    Ok(expression) => expression,
                    Err(error) => {
                        return match error {
                            Error::Unrecoverable(error) => Err(error),
                            Error::OutOfOrderBinding => {
                                self.scope.out_of_order_bindings.push(registration);
                                self.scope.carry_out(Registration::ValueBinding {
                                    binder,
                                    type_: type_.clone(),
                                    value: None,
                                })
                            }
                            Error::TypeMismatch { .. } => unreachable!(),
                        }
                        .many_err();
                    }
                };

                recover_error!(
                    self.scope,
                    registration;
                    self.it_is_actual(type_.clone(), infered_type.clone(), &FunctionScope::CrateScope),
                    actual = value,
                    expected = type_
                );
                self.scope
                    .carry_out(Registration::ValueBinding {
                        binder,
                        type_: infered_type,
                        value: Some(value),
                    })
                    .many_err()?;
            }
            DataBinding { binder, type_ } => {
                recover_error!(
                    self.scope,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::CrateScope),
                    actual = type_
                );
                let type_ = self
                    .interpreter()
                    .evaluate_expression(
                        type_,
                        interpreter::Context {
                            scope: &FunctionScope::CrateScope,
                            form: Form::Normal, /* Form::WeakHeadNormal */
                        },
                    )
                    .many_err()?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    expr! { Type { Attributes::default(), Span::SHAM } },
                )
                .many_err()?;

                self.scope
                    .carry_out(Registration::DataBinding { binder, type_ })
                    .many_err()?;
            }
            ConstructorBinding {
                binder,
                type_,
                data,
            } => {
                recover_error!(
                    self.scope,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::CrateScope),
                    actual = type_
                );
                let type_ = self
                    .interpreter()
                    .evaluate_expression(
                        type_,
                        interpreter::Context {
                            scope: &FunctionScope::CrateScope,
                            form: Form::Normal, /* Form::WeakHeadNormal */
                        },
                    )
                    .many_err()?;

                self.assert_constructor_is_instance_of_type(
                    type_.clone(),
                    data.clone().to_expression(),
                )
                .many_err()?;

                self.scope
                    .carry_out(Registration::ConstructorBinding {
                        binder,
                        type_: type_.clone(),
                        data,
                    })
                    .many_err()?;
            }
            ForeignValueBinding { binder, type_ } => {
                recover_error!(
                    self.scope,
                    registration;
                    self.it_is_a_type(type_.clone(), &FunctionScope::CrateScope),
                    actual = type_
                );
                let type_ = self
                    .interpreter()
                    .evaluate_expression(
                        type_,
                        interpreter::Context {
                            scope: &FunctionScope::CrateScope,
                            form: Form::Normal, /* Form::WeakHeadNormal */
                        },
                    )
                    .many_err()?;

                self.scope
                    .carry_out(ForeignValueBinding { binder, type_ })
                    .many_err()?;
            }
            ForeignDataBinding { binder } => {
                self.scope
                    .carry_out(ForeignDataBinding { binder })
                    .many_err()?;
            }
        }

        macro recover_error(
            $scope:expr,
            $registration:expr;
            $check:expr,
            actual = $actual_value:expr
            $(, expected = $expected_reason:expr )?
        ) {
            match $check {
                Ok(expression) => expression,
                Err(error) => {
                    return match error {
                        Error::Unrecoverable(error) => Err(error),
                        Error::OutOfOrderBinding => {
                            $scope.out_of_order_bindings.push($registration);
                            Ok(())
                        }
                        Error::TypeMismatch { expected, actual } => {
                            Err(Diagnostic::error()
                                .with_code(Code::E032)
                                .with_message(format!(
                                    "expected type `{}`, got type `{}`",
                                    expected.with($scope), actual.with($scope)
                                ))
                                .with_labeled_span(
                                    &$actual_value,
                                    "has wrong type",
                                )
                                $( .with_labeled_span(&$expected_reason, "expected due to this") )?
                            )
                        }
                    }
                    .many_err()
                }
            }
        }

        Ok(())
    }

    fn infer_types_of_out_of_order_bindings(&mut self) -> Results<()> {
        while !self.scope.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.scope.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                self.evaluate_registration(binding)?;
            }

            if previous_amount == self.scope.out_of_order_bindings.len() {
                if self.poisoned {
                    return Ok(());
                }

                return Err(Diagnostic::bug()
                    .with_message("found some weird circular binding(s) during type checking"))
                .many_err();
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
        // changing the definition of FunctionScope... it's FunctionScope::CrateScope now, no payload
        // this means more boilerplate methods (either duplication or as in resolver: every FunctionScope method takes
        // a scope: &CrateScope parameter)
        &mut self,
        expression: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<Expression, Error> {
        use hir::ExpressionKind::*;
        use interpreter::Substitution::*;

        Ok(match expression.kind {
            Binding(binding) => scope
                .lookup_type(&binding.binder, &self.scope)
                .ok_or(Error::OutOfOrderBinding)?,
            Type => expr! { Type { Attributes::default(), Span::SHAM } },
            Number(number) => self
                .scope
                .lookup_foreign_number_type(&number, Some(expression.span))?,
            Text(_) => self
                .scope
                .lookup_foreign_type(ffi::Type::TEXT, Some(expression.span))?,
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

                expr! { Type { Attributes::default(), Span::SHAM } }
            }
            Lambda(lambda) => {
                let parameter_type: Expression = lambda
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;

                self.it_is_a_type(parameter_type.clone(), scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let infered_body_type =
                    self.infer_type_of_expression(lambda.body.clone(), &scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    self.it_is_a_type(body_type_annotation.clone(), &scope)?;
                    self.it_is_actual(body_type_annotation, infered_body_type.clone(), &scope)?;
                }

                expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        parameter: Some(lambda.parameter.clone()),
                        domain: parameter_type,
                        codomain: infered_body_type,
                        explicitness: Explicit,
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

                match &type_of_callee.kind {
                    PiType(pi) => {
                        let argument_type =
                            self.infer_type_of_expression(application.argument.clone(), scope)?;
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        self.it_is_actual(pi.domain.clone(), argument_type, scope)
                            .map_err(|error| match error {
                                Error::Unrecoverable(error) => error,
                                Error::TypeMismatch { expected, actual } => Diagnostic::error()
                                    .with_code(Code::E032)
                                    .with_message(format!(
                                        "expected type `{}`, got type `{}`",
                                        expected.with(self.scope),
                                        actual.with(self.scope)
                                    ))
                                    .with_labeled_span(&application.argument, "has wrong type")
                                    .with_labeled_span(&expected, "expected due to this"),
                                _ => unreachable!(),
                            })?;

                        match pi.parameter.clone() {
                            Some(_) => expr! {
                                Substitution {
                                    Attributes::default(),
                                    Span::SHAM;
                                    substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                                    expression: pi.codomain.clone(),
                                }
                            },
                            None => pi.codomain.clone(),
                        }
                    }
                    _ => {
                        return Err(Diagnostic::error()
                            .with_code(Code::E031)
                            .with_message(format!(
                                "expected type `_ -> _`, got type `{}`",
                                type_of_callee.with(self.scope)
                            ))
                            .with_labeled_span(&application.callee, "has wrong type")
                            .with_labeled_span(&application.argument, "applied to this")
                            .into());
                    }
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

                match &subject_type.clone().kind {
                    Binding(_) => {}
                    Application(_application) => todo!("polymorphic types in patterns"),
                    _ if self.is_a_type(subject_type.clone(), scope)? => {
                        return Err(Diagnostic::error()
                            .with_code(Code::E035)
                            .with_message("attempt to analyze a type")
                            .with_span(&expression.span)
                            .with_note("forbidden to uphold parametricity and type erasure")
                            .into());
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        subject_type.with(self.scope)
                    ),
                };

                let mut type_of_previous_body = None::<Expression>;

                for case in analysis.cases.iter() {
                    let mut types = Vec::new();

                    let handle_type_mismatch = |error, scope| match error {
                        Error::TypeMismatch { expected, actual } => Diagnostic::error()
                            .with_code(Code::E032)
                            .with_message(format!(
                                "expected type `{}`, got type `{}`",
                                expected.with(scope),
                                actual.with(scope)
                            ))
                            .with_labeled_span(&case.pattern, "has wrong type")
                            .with_labeled_span(&analysis.subject, "expected due to this")
                            .into(),
                        error => error,
                    };

                    use hir::PatternKind::*;

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pattern.kind {
                        Number(number) => {
                            let number_type = self
                                .scope
                                .lookup_foreign_number_type(number, Some(case.pattern.span))?;
                            self.it_is_actual(subject_type.clone(), number_type, scope)
                                .map_err(|error| handle_type_mismatch(error, &self.scope))?;
                        }
                        Text(_) => {
                            let text_type = self
                                .scope
                                .lookup_foreign_type(ffi::Type::TEXT, Some(case.pattern.span))?;
                            self.it_is_actual(subject_type.clone(), text_type, scope)
                                .map_err(|error| handle_type_mismatch(error, &self.scope))?;
                        }
                        Binding(binding) => {
                            let constructor_type =
                                scope.lookup_type(&binding.binder, &self.scope).unwrap();

                            self.it_is_actual(
                                subject_type.clone(),
                                constructor_type.clone(),
                                scope,
                            )
                            .map_err(|error| handle_type_mismatch(error, &self.scope))?;
                        }
                        Binder(_) => {
                            // @Temporary @Beacon @Bug error prone (once we try to impl deappl)
                            // @Update @Note don't push the type of subject but the type of the binder
                            types.push(subject_type.clone());
                        }
                        // @Task
                        Deapplication(deapplication) => {
                            // @Beacon @Task check that subject type is a pi type

                            match (&deapplication.callee.kind, &deapplication.argument.kind) {
                                // @Note should be an error obviously but does this need to be special-cased
                                // or can we defer this to an it_is_actual call??
                                (Number(_) | Text(_), _argument) => todo!(),
                                (Binding(binding), _argument) => {
                                    let constructor_type =
                                        scope.lookup_type(&binding.binder, &self.scope).unwrap();
                                    dbg!(
                                        &subject_type.with(&self.scope),
                                        deapplication.callee.with(&self.scope),
                                        &constructor_type.with(&self.scope)
                                    );

                                    todo!();
                                }
                                // @Task make error less fatal (keep processing next cases (match arms))
                                (Binder(binder), _) => {
                                    return Err(Diagnostic::error()
                                        .with_code(Code::E034)
                                        .with_message(format!(
                                            "binder `{}` used in callee position inside pattern",
                                            binder.binder
                                        ))
                                        .with_span(&binder.binder)
                                        .with_help("consider refering to a concrete binding")
                                        .into())
                                }
                                (Deapplication(_), _argument) => todo!(),
                                (Invalid, _) => unreachable!(),
                            };
                        }
                        Invalid => unreachable!(),
                    }

                    let type_ = self.infer_type_of_expression(
                        case.body.clone(),
                        &scope.extend_with_pattern_binders(types),
                    )?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            self.it_is_actual(previous_type.clone(), type_, scope)?;
                        }
                        None => {
                            type_of_previous_body = Some(type_);
                        }
                    }
                }

                //  @Temporary unhandled case
                type_of_previous_body.expect("caseless case analyses")
            }
            // @Beacon @Task
            ForeignApplication(_) => todo!(),
            IO(_) => self
                .scope
                .lookup_foreign_type(ffi::Type::IO, Some(expression.span))?,
            Invalid => expression,
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
            expr! { Type { Attributes::default(), Span::SHAM } },
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
            expr! { Type { Attributes::default(), Span::SHAM } },
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
            expected.clone(),
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
            return Err(Error::TypeMismatch { expected, actual });
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
            expected.clone(),
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

        self.interpreter()
            .equals(expected.clone(), actual.clone(), scope)
    }

    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(&mut self, expression: Expression, scope: &FunctionScope<'_>) -> Expression {
        use hir::ExpressionKind::*;

        match expression.kind {
            PiType(literal) => {
                if literal.parameter.is_some() {
                    let scope = scope.extend_with_parameter(literal.domain.clone());
                    self.result_type(literal.codomain.clone(), &scope)
                } else {
                    self.result_type(literal.codomain.clone(), scope)
                }
            }
            Application(_)
            | Type
            | Binding(_) => expression,
            Lambda(_)
            | Number(_)
            | Text(_)
            | UseIn
            | CaseAnalysis(_)
            | IO(_)
            // @Note not sure
            | Substitution(_)
            | ForeignApplication(_) => unreachable!(),
            Invalid => expression,
        }
    }

    /// Returns the callee of an expression.
    ///
    /// Example: Returns the `f` in `f a b c`.
    fn callee(&mut self, mut expression: Expression) -> Expression {
        loop {
            expression = match expression.kind {
                hir::ExpressionKind::Application(application) => application.callee.clone(),
                _ => return expression,
            }
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
    ) -> Result<()> {
        let result_type = self.result_type(constructor, &FunctionScope::CrateScope);
        let callee = self.callee(result_type.clone());

        if !self
            .interpreter()
            .equals(type_.clone(), callee, &FunctionScope::CrateScope)?
        {
            Err(Diagnostic::error()
                .with_code(Code::E033)
                .with_message(format!(
                    "`{}` is not an instance of `{}`",
                    result_type.with(&self.scope),
                    type_.with(&self.scope)
                ))
                .with_span(&result_type.span))
        } else {
            Ok(())
        }
    }
}

// @Note maybe we should redesign this as a trait (object) looking at those
// methods mirroring the variants
pub enum Error {
    Unrecoverable(Diagnostic),
    OutOfOrderBinding,
    TypeMismatch {
        expected: Expression,
        actual: Expression,
    },
}

impl From<Diagnostic> for Error {
    fn from(error: Diagnostic) -> Self {
        Self::Unrecoverable(error)
    }
}
