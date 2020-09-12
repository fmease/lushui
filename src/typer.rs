//! The type checker.

pub mod interpreter;

use crate::{
    ast::{AttributeKind, Explicit},
    diagnostic::{Code, Diagnostic, Result, Results},
    hir::{self, *},
    resolver::{CrateScope, Identifier, Resolved},
    support::{accumulate_errors::*, DisplayWith, ManyErrExt, TransposeExt},
};
use interpreter::{
    ffi,
    scope::{FunctionScope, Registration},
    Form,
};
use std::default::default;

pub type Declaration = hir::Declaration<Resolved>;
pub type Expression = hir::Expression<Resolved>;
pub type Pattern = hir::Pattern<Resolved>;

const TYPE: Expression = expr! { Type[] };

pub(crate) fn missing_annotation() -> Diagnostic {
    // @Task add span
    Diagnostic::bug()
        .with_code(Code::E030)
        .with_message("currently lambda literal parameters and patterns must be type-annotated")
}

#[derive(Default)]
struct Environment {
    data: Option<Identifier>,
}

// @Beacon @Task restructure so that we have a Typer struct with a lot of information

impl Declaration {
    /// Try to type check a declaration modifying the given scope.
    pub fn infer_type(&self, scope: &mut CrateScope) -> Results<()> {
        let results_first_pass = self.infer_type_first_pass(scope, default());
        let poisoned = results_first_pass.is_err();

        (
            results_first_pass,
            scope.infer_type_of_out_of_order_bindings(poisoned),
        )
            .accumulate_err()?;

        Ok(())
    }

    fn infer_type_first_pass(
        &self,
        scope: &mut CrateScope,
        environment: Environment,
    ) -> Results<()> {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                if self.attributes.has(AttributeKind::Foreign) {
                    Registration::ForeignValueBinding {
                        binder: declaration.binder.clone(),
                        type_: declaration.type_annotation.clone(),
                    }
                    .evaluate(scope)?;
                } else {
                    Registration::ValueBinding {
                        binder: declaration.binder.clone(),
                        type_: declaration.type_annotation.clone(),
                        value: Some(declaration.expression.clone().unwrap()),
                    }
                    .evaluate(scope)?;
                }
            }
            Data(data) => {
                // @Question don't return early??
                Registration::DataBinding {
                    binder: data.binder.clone(),
                    type_: data.type_annotation.clone(),
                }
                .evaluate(scope)?;

                if self.attributes.has(AttributeKind::Foreign) {
                    Registration::ForeignDataBinding {
                        binder: data.binder.clone(),
                    }
                    .evaluate(scope)?;
                } else {
                    let constructors = data.constructors.as_ref().unwrap();

                    // @Task @Beacon move to resolver
                    if let Some(inherent) = self.attributes.get(AttributeKind::Inherent) {
                        ffi::register_inherent_bindings(
                            &data.binder,
                            constructors
                                .iter()
                                .map(|constructor| constructor.unwrap_constructor()),
                            self,
                            inherent,
                            scope,
                        )
                        .many_err()?;
                    }

                    constructors
                        .iter()
                        .map(|constructor| {
                            constructor.infer_type_first_pass(
                                scope,
                                Environment {
                                    data: Some(data.binder.clone()),
                                },
                            )
                        })
                        .collect::<Vec<_>>()
                        .transpose()?;
                }
            }
            Constructor(constructor) => {
                Registration::ConstructorBinding {
                    binder: constructor.binder.clone(),
                    type_: constructor.type_annotation.clone(),
                    data: environment.data.unwrap(),
                }
                .evaluate(scope)?;
            }
            Module(module) => {
                module
                    .declarations
                    .iter()
                    .map(|declaration| declaration.infer_type_first_pass(scope, default()))
                    .collect::<Vec<_>>()
                    .transpose()?;
            }
            Use(_) | Invalid => {}
        }

        Ok(())
    }
}

// @Note very strange API going on here
// @Note we might want to store the evaluated types into the scopes instead of the
// unevaluated ones. This dependends on how we'd like to normalize (WeakHead|Normal)
impl Registration {
    // @Task @Beacon @Beacon somehow (*somehow*!) restructure this code so it is not DRY.
    // it is DRY even though we use an ugly macro..how sad is that??
    // we need to design the error handling here, it's super difficult, fragile, …
    fn evaluate(self, scope: &mut CrateScope) -> Results<()> {
        use Registration::*;

        match self.clone() {
            ValueBinding {
                binder,
                type_,
                value,
            } => {
                let value = value.clone().unwrap();

                recover_error!(
                    scope,
                    self;
                    type_.clone().it_is_a_type(&(&*scope).into()),
                    actual = type_
                );
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;

                let infered_type = match value.clone().infer_type(&(&*scope).into()) {
                    Ok(expression) => expression,
                    Err(error) => {
                        return match error {
                            Error::Unrecoverable(error) => Err(error),
                            Error::OutOfOrderBinding => {
                                scope.out_of_order_bindings.push(self);
                                scope.carry_out(Registration::ValueBinding {
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
                    scope,
                    self;
                    type_.clone().it_is_actual(infered_type.clone(), &(&*scope).into()),
                    actual = value,
                    expected = type_
                );
                scope
                    .carry_out(Registration::ValueBinding {
                        binder,
                        type_: infered_type,
                        value: Some(value),
                    })
                    .many_err()?;
            }
            DataBinding { binder, type_ } => {
                recover_error!(
                    scope,
                    self;
                    type_.clone().it_is_a_type(&(&*scope).into()),
                    actual = type_
                );
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;

                assert_constructor_is_instance_of_type(type_.clone(), TYPE, scope).many_err()?;

                scope
                    .carry_out(Registration::DataBinding { binder, type_ })
                    .many_err()?;
            }
            ConstructorBinding {
                binder,
                type_,
                data,
            } => {
                recover_error!(
                    scope,
                    self;
                    type_.clone().it_is_a_type(&(&*scope).into()),
                    actual = type_
                );
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;

                assert_constructor_is_instance_of_type(
                    type_.clone(),
                    data.clone().to_expression(),
                    scope,
                )
                .many_err()?;

                scope
                    .carry_out(Registration::ConstructorBinding {
                        binder,
                        type_: type_.clone(),
                        data,
                    })
                    .many_err()?;
            }
            ForeignValueBinding { binder, type_ } => {
                recover_error!(
                    scope,
                    self;
                    type_.clone().it_is_a_type(&(&*scope).into()),
                    actual = type_
                );
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;

                scope
                    .carry_out(Self::ForeignValueBinding { binder, type_ })
                    .many_err()?;
            }
            Self::ForeignDataBinding { binder } => {
                scope
                    .carry_out(Registration::ForeignDataBinding { binder })
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
}

impl CrateScope {
    /// `poisoned`: Whether the previous pass failed
    fn infer_type_of_out_of_order_bindings(&mut self, poisoned: bool) -> Results<()> {
        while !self.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                binding.evaluate(self)?;
            }

            if previous_amount == self.out_of_order_bindings.len() {
                if poisoned {
                    return Ok(());
                }

                return Err(Diagnostic::bug()
                    .with_message("found some weird circular binding during type checking"))
                .many_err();
            }
        }

        Ok(())
    }
}

impl Expression {
    /// Try to infer the type of an expression.
    // @Beacon @Beacon @Task verify and implement that all it_is_a_type and it_is_actual lead to good error messages
    // and keep it DRY (try to abstract over error handling, find a good API)
    // @Task make independent (^^) type errors non-fatal in respect to each other, i.e. return more than one
    // type error in possible cases
    fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self, Error> {
        use crate::typer::interpreter::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope
                .lookup_type(&binding.binder)
                .ok_or(Error::OutOfOrderBinding)?,
            Type => TYPE,
            Number(number) => scope
                .crate_scope()
                .lookup_foreign_number_type(&number, Some(self.span))?,
            Text(_) => scope
                .crate_scope()
                .lookup_foreign_type(ffi::Type::TEXT, Some(self.span))?,
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type`) has been normalized?
                literal.domain.clone().it_is_a_type(scope)?;

                if literal.parameter.is_some() {
                    literal
                        .codomain
                        .clone()
                        .it_is_a_type(&mut scope.extend_with_parameter(literal.domain.clone()))?;
                } else {
                    literal.codomain.clone().it_is_a_type(scope)?;
                }

                TYPE
            }
            Lambda(lambda) => {
                let parameter_type: Self = lambda
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;

                parameter_type.clone().it_is_a_type(scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let infered_body_type = lambda.body.clone().infer_type(&scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    body_type_annotation.clone().it_is_a_type(&scope)?;
                    body_type_annotation.it_is_actual(infered_body_type.clone(), &scope)?;
                }

                expr! {
                    PiType[self.span] {
                        parameter: Some(lambda.parameter.clone()),
                        domain: parameter_type,
                        codomain: infered_body_type,
                        explicitness: Explicit,
                    }
                }
            }
            Application(application) => {
                // @Note this is an example where we normalize after an infer_type which means infer_type
                // returns possibly non-normalized expressions, can we do better?
                let type_of_callee = application.callee.clone().infer_type(scope)?.evaluate(
                    interpreter::Context {
                        scope,
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    },
                )?;

                match &type_of_callee.kind {
                    PiType(pi) => {
                        let argument_type = application.argument.clone().infer_type(scope)?;
                        // @Bug this error handling might *steal* the error from other handlers further
                        // down the call chain
                        pi.domain
                            .clone()
                            .it_is_actual(argument_type, scope)
                            .map_err(|error| match error {
                                Error::Unrecoverable(error) => error,
                                Error::TypeMismatch { expected, actual } => Diagnostic::error()
                                    .with_code(Code::E032)
                                    .with_message(format!(
                                        "expected type `{}`, got type `{}`",
                                        expected.with(scope.crate_scope()),
                                        actual.with(scope.crate_scope())
                                    ))
                                    .with_labeled_span(&application.argument, "has wrong type")
                                    .with_labeled_span(&expected, "expected due to this"),
                                _ => unreachable!(),
                            })?;

                        match pi.parameter.clone() {
                            Some(_) => expr! {
                                Substitution[] {
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
                                type_of_callee.with(scope.crate_scope())
                            ))
                            .with_labeled_span(&application.callee, "has wrong type")
                            .with_labeled_span(&application.argument, "applied to this")
                            .into());
                    }
                }
            }
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .infer_type(scope)?,
            UseIn => todo!("1stP infer type of use/in"),
            CaseAnalysis(analysis) => {
                let subject_type = analysis
                    .subject
                    .clone()
                    .infer_type(scope)?
                    // to get rid of Substitutions
                    .evaluate(interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. type_ is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all analysis.cases>>.expressions are of the same type

                match &subject_type.clone().kind {
                    Binding(_) => {}
                    Application(_application) => todo!("polymorphic types in patterns"),
                    _ if subject_type.clone().is_a_type(scope)? => {
                        return Err(Diagnostic::error()
                            .with_code(Code::E035)
                            .with_message("attempt to analyze a type")
                            .with_span(&self.span)
                            .with_note("forbidden to uphold parametricity and type erasure")
                            .into());
                    }
                    _ => todo!(
                        "encountered unsupported type to be case-analysed type={}",
                        subject_type.with(scope.crate_scope())
                    ),
                };

                let mut type_of_previous_body = None::<Self>;

                for case in analysis.cases.iter() {
                    let mut types = Vec::new();

                    let handle_type_mismatch = |error| match error {
                        Error::TypeMismatch { expected, actual } => Diagnostic::error()
                            .with_code(Code::E032)
                            .with_message(format!(
                                "expected type `{}`, got type `{}`",
                                expected.with(scope.crate_scope()),
                                actual.with(scope.crate_scope())
                            ))
                            .with_labeled_span(&case.pattern, "has wrong type")
                            .with_labeled_span(&analysis.subject, "expected due to this")
                            .into(),
                        error => error,
                    };

                    use PatternKind::*;

                    // @Task add help subdiagnostic when a constructor is (de)applied to too few arguments
                    // @Update @Note or just replace the type mismatch error (hmm) with an arity mismatch error
                    // not sure
                    match &case.pattern.kind {
                        Number(number) => {
                            let number_type = scope
                                .crate_scope()
                                .lookup_foreign_number_type(number, Some(case.pattern.span))?;
                            subject_type
                                .clone()
                                .it_is_actual(number_type, scope)
                                .map_err(handle_type_mismatch)?;
                        }
                        Text(_) => {
                            let text_type = scope
                                .crate_scope()
                                .lookup_foreign_type(ffi::Type::TEXT, Some(case.pattern.span))?;
                            subject_type
                                .clone()
                                .it_is_actual(text_type, scope)
                                .map_err(handle_type_mismatch)?
                        }
                        Binding(binding) => {
                            let constructor_type = scope.lookup_type(&binding.binder).unwrap();
                            subject_type
                                .clone()
                                .it_is_actual(constructor_type.clone(), scope)
                                .map_err(handle_type_mismatch)?;
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
                                        scope.lookup_type(&binding.binder).unwrap();
                                    dbg!(
                                        &subject_type.with(scope.crate_scope()),
                                        deapplication.callee.with(scope.crate_scope()),
                                        &constructor_type.with(scope.crate_scope())
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
                            };
                        }
                    }

                    let type_ = case
                        .body
                        .clone()
                        .infer_type(&scope.extend_with_pattern_binders(types))?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            previous_type.clone().it_is_actual(type_, scope)?;
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
            IO(_) => scope
                .crate_scope()
                .lookup_foreign_type(ffi::Type::IO, Some(self.span))?,
            Invalid => self,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn it_is_a_type(self, scope: &FunctionScope<'_>) -> Result<(), Error> {
        let type_ = self.infer_type(scope)?;
        TYPE.it_is_actual(type_, scope)
    }

    fn is_a_type(self, scope: &FunctionScope<'_>) -> Result<bool, Error> {
        let type_ = self.infer_type(scope)?;
        TYPE.is_actual(type_, scope).map_err(Into::into)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn it_is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<(), Error> {
        let expected = self.clone().evaluate(interpreter::Context {
            scope,
            form: Form::Normal, /* Form::WeakHeadNormal */
        })?;
        let actual = actual.evaluate(interpreter::Context {
            scope,
            form: Form::Normal, /* Form::WeakHeadNormal */
        })?;

        if !expected.clone().equals(actual.clone(), scope)? {
            return Err(Error::TypeMismatch {
                expected: self,
                actual,
            });
        }

        Ok(())
    }

    fn is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<bool> {
        let expected = self.clone().evaluate(interpreter::Context {
            scope,
            form: Form::Normal, /* Form::WeakHeadNormal */
        })?;
        let actual = actual.evaluate(interpreter::Context {
            scope,
            form: Form::Normal, /* Form::WeakHeadNormal */
        })?;

        expected.clone().equals(actual.clone(), scope)
    }

    // @Question @Bug returns are type that might depend on parameters which we don't supply!!
    // gets R in A -> B -> C -> R plus an environment b.c. R could depend on outer stuff
    // @Note this function assumes that the expression has already been normalized!
    fn result_type(self, scope: &FunctionScope<'_>) -> Self {
        use ExpressionKind::*;

        match self.kind {
            PiType(literal) => {
                if literal.parameter.is_some() {
                    let scope = scope.extend_with_parameter(literal.domain.clone());
                    literal.codomain.clone().result_type(&scope)
                } else {
                    literal.codomain.clone().result_type(scope)
                }
            }
            Application(_)
            | Type
            | Binding(_) => self,
            Lambda(_)
            | Number(_)
            | Text(_)
            | UseIn
            | CaseAnalysis(_)
            | IO(_)
            // @Note not sure
            | Substitution(_)
            | ForeignApplication(_) => unreachable!(),
            Invalid => self,
        }
    }

    /// Returns the callee of an expression.
    ///
    /// Example: Returns the `f` in `f a b c`.
    fn callee(mut self) -> Self {
        loop {
            self = match self.kind {
                ExpressionKind::Application(application) => application.callee.clone(),
                _ => return self,
            }
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
    constructor: Expression,
    type_: Expression,
    scope: &CrateScope,
) -> Result<()> {
    let result_type = constructor.result_type(&scope.into());
    let callee = result_type.clone().callee();

    if !type_.clone().equals(callee, &scope.into())? {
        Err(Diagnostic::error()
            .with_code(Code::E033)
            .with_message(format!(
                "`{}` is not an instance of `{}`",
                result_type.with(scope),
                type_.with(scope)
            ))
            .with_span(&result_type.span))
    } else {
        Ok(())
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
