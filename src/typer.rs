//! The type checker.

mod types;

use crate::{
    diagnostic::todo,
    diagnostic::{Code, Diagnostic, Result, Results},
    hir::{self, *},
    interpreter::{
        self, ffi,
        scope::{FunctionScope, Registration},
        CrateScope, Form,
    },
    parser::{AttributeKind, Explicit},
    resolver::{Identifier, Resolved},
    support::{accumulate_errors::*, ManyErrExt, TransposeExt},
};
use std::default::default;

pub type Declaration = hir::Declaration<Resolved>;
pub type Expression = hir::Expression<Resolved>;

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

impl Declaration {
    /// Try to type check a declaration modifying the given scope.
    pub fn infer_type(&self, scope: &mut CrateScope) -> Results<()> {
        (
            self.infer_type_first_pass(scope, default()),
            scope.infer_type_of_out_of_order_bindings(),
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
            Use(_) => {}
            Invalid => {}
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
                    type_.clone().is_a_type(&(&*scope).into()),
                    actual = type_
                );

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
                    type_.clone().is_actual(infered_type.clone(), &(&*scope).into()),
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
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;
                recover_error!(
                    scope,
                    self;
                    type_.clone().is_a_type(&(&*scope).into()),
                    actual = type_
                );

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                types::instance::assert_constructor_is_instance_of_type(type_.clone(), TYPE, scope)
                    .many_err()?;

                scope
                    .carry_out(Registration::DataBinding { binder, type_ })
                    .many_err()?;
            }
            ConstructorBinding {
                binder,
                type_,
                data,
            } => {
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;
                recover_error!(
                    scope,
                    self;
                    type_.clone().is_a_type(&(&*scope).into()),
                    actual = type_
                );

                types::instance::assert_constructor_is_instance_of_type(
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
                let type_ = type_
                    .evaluate(interpreter::Context {
                        scope: &(&*scope).into(),
                        form: Form::Normal, /* Form::WeakHeadNormal */
                    })
                    .many_err()?;
                recover_error!(
                    scope,
                    self;
                    type_.clone().is_a_type(&(&*scope).into()),
                    actual = type_
                );

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
                                    expected, actual
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
    fn infer_type_of_out_of_order_bindings(&mut self) -> Results<()> {
        while !self.out_of_order_bindings.is_empty() {
            let bindings = std::mem::take(&mut self.out_of_order_bindings);
            let previous_amount = bindings.len();

            for binding in bindings {
                binding.evaluate(self)?;
            }

            if previous_amount == self.out_of_order_bindings.len() {
                // @Temporary
                // @Note this case might occur when the bindings is its own type (like Type-in-Type)
                // I don't know if there are any other cases
                return Err(Diagnostic::error().with_message(
                    "found equi-recursive? or circular thingy trying to type-check",
                ))
                .many_err();
            }
        }

        Ok(())
    }
}

impl Expression {
    /// Try to infer the type of an expression.
    // @Beacon @Beacon @Task verify and implement that all is_a_type and is_actual lead to good error messages
    // and keep it DRY (try to abstract over error handling, find a good API)
    // @Task make independent (^^) type errors non-fatal in respect to each other, i.e. return more than one
    // type error in possible cases
    fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self, Error> {
        use crate::interpreter::Substitution::*;
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
                literal.domain.clone().is_a_type(scope)?;

                if literal.parameter.is_some() {
                    literal
                        .codomain
                        .clone()
                        .is_a_type(&mut scope.extend_with_parameter(literal.domain.clone()))?;
                } else {
                    literal.codomain.clone().is_a_type(scope)?;
                }

                TYPE
            }
            Lambda(lambda) => {
                let parameter_type: Self = lambda
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;

                parameter_type.clone().is_a_type(scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let infered_body_type = lambda.body.clone().infer_type(&scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    body_type_annotation.clone().is_a_type(&scope)?;
                    body_type_annotation.is_actual(infered_body_type.clone(), &scope)?;
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
                            .is_actual(argument_type, scope)
                            .map_err(|error| match error {
                                Error::Unrecoverable(error) => error,
                                Error::TypeMismatch { expected, actual } => Diagnostic::error()
                                    .with_message(format!(
                                        "expected type `{}`, got type `{}`",
                                        expected, actual
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
                        // @Task @Beacon @Beacon @Beacon improve error diagnostic
                        // @Task add span
                        return Err(Error::Unrecoverable(
                            Diagnostic::error()
                                .with_code(Code::E031)
                                .with_message(format!(
                                    "expected type `_ -> _`, got type `{}`",
                                    type_of_callee
                                ))
                                .with_labeled_span(&application.callee, "has wrong type")
                                .with_labeled_span(&application.argument, "applied to this"),
                        ));
                    }
                }
            }
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .infer_type(scope)?,
            UseIn => todo!(? "1stP infer type of use/in"),
            CaseAnalysis(case_analysis) => {
                let type_ = case_analysis
                    .subject
                    .clone()
                    .infer_type(scope)?
                    // to get rid of Substitutions
                    .evaluate(interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. type_ is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all case_analysis.cases>>.expressions are of the same type

                match &type_.kind {
                    Binding(_) => {}
                    Application(_application) => todo!(? "polymorphic types in patterns"),
                    _ => todo!(? "encountered unsupported type to be case-analysed"),
                };

                let mut type_of_previous_body = None::<Self>;

                for case in case_analysis.cases.iter() {
                    let mut types = Vec::new();

                    match &case.pattern.kind {
                        PatternKind::Number(number) => {
                            type_.clone().is_actual(
                                // @Bug @Beacon
                                scope
                                    .crate_scope()
                                    .lookup_foreign_number_type(number, Some(case.pattern.span))?,
                                scope,
                            )?;
                        }
                        PatternKind::Text(_text) => {
                            type_.clone().is_actual(
                                scope.crate_scope().lookup_foreign_type(
                                    ffi::Type::TEXT,
                                    Some(case.pattern.span),
                                )?,
                                scope,
                            )?;
                        }
                        PatternKind::Binding(binding) => {
                            let type_of_constructor = scope.lookup_type(&binding.binder).unwrap();
                            // @Note error message very general, could be specialized to constructors
                            type_
                                .clone()
                                .is_actual(type_of_constructor.clone(), scope)?;
                        }
                        PatternKind::Binder(_) => {
                            // @Temporary @Beacon error prone (once we try to impl deappl)
                            types.push(type_.clone());
                        }
                        PatternKind::Deapplication(_deapplication) => {
                            todo!(? "handle deapplications in patterns", &case.pattern)
                        }
                    }
                    let type_ = case
                        .body
                        .clone()
                        .infer_type(&scope.extend_with_pattern_binders(types))?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            previous_type.clone().is_actual(type_, scope)?;
                        }
                        None => {
                            type_of_previous_body = Some(type_);
                        }
                    }
                }

                type_of_previous_body.ok_or_else(|| todo!("caseless case analyses"))?
            }
            ForeignApplication(_) => unreachable!(),
            Invalid => self,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn is_a_type(self, scope: &FunctionScope<'_>) -> Result<(), Error> {
        let type_ = self.infer_type(scope)?;
        TYPE.is_actual(type_, scope)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<(), Error> {
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
}

// @Note maybe we should redesign this as a trait (object) looking at those
// methods mirroring the variants
enum Error {
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
