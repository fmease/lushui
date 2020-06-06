//! The type checker.

mod types;

use crate::{
    diagnostic::todo,
    diagnostic::*,
    hir::{self, *},
    interpreter::{
        self, ffi,
        scope::{FunctionScope, Registration},
        CrateScope, Form,
    },
    parser::{AttributeKind, Explicit},
    resolver::{Identifier, Resolved},
};

pub type Declaration = hir::Declaration<Resolved>;
pub type Expression = hir::Expression<Resolved>;

const TYPE: Expression = expr! { Type[] };

pub(crate) fn missing_annotation() -> Diagnostic {
    // @Task add span
    Diagnostic::new(
        Level::Bug,
        Code::E030,
        "currently lambda literal parameters and patterns must be type-annotated",
    )
}

#[derive(Default)]
struct Environment {
    data: Option<Identifier>,
}

impl Declaration {
    /// Try to type check a declaration modifying the given scope.
    pub fn infer_type(&self, scope: &mut CrateScope) -> Result<()> {
        self.infer_type_first_pass(scope, Default::default())?;
        scope.infer_type_of_out_of_order_bindings()
    }

    fn infer_type_first_pass(
        &self,
        scope: &mut CrateScope,
        environment: Environment,
    ) -> Result<()> {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                if self.attributes.has(AttributeKind::Foreign) {
                    Registration::ForeignValueBinding {
                        binder: declaration.binder.clone(),
                        r#type: declaration.type_annotation.clone(),
                    }
                    .evaluate(scope)?;
                } else {
                    Registration::ValueBinding {
                        binder: declaration.binder.clone(),
                        r#type: declaration.type_annotation.clone(),
                        value: Some(declaration.expression.clone().unwrap()),
                    }
                    .evaluate(scope)?;
                }
            }
            Data(data) => {
                // @Question don't return early??
                Registration::DataBinding {
                    binder: data.binder.clone(),
                    r#type: data.type_annotation.clone(),
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
                        )?;
                    }

                    for constructor in constructors {
                        constructor.infer_type_first_pass(
                            scope,
                            Environment {
                                data: Some(data.binder.clone()),
                            },
                        )?;
                    }
                }
            }
            Constructor(constructor) => {
                Registration::ConstructorBinding {
                    binder: constructor.binder.clone(),
                    r#type: constructor.type_annotation.clone(),
                    data: environment.data.unwrap(),
                }
                .evaluate(scope)?;
            }
            Module(module) => {
                for declaration in &module.declarations {
                    declaration.infer_type_first_pass(scope, Default::default())?;
                }
            }
            Use(_) => {}
            Invalid => {}
        }

        Ok(())
    }
}

// @Note very strange API goiong on here
// @Note we might want to store the evaluated types into the scopes instead of the
// unevaluated ones. This dependends on how we'd like to normalize (WeakHead|Normal)
impl Registration {
    fn evaluate(self, scope: &mut CrateScope) -> Result<()> {
        use Registration::*;

        match self.clone() {
            ValueBinding {
                binder,
                r#type,
                value,
            } => {
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&(&*scope).into())
                );
                let infered_type = match value.clone().unwrap().infer_type(&(&*scope).into()) {
                    Err(Error::Recoverable(OutOfOrderBinding)) => {
                        scope.out_of_order_bindings.push(self);
                        scope.carry_out(Registration::ValueBinding {
                            binder,
                            r#type,
                            value: None,
                        })?;
                        return Ok(());
                    }
                    Err(error) => return Err(error.try_into().unwrap()),
                    Ok(expression) => expression,
                };

                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.is_actual(infered_type.clone(), &(&*scope).into())
                );
                scope.carry_out(Registration::ValueBinding {
                    binder,
                    r#type: infered_type,
                    value,
                })?;
            }
            DataBinding { binder, r#type } => {
                let r#type = r#type.evaluate(interpreter::Context {
                    scope: &(&*scope).into(),
                    form: Form::Normal, /* Form::WeakHeadNormal */
                })?;
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&(&*scope).into())
                );

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                types::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    r#type.clone(),
                    TYPE,
                    scope,
                )?;

                scope.carry_out(Registration::DataBinding { binder, r#type })?;
            }
            ConstructorBinding {
                binder,
                r#type,
                data,
            } => {
                let r#type = r#type.evaluate(interpreter::Context {
                    scope: &(&*scope).into(),
                    form: Form::Normal, /* Form::WeakHeadNormal */
                })?;
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&(&*scope).into())
                );

                types::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    r#type.clone(),
                    data.clone().to_expression(),
                    scope,
                )?;

                scope.carry_out(Registration::ConstructorBinding {
                    binder,
                    r#type: r#type.clone(),
                    data,
                })?;
            }
            ForeignValueBinding { binder, r#type } => {
                let r#type = r#type.evaluate(interpreter::Context {
                    scope: &(&*scope).into(),
                    form: Form::Normal, /* Form::WeakHeadNormal */
                })?;
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&(&*scope).into())
                );

                scope.carry_out(Self::ForeignValueBinding { binder, r#type })?;
            }
            Self::ForeignDataBinding { binder } => {
                scope.carry_out(Registration::ForeignDataBinding { binder })?;
            }
        }

        // @Note ugly as hell!
        macro handle_out_of_order_binding($scope:expr, $registration:expr, $check:expr) {
            match $check {
                Err(Error::Recoverable(OutOfOrderBinding)) => {
                    $scope.out_of_order_bindings.push($registration);
                    return Ok(());
                }
                Err(error) => return Err(error.try_into().unwrap()),
                Ok(expression) => expression,
            }
        }

        Ok(())
    }
}

impl CrateScope {
    fn infer_type_of_out_of_order_bindings(&mut self) -> Result<()> {
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
                return Err(Diagnostic::new(
                    Level::Fatal,
                    None,
                    "found equi-recursive? or circular thingy trying to type-check",
                ));
            }
        }

        Ok(())
    }
}

impl Expression {
    /// Try to infer the type of an expression.
    fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self, Error> {
        use crate::interpreter::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope
                .lookup_type(&binding.binder)
                .ok_or(Error::Recoverable(OutOfOrderBinding))?,
            Type => TYPE,
            Nat(_) => scope
                .crate_scope()
                .lookup_foreign_type(ffi::Type::NAT, Some(self.span))?,
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
                        pi.domain.clone().is_actual(argument_type, scope)?;

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
                        // @Task improve error diagnostic
                        // @Task add span
                        return Err(Error::Unrecoverable(Diagnostic::new(
                            Level::Fatal,
                            Code::E031,
                            format!(
                                "cannot apply `{}` to a `{}`",
                                application.argument, type_of_callee
                            ),
                        )));
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
                let r#type = case_analysis
                    .subject
                    .clone()
                    .infer_type(scope)?
                    // to get rid of Substitutions
                    .evaluate(interpreter::Context::new(scope))?;

                // @Task verify that
                // * patterns are of correct type (i.e. r#type is an ADT and the constructors are the valid ones)
                // * all constructors are covered
                // * all case_analysis.cases>>.expressions are of the same type

                match &r#type.kind {
                    Binding(_) => {}
                    Application(_application) => todo!(? "polymorphic types in patterns"),
                    _ => todo!(? "encountered unsupported type to be case-analysed"),
                };

                let mut type_of_previous_body = None::<Self>;

                for case in case_analysis.cases.iter() {
                    let mut types = Vec::new();

                    match &case.pattern.kind {
                        PatternKind::Nat(_) => {
                            r#type.clone().is_actual(
                                scope
                                    .crate_scope()
                                    .lookup_foreign_type(ffi::Type::NAT, Some(case.pattern.span))?,
                                scope,
                            )?;
                        }
                        PatternKind::Text(_text) => {
                            r#type.clone().is_actual(
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
                            r#type
                                .clone()
                                .is_actual(type_of_constructor.clone(), scope)?;
                        }
                        PatternKind::Binder(_) => {
                            // @Temporary @Beacon error prone (once we try to impl deappl)
                            types.push(r#type.clone());
                        }
                        PatternKind::Deapplication(_deapplication) => {
                            todo!(? "handle deapplications in patterns", &case.pattern)
                        }
                    }
                    let r#type = case
                        .body
                        .clone()
                        .infer_type(&scope.extend_with_pattern_binders(types))?;

                    match type_of_previous_body {
                        Some(ref previous_type) => {
                            previous_type.clone().is_actual(r#type, scope)?;
                        }
                        None => {
                            type_of_previous_body = Some(r#type);
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
        let r#type = self.infer_type(scope)?;
        TYPE.is_actual(r#type, scope)
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
            // @Task improve diagnostic, add span information, @Bug span information are *so* off!!
            // @Task we need to return an Error::Recoverable(TypeMismatch(…))
            return Err(
                // Error::Unrecoverable(
                //     Diagnostic::new(Level::Fatal, Code::E032, "mismatched types")
                //         .with_labeled_span(
                //             &actual,
                //             format!("expected `{}`, got `{}`", self, actual),
                //         ),
                // ),
                Error::Unrecoverable(Diagnostic::new(
                    Level::Fatal,
                    Code::E032,
                    format!("mismatched types. expected `{}`, got `{}`", self, actual),
                )),
            );
        }

        Ok(())
    }
}

type Error = crate::support::Error<RecoverableError>;

use RecoverableError::*;

enum RecoverableError {
    OutOfOrderBinding,
}

use std::convert::{TryFrom, TryInto};

impl TryFrom<RecoverableError> for Diagnostic {
    type Error = ();

    fn try_from(error: RecoverableError) -> Result<Self, Self::Error> {
        match error {
            OutOfOrderBinding => Err(()),
        }
    }
}
