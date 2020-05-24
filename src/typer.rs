//! The type checker.

mod types;

use crate::{
    diagnostic::*,
    hir::{self, *},
    interpreter::{
        ffi,
        scope::{FunctionScope, Registration},
        CrateScope, Form,
    },
    parser::{AttributeKind, Explicitness},
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
struct Context {
    data: Option<Identifier>,
}

impl Declaration {
    /// Try to type check a declaration modifying the given scope.
    pub fn infer_type(&self, scope: &mut CrateScope) -> Result<()> {
        self.infer_type_first_pass(scope, Default::default())?;
        scope.infer_type_of_out_of_order_bindings()
    }

    fn infer_type_first_pass(&self, scope: &mut CrateScope, context: Context) -> Result<()> {
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
                        value: declaration.expression.clone().unwrap(),
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

                    // @Note could be done in the resolver once we unify the 2 CrateScopes
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
                            Context {
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
                    data: context.data.unwrap(),
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
impl Registration {
    fn evaluate(self, scope: &mut CrateScope) -> Result<()> {
        match self.clone() {
            Self::ValueBinding {
                binder,
                r#type,
                value,
            } => {
                let (r#type, value) = {
                    let function_scope = FunctionScope::Empty;
                    // @Note too conservative
                    handle_out_of_order_binding!(
                        scope,
                        self,
                        r#type.clone().is_a_type(&function_scope, scope)
                    );
                    let infered_type = handle_out_of_order_binding!(
                        scope,
                        self,
                        value.clone().infer_type(&function_scope, scope)
                    );
                    handle_out_of_order_binding!(
                        scope,
                        self,
                        r#type.is_actual(infered_type.clone(), &function_scope, scope,)
                    );
                    (infered_type, value)
                };
                scope.carry_out(Registration::ValueBinding {
                    binder,
                    r#type,
                    value,
                })?;
            }
            Self::DataBinding { binder, r#type } => {
                let r#type = r#type
                    // .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                    .evaluate(&FunctionScope::Empty, scope, Form::Normal)?;
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&FunctionScope::Empty, scope)
                );

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                types::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    r#type.clone(),
                    TYPE,
                )?;

                scope.carry_out(Registration::DataBinding { binder, r#type })?;
            }
            Self::ConstructorBinding {
                binder,
                r#type,
                data,
            } => {
                let r#type = r#type
                    // .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                    .evaluate(&FunctionScope::Empty, scope, Form::Normal)?;
                handle_out_of_order_binding!(
                    scope,
                    self,
                    r#type.clone().is_a_type(&FunctionScope::Empty, scope)
                );

                types::instance::assert_constructor_is_instance_of_type(
                    binder.clone(),
                    r#type.clone(),
                    data.clone().to_expression(),
                )?;

                scope.carry_out(Registration::ConstructorBinding {
                    binder,
                    r#type: r#type.clone(),
                    data,
                })?;
            }
            Self::ForeignValueBinding { binder, r#type } => {
                let r#type = {
                    let function_scope = FunctionScope::Empty;
                    let r#type = r#type
                        // .evaluate(&scope, Form::WeakHeadNormal)?;
                        .evaluate(&function_scope, scope, Form::Normal)?;
                    handle_out_of_order_binding!(
                        scope,
                        self,
                        r#type.clone().is_a_type(&function_scope, scope)
                    );
                    r#type
                };

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
                // @Task @Beacon add spans etc
                return Err(Diagnostic::new(Level::Fatal, None, "found infinite type"));
            }
        }

        Ok(())
    }
}

impl Expression {
    /// Try to infer the type of an expression.
    fn infer_type(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &mut CrateScope,
    ) -> Result<Self, Error> {
        use crate::interpreter::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope
                .lookup_type(&binding.binder, crate_scope)
                .ok_or(Error::Recoverable(OutOfOrderBinding))?,
            Type => TYPE,
            Nat(_) => crate_scope.lookup_foreign_type(ffi::Type::NAT, Some(self))?,
            Text(_) => crate_scope.lookup_foreign_type(ffi::Type::TEXT, Some(self))?,
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type`) has been normalized?
                literal.domain.clone().is_a_type(scope, crate_scope)?;

                if literal.parameter.is_some() {
                    literal.codomain.clone().is_a_type(
                        &mut scope.extend_with_parameter(literal.domain.clone()),
                        crate_scope,
                    )?;
                } else {
                    literal.codomain.clone().is_a_type(scope, crate_scope)?;
                }

                TYPE
            }
            Lambda(lambda) => {
                let parameter_type: Self = lambda
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;

                parameter_type.clone().is_a_type(scope, crate_scope)?;

                let scope = scope.extend_with_parameter(parameter_type.clone());
                let infered_body_type = lambda.body.clone().infer_type(&scope, crate_scope)?;

                if let Some(body_type_annotation) = lambda.body_type_annotation.clone() {
                    body_type_annotation
                        .clone()
                        .is_a_type(&scope, crate_scope)?;
                    body_type_annotation.is_actual(
                        infered_body_type.clone(),
                        &scope,
                        crate_scope,
                    )?;
                }

                expr! {
                    PiType[self.span] {
                        parameter: Some(lambda.parameter.clone()),
                        domain: parameter_type,
                        codomain: infered_body_type,
                        explicitness: Explicitness::Explicit,
                    }
                }
            }
            Application(application) => {
                // @Note this is an example where we normalize after an infer_type which means infer_type
                // returns possibly non-normalized expressions, can we do better?
                let type_of_callee = application
                    .callee
                    .clone()
                    .infer_type(scope, crate_scope)?
                    // .evaluate(scope, Form::WeakHeadNormal)?;
                    .evaluate(scope, crate_scope, Form::Normal)?;

                match &type_of_callee.kind {
                    PiType(pi) => {
                        let argument_type = application
                            .argument
                            .clone()
                            .infer_type(scope, crate_scope)?;
                        pi.domain
                            .clone()
                            .is_actual(argument_type, scope, crate_scope)?;

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
                .infer_type(scope, crate_scope)?,
            UseIn => todo!("1stP infer type of use/in"),
            // @Beacon @Beacon @Beacon @Temporary @Task
            // first: fiddeling, then: building abstractions
            // @Bug this is *not* principled design
            CaseAnalysis(_case_analysis) => {
                todo!("1stP infer type of case analysis") // @Task @Beacon

                // let r#type = case_analysis.subject.clone().infer_type(scope)?;
                // // @Task verify that
                // // * patterns are of correct type (i.e. r#type is an ADT and the constructors are the valid ones)
                // // * all constructors are covered
                // // * all case_analysis.cases>>.expressions are of the same type

                // match &r#type.kind {
                //     Binding(_) => {}
                //     Application(_application) => todo!("polymorphic types in patterns"),
                //     _ => todo!("encountered unsupported type to be case-analysed"),
                // };

                // use desugar::expression::Pattern;

                // let mut type_of_previous_body = None::<Self>;

                // for case in case_analysis.cases.iter() {
                //     match &case.pattern {
                //         Pattern::Nat(_) => todo!("nat literal patterns"),
                //         Pattern::Binding {
                //             binder,
                //             type_annotation,
                //         } => {
                //             if scope.is_constructor(&binder) {
                //                 let type_of_constructor = scope.lookup_type(&binder).unwrap();
                //                 if let Some(annotation) = type_annotation {
                //                     annotation
                //                         .clone()
                //                         .is_actual(type_of_constructor.clone(), scope)?;
                //                 }
                //                 // @Note error message very general, could be specialized to constructors
                //                 // once our diagnostic system is in place
                //                 r#type
                //                     .clone()
                //                     .is_actual(type_of_constructor.clone(), scope)?;
                //             } else {
                //                 todo!("bindings inside of patterns");
                //             }
                //             // todo!() // @Beacon @Beacon @Beacon @Task
                //         }
                //         Pattern::Application {
                //             callee: _,
                //             argument: _,
                //         } => todo!("application patterns"),
                //     }
                //     // @Task @Beacon insert bindings from pattern when type checking body
                //     let r#type = case.body.clone().infer_type(scope)?;

                //     match type_of_previous_body {
                //         Some(ref previous_type) => {
                //             previous_type.clone().is_actual(r#type, scope)?;
                //         }
                //         None => {
                //             type_of_previous_body = Some(r#type);
                //         }
                //     }
                // }

                // type_of_previous_body.unwrap_or_else(|| {
                //     let parameter = desugar::Identifier::sourced("A").refresh();

                //     expr! {
                //         PiType[self.span] {
                //             parameter: Some(parameter.clone()),
                //             domain: TYPE,
                //             codomain: expr! { Binding[self.span] { binder: parameter } },
                //             explicitness: Explicitness::Implicit,
                //         }
                //     }
                // })
            }
            ForeignApplication(_) => unreachable!(),
            Invalid => self,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn is_a_type(
        self,
        scope: &FunctionScope<'_>,
        crate_scope: &mut CrateScope,
    ) -> Result<(), Error> {
        let r#type = self.infer_type(scope, crate_scope)?;
        TYPE.is_actual(r#type, scope, crate_scope)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    // @Update this happens with Form::Normal, too. what a bummer
    fn is_actual(
        self,
        actual: Self,
        scope: &FunctionScope<'_>,
        crate_scope: &mut CrateScope,
    ) -> Result<(), Error> {
        // let expected = self.evaluate(scope, Form::WeakHeadNormal)?;
        // let actual = actual.evaluate(scope, Form::WeakHeadNormal)?;
        let expected = self.clone().evaluate(scope, crate_scope, Form::Normal)?;
        let actual = actual.evaluate(scope, crate_scope, Form::Normal)?;

        if !expected.clone().equals(actual.clone(), scope)? {
            // @Task improve diagnostic
            // @Task add span information
            // @Bug span information are *so* off!!
            // @Task we need to return a different error type (apart from Diagnostic) to
            // handle type mismatches without placing wrong spans
            Err(
                // Diagnostic::new(Level::Fatal, Code::E032, "mismatched types").with_labeled_span(
                //     actual.span,
                //     format!("expected `{}`, got `{}`", self, actual),
                // ),
                Error::Unrecoverable(Diagnostic::new(
                    Level::Fatal,
                    Code::E032,
                    format!("mismatched types. expected `{}`, got `{}`", self, actual),
                )),
            )
        } else {
            Ok(())
        }
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
