//! The type checker.

mod types;

use crate::{
    diagnostic::*,
    hir::{self, *},
    interpreter::{ffi, CrateScope, Form, FunctionScope},
    parser::{AttributeKind, Explicitness},
    resolver::Resolved,
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

impl Declaration {
    /// Try to type check a declaration modifying the given scope.
    pub fn infer_type(&self, scope: &mut CrateScope) -> Result<()> {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                if self.attributes.has(AttributeKind::Foreign) {
                    let r#type = {
                        let scope = FunctionScope::Module(scope);
                        let r#type = declaration
                            .type_annotation
                            .clone()
                            // .evaluate(&scope, Form::WeakHeadNormal)?;
                            .evaluate(&scope, Form::Normal)?;
                        r#type.clone().is_a_type(&scope)?;
                        r#type
                    };

                    scope.complete_foreign_binding(declaration.binder.clone(), r#type)?;
                } else {
                    let (r#type, expression) = {
                        let scope = FunctionScope::Module(scope);
                        let expression = declaration.expression.clone().unwrap();
                        declaration.type_annotation.clone().is_a_type(&scope)?;
                        let infered_type = expression.clone().infer_type(&scope)?;
                        declaration
                            .type_annotation
                            .clone()
                            .is_actual(infered_type.clone(), &scope)?;
                        (infered_type, expression)
                    };
                    scope.insert_value_binding(declaration.binder.clone(), r#type, expression);
                }
            }
            Data(data) => {
                let r#type = data
                    .type_annotation
                    .clone()
                    // .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                    .evaluate(&FunctionScope::Module(scope), Form::Normal)?;
                r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                types::instance::assert_constructor_is_instance_of_type(
                    data.binder.clone(),
                    r#type.clone(),
                    TYPE,
                    scope,
                )?;

                scope.insert_data_binding(data.binder.clone(), r#type);

                if self.attributes.has(AttributeKind::Foreign) {
                    scope.insert_foreign_data(&data.binder)?;
                } else {
                    let constructors = data.constructors.as_ref().unwrap();

                    // @Task move this logic into the `Constructor` match arm,
                    // @Note this only works if we are able to pass extra parameters,
                    // in our case `data.binder` (for the instance check)
                    for constructor in constructors {
                        let constructor = constructor.unwrap_constructor();

                        let r#type = constructor
                            .type_annotation
                            .clone()
                            // .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                            .evaluate(&FunctionScope::Module(scope), Form::Normal)?;
                        r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                        types::instance::assert_constructor_is_instance_of_type(
                            constructor.binder.clone(),
                            r#type.clone(),
                            expr! { Binding[self.span] { binder: data.binder.clone() } },
                            scope,
                        )?;

                        scope.insert_constructor_binding(
                            constructor.binder.clone(),
                            r#type.clone(),
                            &data.binder,
                        );
                    }

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
                }
            }
            // handled in the `Data` arm
            Constructor(_) => unreachable!(),
            Module(module) => {
                for declaration in &module.declarations {
                    declaration.infer_type(scope)?;
                }
            }
            Use(_) => {}
        }

        Ok(())
    }
}

impl Expression {
    /// Try to infer the type of an expression.
    pub fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self> {
        use crate::interpreter::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope.lookup_type(&binding.binder).ok_or_else(|| {
                Diagnostic::new(
                    Level::Bug,
                    None,
                    "out-of-order declarations not supported yet",
                )
                .with_span(&binding.binder)
            })?,
            Type => TYPE,
            Nat(_) => scope
                .module()
                .lookup_foreign_type(ffi::Type::NAT, Some(self))?,
            Text(_) => scope
                .module()
                .lookup_foreign_type(ffi::Type::TEXT, Some(self))?,
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type`) has been normalized?
                literal.domain.clone().is_a_type(scope)?;

                if literal.parameter.is_some() {
                    literal
                        .codomain
                        .clone()
                        .is_a_type(&scope.extend_with_parameter(literal.domain.clone()))?;
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

                let scope = &scope.extend_with_parameter(parameter_type.clone());
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
                    .infer_type(scope)?
                    // .evaluate(scope, Form::WeakHeadNormal)?;
                    .evaluate(scope, Form::Normal)?;

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
                        return Err(Diagnostic::new(
                            Level::Fatal,
                            Code::E031,
                            format!(
                                "cannot apply `{}` to a `{}`",
                                application.argument, type_of_callee
                            ),
                        ));
                    }
                }
            }
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .infer_type(scope)?,
            UseIn => todo!("infer type of use/in"),
            // @Beacon @Beacon @Beacon @Temporary @Task
            // first: fiddeling, then: building abstractions
            // @Bug this is *not* principled design
            CaseAnalysis(_case_analysis) => {
                todo!("infer type of case analysis") // @Task @Beacon

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
        })
    }

    /// Assert that an expression is of type `Type`.
    fn is_a_type(self, scope: &FunctionScope<'_>) -> Result<()> {
        let r#type = self.infer_type(scope)?;
        TYPE.is_actual(r#type, scope)
    }
}
