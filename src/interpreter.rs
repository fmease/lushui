//! The type checker and tree-walk interpreter.
//!
//! The first backend of lushuic. Later, we are going to add a bytecode interpreter for
//! evaluation. Still, this interpreter will stay because it is necessary for type-checking.
//!
//! The plan is to somehow split this file into `typing.rs` and `backend/tree_walk_interpreter.rs`
//! with the module `typing` referencing the moved interpreter.
//!
//! This module is **heavily** under construction!
//!
//! ## Issues
//!
//! * too many bugs
//! * using substitution environments instead of locally nameless/Debruijn-indeces
//! * case analysis not implemented
//! * order-independent declarations and recursion not implemented
//! * modules not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * bad unstructured error reporting without span information
//! * bad API/project structure could be better
//! * integration and regression tests missing

mod data_types;
mod error;
mod scope;

use crate::{
    desugar::{self, expr, Declaration, DeclarationKind, Expression, ExpressionKind},
    parser::Explicitness,
    resolver::Identifier,
    span::Span,
};
use error::{Error, Result};
use scope::FunctionScope;
pub use scope::ModuleScope;

/// An internal compiler bug message.
///
/// We don't do non-trivial type inference yet and thus, type parameters of lambda literals
/// must be annotated with a type. And since we don't have a dedicated `Error::InternalCompilerError` yet,
/// we use this constant for an error message.
// @Bug
const MISSING_ANNOTATION: &str =
    "currently lambda literal parameters and patterns must be type-annotated";

impl Declaration<Identifier> {
    // @Task move type inference logic from Self::evaluate here
    pub fn infer_type(&self, _scope: &mut ModuleScope) -> Result<()> {
        todo!()
    }

    /// Try to type check and evaluate a declaration modifying the given scope.
    // @Task support order-independence, recursion and proper modules
    pub fn evaluate(&self, scope: &mut ModuleScope) -> Result<()> {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                let (infered_type, value) = {
                    let scope = FunctionScope::Module(scope);
                    let infered_type = declaration
                        .expression
                        .clone()
                        .matches_type_annotation(declaration.type_annotation.clone(), &scope)?;
                    let value = declaration
                        .expression
                        .clone()
                        .evaluate(&scope, Form::WeakHeadNormal)?;
                    (infered_type, value)
                };
                scope.insert_value_binding(declaration.binder.clone(), infered_type, value);
            }
            Data(data) => {
                let r#type = data
                    .type_annotation
                    .clone()
                    .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                data_types::instance::assert_constructor_is_instance_of_type(
                    data.binder.clone(),
                    r#type.clone(),
                    expr! { Type[Span::dummy()] },
                    scope,
                )?;

                scope
                    .clone()
                    .insert_data_binding(data.binder.clone(), r#type);

                for desugar::Constructor {
                    binder,
                    type_annotation,
                    span: _,
                } in &data.constructors
                {
                    let r#type = type_annotation
                        .clone()
                        .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                    r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                    data_types::instance::assert_constructor_is_instance_of_type(
                        binder.clone(),
                        r#type.clone(),
                        expr! { Binding[self.span] { binder: data.binder.clone() } },
                        scope,
                    )?;

                    scope.insert_constructor_binding(binder.clone(), r#type.clone(), &data.binder);
                }
            }
            Module(module) => {
                for declaration in &module.declarations {
                    declaration.evaluate(scope)?;
                }
            }
            Use => todo!(),
            Foreign(foreign) => {
                let r#type = {
                    let scope = FunctionScope::Module(scope);
                    let r#type = foreign
                        .type_annotation
                        .clone()
                        .evaluate(&scope, Form::WeakHeadNormal)?;
                    r#type.clone().is_a_type(&scope)?;
                    r#type
                };

                scope.insert_type_for_foreign_binding(foreign.binder.clone(), r#type);
            }
        }

        Ok(())
    }
}

#[derive(Clone, Copy)]
pub enum Form {
    Normal,
    WeakHeadNormal,
}

impl Expression<Identifier> {
    fn substitute(self, substitution: Substitution) -> Self {
        use self::Substitution::*;
        use ExpressionKind::*;

        match (&self.kind, substitution) {
            (Binding(binding), Shift(amount)) => {
                expr! { Binding[self.span] { binder: binding.binder.clone().shift(amount) } }
            }
            (Binding(binding), Use(substitution, _)) => {
                (expr! { Binding[self.span] { binder: binding.binder.clone().unshift() } })
                    .substitute(*substitution)
            }
            (Substitution(substitution0), substitution1) => substitution0
                .expression
                .clone()
                .substitute(substitution0.substitution.clone())
                .substitute(substitution1),
            (Type, _) | (NatType, _) | (TextType, _) | (Nat(_), _) | (Text(_), _) => self,
            (Application(application), substitution) => expr! {
                Application[self.span] {
                    callee: expr! {
                        Substitution[Span::dummy()] {
                            expression: application.callee.clone(),
                            substitution: substitution.clone(),
                        }
                    },
                    argument: expr! {
                        Substitution[Span::dummy()] {
                            expression: application.argument.clone(),
                            substitution,
                        }
                    },
                    explicitness: application.explicitness,
                }
            },
            (PiType(pi), substitution) => {
                let domain = expr! {
                    Substitution[Span::dummy()] {
                        expression: pi.domain.clone(),
                        substitution: substitution.clone(),
                    }
                };

                let codomain = expr! {
                    Substitution[Span::dummy()] {
                        expression: pi.codomain.clone(),
                        substitution: match &pi.parameter {
                            Some(parameter) => {
                                let binder = Identifier::local(parameter);

                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding[binder.source.span] { binder } }
                                )
                            }
                            None => substitution,
                        },
                    }
                };

                expr! {
                    PiType[self.span] {
                        parameter: pi.parameter.clone(),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                    }
                }
            }
            (Lambda(lambda), substitution) => {
                let parameter_type_annotation =
                    lambda.parameter_type_annotation.clone().map(|r#type| {
                        expr! {
                            Substitution[Span::dummy()] {
                                expression: r#type,
                                substitution: substitution.clone(),
                            }
                        }
                    });

                let body_type_annotation = lambda.body_type_annotation.clone().map(|r#type| {
                    expr! {
                        Substitution[Span::dummy()] {
                            expression: r#type,
                            substitution: {
                                let binder = Identifier::local(&lambda.parameter);
                                Use(
                                    Box::new(Shift(1).compose(substitution.clone())),
                                    expr! { Binding[binder.source.span] { binder } }
                                )
                            }
                        }
                    }
                });

                let body = expr! {
                    Substitution[Span::dummy()] {
                        expression: lambda.body.clone(),
                        substitution: {
                                let binder = Identifier::local(&lambda.parameter);

                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding[binder.source.span] { binder } }
                                )
                        },
                    }
                };

                expr! {
                    Lambda[self.span] {
                        parameter: lambda.parameter.clone(),
                        parameter_type_annotation,
                        body_type_annotation,
                        body,
                        explicitness: lambda.explicitness,
                    }
                }
            }
            (CaseAnalysis(_), _) => todo!(),
            (UseIn, _) => todo!(),
            (UnsaturatedForeignApplication(_), _) => todo!(),
        }
    }

    /// Try to infer the type of an expression.
    pub fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope
                .lookup_type(&binding.binder)
                .ok_or_else(|| Error::UndefinedBinding(binding.binder.clone()))?,
            Type | NatType | TextType => {
                expr! { Type[Span::dummy()] }
            }
            Nat(_) => expr! { NatType[Span::dummy()] },
            Text(_) => expr! { TextType[Span::dummy()] },
            PiType(literal) => {
                // ensure domain and codomain are are well-typed
                // @Question why do we need to this? shouldn't this be already handled if
                // `expression` (parameter of `infer_type`) has been normalized?
                literal.domain.clone().is_a_type(scope)?;

                if let Some(parameter) = literal.parameter.clone() {
                    literal.codomain.clone().is_a_type(
                        &scope.extend_with_parameter(parameter, literal.domain.clone()),
                    )?;
                } else {
                    literal.codomain.clone().is_a_type(scope)?;
                }

                expr! { Type[Span::dummy()] }
            }
            Lambda(literal) => {
                let parameter_type: Self = literal
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION);

                parameter_type.clone().is_a_type(scope)?;

                let scope =
                    &scope.extend_with_parameter(literal.parameter.clone(), parameter_type.clone());
                let infered_body_type = literal.body.clone().infer_type(&scope)?;

                if let Some(body_type_annotation) = literal.body_type_annotation.clone() {
                    body_type_annotation.clone().is_a_type(&scope)?;
                    body_type_annotation.is_actual(infered_body_type.clone(), &scope)?;
                }

                expr! {
                    PiType[self.span] {
                        parameter: Some(literal.parameter.clone()),
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
                    .evaluate(scope, Form::WeakHeadNormal)?;

                match type_of_callee.kind {
                    PiType(pi) => {
                        let argument_type = application.argument.clone().infer_type(scope)?;
                        pi.domain.clone().is_actual(argument_type, scope)?;

                        // @Task verify
                        match pi.parameter.clone() {
                            Some(_) => expr! {
                                Substitution[Span::dummy()] {
                                    substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                                    expression: pi.codomain.clone(),
                                }
                            },
                            None => pi.codomain.clone(),
                        }
                    }
                    _ => {
                        return Err(Error::FunctionExpected {
                            actual: type_of_callee,
                            argument: application.argument.clone(),
                        });
                    }
                }
            }
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .infer_type(scope)?,
            UseIn => todo!(),
            // @Beacon @Beacon @Beacon @Temporary @Task
            // first: fiddeling, then: building abstractions
            // @Bug this is *not* principled design
            CaseAnalysis(_case_analysis) => {
                todo!() // @Task @Beacon

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
                //             domain: expr! { Type[Span::dummy()] },
                //             codomain: expr! { Binding[self.span] { binder: parameter } },
                //             explicitness: Explicitness::Implicit,
                //         }
                //     }
                // })
            }
            UnsaturatedForeignApplication(_) => todo!(),
        })
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    // @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
    pub fn evaluate(self, scope: &FunctionScope<'_>, form: Form) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.clone().kind {
            // @Task if path refers to a foreign binding AND arity == 0, then resolve the foreign binding
            Binding(binding) => match scope
                .lookup_value(&binding.binder)
                // @Question @Beacon @Beacon will this ever happen after type checking? I don't think so
                .ok_or_else(|| Error::UndefinedBinding(binding.binder.clone()))?
            {
                // @Question is this normalization necessary? I mean, yes, we got a new scope,
                // but the thing in the previous was already normalized (well, it should have been
                // at least). I guess it is necessary because it can contain parameters which could not
                // be resolved yet but potentially can be now.
                scope::Value::Reducible(expression) => expression.evaluate(scope, form)?,
                scope::Value::Neutral => self,
            },
            Application(application) => {
                let callee = application.callee.clone().evaluate(scope, form)?;
                match callee.kind {
                    Lambda(lambda) => (expr! {
                        Substitution[Span::dummy()] {
                            substitution: Use(Box::new(Shift(0)), application.argument.clone()),
                            expression: lambda.body.clone(),
                        }
                    })
                    .evaluate(scope, form)?,
                    Binding(_) | Application(_) => {
                        expr! {
                            Application[self.span] {
                                // @Question or application.callee (unevaluated)?
                                callee,
                                argument: match form {
                                    Form::Normal => application.argument.clone().evaluate(scope, form)?,
                                    Form::WeakHeadNormal => application.argument.clone(),
                                },
                                explicitness: Explicitness::Explicit,
                            }
                        }
                    }
                    // @Bug currentky not checked that it does not contain any parameters/is a non-value/is not
                    // FFI-compatible
                    // Binding(path) if scope.is_foreign(&binding) => scope
                    //     .try_apply_foreign_binding(dbg!(&binding), {
                    //         let mut arguments = VecDeque::with_capacity(1);
                    //         arguments.push_back(argument);
                    //         arguments
                    //     })?,
                    // UnsaturatedForeignApplication(application) => {
                    //     scope.try_apply_foreign_binding(&application.callee, {
                    //         let mut arguments = application.arguments.clone();
                    //         arguments.push_back(argument);
                    //         arguments
                    //     })?
                    // }
                    _ => unreachable!(),
                }
            }
            Type | NatType | Nat(_) | TextType | Text(_) => self,
            PiType(pi) => match form {
                Form::Normal => {
                    let domain = pi.domain.clone().evaluate(scope, form)?;

                    // @Task verify
                    let codomain = match &pi.parameter {
                        Some(parameter) => pi.codomain.clone().evaluate(
                            &scope.extend_with_parameter(parameter.clone(), domain.clone()),
                            form,
                        )?,
                        None => pi.codomain.clone(),
                    };

                    expr! {
                        PiType[self.span] {
                            parameter: pi.parameter.clone(),
                            domain,
                            codomain,
                            explicitness: Explicitness::Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => self,
            },
            Lambda(lambda) => match form {
                Form::Normal => {
                    let parameter_type = lambda
                        .parameter_type_annotation
                        .clone()
                        .expect(MISSING_ANNOTATION)
                        .evaluate(scope, form)?;
                    let body = lambda.body.clone().evaluate(
                        &scope.extend_with_parameter(
                            lambda.parameter.clone(),
                            parameter_type.clone(),
                        ),
                        form,
                    )?;
                    let body_type = lambda
                        .body_type_annotation
                        .clone()
                        .map(|r#type| {
                            r#type.evaluate(
                                &scope.extend_with_parameter(
                                    lambda.parameter.clone(),
                                    parameter_type.clone(),
                                ),
                                form,
                            )
                        })
                        .transpose()?;

                    expr! {
                        Lambda[self.span] {
                            parameter: lambda.parameter.clone(),
                            parameter_type_annotation: Some(parameter_type),
                            body,
                            body_type_annotation: body_type,
                            explicitness: Explicitness::Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => self,
            },
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .evaluate(scope, form)?,
            UseIn => todo!(),
            // @Note @Beacon, now, meta information would be nice, so we don't need to do
            // double work (the work of `infer_type` again)
            // @Beacon @Beacon @Beacon @Note this code is @Temporary as hell.
            // I just need to implement enough of it till I start building good
            // abstractions
            // @Note partially applied constructors differ from normal values
            // I guess it's very likely that the first code we write will handle them incorrectly
            // because the code will not check for the arity of the neutral application
            // @Note how to handle them: just like functions: case analysis only wotks with a binder-case
            // (default case)
            CaseAnalysis(_case_analysis) => {
                todo!() // @Task @Beacon

                // use desugar::expression::Pattern;

                // let subject = case_analysis.subject.clone().evaluate(scope, form)?;

                // // @Note we assume, subject is composed of only applications, paths
                // // and natural number literals corresponding to the pattern types we
                // // want to support right now
                // // everything else should be impossible because of type checking
                // // but I might be wrong. possible counter examples: unevaluated case
                // // analysis expressions etc
                // match subject.kind {
                //     Binding(subject) => {
                //         // @Beacon @Beacon @Task
                //         if scope.is_constructor(&subject) {
                //             for case in case_analysis.cases.iter() {
                //                 match &case.pattern {
                //                     Pattern::Nat(_) => todo!(),
                //                     Pattern::Binding { binder, .. } => {
                //                         if binder == subject {
                //                             // @Task @Beacon extend with parameters when evaluating
                //                             return case.body.clone().evaluate(scope, form);
                //                         }
                //                     }
                //                     Pattern::Application { .. } => todo!(),
                //                 }
                //             }
                //             // we should not be here
                //             // @Note this is currently reachable because we don't do a check for
                //             // exhaustiveness in `infer_type`, just fyi
                //             unreachable!()
                //         } else {
                //             self
                //         }
                //     }
                //     Application(_application) => todo!(),
                //     Nat(_literal) => todo!(),
                //     // @Note reachable if they contain neutrals, right??
                //     _ => unreachable!(),
                // }
            }
            // @Question or should the argument be normalized *again*? (under a possibly new scope?)
            UnsaturatedForeignApplication(_) => self,
        })
    }

    /// Assert that an expression is of type `Type`.
    fn is_a_type(self, scope: &FunctionScope<'_>) -> Result<()> {
        let r#type = self.infer_type(scope)?;
        (expr! { Type[Span::dummy()] }).is_actual(r#type, scope)
    }

    /// Infer type of expression and match it with the type annotation.
    ///
    /// Returns infered type and for convenience also checks if the supplied type annotation is actually a type.
    fn matches_type_annotation(
        self,
        type_annotation: Self,
        scope: &FunctionScope<'_>,
    ) -> Result<Self> {
        type_annotation.clone().is_a_type(scope)?;
        let infered_type = self.infer_type(scope)?;
        type_annotation.is_actual(infered_type.clone(), scope)?;

        Ok(infered_type)
    }

    /// Assert that two expression are equal under evaluation/normalization.
    fn is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<()> {
        if !self.clone().equals(actual.clone(), scope)? {
            Err(Error::ExpressionsNotEqual {
                expected: self,
                actual,
            })
        } else {
            Ok(())
        }
    }

    /// Dictates if two expressions are alpha-equivalent.
    fn equals(self, other: Self, scope: &FunctionScope<'_>) -> Result<bool> {
        use ExpressionKind::*;

        let expression0 = self.evaluate(scope, Form::WeakHeadNormal)?;
        let expression1 = other.evaluate(scope, Form::WeakHeadNormal)?;

        Ok(match (expression0.kind, expression1.kind) {
            (Binding(binding0), Binding(binding1)) => binding0.binder == binding1.binder,
            (Application(application0), Application(application1)) => {
                application0
                    .callee
                    .clone()
                    .equals(application1.callee.clone(), scope)?
                    && application0
                        .argument
                        .clone()
                        .equals(application1.argument.clone(), scope)?
            }
            (Type, Type) | (NatType, NatType) | (TextType, TextType) => true,
            (Nat(nat0), Nat(nat1)) => nat0.value == nat1.value,
            (Text(text0), Text(text1)) => text0.value == text1.value,
            // @Question what about explicitness?
            (PiType(pi0), PiType(pi1)) => {
                pi0.domain.clone().equals(pi1.domain.clone(), scope)?
                    && match (pi0.parameter.clone(), pi1.parameter.clone()) {
                        // @Task @Beacon verify
                        (Some(parameter0), Some(_parameter1)) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(parameter0, pi0.domain.clone()),
                        )?,
                        (Some(parameter), None) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(parameter, pi0.domain.clone()),
                        )?,
                        (None, Some(parameter)) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(parameter, pi1.domain.clone()),
                        )?,
                        (None, None) => pi0.codomain.clone().equals(pi1.codomain.clone(), scope)?,
                    }
            }
            // @Question what about the body_type_annotation? what about explicitness?
            (Lambda(lambda0), Lambda(lambda1)) => {
                let parameter_type_annotation0 = lambda0
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION);
                let parameter_type_annotation1 = lambda1
                    .parameter_type_annotation
                    .clone()
                    .expect(MISSING_ANNOTATION);

                parameter_type_annotation0
                    .clone()
                    .equals(parameter_type_annotation1, scope)?
                    && lambda0.body.clone().equals(
                        lambda1.body.clone(),
                        &scope.extend_with_parameter(
                            lambda0.parameter.clone(),
                            parameter_type_annotation0,
                        ),
                    )?
            }
            (CaseAnalysis(_), CaseAnalysis(_)) => unreachable!(),
            _ => false,
        })
    }
}

#[derive(Clone, Debug)]
pub enum Substitution {
    Shift(usize),
    Use(Box<Substitution>, Expression<Identifier>),
}

impl Substitution {
    fn compose(self, other: Self) -> Self {
        use Substitution::*;
        match (self, other) {
            (substitution0, Shift(0)) => substitution0,
            (Use(substitution, _), Shift(amount)) => substitution.compose(Shift(amount - 1)),
            (Shift(amount0), Shift(amount1)) => Shift(amount0 + amount1),
            (substitution0, Use(substitution1, expression)) => Use(
                Box::new(substitution0.clone().compose(*substitution1)),
                expr! {
                    Substitution[Span::dummy()] {
                        substitution: substitution0,
                        expression,
                    }
                },
            ),
        }
    }
}
