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
//! * case analysis not implemented
//! * order-independent declarations and recursion not implemented
//! * modules not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * integration and regression tests missing

// @Beacon @Task order-independence and recursion

mod data_types;
mod scope;

use crate::{
    diagnostic::{Code, Diagnostic, Level, Result},
    hir::{self, *},
    parser::{AttributeKind, Explicitness},
    resolver::Identifier,
};
use scope::FunctionScope;
pub use scope::ModuleScope;

type Declaration = hir::Declaration<Identifier>;
type Expression = hir::Expression<Identifier>;

const NAT_TYPE_NAME: &str = "Nat";
const TEXT_TYPE_NAME: &str = "Text";

fn missing_annotation() -> Diagnostic {
    // @Task add span
    Diagnostic::new(
        Level::Bug,
        Code::E030,
        "currently lambda literal parameters and patterns must be type-annotated",
    )
}

impl Declaration {
    /// Try to type check and evaluate a declaration modifying the given scope.
    // @Task move evaluation logic
    pub fn infer_type_and_evaluate(&self, scope: &mut ModuleScope) -> Result<()> {
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

                    scope.insert_type_for_foreign_binding(declaration.binder.clone(), r#type)?;
                } else {
                    let (r#type, value) = {
                        let scope = FunctionScope::Module(scope);
                        let expression = declaration.expression.clone().unwrap();
                        let infered_type = expression
                            .clone()
                            .matches_type_annotation(declaration.type_annotation.clone(), &scope)?;
                        let value = expression.clone().evaluate(&scope, Form::WeakHeadNormal)?;
                        // .evaluate(&scope, Form::Normal)?;
                        (infered_type, value)
                    };
                    scope.insert_value_binding(declaration.binder.clone(), r#type, value);
                }
            }
            Data(data) => {
                let r#type = data
                    .type_annotation
                    .clone()
                    .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                // .evaluate(&FunctionScope::Module(scope), Form::Normal)?;
                r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                // @Task diagnostic note: only `Type` can be extended
                // @Note currently is: invalid constructor X
                data_types::instance::assert_constructor_is_instance_of_type(
                    data.binder.clone(),
                    r#type.clone(),
                    expr! { Type[] },
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
                        let constructor = constructor.constructor().unwrap();

                        let r#type = constructor
                            .type_annotation
                            .clone()
                            .evaluate(&FunctionScope::Module(scope), Form::WeakHeadNormal)?;
                        // .evaluate(&FunctionScope::Module(scope), Form::Normal)?;
                        r#type.clone().is_a_type(&FunctionScope::Module(scope))?;

                        data_types::instance::assert_constructor_is_instance_of_type(
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
                }
            }
            // handled in the `Data` arm
            Constructor(_) => unreachable!(),
            Module(module) => {
                for declaration in &module.declarations {
                    declaration.infer_type_and_evaluate(scope)?;
                }
            }
            Use => todo!("infer type of use declaration"),
        }

        Ok(())
    }

    // @Task
    pub fn evaluate(&self, _scope: &mut ModuleScope) -> Result<()> {
        todo!()
    }
}

#[derive(Clone, Copy)]
pub enum Form {
    Normal,
    WeakHeadNormal,
}

impl Expression {
    fn substitute(self, substitution: Substitution) -> Self {
        use self::Substitution::*;
        use ExpressionKind::*;

        match (&self.kind, substitution) {
            (Binding(binding), Shift(amount)) => {
                expr! { Binding[self.span] { binder: binding.binder.clone().shift(amount) } }
            }
            (Binding(binding), Use(substitution, expression)) => {
                if binding.binder.is_innermost() {
                    expression.substitute(Shift(0))
                } else {
                    {
                        (expr! { Binding[self.span] { binder: binding.binder.clone().unshift() } })
                            .substitute(*substitution)
                    }
                }
            }
            (Substitution(substitution0), substitution1) => substitution0
                .expression
                .clone()
                .substitute(substitution0.substitution.clone())
                .substitute(substitution1),
            (Type, _) | (Nat(_), _) | (Text(_), _) => self,
            (Application(application), substitution) => {
                expr! {
                    Application[self.span] {
                        callee: expr! {
                            Substitution[] {
                                expression: application.callee.clone(),
                                substitution: substitution.clone(),
                            }
                        },
                        argument: expr! {
                            Substitution[] {
                                expression: application.argument.clone(),
                                substitution,
                            }
                        },
                        explicitness: application.explicitness,
                    }
                }
            }
            (PiType(pi), substitution) => {
                let domain = expr! {
                    Substitution[] {
                        expression: pi.domain.clone(),
                        substitution: substitution.clone(),
                    }
                };

                let codomain = expr! {
                    Substitution[] {
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
                            Substitution[] {
                                expression: r#type,
                                substitution: substitution.clone(),
                            }
                        }
                    });

                let body_type_annotation = lambda.body_type_annotation.clone().map(|r#type| {
                    expr! {
                        Substitution[] {
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
                    Substitution[] {
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
            (CaseAnalysis(_), _) => todo!("substitute case analysis"),
            (UseIn, _) => todo!("substitute use/in"),
            (UnsaturatedForeignApplication(_), _) => todo!("substitute foreign application"),
        }
    }

    /// Try to infer the type of an expression.
    pub fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope.lookup_type(&binding.binder),
            Type => {
                expr! { Type[] }
            }
            Nat(_) => scope.module().lookup_foreign_data(NAT_TYPE_NAME, self)?,
            Text(_) => scope.module().lookup_foreign_data(TEXT_TYPE_NAME, self)?,
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

                expr! { Type[] }
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
                    .evaluate(scope, Form::WeakHeadNormal)?;
                // .evaluate(scope, Form::Normal)?;

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
                //             domain: expr! { Type[] },
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
            Binding(binding) => match scope.lookup_value(&binding.binder) {
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
                        Substitution[] {
                            // @Note could very well be a module index (the argument)
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
            Type | Nat(_) | Text(_) => self,
            PiType(pi) => match form {
                Form::Normal => {
                    let domain = pi.domain.clone().evaluate(scope, form)?;

                    let codomain = if pi.parameter.is_some() {
                        pi.codomain
                            .clone()
                            .evaluate(&scope.extend_with_parameter(domain.clone()), form)?
                    } else {
                        pi.codomain.clone()
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
                        .ok_or_else(missing_annotation)?
                        .evaluate(scope, form)?;
                    let body_type = lambda
                        .body_type_annotation
                        .clone()
                        .map(|r#type| {
                            r#type.evaluate(
                                &scope.extend_with_parameter(parameter_type.clone()),
                                form,
                            )
                        })
                        .transpose()?;
                    let body = lambda
                        .body
                        .clone()
                        .evaluate(&scope.extend_with_parameter(parameter_type.clone()), form)?;

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
            UseIn => todo!("evaluate use/in"),
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
                todo!("evaluate case analysis") // @Task @Beacon

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
        (expr! { Type[] }).is_actual(r#type, scope)
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
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    fn is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<()> {
        // let expected = self.evaluate(scope, Form::WeakHeadNormal)?;
        // let actual = actual.evaluate(scope, Form::WeakHeadNormal)?;
        let expected = self.evaluate(scope, Form::Normal)?;
        let actual = actual.evaluate(scope, Form::Normal)?;

        if !expected.clone().equals(actual.clone(), scope)? {
            // @Task improve diagnostic
            // @Task add span information
            Err(Diagnostic::new(
                Level::Fatal,
                Code::E032,
                format!("expected `{}` got `{}`", expected, actual),
            ))
        } else {
            Ok(())
        }
    }

    /// Dictates if two expressions are alpha-equivalent.
    fn equals(self, other: Self, scope: &FunctionScope<'_>) -> Result<bool> {
        use ExpressionKind::*;

        Ok(match (self.kind, other.kind) {
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
            (Type, Type) => true,
            (Nat(nat0), Nat(nat1)) => nat0.value == nat1.value,
            (Text(text0), Text(text1)) => text0.value == text1.value,
            // @Question what about explicitness?
            (PiType(pi0), PiType(pi1)) => {
                pi0.domain.clone().equals(pi1.domain.clone(), scope)?
                    && match (pi0.parameter.clone(), pi1.parameter.clone()) {
                        // @Task @Beacon verify
                        (Some(_), Some(_parameter1)) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (Some(_), None) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (None, Some(_)) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(pi1.domain.clone()),
                        )?,
                        (None, None) => pi0.codomain.clone().equals(pi1.codomain.clone(), scope)?,
                    }
            }
            // @Question what about the body_type_annotation? what about explicitness?
            (Lambda(lambda0), Lambda(lambda1)) => {
                let parameter_type_annotation0 = lambda0
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;
                let parameter_type_annotation1 = lambda1
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(missing_annotation)?;

                parameter_type_annotation0
                    .clone()
                    .equals(parameter_type_annotation1, scope)?
                    && lambda0.body.clone().equals(
                        lambda1.body.clone(),
                        &scope.extend_with_parameter(parameter_type_annotation0),
                    )?
            }
            (CaseAnalysis(_), CaseAnalysis(_)) => unreachable!(),
            _ => false,
        })
    }
}

#[derive(Clone)]
pub enum Substitution {
    Shift(usize),
    Use(Box<Substitution>, Expression),
}

impl Substitution {
    fn compose(self, other: Self) -> Self {
        use self::Substitution::*;
        match (self, other) {
            (substitution0, Shift(0)) => substitution0,
            (Use(substitution, _), Shift(amount)) => substitution.compose(Shift(amount - 1)),
            (Shift(amount0), Shift(amount1)) => Shift(amount0 + amount1),
            (substitution0, Use(substitution1, expression)) => Use(
                Box::new(substitution0.clone().compose(*substitution1)),
                expr! {
                    Substitution[] {
                        substitution: substitution0,
                        expression,
                    }
                },
            ),
        }
    }
}

use std::fmt;

impl fmt::Display for Substitution {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Substitution::*;
        match self {
            Shift(amount) => write!(f, "shift {}", amount),
            Use(substitution, expression) => write!(f, "{}[{}]", expression, substitution),
        }
    }
}
