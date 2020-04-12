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
mod ffi;
mod scope;

use crate::{
    diagnostic::*,
    hir::{self, *},
    parser::{AttributeKind, Explicitness},
    resolver::Identifier,
};
use scope::FunctionScope;
pub use scope::ModuleScope;

type Declaration = hir::Declaration<Identifier>;
type Expression = hir::Expression<Identifier>;

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

                    scope.complete_foreign_binding(declaration.binder.clone(), r#type)?;
                } else {
                    let (r#type, value) = {
                        let scope = FunctionScope::Module(scope);
                        let expression = declaration.expression.clone().unwrap();
                        declaration.type_annotation.clone().is_a_type(&scope)?;
                        let infered_type = expression.clone().infer_type(&scope)?;
                        declaration
                            .type_annotation
                            .clone()
                            .is_actual(infered_type.clone(), &scope)?;
                        // let value = expression.clone().evaluate(&scope, Form::WeakHeadNormal)?;
                        let value = expression.clone().evaluate(&scope, Form::Normal)?;
                        (infered_type, value)
                    };
                    scope.insert_value_binding(declaration.binder.clone(), r#type, value);
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
                data_types::instance::assert_constructor_is_instance_of_type(
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

                    // @Task move into ffi module @Beacon @Beacon
                    if let Some(inherent) = self.attributes.get(AttributeKind::Inherent) {
                        // @Task link to previous definition
                        let duplicate = || {
                            Diagnostic::new(
                                Level::Fatal,
                                Code::E020,
                                format!("`{}` is defined multiple times as inherent", data.binder),
                            )
                            .with_span(self.span)
                        };

                        let find = |value_name, inherent: &mut Option<_>| {
                            if let Some(constructor) = constructors
                                .iter()
                                .map(|constructor| constructor.unwrap_constructor())
                                .find(|constructor| &constructor.binder.source.atom == value_name)
                            {
                                *inherent = Some(constructor.binder.clone().dummified());
                            }
                        };

                        match &*data.binder.source.atom {
                            ffi::Type::UNIT => {
                                if scope.inherent_types.unit.is_some() {
                                    return Err(duplicate());
                                }

                                scope.inherent_types.unit = Some(data.binder.clone().dummified());
                                find(ffi::Value::UNIT, &mut scope.inherent_values.unit);
                            }
                            ffi::Type::BOOL => {
                                if scope.inherent_types.bool.is_some() {
                                    return Err(duplicate());
                                }

                                scope.inherent_types.bool = Some(data.binder.clone().dummified());
                                find(ffi::Value::FALSE, &mut scope.inherent_values.r#false);
                                find(ffi::Value::TRUE, &mut scope.inherent_values.r#true);
                            }
                            ffi::Type::OPTION => {
                                if scope.inherent_types.option.is_some() {
                                    return Err(duplicate());
                                }

                                scope.inherent_types.option = Some(data.binder.clone().dummified());
                                find(ffi::Value::NONE, &mut scope.inherent_values.none);
                                find(ffi::Value::SOME, &mut scope.inherent_values.some);
                            }
                            _ => {
                                return Err(Diagnostic::new(
                                    Level::Fatal,
                                    Code::E062,
                                    format!("`{}` is not an inherent type", data.binder),
                                )
                                .with_span(inherent.span)
                                .with_labeled_span(self.span, "ascribed to this declaration"))
                            }
                        }
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

const TYPE: Expression = expr! { Type[] };

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
            (ForeignApplication(application), substitution) => expr! {
                ForeignApplication[self.span] {
                    callee: application.callee.clone(),
                    arguments: application.arguments.iter().map(|argument| expr! {
                        Substitution[argument.span] {
                            expression: argument.clone(),
                            substitution: substitution.clone(),
                        }
                    }).collect()
                }
            },
        }
    }

    /// Try to infer the type of an expression.
    pub fn infer_type(self, scope: &FunctionScope<'_>) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        Ok(match self.kind {
            Binding(binding) => scope.lookup_type(&binding.binder),
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

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    // @Task differenciate between Expression<InitialPhase> and Expression<Normalized>
    pub fn evaluate(self, scope: &FunctionScope<'_>, form: Form) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        // @Bug we currently don't support zero-arity foreign functions
        Ok(match self.clone().kind {
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
                let argument = application.argument.clone();
                match callee.kind {
                    Lambda(lambda) => (expr! {
                        Substitution[] {
                            substitution: Use(Box::new(Shift(0)), argument),
                            expression: lambda.body.clone(),
                        }
                    })
                    .evaluate(scope, form)?,
                    Binding(binding) if scope.is_foreign(&binding.binder) => (expr! {
                        ForeignApplication[self.span] {
                            callee: binding.binder.clone(),
                            arguments: extended(Vec::new(), argument),

                    }})
                    .evaluate(scope, form)?,
                    Binding(_) | Application(_) => expr! {
                        Application[self.span] {
                            // @Question or application.callee (unevaluated)?
                            callee,
                            argument: match form {
                                Form::Normal => argument.evaluate(scope, form)?,
                                Form::WeakHeadNormal => argument,
                            },
                            explicitness: Explicitness::Explicit,
                        }
                    },
                    ForeignApplication(application) => (expr! {
                        ForeignApplication[self.span] {
                            callee: application.callee.clone(),
                            arguments: extended(application.arguments.clone(), argument),
                        }
                    })
                    .evaluate(scope, form)?,
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
            ForeignApplication(application) => {
                let arguments = application
                    .arguments
                    .clone()
                    .into_iter()
                    .map(|argument| argument.evaluate(scope, form))
                    .collect::<Result<Vec<_>, _>>()?;
                scope
                    .module()
                    .apply_foreign_binding(application.callee.clone(), arguments.clone())?
                    .unwrap_or_else(|| {
                        expr! {
                            ForeignApplication[self.span] {
                                callee: application.callee.clone(),
                                arguments,
                            }
                        }
                    })
            }
        })
    }

    /// Assert that an expression is of type `Type`.
    fn is_a_type(self, scope: &FunctionScope<'_>) -> Result<()> {
        let r#type = self.infer_type(scope)?;
        TYPE.is_actual(r#type, scope)
    }

    // @Question move into its own module?
    fn _is_ffi_compatible(self) -> bool {
        todo!() // @Task
    }

    /// Assert that two expression are equal under evaluation/normalization.
    // @Bug @Beacon @Beacon if form == WeakHeadNormal, type mismatches occur when there shouldn't
    // @Update that is because `equals` is called on 2 `Substitutions` but 2 of those are never
    // equal. I think they should be "killed" earlier. probably a bug
    fn is_actual(self, actual: Self, scope: &FunctionScope<'_>) -> Result<()> {
        // let expected = self.evaluate(scope, Form::WeakHeadNormal)?;
        // let actual = actual.evaluate(scope, Form::WeakHeadNormal)?;
        let expected = self.clone().evaluate(scope, Form::Normal)?;
        let actual = actual.evaluate(scope, Form::Normal)?;

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
                Diagnostic::new(
                    Level::Fatal,
                    Code::E032,
                    format!("mismatched types. expected `{}`, got `{}`", self, actual),
                ),
            )
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
                        (Some(_), Some(_)) => pi0.codomain.clone().equals(
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

fn extended<T>(mut vec: Vec<T>, value: T) -> Vec<T> {
    vec.push(value);
    vec
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
