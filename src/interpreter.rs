//! The tree-walk interpreter.
//!
//! The first backend of lushuic. Later, we are going to add a bytecode interpreter for
//! evaluation. Still, this interpreter will stay because it is necessary for type-checking.
//!
//! ## Issues
//!
//! * too many bugs
//! * case analysis not implemented
//! * recursion not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * integration and regression tests missing

pub(crate) mod ffi;
pub(crate) mod scope;

use crate::{
    diagnostic::*,
    hir::{self, *},
    parser::Explicit,
    resolver::Resolved,
    span::Spanning,
    support::MayBeInvalid,
    typer,
};
pub use scope::CrateScope;
use scope::{FunctionScope, ValueView};

type Expression = hir::Expression<Resolved>;

impl CrateScope {
    /// Run the entry point of the crate.
    pub fn run(mut self) -> Result<Expression> {
        if let Some(program_entry) = self.program_entry.take() {
            program_entry.to_expression().evaluate(Context {
                scope: &(&self).into(),
                form: Form::Normal,
                phase: RunTime,
            })
        } else {
            Err(Diagnostic::new(
                Level::Fatal,
                Code::E050,
                "missing program entry",
            ))
        }
    }
}

#[derive(Clone, Copy)]
pub enum Form {
    Normal,
    WeakHeadNormal,
}

#[derive(Clone, Copy)]
pub enum Phase {
    CompileTime,
    RunTime,
}

pub use Phase::*;

/// Evaluation context.
// @Beacon @Task merge CrateScope back into FunctionScope
// @Task a recursion_depth: usize, @Note if we do that,
// remove Copy and have a custom Clone impl incrementing the value
#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub(crate) scope: &'a FunctionScope<'a>,
    pub(crate) form: Form,
    pub(crate) phase: Phase,
}

impl<'a> Context<'a> {
    pub(crate) fn with_scope(&self, scope: &'a FunctionScope<'_>) -> Self {
        Self { scope, ..*self }
    }
}

impl Expression {
    pub(crate) fn substitute(self, substitution: Substitution) -> Self {
        use self::Substitution::*;
        use ExpressionKind::*;

        match (&self.kind, substitution) {
            (Binding(binding), Shift(amount)) => {
                expr! { Binding[self.span] { binder: binding.binder.clone().shift(amount) } }
            }
            // @Beacon @Beacon @Question @Bug
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
            // @Beacon @Beacon @Question @Bug
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
                                let binder = parameter.as_innermost();

                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding[binder.span()] { binder } }
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
                                let binder = lambda.parameter.as_innermost();
                                Use(
                                    Box::new(Shift(1).compose(substitution.clone())),
                                    expr! { Binding[binder.span()] { binder } }
                                )
                            }
                        }
                    }
                });

                let body = expr! {
                    Substitution[] {
                        expression: lambda.body.clone(),
                        substitution: {
                                let binder = lambda.parameter.as_innermost();

                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding[binder.span()] { binder } }
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
            (Invalid, _) => MayBeInvalid::invalid(),
        }
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    pub fn evaluate(self, context: Context<'_>) -> Result<Self> {
        use self::Substitution::*;
        use ExpressionKind::*;

        // @Bug we currently don't support zero-arity foreign functions
        Ok(match self.clone().kind {
            Binding(binding) => match context.scope.lookup_value(&binding.binder) {
                // @Question is this normalization necessary? I mean, yes, we got a new scope,
                // but the thing in the previous was already normalized (well, it should have been
                // at least). I guess it is necessary because it can contain parameters which could not
                // be resolved yet but potentially can be now.
                ValueView::Reducible(expression) => expression.evaluate(context)?,
                ValueView::Neutral => self,
            },
            Application(application) => {
                let callee = application.callee.clone().evaluate(context)?;
                let argument = application.argument.clone();
                match callee.kind {
                    Lambda(lambda) => (expr! {
                        Substitution[] {
                            substitution: Use(Box::new(Shift(0)), argument),
                            expression: lambda.body.clone(),
                        }
                    })
                    .evaluate(context)?,
                    Binding(binding) if context.scope.is_foreign(&binding.binder) => (expr! {
                        ForeignApplication[self.span] {
                            callee: binding.binder.clone(),
                            arguments: extended(Vec::new(), argument),

                    }})
                    .evaluate(context)?,
                    Binding(_) | Application(_) => expr! {
                        Application[self.span] {
                            // @Question or application.callee (unevaluated)?
                            callee,
                            argument: match context.form {
                                Form::Normal => argument.evaluate(context)?,
                                Form::WeakHeadNormal => argument,
                            },
                            explicitness: Explicit,
                        }
                    },
                    ForeignApplication(application) => (expr! {
                        ForeignApplication[self.span] {
                            callee: application.callee.clone(),
                            arguments: extended(application.arguments.clone(), argument),
                        }
                    })
                    .evaluate(context)?,
                    _ => unreachable!(),
                }
            }
            Type | Nat(_) | Text(_) => self,
            PiType(pi) => match context.form {
                Form::Normal => {
                    let domain = pi.domain.clone().evaluate(context)?;

                    let codomain = if pi.parameter.is_some() {
                        let scope = context.scope.extend_with_parameter(domain.clone());
                        pi.codomain.clone().evaluate(context.with_scope(&scope))?
                    } else {
                        pi.codomain.clone()
                    };

                    expr! {
                        PiType[self.span] {
                            parameter: pi.parameter.clone(),
                            domain,
                            codomain,
                            explicitness: Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => self,
            },
            Lambda(lambda) => match context.form {
                Form::Normal => {
                    let parameter_type = lambda
                        .parameter_type_annotation
                        .clone()
                        .ok_or_else(typer::missing_annotation)?
                        .evaluate(context)?;
                    let body_type = lambda
                        .body_type_annotation
                        .clone()
                        .map(|r#type| {
                            let scope = context.scope.extend_with_parameter(parameter_type.clone());
                            r#type.evaluate(context.with_scope(&scope))
                        })
                        .transpose()?;
                    let scope = context.scope.extend_with_parameter(parameter_type.clone());
                    let body = lambda.body.clone().evaluate(context.with_scope(&scope))?;

                    expr! {
                        Lambda[self.span] {
                            parameter: lambda.parameter.clone(),
                            parameter_type_annotation: Some(parameter_type),
                            body,
                            body_type_annotation: body_type,
                            explicitness: Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => self,
            },
            Substitution(substitution) => substitution
                .expression
                .clone()
                .substitute(substitution.substitution.clone())
                .evaluate(context)?,
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
                    .map(|argument| argument.evaluate(context))
                    .collect::<Result<Vec<_>, _>>()?;
                context
                    .scope
                    .crate_scope()
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
            Invalid => MayBeInvalid::invalid(),
        })
    }

    // @Question move into its own module?
    fn _is_ffi_compatible(self) -> bool {
        todo!() // @Task
    }

    /// Dictates if two expressions are alpha-equivalent.
    // @Note this function is not powerful enough, it cannot handle equi-recursive types,
    // @Task write a unifier
    pub(crate) fn equals(self, other: Self, scope: &FunctionScope<'_>) -> Result<bool> {
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
                use self::Substitution::Shift;

                pi0.domain.clone().equals(pi1.domain.clone(), scope)?
                    && match (pi0.parameter.clone(), pi1.parameter.clone()) {
                        (Some(_), Some(_)) => pi0.codomain.clone().equals(
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (Some(_), None) => pi0.codomain.clone().equals(
                            pi1.codomain.clone().substitute(Shift(1)),
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (None, Some(_)) => pi1.codomain.clone().equals(
                            pi0.codomain.clone().substitute(Shift(1)),
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
                    .ok_or_else(typer::missing_annotation)?;
                let parameter_type_annotation1 = lambda1
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(typer::missing_annotation)?;

                parameter_type_annotation0
                    .clone()
                    .equals(parameter_type_annotation1, scope)?
                    && lambda0.body.clone().equals(
                        lambda1.body.clone(),
                        &scope.extend_with_parameter(parameter_type_annotation0),
                    )?
            }
            (CaseAnalysis(_), CaseAnalysis(_)) => unreachable!(),
            (Invalid, _) | (_, Invalid) => panic!("trying to check equality on an invalid node"),
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
