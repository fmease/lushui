//! The tree-walk interpreter necessary for type-checking.
//!
//! For reference, here is the bytecode interpreter: [crate::compiler::interpreter].
//!
//! ## Issues
//!
//! * too many bugs
//! * full case analysis not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations

pub(crate) mod ffi;
pub(crate) mod scope;

use super::Expression;
use crate::{
    ast::Explicit,
    diagnostic::{Code, Diagnostic, Diagnostics, Result},
    hir::*,
    resolver::CrateScope,
    span::Spanning,
    support::InvalidFallback,
};
use scope::{FunctionScope, ValueView};

#[derive(Clone, Copy)]
pub enum Form {
    Normal,
    WeakHeadNormal,
}

/// Evaluation context.
// @Task a recursion_depth: usize, @Note if we do that,
// remove Copy and have a custom Clone impl incrementing the value
#[derive(Clone, Copy)]
pub struct Context<'a> {
    pub(crate) scope: &'a FunctionScope<'a>,
    pub(crate) form: Form,
}

impl<'a> Context<'a> {
    /// Temporarily, for convenience and until we fix some bugs, the form
    /// is [Form::Normal] by default.
    pub fn new(scope: &'a FunctionScope<'_>) -> Self {
        // @Temporary: Normal
        Self {
            scope,
            form: Form::Normal,
            // form: Form::WeakHeadNormal,
        }
    }
}

impl<'a> Context<'a> {
    pub(crate) fn with_scope(&self, scope: &'a FunctionScope<'_>) -> Self {
        Self { scope, ..*self }
    }
}

// @Task add recursion depth
pub struct Interpreter<'a> {
    scope: &'a CrateScope,
    warnings: &'a mut Diagnostics,
}

impl<'a> Interpreter<'a> {
    pub fn new(scope: &'a CrateScope, warnings: &'a mut Diagnostics) -> Self {
        Self { scope, warnings }
    }

    #[allow(dead_code)]
    fn warn(&mut self, warning: Diagnostic) {
        self.warnings.insert(warning);
    }

    /// Run the entry point of the crate.
    pub fn run(&mut self) -> Result<Expression> {
        if let Some(program_entry) = &self.scope.program_entry {
            self.evaluate_expression(
                program_entry.clone().to_expression(),
                Context {
                    scope: &FunctionScope::CrateScope,
                    // form: Form::Normal,
                    form: Form::WeakHeadNormal,
                },
            )
        } else {
            // @Task this should be checked statically
            Err(Diagnostic::error()
                .with_code(Code::E050)
                .with_message("missing program entry"))
        }
    }

    pub(crate) fn substitute_expression(
        &mut self,
        expression: Expression,
        substitution: Substitution,
    ) -> Expression {
        use self::Substitution::*;
        use ExpressionKind::*;

        match (&expression.kind, substitution) {
            (Binding(binding), Shift(amount)) => {
                expr! { Binding[expression.span] { binder: binding.binder.clone().shift(amount) } }
            }
            // @Beacon @Beacon @Question @Bug
            (Binding(binding), Use(substitution, expression)) => {
                if binding.binder.is_innermost() {
                    self.substitute_expression(expression, Shift(0))
                } else {
                    {
                        self.substitute_expression(
                            expr! { Binding[expression.span] { binder: binding.binder.clone().unshift() } },
                            *substitution
                        )
                    }
                }
            }
            // @Beacon @Beacon @Question @Bug
            (Substitution(substitution0), substitution1) => {
                let expression = self.substitute_expression(
                    substitution0.expression.clone(),
                    substitution0.substitution.clone(),
                );
                self.substitute_expression(expression, substitution1)
            }
            (Type, _) | (Number(_), _) | (Text(_), _) => expression,
            // @Temporary @Note once we support next: Expression in IO, we prob. need to substitute
            // the former
            (IO(_), _) => expression,
            (Application(application), substitution) => {
                expr! {
                    Application[expression.span] {
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
                    PiType[expression.span] {
                        parameter: pi.parameter.clone(),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                    }
                }
            }
            (Lambda(lambda), substitution) => {
                let parameter_type_annotation =
                    lambda.parameter_type_annotation.clone().map(|type_| {
                        expr! {
                            Substitution[] {
                                expression: type_,
                                substitution: substitution.clone(),
                            }
                        }
                    });

                let body_type_annotation = lambda.body_type_annotation.clone().map(|type_| {
                    expr! {
                        Substitution[] {
                            expression: type_,
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
                    Lambda[expression.span] {
                        parameter: lambda.parameter.clone(),
                        parameter_type_annotation,
                        body_type_annotation,
                        body,
                        explicitness: lambda.explicitness,
                    }
                }
            }
            (CaseAnalysis(analysis), substitution) => {
                expr! {
                    CaseAnalysis[expression.span] {
                        cases: analysis.cases.iter().map(|case| Case {
                            pattern: case.pattern.clone(),
                            body: expr! {
                                Substitution[] {
                                    expression: case.body.clone(),
                                    substitution: substitution.clone(),
                                }
                            },
                        }).collect(),
                        subject: expr! {
                            Substitution[] {
                                expression: analysis.subject.clone(),
                                substitution,
                            }
                        },
                    }
                }
            }
            (UseIn, _) => todo!("substitute use/in"),
            (ForeignApplication(application), substitution) => expr! {
                ForeignApplication[expression.span] {
                    callee: application.callee.clone(),
                    arguments: application.arguments.iter().map(|argument| expr! {
                        Substitution[argument.span] {
                            expression: argument.clone(),
                            substitution: substitution.clone(),
                        }
                    }).collect()
                }
            },
            (Invalid, _) => InvalidFallback::invalid(),
        }
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    pub fn evaluate_expression(
        &mut self,
        expression: Expression,
        context: Context<'_>,
    ) -> Result<Expression> {
        use self::Substitution::*;
        use ExpressionKind::*;

        // @Bug we currently don't support zero-arity foreign functions
        Ok(match expression.clone().kind {
            Binding(binding) => match context.scope.lookup_value(&binding.binder, &self.scope) {
                // @Question is this normalization necessary? I mean, yes, we got a new scope,
                // but the thing in the previous was already normalized (well, it should have been
                // at least). I guess it is necessary because it can contain parameters which could not
                // be resolved yet but potentially can be now.
                ValueView::Reducible(expression) => {
                    self.evaluate_expression(expression, context)?
                }
                ValueView::Neutral => expression,
            },
            Application(application) => {
                let callee = self.evaluate_expression(application.callee.clone(), context)?;
                let argument = application.argument.clone();
                match callee.kind {
                    Lambda(lambda) => self.evaluate_expression(
                        expr! {
                            Substitution[] {
                                substitution: Use(Box::new(Shift(0)), argument),
                                expression: lambda.body.clone(),
                            }
                        },
                        context,
                    )?,
                    Binding(binding) if context.scope.is_foreign(&binding.binder, &self.scope) => {
                        self.evaluate_expression(
                            expr! {
                                ForeignApplication[expression.span] {
                                    callee: binding.binder.clone(),
                                    arguments: extended(Vec::new(), argument),

                            }},
                            context,
                        )?
                    }
                    Binding(_) | Application(_) => expr! {
                        Application[expression.span] {
                            callee,
                            argument: match context.form {
                                // Form::Normal => argument.evaluate(context)?,
                                // Form::WeakHeadNormal => argument,
                                _ => self.evaluate_expression(argument, context)?,
                            },
                            explicitness: Explicit,
                        }
                    },
                    ForeignApplication(application) => self.evaluate_expression(
                        expr! {
                            ForeignApplication[expression.span] {
                                callee: application.callee.clone(),
                                arguments: extended(application.arguments.clone(), argument),
                            }
                        },
                        context,
                    )?,
                    _ => unreachable!(),
                }
            }
            Type | Number(_) | Text(_) | IO(_) => expression,
            PiType(pi) => match context.form {
                Form::Normal => {
                    let domain = self.evaluate_expression(pi.domain.clone(), context)?;

                    let codomain = if pi.parameter.is_some() {
                        // @Beacon @Beacon @Question whyy do we need type information here in *evaluate*???
                        let scope = context.scope.extend_with_parameter(domain.clone());
                        self.evaluate_expression(pi.codomain.clone(), context.with_scope(&scope))?
                    } else {
                        self.evaluate_expression(pi.codomain.clone(), context)?
                    };

                    expr! {
                        PiType[expression.span] {
                            parameter: pi.parameter.clone(),
                            domain,
                            codomain,
                            explicitness: Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => expression,
            },
            Lambda(lambda) => match context.form {
                Form::Normal => {
                    let parameter_type = self.evaluate_expression(
                        lambda
                            .parameter_type_annotation
                            .clone()
                            .ok_or_else(super::missing_annotation)?,
                        context,
                    )?;
                    let body_type = lambda
                        .body_type_annotation
                        .clone()
                        .map(|type_| {
                            // @Beacon @Beacon @Question whyy do we need type information here in *evaluate*???
                            let scope = context.scope.extend_with_parameter(parameter_type.clone());
                            self.evaluate_expression(type_, context.with_scope(&scope))
                        })
                        .transpose()?;
                    // @Beacon @Beacon @Question whyy do we need type information here in *evaluate*???
                    let scope = context.scope.extend_with_parameter(parameter_type.clone());
                    let body =
                        self.evaluate_expression(lambda.body.clone(), context.with_scope(&scope))?;

                    expr! {
                        Lambda[expression.span] {
                            parameter: lambda.parameter.clone(),
                            parameter_type_annotation: Some(parameter_type),
                            body,
                            body_type_annotation: body_type,
                            explicitness: Explicit,
                        }
                    }
                }
                Form::WeakHeadNormal => expression,
            },
            Substitution(substitution) => {
                let expression = self.substitute_expression(
                    substitution.expression.clone(),
                    substitution.substitution.clone(),
                );
                self.evaluate_expression(expression, context)?
            }
            UseIn => todo!("evaluate use/in"),
            // @Note partially applied constructors differ from normal values
            // I guess it's very likely that the first code we write will handle them incorrectly
            // because the code will not check for the arity of the neutral application
            // @Note how to handle them: just like functions: case analysis only wotks with a binder-case
            // (default case)
            CaseAnalysis(analysis) => {
                let subject = self.evaluate_expression(analysis.subject.clone(), context)?;

                // @Note we assume, subject is composed of only applications, bindings etc corresponding to the pattern types
                // everything else should be impossible because of type checking but I might be wrong.
                // possible counter examples: unevaluated case analysis expression
                // @Note @Beacon think about having a variable `matches: bool` (whatever) to avoid repetition
                match subject.kind {
                    Binding(subject) => {
                        for case in analysis.cases.iter() {
                            match &case.pattern.kind {
                                PatternKind::Number(_) => todo!(),
                                PatternKind::Text(_) => todo!(),
                                PatternKind::Binding(binding) => {
                                    if binding.binder == subject.binder {
                                        // @Task @Beacon extend with parameters when evaluating
                                        return self
                                            .evaluate_expression(case.body.clone(), context);
                                    }
                                }
                                PatternKind::Binder(_) => todo!(),
                                PatternKind::Deapplication(_) => todo!(),
                                PatternKind::Invalid => unreachable!(),
                            }
                        }
                        // we should not be here
                        // @Note this is currently reachable because we don't do a check for
                        // exhaustiveness in `infer_type`, just fyi
                        unreachable!()
                    }
                    // @Beacon @Beacon @Task
                    Application(_application) => todo!(),
                    Number(literal0) => {
                        for case in analysis.cases.iter() {
                            match &case.pattern.kind {
                                PatternKind::Number(literal1) => {
                                    if &literal0 == literal1 {
                                        return self
                                            .evaluate_expression(case.body.clone(), context);
                                    }
                                }
                                PatternKind::Text(_) => todo!(),
                                PatternKind::Binding(_) => todo!(),
                                PatternKind::Binder(_) => {
                                    // @Beacon @Beacon @Question whyy do we need type information here in *evaluate*???
                                    let scope = context
                                        .scope
                                        .extend_with_parameter(InvalidFallback::invalid());
                                    return self.evaluate_expression(
                                        case.body.clone(),
                                        context.with_scope(&scope),
                                    );
                                }
                                PatternKind::Deapplication(_) => todo!(),
                                PatternKind::Invalid => unreachable!(),
                            }
                        }
                        // we should not be here
                        // @Note this is currently reachable because we don't do a check for
                        // exhaustiveness in `infer_type`, just fyi
                        unreachable!()
                    }
                    // @Note reachable if they contain neutrals, right??
                    _ => unreachable!(),
                }
            }
            ForeignApplication(application) => {
                let arguments = application
                    .arguments
                    .clone()
                    .into_iter()
                    .map(|argument| self.evaluate_expression(argument, context))
                    .collect::<Result<Vec<_>, _>>()?;
                self.scope
                    .apply_foreign_binding(application.callee.clone(), arguments.clone())?
                    .unwrap_or_else(|| {
                        expr! {
                            ForeignApplication[expression.span] {
                                callee: application.callee.clone(),
                                arguments,
                            }
                        }
                    })
            }
            Invalid => InvalidFallback::invalid(),
        })
    }

    // @Question move into its own module?
    fn _is_ffi_compatible(&mut self, _expression: Expression) -> bool {
        todo!() // @Task
    }

    /// Dictates if two expressions are alpha-equivalent.
    // @Note this function is not powerful enough, it cannot handle equi-recursive types,
    // @Task write a unifier
    // @Task rename to expressions_equal
    pub(crate) fn equals(
        &mut self,
        expression0: Expression,
        expression1: Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        use ExpressionKind::*;

        Ok(match (expression0.kind, expression1.kind) {
            (Binding(binding0), Binding(binding1)) => binding0.binder == binding1.binder,
            (Application(application0), Application(application1)) => {
                self.equals(
                    application0.callee.clone(),
                    application1.callee.clone(),
                    scope,
                )? && self.equals(
                    application0.argument.clone(),
                    application1.argument.clone(),
                    scope,
                )?
            }
            (Type, Type) => true,
            (Number(number0), Number(number1)) => number0 == number1,
            (Text(text0), Text(text1)) => text0 == text1,
            // @Question what about explicitness?
            (PiType(pi0), PiType(pi1)) => {
                use self::Substitution::Shift;

                self.equals(pi0.domain.clone(), pi1.domain.clone(), scope)?
                    && match (pi0.parameter.clone(), pi1.parameter.clone()) {
                        (Some(_), Some(_)) => self.equals(
                            pi0.codomain.clone(),
                            pi1.codomain.clone(),
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (Some(_), None) => {
                            let codomain1 =
                                self.substitute_expression(pi1.codomain.clone(), Shift(1));
                            self.equals(
                                codomain1,
                                pi0.codomain.clone(),
                                &scope.extend_with_parameter(pi0.domain.clone()),
                            )?
                        }
                        (None, Some(_)) => {
                            let codomain0 =
                                self.substitute_expression(pi0.codomain.clone(), Shift(1));
                            self.equals(
                                pi1.codomain.clone(),
                                codomain0,
                                &scope.extend_with_parameter(pi1.domain.clone()),
                            )?
                        }
                        (None, None) => {
                            self.equals(pi0.codomain.clone(), pi1.codomain.clone(), scope)?
                        }
                    }
            }
            // @Question what about the body_type_annotation? what about explicitness?
            (Lambda(lambda0), Lambda(lambda1)) => {
                let parameter_type_annotation0 = lambda0
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(super::missing_annotation)?;
                let parameter_type_annotation1 = lambda1
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(super::missing_annotation)?;

                self.equals(
                    parameter_type_annotation0.clone(),
                    parameter_type_annotation1,
                    scope,
                )? && self.equals(
                    lambda0.body.clone(),
                    lambda1.body.clone(),
                    &scope.extend_with_parameter(parameter_type_annotation0),
                )?
            }
            (CaseAnalysis(_), CaseAnalysis(_)) => unreachable!(),
            (ForeignApplication(foreign0), ForeignApplication(foreign1)) => {
                foreign0.callee == foreign1.callee
                    && foreign0
                        .arguments
                        .clone()
                        .into_iter()
                        .zip(foreign1.arguments.clone())
                        .map(|(argument0, argument1)| self.equals(argument0, argument1, scope))
                        .fold(Ok(true), |all: Result<_>, this| Ok(all? && this?))?
            }
            // @Question is that what we want or should we just evaluate again?
            (Substitution(_), Substitution(_)) => {
                return Err(Diagnostic::bug()
                    .with_message("attempt to check two substitutions for equivalence")
                    .with_note("they should not exist in this part of the code but should have already been evaluated"))
            }
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

use crate::support::DisplayWith;
use std::fmt;

impl DisplayWith for Substitution {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Substitution::*;
        match self {
            Shift(amount) => write!(f, "shift {}", amount),
            Use(substitution, expression) => write!(
                f,
                "{}[{}]",
                expression.with(scope),
                substitution.with(scope)
            ),
        }
    }
}
