//! The tree-walk interpreter necessary for type-checking.
//!
//! For reference, here is the bytecode interpreter: [`crate::compiler::interpreter`].
//!
//! # Issues
//!
//! * too many bugs
//! * full case analysis not implemented
//! * non-trivial type inference not done
//! * untyped/unkinded AST-transformations
//! * does not use weak-head normal form

// @Beacon @Task use weak-normal form extensively again!!!

pub(crate) mod scope;

use super::Expression;
use crate::{
    diagnostics::{Diagnostic, Reporter},
    entity::Entity,
    error::{PossiblyErroneous, Result},
    format::DisplayWith,
    hir::{self, expr, DeclarationIndex, Identifier},
    package::{
        session::{KnownBinding, Value},
        BuildSession,
    },
    resolver::Crate,
    span::{Span, Spanning},
    syntax::{ast::Explicit, lowered_ast::Attributes},
};
use scope::{FunctionScope, ValueView};
use std::fmt;

/// Run the entry point of the crate.
pub fn evaluate_main_function(
    crate_: &Crate,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<Expression> {
    let mut interpreter = Interpreter::new(crate_, session, reporter);

    interpreter.evaluate_expression(
        interpreter
            .crate_
            .program_entry
            .clone()
            .unwrap()
            .into_expression(),
        Context {
            scope: &FunctionScope::Crate,
            // form: Form::Normal,
            form: Form::WeakHeadNormal,
        },
    )
}

pub(crate) struct Interpreter<'a> {
    // @Task add recursion depth
    crate_: &'a Crate,
    session: &'a BuildSession,
    reporter: &'a Reporter,
}

impl<'a> Interpreter<'a> {
    pub(super) fn new(
        crate_: &'a Crate,
        session: &'a BuildSession,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            crate_,
            session,
            reporter,
        }
    }

    pub(crate) fn substitute_expression(
        &mut self,
        expression: Expression,
        substitution: Substitution,
    ) -> Expression {
        use self::Substitution::*;
        use hir::ExpressionKind::*;

        #[allow(clippy::match_same_arms)] // @Temporary
        match (&expression.value, substitution) {
            (Binding(binding), Shift(amount)) => {
                expr! {
                    Binding {
                        expression.attributes,
                        expression.span;
                        binder: binding.binder.clone().shift(amount)
                    }
                }
            }
            // @Beacon @Beacon @Question @Bug
            (Binding(binding), Use(substitution, expression)) => {
                if binding.binder.is_innermost() {
                    self.substitute_expression(expression, Shift(0))
                } else {
                    {
                        self.substitute_expression(
                            expr! {
                                Binding {
                                    expression.attributes,
                                    expression.span;
                                    binder: binding.binder.clone().unshift()
                                }
                            },
                            *substitution,
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
            (Type | Number(_) | Text(_), _) => expression,
            // @Task verify
            (Projection(_), _) => expression,
            // @Temporary
            (IO(_), _) => expression,
            (Application(application), substitution) => {
                expr! {
                    Application {
                        expression.attributes,
                        expression.span;
                        callee: expr! {
                            Substitution {
                                Attributes::default(), Span::default();
                                expression: application.callee.clone(),
                                substitution: substitution.clone(),
                            }
                        },
                        argument: expr! {
                            Substitution {
                                Attributes::default(), Span::default();
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
                    Substitution {
                        Attributes::default(), Span::default();
                        expression: pi.domain.clone(),
                        substitution: substitution.clone(),
                    }
                };

                let codomain = expr! {
                    Substitution {
                        Attributes::default(), Span::default();
                        expression: pi.codomain.clone(),
                        substitution: match &pi.parameter {
                            Some(parameter) => {
                                let binder = parameter.as_innermost();

                                // @Question what about the attributes of the binder?
                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding { Attributes::default(), binder.span(); binder } }
                                )
                            }
                            None => substitution,
                        },
                    }
                };

                expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        explicitness: pi.explicitness,
                        laziness: pi.laziness,
                        parameter: pi.parameter.clone(),
                        domain,
                        codomain,
                    }
                }
            }
            (Lambda(lambda), substitution) => {
                let parameter_type_annotation =
                    lambda.parameter_type_annotation.clone().map(|type_| {
                        expr! {
                            Substitution {
                                Attributes::default(), Span::default();
                                expression: type_,
                                substitution: substitution.clone(),
                            }
                        }
                    });

                let body_type_annotation = lambda.body_type_annotation.clone().map(|type_| {
                    expr! {
                        Substitution {
                            Attributes::default(), Span::default();
                            expression: type_,
                            substitution: {
                                let binder = lambda.parameter.as_innermost();
                                // @Question what about the attributes of the binder?
                                Use(
                                    Box::new(Shift(1).compose(substitution.clone())),
                                    expr! { Binding { Attributes::default(), binder.span(); binder } }
                                )
                            }
                        }
                    }
                });

                let body = expr! {
                    Substitution {
                        Attributes::default(), Span::default();
                        expression: lambda.body.clone(),
                        substitution: {
                                let binder = lambda.parameter.as_innermost();

                                // @Question what about the attributes of the binder?
                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    expr! { Binding { Attributes::default(), binder.span(); binder } }
                                )
                        },
                    }
                };

                expr! {
                    Lambda {
                        expression.attributes,
                        expression.span;
                        parameter: lambda.parameter.clone(),
                        parameter_type_annotation,
                        body_type_annotation,
                        body,
                        explicitness: lambda.explicitness,
                        laziness: lambda.laziness,
                    }
                }
            }
            (CaseAnalysis(analysis), substitution) => {
                expr! {
                    CaseAnalysis {
                        expression.attributes,
                        expression.span;
                        cases: analysis.cases.iter().map(|case| hir::Case {
                            pattern: case.pattern.clone(),
                            body: expr! {
                                Substitution {
                                    Attributes::default(), Span::default();
                                    expression: case.body.clone(),
                                    substitution: substitution.clone(),
                                }
                            },
                        }).collect(),
                        subject: expr! {
                            Substitution {
                                Attributes::default(), Span::default();
                                expression: analysis.subject.clone(),
                                substitution,
                            }
                        },
                    }
                }
            }
            (UseIn, _) => todo!("substitute use/in"),
            (IntrinsicApplication(application), substitution) => expr! {
                IntrinsicApplication {
                    expression.attributes,
                    expression.span;
                    callee: application.callee.clone(),
                    arguments: application.arguments.iter().map(|argument| expr! {
                        Substitution {
                            argument.attributes.clone(), argument.span;
                            expression: argument.clone(),
                            substitution: substitution.clone(),
                        }
                    }).collect()
                }
            },
            (Error, _) => PossiblyErroneous::error(),
        }
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    pub(crate) fn evaluate_expression(
        &mut self,
        expression: Expression,
        context: Context<'_>,
    ) -> Result<Expression> {
        use self::Substitution::*;
        use hir::ExpressionKind::*;

        // @Bug we currently don't support zero-arity intrinsic functions
        Ok(match expression.clone().value {
            Binding(binding) => {
                match self.look_up_value(&binding.binder) {
                    // @Question is this normalization necessary? I mean, yes, we got a new scope,
                    // but the thing in the previous was already normalized (well, it should have been
                    // at least). I guess it is necessary because it can contain parameters which could not
                    // be resolved yet but potentially can be now.
                    ValueView::Reducible(expression) => {
                        self.evaluate_expression(expression, context)?
                    }
                    ValueView::Neutral => expression,
                }
            }
            Application(application) => {
                let callee = self.evaluate_expression(application.callee.clone(), context)?;
                let argument = application.argument.clone();
                match callee.value {
                    Lambda(lambda) => {
                        // @Bug because we don't reduce to weak-head normal form anywhere,
                        // diverging expressions are still going to diverge even if "lazy"/
                        // auto-closured to thunks since the body always gets evaluated to
                        // normal form
                        let argument = if lambda.laziness.is_some() {
                            expr! {
                                Lambda {
                                    Attributes::default(), argument.span;
                                    // @Bug we should not create a fake identifier at all!!
                                    // @Note this problem is solved once we allow identifier to be
                                    // an Option<_> (that's gonna happen when we finally implement
                                    // the discarding identifier `_`)
                                    parameter: Identifier::parameter("__"),
                                    parameter_type_annotation: Some(self.session.look_up_known_binding(KnownBinding::Unit, self.reporter)?),
                                    body_type_annotation: None,
                                    body: expr! {
                                        Substitution {
                                            Attributes::default(), Span::default();
                                            substitution: Shift(1),
                                            expression: argument,
                                        }
                                    },
                                    explicitness: Explicit,
                                    laziness: None,
                                }
                            }
                        } else {
                            argument
                        };

                        self.evaluate_expression(
                            expr! {
                                Substitution {
                                    Attributes::default(), Span::default();
                                    substitution: Use(Box::new(Shift(0)), argument),
                                    expression: lambda.body.clone(),
                                }
                            },
                            context,
                        )?
                    }
                    Binding(binding) if self.is_intrinsic(&binding.binder) => self
                        .evaluate_expression(
                            expr! {
                                IntrinsicApplication {
                                    expression.attributes, expression.span;
                                    callee: binding.binder.clone(),
                                    arguments: vec![argument],

                            }},
                            context,
                        )?,
                    Binding(_) | Application(_) => expr! {
                        Application {
                            expression.attributes, expression.span;
                            callee,
                            argument: match context.form {
                                // Form::Normal => argument.evaluate(context)?,
                                // Form::WeakHeadNormal => argument,
                                _ => self.evaluate_expression(argument, context)?,
                            },
                            explicitness: Explicit,
                        }
                    },
                    IntrinsicApplication(application) => self.evaluate_expression(
                        expr! {
                            IntrinsicApplication {
                                expression.attributes, expression.span;
                                callee: application.callee.clone(),
                                arguments: {
                                    let mut arguments = application.arguments.clone();
                                    arguments.push(argument);
                                    arguments
                                }
                            }
                        },
                        context,
                    )?,
                    _ => unreachable!(),
                }
            }
            Type | Number(_) | Text(_) | IO(_) => expression,
            Projection(_) => todo!(),
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
                        PiType {
                            expression.attributes, expression.span;
                            explicitness: pi.explicitness,
                            laziness: pi.laziness,
                            parameter: pi.parameter.clone(),
                            domain,
                            codomain,
                        }
                    }
                }
                Form::WeakHeadNormal => expression,
            },
            Lambda(lambda) => match context.form {
                Form::Normal => {
                    let parameter_type = self.evaluate_expression(
                        lambda.parameter_type_annotation.clone().ok_or_else(|| {
                            Diagnostic::missing_annotation().report(self.reporter);
                        })?,
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
                        Lambda {
                            expression.attributes, expression.span;
                            parameter: lambda.parameter.clone(),
                            parameter_type_annotation: Some(parameter_type),
                            body,
                            body_type_annotation: body_type,
                            explicitness: Explicit,
                            laziness: lambda.laziness,
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
            // @Bug panics if the subject reduces to a neutral identifier (i.e. lambda parameter)
            // contains one
            CaseAnalysis(analysis) => {
                let subject = self.evaluate_expression(analysis.subject.clone(), context)?;

                // @Note we assume, subject is composed of only applications, bindings etc corresponding to the pattern types
                // everything else should be impossible because of type checking but I might be wrong.
                // possible counter examples: unevaluated case analysis expression
                // @Note @Beacon think about having a variable `matches: bool` (whatever) to avoid repetition
                match subject.value {
                    Binding(subject) => {
                        // @Temporary hack (bc we do not follow any principled implementation right now):
                        // a case analysis is indirectly neutral if the subject is a neutral binding
                        if self.look_up_value(&subject.binder).is_neutral() {
                            return Ok(expr! {
                                CaseAnalysis {
                                    expression.attributes, expression.span;
                                    subject: subject.binder.clone().into_expression(),
                                    cases: analysis.cases.clone(),
                                }
                            });
                        }

                        for case in &analysis.cases {
                            use hir::PatternKind::*;

                            match &case.pattern.value {
                                Binding(binding) => {
                                    dbg!(&binding.binder);

                                    if binding.binder == subject.binder {
                                        // @Task @Beacon extend with parameters when evaluating
                                        return self
                                            .evaluate_expression(case.body.clone(), context);
                                    }
                                }
                                Number(_) | Text(_) | Binder(_) | Deapplication(_) => todo!(),
                                Error => unreachable!(),
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
                        for case in &analysis.cases {
                            use hir::PatternKind::*;

                            match &case.pattern.value {
                                Number(literal1) => {
                                    if &literal0 == literal1 {
                                        return self
                                            .evaluate_expression(case.body.clone(), context);
                                    }
                                }
                                Binder(_) => {
                                    // @Beacon @Beacon @Question whyy do we need type information here in *evaluate*???
                                    let scope = context
                                        .scope
                                        .extend_with_parameter(PossiblyErroneous::error());
                                    return self.evaluate_expression(
                                        case.body.clone(),
                                        context.with_scope(&scope),
                                    );
                                }
                                Text(_) | Binding(_) | Deapplication(_) => todo!(),
                                Error => unreachable!(),
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
            IntrinsicApplication(application) => {
                let arguments = application
                    .arguments
                    .clone()
                    .into_iter()
                    .map(|argument| self.evaluate_expression(argument, context))
                    .collect::<Result<Vec<_>, _>>()?;
                self.apply_intrinsic_function(application.callee.clone(), arguments.clone())?
                    .unwrap_or_else(|| {
                        expr! {
                            IntrinsicApplication {
                                expression.attributes,
                                expression.span;
                                callee: application.callee.clone(),
                                arguments,
                            }
                        }
                    })
            }
            Error => PossiblyErroneous::error(),
        })
    }

    /// Try applying an intrinsic function.
    ///
    /// # Panics
    ///
    /// Panics if `binder` is either not bound or not an intrinsic.
    // @Task correctly handle
    // * pure vs impure
    // * polymorphism
    // * illegal neutrals
    // * types (arguments of type `Type`): skip them
    // @Note: we need to convert to be able to convert to ffi::Value
    pub(crate) fn apply_intrinsic_function(
        &self,
        binder: Identifier,
        arguments: Vec<Expression>,
    ) -> Result<Option<Expression>> {
        match self.look_up(binder.declaration_index().unwrap()).kind {
            crate::entity::EntityKind::Intrinsic {
                arity, function, ..
            } => Ok(if arguments.len() == arity {
                let mut value_arguments = Vec::new();

                // @Task tidy up with iterator combinators
                for argument in arguments {
                    if let Some(argument) = Value::from_expression(&argument, self.session) {
                        value_arguments.push(argument);
                    } else {
                        return Ok(None);
                    }
                }

                Some(function(value_arguments).into_expression(
                    self.crate_,
                    self.session,
                    self.reporter,
                )?)
            } else {
                None
            }),
            _ => unreachable!(),
        }
    }

    // @Question move into its own module?
    #[allow(clippy::unused_self)]
    fn _is_ffi_compatible(&mut self, _expression: Expression) -> bool {
        todo!() // @Task
    }

    /// Dictates if two expressions are alpha-equivalent.
    // @Task write a unifier
    // @Task rename to expressions_equal
    pub(crate) fn equals(
        &mut self,
        expression0: &Expression,
        expression1: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        use hir::ExpressionKind::*;

        Ok(match (&expression0.value, &expression1.value) {
            (Binding(binding0), Binding(binding1)) => binding0.binder == binding1.binder,
            (Application(application0), Application(application1)) => {
                self.equals(&application0.callee, &application1.callee, scope)?
                    && self.equals(&application0.argument, &application1.argument, scope)?
            }
            (Type, Type) => true,
            (Number(number0), Number(number1)) => number0 == number1,
            (Text(text0), Text(text1)) => text0 == text1,
            // @Question what about explicitness?
            (PiType(pi0), PiType(pi1)) => {
                use self::Substitution::Shift;

                self.equals(&pi0.domain, &pi1.domain, scope)?
                    && match (pi0.parameter.clone(), pi1.parameter.clone()) {
                        (Some(_), Some(_)) => self.equals(
                            &pi0.codomain,
                            &pi1.codomain,
                            &scope.extend_with_parameter(pi0.domain.clone()),
                        )?,
                        (Some(_), None) => {
                            let codomain1 =
                                self.substitute_expression(pi1.codomain.clone(), Shift(1));
                            self.equals(
                                &codomain1,
                                &pi0.codomain,
                                &scope.extend_with_parameter(pi0.domain.clone()),
                            )?
                        }
                        (None, Some(_)) => {
                            let codomain0 =
                                self.substitute_expression(pi0.codomain.clone(), Shift(1));
                            self.equals(
                                &pi1.codomain,
                                &codomain0,
                                &scope.extend_with_parameter(pi1.domain.clone()),
                            )?
                        }
                        (None, None) => self.equals(&pi0.codomain, &pi1.codomain, scope)?,
                    }
            }
            // @Question what about the body_type_annotation? what about explicitness?
            (Lambda(lambda0), Lambda(lambda1)) => {
                let parameter_type_annotation0 = lambda0
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(|| Diagnostic::missing_annotation().report(self.reporter))?;
                let parameter_type_annotation1 = lambda1
                    .parameter_type_annotation
                    .clone()
                    .ok_or_else(|| Diagnostic::missing_annotation().report(self.reporter))?;

                self.equals(
                    &parameter_type_annotation0,
                    &parameter_type_annotation1,
                    scope,
                )? && self.equals(
                    &lambda0.body,
                    &lambda1.body,
                    &scope.extend_with_parameter(parameter_type_annotation0),
                )?
            }
            // @Temporary implementation
            (CaseAnalysis(analysis0), CaseAnalysis(analysis1)) => {
                self.equals(&analysis0.subject, &analysis1.subject, scope)?
            }
            (IntrinsicApplication(intrinsic0), IntrinsicApplication(intrinsic1)) => {
                intrinsic0.callee == intrinsic1.callee
                    && intrinsic0
                        .arguments
                        .iter()
                        .zip(&intrinsic1.arguments)
                        .map(|(argument0, argument1)| self.equals(argument0, argument1, scope))
                        .fold(Ok(true), |all: Result<_>, this| Ok(all? && this?))?
            }
            // @Question is that what we want or should we just evaluate again?
            (Substitution(_), Substitution(_)) => {
                Diagnostic::bug()
                    .message("attempt to check two substitutions for equivalence")
                    .note("they should not exist in this part of the code but should have already been evaluated").report(self.reporter);
                return Err(());
            }
            // @Task probably should just be `true` once we support errors in subexpressions
            (Error, _) | (_, Error) => panic!("trying to check equality on erroneous expressions"),
            _ => false,
        })
    }

    fn look_up(&self, index: DeclarationIndex) -> &Entity {
        match index.local(self.crate_) {
            Some(index) => &self.crate_[index],
            None => &self.session[index],
        }
    }

    pub(crate) fn look_up_value(&self, binder: &Identifier) -> ValueView {
        match binder.index {
            hir::Index::Declaration(index) => self.look_up(index).value(),
            hir::Index::DeBruijn(_) => ValueView::Neutral,
            hir::Index::DeBruijnParameter => unreachable!(),
        }
    }

    pub(crate) fn look_up_type(
        &self,
        binder: &Identifier,
        scope: &FunctionScope<'_>,
    ) -> Option<Expression> {
        match binder.index {
            hir::Index::Declaration(index) => self.look_up(index).type_(),
            hir::Index::DeBruijn(index) => Some(scope.look_up_type(index)),
            hir::Index::DeBruijnParameter => unreachable!(),
        }
    }

    pub(crate) fn is_intrinsic(&self, binder: &Identifier) -> bool {
        match binder.index {
            hir::Index::Declaration(index) => self.look_up(index).is_intrinsic(),
            hir::Index::DeBruijn(_) => false,
            hir::Index::DeBruijnParameter => unreachable!(),
        }
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
                    Substitution {
                        Attributes::default(), Span::default();
                        substitution: substitution0,
                        expression,
                    }
                },
            ),
        }
    }
}

impl DisplayWith for Substitution {
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use self::Substitution::*;
        match self {
            Shift(amount) => write!(f, "shift {}", amount),
            Use(substitution, expression) => write!(
                f,
                "{}[{}]",
                expression.with(context),
                substitution.with(context)
            ),
        }
    }
}

#[derive(Clone, Copy)]
pub(crate) enum Form {
    Normal,
    WeakHeadNormal,
}

/// Evaluation context.
// @Task a recursion_depth: usize, @Note if we do that,
// remove Copy and have a custom Clone impl incrementing the value
#[derive(Clone, Copy)]
pub(crate) struct Context<'a> {
    pub(crate) scope: &'a FunctionScope<'a>,
    pub(crate) form: Form,
}

impl<'a> Context<'a> {
    /// Temporarily, for convenience and until we fix some bugs, the form
    /// is [`Form::Normal`] by default.
    pub(crate) fn new(scope: &'a FunctionScope<'_>) -> Self {
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
