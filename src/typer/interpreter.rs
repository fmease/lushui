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

use super::Expression;
use crate::{
    component::Component,
    diagnostics::{Diagnostic, Reporter},
    entity::Entity,
    error::{PossiblyErroneous, Result},
    hir::{self, DeBruijnIndex, DeclarationIndex, Identifier},
    session::{BuildSession, KnownBinding, Value},
    syntax::{ast::Explicit, lowered_ast::Attributes},
    utility::{AsDebug, DisplayWith},
};
use std::{default::default, fmt};
use Substitution::Shift;

/// Run the entry point of the given executable component.
pub fn evaluate_main_function(
    component: &Component,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<Expression> {
    Interpreter::new(component, session, reporter).evaluate_expression(
        Interpreter::new(component, session, reporter)
            .component
            .entry
            .clone()
            .unwrap()
            .into_expression(),
        Context::new(&FunctionScope::Module),
    )
}

pub(crate) struct Interpreter<'a> {
    // @Task add recursion depth
    component: &'a Component,
    session: &'a BuildSession,
    reporter: &'a Reporter,
}

impl<'a> Interpreter<'a> {
    pub(super) fn new(
        component: &'a Component,
        session: &'a BuildSession,
        reporter: &'a Reporter,
    ) -> Self {
        Self {
            component,
            session,
            reporter,
        }
    }

    pub(crate) fn substitute_expression(
        &self,
        expression: Expression,
        substitution: Substitution,
    ) -> Expression {
        use self::Substitution::*;
        use hir::ExpressionKind::*;

        #[allow(clippy::match_same_arms)] // @Temporary
        match (&expression.value, substitution) {
            (Binding(binding), Shift(amount)) => hir::Expression::new(
                expression.attributes,
                expression.span,
                hir::Binding(binding.0.clone().shift(amount)).into(),
            ),
            // @Beacon @Beacon @Question @Bug
            (Binding(binding), Use(substitution, expression)) => {
                if binding.0.is_innermost() {
                    self.substitute_expression(expression, Shift(0))
                } else {
                    {
                        self.substitute_expression(
                            hir::Expression::new(
                                expression.attributes,
                                expression.span,
                                hir::Binding(binding.0.clone().unshift()).into(),
                            ),
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
            (Application(application), substitution) => Expression::new(
                expression.attributes,
                expression.span,
                hir::Application {
                    callee: Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            expression: application.callee.clone(),
                            substitution: substitution.clone(),
                        }
                        .into(),
                    ),
                    argument: Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            expression: application.argument.clone(),
                            substitution,
                        }
                        .into(),
                    ),
                    explicitness: application.explicitness,
                }
                .into(),
            ),
            (PiType(pi), substitution) => {
                let domain = Expression::new(
                    default(),
                    default(),
                    hir::Substitution {
                        expression: pi.domain.clone(),
                        substitution: substitution.clone(),
                    }
                    .into(),
                );

                let codomain = Expression::new(
                    default(),
                    default(),
                    hir::Substitution {
                        expression: pi.codomain.clone(),
                        substitution: match &pi.parameter {
                            Some(parameter) => {
                                let binder = parameter.as_innermost();

                                // @Question what about the attributes of the binder?
                                Use(
                                    Box::new(Shift(1).compose(substitution)),
                                    binder.into_expression(),
                                )
                            }
                            None => substitution,
                        },
                    }
                    .into(),
                );

                Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::PiType {
                        explicitness: pi.explicitness,
                        laziness: pi.laziness,
                        parameter: pi.parameter.clone(),
                        domain,
                        codomain,
                    }
                    .into(),
                )
            }
            (Lambda(lambda), substitution) => {
                let parameter_type_annotation =
                    lambda.parameter_type_annotation.clone().map(|type_| {
                        Expression::new(
                            default(),
                            default(),
                            hir::Substitution {
                                expression: type_,
                                substitution: substitution.clone(),
                            }
                            .into(),
                        )
                    });

                let body_type_annotation = lambda.body_type_annotation.clone().map(|type_| {
                    Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            expression: type_,
                            substitution: {
                                let binder = lambda.parameter.as_innermost();
                                // @Question what about the attributes of the binder?
                                Use(
                                    Box::new(Shift(1).compose(substitution.clone())),
                                    binder.into_expression(),
                                )
                            },
                        }
                        .into(),
                    )
                });

                let body = Expression::new(
                    default(),
                    default(),
                    hir::Substitution {
                        expression: lambda.body.clone(),
                        substitution: {
                            let binder = lambda.parameter.as_innermost();

                            // @Question what about the attributes of the binder?
                            Use(
                                Box::new(Shift(1).compose(substitution)),
                                binder.into_expression(),
                            )
                        },
                    }
                    .into(),
                );

                Expression::new(
                    expression.attributes,
                    expression.span,
                    hir::Lambda {
                        parameter: lambda.parameter.clone(),
                        parameter_type_annotation,
                        body_type_annotation,
                        body,
                        explicitness: lambda.explicitness,
                        laziness: lambda.laziness,
                    }
                    .into(),
                )
            }
            (CaseAnalysis(analysis), substitution) => Expression::new(
                expression.attributes,
                expression.span,
                hir::CaseAnalysis {
                    cases: analysis
                        .cases
                        .iter()
                        .map(|case| hir::Case {
                            pattern: case.pattern.clone(),
                            body: Expression::new(
                                default(),
                                default(),
                                hir::Substitution {
                                    expression: case.body.clone(),
                                    substitution: substitution.clone(),
                                }
                                .into(),
                            ),
                        })
                        .collect(),
                    scrutinee: Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            expression: analysis.scrutinee.clone(),
                            substitution,
                        }
                        .into(),
                    ),
                }
                .into(),
            ),
            (UseIn, _) => todo!("substitute use/in"),
            (IntrinsicApplication(application), substitution) => Expression::new(
                expression.attributes,
                expression.span,
                hir::IntrinsicApplication {
                    callee: application.callee.clone(),
                    arguments: application
                        .arguments
                        .iter()
                        .map(|argument| {
                            Expression::new(
                                argument.attributes.clone(),
                                argument.span,
                                hir::Substitution {
                                    expression: argument.clone(),
                                    substitution: substitution.clone(),
                                }
                                .into(),
                            )
                        })
                        .collect(),
                }
                .into(),
            ),
            (Error, _) => PossiblyErroneous::error(),
        }
    }

    /// Try to evaluate an expression.
    ///
    /// This is beta-reduction I think.
    pub(crate) fn evaluate_expression(
        &self,
        expression: Expression,
        context: Context<'_>,
    ) -> Result<Expression> {
        use self::Substitution::*;
        use hir::ExpressionKind::*;

        // @Bug we currently don't support zero-arity intrinsic functions
        Ok(match expression.clone().value {
            Binding(binding) => {
                match self.look_up_value(&binding.0) {
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
                            Expression::new(
                                default(),
                                argument.span,
                                hir::Lambda {
                                    // @Bug we should not create a fake identifier at all!!
                                    // @Note this problem is solved once we allow identifier to be
                                    // an Option<_> (that's gonna happen when we finally implement
                                    // the discarding identifier `_`)
                                    parameter: Identifier::parameter("__"),
                                    parameter_type_annotation: Some(
                                        self.session.look_up_known_binding(
                                            KnownBinding::Unit,
                                            self.reporter,
                                        )?,
                                    ),
                                    body_type_annotation: None,
                                    body: Expression::new(
                                        default(),
                                        default(),
                                        hir::Substitution {
                                            substitution: Shift(1),
                                            expression: argument,
                                        }
                                        .into(),
                                    ),
                                    explicitness: Explicit,
                                    laziness: None,
                                }
                                .into(),
                            )
                        } else {
                            argument
                        };

                        self.evaluate_expression(
                            Expression::new(
                                default(),
                                default(),
                                hir::Substitution {
                                    substitution: Use(Box::new(Shift(0)), argument),
                                    expression: lambda.body.clone(),
                                }
                                .into(),
                            ),
                            context,
                        )?
                    }
                    Binding(binding) if self.is_intrinsic_function(&binding.0) => self
                        .evaluate_expression(
                            Expression::new(
                                expression.attributes,
                                expression.span,
                                hir::IntrinsicApplication {
                                    callee: binding.0.clone(),
                                    arguments: vec![argument],
                                }
                                .into(),
                            ),
                            context,
                        )?,
                    Binding(_) | Application(_) => Expression::new(
                        expression.attributes,
                        expression.span,
                        hir::Application {
                            callee,
                            // argument: match context.form {
                            //     Form::Normal => argument.evaluate_expression(context)?,
                            //     Form::WeakHeadNormal => argument,
                            // },
                            argument: self.evaluate_expression(argument, context)?,
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    IntrinsicApplication(application) => self.evaluate_expression(
                        Expression::new(
                            expression.attributes,
                            expression.span,
                            hir::IntrinsicApplication {
                                callee: application.callee.clone(),
                                arguments: {
                                    let mut arguments = application.arguments.clone();
                                    arguments.push(argument);
                                    arguments
                                },
                            }
                            .into(),
                        ),
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

                    Expression::new(
                        expression.attributes,
                        expression.span,
                        hir::PiType {
                            explicitness: pi.explicitness,
                            laziness: pi.laziness,
                            parameter: pi.parameter.clone(),
                            domain,
                            codomain,
                        }
                        .into(),
                    )
                }
                Form::WeakHeadNormal => expression,
            },
            Lambda(lambda) => match context.form {
                Form::Normal => {
                    let parameter_type = self.evaluate_expression(
                        lambda.parameter_type_annotation.clone().ok_or_else(|| {
                            Diagnostic::missing_annotation().report(self.reporter)
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

                    Expression::new(
                        expression.attributes,
                        expression.span,
                        hir::Lambda {
                            parameter: lambda.parameter.clone(),
                            parameter_type_annotation: Some(parameter_type),
                            body,
                            body_type_annotation: body_type,
                            explicitness: Explicit,
                            laziness: lambda.laziness,
                        }
                        .into(),
                    )
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
                let scrutinee = self.evaluate_expression(analysis.scrutinee.clone(), context)?;

                // @Note we assume, subject is composed of only applications, bindings etc corresponding to the pattern types
                // everything else should be impossible because of type checking but I might be wrong.
                // possible counter examples: unevaluated case analysis expression
                // @Note @Beacon think about having a variable `matches: bool` (whatever) to avoid repetition
                match scrutinee.value {
                    Binding(scrutinee) => {
                        // @Temporary hack (bc we do not follow any principled implementation right now):
                        // a case analysis is indirectly neutral if the subject is a neutral binding
                        if self.look_up_value(&scrutinee.0).is_neutral() {
                            return Ok(Expression::new(
                                expression.attributes,
                                expression.span,
                                hir::CaseAnalysis {
                                    scrutinee: scrutinee.0.clone().into_expression(),
                                    cases: analysis.cases.clone(),
                                }
                                .into(),
                            ));
                        }

                        for case in &analysis.cases {
                            use hir::PatternKind::*;

                            match &case.pattern.value {
                                Binding(binding) => {
                                    if binding.0 == scrutinee.0 {
                                        // @Task @Beacon extend with parameters when evaluating
                                        return self
                                            .evaluate_expression(case.body.clone(), context);
                                    }
                                }
                                Number(_) | Text(_) | Binder(_) | Application(_) => todo!(),
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
                                Text(_) | Binding(_) | Application(_) => todo!(),
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
                        Expression::new(
                            expression.attributes,
                            expression.span,
                            hir::IntrinsicApplication {
                                callee: application.callee.clone(),
                                arguments,
                            }
                            .into(),
                        )
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
            crate::entity::EntityKind::IntrinsicFunction {
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
                    self.component,
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
    fn _is_ffi_compatible(&self, _expression: Expression) -> bool {
        todo!() // @Task
    }

    /// Dictates if two expressions are alpha-equivalent.
    // @Task write a unifier
    // @Task rename to expressions_equal
    pub(crate) fn equals(
        &self,
        expression0: &Expression,
        expression1: &Expression,
        scope: &FunctionScope<'_>,
    ) -> Result<bool> {
        use hir::ExpressionKind::*;

        Ok(match (&expression0.value, &expression1.value) {
            (Binding(binding0), Binding(binding1)) => binding0.0 == binding1.0,
            (Application(application0), Application(application1)) => {
                self.equals(&application0.callee, &application1.callee, scope)?
                    && self.equals(&application0.argument, &application1.argument, scope)?
            }
            (Type, Type) => true,
            (Number(number0), Number(number1)) => number0 == number1,
            (Text(text0), Text(text1)) => text0 == text1,
            // @Question what about explicitness?
            (PiType(pi0), PiType(pi1)) => {
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
                self.equals(&analysis0.scrutinee, &analysis1.scrutinee, scope)?
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
                return Err(Diagnostic::bug()
                    .message("attempt to check two substitutions for equivalence")
                    .note("they should not exist in this part of the code but should have already been evaluated").report(self.reporter));
            }
            // @Task probably should just be `true` once we support errors in subexpressions
            (Error, _) | (_, Error) => panic!("trying to check equality on erroneous expressions"),
            _ => false,
        })
    }

    fn look_up(&self, index: DeclarationIndex) -> &Entity {
        match index.local(self.component) {
            Some(index) => &self.component[index],
            None => &self.session[index],
        }
    }

    pub(crate) fn look_up_value(&self, binder: &Identifier) -> ValueView {
        use hir::Index::*;

        match binder.index {
            Declaration(index) => self.look_up(index).value(),
            DeBruijn(_) => ValueView::Neutral,
            DeBruijnParameter => unreachable!(),
        }
    }

    pub(crate) fn look_up_type(
        &self,
        binder: &Identifier,
        scope: &FunctionScope<'_>,
    ) -> Option<Expression> {
        use hir::Index::*;

        match binder.index {
            Declaration(index) => self.look_up(index).type_(),
            DeBruijn(index) => Some(scope.look_up_type(index)),
            DeBruijnParameter => unreachable!(),
        }
    }

    pub(crate) fn is_intrinsic_function(&self, binder: &Identifier) -> bool {
        use hir::Index::*;

        match binder.index {
            Declaration(index) => self.look_up(index).is_intrinsic_function(),
            DeBruijn(_) => false,
            DeBruijnParameter => unreachable!(),
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
                Expression::new(
                    default(),
                    default(),
                    hir::Substitution {
                        substitution: substitution0,
                        expression,
                    }
                    .into(),
                ),
            ),
        }
    }
}

impl DisplayWith for Substitution {
    type Context<'a> = (&'a Component, &'a BuildSession);

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
    #[allow(dead_code)]
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
        Self {
            scope,
            // @Temporary
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

#[derive(Clone)] // @Question expensive attributes clone?
pub(crate) struct BindingRegistration {
    pub(crate) attributes: Attributes,
    pub(crate) kind: BindingRegistrationKind,
}

#[derive(Clone)]
pub(crate) enum BindingRegistrationKind {
    Function {
        binder: Identifier,
        type_: Expression,
        value: Option<Expression>,
    },
    Data {
        binder: Identifier,
        type_: Expression,
    },
    Constructor {
        binder: Identifier,
        type_: Expression,
        owner_data_type: Identifier,
    },
    IntrinsicFunction {
        binder: Identifier,
        type_: Expression,
    },
}

// only used to report "cyclic" types (currently treated as a bug)
impl DisplayWith for BindingRegistration {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use BindingRegistrationKind::*;

        match &self.kind {
            Function {
                binder,
                type_,
                value,
            } => {
                let mut compound = f.debug_struct("Value");
                compound
                    .field("binder", binder)
                    .field("type", &type_.with(context).as_debug());
                match value {
                    Some(value) => compound.field("value", &value.with(context).as_debug()),
                    None => compound.field("value", &"?(none)"),
                }
                .finish()
            }
            Data { binder, type_ } => f
                .debug_struct("Data")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .finish(),
            Constructor {
                binder,
                type_,
                owner_data_type: data,
            } => f
                .debug_struct("Constructor")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .field("data", data)
                .finish(),
            IntrinsicFunction { binder, type_ } => f
                .debug_struct("IntrinsicFunction")
                .field("binder", binder)
                .field("type", &type_.with(context).as_debug())
                .finish(),
        }
    }
}

// @Task find out if we can get rid of this type by letting `ModuleScope::lookup_value` resolve to the Binder
// if it's neutral
pub(crate) enum ValueView {
    Reducible(Expression),
    Neutral,
}

impl ValueView {
    pub(crate) fn is_neutral(&self) -> bool {
        matches!(self, Self::Neutral)
    }
}

/// The scope of bindings inside of a function.
pub(crate) enum FunctionScope<'a> {
    Module,
    FunctionParameter {
        parent: &'a Self,
        type_: Expression,
    },
    PatternBinders {
        parent: &'a Self,
        // @Note idk
        types: Vec<Expression>,
    },
}

impl<'a> FunctionScope<'a> {
    pub(crate) fn extend_with_parameter(&'a self, type_: Expression) -> Self {
        Self::FunctionParameter {
            parent: self,
            type_,
        }
    }

    pub(crate) fn extend_with_pattern_binders(&'a self, types: Vec<Expression>) -> Self {
        Self::PatternBinders {
            parent: self,
            types,
        }
    }

    pub(super) fn look_up_type(&self, index: DeBruijnIndex) -> Expression {
        self.look_up_type_with_depth(index, 0)
    }

    fn look_up_type_with_depth(&self, index: DeBruijnIndex, depth: usize) -> Expression {
        match self {
            Self::FunctionParameter { parent, type_ } => {
                if depth == index.0 {
                    Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                        .into(),
                    )
                } else {
                    parent.look_up_type_with_depth(index, depth + 1)
                }
            }
            Self::PatternBinders { parent, types } => {
                match types
                    .iter()
                    .rev()
                    .zip(depth..)
                    .find(|(_, depth)| *depth == index.0)
                {
                    Some((type_, depth)) => Expression::new(
                        default(),
                        default(),
                        hir::Substitution {
                            // @Task verify this shift
                            substitution: Shift(depth + 1),
                            expression: type_.clone(),
                        }
                        .into(),
                    ),
                    None => parent.look_up_type_with_depth(index, depth + types.len()),
                }
            }
            Self::Module => unreachable!(),
        }
    }
}
