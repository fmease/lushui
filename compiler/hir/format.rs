//! The definition of the textual representation of the [HIR](crate::hir).

use super::{Component, Declaration, Expression, Pattern};
use crate::{session::BuildSession, utility::DisplayWith};
use joinery::JoinableIterator;
use std::fmt;

impl DisplayWith for Declaration {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(
        &self,
        (component, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.format_with_depth(component, session, 0, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(
        &self,
        component: &Component,
        session: &BuildSession,
        depth: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use super::DeclarationKind::*;
        use crate::syntax::lexer::INDENTATION;
        let context = (component, session);

        match &self.bare {
            Function(function) => {
                write!(
                    f,
                    "{}: {}",
                    function.binder,
                    function.type_annotation.with(context)
                )?;
                if let Some(expression) = &function.expression {
                    write!(f, " = {}", expression.with(context))?;
                }
                writeln!(f)
            }
            Data(type_) => match &type_.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} of",
                        type_.binder,
                        type_.type_annotation.with(context)
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION.0),
                            constructor.with(context)
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    type_.binder,
                    type_.type_annotation.with(context)
                ),
            },
            Constructor(constructor) => writeln!(
                f,
                "{}: {}",
                constructor.binder,
                constructor.type_annotation.with(context)
            ),
            Module(module) => {
                writeln!(f, "module {} of", module.binder)?;
                for declaration in &module.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                    declaration.format_with_depth(component, session, depth, f)?;
                }
                Ok(())
            }
            Use(use_) => match &use_.binder {
                Some(binder) => writeln!(f, "use {} as {}", use_.target, binder),
                None => writeln!(f, "use {}", use_.target),
            },
            Error => writeln!(f, "?(error)"),
        }
    }
}

// @Note many wasted allocations (intermediate Strings)
impl DisplayWith for Expression {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(
        &self,
        (component, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        format_pi_type_literal_or_lower(self, component, session, f)
    }
}

fn format_pi_type_literal_or_lower(
    expression: &Expression,
    component: &Component,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;
    let context = (component, session);

    // In here, we format `Lambda`, `UseIn` and `CaseAnalysis` as a pi-type-literal-or-lower instead of
    // a lowered expression — which you might have expected from reading through the grammar and the parser.
    // The reason for this is the way we treat bracketed expressions: We do not represent them in the AST
    // (as their own nodes).
    // We could add more checks in the implementation of the pretty-printer since we "lost" information.
    // But actually, we do not need extra checks if we use a different grammar from the parser's.
    // Note that, syntactically, applications in lushui are so flexible that they actually
    // allow bare complex expressions as arguments e.g. `call \x => x` and `call do pure unit`.
    // Haskell adopted this change at some point, too, with the extension `BlockArguments`.
    // Read https://typeclasses.com/ghc/block-arguments and/or
    // https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html?highlight=xblockarguments.
    // This leads to the phenomenon that `(read \this => this) alpha` and `read (\this => this) alpha`
    // parse to the identical AST modulo spans.
    // However for pretty-printing, we only use the last form — `read (\this => this) alpha` — to avoid
    // extra checks. For the case above, this decision is neither liberal nor conservate resulting in
    // an equal amount of brackets (that being one). This is not the case for `topmost.take (\it => it)` which
    // we *might* want to print as `topmost.take \it => it`. This would probably require passing some flags to
    // the formatting functions and adding more checks.
    //
    // See also `crate::syntax::parser::test::application_lambda_literal_argument_{lax,strict}_grouping` and the
    // comment at the grammar definition of `Pi-Type-Literal-Or-Lower` (in `/misc/grammar/lushui.grammar`)
    // for further details.
    match &expression.bare {
        PiType(pi) => {
            write!(f, "{}", pi.explicitness)?;

            // @Note fragile
            let domain_needs_brackets = pi.parameter.is_some() || pi.laziness.is_some();

            if domain_needs_brackets {
                write!(f, "(")?;
                if pi.laziness.is_some() {
                    write!(f, "lazy ")?;
                }

                if let Some(parameter) = &pi.parameter {
                    write!(f, "{parameter}: ")?;
                }

                write!(f, "{}", pi.domain.with(context))?;
                write!(f, ")")?;
            } else {
                format_application_or_lower(&pi.domain, component, session, f)?;
            }
            write!(f, " -> ")?;
            format_pi_type_literal_or_lower(&pi.codomain, component, session, f)
        }
        Lambda(lambda) => {
            write!(f, r"\{}", lambda.explicitness)?;
            let parameter_needs_brackets =
                lambda.parameter_type_annotation.is_some() || lambda.laziness.is_some();

            if parameter_needs_brackets {
                write!(f, "(")?;
                if lambda.laziness.is_some() {
                    write!(f, "lazy ")?;
                }
                write!(f, "{}", lambda.parameter)?;
                if let Some(annotation) = &lambda.parameter_type_annotation {
                    write!(f, ": {}", annotation.with(context))?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(f, ": {}", annotation.with(context))?;
            }

            write!(f, " => {}", lambda.body.with(context))
        }
        UseIn => todo!(),
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            writeln!(f, "case {} of", analysis.scrutinee.with(context))?;
            for case in &analysis.cases {
                writeln!(
                    f,
                    "{} => {}",
                    case.pattern.with(context),
                    case.body.with(context)
                )?;
            }
            Ok(())
        }
        _ => format_application_or_lower(expression, component, session, f),
    }
}

// @Task write named arguments
fn format_application_or_lower(
    expression: &Expression,
    component: &Component,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    match &expression.bare {
        Application(application) => {
            format_application_or_lower(&application.callee, component, session, f)?;
            write!(f, " {}", application.explicitness)?;
            format_lower_expression(&application.argument, component, session, f)
        }
        IntrinsicApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                format_lower_expression(argument, component, session, f)?;
            }

            Ok(())
        }
        _ => format_lower_expression(expression, component, session, f),
    }
}

fn format_lower_expression(
    expression: &Expression,
    component: &Component,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;
    let context = (component, session);

    for attribute in &expression.attributes.0 {
        write!(f, "{} ", attribute)?;
    }

    match &expression.bare {
        Type => write!(f, "Type"),
        Number(literal) => write!(f, "{literal}"),
        Text(literal) => write!(f, "{literal}"),
        Binding(binding) => write!(
            f,
            "{}",
            super::FunctionScope::path_to_string(&binding.0, component, session)
        ),
        // @Beacon @Temporary @Task just write out the path
        Projection(_projection) => write!(f, "?(projection)"),
        IO(io) => write!(
            f,
            "?(io {} {})",
            io.index,
            io.arguments
                .iter()
                .map(|argument| argument.with(context))
                .join_with(' ')
        ),
        Substitution(substitution) => write!(
            f,
            "?(substitution {} {})",
            substitution.substitution.with(context),
            substitution.expression.with(context)
        ),
        Error => write!(f, "?(error)"),
        _ => write!(f, "({})", expression.with(context)),
    }
}

// @Task @Beacon update bracket business
impl DisplayWith for Pattern {
    type Context<'a> = (&'a Component, &'a BuildSession);

    fn format(
        &self,
        context @ (component, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use super::PatternKind::*;

        match &self.bare {
            Number(number) => write!(f, "{number}"),
            Text(text) => write!(f, "{text}"),
            Binding(binding) => write!(
                f,
                "{}",
                super::FunctionScope::path_to_string(&binding.0, component, session)
            ),

            Binder(binder) => write!(f, "\\{}", binder.0),
            Application(application) => write!(
                f,
                "({}) ({})",
                application.callee.with(context),
                application.argument.with(context)
            ),
            Error => write!(f, "?(error)"),
        }
    }
}

impl fmt::Display for super::Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{value}"),
            Self::Nat32(value) => write!(f, "{value}"),
            Self::Nat64(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Int32(value) => write!(f, "{value}"),
            Self::Int64(value) => write!(f, "{value}"),
        }
    }
}

impl fmt::Display for super::Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // @Task don't use Rust's Debug impl for str!
            super::Text::Text(text) => write!(f, "{text:?}"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        component::Component,
        entity::{Entity, EntityKind},
        hir::{self, Expression, Identifier, LocalDeclarationIndex, Number, Text},
        resolver::Exposure,
        session::BuildSession,
        span::Span,
        syntax::{
            ast::{self, Explicitness::*},
            lowered_ast::{Attribute, AttributeKind, Attributes},
        },
        utility::{difference, DisplayWith},
    };
    use std::default::default;

    // @Beacon @Task do something smart if spaces differ (which cannot have color)
    // like replacing them with a different character like the Unicode space symbol
    // or however it is called ("SP")
    fn assert_eq(expected: &str, actual: impl AsRef<str>) {
        let actual = actual.as_ref();

        if actual != expected {
            panic!(
                "the actual textual representation of the HIR node does not match the expected one:\n{}",
                difference(expected, actual, ""),
            );
        }
    }

    impl Component {
        fn add(&mut self, name: &str, kind: EntityKind) -> Identifier {
            self.add_below(name, kind, self.local_root())
        }

        fn add_below(
            &mut self,
            name: &str,
            kind: EntityKind,
            parent: LocalDeclarationIndex,
        ) -> Identifier {
            let identifier = ast::Identifier::new_unchecked(name.into(), default());
            let entity = Entity {
                source: identifier.clone(),
                parent: Some(parent),
                exposure: Exposure::Unrestricted,
                attributes: default(),
                kind,
            };
            let index = self.bindings.insert(entity);
            Identifier::new(index.global(self), identifier)
        }
    }

    fn type_() -> Expression {
        Expression::new(default(), default(), hir::ExpressionKind::Type)
    }

    impl Attribute {
        fn stripped(kind: AttributeKind) -> Self {
            Self::new(default(), kind)
        }
    }

    #[test]
    fn pi_type_application_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let array = component
            .add("Array", EntityKind::untyped_data_type())
            .into_expression();
        let int = component
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "topmost.Array topmost.Int -> Type",
            (Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: array,
                            argument: int,
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    codomain: type_(),
                }
                .into(),
            ))
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn pi_type_named_parameter() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let array = component.add("Array", EntityKind::untyped_data_type());
        let int = component.add("Int", EntityKind::untyped_data_type());
        let container = component.add("Container", EntityKind::untyped_data_type());
        let alpha = Identifier::parameter("alpha");

        assert_eq(
            "(alpha: topmost.Array topmost.Int) -> topmost.Container alpha",
            Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Explicit,
                    laziness: None,
                    parameter: Some(alpha.clone()),
                    domain: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: array.into_expression(),
                            argument: int.into_expression(),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    codomain: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: container.into_expression(),
                            argument: alpha.into_expression(),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn pi_type_implicit_parameter() {
        let session = BuildSession::test();
        let component = Component::test();

        assert_eq(
            "'(whatever: Type) -> Type",
            Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Implicit,
                    laziness: None,
                    parameter: Some(Identifier::parameter("whatever")),
                    domain: type_(),
                    codomain: type_(),
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_two_curried_arguments].
    #[test]
    fn pi_type_higher_order_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();
        let int = component
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "(topmost.Int -> topmost.Int) -> topmost.Int",
            Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: Expression::new(
                        default(),
                        default(),
                        hir::PiType {
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: int.clone(),
                            codomain: int.clone(),
                        }
                        .into(),
                    ),
                    codomain: int,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_higher_order_argument].
    #[test]
    fn pi_type_two_curried_arguments() {
        let session = BuildSession::test();
        let mut component = Component::test();
        let int = component
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();
        let text = component
            .add("Text", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "topmost.Int -> topmost.Text -> Type",
            Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: int,
                    codomain: Expression::new(
                        default(),
                        default(),
                        hir::PiType {
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: text,
                            codomain: type_(),
                        }
                        .into(),
                    ),
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [lambda_pi_type_body].
    #[test]
    fn pi_type_lambda_domain() {
        let session = BuildSession::test();
        let component = Component::test();

        let x = Identifier::parameter("x");

        assert_eq(
            r"(\x => x) -> Type",
            Expression::new(
                default(),
                default(),
                hir::PiType {
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: Expression::new(
                        default(),
                        default(),
                        hir::Lambda {
                            parameter: x.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            body: x.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                        .into(),
                    ),
                    codomain: type_(),
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_three_curried_arguments() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let beta = component.add("beta", EntityKind::UntypedFunction);

        assert_eq(
            "alpha topmost.beta (gamma Type) 0",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: Expression::new(
                                default(),
                                default(),
                                hir::Application {
                                    callee: Identifier::parameter("alpha").into_expression(),
                                    argument: beta.into_expression(),
                                    explicitness: Explicit,
                                }
                                .into(),
                            ),
                            argument: Expression::new(
                                default(),
                                default(),
                                hir::Application {
                                    callee: Identifier::parameter("gamma").into_expression(),
                                    argument: type_(),
                                    explicitness: Explicit,
                                }
                                .into(),
                            ),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    argument: Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                    explicitness: Explicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [application_lambda_argument].
    #[test]
    fn application_lambda_last_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let take = component.add("take", EntityKind::UntypedFunction);
        let it = Identifier::parameter("it");

        // we might want to format this special case as `topmost.take \it => it` in the future
        assert_eq(
            r"topmost.take (\it => it)",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: take.into_expression(),
                    argument: Expression::new(
                        default(),
                        default(),
                        hir::Lambda {
                            parameter: it.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            // technically not correct
                            body: it.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [application_lambda_last_argument].
    #[test]
    fn application_lambda_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let take = component.add("take", EntityKind::UntypedFunction);
        let it = Identifier::parameter("it");

        assert_eq(
            r#"topmost.take (\it => it) "who""#,
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: take.into_expression(),
                            argument: Expression::new(
                                default(),
                                default(),
                                hir::Lambda {
                                    parameter: it.clone(),
                                    parameter_type_annotation: None,
                                    body_type_annotation: None,
                                    // technically not correct
                                    body: it.into_expression(),
                                    explicitness: Explicit,
                                    laziness: None,
                                }
                                .into(),
                            ),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    argument: Expression::new(
                        default(),
                        default(),
                        Text::Text("who".into()).into(),
                    ),
                    explicitness: Explicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_implicit_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let identity = component.add("identity", EntityKind::UntypedFunction);

        assert_eq(
            r"topmost.identity 'Type",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: identity.into_expression(),
                    argument: type_(),
                    explicitness: Implicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_complex_implicit_argument() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let identity = component.add("identity", EntityKind::UntypedFunction);
        let text = component.add("Text", EntityKind::untyped_data_type());

        assert_eq(
            r"topmost.identity '(prepare topmost.Text)",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: identity.into_expression(),
                    argument: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: Identifier::parameter("prepare").into_expression(),
                            argument: text.into_expression(),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    explicitness: Implicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_intrinsic_application_callee() {
        let session = BuildSession::test();
        let component = Component::test();

        assert_eq(
            "eta 10 omicron",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: Expression::new(
                        default(),
                        default(),
                        hir::IntrinsicApplication {
                            callee: Identifier::parameter("eta"),
                            arguments: vec![Expression::new(
                                default(),
                                default(),
                                Number::Nat(10u8.into()).into(),
                            )],
                        }
                        .into(),
                    ),
                    argument: Identifier::parameter("omicron").into_expression(),
                    explicitness: Explicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_body_type_annotation() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let output = component.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\input: topmost.Output => 0",
            Expression::new(
                default(),
                default(),
                hir::Lambda {
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: None,
                    body_type_annotation: Some(output.into_expression()),
                    body: Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                    explicitness: Explicit,
                    laziness: None,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_parameter_type_annotation_body_type_annotation() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let input = component.add("Input", EntityKind::untyped_data_type());
        let output = component.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\(input: topmost.Input): topmost.Output => Type",
            Expression::new(
                default(),
                default(),
                hir::Lambda {
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: Some(input.into_expression()),
                    body_type_annotation: Some(output.into_expression()),
                    body: type_(),
                    explicitness: Explicit,
                    laziness: None,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_implicit_parameter() {
        let session = BuildSession::test();
        let component = Component::test();

        assert_eq(
            r"\'(Input: Type) => Type",
            Expression::new(
                default(),
                default(),
                hir::Lambda {
                    parameter: Identifier::parameter("Input"),
                    parameter_type_annotation: Some(type_()),
                    body_type_annotation: None,
                    body: type_(),
                    explicitness: Implicit,
                    laziness: None,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_implicit_unannotated_parameter() {
        let session = BuildSession::test();
        let component = Component::test();
        let a = Identifier::parameter("a");

        assert_eq(
            r"\'A => \a => a",
            Expression::new(
                default(),
                default(),
                hir::Lambda {
                    parameter: Identifier::parameter("A"),
                    parameter_type_annotation: None,
                    body_type_annotation: None,
                    body: Expression::new(
                        default(),
                        default(),
                        hir::Lambda {
                            parameter: a.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            body: a.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                        .into(),
                    ),
                    explicitness: Implicit,
                    laziness: None,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_lambda_domain].
    #[test]
    fn lambda_pi_type_body() {
        let session = BuildSession::test();
        let component = Component::test();

        let x = Identifier::parameter("x");

        assert_eq(
            r"\x => x -> Type",
            Expression::new(
                default(),
                default(),
                hir::Lambda {
                    parameter: x.clone(),
                    parameter_type_annotation: None,
                    body_type_annotation: None,
                    body: Expression::new(
                        default(),
                        default(),
                        hir::PiType {
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: x.into_expression(),
                            codomain: type_(),
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
                    laziness: None,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn intrinsic_application_no_arguments() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let add = component.add("add", EntityKind::UntypedFunction);

        assert_eq(
            "add",
            Expression::new(
                default(),
                default(),
                hir::IntrinsicApplication {
                    callee: add,
                    arguments: Vec::new(),
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn intrinsic_application_two_arguments() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let add = component.add("add", EntityKind::UntypedFunction);

        assert_eq(
            "add (add 1 3000) 0",
            Expression::new(
                default(),
                default(),
                hir::IntrinsicApplication {
                    callee: add.clone(),
                    arguments: vec![
                        Expression::new(
                            default(),
                            default(),
                            hir::IntrinsicApplication {
                                callee: add,
                                arguments: vec![
                                    Expression::new(
                                        default(),
                                        default(),
                                        Number::Nat(1u8.into()).into(),
                                    ),
                                    Expression::new(
                                        default(),
                                        default(),
                                        Number::Nat(3000u16.into()).into(),
                                    ),
                                ],
                            }
                            .into(),
                        ),
                        Expression::new(default(), default(), Number::Nat(0u8.into()).into()),
                    ],
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        );
    }

    #[test]
    fn attributes() {
        let session = BuildSession::test();
        let component = Component::test();

        assert_eq(
            "== @static @unsafe 3 @static (increment 1)",
            Expression::new(
                default(),
                default(),
                hir::Application {
                    callee: Expression::new(
                        default(),
                        default(),
                        hir::Application {
                            callee: Identifier::parameter("==").into_expression(),
                            argument: Expression::new(
                                Attributes(vec![
                                    Attribute::stripped(AttributeKind::Static),
                                    Attribute::stripped(AttributeKind::Unsafe),
                                ]),
                                Span::default(),
                                Number::Nat(3u8.into()).into(),
                            ),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    argument: Expression::new(
                        Attributes(vec![Attribute::stripped(AttributeKind::Static)]),
                        default(),
                        hir::Application {
                            callee: Identifier::parameter("increment").into_expression(),
                            argument: Expression::new(
                                default(),
                                default(),
                                Number::Nat(1u8.into()).into(),
                            ),
                            explicitness: Explicit,
                        }
                        .into(),
                    ),
                    explicitness: Explicit,
                }
                .into(),
            )
            .with((&component, &session))
            .to_string(),
        )
    }

    #[test]
    fn path() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let overarching = component.add("overarching", EntityKind::module());
        let middle = component.add_below(
            "middle",
            EntityKind::module(),
            overarching.local_declaration_index(&component).unwrap(),
        );
        let sink = component.add_below(
            "sink",
            EntityKind::UntypedFunction,
            middle.local_declaration_index(&component).unwrap(),
        );

        assert_eq(
            "topmost.overarching.middle.sink",
            sink.into_expression()
                .with((&component, &session))
                .to_string(),
        );
    }

    #[test]
    fn path_identifier_punctuation_punctuation_identifier_segments() {
        let session = BuildSession::test();
        let mut component = Component::test();

        let overarching = component.add("overarching", EntityKind::module());
        let noisy = component.add_below(
            "&/.~##",
            EntityKind::module(),
            overarching.local_declaration_index(&component).unwrap(),
        );
        let zickzack = component.add_below(
            "^^^",
            EntityKind::module(),
            noisy.local_declaration_index(&component).unwrap(),
        );
        let sink = component.add_below(
            "sink",
            EntityKind::UntypedFunction,
            zickzack.local_declaration_index(&component).unwrap(),
        );

        assert_eq(
            "topmost.overarching.&/.~## . ^^^ .sink",
            sink.into_expression()
                .with((&component, &session))
                .to_string(),
        );
    }
}
