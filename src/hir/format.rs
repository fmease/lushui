//! The definition of the textual representation of the [HIR](crate::hir).

use super::{Crate, Declaration, Expression, Pattern};
use crate::{format::DisplayWith, package::BuildSession};
use joinery::JoinableIterator;
use std::fmt;

impl DisplayWith for Declaration {
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(
        &self,
        (crate_, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        self.format_with_depth(crate_, session, 0, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(
        &self,
        crate_: &Crate,
        session: &BuildSession,
        depth: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use super::DeclarationKind::*;
        use crate::syntax::lexer::INDENTATION;
        let context = (crate_, session);

        match &self.value {
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
                    declaration.format_with_depth(crate_, session, depth, f)?;
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
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(
        &self,
        (crate_, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        format_pi_type_literal_or_lower(self, crate_, session, f)
    }
}

fn format_pi_type_literal_or_lower(
    expression: &Expression,
    crate_: &Crate,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;
    let context = (crate_, session);

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
    // an equal amount of brackets (that being one). This is not the case for `crate.take (\it => it)` which
    // we *might* want to print as `crate.take \it => it`. This would probably require passing some flags to
    // the formatting functions and adding more checks.
    //
    // See also `crate::parser::test::application_lambda_literal_argument_{lax,strict}_grouping` and the
    // comment at the grammar definition of `Pi-Type-Literal-Or-Lower` (in `/misc/grammar/lushui.grammar`)
    // for further details.
    match &expression.value {
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
                format_application_or_lower(&pi.domain, crate_, session, f)?;
            }
            write!(f, " -> ")?;
            format_pi_type_literal_or_lower(&pi.codomain, crate_, session, f)
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
            writeln!(f, "case {} of", analysis.subject.with(context))?;
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
        _ => format_application_or_lower(expression, crate_, session, f),
    }
}

// @Task write named arguments
fn format_application_or_lower(
    expression: &Expression,
    crate_: &Crate,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    match &expression.value {
        Application(application) => {
            format_application_or_lower(&application.callee, crate_, session, f)?;
            write!(f, " {}", application.explicitness)?;
            format_lower_expression(&application.argument, crate_, session, f)
        }
        IntrinsicApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                format_lower_expression(argument, crate_, session, f)?;
            }

            Ok(())
        }
        _ => format_lower_expression(expression, crate_, session, f),
    }
}

fn format_lower_expression(
    expression: &Expression,
    crate_: &Crate,
    session: &BuildSession,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;
    let context = (crate_, session);

    for attribute in &expression.attributes.0 {
        write!(f, "{} ", attribute)?;
    }

    match &expression.value {
        Type => write!(f, "Type"),
        Number(literal) => write!(f, "{}", literal),
        // @Bug this uses Rust's way of printing strings, not Lushui's:
        // The escape sequences differ
        // @Task use custom escaping logic
        Text(literal) => write!(f, "{:?}", literal),
        Binding(binding) => write!(
            f,
            "{}",
            super::FunctionScope::absolute_path_to_string(&binding.binder, crate_, session)
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
    type Context<'a> = (&'a Crate, &'a BuildSession);

    fn format(
        &self,
        context @ (crate_, session): Self::Context<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use super::PatternKind::*;

        match &self.value {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(
                f,
                "{}",
                super::FunctionScope::absolute_path_to_string(&binding.binder, crate_, session)
            ),

            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => write!(
                f,
                "({}) ({})",
                application.callee.with(context),
                application.argument.with(context)
            ),
            Error => write!(f, "?(error)"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        entity::{Entity, EntityKind},
        format::DisplayWith,
        hir::{expr, Expression, Identifier, LocalDeclarationIndex},
        package::{BuildSession, CrateIndex, CrateType, PackageIndex},
        resolver::{Crate, Exposure},
        span::Span,
        syntax::{
            ast::{self, Explicitness::*},
            lowered_ast::{
                attributes::{Attribute, AttributeKind, Attributes},
                Number,
            },
            CrateName,
        },
    };
    use std::{default::default, path::PathBuf};

    // @Beacon @Task do something smart if spaces differ (which cannot have color)
    // like replacing them with a different character like the Unicode space symbol
    // or however it is called ("SP")
    fn assert_eq(expected: &str, actual: impl AsRef<str>) {
        let actual = actual.as_ref();

        if actual != expected {
            panic!(
                "the actual textual representation of the HIR node does not match the expected one:\n{}",
                difference::Changeset::new(expected, actual, "")
            );
        }
    }

    const CRATE_INDEX: CrateIndex = CrateIndex(0);
    const PACKAGE_INDEX: PackageIndex = PackageIndex(0);

    impl Crate {
        fn test() -> Self {
            let mut crate_ = Self::new(
                CrateName::parse("test").ok().unwrap(),
                CRATE_INDEX,
                PACKAGE_INDEX,
                PathBuf::new(),
                CrateType::Library,
            );
            crate_.bindings.insert(Entity {
                source: ast::Identifier::new_unchecked("test".into(), default()),
                parent: None,
                exposure: Exposure::Unrestricted,
                kind: EntityKind::module(),
                attributes: default(),
            });
            crate_
        }

        fn add(&mut self, name: &str, kind: EntityKind) -> Identifier {
            self.add_below(name, kind, self.root())
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
        expr! {
            Type {
                Attributes::default(), Span::default()
            }
        }
    }

    impl Attribute {
        fn stripped(kind: AttributeKind) -> Self {
            Self::new(default(), kind)
        }
    }

    #[test]
    fn pi_type_application_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let array = crate_
            .add("Array", EntityKind::untyped_data_type())
            .into_expression();
        let int = crate_
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "crate.Array crate.Int -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: array,
                            argument: int,
                            explicitness: Explicit,
                        }
                    },
                    codomain: type_(),
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn pi_type_named_parameter() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let array = crate_.add("Array", EntityKind::untyped_data_type());
        let int = crate_.add("Int", EntityKind::untyped_data_type());
        let container = crate_.add("Container", EntityKind::untyped_data_type());
        let alpha = Identifier::parameter("alpha");

        assert_eq(
            "(alpha: crate.Array crate.Int) -> crate.Container alpha",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Explicit,
                    laziness: None,
                    parameter: Some(alpha.clone()),
                    domain: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: array.into_expression(),
                            argument: int.into_expression(),
                            explicitness: Explicit,
                        }
                    },
                    codomain: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: container.into_expression(),
                            argument: alpha.into_expression(),
                            explicitness: Explicit,
                        }
                    },
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn pi_type_implicit_parameter() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        assert_eq(
            "'(whatever: Type) -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Implicit,
                    laziness: None,
                    parameter: Some(Identifier::parameter("whatever")),
                    domain: type_(),
                    codomain: type_(),
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_two_curried_arguments].
    #[test]
    fn pi_type_higher_order_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();
        let int = crate_
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "(crate.Int -> crate.Int) -> crate.Int",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: expr! {
                        PiType {
                            Attributes::default(), Span::default();
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: int.clone(),
                            codomain: int.clone(),
                        }
                    },
                    codomain: int,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_higher_order_argument].
    #[test]
    fn pi_type_two_curried_arguments() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();
        let int = crate_
            .add("Int", EntityKind::untyped_data_type())
            .into_expression();
        let text = crate_
            .add("Text", EntityKind::untyped_data_type())
            .into_expression();

        assert_eq(
            "crate.Int -> crate.Text -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: int,
                    codomain: expr! {
                        PiType {
                            Attributes::default(), Span::default();
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: text,
                            codomain: expr! {
                                Type {
                                    Attributes::default(), Span::default()
                                }
                            },
                        }
                    },
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [lambda_pi_type_body].
    #[test]
    fn pi_type_lambda_domain() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        let x = Identifier::parameter("x");

        assert_eq(
            r"(\x => x) -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::default();
                    explicitness: Explicit,
                    laziness: None,
                    parameter: None,
                    domain: expr! {
                        Lambda {
                            Attributes::default(), Span::default();
                            parameter: x.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            body: x.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                    },
                    codomain: type_(),
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_three_curried_arguments() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let beta = crate_.add("beta", EntityKind::UntypedFunction);

        assert_eq(
            "alpha crate.beta (gamma Type) 0",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: expr! {
                                Application {
                                    Attributes::default(), Span::default();
                                    callee: Identifier::parameter("alpha").into_expression(),
                                    argument: beta.into_expression(),
                                    explicitness: Explicit,
                                }
                            },
                            argument: expr! {
                                Application {
                                    Attributes::default(), Span::default();
                                    callee: Identifier::parameter("gamma").into_expression(),
                                    argument: type_(),
                                    explicitness: Explicit,
                                }
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Number(Attributes::default(), Span::default(); Number::Nat(0u8.into()))
                    },
                    explicitness: Explicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [application_lambda_argument].
    #[test]
    fn application_lambda_last_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let take = crate_.add("take", EntityKind::UntypedFunction);
        let it = Identifier::parameter("it");

        // we might want to format this special case as `crate.take \it => it` in the future
        assert_eq(
            r"crate.take (\it => it)",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: take.into_expression(),
                    argument: expr! {
                        Lambda {
                            Attributes::default(), Span::default();
                            parameter: it.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            // technically not correct
                            body: it.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                    },
                    explicitness: Explicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [application_lambda_last_argument].
    #[test]
    fn application_lambda_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let take = crate_.add("take", EntityKind::UntypedFunction);
        let it = Identifier::parameter("it");

        assert_eq(
            r#"crate.take (\it => it) "who""#,
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: take.into_expression(),
                            argument: expr! {
                                Lambda {
                                    Attributes::default(), Span::default();
                                    parameter: it.clone(),
                                    parameter_type_annotation: None,
                                    body_type_annotation: None,
                                    // technically not correct
                                    body: it.into_expression(),
                                    explicitness: Explicit,
                                    laziness: None,
                                }
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Text(
                            Attributes::default(), Span::default();
                            String::from("who"),
                        )
                    },
                    explicitness: Explicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_implicit_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let identity = crate_.add("identity", EntityKind::UntypedFunction);

        assert_eq(
            r"crate.identity 'Type",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: identity.into_expression(),
                    argument: type_(),
                    explicitness: Implicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_complex_implicit_argument() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let identity = crate_.add("identity", EntityKind::UntypedFunction);
        let text = crate_.add("Text", EntityKind::untyped_data_type());

        assert_eq(
            r"crate.identity '(prepare crate.Text)",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: identity.into_expression(),
                    argument: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: Identifier::parameter("prepare").into_expression(),
                            argument: text.into_expression(),
                            explicitness: Explicit,
                        }
                    },
                    explicitness: Implicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn application_intrinsic_application_callee() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        assert_eq(
            "eta 10 omicron",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: expr! {
                        IntrinsicApplication {
                            Attributes::default(), Span::default();
                            callee: Identifier::parameter("eta"),
                            arguments: vec![
                                expr! {
                                    Number(Attributes::default(), Span::default(); Number::Nat(10u8.into()))
                                }
                            ],
                        }
                    },
                    argument: Identifier::parameter("omicron").into_expression(),
                    explicitness: Explicit,
                }
            }).with((&crate_, &session)).to_string(),
        );
    }

    #[test]
    fn lambda_body_type_annotation() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let output = crate_.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\input: crate.Output => 0",
            (expr! {
                Lambda {
                    Attributes::default(), Span::default();
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: None,
                    body_type_annotation: Some(output.into_expression()),
                    body: expr! {
                        Number(Attributes::default(), Span::default(); Number::Nat(0u8.into()))
                    },
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_parameter_type_annotation_body_type_annotation() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let input = crate_.add("Input", EntityKind::untyped_data_type());
        let output = crate_.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\(input: crate.Input): crate.Output => Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::default();
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: Some(input.into_expression()),
                    body_type_annotation: Some(output.into_expression()),
                    body: type_(),
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_implicit_parameter() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        assert_eq(
            r"\'(Input: Type) => Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::default();
                    parameter: Identifier::parameter("Input"),
                    parameter_type_annotation: Some(type_()),
                    body_type_annotation: None,
                    body: type_(),
                    explicitness: Implicit,
                    laziness: None,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn lambda_implicit_unannotated_parameter() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();
        let a = Identifier::parameter("a");

        assert_eq(
            r"\'A => \a => a",
            (expr! {
                Lambda {
                    Attributes::default(), Span::default();
                    parameter: Identifier::parameter("A"),
                    parameter_type_annotation: None,
                    body_type_annotation: None,
                    body: expr! {
                        Lambda {
                            Attributes::default(), Span::default();
                            parameter: a.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            body: a.into_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                    },
                    explicitness: Implicit,
                    laziness: None,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    /// Compare with [pi_type_lambda_domain].
    #[test]
    fn lambda_pi_type_body() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        let x = Identifier::parameter("x");

        assert_eq(
            r"\x => x -> Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::default();
                    parameter: x.clone(),
                    parameter_type_annotation: None,
                    body_type_annotation: None,
                    body: expr! {
                        PiType {
                            Attributes::default(), Span::default();
                            explicitness: Explicit,
                            laziness: None,
                            parameter: None,
                            domain: x.into_expression(),
                            codomain: type_(),
                        }
                    },
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn intrinsic_application_no_arguments() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let add = crate_.add("add", EntityKind::UntypedFunction);

        assert_eq(
            "add",
            (expr! {
                IntrinsicApplication {
                    Attributes::default(), Span::default();
                    callee: add,
                    arguments: Vec::new(),
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn intrinsic_application_two_arguments() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let add = crate_.add("add", EntityKind::UntypedFunction);

        assert_eq(
            "add (add 1 3000) 0",
            (expr! {
                IntrinsicApplication {
                    Attributes::default(), Span::default();
                    callee: add.clone(),
                    arguments: vec![
                        expr! {
                            IntrinsicApplication {
                                Attributes::default(), Span::default();
                                callee: add,
                                arguments: vec![
                                    expr! {
                                        Number(
                                            Attributes::default(), Span::default();
                                            Number::Nat(1u8.into()),
                                        )
                                    },
                                    expr! {
                                        Number(
                                            Attributes::default(), Span::default();
                                            Number::Nat(3000u16.into()),
                                        )
                                    },
                                ],
                            }
                        },
                        expr! {
                            Number(
                                Attributes::default(), Span::default();
                                Number::Nat(0u8.into()),
                            )
                        },
                    ],
                }
            })
            .with((&crate_, &session))
            .to_string(),
        );
    }

    #[test]
    fn attributes() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let crate_ = Crate::test();

        assert_eq(
            "== @static @unsafe 3 @static (increment 1)",
            (expr! {
                Application {
                    Attributes::default(), Span::default();
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::default();
                            callee: Identifier::parameter("==").into_expression(),
                            argument: expr! {
                                Number(
                                    Attributes(vec![
                                        Attribute::stripped(AttributeKind::Static),
                                        Attribute::stripped(AttributeKind::Unsafe)
                                    ]),
                                    Span::default();
                                    Number::Nat(3u8.into()),
                                )
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Application {
                            Attributes(vec![Attribute::stripped(AttributeKind::Static)]), Span::default();
                            callee: Identifier::parameter("increment").into_expression(),
                            argument: expr! {
                                Number(Attributes::default(), Span::default(); Number::Nat(1u8.into()))
                            },
                            explicitness: Explicit,
                        }
                    },
                    explicitness: Explicit,
                }
            })
            .with((&crate_, &session))
            .to_string(),
        )
    }

    #[test]
    fn path() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let overarching = crate_.add("overarching", EntityKind::module());
        let middle = crate_.add_below(
            "middle",
            EntityKind::module(),
            overarching.local_declaration_index(&crate_).unwrap(),
        );
        let sink = crate_.add_below(
            "sink",
            EntityKind::UntypedFunction,
            middle.local_declaration_index(&crate_).unwrap(),
        );

        assert_eq(
            "crate.overarching.middle.sink",
            sink.into_expression().with((&crate_, &session)).to_string(),
        );
    }

    #[test]
    fn path_identifier_punctuation_punctuation_identifier_segments() {
        let session = BuildSession::empty(CRATE_INDEX, PACKAGE_INDEX);
        let mut crate_ = Crate::test();

        let overarching = crate_.add("overarching", EntityKind::module());
        let noisy = crate_.add_below(
            "&/.~##",
            EntityKind::module(),
            overarching.local_declaration_index(&crate_).unwrap(),
        );
        let zickzack = crate_.add_below(
            "^^^",
            EntityKind::module(),
            noisy.local_declaration_index(&crate_).unwrap(),
        );
        let sink = crate_.add_below(
            "sink",
            EntityKind::UntypedFunction,
            zickzack.local_declaration_index(&crate_).unwrap(),
        );

        assert_eq(
            "crate.overarching.&/.~## . ^^^ .sink",
            sink.into_expression().with((&crate_, &session)).to_string(),
        );
    }
}
