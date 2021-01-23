//! The definition of the textual representation of the [HIR](crate::hir).

use super::{CrateScope, Declaration, Expression, Pattern};
use crate::{ast::Implicit, support::DisplayWith};
use joinery::JoinableIterator;
use std::{default::default, fmt};

impl DisplayWith for Declaration {
    type Linchpin = CrateScope;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(linchpin, 0, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(
        &self,
        scope: &CrateScope,
        depth: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use super::DeclarationKind::*;
        use crate::INDENTATION_IN_SPACES;

        match &self.kind {
            Value(declaration) => {
                write!(
                    f,
                    "{}: {}",
                    declaration.binder,
                    declaration.type_annotation.with(scope)
                )?;
                if let Some(expression) = &declaration.expression {
                    write!(f, " = {}", expression.with(scope))?;
                }
                writeln!(f)
            }
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder,
                        declaration.type_annotation.with(scope)
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION_IN_SPACES),
                            constructor.with(scope)
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder,
                    declaration.type_annotation.with(scope)
                ),
            },
            Constructor(constructor) => writeln!(
                f,
                "{}: {}",
                constructor.binder,
                constructor.type_annotation.with(scope)
            ),
            Module(declaration) => {
                writeln!(f, "module {} =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(scope, depth, f)?;
                }
                Ok(())
            }
            Use(declaration) => match &declaration.binder {
                Some(binder) => writeln!(f, "use {} as {}", declaration.target, binder),
                None => writeln!(f, "use {}", declaration.target),
            },
            Invalid => writeln!(f, "?(invalid)"),
        }
    }
}

// @Note many wasted allocations (intermediate Strings)
impl DisplayWith for Expression {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format_pi_type_literal_or_lower(self, scope, f)
    }
}

fn format_pi_type_literal_or_lower(
    expression: &Expression,
    scope: &CrateScope,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

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
    // an equal amount of brackets (that being one). This is not the case for `crate.take (\it => it)` where
    // which we *might* want to print as `crate.take \it => it`. This would probably require passing some flags to
    // the formatting functions and adding more checks.
    //
    // Also see `crate::parser::test::application_lambda_literal_argument_{lax,strict}_grouping` and the
    // comment at the grammar definition of `Pi-Type-Literal-Or-Lower` (in `misc/grammar/lushui.grammar`)
    // for further details.
    match &expression.kind {
        PiType(pi) => {
            let domain_needs_brackets = pi.parameter.is_some() || pi.aspect != default();

            // @Task add tests to check if parameter aspect is handled correctly
            if domain_needs_brackets {
                write!(f, "(")?;
                write!(f, "{}", pi.aspect)?;

                if let Some(parameter) = &pi.parameter {
                    write!(f, "{parameter}: ")?;
                }

                write!(f, "{}", pi.domain.with(scope))?;
                write!(f, ")")?;
            } else {
                format_application_or_lower(&pi.domain, scope, f)?;
            }
            write!(f, " -> ")?;
            format_pi_type_literal_or_lower(&pi.codomain, scope, f)
        }
        Lambda(lambda) => {
            write!(f, r"\")?;
            let parameter_needs_brackets = lambda.parameter_type_annotation.is_some()
                || lambda.explicitness == Implicit
                || lambda.laziness.is_some();

            if parameter_needs_brackets {
                write!(f, "({}", lambda.explicitness)?;
                if lambda.laziness.is_some() {
                    write!(f, "lazy ")?;
                }
                write!(f, "{}", lambda.parameter)?;
                if let Some(annotation) = &lambda.parameter_type_annotation {
                    write!(f, ": {}", annotation.with(scope))?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(f, ": {}", annotation.with(scope))?;
            }

            write!(f, " => {}", lambda.body.with(scope))
        }
        UseIn => todo!(),
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            writeln!(f, "case {} of", analysis.subject.with(scope))?;
            for case in &analysis.cases {
                writeln!(
                    f,
                    "{} => {}",
                    case.pattern.with(scope),
                    case.body.with(scope)
                )?;
            }
            Ok(())
        }
        _ => format_application_or_lower(expression, scope, f),
    }
}

// @Task write named arguments
fn format_application_or_lower(
    expression: &Expression,
    scope: &CrateScope,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    match &expression.kind {
        Application(application) => {
            format_application_or_lower(&application.callee, scope, f)?;
            write!(f, " ")?;
            if application.explicitness == Implicit
            /*|| application.binder.is_some()*/
            {
                write!(
                    f,
                    "({}{})",
                    application.explicitness,
                    application.argument.with(scope)
                )
            } else {
                format_lower_expression(&application.argument, scope, f)
            }
        }
        ForeignApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                format_lower_expression(argument, scope, f)?;
            }

            Ok(())
        }
        _ => format_lower_expression(expression, scope, f),
    }
}

fn format_lower_expression(
    expression: &Expression,
    scope: &CrateScope,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    for attribute in expression.attributes.iter() {
        write!(f, "{} ", attribute)?;
    }

    match &expression.kind {
        Type => write!(f, "Type"),
        Number(literal) => write!(f, "{}", literal),
        // @Bug this uses Rust's way of printing strings, not Lushui's:
        // The escape sequences differ
        // @Task use custom escaping logic
        Text(literal) => write!(f, "{:?}", literal),
        Binding(binding) => write!(
            f,
            "{}",
            super::FunctionScope::absolute_path(&binding.binder, scope)
        ),
        // @Beacon @Temporary @Task just write out the path
        Projection(_projection) => write!(f, "?(projection)"),
        IO(io) => write!(
            f,
            "?(io {} {})",
            io.index,
            io.arguments
                .iter()
                .map(|argument| argument.with(scope))
                .join_with(' ')
        ),
        Substitution(substitution) => write!(
            f,
            "?(substitution {} {})",
            substitution.substitution.with(scope),
            substitution.expression.with(scope)
        ),
        Invalid => write!(f, "?(invalid)"),
        _ => write!(f, "({})", expression.with(scope)),
    }
}

// @Task @Beacon update bracket business
impl DisplayWith for Pattern {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(
                f,
                "{}",
                super::FunctionScope::absolute_path(&binding.binder, scope)
            ),

            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => write!(
                f,
                "({}) ({})",
                application.callee.with(scope),
                application.argument.with(scope)
            ),
            Invalid => write!(f, "?(invalid)"),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{
        ast::{self, Explicitness::*},
        entity::{Entity, EntityKind},
        hir::{expr, Expression},
        lowered_ast::{Attribute, AttributeKind, Attributes, Number},
        resolver::{CrateIndex, CrateScope, Exposure, Identifier},
        span::Span,
        support::DisplayWith,
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
                difference::Changeset::new(expected, actual, "")
            );
        }
    }

    impl CrateScope {
        fn new() -> Self {
            let mut scope = Self::default();
            scope.bindings.push(Entity {
                source: ast::Identifier::new("test".into(), Span::SHAM),
                parent: None,
                exposure: Exposure::Unrestricted,
                kind: EntityKind::module(),
            });
            scope
        }

        fn add(&mut self, name: &str, kind: EntityKind) -> Identifier {
            self.add_below(name, kind, self.root())
        }

        fn add_below(&mut self, name: &str, kind: EntityKind, parent: CrateIndex) -> Identifier {
            let identifier = ast::Identifier::new(name.into(), Span::SHAM);
            let entity = Entity {
                source: identifier.clone(),
                parent: Some(parent),
                exposure: Exposure::Unrestricted,
                kind,
            };
            let index = self.bindings.push(entity);
            Identifier::new(index, identifier)
        }
    }

    fn type_() -> Expression {
        expr! {
            Type {
                Attributes::default(), Span::SHAM
            }
        }
    }

    impl Attribute {
        const fn sham(kind: AttributeKind) -> Self {
            Self::new(Span::SHAM, kind)
        }
    }

    #[test]
    fn pi_type_application_argument() {
        let mut scope = CrateScope::new();

        let array = scope
            .add("Array", EntityKind::untyped_data_type())
            .to_expression();
        let int = scope
            .add("Int", EntityKind::untyped_data_type())
            .to_expression();

        assert_eq(
            "crate.Array crate.Int -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: default(),
                    parameter: None,
                    domain: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: array,
                            argument: int,
                            explicitness: Explicit,
                        }
                    },
                    codomain: type_(),
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn pi_type_named_parameter() {
        let mut scope = CrateScope::new();

        let array = scope.add("Array", EntityKind::untyped_data_type());
        let int = scope.add("Int", EntityKind::untyped_data_type());
        let container = scope.add("Container", EntityKind::untyped_data_type());
        let alpha = Identifier::parameter("alpha");

        assert_eq(
            "(alpha: crate.Array crate.Int) -> crate.Container alpha",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: default(),
                    parameter: Some(alpha.clone()),
                    domain: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: array.to_expression(),
                            argument: int.to_expression(),
                            explicitness: Explicit,
                        }
                    },
                    codomain: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: container.to_expression(),
                            argument: alpha.to_expression(),
                            explicitness: Explicit,
                        }
                    },
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn pi_type_implicit_parameter() {
        let scope = CrateScope::new();

        assert_eq(
            "(,whatever: Type) -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: Implicit.into(),
                    parameter: Some(Identifier::parameter("whatever")),
                    domain: type_(),
                    codomain: type_(),
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [pi_type_two_curried_arguments].
    #[test]
    fn pi_type_higher_order_argument() {
        let mut scope = CrateScope::new();
        let int = scope
            .add("Int", EntityKind::untyped_data_type())
            .to_expression();

        assert_eq(
            "(crate.Int -> crate.Int) -> crate.Int",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: default(),
                    parameter: None,
                    domain: expr! {
                        PiType {
                            Attributes::default(), Span::SHAM;
                            aspect: default(),
                            parameter: None,
                            domain: int.clone(),
                            codomain: int.clone(),
                        }
                    },
                    codomain: int,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [pi_type_higher_order_argument].
    #[test]
    fn pi_type_two_curried_arguments() {
        let mut scope = CrateScope::new();
        let int = scope
            .add("Int", EntityKind::untyped_data_type())
            .to_expression();
        let text = scope
            .add("Text", EntityKind::untyped_data_type())
            .to_expression();

        assert_eq(
            "crate.Int -> crate.Text -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: default(),
                    parameter: None,
                    domain: int,
                    codomain: expr! {
                        PiType {
                            Attributes::default(), Span::SHAM;
                            aspect: default(),
                            parameter: None,
                            domain: text,
                            codomain: expr! {
                                Type {
                                    Attributes::default(), Span::SHAM
                                }
                            },
                        }
                    },
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [lambda_pi_type_body].
    #[test]
    fn pi_type_lambda_domain() {
        let scope = CrateScope::new();

        let x = Identifier::parameter("x");

        assert_eq(
            r"(\x => x) -> Type",
            (expr! {
                PiType {
                    Attributes::default(), Span::SHAM;
                    aspect: default(),
                    parameter: None,
                    domain: expr! {
                        Lambda {
                            Attributes::default(), Span::SHAM;
                            parameter: x.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            body: x.to_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                    },
                    codomain: type_(),
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn application_three_curried_arguments() {
        let mut scope = CrateScope::new();

        let beta = scope.add("beta", EntityKind::UntypedValue);

        assert_eq(
            "alpha crate.beta (gamma Type) 0",
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: expr! {
                                Application {
                                    Attributes::default(), Span::SHAM;
                                    callee: Identifier::parameter("alpha").to_expression(),
                                    argument: beta.to_expression(),
                                    explicitness: Explicit,
                                }
                            },
                            argument: expr! {
                                Application {
                                    Attributes::default(), Span::SHAM;
                                    callee: Identifier::parameter("gamma").to_expression(),
                                    argument: type_(),
                                    explicitness: Explicit,
                                }
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Number(Attributes::default(), Span::SHAM; Number::Nat(0u8.into()))
                    },
                    explicitness: Explicit,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [application_lambda_argument].
    #[test]
    fn application_lambda_last_argument() {
        let mut scope = CrateScope::new();

        let take = scope.add("take", EntityKind::UntypedValue);
        let it = Identifier::parameter("it");

        // we might want to format this special case as `crate.take \it => it` in the future
        assert_eq(
            r"crate.take (\it => it)",
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: take.to_expression(),
                    argument: expr! {
                        Lambda {
                            Attributes::default(), Span::SHAM;
                            parameter: it.clone(),
                            parameter_type_annotation: None,
                            body_type_annotation: None,
                            // technically not correct
                            body: it.to_expression(),
                            explicitness: Explicit,
                            laziness: None,
                        }
                    },
                    explicitness: Explicit,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [application_lambda_last_argument].
    #[test]
    fn application_lambda_argument() {
        let mut scope = CrateScope::new();

        let take = scope.add("take", EntityKind::UntypedValue);
        let it = Identifier::parameter("it");

        assert_eq(
            r#"crate.take (\it => it) "who""#,
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: take.to_expression(),
                            argument: expr! {
                                Lambda {
                                    Attributes::default(), Span::SHAM;
                                    parameter: it.clone(),
                                    parameter_type_annotation: None,
                                    body_type_annotation: None,
                                    // technically not correct
                                    body: it.to_expression(),
                                    explicitness: Explicit,
                                    laziness: None,
                                }
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Text(
                            Attributes::default(), Span::SHAM;
                            String::from("who"),
                        )
                    },
                    explicitness: Explicit,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn application_implicit_argument() {
        let mut scope = CrateScope::new();

        let identity = scope.add("identity", EntityKind::UntypedValue);

        assert_eq(
            r"crate.identity (,Type)",
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: identity.to_expression(),
                    argument: type_(),
                    explicitness: Implicit,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn application_foreign_application_callee() {
        let scope = CrateScope::new();

        assert_eq(
            "eta 10 omicron",
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: expr! {
                        ForeignApplication {
                            Attributes::default(), Span::SHAM;
                            callee: Identifier::parameter("eta"),
                            arguments: vec![
                                expr! {
                                    Number(Attributes::default(), Span::SHAM; Number::Nat(10u8.into()))
                                }
                            ],
                        }
                    },
                    argument: Identifier::parameter("omicron").to_expression(),
                    explicitness: Explicit,
                }
            }).with(&scope).to_string(),
        );
    }

    #[test]
    fn lambda_body_type_annotation() {
        let mut scope = CrateScope::new();

        let output = scope.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\input: crate.Output => 0",
            (expr! {
                Lambda {
                    Attributes::default(), Span::SHAM;
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: None,
                    body_type_annotation: Some(output.to_expression()),
                    body: expr! {
                        Number(Attributes::default(), Span::SHAM; Number::Nat(0u8.into()))
                    },
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn lambda_parameter_type_annotation_body_type_annotation() {
        let mut scope = CrateScope::new();

        let input = scope.add("Input", EntityKind::untyped_data_type());
        let output = scope.add("Output", EntityKind::untyped_data_type());

        assert_eq(
            r"\(input: crate.Input): crate.Output => Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::SHAM;
                    parameter: Identifier::parameter("input"),
                    parameter_type_annotation: Some(input.to_expression()),
                    body_type_annotation: Some(output.to_expression()),
                    body: type_(),
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn lambda_implicit_parameter() {
        let scope = CrateScope::new();

        assert_eq(
            r"\(,Input: Type) => Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::SHAM;
                    parameter: Identifier::parameter("Input"),
                    parameter_type_annotation: Some(type_()),
                    body_type_annotation: None,
                    body: type_(),
                    explicitness: Implicit,
                    laziness: None,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    /// Compare with [pi_type_lambda_domain].
    #[test]
    fn lambda_pi_type_body() {
        let scope = CrateScope::new();

        let x = Identifier::parameter("x");

        assert_eq(
            r"\x => x -> Type",
            (expr! {
                Lambda {
                    Attributes::default(), Span::SHAM;
                    parameter: x.clone(),
                    parameter_type_annotation: None,
                    body_type_annotation: None,
                    body: expr! {
                        PiType {
                            Attributes::default(), Span::SHAM;
                            aspect: default(),
                            parameter: None,
                            domain: x.to_expression(),
                            codomain: type_(),
                        }
                    },
                    explicitness: Explicit,
                    laziness: None,
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn foreign_application_no_arguments() {
        let mut scope = CrateScope::new();

        let add = scope.add("add", EntityKind::UntypedValue);

        assert_eq(
            "add",
            (expr! {
                ForeignApplication {
                    Attributes::default(), Span::SHAM;
                    callee: add,
                    arguments: Vec::new(),
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn foreign_application_two_arguments() {
        let mut scope = CrateScope::new();

        let add = scope.add("add", EntityKind::UntypedValue);

        assert_eq(
            "add (add 1 3000) 0",
            (expr! {
                ForeignApplication {
                    Attributes::default(), Span::SHAM;
                    callee: add.clone(),
                    arguments: vec![
                        expr! {
                            ForeignApplication {
                                Attributes::default(), Span::SHAM;
                                callee: add,
                                arguments: vec![
                                    expr! {
                                        Number(
                                            Attributes::default(), Span::SHAM;
                                            Number::Nat(1u8.into()),
                                        )
                                    },
                                    expr! {
                                        Number(
                                            Attributes::default(), Span::SHAM;
                                            Number::Nat(3000u16.into()),
                                        )
                                    },
                                ],
                            }
                        },
                        expr! {
                            Number(
                                Attributes::default(), Span::SHAM;
                                Number::Nat(0u8.into()),
                            )
                        },
                    ],
                }
            })
            .with(&scope)
            .to_string(),
        );
    }

    #[test]
    fn attributes() {
        let scope = CrateScope::new();

        assert_eq(
            "== @static @unsafe 3 @static (increment 1)",
            (expr! {
                Application {
                    Attributes::default(), Span::SHAM;
                    callee: expr! {
                        Application {
                            Attributes::default(), Span::SHAM;
                            callee: Identifier::parameter("==").to_expression(),
                            argument: expr! {
                                Number(
                                    Attributes::new(vec![
                                        Attribute::sham(AttributeKind::Static),
                                        Attribute::sham(AttributeKind::Unsafe)
                                    ]),
                                    Span::SHAM;
                                    Number::Nat(3u8.into()),
                                )
                            },
                            explicitness: Explicit,
                        }
                    },
                    argument: expr! {
                        Application {
                            Attributes::new(vec![Attribute::sham(AttributeKind::Static)]), Span::SHAM;
                            callee: Identifier::parameter("increment").to_expression(),
                            argument: expr! {
                                Number(Attributes::default(), Span::SHAM; Number::Nat(1u8.into()))
                            },
                            explicitness: Explicit,
                        }
                    },
                    explicitness: Explicit,
                }
            })
            .with(&scope)
            .to_string(),
        )
    }

    #[test]
    fn path() {
        let mut scope = CrateScope::new();

        let overarching = scope.add("overarching", EntityKind::module());
        let middle = scope.add_below(
            "middle",
            EntityKind::module(),
            overarching.crate_index().unwrap(),
        );
        let sink = scope.add_below(
            "sink",
            EntityKind::UntypedValue,
            middle.crate_index().unwrap(),
        );

        assert_eq(
            "crate.overarching.middle.sink",
            sink.to_expression().with(&scope).to_string(),
        );
    }

    #[test]
    fn path_identifier_punctuation_punctuation_identifier_segments() {
        let mut scope = CrateScope::new();

        let overarching = scope.add("overarching", EntityKind::module());
        let noisy = scope.add_below(
            "&/.~##",
            EntityKind::module(),
            overarching.crate_index().unwrap(),
        );
        let zickzack = scope.add_below("^^^", EntityKind::module(), noisy.crate_index().unwrap());
        let sink = scope.add_below(
            "sink",
            EntityKind::UntypedValue,
            zickzack.crate_index().unwrap(),
        );

        assert_eq(
            "crate.overarching.&/.~## . ^^^ .sink",
            sink.to_expression().with(&scope).to_string(),
        );
    }
}
