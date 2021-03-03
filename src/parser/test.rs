//! Parser tests.
//!
//! Intended for testing edge cases. The majority of parsing tests should be
//! golden UI tests ([crate::golden], `tests/`).

// @Question maybe create builders for the elements instead of using macros?

use std::rc::Rc;

use super::{
    ast::{
        decl, expr, Attribute, Attributes, Declaration, Domain, Explicitness::*, Expression,
        Format, Identifier, Item, ParameterGroup, Path, UsePathTree, UsePathTreeKind,
    },
    Parser, Result,
};
use crate::{
    diagnostics::Diagnostics,
    lexer::Lexer,
    smallvec,
    span::{span, SourceFile, Span},
};
use std::default::default;

// @Question @Bug why doesn't this seemingly return an Err(()) on error but a partial AST?
fn parse_expression(source: &str) -> Result<Expression> {
    let mut diagnostics = Diagnostics::default();
    let file = Rc::new(SourceFile::fake(source.to_owned()));
    let lexer = Lexer::new(&file, &mut diagnostics);
    let tokens = lexer.lex().map_err(|_| ())?;
    let mut parser = Parser::new(file, &tokens, &mut diagnostics);
    parser.parse_expression()
}

// @Task improve API: don't return file @Note that's possible once we use
// source file indices indead of Rc<SourceFile> in the span API
fn parse_declaration(source: &str) -> Result<(Rc<SourceFile>, Declaration)> {
    let mut diagnostics = Diagnostics::default();
    let file = Rc::new(SourceFile::fake(source.to_owned()));
    let lexer = Lexer::new(&file, &mut diagnostics);
    let tokens = lexer.lex().map_err(|_| ())?;
    let mut parser = Parser::new(file.clone(), &tokens, &mut diagnostics);
    parser
        .parse(test_module_name())
        .map(|declaration| (file, declaration))
        .map_err(|_| ())
}

/// The name of the module returned by [parse_declaration].
fn test_module_name() -> Identifier {
    Identifier::new("test".into(), Span::SHAM)
}

fn assert_eq<ItemKind>(actual: Result<Item<ItemKind>>, expected: Result<Item<ItemKind>>)
where
    ItemKind: Eq + Format + std::fmt::Debug,
{
    match (expected, actual) {
        (Ok(expected), Ok(actual)) => {
            if actual != expected {
                panic!(
                    "the actual output of the parser does not match the expected one:\n{}",
                    difference::Changeset::new(
                        &format!("{:?}", expected),
                        &format!("{:?}", actual),
                        "\n"
                    ),
                );
            }
        }
        (Ok(expected), Err(error)) => {
            panic!(
                "expected the parser to successfully parse the input\n\
                {:?}\n\
                but it failed with the following diagnostics:\n\
                {:?}",
                expected, error
            )
        }
        (Err(_), _) => unreachable!(),
    }
}

#[allow(unused_macros)]
macro no_std_assert($( $anything:tt )*) {
    compile_error!("use function `assert_eq` instead of macro `assert_eq` and similar")
}

#[allow(unused_imports)]
use no_std_assert as assert_eq;
#[allow(unused_imports)]
use no_std_assert as assert_ne;

/// Compare with [application_lambda_literal_argument_strict_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_lax_grouping() {
    assert_eq(
        parse_expression(r"(read \this => this) alpha"),
        Ok(expr! {
            Application {
                Attributes::new(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::new(), span(1, 20);
                        callee: Identifier::new("read".into(), span(2, 5)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::new(), span(7, 19);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        aspect: default(),
                                        parameters: smallvec![Identifier::new("this".into(), span(8, 11))],
                                        type_annotation: None,
                                        span: span(8, 11),
                                    }
                                ],
                                body_type_annotation: None,
                                body: Identifier::new("this".into(), span(16, 19)).into(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new("alpha".into(), span(22, 26)).into(),
                binder: None,
                explicitness: Explicit,
            }
        }),
    );
}

/// Compare with [application_lambda_literal_argument_lax_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_strict_grouping() {
    assert_eq(
        parse_expression(r"read (\this => this) alpha"),
        Ok(expr! {
            Application {
                Attributes::new(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::new(), span(1, 20);
                        callee: Identifier::new("read".into(), span(1, 4)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::new(), span(6, 20);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        aspect: default(),
                                        parameters: smallvec![Identifier::new("this".into(), span(8, 11))],
                                        type_annotation: None,
                                        span: span(8, 11),
                                    }
                                ],
                                body_type_annotation: None,
                                body: Identifier::new("this".into(), span(16, 19)).into(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new("alpha".into(), span(22, 26)).into(),
                binder: None,
                explicitness: Explicit,
            }
        }),
    );
}

#[test]
fn pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("Alpha (Beta) -> Gamma"),
        Ok(expr! {
            PiTypeLiteral {
                Attributes::new(), span(1, 21);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 12);
                            callee: Identifier::new("Alpha".into(), span(1, 5)).into(),
                            argument: Identifier::new("Beta".into(), span(7, 12)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                    },
                },
                codomain: Identifier::new("Gamma".into(), span(17, 21)).into(),
            }
        }),
    );
}

#[test]
fn bracketed_pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("(Alpha (Beta) -> Gamma)"),
        Ok(expr! {
            PiTypeLiteral {
                Attributes::new(), span(1, 23);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(2, 13);
                            callee: Identifier::new("Alpha".into(), span(2, 6)).into(),
                            argument: Identifier::new("Beta".into(), span(8, 13)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                    },
                },
                codomain: Identifier::new("Gamma".into(), span(18, 22)).into(),
            }
        }),
    );
}

/// Compare this with `f Int -> Type`.
///
/// This should demonstrate why we don't want `'A -> Type` to mean `'(_: A) -> Type`.
/// Compare this with [application_pi_type_literal_implicit_domain], too.
#[test]
fn pi_type_literal_application_implicit_argument_domain() {
    assert_eq(
        parse_expression("f 'Int -> Type"),
        Ok(expr! {
            PiTypeLiteral {
                Attributes::new(), span(1, 14);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 6);
                            callee: Identifier::new("f".into(), span(1, 1)).into(),
                            argument: Identifier::new("Int".into(), span(4, 6)).into(),
                            explicitness: Implicit,
                            binder: None,
                        }
                    }
                },
                codomain: expr! { TypeLiteral { Attributes::new(), span(11, 14) } },
            }
        }),
    );
}

#[test]
fn pi_type_literal_application_implicit_named_argument_domain() {
    assert_eq(
        parse_expression("f '(T = Int) -> Type"),
        Ok(expr! {
            PiTypeLiteral {
                Attributes::new(), span(1, 20);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 12);
                            callee: Identifier::new("f".into(), span(1, 1)).into(),
                            argument: Identifier::new("Int".into(), span(9, 11)).into(),
                            explicitness: Implicit,
                            binder: Some(Identifier::new("T".into(), span(5, 5))),
                        }
                    }
                },
                codomain: expr! { TypeLiteral { Attributes::new(), span(17, 20) } },
            }
        }),
    );
}

/// Compare with [pi_type_literal_application_implicit_argument_domain].
#[test]
fn application_pi_type_literal_implicit_domain() {
    assert_eq(
        parse_expression("receive ('(n: Int) -> Type)"),
        Ok(expr! {
            Application {
                Attributes::new(), span(1, 27);
                callee: Identifier::new("receive".into(), span(1, 7)).into(),
                argument: expr! {
                    PiTypeLiteral {
                        Attributes::new(), span(9, 27);
                        domain: Domain {
                            explicitness: Implicit,
                            aspect: default(),
                            binder: Some(Identifier::new("n".into(), span(12, 12))),
                            expression: Identifier::new("Int".into(), span(15, 17)).into(),
                        },
                        codomain: expr! { TypeLiteral { Attributes::new(), span(23, 26) } },
                    }
                },
                explicitness: Explicit,
                binder: None,
            }
        }),
    );
}

#[test]
fn chained_fields() {
    assert_eq(
        parse_expression("base::member::protrusion"),
        Ok(expr! {
            Field {
                Attributes::new(), span(1, 24);
                base: expr! {
                    Field {
                        Attributes::new(), span(1, 12);
                        base: Identifier::new("base".into(), span(1, 4)).into(),
                        member: Identifier::new("member".into(), span(7, 12))
                    }
                },
                member: Identifier::new("protrusion".into(), span(15, 24))
            }
        }),
    );
}

#[test]
fn namespaced_base_with_field() {
    assert_eq(
        parse_expression("path.to.base::member"),
        Ok(expr! {
            Field {
                Attributes::new(), span(1, 20);
                base: expr! {
                    Path {
                        Attributes::new(), span(1, 12);
                        hanger: None,
                        segments: smallvec![
                            Identifier::new("path".into(), span(1, 4)),
                            Identifier::new("to".into(), span(6, 7)),
                            Identifier::new("base".into(), span(9, 12)),
                        ],
                    }
                },
                member: Identifier::new("member".into(), span(15, 20)),
            }
        }),
    );
}

/// Compare with [base_with_attribute_and_field].
#[test]
fn field_with_attribute() {
    assert_eq(
        parse_expression("@overall compound::projection"),
        Ok(expr! {
            Field {
                vec![Attribute {
                    binder: Identifier::new("overall".into(), span(2, 8)),
                    arguments: default(),
                    span: span(1, 8),
                }],
                span(10, 29);
                base: Identifier::new("compound".into(), span(10, 17)).into(),
                member: Identifier::new("projection".into(), span(20, 29)),
            }
        }),
    );
}

/// Compare with [field_with_attribute].
#[test]
fn base_with_attribute_and_field() {
    assert_eq(
        parse_expression("(@specifically compound)::projection"),
        Ok(expr! {
            Field {
                Attributes::new(), span(1, 36);
                base: expr! {
                    Path(
                        vec![Attribute {
                            binder: Identifier::new("specifically".into(), span(3, 14)),
                            arguments: default(),
                            span: span(2, 14),
                        }],
                        span(1, 24);
                        Path::from(Identifier::new("compound".into(), span(16, 23)))
                    )
                },
                member: Identifier::new("projection".into(), span(27, 36)),
            }
        }),
    );
}

#[test]
fn field_inside_application() {
    assert_eq(
        parse_expression("cb::cm ab::am"),
        Ok(expr! {
            Application {
                Attributes::new(), span(1, 13);
                callee: expr! {
                    Field {
                        Attributes::new(), span(1, 6);
                        base: Identifier::new("cb".into(), span(1, 2)).into(),
                        member: Identifier::new("cm".into(), span(5, 6)),
                    }
                },
                argument: expr! {
                    Field {
                        Attributes::new(), span(8, 13);
                        base: Identifier::new("ab".into(), span(8, 9)).into(),
                        member: Identifier::new("am".into(), span(12, 13)),
                    }
                },
                binder: None,
                explicitness: Explicit,
            }
        }),
    );
}

#[test]
fn outer_and_inner_attributes() {
    assert_eq(
        parse_expression("@outer @outer (@inner Type)"),
        Ok(expr! {
            TypeLiteral {
                vec![
                    Attribute {
                        binder: Identifier::new("inner".into(), span(17, 21)),
                        arguments: default(),
                        span: span(16, 21),
                    },
                    Attribute {
                        binder: Identifier::new("outer".into(), span(2, 6)),
                        arguments: default(),
                        span: span(1, 6),
                    },
                    Attribute {
                        binder: Identifier::new("outer".into(), span(9, 13)),
                        arguments: default(),
                        span: span(8, 13),
                    }
                ],
                span(15, 27)
            }
        }),
    );
}

// @Task
// #[test]
// fn expression_attributes() {
//     assert_eq(parse_expression("@(alpha) "), Ok(todo!()));
// }

/// Compare with [use_as_double_brackets].
#[test]
fn use_as_plain() {
    let (file, declaration) = parse_declaration("use alpha.beta as gamma\n").unwrap();

    assert_eq(
        Ok(declaration),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 24);
                binder: test_module_name(),
                file,
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::new(), span(1, 23);
                            bindings: UsePathTree::new(
                                span(5, 23),
                                UsePathTreeKind::Single {
                                    target: Path {
                                        hanger: None,
                                        segments: smallvec![
                                            Identifier::new("alpha".into(), span(5, 9)),
                                            Identifier::new("beta".into(), span(11, 14)),
                                        ],
                                    },
                                    binder: Some(Identifier::new("gamma".into(),span(19, 23))),
                                }
                            )
                        }
                    }
                ]),
            }
        }),
    )
}

/// Compare with [use_as_plain].
#[test]
fn use_as_double_brackets() {
    let (file, declaration) = parse_declaration("use alpha.((beta as gamma))\n").unwrap();

    assert_eq(
        Ok(declaration),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 28);
                binder: test_module_name(),
                file,
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::new(), span(1, 27);
                            bindings: UsePathTree::new(
                                span(5, 27),
                                UsePathTreeKind::Multiple {
                                    path: Identifier::new("alpha".into(), span(5, 9)).into(),
                                    bindings: vec![
                                        UsePathTree::new(
                                            span(12, 26),
                                            UsePathTreeKind::Single {
                                                target:  Identifier::new("beta".into(), span(13, 16)).into(),
                                                binder: Some(Identifier::new("gamma".into(),span(21, 25))),
                                            }
                                        )
                                    ],
                                }
                            ),
                        }
                    }
                ]),
            }
        }),
    )
}
