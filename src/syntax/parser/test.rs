//! Parser tests.
//!
//! Intended for testing edge cases. The majority of parsing tests should be
//! golden UI tests ([crate::golden], `tests/`).

// @Question maybe create builders for the elements instead of using macros?

use super::{
    ast::{
        decl, expr, pat, Attribute, AttributeKind, Attributes, Case, Declaration, Domain,
        Explicitness::*, Expression, Format, Identifier, Item, ParameterGroup, Parameters, Path,
        UsePathTree, UsePathTreeKind,
    },
    parse, Parser, Result,
};
use crate::{
    diagnostics::reporter::SilentReporter,
    error::{outcome, Health},
    span::{span, SourceFileIndex, SourceMap},
    syntax::lexer::lex,
};
use index_map::Index as _;
use smallvec::smallvec;
use std::default::default;

fn parse_expression(source: &str) -> Result<Expression> {
    let map = SourceMap::shared();
    let file = map.borrow_mut().add(None, source.to_owned());
    let reporter = SilentReporter.into();
    let outcome!(tokens, health) = lex(&map.borrow()[file], &reporter)?;
    let expression = Parser::new(&tokens, file, map, &reporter).parse_expression();
    if health.is_tainted() {
        return Err(());
    }
    expression
}

fn parse_declaration(source: &str) -> Result<Declaration> {
    let map = SourceMap::shared();
    let file = map.borrow_mut().add(None, source.to_owned());
    let reporter = SilentReporter.into();
    let outcome!(tokens, health) = lex(&map.borrow()[file], &reporter)?;

    let declaration = parse(&tokens, file.clone(), test_module_name(), map, &reporter);
    if health == Health::Tainted {
        return Err(());
    }
    declaration
}

/// The name of the module returned by [parse_declaration].
fn test_module_name() -> Identifier {
    Identifier::new_unchecked("test".into(), default())
}

fn test_file_index() -> SourceFileIndex {
    SourceFileIndex::new(0)
}

fn assert_eq<ItemKind>(actual: Result<Item<ItemKind>>, expected: Result<Item<ItemKind>>)
where
    ItemKind: Eq + Format,
{
    match (expected, actual) {
        (Ok(expected), Ok(actual)) => {
            if actual != expected {
                // the colored Format-output overwrites the colored highlighting of
                // the Changeset, this prevent that from happening
                colored::control::set_override(false);

                let difference = difference::Changeset::new(
                    &format!("{:?}", expected),
                    &format!("{:?}", actual),
                    "\n",
                );

                // colored::control::unset_override(); // conflicts with parallel test execution

                panic!(
                    "the actual output of the parser does not match the expected one:\n{}",
                    difference,
                );
            }
        }
        (Ok(expected), Err(error)) => {
            panic!(
                "expected the parser to successfully parse the input to the following AST:\n\
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
                Attributes::new(), span(1, 27);
                callee: expr! {
                    Application {
                        Attributes::new(), span(1, 21);
                        callee: Identifier::new_unchecked("read".into(), span(2, 6)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::new(), span(7, 20);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        aspect: default(),
                                        parameters: smallvec![Identifier::new_unchecked("this".into(), span(8, 12))],
                                        type_annotation: None,
                                        span: span(8, 12),
                                    }
                                ],
                                body_type_annotation: None,
                                body: Identifier::new_unchecked("this".into(), span(16, 20)).into(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new_unchecked("alpha".into(), span(22, 27)).into(),
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
                Attributes::new(), span(1, 27);
                callee: expr! {
                    Application {
                        Attributes::new(), span(1, 21);
                        callee: Identifier::new_unchecked("read".into(), span(1, 5)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::new(), span(6, 21);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        aspect: default(),
                                        parameters: smallvec![Identifier::new_unchecked("this".into(), span(8, 12))],
                                        type_annotation: None,
                                        span: span(8, 12),
                                    }
                                ],
                                body_type_annotation: None,
                                body: Identifier::new_unchecked("this".into(), span(16, 20)).into(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new_unchecked("alpha".into(), span(22, 27)).into(),
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
                Attributes::new(), span(1, 22);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 13);
                            callee: Identifier::new_unchecked("Alpha".into(), span(1, 6)).into(),
                            argument: Identifier::new_unchecked("Beta".into(), span(7, 13)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                    },
                },
                codomain: Identifier::new_unchecked("Gamma".into(), span(17, 22)).into(),
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
                Attributes::new(), span(1, 24);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(2, 14);
                            callee: Identifier::new_unchecked("Alpha".into(), span(2, 7)).into(),
                            argument: Identifier::new_unchecked("Beta".into(), span(8, 14)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                    },
                },
                codomain: Identifier::new_unchecked("Gamma".into(), span(18, 23)).into(),
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
                Attributes::new(), span(1, 15);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 7);
                            callee: Identifier::new_unchecked("f".into(), span(1, 2)).into(),
                            argument: Identifier::new_unchecked("Int".into(), span(4, 7)).into(),
                            explicitness: Implicit,
                            binder: None,
                        }
                    }
                },
                codomain: expr! { TypeLiteral { Attributes::new(), span(11, 15) } },
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
                Attributes::new(), span(1, 21);
                domain: Domain {
                    explicitness: Explicit,
                    aspect: default(),
                    binder: None,
                    expression: expr! {
                        Application {
                            Attributes::new(), span(1, 13);
                            callee: Identifier::new_unchecked("f".into(), span(1, 2)).into(),
                            argument: Identifier::new_unchecked("Int".into(), span(9, 12)).into(),
                            explicitness: Implicit,
                            binder: Some(Identifier::new_unchecked("T".into(), span(5, 6))),
                        }
                    }
                },
                codomain: expr! { TypeLiteral { Attributes::new(), span(17, 21) } },
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
                Attributes::new(), span(1, 28);
                callee: Identifier::new_unchecked("receive".into(), span(1, 8)).into(),
                argument: expr! {
                    PiTypeLiteral {
                        Attributes::new(), span(9, 28);
                        domain: Domain {
                            explicitness: Implicit,
                            aspect: default(),
                            binder: Some(Identifier::new_unchecked("n".into(), span(12, 13))),
                            expression: Identifier::new_unchecked("Int".into(), span(15, 18)).into(),
                        },
                        codomain: expr! { TypeLiteral { Attributes::new(), span(23, 27) } },
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
                Attributes::new(), span(1, 25);
                base: expr! {
                    Field {
                        Attributes::new(), span(1, 13);
                        base: Identifier::new_unchecked("base".into(), span(1, 5)).into(),
                        member: Identifier::new_unchecked("member".into(), span(7, 13))
                    }
                },
                member: Identifier::new_unchecked("protrusion".into(), span(15, 25))
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
                Attributes::new(), span(1, 21);
                base: expr! {
                    Path {
                        Attributes::new(), span(1, 13);
                        hanger: None,
                        segments: smallvec![
                            Identifier::new_unchecked("path".into(), span(1, 5)),
                            Identifier::new_unchecked("to".into(), span(6, 8)),
                            Identifier::new_unchecked("base".into(), span(9, 13)),
                        ],
                    }
                },
                member: Identifier::new_unchecked("member".into(), span(15, 21)),
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
                vec![Attribute::new(span(1, 9), AttributeKind::Regular {
                    binder: Identifier::new_unchecked("overall".into(), span(2, 9)),
                    arguments: default(),
                })],
                span(10, 30);
                base: Identifier::new_unchecked("compound".into(), span(10, 18)).into(),
                member: Identifier::new_unchecked("projection".into(), span(20, 30)),
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
                Attributes::new(), span(1, 37);
                base: expr! {
                    Path(
                        vec![Attribute::new(span(2, 15), AttributeKind::Regular {
                            binder: Identifier::new_unchecked("specifically".into(), span(3, 15)),
                            arguments: default(),
                        })],
                        span(1, 25);
                        Path::from(Identifier::new_unchecked("compound".into(), span(16, 24)))
                    )
                },
                member: Identifier::new_unchecked("projection".into(), span(27, 37)),
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
                Attributes::new(), span(1, 14);
                callee: expr! {
                    Field {
                        Attributes::new(), span(1, 7);
                        base: Identifier::new_unchecked("cb".into(), span(1, 3)).into(),
                        member: Identifier::new_unchecked("cm".into(), span(5, 7)),
                    }
                },
                argument: expr! {
                    Field {
                        Attributes::new(), span(8, 14);
                        base: Identifier::new_unchecked("ab".into(), span(8, 10)).into(),
                        member: Identifier::new_unchecked("am".into(), span(12, 14)),
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
                    Attribute::new(span(16, 22), AttributeKind::Regular {
                        binder: Identifier::new_unchecked("inner".into(), span(17, 22)),
                        arguments: default(),
                    }),
                    Attribute::new(span(1, 7), AttributeKind::Regular {
                        binder: Identifier::new_unchecked("outer".into(), span(2, 7)),
                        arguments: default(),
                    }),
                    Attribute::new(span(8, 14), AttributeKind::Regular {
                        binder: Identifier::new_unchecked("outer".into(), span(9, 14)),
                        arguments: default(),
                    })
                ],
                span(15, 28)
            }
        }),
    );
}

// @Task
// #[test]
// fn expression_attributes() {
//     assert_eq(parse_expression("@(alpha) "), Ok(todo!()));
// }

#[test]
fn bracketed_empty_case_analysis() {
    assert_eq(
        parse_expression("(case 1 of)"),
        Ok(expr! {
            CaseAnalysis {
                Attributes::new(), span(1, 12);
                scrutinee: expr! {
                    NumberLiteral(
                        Attributes::new(),
                        span(7, 8);
                        String::from("1"),
                    )
                },
                cases: Vec::new(),
            }
        }),
    );
}

/// Btw, notice how the case (“match arm”) is effectively outdented relative to the
/// start of the case analysis, the keyword `case`. This may be counter-intuitive but technically,
/// it is correct since indentation is relative to the start of the line.
// @Temporary name
#[test]
fn bracketed_case_analysis() {
    assert_eq(
        parse_expression(
            "\
lengthy-space-filler (case 0 of
    \\n => n)",
        ),
        Ok(expr! {
            Application {
                Attributes::new(), span(1, 45);
                explicitness: Explicit,
                binder: None,
                callee: Identifier::new_unchecked("lengthy-space-filler".into(), span(1, 21)).into(),
                argument: expr! {
                    CaseAnalysis {
                        Attributes::new(), span(22, 45);
                        scrutinee: expr! {
                            NumberLiteral(
                                Attributes::new(),
                                span(28, 29);
                                String::from("0"),
                            )
                        },
                        cases: vec![
                            Case {
                                pattern: pat! {
                                    Binder {
                                        Attributes::new(), span(37, 39);
                                        binder: Identifier::new_unchecked("n".into(), span(38, 39)),
                                    }
                                },
                                body: Identifier::new_unchecked("n".into(), span(43, 44)).into(),
                            }
                        ],
                    }
                }
            }
        }),
    );
}

// @Note we probably don't need this as a parser but as a lexer test
// (exercising the sections.truncate call)
// #[test]
// #[ignore]
// fn yyy() {
//     assert_eq(
//         parse_expression(
//             "\
// lengthy-space-filler (case 0 of
//     \\n => n
//         )",
//         ),
//         Ok(todo!()),
//     )
// }

// @Task add test (here or as golden) for expression `f (a = g b)` (this couldn't be parsed until now
// because of a bug; only `f (a = (g b))` was valid)

/// Compare with [use_as_double_brackets].
#[test]
fn use_as_plain() {
    assert_eq(
        parse_declaration("use alpha.beta as gamma\n"),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 25);
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::new(), span(1, 24);
                            bindings: UsePathTree::new(
                                span(5, 24),
                                UsePathTreeKind::Single {
                                    target: Path {
                                        hanger: None,
                                        segments: smallvec![
                                            Identifier::new_unchecked("alpha".into(), span(5, 10)),
                                            Identifier::new_unchecked("beta".into(), span(11, 15)),
                                        ],
                                    },
                                    binder: Some(Identifier::new_unchecked("gamma".into(),span(19, 24))),
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
    assert_eq(
        parse_declaration("use alpha.((beta as gamma))\n"),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 29);
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::new(), span(1, 28);
                            bindings: UsePathTree::new(
                                span(5, 28),
                                UsePathTreeKind::Multiple {
                                    path: Identifier::new_unchecked("alpha".into(), span(5, 10)).into(),
                                    bindings: vec![
                                        UsePathTree::new(
                                            span(12, 27),
                                            UsePathTreeKind::Single {
                                                target:  Identifier::new_unchecked("beta".into(), span(13, 17)).into(),
                                                binder: Some(Identifier::new_unchecked("gamma".into(),span(21, 26))),
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

/// Compare with [case_analysis_not_indented]. They are identical up to span.
#[test]
fn case_analysis_indented() {
    assert_eq(
        parse_declaration(
            "\
main =
    case x of
        false => 0
        \\bar =>
            \"bar\"
",
        ),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 75);
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![
                    decl! {
                        Function {
                            Attributes::new(), span(1, 75);
                            binder: Identifier::new_unchecked("main".into(), span(1, 5)),
                            parameters: Parameters::new(),
                            type_annotation: None,
                            body: Some(expr! {
                                CaseAnalysis {
                                    Attributes::new(), span(12, 75);
                                    scrutinee: Identifier::new_unchecked("x".into(), span(17, 18)).into(),
                                    cases: vec![
                                        Case {
                                            pattern: pat! {
                                                Path(
                                                    Attributes::new(), span(30, 35);
                                                    Path::from(Identifier::new_unchecked("false".into(), span(30, 35))),
                                                )
                                            },
                                            body: expr! {
                                                NumberLiteral(
                                                    Attributes::new(), span(39, 40);
                                                    String::from("0"),
                                                )
                                            },
                                        },
                                        Case {
                                            pattern: pat! {
                                                Binder {
                                                    Attributes::new(), span(49, 53);
                                                    binder: Identifier::new_unchecked("bar".into(), span(50, 53)),
                                                }
                                            },
                                            body: expr! {
                                                TextLiteral(
                                                    Attributes::new(), span(69, 74);
                                                    String::from("bar"),
                                                )
                                            },
                                        },
                                    ],
                                }
                            }),
                        }
                    }
                ]),
            }
        }),
    );
}

/// Compare with [case_analysis_indented]. They are identical up to span.
#[test]
fn case_analysis_not_indented() {
    assert_eq(
        parse_declaration(
            "\
main = case x of
    false => 0
    \\bar =>
        \"bar\"
",
        ),
        Ok(decl! {
            Module {
                Attributes::new(), span(1, 59);
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![
                    decl! {
                        Function {
                            Attributes::new(), span(1, 59);
                            binder: Identifier::new_unchecked("main".into(), span(1, 5)),
                            parameters: Parameters::new(),
                            type_annotation: None,
                            body: Some(expr! {
                                CaseAnalysis {
                                    Attributes::new(), span(8, 59);
                                    scrutinee: Identifier::new_unchecked("x".into(), span(13, 14)).into(),
                                    cases: vec![
                                        Case {
                                            pattern: pat! {
                                                Path(
                                                    Attributes::new(), span(22, 27);
                                                    Path::from(Identifier::new_unchecked("false".into(), span(22, 27))),
                                                )
                                            },
                                            body: expr! {
                                                NumberLiteral(
                                                    Attributes::new(), span(31, 32);
                                                    String::from("0"),
                                                )
                                            },
                                        },
                                        Case {
                                            pattern: pat! {
                                                Binder {
                                                    Attributes::new(), span(37, 41);
                                                    binder: Identifier::new_unchecked("bar".into(), span(38, 41)),
                                                }
                                            },
                                            body: expr! {
                                                TextLiteral(
                                                    Attributes::new(), span(53, 58);
                                                    String::from("bar"),
                                                )
                                            },
                                        },
                                    ],
                                }
                            }),
                        }
                    }
                ]),
            }
        }),
    );
}
