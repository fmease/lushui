//! Positive behavior tests for the parser.
//!
//! Intended for edge cases. The majority of parser tests should be UI tests.

use super::{
    ast::{
        self, Attribute, Attributes, BareAttribute, BareParameter, BareUsePathTree, Case,
        Declaration, Domain,
        Explicitness::{Explicit, Implicit},
        Expression, Format, Identifier, Item, Parameter, Parameters, Path, Pattern, UsePathTree,
    },
    Parser, Result,
};
use crate::{
    session::BuildSession,
    span::{span, SourceFileIndex, Span, Spanned},
    syntax::{lexer::lex, parse_module_file},
    utility::SmallVec,
};
use index_map::Index as _;
use smallvec::smallvec;
use std::{default::default, sync::Arc};

fn parse_expression(source: &str) -> Result<Expression> {
    let session = BuildSession::test();
    let file = session.map().add(None, Arc::new(source.to_owned()), None);
    Parser::new(&lex(file, &session)?.bare, file, &session).parse_expression()
}

fn parse_pattern(source: &str) -> Result<Pattern> {
    let session = BuildSession::test();
    let file = session.map().add(None, Arc::new(source.to_owned()), None);
    Parser::new(&lex(file, &session)?.bare, file, &session).parse_pattern()
}

fn parse_declaration(source: &str) -> Result<Declaration> {
    let session = BuildSession::test();
    let file = session.map().add(None, Arc::new(source.to_owned()), None);
    parse_module_file(file, test_module_name(), &session)
}

/// The name of the module returned by [parse_declaration].
fn test_module_name() -> Identifier {
    identifier("test", default())
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
                    &format!("{expected:?}"),
                    &format!("{actual:?}"),
                    "\n",
                );

                // colored::control::unset_override(); // conflicts with parallel test execution

                panic!(
                    "the actual output of the parser does not match the expected one:\n{difference}",
                );
            }
        }
        (Ok(expected), Err(_)) => {
            panic!(
                "expected the parser to successfully parse the input to the following AST:\n\
                {expected:?}\n\
                but it (silently) reported an error"
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

fn identifier(name: &str, span: Span) -> Identifier {
    Identifier::new_unchecked(name.into(), span)
}

/// Compare with [application_lambda_literal_argument_strict_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_lax_grouping() {
    assert_eq(
        parse_expression(r"(read \this => this) alpha"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 27),
            ast::Application {
                callee: Expression::new(
                    Attributes::new(),
                    span(1, 21),
                    ast::Application {
                        callee: identifier("read", span(2, 6)).into(),
                        argument: Expression::new(
                            Attributes::new(),
                            span(7, 20),
                            ast::LambdaLiteral {
                                parameters: vec![Parameter::new(
                                    span(8, 12),
                                    BareParameter {
                                        explicitness: Explicit,
                                        laziness: None,
                                        binder: identifier("this", span(8, 12)),
                                        type_annotation: None,
                                    },
                                )],
                                body_type_annotation: None,
                                body: identifier("this", span(16, 20)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: identifier("alpha", span(22, 27)).into(),
                binder: None,
                explicitness: Explicit,
            }
            .into(),
        )),
    );
}

/// Compare with [application_lambda_literal_argument_lax_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_strict_grouping() {
    assert_eq(
        parse_expression(r"read (\this => this) alpha"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 27),
            ast::Application {
                callee: Expression::new(
                    Attributes::new(),
                    span(1, 21),
                    ast::Application {
                        callee: identifier("read", span(1, 5)).into(),
                        argument: Expression::new(
                            Attributes::new(),
                            span(6, 21),
                            ast::LambdaLiteral {
                                parameters: vec![Parameter::new(
                                    span(8, 12),
                                    BareParameter {
                                        explicitness: Explicit,
                                        laziness: None,
                                        binder: identifier("this", span(8, 12)),
                                        type_annotation: None,
                                    },
                                )],
                                body_type_annotation: None,
                                body: identifier("this", span(16, 20)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        explicitness: Explicit,
                    }
                    .into(),
                ),
                argument: identifier("alpha", span(22, 27)).into(),
                binder: None,
                explicitness: Explicit,
            }
            .into(),
        )),
    );
}

#[test]
fn pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("Alpha (Beta) -> Gamma"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 22),
            ast::PiTypeLiteral {
                domain: Domain {
                    explicitness: Explicit,
                    laziness: None,
                    binder: None,
                    expression: Expression::new(
                        Attributes::new(),
                        span(1, 13),
                        ast::Application {
                            callee: identifier("Alpha", span(1, 6)).into(),
                            argument: identifier("Beta", span(7, 13)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                        .into(),
                    ),
                },
                codomain: identifier("Gamma", span(17, 22)).into(),
            }
            .into(),
        )),
    );
}

#[test]
fn bracketed_pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("(Alpha (Beta) -> Gamma)"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 24),
            ast::PiTypeLiteral {
                domain: Domain {
                    explicitness: Explicit,
                    laziness: None,
                    binder: None,
                    expression: Expression::new(
                        Attributes::new(),
                        span(2, 14),
                        ast::Application {
                            callee: identifier("Alpha", span(2, 7)).into(),
                            argument: identifier("Beta", span(8, 14)).into(),
                            explicitness: Explicit,
                            binder: None,
                        }
                        .into(),
                    ),
                },
                codomain: identifier("Gamma", span(18, 23)).into(),
            }
            .into(),
        )),
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
        Ok(Expression::new(
            Attributes::new(),
            span(1, 15),
            ast::PiTypeLiteral {
                domain: Domain {
                    explicitness: Explicit,
                    laziness: None,
                    binder: None,
                    expression: Expression::new(
                        Attributes::new(),
                        span(1, 7),
                        ast::Application {
                            callee: identifier("f", span(1, 2)).into(),
                            argument: identifier("Int", span(4, 7)).into(),
                            explicitness: Implicit,
                            binder: None,
                        }
                        .into(),
                    ),
                },
                codomain: Expression::new(
                    Attributes::new(),
                    span(11, 15),
                    ast::BareExpression::TypeLiteral,
                ),
            }
            .into(),
        )),
    );
}

#[test]
fn pi_type_literal_application_implicit_named_argument_domain() {
    assert_eq(
        parse_expression("f '(T = Int) -> Type"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 21),
            ast::PiTypeLiteral {
                domain: Domain {
                    explicitness: Explicit,
                    laziness: None,
                    binder: None,
                    expression: Expression::new(
                        Attributes::new(),
                        span(1, 13),
                        ast::Application {
                            callee: identifier("f", span(1, 2)).into(),
                            argument: identifier("Int", span(9, 12)).into(),
                            explicitness: Implicit,
                            binder: Some(identifier("T", span(5, 6))),
                        }
                        .into(),
                    ),
                },
                codomain: Expression::new(
                    Attributes::new(),
                    span(17, 21),
                    ast::BareExpression::TypeLiteral,
                ),
            }
            .into(),
        )),
    );
}

/// Compare with [pi_type_literal_application_implicit_argument_domain].
#[test]
fn application_pi_type_literal_implicit_domain() {
    assert_eq(
        parse_expression("receive ('(n: Int) -> Type)"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 28),
            ast::Application {
                callee: identifier("receive", span(1, 8)).into(),
                argument: Expression::new(
                    Attributes::new(),
                    span(9, 28),
                    ast::PiTypeLiteral {
                        domain: Domain {
                            explicitness: Implicit,
                            laziness: None,
                            binder: Some(identifier("n", span(12, 13))),
                            expression: identifier("Int", span(15, 18)).into(),
                        },
                        codomain: Expression::new(
                            Attributes::new(),
                            span(23, 27),
                            ast::BareExpression::TypeLiteral,
                        ),
                    }
                    .into(),
                ),
                explicitness: Explicit,
                binder: None,
            }
            .into(),
        )),
    );
}

#[test]
fn chained_fields() {
    assert_eq(
        parse_expression("base::member::protrusion"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 25),
            ast::Field {
                base: Expression::new(
                    Attributes::new(),
                    span(1, 13),
                    ast::Field {
                        base: identifier("base", span(1, 5)).into(),
                        member: identifier("member", span(7, 13)),
                    }
                    .into(),
                ),
                member: identifier("protrusion", span(15, 25)),
            }
            .into(),
        )),
    );
}

#[test]
fn namespaced_base_with_field() {
    assert_eq(
        parse_expression("path.to.base::member"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 21),
            ast::Field {
                base: Expression::new(
                    Attributes::new(),
                    span(1, 13),
                    ast::Path {
                        hanger: None,
                        segments: smallvec![
                            identifier("path", span(1, 5)),
                            identifier("to", span(6, 8)),
                            identifier("base", span(9, 13)),
                        ],
                    }
                    .into(),
                ),
                member: identifier("member", span(15, 21)),
            }
            .into(),
        )),
    );
}

/// Compare with [base_with_attribute_and_field].
#[test]
fn field_with_attribute() {
    assert_eq(
        parse_expression("@overall compound::projection"),
        Ok(Expression::new(
            vec![Attribute::new(
                span(1, 9),
                BareAttribute::Regular {
                    binder: identifier("overall", span(2, 9)),
                    arguments: default(),
                },
            )],
            span(10, 30),
            ast::Field {
                base: identifier("compound", span(10, 18)).into(),
                member: identifier("projection", span(20, 30)),
            }
            .into(),
        )),
    );
}

/// Compare with [field_with_attribute].
#[test]
fn base_with_attribute_and_field() {
    assert_eq(
        parse_expression("(@specifically compound)::projection"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 37),
            ast::Field {
                base: Expression::new(
                    vec![Attribute::new(
                        span(2, 15),
                        BareAttribute::Regular {
                            binder: identifier("specifically", span(3, 15)),
                            arguments: default(),
                        },
                    )],
                    span(1, 25),
                    Path::from(identifier("compound", span(16, 24))).into(),
                ),
                member: identifier("projection", span(27, 37)),
            }
            .into(),
        )),
    );
}

#[test]
fn field_inside_application() {
    assert_eq(
        parse_expression("cb::cm ab::am"),
        Ok(Expression::new(
            Attributes::new(),
            span(1, 14),
            ast::Application {
                callee: Expression::new(
                    Attributes::new(),
                    span(1, 7),
                    ast::Field {
                        base: identifier("cb", span(1, 3)).into(),
                        member: identifier("cm", span(5, 7)),
                    }
                    .into(),
                ),
                argument: Expression::new(
                    Attributes::new(),
                    span(8, 14),
                    ast::Field {
                        base: identifier("ab", span(8, 10)).into(),
                        member: identifier("am", span(12, 14)),
                    }
                    .into(),
                ),
                binder: None,
                explicitness: Explicit,
            }
            .into(),
        )),
    );
}

#[test]
fn outer_and_inner_attributes() {
    assert_eq(
        parse_expression("@outer @outer (@inner Type)"),
        Ok(Expression::new(
            vec![
                Attribute::new(
                    span(16, 22),
                    BareAttribute::Regular {
                        binder: identifier("inner", span(17, 22)),
                        arguments: default(),
                    },
                ),
                Attribute::new(
                    span(1, 7),
                    BareAttribute::Regular {
                        binder: identifier("outer", span(2, 7)),
                        arguments: default(),
                    },
                ),
                Attribute::new(
                    span(8, 14),
                    BareAttribute::Regular {
                        binder: identifier("outer", span(9, 14)),
                        arguments: default(),
                    },
                ),
            ],
            span(15, 28),
            ast::BareExpression::TypeLiteral,
        )),
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
        Ok(Expression::new(
            Attributes::new(),
            span(1, 12),
            ast::CaseAnalysis {
                scrutinee: Expression::new(
                    Attributes::new(),
                    span(7, 8),
                    ast::NumberLiteral {
                        path: None,
                        literal: Spanned::new(span(7, 8), "1".into()),
                    }
                    .into(),
                ),
                cases: Vec::new(),
            }
            .into(),
        )),
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
        Ok(Expression::new(
            Attributes::new(),
            span(1, 45),
            ast::Application {
                explicitness: Explicit,
                binder: None,
                callee: identifier("lengthy-space-filler".into(), span(1, 21)).into(),
                argument: Expression::new(
                    Attributes::new(),
                    span(22, 45),
                    ast::CaseAnalysis {
                        scrutinee: Expression::new(
                            Attributes::new(),
                            span(28, 29),
                            ast::NumberLiteral {
                                path: None,
                                literal: Spanned::new(span(28, 29), "0".into()),
                            }
                            .into(),
                        ),
                        cases: vec![Case {
                            pattern: Pattern::new(
                                Attributes::new(),
                                span(37, 39),
                                identifier("n".into(), span(38, 39)).into(),
                            ),
                            body: identifier("n".into(), span(43, 44)).into(),
                        }],
                    }
                    .into(),
                ),
            }
            .into(),
        )),
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
        Ok(Declaration::new(
            Attributes::new(),
            span(1, 25),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::new(
                    Attributes::new(),
                    span(1, 24),
                    ast::Use {
                        bindings: UsePathTree::new(
                            span(5, 24),
                            BareUsePathTree::Single {
                                target: Path {
                                    hanger: None,
                                    segments: smallvec![
                                        identifier("alpha".into(), span(5, 10)),
                                        identifier("beta".into(), span(11, 15)),
                                    ],
                                },
                                binder: Some(identifier("gamma".into(), span(19, 24))),
                            },
                        ),
                    }
                    .into(),
                )]),
            }
            .into(),
        )),
    )
}

/// Compare with [use_as_plain].
#[test]
fn use_as_double_brackets() {
    assert_eq(
        parse_declaration("use alpha.((beta as gamma))\n"),
        Ok(Declaration::new(
            Attributes::new(),
            span(1, 29),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::new(
                    Attributes::new(),
                    span(1, 28),
                    ast::Use {
                        bindings: UsePathTree::new(
                            span(5, 28),
                            BareUsePathTree::Multiple {
                                path: identifier("alpha".into(), span(5, 10)).into(),
                                bindings: vec![UsePathTree::new(
                                    span(12, 27),
                                    BareUsePathTree::Single {
                                        target: identifier("beta".into(), span(13, 17)).into(),
                                        binder: Some(identifier("gamma".into(), span(21, 26))),
                                    },
                                )],
                            },
                        ),
                    }
                    .into(),
                )]),
            }
            .into(),
        )),
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
        Ok(Declaration::new(
            Attributes::new(),
            span(1, 75),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::new(
                    Attributes::new(),
                    span(1, 75),
                    ast::Function {
                        binder: identifier("main".into(), span(1, 5)),
                        parameters: Parameters::new(),
                        type_annotation: None,
                        body: Some(Expression::new(
                            Attributes::new(),
                            span(12, 75),
                            ast::CaseAnalysis {
                                scrutinee: identifier("x".into(), span(17, 18)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pattern::new(
                                            Attributes::new(),
                                            span(30, 35),
                                            Path::from(identifier("false", span(30, 35))).into(),
                                        ),
                                        body: Expression::new(
                                            Attributes::new(),
                                            span(39, 40),
                                            ast::NumberLiteral {
                                                path: None,
                                                literal: Spanned::new(span(39, 40), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pattern::new(
                                            Attributes::new(),
                                            span(49, 53),
                                            identifier("bar".into(), span(50, 53)).into(),
                                        ),
                                        body: Expression::new(
                                            Attributes::new(),
                                            span(69, 74),
                                            ast::TextLiteral {
                                                path: None,
                                                literal: Spanned::new(span(69, 74), "bar".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                ],
                            }
                            .into(),
                        )),
                    }
                    .into(),
                )]),
            }
            .into(),
        )),
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
        Ok(Declaration::new(
            Attributes::new(),
            span(1, 59),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::new(
                    Attributes::new(),
                    span(1, 59),
                    ast::Function {
                        binder: identifier("main".into(), span(1, 5)),
                        parameters: Parameters::new(),
                        type_annotation: None,
                        body: Some(Expression::new(
                            Attributes::new(),
                            span(8, 59),
                            ast::CaseAnalysis {
                                scrutinee: identifier("x".into(), span(13, 14)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pattern::new(
                                            Attributes::new(),
                                            span(22, 27),
                                            Path::from(identifier("false".into(), span(22, 27)))
                                                .into(),
                                        ),
                                        body: Expression::new(
                                            Attributes::new(),
                                            span(31, 32),
                                            ast::NumberLiteral {
                                                path: None,
                                                literal: Spanned::new(span(31, 32), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pattern::new(
                                            Attributes::new(),
                                            span(37, 41),
                                            identifier("bar".into(), span(38, 41)).into(),
                                        ),
                                        body: Expression::new(
                                            Attributes::new(),
                                            span(53, 58),
                                            ast::TextLiteral {
                                                path: None,
                                                literal: Spanned::new(span(53, 58), "bar".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                ],
                            }
                            .into(),
                        )),
                    }
                    .into(),
                )]),
            }
            .into(),
        )),
    );
}

#[test]
fn pattern_with_attributes() {
    assert_eq(
        parse_pattern("@it (has @IT HAS)"),
        Ok(Pattern::new(
            vec![Attribute::new(
                span(1, 4),
                BareAttribute::Regular {
                    binder: identifier("it".into(), span(2, 4)),
                    arguments: SmallVec::new(),
                },
            )],
            span(5, 18),
            ast::Application {
                callee: Pattern::new(
                    Attributes::new(),
                    span(6, 9),
                    Path::from(identifier("has".into(), span(6, 9))).into(),
                ),
                explicitness: Explicit,
                binder: None,
                argument: Pattern::new(
                    vec![Attribute::new(
                        span(10, 13),
                        BareAttribute::Regular {
                            binder: identifier("IT".into(), span(11, 13)),
                            arguments: SmallVec::new(),
                        },
                    )],
                    span(14, 17),
                    Path::from(identifier("HAS".into(), span(14, 17))).into(),
                ),
            }
            .into(),
        )),
    )
}
