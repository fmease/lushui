//! Positive behavior tests for the parser.
//!
//! Intended for edge cases. The majority of parser tests should be UI tests.

use super::parse;
use ast::{
    Attribute, BareAttribute, BareParameter, BareUsePathTree, Case, Debug, Declaration, Expression,
    Format, Identifier, Item, Parameter,
    ParameterKind::{Explicit, Implicit},
    Parameters, Path, Pattern, UsePathTree,
};
use diagnostics::{error::Result, Reporter};
use index_map::Index as _;
use lexer::lex;
use span::{span, FileName, SourceFileIndex, SourceMap, Span, Spanned};
use std::sync::{Arc, RwLock};
use utility::{default, displayed, smallvec, SmallVec};

fn parse_expression(source: &str) -> Result<Expression> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anonymous, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr().with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_expression(),
        file,
        &map,
        &reporter,
    )
}

fn parse_pattern(source: &str) -> Result<Pattern> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anonymous, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr().with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_pattern(),
        file,
        &map,
        &reporter,
    )
}

fn parse_declaration(source: &str) -> Result<Declaration> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anonymous, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr().with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_top_level(test_module_name()),
        file,
        &map,
        &reporter,
    )
}

/// The name of the module returned by [parse_declaration].
fn test_module_name() -> Identifier {
    identifier("test", default())
}

fn test_file_index() -> SourceFileIndex {
    SourceFileIndex::new(0)
}

fn assert_eq<BareItem>(actual: Result<Item<BareItem>>, expected: Result<Item<BareItem>>)
where
    BareItem: Eq + Format,
{
    match (expected, actual) {
        (Ok(expected), Ok(actual)) => {
            if actual != expected {
                // the colored Format-output overwrites the colored highlighting of
                // the Changeset, this prevent that from happening
                colored::control::set_override(false);

                let difference = difference::Changeset::new(
                    &displayed(|f| expected.write(f)).to_string(),
                    &displayed(|f| actual.write(f)).to_string(),
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
                {}\n\
                but it (silently) reported an error",
                displayed(|f| expected.write(f))
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

// @Task swap arguments
fn identifier(name: &str, span: Span) -> Identifier {
    Identifier::new_unchecked(span, name.into())
}

/// Compare with [application_lambda_literal_argument_strict_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_lax_grouping() {
    assert_eq(
        parse_expression(r"(read for this => this) alpha"),
        Ok(Expression::common(
            span(1, 30),
            ast::Application {
                callee: Expression::common(
                    span(1, 24),
                    ast::Application {
                        callee: identifier("read", span(2, 6)).into(),
                        argument: Expression::common(
                            span(7, 23),
                            ast::LambdaLiteral {
                                parameters: smallvec![Parameter::new(
                                    span(11, 15),
                                    BareParameter {
                                        kind: Explicit,
                                        binder: Some(identifier("this", span(11, 15)).into()),
                                        type_: None,
                                    },
                                )],
                                codomain: None,
                                body: identifier("this", span(19, 23)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        kind: Explicit,
                    }
                    .into(),
                ),
                argument: identifier("alpha", span(25, 30)).into(),
                binder: None,
                kind: Explicit,
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
        parse_expression(r"read (for this => this) alpha"),
        Ok(Expression::common(
            span(1, 30),
            ast::Application {
                callee: Expression::common(
                    span(1, 24),
                    ast::Application {
                        callee: identifier("read", span(1, 5)).into(),
                        argument: Expression::common(
                            span(6, 24),
                            ast::LambdaLiteral {
                                parameters: smallvec![Parameter::new(
                                    span(11, 15),
                                    BareParameter {
                                        kind: Explicit,
                                        binder: Some(identifier("this", span(11, 15)).into()),
                                        type_: None,
                                    },
                                )],
                                codomain: None,
                                body: identifier("this", span(19, 23)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        kind: Explicit,
                    }
                    .into(),
                ),
                argument: identifier("alpha", span(25, 30)).into(),
                binder: None,
                kind: Explicit,
            }
            .into(),
        )),
    );
}

#[test]
fn pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("Alpha (Beta) -> Gamma"),
        Ok(Expression::common(
            span(1, 22),
            ast::QuantifiedType {
                quantifier: ast::Quantifier::Pi,
                parameters: smallvec![Parameter::new(
                    span(1, 13),
                    BareParameter {
                        kind: Explicit,
                        binder: None,
                        type_: Some(Expression::common(
                            span(1, 13),
                            ast::Application {
                                callee: identifier("Alpha", span(1, 6)).into(),
                                argument: identifier("Beta", span(7, 13)).into(),
                                kind: Explicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                ),],
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
        Ok(Expression::common(
            span(1, 24),
            ast::QuantifiedType {
                quantifier: ast::Quantifier::Pi,
                parameters: smallvec![Parameter::new(
                    span(2, 14),
                    BareParameter {
                        kind: Explicit,
                        binder: None,
                        type_: Some(Expression::common(
                            span(2, 14),
                            ast::Application {
                                callee: identifier("Alpha", span(2, 7)).into(),
                                argument: identifier("Beta", span(8, 14)).into(),
                                kind: Explicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                )],
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
        Ok(Expression::common(
            span(1, 15),
            ast::QuantifiedType {
                quantifier: ast::Quantifier::Pi,
                parameters: smallvec![Parameter::new(
                    span(1, 7),
                    BareParameter {
                        kind: Explicit,
                        binder: None,
                        type_: Some(Expression::common(
                            span(1, 7),
                            ast::Application {
                                callee: identifier("f", span(1, 2)).into(),
                                argument: identifier("Int", span(4, 7)).into(),
                                kind: Implicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                )],
                codomain: identifier("Type", span(11, 15)).into(),
            }
            .into(),
        )),
    );
}

#[test]
fn pi_type_literal_application_implicit_named_argument_domain() {
    assert_eq(
        parse_expression("f '(T = Int) -> Type"),
        Ok(Expression::common(
            span(1, 21),
            ast::QuantifiedType {
                quantifier: ast::Quantifier::Pi,
                parameters: smallvec![Parameter::new(
                    span(1, 13),
                    BareParameter {
                        kind: Explicit,
                        binder: None,
                        type_: Some(Expression::common(
                            span(1, 13),
                            ast::Application {
                                callee: identifier("f", span(1, 2)).into(),
                                argument: identifier("Int", span(9, 12)).into(),
                                kind: Implicit,
                                binder: Some(identifier("T", span(5, 6))),
                            }
                            .into(),
                        )),
                    }
                )],
                codomain: identifier("Type", span(17, 21)).into(),
            }
            .into(),
        )),
    );
}

/// Compare with [pi_type_literal_application_implicit_argument_domain].
#[test]
fn application_pi_type_literal_implicit_domain() {
    assert_eq(
        parse_expression("receive (For '(n: Int) -> Type)"),
        Ok(Expression::common(
            span(1, 32),
            ast::Application {
                callee: identifier("receive", span(1, 8)).into(),
                argument: Expression::common(
                    span(9, 32),
                    ast::QuantifiedType {
                        quantifier: ast::Quantifier::Pi,
                        parameters: smallvec![Parameter::new(
                            span(14, 23),
                            BareParameter {
                                kind: Implicit,
                                binder: Some(identifier("n", span(16, 17)).into()),
                                type_: Some(identifier("Int", span(19, 22)).into()),
                            }
                        )],
                        codomain: identifier("Type", span(27, 31)).into(),
                    }
                    .into(),
                ),
                kind: Explicit,
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
        Ok(Expression::common(
            span(1, 25),
            ast::Projection {
                basis: Expression::common(
                    span(1, 13),
                    ast::Projection {
                        basis: identifier("base", span(1, 5)).into(),
                        field: identifier("member", span(7, 13)),
                    }
                    .into(),
                ),
                field: identifier("protrusion", span(15, 25)),
            }
            .into(),
        )),
    );
}

#[test]
fn namespaced_base_with_field() {
    assert_eq(
        parse_expression("path.to.base::member"),
        Ok(Expression::common(
            span(1, 21),
            ast::Projection {
                basis: Expression::common(
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
                field: identifier("member", span(15, 21)),
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
            ast::Projection {
                basis: identifier("compound", span(10, 18)).into(),
                field: identifier("projection", span(20, 30)),
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
        Ok(Expression::common(
            span(1, 37),
            ast::Projection {
                basis: Expression::new(
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
                field: identifier("projection", span(27, 37)),
            }
            .into(),
        )),
    );
}

#[test]
fn field_inside_application() {
    assert_eq(
        parse_expression("cb::cm ab::am"),
        Ok(Expression::common(
            span(1, 14),
            ast::Application {
                callee: Expression::common(
                    span(1, 7),
                    ast::Projection {
                        basis: identifier("cb", span(1, 3)).into(),
                        field: identifier("cm", span(5, 7)),
                    }
                    .into(),
                ),
                argument: Expression::common(
                    span(8, 14),
                    ast::Projection {
                        basis: identifier("ab", span(8, 10)).into(),
                        field: identifier("am", span(12, 14)),
                    }
                    .into(),
                ),
                binder: None,
                kind: Explicit,
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
            Path::from(identifier("Type", span(15, 28))).into(),
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
        Ok(Expression::common(
            span(1, 12),
            ast::CaseAnalysis {
                scrutinee: Expression::common(
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
#[test]
fn bracketed_case_analysis() {
    assert_eq(
        parse_expression(
            "\
lengthy-space-filler (case 0 of
    let n => n)",
        ),
        Ok(Expression::common(
            span(1, 48),
            ast::Application {
                kind: Explicit,
                binder: None,
                callee: identifier("lengthy-space-filler", span(1, 21)).into(),
                argument: Expression::common(
                    span(22, 48),
                    ast::CaseAnalysis {
                        scrutinee: Expression::common(
                            span(28, 29),
                            ast::NumberLiteral {
                                path: None,
                                literal: Spanned::new(span(28, 29), "0".into()),
                            }
                            .into(),
                        ),
                        cases: vec![Case {
                            pattern: Pattern::common(
                                span(37, 42),
                                ast::LocalBinder::Named(identifier("n", span(41, 42))).into(),
                            ),
                            body: identifier("n", span(46, 47)).into(),
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
        Ok(Declaration::common(
            span(1, 25),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::common(
                    span(1, 24),
                    ast::Use {
                        bindings: UsePathTree::new(
                            span(5, 24),
                            BareUsePathTree::Single {
                                target: Path {
                                    hanger: None,
                                    segments: smallvec![
                                        identifier("alpha", span(5, 10)),
                                        identifier("beta", span(11, 15)),
                                    ],
                                },
                                binder: Some(identifier("gamma", span(19, 24))),
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

/// Compare with [unindented_case_analysis]. They are identical up to span.
#[test]
fn indented_case_analysis() {
    assert_eq(
        parse_declaration(
            "\
main =
    case x of
        false => 0
        let bar =>
            \"bar\"
",
        ),
        Ok(Declaration::common(
            span(1, 78),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::common(
                    span(1, 78),
                    ast::Function {
                        binder: identifier("main", span(1, 5)),
                        parameters: Parameters::new(),
                        type_: None,
                        body: Some(Expression::common(
                            span(12, 78),
                            ast::CaseAnalysis {
                                scrutinee: identifier("x", span(17, 18)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pattern::common(
                                            span(30, 35),
                                            Path::from(identifier("false", span(30, 35))).into(),
                                        ),
                                        body: Expression::common(
                                            span(39, 40),
                                            ast::NumberLiteral {
                                                path: None,
                                                literal: Spanned::new(span(39, 40), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pattern::common(
                                            span(49, 56),
                                            ast::LocalBinder::Named(identifier(
                                                "bar",
                                                span(53, 56),
                                            ))
                                            .into(),
                                        ),
                                        body: Expression::common(
                                            span(72, 77),
                                            ast::TextLiteral {
                                                path: None,
                                                literal: Spanned::new(span(72, 77), "bar".into()),
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

/// Compare with [indented_case_analysis]. They are identical up to span.
#[test]
fn unindented_case_analysis() {
    assert_eq(
        parse_declaration(
            "\
main = case x of
    false => 0
    let bar =>
        \"bar\"
",
        ),
        Ok(Declaration::common(
            span(1, 62),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                declarations: Some(vec![Declaration::common(
                    span(1, 62),
                    ast::Function {
                        binder: identifier("main", span(1, 5)),
                        parameters: Parameters::new(),
                        type_: None,
                        body: Some(Expression::common(
                            span(8, 62),
                            ast::CaseAnalysis {
                                scrutinee: identifier("x", span(13, 14)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pattern::common(
                                            span(22, 27),
                                            Path::from(identifier("false", span(22, 27))).into(),
                                        ),
                                        body: Expression::common(
                                            span(31, 32),
                                            ast::NumberLiteral {
                                                path: None,
                                                literal: Spanned::new(span(31, 32), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pattern::common(
                                            span(37, 44),
                                            ast::LocalBinder::Named(identifier(
                                                "bar",
                                                span(41, 44),
                                            ))
                                            .into(),
                                        ),
                                        body: Expression::common(
                                            span(56, 61),
                                            ast::TextLiteral {
                                                path: None,
                                                literal: Spanned::new(span(56, 61), "bar".into()),
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
                callee: Pattern::common(
                    span(6, 9),
                    Path::from(identifier("has".into(), span(6, 9))).into(),
                ),
                kind: Explicit,
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
