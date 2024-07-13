//! Positive behavior tests for the parser.
//!
//! Intended for edge cases. The majority of parser tests should be UI tests.

use super::parse;
use ast::{
    Attr, BareAttr, BareParam, BareUsePathTree, Case, Decl, Expr, Ident, Item, Param,
    ParamKind::{Explicit, Implicit},
    Params, Pat, Path, Render, UsePathTree,
};
use diagnostics::{error::Result, Reporter};
use index_map::Index as _;
use lexer::lex;
use span::{span, FileName, SourceMap, Span, Spanned, SrcFileIdx};
use std::sync::{Arc, RwLock};
use utility::{
    default,
    paint::{epaint, paint_to_string, ColorChoice},
    smallvec, Changeset, ChangesetExt, SmallVec,
};

fn parse_expression(source: &str) -> Result<Expr> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anon, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr(ColorChoice::Auto).with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_expr(),
        file,
        &map,
        &reporter,
    )
}

fn parse_pattern(source: &str) -> Result<Pat> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anon, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr(ColorChoice::Auto).with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_pattern(),
        file,
        &map,
        &reporter,
    )
}

fn parse_declaration(source: &str) -> Result<Decl> {
    let mut map = SourceMap::default();
    let file = map.add_str(FileName::Anon, source);
    let map = Arc::new(RwLock::new(map));
    let reporter = Reporter::stderr(ColorChoice::Auto).with_map(map.clone());
    let map = map.read().unwrap();
    parse(
        lex(&map[file], &default()),
        |parser| parser.parse_top_level(test_module_name()),
        file,
        &map,
        &reporter,
    )
}

/// The name of the module returned by [`parse_declaration`].
fn test_module_name() -> Ident {
    ident("test", default())
}

fn test_file_index() -> SrcFileIdx {
    SrcFileIdx::new(0)
}

fn assert_eq<BareItem>(actual: Result<Item<BareItem>>, expected: Result<Item<BareItem>>)
where
    BareItem: Eq + Render,
{
    match (expected, actual) {
        (Ok(expected), Ok(actual)) => {
            if actual != expected {
                // Don't color the AST since it doesn't go super well with the colored diff.
                let expected = paint_to_string(
                    |painter| expected.render(default(), painter),
                    ColorChoice::Never,
                )
                .unwrap();
                let actual = paint_to_string(
                    |painter| actual.render(default(), painter),
                    ColorChoice::Never,
                )
                .unwrap();

                // We also lock stdout since the test runner would otherwise interfere.
                let stdout = std::io::stdout().lock();
                epaint(
                    |painter| Changeset::new(&expected, &actual, "\n").render_with_ledge(painter),
                    ColorChoice::Auto,
                )
                .unwrap();
                drop(stdout);

                panic!("the actual output of the parser does not match the expected one");
            }
        }
        (Ok(_), Err(_)) => {
            panic!("expected the parser to successfully parse the input")
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
fn ident(name: &str, span: Span) -> Ident {
    Ident::new_unchecked(span, name.into())
}

/// Compare with [`app_lam_lit_arg_strict_grouping`].
/// They parse to the same AST modulo spans.
#[test]
fn app_lam_lit_arg_lax_grouping() {
    assert_eq(
        parse_expression(r"(read for this => this) alpha"),
        Ok(Expr::common(
            span(1, 30),
            ast::App {
                callee: Expr::common(
                    span(1, 24),
                    ast::App {
                        callee: ident("read", span(2, 6)).into(),
                        arg: Expr::common(
                            span(7, 23),
                            ast::LamLit {
                                params: smallvec![Param::new(
                                    span(11, 15),
                                    BareParam {
                                        kind: Explicit,
                                        binder: Some(ident("this", span(11, 15)).into()),
                                        ty: None,
                                    },
                                )],
                                codomain: None,
                                body: ident("this", span(19, 23)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        kind: Explicit,
                    }
                    .into(),
                ),
                arg: ident("alpha", span(25, 30)).into(),
                binder: None,
                kind: Explicit,
            }
            .into(),
        )),
    );
}

/// Compare with [`app_lam_lit_arg_lax_grouping`].
/// They parse to the same AST modulo spans.
#[test]
fn app_lam_lit_arg_strict_grouping() {
    assert_eq(
        parse_expression(r"read (for this => this) alpha"),
        Ok(Expr::common(
            span(1, 30),
            ast::App {
                callee: Expr::common(
                    span(1, 24),
                    ast::App {
                        callee: ident("read", span(1, 5)).into(),
                        arg: Expr::common(
                            span(6, 24),
                            ast::LamLit {
                                params: smallvec![Param::new(
                                    span(11, 15),
                                    BareParam {
                                        kind: Explicit,
                                        binder: Some(ident("this", span(11, 15)).into()),
                                        ty: None,
                                    },
                                )],
                                codomain: None,
                                body: ident("this", span(19, 23)).into(),
                            }
                            .into(),
                        ),
                        binder: None,
                        kind: Explicit,
                    }
                    .into(),
                ),
                arg: ident("alpha", span(25, 30)).into(),
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
        Ok(Expr::common(
            span(1, 22),
            ast::QuantifiedTy {
                quantifier: ast::Quantifier::Pi,
                params: smallvec![Param::new(
                    span(1, 13),
                    BareParam {
                        kind: Explicit,
                        binder: None,
                        ty: Some(Expr::common(
                            span(1, 13),
                            ast::App {
                                callee: ident("Alpha", span(1, 6)).into(),
                                arg: ident("Beta", span(7, 13)).into(),
                                kind: Explicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                ),],
                codomain: ident("Gamma", span(17, 22)).into(),
            }
            .into(),
        )),
    );
}

#[test]
fn bracketed_pi_type_literal_application_bracketed_argument_domain() {
    assert_eq(
        parse_expression("(Alpha (Beta) -> Gamma)"),
        Ok(Expr::common(
            span(1, 24),
            ast::QuantifiedTy {
                quantifier: ast::Quantifier::Pi,
                params: smallvec![Param::new(
                    span(2, 14),
                    BareParam {
                        kind: Explicit,
                        binder: None,
                        ty: Some(Expr::common(
                            span(2, 14),
                            ast::App {
                                callee: ident("Alpha", span(2, 7)).into(),
                                arg: ident("Beta", span(8, 14)).into(),
                                kind: Explicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                )],
                codomain: ident("Gamma", span(18, 23)).into(),
            }
            .into(),
        )),
    );
}

/// Compare this with `f Int -> Type`.
///
/// This should demonstrate why we don't want `'A -> Type` to mean `'(_: A) -> Type`.
/// Compare this with [`application_pi_type_literal_implicit_domain`], too.
#[test]
fn pi_type_literal_application_implicit_argument_domain() {
    assert_eq(
        parse_expression("f 'Int -> Type"),
        Ok(Expr::common(
            span(1, 15),
            ast::QuantifiedTy {
                quantifier: ast::Quantifier::Pi,
                params: smallvec![Param::new(
                    span(1, 7),
                    BareParam {
                        kind: Explicit,
                        binder: None,
                        ty: Some(Expr::common(
                            span(1, 7),
                            ast::App {
                                callee: ident("f", span(1, 2)).into(),
                                arg: ident("Int", span(4, 7)).into(),
                                kind: Implicit,
                                binder: None,
                            }
                            .into(),
                        )),
                    }
                )],
                codomain: ident("Type", span(11, 15)).into(),
            }
            .into(),
        )),
    );
}

#[test]
fn pi_type_literal_application_implicit_named_argument_domain() {
    assert_eq(
        parse_expression("f '(T = Int) -> Type"),
        Ok(Expr::common(
            span(1, 21),
            ast::QuantifiedTy {
                quantifier: ast::Quantifier::Pi,
                params: smallvec![Param::new(
                    span(1, 13),
                    BareParam {
                        kind: Explicit,
                        binder: None,
                        ty: Some(Expr::common(
                            span(1, 13),
                            ast::App {
                                callee: ident("f", span(1, 2)).into(),
                                arg: ident("Int", span(9, 12)).into(),
                                kind: Implicit,
                                binder: Some(ident("T", span(5, 6))),
                            }
                            .into(),
                        )),
                    }
                )],
                codomain: ident("Type", span(17, 21)).into(),
            }
            .into(),
        )),
    );
}

/// Compare with [`pi_type_literal_application_implicit_argument_domain`].
#[test]
fn application_pi_type_literal_implicit_domain() {
    assert_eq(
        parse_expression("receive (For '(n: Int) -> Type)"),
        Ok(Expr::common(
            span(1, 32),
            ast::App {
                callee: ident("receive", span(1, 8)).into(),
                arg: Expr::common(
                    span(9, 32),
                    ast::QuantifiedTy {
                        quantifier: ast::Quantifier::Pi,
                        params: smallvec![Param::new(
                            span(14, 23),
                            BareParam {
                                kind: Implicit,
                                binder: Some(ident("n", span(16, 17)).into()),
                                ty: Some(ident("Int", span(19, 22)).into()),
                            }
                        )],
                        codomain: ident("Type", span(27, 31)).into(),
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
        Ok(Expr::common(
            span(1, 25),
            ast::Proj {
                basis: Expr::common(
                    span(1, 13),
                    ast::Proj {
                        basis: ident("base", span(1, 5)).into(),
                        field: ident("member", span(7, 13)),
                    }
                    .into(),
                ),
                field: ident("protrusion", span(15, 25)),
            }
            .into(),
        )),
    );
}

#[test]
fn namespaced_base_with_field() {
    assert_eq(
        parse_expression("path.to.base::member"),
        Ok(Expr::common(
            span(1, 21),
            ast::Proj {
                basis: Expr::common(
                    span(1, 13),
                    ast::Path {
                        hanger: None,
                        segments: smallvec![
                            ident("path", span(1, 5)),
                            ident("to", span(6, 8)),
                            ident("base", span(9, 13)),
                        ],
                    }
                    .into(),
                ),
                field: ident("member", span(15, 21)),
            }
            .into(),
        )),
    );
}

/// Compare with [`base_with_attribute_and_field`].
#[test]
fn field_with_attribute() {
    assert_eq(
        parse_expression("@overall compound::projection"),
        Ok(Expr::new(
            vec![Attr::new(
                span(1, 9),
                BareAttr::Reg {
                    binder: ident("overall", span(2, 9)),
                    args: default(),
                },
            )],
            span(10, 30),
            ast::Proj {
                basis: ident("compound", span(10, 18)).into(),
                field: ident("projection", span(20, 30)),
            }
            .into(),
        )),
    );
}

/// Compare with [`field_with_attribute`].
#[test]
fn base_with_attribute_and_field() {
    assert_eq(
        parse_expression("(@specifically compound)::projection"),
        Ok(Expr::common(
            span(1, 37),
            ast::Proj {
                basis: Expr::new(
                    vec![Attr::new(
                        span(2, 15),
                        BareAttr::Reg {
                            binder: ident("specifically", span(3, 15)),
                            args: default(),
                        },
                    )],
                    span(1, 25),
                    Path::from(ident("compound", span(16, 24))).into(),
                ),
                field: ident("projection", span(27, 37)),
            }
            .into(),
        )),
    );
}

#[test]
fn field_inside_application() {
    assert_eq(
        parse_expression("cb::cm ab::am"),
        Ok(Expr::common(
            span(1, 14),
            ast::App {
                callee: Expr::common(
                    span(1, 7),
                    ast::Proj {
                        basis: ident("cb", span(1, 3)).into(),
                        field: ident("cm", span(5, 7)),
                    }
                    .into(),
                ),
                arg: Expr::common(
                    span(8, 14),
                    ast::Proj {
                        basis: ident("ab", span(8, 10)).into(),
                        field: ident("am", span(12, 14)),
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
        Ok(Expr::new(
            vec![
                Attr::new(
                    span(16, 22),
                    BareAttr::Reg {
                        binder: ident("inner", span(17, 22)),
                        args: default(),
                    },
                ),
                Attr::new(
                    span(1, 7),
                    BareAttr::Reg {
                        binder: ident("outer", span(2, 7)),
                        args: default(),
                    },
                ),
                Attr::new(
                    span(8, 14),
                    BareAttr::Reg {
                        binder: ident("outer", span(9, 14)),
                        args: default(),
                    },
                ),
            ],
            span(15, 28),
            Path::from(ident("Type", span(15, 28))).into(),
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
        Ok(Expr::common(
            span(1, 12),
            ast::CaseAnalysis {
                scrutinee: Expr::common(
                    span(7, 8),
                    ast::NumLit {
                        path: None,
                        lit: Spanned::new(span(7, 8), "1".into()),
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
        Ok(Expr::common(
            span(1, 48),
            ast::App {
                kind: Explicit,
                binder: None,
                callee: ident("lengthy-space-filler", span(1, 21)).into(),
                arg: Expr::common(
                    span(22, 48),
                    ast::CaseAnalysis {
                        scrutinee: Expr::common(
                            span(28, 29),
                            ast::NumLit {
                                path: None,
                                lit: Spanned::new(span(28, 29), "0".into()),
                            }
                            .into(),
                        ),
                        cases: vec![Case {
                            pattern: Pat::common(
                                span(37, 42),
                                ast::LocalBinder::Named(ident("n", span(41, 42))).into(),
                            ),
                            body: ident("n", span(46, 47)).into(),
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

#[test]
fn use_as_plain() {
    assert_eq(
        parse_declaration("use alpha.beta as gamma\n"),
        Ok(Decl::common(
            span(1, 25),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                decls: Some(vec![Decl::common(
                    span(1, 24),
                    ast::Use {
                        bindings: UsePathTree::new(
                            span(5, 24),
                            BareUsePathTree::Single {
                                target: Path {
                                    hanger: None,
                                    segments: smallvec![
                                        ident("alpha", span(5, 10)),
                                        ident("beta", span(11, 15)),
                                    ],
                                },
                                binder: Some(ident("gamma", span(19, 24))),
                            },
                        ),
                    }
                    .into(),
                )]),
            }
            .into(),
        )),
    );
}

/// Compare with [`unindented_case_analysis`]. They are identical up to span.
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
        Ok(Decl::common(
            span(1, 78),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                decls: Some(vec![Decl::common(
                    span(1, 78),
                    ast::Func {
                        binder: ident("main", span(1, 5)),
                        params: Params::new(),
                        ty: None,
                        body: Some(Expr::common(
                            span(12, 78),
                            ast::CaseAnalysis {
                                scrutinee: ident("x", span(17, 18)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pat::common(
                                            span(30, 35),
                                            Path::from(ident("false", span(30, 35))).into(),
                                        ),
                                        body: Expr::common(
                                            span(39, 40),
                                            ast::NumLit {
                                                path: None,
                                                lit: Spanned::new(span(39, 40), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pat::common(
                                            span(49, 56),
                                            ast::LocalBinder::Named(ident("bar", span(53, 56)))
                                                .into(),
                                        ),
                                        body: Expr::common(
                                            span(72, 77),
                                            ast::TextLit {
                                                path: None,
                                                lit: Spanned::new(span(72, 77), "bar".into()),
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

/// Compare with [`indented_case_analysis`]. They are identical up to span.
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
        Ok(Decl::common(
            span(1, 62),
            ast::Module {
                binder: test_module_name(),
                file: test_file_index(),
                decls: Some(vec![Decl::common(
                    span(1, 62),
                    ast::Func {
                        binder: ident("main", span(1, 5)),
                        params: Params::new(),
                        ty: None,
                        body: Some(Expr::common(
                            span(8, 62),
                            ast::CaseAnalysis {
                                scrutinee: ident("x", span(13, 14)).into(),
                                cases: vec![
                                    Case {
                                        pattern: Pat::common(
                                            span(22, 27),
                                            Path::from(ident("false", span(22, 27))).into(),
                                        ),
                                        body: Expr::common(
                                            span(31, 32),
                                            ast::NumLit {
                                                path: None,
                                                lit: Spanned::new(span(31, 32), "0".into()),
                                            }
                                            .into(),
                                        ),
                                    },
                                    Case {
                                        pattern: Pat::common(
                                            span(37, 44),
                                            ast::LocalBinder::Named(ident("bar", span(41, 44)))
                                                .into(),
                                        ),
                                        body: Expr::common(
                                            span(56, 61),
                                            ast::TextLit {
                                                path: None,
                                                lit: Spanned::new(span(56, 61), "bar".into()),
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
        Ok(Pat::new(
            vec![Attr::new(
                span(1, 4),
                BareAttr::Reg {
                    binder: ident("it", span(2, 4)),
                    args: SmallVec::new(),
                },
            )],
            span(5, 18),
            ast::App {
                callee: Pat::common(span(6, 9), Path::from(ident("has", span(6, 9))).into()),
                kind: Explicit,
                binder: None,
                arg: Pat::new(
                    vec![Attr::new(
                        span(10, 13),
                        BareAttr::Reg {
                            binder: ident("IT", span(11, 13)),
                            args: SmallVec::new(),
                        },
                    )],
                    span(14, 17),
                    Path::from(ident("HAS", span(14, 17))).into(),
                ),
            }
            .into(),
        )),
    );
}
