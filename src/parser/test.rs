//! Parser tests.
//!
//! Intended for testing edge cases. The majority of parsing tests should be
//! golden UI tests ([crate::golden], `tests/`).

// @Beacon @Task add more tests!!
// @Question maybe create builders for the elements?

use std::rc::Rc;

use super::{
    ast::{
        decl, expr, Attributes, Declaration, Explicitness::*, Expression, Identifier, Item,
        ParameterGroup, Path, PathTreeKind, UsePathTree,
    },
    Parser, Result,
};
use crate::{
    diagnostics::Diagnostics,
    lexer::Lexer,
    smallvec,
    span::{span, SourceFile, Span},
};

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
    ItemKind: Eq + std::fmt::Debug,
{
    match (expected, actual) {
        (Ok(expected), Ok(actual)) => {
            if actual != expected {
                panic!(
                    "the actual output of the parser does not match the expected one:\n{}",
                    difference::Changeset::new(
                        &format!("{:#?}", expected),
                        &format!("{:#?}", actual),
                        " "
                    ),
                );
            }
        }
        (Ok(expected), Err(error)) => {
            panic!(
                "expected the parser to successfully parse the input\n\
                {:?}\n\
                but it failed with the following diagnostics:\n\
                {:#?}",
                expected, error
            )
        }
        (Err(_), _) => unreachable!(),
    }
}

/// Compare with [application_lambda_literal_argument_strict_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_lax_grouping() {
    assert_eq(
        parse_expression(r"(read \this => this) alpha"),
        Ok(expr! {
            Application {
                Attributes::default(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::default(), span(1, 20);
                        callee: Identifier::new("read".into(), span(2, 5)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::default(), span(7, 19);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        fieldness: None,
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
                Attributes::default(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::default(), span(1, 20);
                        callee: Identifier::new("read".into(), span(1, 4)).into(),
                        argument: expr! {
                            LambdaLiteral {
                                Attributes::default(), span(6, 20);
                                parameters: vec![
                                    ParameterGroup {
                                        explicitness: Explicit,
                                        fieldness: None,
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
                Attributes::default(), span(1, 21);
                binder: None,
                domain: expr! {
                    Application {
                        Attributes::default(), span(1, 12);
                        callee: Identifier::new("Alpha".into(), span(1, 5)).into(),
                        argument: Identifier::new("Beta".into(), span(7, 12)).into(),
                        explicitness: Explicit,
                        binder: None,
                    }
                },
                codomain: Identifier::new("Gamma".into(), span(17, 21)).into(),
                explicitness: Explicit,
                fieldness: None,
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
                Attributes::default(), span(1, 23);
                binder: None,
                domain: expr! {
                    Application {
                        Attributes::default(), span(2, 13);
                        callee: Identifier::new("Alpha".into(), span(2, 6)).into(),
                        argument: Identifier::new("Beta".into(), span(8, 13)).into(),
                        explicitness: Explicit,
                        binder: None,
                    }
                },
                codomain: Identifier::new("Gamma".into(), span(18, 22)).into(),
                explicitness: Explicit,
                fieldness: None,
            }
        }),
    );
}

/// Compare with [use_as_double_brackets].
#[test]
fn use_as_plain() {
    let (file, declaration) = parse_declaration("use alpha.beta as gamma\n").unwrap();

    assert_eq(
        Ok(declaration),
        Ok(decl! {
            Module {
                Attributes::default(), span(1, 24);
                binder: test_module_name(),
                file,
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::default(), span(1, 23);
                            bindings: UsePathTree::new(
                                span(5, 23),
                                PathTreeKind::Single {
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
                Attributes::default(), span(1, 28);
                binder: test_module_name(),
                file,
                declarations: Some(vec![
                    decl! {
                        Use {
                            Attributes::default(), span(1, 27);
                            bindings: UsePathTree::new(
                                span(5, 27),
                                PathTreeKind::Multiple {
                                    path: Identifier::new("alpha".into(), span(5, 9)).into(),
                                    bindings: vec![
                                        UsePathTree::new(
                                            span(12, 26),
                                            PathTreeKind::Single {
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
