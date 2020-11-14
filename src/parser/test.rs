// @Beacon @Task add more tests!!

use super::{
    ast::{expr, Attributes, Explicitness::*, Expression, Identifier, ParameterGroup},
    Parser,
};
use crate::{
    diagnostic::{Diagnostics, Results},
    lexer::Lexer,
    smallvec,
    span::{span, SourceFile},
    support::ManyErrExt,
};

fn parse_expression(source: &str) -> Results<Expression> {
    let mut diagnostics = Diagnostics::default();
    let file = std::rc::Rc::new(SourceFile::fake(source.to_owned()));
    let lexer = Lexer::new(&file, &mut diagnostics);
    let tokens = lexer.lex()?;
    let mut parser = Parser::new(file, &tokens, &mut diagnostics);
    parser.parse_expression().many_err()
}

/// Compare with [application_lambda_literal_argument_strict_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_lax_grouping() {
    assert_eq!(
        parse_expression(r"(read \this => this) alpha"),
        Ok(expr! {
            Application {
                Attributes::default(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::default(), span(1, 20);
                        callee: Identifier::new("read".into(), span(2, 5)).to_expression(),
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
                                body: Identifier::new("this".into(), span(16, 19)).to_expression(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new("alpha".into(), span(22, 26)).to_expression(),
                binder: None,
                explicitness: Explicit,
            }
        })
    );
}

/// Compare with [application_lambda_literal_argument_lax_grouping].
/// They parse to the same AST modulo spans.
#[test]
fn application_lambda_literal_argument_strict_grouping() {
    assert_eq!(
        parse_expression(r"read (\this => this) alpha"),
        Ok(expr! {
            Application {
                Attributes::default(), span(1, 26);
                callee: expr! {
                    Application {
                        Attributes::default(), span(1, 20);
                        callee: Identifier::new("read".into(), span(1, 4)).to_expression(),
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
                                body: Identifier::new("this".into(), span(16, 19)).to_expression(),
                            }
                        },
                        binder: None,
                        explicitness: Explicit,
                    }
                },
                argument: Identifier::new("alpha".into(), span(22, 26)).to_expression(),
                binder: None,
                explicitness: Explicit,
            }
        })
    );
}
