use super::{Token, TokenKind::*};
use crate::{
    diagnostics::Results,
    span::{span, Span},
};

fn lex(source: &'static str) -> Results<Vec<Token>> {
    super::lex(source.to_owned())
}

fn assert_ok_token(actual: Results<Vec<Token>>, expected: Vec<Token>) {
    match actual {
        Ok(actual) => {
            if actual != expected {
                panic!(
                    "the actual tokens outputted by the lexer do not match the expected ones:\n{}",
                    difference::Changeset::new(
                        &format!("{:#?}", expected),
                        &format!("{:#?}", actual),
                        "\n"
                    )
                );
            }
        }
        Err(_) => panic!("expected the tokens `{:?}` but got an `Err`", expected),
    }
}

fn assert_err(actual: Results<Vec<Token>>, expected_spans: &[&[Span]]) {
    match actual {
        Ok(actual) => panic!("expected an `Err` but got the tokens `{:?}`", actual),
        Err(diagnostics) => {
            let mut actual_spans: Vec<Vec<Span>> = diagnostics
                .into_iter()
                .map(|mut diagnostic| {
                    diagnostic.cancel();
                    diagnostic.sorted_spans()
                })
                .collect();

            actual_spans.sort();

            if actual_spans != expected_spans {
                panic!(
                    "expected the spans `{:?}` but got the spans `{:?}`",
                    expected_spans, actual_spans
                );
            }
        }
    }
}

#[allow(unused_macros)]
macro no_std_assert($( $anything:tt )*) {
    compile_error!(
        "use function `assert_ok_token` or `assert_err` instead of macro `assert_eq` and similar"
    )
}

#[allow(unused_imports)]
use no_std_assert as assert_eq;
#[allow(unused_imports)]
use no_std_assert as assert_ne;

#[test]
fn lex_comment() {
    assert_ok_token(
        lex("
;; bland commentary ensues
;; a filler line
;; and an end
"),
        vec![
            Token::new(LineBreak, span(1, 1)),
            Token::new(EndOfInput, span(59, 59)),
        ],
    );
}

#[test]
fn lex_documentation_comment() {
    assert_ok_token(
        lex("\
alpha;;文本
0401 ; stray documentation comment
; next one
;有意思的信"),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_number_literal("0401".into(), span(15, 18)),
            Token::new(DocumentationComment, span(20, 49)),
            Token::new(DocumentationComment, span(50, 60)),
            Token::new(DocumentationComment, span(61, 76)),
            Token::new(EndOfInput, span(76, 76)),
        ],
    );
}

#[test]
fn lex_identifier() {
    assert_ok_token(
        lex("alpha alpha0 _alpha al6ha_beta_"),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_identifier("alpha0".into(), span(7, 12)),
            Token::new_identifier("_alpha".into(), span(14, 19)),
            Token::new_identifier("al6ha_beta_".into(), span(21, 31)),
            Token::new(EndOfInput, span(31, 31)),
        ],
    );
}

#[test]
fn lex_identifier_dashes() {
    assert_ok_token(
        lex("ALPH4-G4MM4 alpha-gamma _-_"),
        vec![
            Token::new_identifier("ALPH4-G4MM4".into(), span(1, 11)),
            Token::new_identifier("alpha-gamma".into(), span(13, 23)),
            Token::new_identifier("_-_".into(), span(25, 27)),
            Token::new(EndOfInput, span(27, 27)),
        ],
    );
}

#[test]
fn lex_keywords() {
    assert_ok_token(
        lex("self   Type Type_ Type-Type in _"),
        vec![
            Token::new(Self_, span(1, 4)),
            Token::new(Type, span(8, 11)),
            Token::new_identifier("Type_".into(), span(13, 17)),
            Token::new_identifier("Type-Type".into(), span(19, 27)),
            Token::new(In, span(29, 30)),
            Token::new(Underscore, span(32, 32)),
            Token::new(EndOfInput, span(32, 32)),
        ],
    );
}

#[test]
fn do_not_lex_identifier_single_prime() {
    assert_ok_token(
        lex("'"),
        vec![
            Token::new_illegal('\'', span(1, 1)),
            Token::new(EndOfInput, span(1, 1)),
        ],
    );
}

#[test]
fn do_not_lex_identifier_leading_prime() {
    assert_ok_token(
        lex("'alpha"),
        vec![
            Token::new_illegal('\'', span(1, 1)),
            Token::new_identifier("alpha".into(), span(2, 6)),
            Token::new(EndOfInput, span(6, 6)),
        ],
    );
}

#[test]
fn do_not_lex_identifier_trailing_dash() {
    assert_err(lex("alpha-"), &[&[span(6, 6)]]);
}

#[test]
fn do_not_lex_identifier_trailing_dash_punctuation() {
    assert_err(lex("alpha-:"), &[&[span(6, 6)]]);
}

#[test]
fn do_not_lex_identifier_trailing_dash_number_literal() {
    assert_err(lex("alpha-0"), &[&[span(6, 6)]]);
}

#[test]
fn do_not_lex_identifier_consecutive_dashes() {
    assert_err(lex("alpha--gamma"), &[&[span(6, 6)]]);
}

#[test]
fn lex_indentation() {
    assert_ok_token(
        lex("
alpha
    alpha,
    <$
beta
    gamma
        delta
+
    -
        *
    /"),
        vec![
            Token::new(LineBreak, span(1, 1)),
            Token::new_identifier("alpha".into(), span(2, 6)),
            Token::new(LineBreak, span(7, 7)),
            Token::new(Indentation, span(8, 11)),
            Token::new_identifier("alpha".into(), span(12, 16)),
            Token::new(Comma, span(17, 17)),
            Token::new(LineBreak, span(18, 18)),
            Token::new_punctuation("<$".into(), span(23, 24)),
            Token::new(LineBreak, span(25, 25)),
            Token::new(Dedentation, span(25, 25)),
            Token::new_identifier("beta".into(), span(26, 29)),
            Token::new(LineBreak, span(30, 30)),
            Token::new(Indentation, span(31, 34)),
            Token::new_identifier("gamma".into(), span(35, 39)),
            Token::new(LineBreak, span(40, 40)),
            Token::new(Indentation, span(45, 48)),
            Token::new_identifier("delta".into(), span(49, 53)),
            Token::new(LineBreak, span(54, 54)),
            Token::new(Dedentation, span(54, 54)),
            Token::new(Dedentation, span(54, 54)),
            Token::new_punctuation("+".into(), span(55, 55)),
            Token::new(LineBreak, span(56, 56)),
            Token::new(Indentation, span(57, 60)),
            Token::new_punctuation("-".into(), span(61, 61)),
            Token::new(LineBreak, span(62, 62)),
            Token::new(Indentation, span(67, 70)),
            Token::new_punctuation("*".into(), span(71, 71)),
            Token::new(LineBreak, span(72, 72)),
            Token::new(Dedentation, span(75, 75)),
            Token::new_punctuation("/".into(), span(77, 77)),
            Token::new(Dedentation, span(77, 77)),
            Token::new(EndOfInput, span(77, 77)),
        ],
    );
}

#[test]
fn do_not_lex_too_shallow_indentation() {
    assert_err(
        lex("
  ="),
        &[&[span(2, 3)]],
    );
}

#[test]
fn do_not_lex_too_deep_indentation() {
    assert_err(
        lex("
        |
    "),
        &[&[span(2, 9)]],
    );
}

#[test]
fn lex_punctuation() {
    assert_ok_token(
        lex("+ +>alpha//$~%  #0 . .."),
        vec![
            Token::new_punctuation("+".into(), span(1, 1)),
            Token::new_punctuation("+>".into(), span(3, 4)),
            Token::new_identifier("alpha".into(), span(5, 9)),
            Token::new_punctuation("//$~%".into(), span(10, 14)),
            Token::new_punctuation("#".into(), span(17, 17)),
            Token::new_number_literal("0".into(), span(18, 18)),
            Token::new(Dot, span(20, 20)),
            Token::new_punctuation("..".into(), span(22, 23)),
            Token::new(EndOfInput, span(23, 23)),
        ],
    );
}

#[test]
fn lex_identifier_trailing_dot() {
    assert_ok_token(
        lex("namespace."),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new(EndOfInput, span(10, 10)),
        ],
    )
}

#[test]
fn lex_identifier_dot_punctuation() {
    assert_ok_token(
        lex("namespace.+>!"),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new_punctuation("+>!".into(), span(11, 13)),
            Token::new(EndOfInput, span(13, 13)),
        ],
    )
}

#[test]
fn lex_identifier_dot_dotted_punctuation() {
    assert_ok_token(
        lex("namespace.$.?!."),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new_punctuation("$.?!.".into(), span(11, 15)),
            Token::new(EndOfInput, span(15, 15)),
        ],
    )
}

#[test]
fn lex_keyword_dot_punctuation() {
    assert_ok_token(
        lex("data.#"),
        vec![
            Token::new(Data, span(1, 4)),
            Token::new(Dot, span(5, 5)),
            Token::new_punctuation("#".into(), span(6, 6)),
            Token::new(EndOfInput, span(6, 6)),
        ],
    )
}

#[test]
fn lex_number_literals() {
    assert_ok_token(
        lex("1001409409220293022239833211 01"),
        vec![
            Token::new_number_literal("1001409409220293022239833211".into(), span(1, 28)),
            Token::new_number_literal("01".into(), span(30, 31)),
            Token::new(EndOfInput, span(31, 31)),
        ],
    );
}

#[test]
fn lex_number_literals_with_separators() {
    assert_ok_token(
        lex(r#"334 1'000what 3'2'2'1"" 500 10"" -23"#),
        vec![
            Token::new_number_literal("334".into(), span(1, 3)),
            Token::new_number_literal("1000".into(), span(5, 9)),
            Token::new_identifier("what".into(), span(10, 13)),
            Token::new_number_literal("3221".into(), span(15, 21)),
            Token::new_text_literal(String::new(), span(22, 23), true),
            Token::new_number_literal("500".into(), span(25, 27)),
            Token::new_number_literal("10".into(), span(29, 30)),
            Token::new_text_literal(String::new(), span(31, 32), true),
            Token::new_number_literal("-23".into(), span(34, 36)),
            Token::new(EndOfInput, span(36, 36)),
        ],
    );
}

#[test]
fn do_no_lex_number_literal_with_consecutive_separators() {
    assert_err(lex("3''100"), &[&[span(1, 6)]]);
}

#[test]
fn do_not_lex_number_literal_with_trailing_separator() {
    assert_err(lex("10' "), &[&[span(1, 3)]]);
}

#[test]
fn do_not_lex_number_literal_with_trailing_separator_right_before_eoi() {
    assert_err(lex("10'"), &[&[span(1, 3)]]);
}

#[test]
fn lex_text_literal() {
    assert_ok_token(
        lex(r#""
    al
  pha""#),
        vec![
            Token::new_text_literal(
                "
    al
  pha"
                .into(),
                span(1, 15),
                true,
            ),
            Token::new(EndOfInput, span(15, 15)),
        ],
    );
}

#[test]
fn lex_unterminated_text_literal() {
    assert_ok_token(
        lex(r#""text message"#),
        vec![
            Token::new_text_literal("text message".into(), span(1, 13), false),
            Token::new(EndOfInput, span(13, 13)),
        ],
    );
}

#[test]
fn lex_brackets() {
    assert_ok_token(
        lex("(( )( ))"),
        vec![
            Token::new(OpeningRoundBracket, span(1, 1)),
            Token::new(OpeningRoundBracket, span(2, 2)),
            Token::new(ClosingRoundBracket, span(4, 4)),
            Token::new(OpeningRoundBracket, span(5, 5)),
            Token::new(ClosingRoundBracket, span(7, 7)),
            Token::new(ClosingRoundBracket, span(8, 8)),
            Token::new(EndOfInput, span(8, 8)),
        ],
    );
}

#[test]
fn do_not_lex_unbalanced_round_brackets_too_few_closing() {
    assert_err(lex("(("), &[&[span(1, 1)], &[span(2, 2)]]);
}

#[test]
fn do_not_lex_unbalanced_round_brackets_too_few_opening() {
    assert_err(lex(")))"), &[&[span(1, 1)]]);
}

#[test]
fn lex_bare_non_ascii_as_illegal() {
    assert_ok_token(
        lex("函数"),
        vec![
            Token::new_illegal('\u{51FD}', span(1, 3)),
            Token::new_illegal('\u{6570}', span(4, 6)),
            Token::new(EndOfInput, span(6, 6)),
        ],
    );
}

#[test]
fn lex_bare_non_ascii_as_illegal_and_keep_lexing() {
    assert_ok_token(
        lex(" 函数 function"),
        vec![
            Token::new_illegal('\u{51FD}', span(2, 4)),
            Token::new_illegal('\u{6570}', span(5, 7)),
            Token::new_identifier("function".into(), span(9, 16)),
            Token::new(EndOfInput, span(16, 16)),
        ],
    );
}

#[test]
fn lex_backtick_as_illegal() {
    assert_ok_token(
        lex("`"),
        vec![
            Token::new_illegal('`', span(1, 1)),
            Token::new(EndOfInput, span(1, 1)),
        ],
    );
}

#[test]
fn lex_backtick_as_illegal_right_after_number_literal() {
    assert_ok_token(
        lex("1`"),
        vec![
            Token::new_number_literal("1".into(), span(1, 1)),
            Token::new_illegal('`', span(2, 2)),
            Token::new(EndOfInput, span(2, 2)),
        ],
    );
}

#[test]
fn lex_tabs_as_illegal() {
    assert_ok_token(
        lex("\t\t"),
        vec![
            Token::new_illegal('\t', span(1, 1)),
            Token::new_illegal('\t', span(2, 2)),
            Token::new(EndOfInput, span(2, 2)),
        ],
    );
}
