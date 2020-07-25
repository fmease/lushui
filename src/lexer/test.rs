use super::{lex, token::TokenData, Token, TokenKind::*};
use crate::{
    diagnostic::Results,
    span::{ByteIndex, Span},
};

fn span(start: u32, end: u32) -> Span {
    Span::new(ByteIndex::new(start), ByteIndex::new(end))
}

fn assert_ok_token(actual: Results<Vec<Token>>, expected: Vec<Token>) {
    match actual {
        Ok(actual) => {
            if actual != expected {
                panic!(
                    "expected the tokens `{:#?}` but got the tokens `{:#?}`",
                    expected, actual
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
                .iter()
                .map(|diagnostic| diagnostic.spans())
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

#[test]
fn lex_comment() {
    assert_ok_token(
        lex("
;; bland commentary ensues
;; a filler line
;; and an end
"
        .into()),
        vec![
            Token::new(LineBreak, span(1, 1)),
            Token::new(EndOfInput, span(59, 59)),
        ],
    );

    assert_ok_token(
        lex("\
alpha;;文本
0401 ; stray documentation comment
; next one
;有意思的信"
            .into()),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(401u32.into()),
                span(15, 18),
            ),
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
        lex("alpha alpha' alpha'''".into()),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_identifier("alpha'".into(), span(7, 12)),
            Token::new_identifier("alpha'''".into(), span(14, 21)),
            Token::new(EndOfInput, span(21, 21)),
        ],
    );

    assert_ok_token(
        lex("ALPH4-G4MM4 alpha'-gamma' d000''-e000''-z999".into()),
        vec![
            Token::new_identifier("ALPH4-G4MM4".into(), span(1, 11)),
            Token::new_identifier("alpha'-gamma'".into(), span(13, 25)),
            Token::new_identifier("d000''-e000''-z999".into(), span(27, 44)),
            Token::new(EndOfInput, span(44, 44)),
        ],
    );

    assert_ok_token(
        lex("self   Type Type' Type-Type in".into()),
        vec![
            Token::new(Self_, span(1, 4)),
            Token::new(Type, span(8, 11)),
            Token::new_identifier("Type'".into(), span(13, 17)),
            Token::new_identifier("Type-Type".into(), span(19, 27)),
            Token::new(In, span(29, 30)),
            Token::new(EndOfInput, span(30, 30)),
        ],
    );

    assert_err(lex("'".into()), &[&[span(1, 1)]]);
    assert_err(lex("'alpha".into()), &[&[span(1, 1)]]);
    assert_err(lex("alpha-".into()), &[&[span(6, 6)]]);
    assert_err(lex("alpha-'".into()), &[&[span(6, 6)]]);
    assert_err(lex("alpha-:".into()), &[&[span(6, 6)]]);
    assert_err(lex("alpha-0".into()), &[&[span(6, 6)]]);
    assert_err(lex("alpha--gamma".into()), &[&[span(6, 6)]]);
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
    /"
        .into()),
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

    assert_err(
        lex("
  ="
        .into()),
        &[&[span(2, 3)]],
    );
    assert_err(
        lex("
        |
    "
        .into()),
        &[&[span(2, 9)]],
    );
}

#[test]
fn lex_punctuation() {
    assert_ok_token(
        lex("+ +>alpha//$~%  #0 . ..".into()),
        vec![
            Token::new_punctuation("+".into(), span(1, 1)),
            Token::new_punctuation("+>".into(), span(3, 4)),
            Token::new_identifier("alpha".into(), span(5, 9)),
            Token::new_punctuation("//$~%".into(), span(10, 14)),
            Token::new_punctuation("#".into(), span(17, 17)),
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(0u32.into()),
                span(18, 18),
            ),
            Token::new(Dot, span(20, 20)),
            Token::new_punctuation("..".into(), span(22, 23)),
            Token::new(EndOfInput, span(23, 23)),
        ],
    );
}

#[test]
fn lex_number_literal() {
    assert_ok_token(
        lex("1001409409220293022239833211 01".into()),
        vec![
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(1001409409220293022239833211u128.into()),
                span(1, 28),
            ),
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(1u32.into()),
                span(30, 31),
            ),
            Token::new(EndOfInput, span(31, 31)),
        ],
    );

    assert_ok_token(
        lex(r#"334#N 1'000what 3'2'2'1"" 500#N32 10#I"" -23#I64"#.into()),
        vec![
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(334u32.into()),
                span(1, 5),
            ),
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(1000u32.into()),
                span(7, 11),
            ),
            Token::new_identifier("what".into(), span(12, 15)),
            Token::with_data(
                NumberLiteral,
                TokenData::NatLiteral(3221u32.into()),
                span(17, 23),
            ),
            Token::new_text_literal(String::new(), span(24, 25)),
            Token::with_data(NumberLiteral, TokenData::Nat32Literal(500), span(27, 33)),
            Token::with_data(
                NumberLiteral,
                TokenData::IntLiteral(10.into()),
                span(35, 38),
            ),
            Token::new_text_literal(String::new(), span(39, 40)),
            Token::with_data(NumberLiteral, TokenData::Int64Literal(-23), span(42, 48)),
            Token::new(EndOfInput, span(48, 48)),
        ],
    );

    assert_err(lex("3''100".into()), &[&[span(1, 6)]]);
    assert_err(lex("10' ".into()), &[&[span(1, 3)]]);
    assert_err(lex("10'".into()), &[&[span(1, 3)]]);
    assert_err(lex("-23".into()), &[&[span(1, 3)]]);
    assert_err(lex("45#!".into()), &[&[span(1, 3)]]);
    assert_err(lex("12#O33".into()), &[&[span(1, 6)]]);
}

#[test]
fn lex_text_literal() {
    assert_ok_token(
        lex(r#""
    al
  pha""#
            .into()),
        vec![
            Token::new_text_literal(
                "
    al
  pha"
                .into(),
                span(1, 15),
            ),
            Token::new(EndOfInput, span(15, 15)),
        ],
    );

    assert_err(lex(r#""text message"#.into()), &[&[span(1, 13)]]);
}

#[test]
fn lex_other() {
    assert_ok_token(
        lex("___ _ (( )( ))".into()),
        vec![
            Token::new(Underscore, span(1, 1)),
            Token::new(Underscore, span(2, 2)),
            Token::new(Underscore, span(3, 3)),
            Token::new(Underscore, span(5, 5)),
            Token::new(OpeningRoundBracket, span(7, 7)),
            Token::new(OpeningRoundBracket, span(8, 8)),
            Token::new(ClosingRoundBracket, span(10, 10)),
            Token::new(OpeningRoundBracket, span(11, 11)),
            Token::new(ClosingRoundBracket, span(13, 13)),
            Token::new(ClosingRoundBracket, span(14, 14)),
            Token::new(EndOfInput, span(14, 14)),
        ],
    );

    assert_err(lex("((".into()), &[&[span(1, 1)], &[span(2, 2)]]);
    assert_err(lex(")))".into()), &[&[span(1, 1)]]);
}

#[test]
fn illegal() {
    assert_err(lex("函数".into()), &[&[span(1, 3)]]);
    assert_err(lex("  function 函数".into()), &[&[span(12, 14)]]);
    assert_err(lex("`".into()), &[&[span(1, 1)]]);
    assert_err(lex("1`".into()), &[&[span(2, 2)]]);
    assert_err(lex("\t\t".into()), &[&[span(1, 1)]]);
    assert_err(lex("prefix[]".into()), &[&[span(7, 7)]]);
    assert_err(lex("{}".into()), &[&[span(1, 1)]]);
}
