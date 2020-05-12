use super::{
    lex, Diagnostics, Token,
    TokenKind::{self, *},
};
use crate::span::{ByteIndex, Span};

fn span(start: u32, end: u32) -> Span {
    Span::new(ByteIndex::new(start), ByteIndex::new(end))
}

fn token(kind: TokenKind, start: u32, end: u32) -> Token {
    Token::new(kind, span(start, end))
}

fn assert_ok_token(actual: Result<Vec<Token>, Diagnostics>, expected: Vec<Token>) {
    match actual {
        Ok(actual) => {
            if actual != expected {
                panic!(
                    "expected the token `{:?}` but got the token `{:?}`",
                    expected, actual
                );
            }
        }
        Err(_) => panic!("expected the token `{:?}` but got an `Err`", expected),
    }
}

fn assert_err(actual: Result<Vec<Token>, Diagnostics>, expected_spans: &[&[Span]]) {
    match actual {
        Ok(actual) => panic!("expected an `Err` but got the token `{:?}`", actual),
        Err(diagnostics) => {
            let actual_spans: Vec<Vec<Span>> = diagnostics
                .iter()
                .map(|diagnostic| diagnostic.spans())
                .collect();

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
            token(LineBreak, 1, 1),
            token(LineBreak, 28, 28),
            token(LineBreak, 45, 45),
            token(LineBreak, 59, 59),
            token(EndOfInput, 60, 60),
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
            token(Identifier("alpha".into()), 1, 5),
            token(LineBreak, 14, 14),
            token(NatLiteral(401u16.into()), 15, 18),
            token(DocumentationComment, 20, 48),
            token(LineBreak, 49, 49),
            token(DocumentationComment, 50, 59),
            token(LineBreak, 60, 60),
            token(DocumentationComment, 61, 76),
            token(EndOfInput, 77, 77),
        ],
    );
}

#[test]
fn lex_identifier() {
    assert_ok_token(
        lex("alpha alpha' alpha'''".into()),
        vec![
            token(Identifier("alpha".into()), 1, 5),
            token(Identifier("alpha'".into()), 7, 12),
            token(Identifier("alpha'''".into()), 14, 21),
            token(EndOfInput, 22, 22),
        ],
    );

    assert_ok_token(
        lex("ALPH4-G4MM4 alpha'-gamma' d000''-e000''-z999".into()),
        vec![
            token(Identifier("ALPH4-G4MM4".into()), 1, 11),
            token(Identifier("alpha'-gamma'".into()), 13, 25),
            token(Identifier("d000''-e000''-z999".into()), 27, 44),
            token(EndOfInput, 45, 45),
        ],
    );

    assert_ok_token(
        lex("self   Type Type' Type-Type in".into()),
        vec![
            token(Self_, 1, 4),
            token(Type, 8, 11),
            token(Identifier("Type'".into()), 13, 17),
            token(Identifier("Type-Type".into()), 19, 27),
            token(In, 29, 30),
            token(EndOfInput, 31, 31),
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
// @Task invalid indentation after SOI (currently not correctly implemented)
fn lex_indentation() {
    assert_ok_token(
        lex("
alpha
    alpha|
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
            token(LineBreak, 1, 1),
            token(Identifier("alpha".into()), 2, 6),
            token(LineBreak, 7, 7),
            token(Indentation, 8, 11),
            token(Identifier("alpha".into()), 12, 16),
            token(VerticalBar, 17, 17),
            token(LineBreak, 18, 18),
            token(Punctuation, 23, 24),
            token(LineBreak, 25, 25),
            token(Dedentation, 23, 23),
            token(Identifier("beta".into()), 26, 29),
            token(LineBreak, 30, 30),
            token(Indentation, 31, 34),
            token(Identifier("gamma".into()), 35, 39),
            token(LineBreak, 40, 40),
            token(Indentation, 45, 48),
            token(Identifier("delta".into()), 49, 53),
            token(LineBreak, 54, 54),
            token(Dedentation, 48, 48),
            token(Dedentation, 48, 48),
            token(Punctuation, 55, 55),
            token(LineBreak, 56, 56),
            token(Indentation, 57, 60),
            token(Punctuation, 61, 61),
            token(LineBreak, 62, 62),
            token(Indentation, 67, 70),
            token(Punctuation, 71, 71),
            token(LineBreak, 72, 72),
            token(Dedentation, 73, 73),
            token(Punctuation, 77, 77),
            token(Dedentation, 30, 30),
            token(EndOfInput, 78, 78),
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
        lex("+ +>alpha//$~%  @0 . ..".into()),
        vec![
            token(Punctuation, 1, 1),
            token(Punctuation, 3, 4),
            token(Identifier("alpha".into()), 5, 9),
            token(Punctuation, 10, 14),
            token(Punctuation, 17, 17),
            token(NatLiteral(0u8.into()), 18, 18),
            token(Dot, 20, 20),
            token(Punctuation, 22, 23),
            token(EndOfInput, 24, 24),
        ],
    );
}

#[test]
fn lex_nat_literal() {
    assert_ok_token(
        lex("1001409409220293022239833211 01".into()),
        vec![
            token(NatLiteral(1001409409220293022239833211u128.into()), 1, 28),
            token(NatLiteral(1u8.into()), 30, 31),
            token(EndOfInput, 32, 32),
        ],
    );
}

#[test]
fn lex_text_literal() {
    assert_ok_token(
        lex(r#""
    al
  pha""#
            .into()),
        vec![
            token(
                TextLiteral(
                    "
    al
  pha"
                    .into(),
                ),
                1,
                15,
            ),
            token(EndOfInput, 16, 16),
        ],
    );

    assert_err(lex(r#""text message"#.into()), &[&[span(1, 13)]]);
}

#[test]
fn lex_other() {
    assert_ok_token(
        lex("___ _ (( )( ))".into()),
        vec![
            token(Underscore, 1, 1),
            token(Underscore, 2, 2),
            token(Underscore, 3, 3),
            token(Underscore, 5, 5),
            token(OpeningRoundBracket, 7, 7),
            token(OpeningRoundBracket, 8, 8),
            token(ClosingRoundBracket, 10, 10),
            token(OpeningRoundBracket, 11, 11),
            token(ClosingRoundBracket, 13, 13),
            token(ClosingRoundBracket, 14, 14),
            token(EndOfInput, 15, 15),
        ],
    );

    assert_err(lex("((".into()), &[&[span(1, 1)], &[span(2, 2)]]);
    assert_err(lex(")))".into()), &[&[span(1, 1)]]);
}

#[test]
fn illegal() {
    assert_err(lex("函数".into()), &[&[span(1, 3)]]);
    assert_err(lex("`".into()), &[&[span(1, 1)]]);
    assert_err(lex("1`".into()), &[&[span(2, 2)]]);
    assert_err(lex("\t\t".into()), &[&[span(1, 1)]]);
    assert_err(lex("prefix[]".into()), &[&[span(7, 7)]]);
    assert_err(lex("{}".into()), &[&[span(1, 1)]]);
}
