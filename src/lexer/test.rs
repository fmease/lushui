use super::{
    lex, Diagnostics, Token,
    TokenKind::{self, *},
};
use crate::span::{ByteIndex, Span};

fn span(start: u32, end: u32) -> Span {
    Span::new(ByteIndex::new(start), ByteIndex::new(end))
}

fn token(kind: TokenKind, span: Span) -> Token {
    Token::new(kind, span)
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
            token(LineBreak, span(1, 1)),
            token(LineBreak, span(28, 28)),
            token(LineBreak, span(45, 45)),
            token(LineBreak, span(59, 59)),
            token(EndOfInput, span(60, 60)),
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
            token(Identifier("alpha".into()), span(1, 5)),
            token(LineBreak, span(14, 14)),
            token(NatLiteral(401u16.into()), span(15, 18)),
            token(DocumentationComment, span(20, 48)),
            token(LineBreak, span(49, 49)),
            token(DocumentationComment, span(50, 59)),
            token(LineBreak, span(60, 60)),
            token(DocumentationComment, span(61, 76)),
            token(EndOfInput, span(77, 77)),
        ],
    );
}

#[test]
fn lex_identifier() {
    assert_ok_token(
        lex("alpha alpha' alpha'''".into()),
        vec![
            token(Identifier("alpha".into()), span(1, 5)),
            token(Identifier("alpha'".into()), span(7, 12)),
            token(Identifier("alpha'''".into()), span(14, 21)),
            token(EndOfInput, span(22, 22)),
        ],
    );

    assert_ok_token(
        lex("ALPH4-G4MM4 alpha'-gamma' d000''-e000''-z999".into()),
        vec![
            token(Identifier("ALPH4-G4MM4".into()), span(1, 11)),
            token(Identifier("alpha'-gamma'".into()), span(13, 25)),
            token(Identifier("d000''-e000''-z999".into()), span(27, 44)),
            token(EndOfInput, span(45, 45)),
        ],
    );

    assert_ok_token(
        lex("self   Type Type' Type-Type in".into()),
        vec![
            token(Self_, span(1, 4)),
            token(Type, span(8, 11)),
            token(Identifier("Type'".into()), span(13, 17)),
            token(Identifier("Type-Type".into()), span(19, 27)),
            token(In, span(29, 30)),
            token(EndOfInput, span(31, 31)),
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
            token(LineBreak, span(1, 1)),
            token(Identifier("alpha".into()), span(2, 6)),
            token(LineBreak, span(7, 7)),
            token(Indentation, span(8, 11)),
            token(Identifier("alpha".into()), span(12, 16)),
            token(Comma, span(17, 17)),
            token(LineBreak, span(18, 18)),
            token(Punctuation, span(23, 24)),
            token(LineBreak, span(25, 25)),
            token(Dedentation, span(23, 23)),
            token(Identifier("beta".into()), span(26, 29)),
            token(LineBreak, span(30, 30)),
            token(Indentation, span(31, 34)),
            token(Identifier("gamma".into()), span(35, 39)),
            token(LineBreak, span(40, 40)),
            token(Indentation, span(45, 48)),
            token(Identifier("delta".into()), span(49, 53)),
            token(LineBreak, span(54, 54)),
            token(Dedentation, span(48, 48)),
            token(Dedentation, span(48, 48)),
            token(Punctuation, span(55, 55)),
            token(LineBreak, span(56, 56)),
            token(Indentation, span(57, 60)),
            token(Punctuation, span(61, 61)),
            token(LineBreak, span(62, 62)),
            token(Indentation, span(67, 70)),
            token(Punctuation, span(71, 71)),
            token(LineBreak, span(72, 72)),
            token(Dedentation, span(73, 73)),
            token(Punctuation, span(77, 77)),
            token(Dedentation, span(30, 30)),
            token(EndOfInput, span(78, 78)),
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
            token(Punctuation, span(1, 1)),
            token(Punctuation, span(3, 4)),
            token(Identifier("alpha".into()), span(5, 9)),
            token(Punctuation, span(10, 14)),
            token(Punctuation, span(17, 17)),
            token(NatLiteral(0u8.into()), span(18, 18)),
            token(Dot, span(20, 20)),
            token(Punctuation, span(22, 23)),
            token(EndOfInput, span(24, 24)),
        ],
    );
}

#[test]
fn lex_nat_literal() {
    assert_ok_token(
        lex("1001409409220293022239833211 01".into()),
        vec![
            token(
                NatLiteral(1001409409220293022239833211u128.into()),
                span(1, 28),
            ),
            token(NatLiteral(1u8.into()), span(30, 31)),
            token(EndOfInput, span(32, 32)),
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
                span(1, 15),
            ),
            token(EndOfInput, span(16, 16)),
        ],
    );

    assert_err(lex(r#""text message"#.into()), &[&[span(1, 13)]]);
}

#[test]
fn lex_other() {
    assert_ok_token(
        lex("___ _ (( )( ))".into()),
        vec![
            token(Underscore, span(1, 1)),
            token(Underscore, span(2, 2)),
            token(Underscore, span(3, 3)),
            token(Underscore, span(5, 5)),
            token(OpeningRoundBracket, span(7, 7)),
            token(OpeningRoundBracket, span(8, 8)),
            token(ClosingRoundBracket, span(10, 10)),
            token(OpeningRoundBracket, span(11, 11)),
            token(ClosingRoundBracket, span(13, 13)),
            token(ClosingRoundBracket, span(14, 14)),
            token(EndOfInput, span(15, 15)),
        ],
    );

    // @Bug @Beacon fails non-deterministically: order of spans might be switched...but I thought I was only using Vecs here,
    // no HashMaps, what happens here?
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
