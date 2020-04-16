use super::{
    Diagnostics, Lexer, Token,
    TokenKind::{self, *},
};
use crate::span::{ByteIndex, Span};

fn lex(source: &'static str) -> Result<Vec<Token>, Diagnostics> {
    use crate::span::SourceFile;

    let file = SourceFile::new("test.lushui".into(), source.to_owned(), ByteIndex::new(0))
        .unwrap_or_else(|_| panic!());
    Lexer::new(&file).lex()
}

fn token(kind: TokenKind, start: u32, end: u32) -> Token {
    Token::new(kind, Span::new(ByteIndex::new(start), ByteIndex::new(end)))
}

fn assert_ok_token(actual: Result<Vec<Token>, Diagnostics>, expected: Vec<Token>) {
    match actual {
        Ok(actual) => assert_eq!(actual, expected),
        Err(_) => panic!("expected `{:?}`, got an `Err`", expected),
    }
}

fn assert_err(actual: Result<Vec<Token>, Diagnostics>) {
    if let Ok(actual) = actual {
        panic!("expected an `Err`, got `{:?}`", actual);
    }
}

#[test]
fn lex_comment() {
    assert_ok_token(
        lex("
;; bland commentary ensues
;; a filler line
;; and an end
"),
        vec![
            token(LineBreak, 0, 0),
            token(LineBreak, 27, 27),
            token(LineBreak, 44, 44),
            token(LineBreak, 58, 58),
            token(EndOfInput, 59, 59),
        ],
    );

    assert_ok_token(
        lex("\
alpha;;文本
0401 ; stray documentation comment
; next one
;有意思的信"),
        vec![
            token(Identifier("alpha".into()), 0, 4),
            token(LineBreak, 13, 13),
            token(NatLiteral(401u16.into()), 14, 17),
            token(DocumentationComment, 19, 47),
            token(LineBreak, 48, 48),
            token(DocumentationComment, 49, 58),
            token(LineBreak, 59, 59),
            token(DocumentationComment, 60, 75),
            token(EndOfInput, 76, 76),
        ],
    );
}

#[test]
fn lex_identifier() {
    assert_ok_token(
        lex("alpha alpha' alpha''' 'alpha '''alpha'''"),
        vec![
            token(Identifier("alpha".into()), 0, 4),
            token(Identifier("alpha'".into()), 6, 11),
            token(Identifier("alpha'''".into()), 13, 20),
            token(Identifier("'alpha".into()), 22, 27),
            token(Identifier("'''alpha'''".into()), 29, 39),
            token(EndOfInput, 40, 40),
        ],
    );

    assert_ok_token(
        lex("ALPH4-G4MM4 alpha'-gamma' ''d000''-''e000''-z999 ' ''' '-'-'"),
        vec![
            token(Identifier("ALPH4-G4MM4".into()), 0, 10),
            token(Identifier("alpha'-gamma'".into()), 12, 24),
            token(Identifier("''d000''-''e000''-z999".into()), 26, 47),
            token(Identifier("'".into()), 49, 49),
            token(Identifier("'''".into()), 51, 53),
            token(Identifier("'-'-'".into()), 55, 59),
            token(EndOfInput, 60, 60),
        ],
    );

    assert_err(lex("alpha-"));
    assert_err(lex("alpha-:"));
    assert_err(lex("alpha-0"));
    assert_err(lex("alpha--gamma"));

    assert_ok_token(
        lex("self   Type Type' 'Type Type-Type in"),
        vec![
            token(Self_, 0, 3),
            token(Type, 7, 10),
            token(Identifier("Type'".into()), 12, 16),
            token(Identifier("'Type".into()), 18, 22),
            token(Identifier("Type-Type".into()), 24, 32),
            token(In, 34, 35),
            token(EndOfInput, 36, 36),
        ],
    );
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
    /"),
        vec![
            token(LineBreak, 0, 0),
            token(Identifier("alpha".into()), 1, 5),
            token(LineBreak, 6, 6),
            token(Indentation, 7, 10),
            token(Identifier("alpha".into()), 11, 15),
            token(VerticalBar, 16, 16),
            token(LineBreak, 17, 17),
            token(Punctuation, 22, 23),
            token(LineBreak, 24, 24),
            token(Dedentation, 22, 22),
            token(Identifier("beta".into()), 25, 28),
            token(LineBreak, 29, 29),
            token(Indentation, 30, 33),
            token(Identifier("gamma".into()), 34, 38),
            token(LineBreak, 39, 39),
            token(Indentation, 44, 47),
            token(Identifier("delta".into()), 48, 52),
            token(LineBreak, 53, 53),
            token(Dedentation, 47, 47),
            token(Dedentation, 47, 47),
            token(Punctuation, 54, 54),
            token(LineBreak, 55, 55),
            token(Indentation, 56, 59),
            token(Punctuation, 60, 60),
            token(LineBreak, 61, 61),
            token(Indentation, 66, 69),
            token(Punctuation, 70, 70),
            token(LineBreak, 71, 71),
            token(Dedentation, 72, 72),
            token(Punctuation, 76, 76),
            token(Dedentation, 29, 29),
            token(EndOfInput, 77, 77),
        ],
    );

    assert_err(lex("
  ="));
    assert_err(lex("
        |
    "));
}

#[test]
fn lex_punctuation() {
    assert_ok_token(
        lex("+ +>alpha//$~%  @0 . .."),
        vec![
            token(Punctuation, 0, 0),
            token(Punctuation, 2, 3),
            token(Identifier("alpha".into()), 4, 8),
            token(Punctuation, 9, 13),
            token(Punctuation, 16, 16),
            token(NatLiteral(0u8.into()), 17, 17),
            token(Dot, 19, 19),
            token(Punctuation, 21, 22),
            token(EndOfInput, 23, 23),
        ],
    );
}

#[test]
fn lex_nat_literal() {
    assert_ok_token(
        lex("1001409409220293022239833211 01"),
        vec![
            token(NatLiteral(1001409409220293022239833211u128.into()), 0, 27),
            token(NatLiteral(1u8.into()), 29, 30),
            token(EndOfInput, 31, 31),
        ],
    );
}

#[test]
fn lex_text_literal() {
    assert_ok_token(
        lex(r#""
    al
  pha""#),
        vec![
            token(
                TextLiteral(
                    "
    al
  pha"
                    .into(),
                ),
                0,
                14,
            ),
            token(EndOfInput, 15, 15),
        ],
    );

    assert_err(lex(r#""text message"#));
}

#[test]
// @Task test bracket stack implementation once implemented
fn lex_other() {
    assert_ok_token(
        lex("___ _ (( )( ))"),
        vec![
            token(Underscore, 0, 0),
            token(Underscore, 1, 1),
            token(Underscore, 2, 2),
            token(Underscore, 4, 4),
            token(OpeningRoundBracket, 6, 6),
            token(OpeningRoundBracket, 7, 7),
            token(ClosingRoundBracket, 9, 9),
            token(OpeningRoundBracket, 10, 10),
            token(ClosingRoundBracket, 12, 12),
            token(ClosingRoundBracket, 13, 13),
            token(EndOfInput, 14, 14),
        ],
    );

    assert_err(lex("(("));
    assert_err(lex(")))"));
}

#[test]
fn illegal() {
    assert_err(lex("函数"));
    assert_err(lex("`"));
    assert_err(lex("\t"));
    assert_err(lex("[]"));
    assert_err(lex("{}"));
}
