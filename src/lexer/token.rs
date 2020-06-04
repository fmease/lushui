use crate::{
    span::{Span, Spanning},
    Atom, Nat,
};
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub data: TokenData,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self::with_data(kind, TokenData::None, span)
    }

    fn with_data(kind: TokenKind, data: TokenData, span: Span) -> Self {
        Self { kind, data, span }
    }

    pub fn new_identifier(atom: Atom, span: Span) -> Self {
        Self::with_data(Identifier, TokenData::Identifier(atom), span)
    }

    pub fn new_nat_literal(nat: Nat, span: Span) -> Self {
        Self::with_data(NatLiteral, TokenData::NatLiteral(nat), span)
    }

    pub fn new_text_literal(text: String, span: Span) -> Self {
        Self::with_data(TextLiteral, TokenData::TextLiteral(Box::new(text)), span)
    }

    /// Unwrap the data of an [Identifier]. Panics if it isn't one.
    pub fn identifier(self) -> Atom {
        match self.data {
            TokenData::Identifier(atom) => atom,
            _ => unreachable!(),
        }
    }

    /// Unwrap the data of a [NatLiteral]. Panics if it isn't one.
    pub fn nat_literal(self) -> Nat {
        match self.data {
            TokenData::NatLiteral(nat) => nat,
            _ => unreachable!(),
        }
    }

    /// Unwrap the data of a [TextLiteral]. Panics if it isn't one.
    pub fn text_literal(self) -> String {
        match self.data {
            TokenData::TextLiteral(text) => *text,
            _ => unreachable!(),
        }
    }
}

impl Spanning for Token {
    fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.data == TokenData::None {
            write!(f, "{} @ {:?}", self.kind, self.span)
        } else {
            write!(f, "{} ({:?}) @ {:?}", self.kind, self.data, self.span)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenData {
    None,
    Identifier(Atom),
    NatLiteral(Nat),
    // boxed to reduce overall size from 24 to 8, bad cache behavior irrelevant
    // since it's not hot
    TextLiteral(Box<String>),
}

// @Note this would allow size_of::<Token> == 24 instead of 32 (or if String not Boxed 40 from 48)
// but there is a lot of unsafe work and trait implementation boilerplate necessary
// pub union TokenData {
//     none: (),
//     identifier: ManuallyDrop<Atom>,
//     nat_literal: ManuallyDrop<Nat>,
//     text_literal: ManuallyDrop<String>,
// }

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u8)] // as assertion
pub enum TokenKind {
    DocumentationComment,
    Identifier,
    Punctuation,
    NatLiteral,
    TextLiteral,
    At,
    Backslash,
    ClosingRoundBracket,
    Colon,
    Comma,
    Dedentation,
    Dot,
    Equals,
    Indentation,
    LineBreak,
    OpeningRoundBracket,
    ThinArrow,
    Underscore,
    WideArrow,
    As,
    Case,
    Crate,
    Data,
    In,
    Let,
    Module,
    Of,
    Record,
    Self_,
    Super,
    Type,
    Use,
    EndOfInput,
}

use TokenKind::*;

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            DocumentationComment => "documentation comment",
            Identifier => "identifier",
            Punctuation => "punctuation",
            NatLiteral => "number literal",
            TextLiteral => "text literal",
            At => "`@`",
            Backslash => "`\\`",
            ClosingRoundBracket => "`)`",
            Colon => "`:`",
            Comma => "`,`",
            Dedentation => "dedentation",
            Dot => "`.`",
            Equals => "`=`",
            Indentation => "indentation",
            LineBreak => "line break",
            OpeningRoundBracket => "`(`",
            ThinArrow => "`->`",
            Underscore => "`_`",
            WideArrow => "`=>`",
            As => "keyword `as`",
            Case => "keyword `case`",
            Crate => "keyword `crate`",
            Data => "keyword `data`",
            In => "keyword `in`",
            Let => "keyword `let`",
            Module => "keyword `module`",
            Of => "keyword `of`",
            Record => "keyword `record`",
            Self_ => "keyword `self`",
            Super => "keyword `super`",
            Type => "keyword `Type`",
            Use => "keyword `use`",
            EndOfInput => "end of input",
        })
    }
}

impl fmt::Debug for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

pub const PRIME: char = '\'';

pub fn is_punctuation(character: char) -> bool {
    matches!(
        character,
        '.' | ':'
            | '+'
            | '-'
            | '~'
            | '='
            | '<'
            | '>'
            | '*'
            | '^'
            | '!'
            | '?'
            | '|'
            | '/'
            | '\\'
            | '&'
            | '#'
            | '%'
            | '$'
            | '@'
    )
}

pub fn parse_keyword(source: &str) -> Option<TokenKind> {
    Some(match source {
        "as" => As,
        "case" => Case,
        "crate" => Crate,
        "data" => Data,
        "in" => In,
        "let" => Let,
        "module" => Module,
        "of" => Of,
        "record" => Record,
        "self" => Self_,
        "super" => Super,
        "Type" => Type,
        "use" => Use,
        _ => return None,
    })
}

pub fn parse_reserved_punctuation(source: &str) -> Option<TokenKind> {
    Some(match source {
        "." => Dot,
        ":" => Colon,
        "=" => Equals,
        "\\" => Backslash,
        "@" => At,
        "->" => ThinArrow,
        "=>" => WideArrow,
        _ => return None,
    })
}
