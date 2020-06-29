use crate::{
    span::{Span, Spanning},
    Atom, Int, Nat,
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

    pub fn with_data(kind: TokenKind, data: TokenData, span: Span) -> Self {
        Self { kind, data, span }
    }

    pub fn new_identifier(atom: Atom, span: Span) -> Self {
        Self::with_data(Identifier, TokenData::Identifier(atom), span)
    }

    pub fn new_punctuation(atom: Atom, span: Span) -> Self {
        Self::with_data(Punctuation, TokenData::Identifier(atom), span)
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

    /// Unwrap the data of a number literal. Panics if it isn't one.
    pub fn number_literal(self) -> Number {
        match self.data {
            TokenData::NatLiteral(value) => Number::Nat(value),
            TokenData::Nat32Literal(value) => Number::Nat32(value),
            TokenData::Nat64Literal(value) => Number::Nat64(value),
            TokenData::IntLiteral(value) => Number::Int(value),
            TokenData::Int32Literal(value) => Number::Int32(value),
            TokenData::Int64Literal(value) => Number::Int64(value),
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
            write!(f, "{} {:?} @ {:?}", self.kind, self.data, self.span)
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum TokenData {
    None,
    Identifier(Atom),
    NatLiteral(Nat),
    Nat32Literal(u32),
    Nat64Literal(u64),
    IntLiteral(Int),
    Int32Literal(i32),
    Int64Literal(i64),
    // boxed to reduce overall size from 24 to 8 (on my 64bit arch ^^), bad cache behavior irrelevant
    // since it's not hot
    TextLiteral(Box<String>),
}

impl fmt::Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, ""),
            Self::Identifier(value) => write!(f, "{}", value),
            Self::NatLiteral(value) => write!(f, "{}#N", value),
            Self::Nat32Literal(value) => write!(f, "{}#N32", value),
            Self::Nat64Literal(value) => write!(f, "{}#N63", value),
            Self::IntLiteral(value) => write!(f, "{}#I", value),
            Self::Int32Literal(value) => write!(f, "{}#I32", value),
            Self::Int64Literal(value) => write!(f, "{}#I64", value),
            Self::TextLiteral(value) => write!(f, "{:?}", value),
        }
    }
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
    NumberLiteral,
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
            NumberLiteral => "number literal",
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

#[derive(Clone, Copy)]
#[repr(u8)]
pub enum NumberLiteralSuffix {
    N,
    N32,
    N64,
    I,
    I32,
    I64,
}

impl NumberLiteralSuffix {
    pub fn signed(self) -> bool {
        use NumberLiteralSuffix::*;

        match self {
            N | N32 | N64 => false,
            I | I32 | I64 => true,
        }
    }
}

use std::str::FromStr;

impl FromStr for NumberLiteralSuffix {
    type Err = ();

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        use NumberLiteralSuffix::*;

        Ok(match source {
            "N" => N,
            "N32" => N32,
            "N64" => N64,
            "I" => I,
            "I32" => I32,
            "I64" => I64,
            _ => return Err(()),
        })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Number {
    Nat(crate::Nat),
    Nat32(u32),
    Nat64(u64),
    Int(crate::Int),
    Int32(i32),
    Int64(i64),
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{}", value),
            Self::Nat32(value) => write!(f, "{}", value),
            Self::Nat64(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{}", value),
            Self::Int32(value) => write!(f, "{}", value),
            Self::Int64(value) => write!(f, "{}", value),
        }
    }
}
