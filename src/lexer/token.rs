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

    pub fn new_text_literal(text: String, span: Span, terminated: bool) -> Self {
        Self::with_data(
            TextLiteral,
            TokenData::TextLiteral {
                content: Box::new(text),
                terminated,
            },
            span,
        )
    }

    pub fn new_illegal(character: char, span: Span) -> Self {
        Self::with_data(Illegal, TokenData::Illegal(character), span)
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
        use Number::*;
        use TokenData::*;

        match self.data {
            NatLiteral(value) => Nat(value),
            Nat32Literal(value) => Nat32(value),
            Nat64Literal(value) => Nat64(value),
            IntLiteral(value) => Int(value),
            Int32Literal(value) => Int32(value),
            Int64Literal(value) => Int64(value),
            _ => unreachable!(),
        }
    }

    // @Note bad design
    pub fn text_literal_is_terminated(&self) -> bool {
        match self.data {
            TokenData::TextLiteral { terminated, .. } => terminated,
            _ => unreachable!(),
        }
    }

    /// Unwrap the data of a [TextLiteral]. Panics if it isn't one.
    pub fn text_literal(self) -> String {
        match self.data {
            TokenData::TextLiteral { content: text, .. } => *text,
            _ => unreachable!(),
        }
    }

    /// Unwrap the data of an [Illegal] character. Panics if it isn't one.
    pub fn illegal(&self) -> char {
        match self.data {
            TokenData::Illegal(character) => character,
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
            write!(f, "{} {:?}", self.kind, self.span)
        } else {
            write!(f, "{} {:?} {:?}", self.kind, self.data, self.span)
        }
    }
}

// @Beacon @Update we should postpone all that parsing to actual Rust types to the parser
// so that we can "recover" more parsing errors (and only store `Span`s and a small bit of
// meta data)
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
    // @Bug this payload is just gross and makes every single token large
    // but this is only temporary
    TextLiteral {
        content: Box<String>,
        terminated: bool,
    },
    Illegal(char),
}

impl TokenData {
    pub fn parse_number(number: &str, suffix: NumberLiteralSuffix) -> Result<Self, &'static str> {
        use NumberLiteralSuffix::*;
        macro int($interval:literal) {
            concat!("integer interval ", $interval)
        }

        Ok(match suffix {
            N => TokenData::NatLiteral(number.parse().map_err(|_| int!("[0, infinity)"))?),
            N32 => TokenData::Nat32Literal(number.parse().map_err(|_| int!("[0, 2^32-1]"))?),
            N64 => TokenData::Nat64Literal(number.parse().map_err(|_| int!("[0, 2^64-1]"))?),
            I => TokenData::IntLiteral(number.parse().unwrap()),
            I32 => TokenData::Int32Literal(number.parse().map_err(|_| int!("[-2^31, 2^31-1]"))?),
            I64 => TokenData::Int64Literal(number.parse().map_err(|_| int!("[-2^63, 2^63-1]"))?),
        })
    }
}

impl fmt::Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, ""),
            Self::Identifier(value) => write!(f, "{}", value),
            Self::NatLiteral(value) => write!(f, "{}_N", value),
            Self::Nat32Literal(value) => write!(f, "{}_N32", value),
            Self::Nat64Literal(value) => write!(f, "{}_N63", value),
            Self::IntLiteral(value) => write!(f, "{}_I", value),
            Self::Int32Literal(value) => write!(f, "{}_I32", value),
            Self::Int64Literal(value) => write!(f, "{}_I64", value),
            Self::TextLiteral {
                content,
                terminated,
            } => match terminated {
                true => write!(f, "{:?}", content),
                false => write!(f, "{:?}_Unterminated", content),
            },
            &Self::Illegal(char) => write!(f, "U+{:04X}", char as u32),
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
    Comment,
    DocumentationComment,
    Identifier,
    Punctuation,
    NumberLiteral,
    TextLiteral,
    At,
    Backslash,
    QuestionMark,
    Colon,
    Comma,
    Dedentation,
    Dot,
    Equals,
    Indentation,
    LineBreak,
    OpeningRoundBracket,
    OpeningSquareBracket,
    OpeningCurlyBracket,
    ClosingRoundBracket,
    ClosingSquareBracket,
    ClosingCurlyBracket,
    ThinArrowRight,
    ThinArrowLeft,
    Underscore,
    WideArrow,
    As,
    Case,
    Crate,
    Data,
    Do,
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
    Illegal,
}

use TokenKind::*;

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro keyword($keyword:ident) {
            concat!("keyword ", quoted!(stringify!($keyword)))
        }
        macro quoted($code:expr) {
            concat!("`", $code, "`")
        }

        f.write_str(match self {
            Comment => "comment",
            DocumentationComment => "documentation comment",
            Identifier => "identifier",
            Punctuation => "punctuation",
            NumberLiteral => "number literal",
            TextLiteral => "text literal",
            At => quoted!("@"),
            Backslash => quoted!(r"\"),
            QuestionMark => quoted!("?"),
            Colon => quoted!(":"),
            Comma => quoted!(","),
            Dedentation => "dedentation",
            Dot => quoted!("."),
            Equals => quoted!("="),
            Indentation => "indentation",
            LineBreak => "line break",
            OpeningRoundBracket => quoted!("("),
            OpeningSquareBracket => quoted!("["),
            OpeningCurlyBracket => quoted!("{"),
            ClosingRoundBracket => quoted!(")"),
            ClosingSquareBracket => quoted!("]"),
            ClosingCurlyBracket => quoted!("}"),
            ThinArrowRight => quoted!("->"),
            ThinArrowLeft => quoted!("<-"),
            // @Task make it an identifier
            Underscore => quoted!("_"),
            WideArrow => quoted!("=>"),
            As => keyword!(as),
            Case => keyword!(case),
            Crate => keyword!(crate),
            Data => keyword!(data),
            Do => keyword!(do),
            In => keyword!(in),
            Let => keyword!(let),
            Module => keyword!(module),
            Of => keyword!(of),
            Record => keyword!(record),
            Self_ => keyword!(self),
            Super => keyword!(super),
            Type => keyword!(Type),
            Use => keyword!(use),
            EndOfInput => "end of input",
            Illegal => "illegal token",
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
        "do" => Do,
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
        "?" => QuestionMark,
        "@" => At,
        "->" => ThinArrowRight,
        "<-" => ThinArrowLeft,
        "=>" => WideArrow,
        _ => return None,
    })
}

// @Task remove this type
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
    pub const fn type_name(self) -> &'static str {
        match self {
            Self::N => "Nat",
            Self::N32 => "Nat32",
            Self::N64 => "Nat64",
            Self::I => "Int",
            Self::I32 => "Int32",
            Self::I64 => "Int64",
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
