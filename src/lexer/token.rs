use crate::{
    diagnostics::{Code, Diagnostic},
    span::{Span, Spanning},
    util::obtain,
    Atom,
};
use std::fmt;

#[derive(Clone, PartialEq, Eq)]
pub struct Token {
    pub kind: TokenKind,
    pub data: TokenData,
    pub span: Span,
}

impl Token {
    pub const fn new(kind: TokenKind, span: Span) -> Self {
        Self::with_data(kind, TokenData::None, span)
    }

    pub const fn new_virtual(kind: TokenKind, span: Span) -> Self {
        Self::with_data(kind, TokenData::IsVirtual, span)
    }

    pub const fn with_data(kind: TokenKind, data: TokenData, span: Span) -> Self {
        Self { kind, data, span }
    }

    pub fn new_identifier(atom: Atom, span: Span) -> Self {
        Self::with_data(Identifier, TokenData::Identifier(atom), span)
    }

    pub fn new_punctuation(atom: Atom, span: Span) -> Self {
        Self::with_data(Punctuation, TokenData::Identifier(atom), span)
    }

    pub fn new_number_literal(representation: String, span: Span) -> Self {
        Self::with_data(
            NumberLiteral,
            TokenData::NumberLiteral(representation),
            span,
        )
    }

    pub fn new_text_literal(text: String, span: Span, terminated: bool) -> Self {
        Self::with_data(
            TextLiteral,
            TokenData::TextLiteral {
                content: text,
                is_terminated: terminated,
            },
            span,
        )
    }

    pub fn new_illegal(character: char, span: Span) -> Self {
        Self::with_data(Illegal, TokenData::Illegal(character), span)
    }

    pub fn is_virtual(&self) -> bool {
        self.data == TokenData::IsVirtual
    }

    pub fn is_line_break(&self) -> bool {
        self.kind == Semicolon && self.is_virtual()
    }

    pub fn identifier(self) -> Option<Atom> {
        obtain!(self.data, TokenData::Identifier(atom) => atom)
    }

    pub fn number_literal(self) -> Option<String> {
        obtain!(self.data, TokenData::NumberLiteral(number) => number)
    }

    pub fn text_literal(self) -> Option<Result<String, Diagnostic>> {
        match self.data {
            TokenData::TextLiteral {
                content: text,
                is_terminated,
            } => Some(if is_terminated {
                Ok(text)
            } else {
                Err(Diagnostic::error()
                    .code(Code::E004)
                    .message("unterminated text literal")
                    .primary_span(self.span))
            }),
            _ => None,
        }
    }

    pub fn illegal(&self) -> Option<char> {
        obtain!(self.data, TokenData::Illegal(character) => character)
    }
}

impl Spanning for Token {
    fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Debug for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} {}",
            format!("{:?}", self.span).bright_black(),
            self.kind
        )?;

        if self.data != TokenData::None {
            write!(f, " {}", format!("{:?}", self.data).bright_yellow())?;
        }

        Ok(())
    }
}

// @Beacon @Update we should postpone all that parsing to actual Rust types to the parser
// so that we can "recover" more parsing errors (and only store `Span`s and a small bit of
// meta data)
#[derive(Clone, PartialEq, Eq)]
pub enum TokenData {
    None,
    IsVirtual,
    Identifier(Atom),
    // @Question how should we store this?
    NumberLiteral(String),
    // @Bug this payload is just gross and makes every single token large
    // but this is only temporary
    TextLiteral {
        content: String,
        is_terminated: bool,
    },
    Illegal(char),
}

impl fmt::Debug for TokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::None => write!(f, ""),
            Self::IsVirtual => write!(f, "virtual"),
            Self::Identifier(value) => write!(f, "{}", value),
            Self::NumberLiteral(value) => write!(f, "{}", value),
            Self::TextLiteral {
                content,
                is_terminated: true,
            } => write!(f, "{:?}", content),
            Self::TextLiteral {
                content,
                is_terminated: false,
            } => write!(f, "@unterminated {:?}", content),
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
    /// For attributes.
    At,
    /// For lambda literals and pattern binders.
    Backslash,
    /// For typed holes.
    QuestionMark,
    /// For type annotations.
    Colon,
    /// For record fields.
    DoubleColon,
    /// For paths.
    Dot,
    /// For definitions.
    Equals,
    /// For implicit parameters.
    SingleQuote,
    /// Delimiter.
    Semicolon,
    OpeningRoundBracket,
    OpeningSquareBracket,
    OpeningCurlyBracket,
    ClosingRoundBracket,
    ClosingSquareBracket,
    ClosingCurlyBracket,
    /// For pi type literals.
    ThinArrowRight,
    /// For monadic binds in do-blocks.
    ThinArrowLeft,
    /// For value inference and unnameable unique identifiers.
    Underscore,
    /// For lambda literals and case analyses.
    WideArrow,
    /// For use-declarations.
    As,
    /// For case analyses / case/of-expressions.
    Case,
    /// For paths relative to the current crate.
    Crate,
    /// For data declarations.
    Data,
    /// For do-blocks.
    Do,
    /// For let- and use-bindings.
    ///
    /// That is let/in-expressions and statements and
    /// use/in-expressions and statements.
    In,
    /// For lazy parameters.
    Lazy,
    /// For let-bindings / let/in-expressions and statements.
    Let,
    /// For module declarations.
    Module,
    /// For case analyses / case/of-expressions.
    Of,
    /// For paths relative to the current module/namespace.
    Self_,
    /// For paths relative to the parent module/namespace.
    Super,
    /// For type literals.
    Type,
    /// For use-declarations and use-bindings.
    ///
    /// The latter being use/in-expressions and statements.
    Use,
    EndOfInput,
    Illegal,
}

impl TokenKind {
    /// Test if the token may appear at the start of a [path](crate::ast::Path).
    pub const fn is_path_head(self) -> bool {
        matches!(self, Identifier | Punctuation | Crate | Super | Self_)
    }

    pub const fn is_terminator(self) -> bool {
        matches!(self, Semicolon | ClosingCurlyBracket | EndOfInput)
    }

    pub const fn introduces_indented_section(self) -> bool {
        matches!(self, Do | Of)
    }
}

use colored::Colorize;
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
            DoubleColon => quoted!("::"),
            Dot => quoted!("."),
            Equals => quoted!("="),
            SingleQuote => quoted!("'"),
            Semicolon => quoted!(";"),
            OpeningRoundBracket => quoted!("("),
            OpeningSquareBracket => quoted!("["),
            OpeningCurlyBracket => quoted!("{"),
            ClosingRoundBracket => quoted!(")"),
            ClosingSquareBracket => quoted!("]"),
            ClosingCurlyBracket => quoted!("}"),
            ThinArrowRight => quoted!("->"),
            ThinArrowLeft => quoted!("<-"),
            Underscore => quoted!("_"),
            WideArrow => quoted!("=>"),
            As => keyword!(as),
            Case => keyword!(case),
            Crate => keyword!(crate),
            Data => keyword!(data),
            Do => keyword!(do),
            In => keyword!(in),
            Lazy => keyword!(lazy),
            Let => keyword!(let),
            Module => keyword!(module),
            Of => keyword!(of),
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

pub const fn is_punctuation(character: char) -> bool {
    #[rustfmt::skip]
    matches!(
        character,
        '.' | ':' | '+' | '-' | '~' | '=' | '<' | '>' | '*' | '^' |
        '!' | '?' | '|' | '/' | '\\' | '&' | '#' | '%' | '$' | '@'
    )
}

pub const fn is_identifier_segment_start(character: char) -> bool {
    character.is_ascii_alphabetic() || character == '_'
}

pub const fn is_identifier_segment_middle(character: char) -> bool {
    character.is_ascii_alphanumeric() || character == '_'
}

pub const NUMERIC_SEPARATOR: char = '\'';

pub const fn is_number_literal_middle(character: char) -> bool {
    character.is_ascii_digit() || character == NUMERIC_SEPARATOR
}

pub fn parse_keyword(source: &str) -> Option<TokenKind> {
    Some(match source {
        "_" => Underscore,
        "as" => As,
        "case" => Case,
        "crate" => Crate,
        "data" => Data,
        "do" => Do,
        "in" => In,
        "lazy" => Lazy,
        "let" => Let,
        "module" => Module,
        "of" => Of,
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
        "::" => DoubleColon,
        _ => return None,
    })
}
