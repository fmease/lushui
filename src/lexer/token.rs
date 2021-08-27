use crate::{
    diagnostics::{Code, Diagnostic},
    format::quoted,
    span::Spanned,
    util::{obtain, Atom},
};
use discriminant::Discriminant;
use std::fmt;
use TokenName::*;

pub type Token = Spanned<TokenKind>;

impl Token {
    pub fn name(&self) -> TokenName {
        self.kind.discriminant()
    }

    pub const fn provenance(&self) -> Provenance {
        use TokenKind::*;

        match self.kind {
            Semicolon(provenance)
            | OpeningCurlyBracket(provenance)
            | ClosingCurlyBracket(provenance) => provenance,
            _ => Provenance::Source,
        }
    }

    pub fn is_line_break(&self) -> bool {
        matches!(self.kind, TokenKind::Semicolon(Provenance::Lexer))
    }

    pub fn into_identifier(self) -> Option<Atom> {
        use TokenKind::*;

        obtain!(self.kind, Identifier(atom) | Punctuation(atom) => atom)
    }

    pub fn into_number_literal(self) -> Option<String> {
        obtain!(self.kind, TokenKind::NumberLiteral(number) => number)
    }

    pub fn into_text_literal(self) -> Option<Result<String, Diagnostic>> {
        use TokenKind::*;

        match self.kind {
            TextLiteral(Ok(content)) => Some(Ok(content)),
            TextLiteral(Err(_)) => Some(Err(Diagnostic::error()
                .code(Code::E004)
                .message("unterminated text literal")
                .primary_span(self.span))),
            _ => None,
        }
    }

    pub fn as_illegal(&self) -> Option<char> {
        obtain!(self.kind, TokenKind::Illegal(character) => character)
    }
}

#[derive(Clone, PartialEq, Eq, Discriminant, Debug)]
#[discriminant(TokenName)]
pub enum TokenKind {
    Comment,
    DocumentationComment,
    Identifier(Atom),
    Punctuation(Atom),
    // @Question box it?
    NumberLiteral(String),
    // @Question box it?
    TextLiteral(Result<String, UnterminatedTextLiteral>),
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
    Semicolon(Provenance),
    OpeningRoundBracket,
    OpeningSquareBracket,
    OpeningCurlyBracket(Provenance),
    ClosingRoundBracket,
    ClosingSquareBracket,
    ClosingCurlyBracket(Provenance),
    /// For pi type literals.
    ThinArrowRight,
    /// For monadic binds in do-blocks.
    ThinArrowLeft,
    /// For value inference and unnameable unique identifiers.
    Underscore,
    /// For lambda literals and case analyses.
    WideArrowRight,
    /// For use-declarations.
    As,
    /// For case analyses / case/of-expressions.
    Case,
    /// For paths relative to the current crate.
    Crate,
    /// For paths relative to the collection of linked crates.
    Crates,
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
    Illegal(char),
}

impl TokenName {
    /// Test if the token may appear at the start of a [path](crate::ast::Path).
    pub const fn is_path_head(self) -> bool {
        matches!(self, Identifier | Punctuation) || self.is_path_hanger()
    }

    pub const fn is_path_hanger(self) -> bool {
        matches!(self, Crates | Crate | Super | Self_)
    }

    pub const fn is_terminator(self) -> bool {
        matches!(self, Semicolon | ClosingCurlyBracket | EndOfInput)
    }

    pub const fn introduces_indented_section(self) -> bool {
        matches!(self, Do | Of)
    }
}

impl fmt::Display for TokenName {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        macro keyword($keyword:ident) {
            concat!("keyword ", quoted!(stringify!($keyword)))
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
            WideArrowRight => quoted!("=>"),
            As => keyword!(as),
            Case => keyword!(case),
            Crate => keyword!(crate),
            Crates => keyword!(crates),
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
            Illegal => "illegal character",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct UnterminatedTextLiteral;

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Provenance {
    Source,
    Lexer,
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
    use TokenKind::*;

    Some(match source {
        "_" => Underscore,
        "as" => As,
        "case" => Case,
        "crate" => Crate,
        "crates" => Crates,
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
    use TokenKind::*;

    Some(match source {
        "." => Dot,
        ":" => Colon,
        "=" => Equals,
        "\\" => Backslash,
        "?" => QuestionMark,
        "@" => At,
        "->" => ThinArrowRight,
        "<-" => ThinArrowLeft,
        "=>" => WideArrowRight,
        "::" => DoubleColon,
        _ => return None,
    })
}
