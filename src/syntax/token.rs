//! The tokens emitted by the lexer.

use crate::{
    diagnostics::{Code, Diagnostic},
    span::Spanned,
    utility::{obtain, quoted, Atom},
};
use derivation::Discriminant;
use std::fmt;
use TokenName::*;

pub type Token = Spanned<TokenKind>;

impl Token {
    pub(crate) const fn name(&self) -> TokenName {
        self.value.name()
    }

    #[allow(dead_code)]
    pub(crate) const fn provenance(&self) -> Provenance {
        use TokenKind::*;

        match self.value {
            Semicolon(provenance)
            | OpeningCurlyBracket(provenance)
            | ClosingCurlyBracket(provenance) => provenance,
            _ => Provenance::Source,
        }
    }

    pub(crate) fn is_line_break(&self) -> bool {
        matches!(self.value, TokenKind::Semicolon(Provenance::Lexer))
    }

    pub(crate) fn into_identifier(self) -> Option<Atom> {
        use TokenKind::*;

        obtain!(self.value, Word(atom) | Punctuation(atom) => atom)
    }

    pub(crate) fn into_number_literal(self) -> Option<Atom> {
        obtain!(self.value, TokenKind::NumberLiteral(number) => number)
    }

    pub(crate) fn into_text_literal(self) -> Option<Result<Atom, Diagnostic>> {
        use TokenKind::*;

        match self.value {
            TextLiteral(Ok(content)) => Some(Ok(content)),
            TextLiteral(Err(_)) => Some(Err(Diagnostic::error()
                .code(Code::E004)
                .message("unterminated text literal")
                .primary_span(self.span))),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Discriminant, Debug)]
#[discriminant(name: TokenName)]
pub enum TokenKind {
    Comment,
    DocumentationComment,
    Word(Atom),        // @Beacon @Beacon @Beacon @Task create newtype Word
    Punctuation(Atom), // @Beacon @Beacon @Beacon @Task create newtype Punctuation
    NumberLiteral(Atom),
    TextLiteral(Result<Atom, UnterminatedTextLiteral>),
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
    Apostrophe,
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
    /// For paths relative to the collection of linked components.
    Extern,
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
    /// For paths relative to the current namespace.
    Self_,
    /// For paths relative to the parent namespace.
    Super,
    /// For paths relative to the root module / the component root.
    Topmost,
    /// For type literals.
    Type,
    /// For use-declarations and use-bindings.
    ///
    /// The latter being use/in-expressions and statements.
    Use,
    EndOfInput,
    Illegal(char),
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();

        match *self {
            Self::Semicolon(Provenance::Lexer) => write!(f, "line break"),
            Self::Illegal(character) => {
                write!(f, "{} U+{:04X} `{}`", name, character as u32, character)
            }
            _ => write!(f, "{name}"),
        }
    }
}

impl TokenName {
    /// Test if the token may appear at the start of a [path](crate::syntax::ast::Path).
    pub(crate) const fn is_path_head(self) -> bool {
        matches!(self, Word | Punctuation) || self.is_path_hanger()
    }

    pub(crate) const fn is_path_hanger(self) -> bool {
        matches!(self, Extern | Topmost | Super | Self_)
    }

    /// Test if the token may terminate declarations.
    pub(crate) const fn is_terminator(self) -> bool {
        matches!(self, Semicolon | ClosingCurlyBracket | EndOfInput)
    }

    pub(crate) const fn introduces_indented_section(self) -> bool {
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
            Word => "word",
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
            Apostrophe => quoted!("'"),
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
            Extern => keyword!(extern),
            Data => keyword!(data),
            Do => keyword!(do),
            In => keyword!(in),
            Lazy => keyword!(lazy),
            Let => keyword!(let),
            Module => keyword!(module),
            Of => keyword!(of),
            Self_ => keyword!(self),
            Super => keyword!(super),
            Topmost => keyword!(topmost),
            Type => keyword!(Type),
            Use => keyword!(use),
            EndOfInput => "end of input",
            Illegal => "illegal character",
        })
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct UnterminatedTextLiteral;

/// The provenance of a token output by the lexer.
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum Provenance {
    /// The token corresponds 1:1 in type and data to the one found in the source code.
    Source,
    /// The token differs in type or in data from the one found in the source code.
    ///
    /// Thus, the token was artifically generated by the _lexer_. Those tokens are also
    /// called _virtual_ tokens.
    Lexer,
}
