//! The tokens emitted by the lexer.
#![feature(decl_macro)]

use derivation::Discriminant;
use lushui_diagnostics::{Diagnostic, ErrorCode};
use lushui_span::Spanned;
use lushui_utilities::{obtain, quoted, Atom};
use std::fmt;
pub use word::Word;
use TokenName::*;

mod word;

pub type Token = Spanned<BareToken>;

pub trait TokenExt {
    fn name(&self) -> TokenName;
    fn provenance(&self) -> Provenance;
    fn is_line_break(&self) -> bool;
    fn into_identifier(self) -> Option<Atom>;
    fn into_number_literal(self) -> Option<Atom>;
    fn into_text_literal(self) -> Option<Result<Atom, Diagnostic>>;
}

impl TokenExt for Token {
    fn name(&self) -> TokenName {
        self.bare.name()
    }

    fn provenance(&self) -> Provenance {
        use BareToken::*;

        match self.bare {
            Semicolon(provenance)
            | OpeningCurlyBracket(provenance)
            | ClosingCurlyBracket(provenance) => provenance,
            _ => Provenance::Source,
        }
    }

    fn is_line_break(&self) -> bool {
        matches!(self.bare, BareToken::Semicolon(Provenance::Lexer))
    }

    fn into_identifier(self) -> Option<Atom> {
        use BareToken::*;

        obtain!(self.bare, Word(atom) | Punctuation(atom) => atom)
    }

    fn into_number_literal(self) -> Option<Atom> {
        obtain!(self.bare, BareToken::NumberLiteral(number) => number)
    }

    fn into_text_literal(self) -> Option<Result<Atom, Diagnostic>> {
        use BareToken::*;

        match self.bare {
            TextLiteral(Ok(content)) => Some(Ok(content)),
            TextLiteral(Err(_)) => Some(Err(Diagnostic::error()
                .code(ErrorCode::E047)
                .message("unterminated text literal")
                .primary_span(self.span))),
            _ => None,
        }
    }
}

#[derive(Clone, PartialEq, Eq, Discriminant, Debug)]
#[discriminant(name: TokenName)]
pub enum BareToken {
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

impl fmt::Display for BareToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();

        match *self {
            Self::Semicolon(Provenance::Lexer) => write!(f, "line break"),
            Self::Illegal(character) => {
                write!(f, "{} U+{:04X} ‘{}’", name, character as u32, character)
            }
            _ => write!(f, "{name}"),
        }
    }
}

impl TokenName {
    /// Test if the token may appear at the start of a [path](crate::syntax::ast::Path).
    pub const fn is_path_head(self) -> bool {
        matches!(self, Word | Punctuation) || self.is_path_hanger()
    }

    pub const fn is_path_hanger(self) -> bool {
        matches!(self, Extern | Topmost | Super | Self_)
    }

    /// Test if the token may terminate declarations.
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
