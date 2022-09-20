//! The tokens emitted by the lexer.
#![feature(decl_macro, stmt_expr_attributes, int_roundings)]

use derivation::{Discriminant, Str};
use span::Spanned;
use std::{cmp::Ordering, fmt};
use utilities::{obtain, quoted, Atom};
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
    fn into_text_literal(self) -> Option<Atom>;
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

        obtain!(self.bare, Word(identifier) | Symbol(identifier) => identifier)
    }

    fn into_number_literal(self) -> Option<Atom> {
        obtain!(self.bare, BareToken::NumberLiteral(number) => number)
    }

    fn into_text_literal(self) -> Option<Atom> {
        obtain!(self.bare, BareToken::TextLiteral(text) => text)
    }
}

#[derive(Clone, PartialEq, Eq, Discriminant, Debug)]
#[discriminant(name: TokenName)]
pub enum BareToken {
    Shebang,
    Comment,
    DocumentationComment,
    Word(Atom),   // @Task use crate::Word
    Symbol(Atom), // @Task create newtype Symbol
    NumberLiteral(Atom),
    TextLiteral(Atom),
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
    /// For use-declarations and use-bindings.
    ///
    /// The latter being use/in-expressions and statements.
    Use,
    EndOfInput,
}

impl fmt::Display for BareToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let name = self.name();

        match *self {
            Self::Semicolon(Provenance::Lexer) => write!(f, "line break"),
            _ => write!(f, "{name}"),
        }
    }
}

impl TokenName {
    /// Test if the token may appear at the start of a path.
    pub const fn is_path_head(self) -> bool {
        matches!(self, Word | Symbol) || self.is_path_hanger()
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
            Shebang => "shebang",
            Comment => "comment",
            DocumentationComment => "documentation comment",
            Word => "word",
            Symbol => "symbol",
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
            Use => keyword!(use),
            EndOfInput => "end of input",
        })
    }
}

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

// @Question should this reside in `lexer`?
pub const fn is_symbol(character: char) -> bool {
    #[rustfmt::skip]
    matches!(
        character,
        '.' | ':' | '+' | '-' | '~' | '=' | '<' | '>' | '*' | '^' |
        '!' | '?' | '|' | '/' | '\\' | '&' | '#' | '%' | '$' | '@'
    )
}

#[derive(PartialEq, Eq, Debug)]
pub struct Bracket {
    pub kind: BracketKind,
    pub orientation: BracketOrientation,
}

impl Bracket {
    pub const fn new(kind: BracketKind, orientation: BracketOrientation) -> Self {
        Self { kind, orientation }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Str, Debug)]
#[format(dash_case)]
pub enum BracketKind {
    Round,
    Square,
    Curly,
}

impl BracketKind {
    pub const fn opening(self) -> BareToken {
        use BareToken::*;

        match self {
            Self::Round => OpeningRoundBracket,
            Self::Square => OpeningSquareBracket,
            Self::Curly => OpeningCurlyBracket(Provenance::Source),
        }
    }

    pub const fn closing(self) -> BareToken {
        use BareToken::*;

        match self {
            Self::Round => ClosingRoundBracket,
            Self::Square => ClosingSquareBracket,
            Self::Curly => ClosingCurlyBracket(Provenance::Source),
        }
    }
}

impl fmt::Display for BracketKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

#[derive(Clone, Copy, Str, PartialEq, Eq, Debug)]
#[format(dash_case)]
pub enum BracketOrientation {
    Opening,
    Closing,
}

impl std::ops::Not for BracketOrientation {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Opening => Self::Closing,
            Self::Closing => Self::Opening,
        }
    }
}

impl fmt::Display for BracketOrientation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}

/// The unit indentation in spaces.
pub const INDENTATION: Spaces = Indentation::UNIT.to_spaces();

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub struct Spaces(pub usize);

impl Spaces {
    /// The ordering / direction / sign together with the absolute difference.
    pub fn difference(self, other: Self) -> (Ordering, Self) {
        let change = self.0.cmp(&other.0);
        let difference = match change {
            Ordering::Greater => self.0 - other.0,
            Ordering::Less => other.0 - self.0,
            Ordering::Equal => 0,
        };
        (change, Self(difference))
    }
}

impl<S: Into<Spaces>> std::ops::Sub<S> for Spaces {
    type Output = Self;

    fn sub(self, other: S) -> Self::Output {
        Self(self.0.saturating_sub(other.into().0))
    }
}

impl<S: Into<Spaces>> std::ops::SubAssign<S> for Spaces {
    fn sub_assign(&mut self, other: S) {
        *self = *self - other;
    }
}

// @Note this whole type seems really over-engineered: it is only used in
//       3 lines in the lexer. the actual logic is the TryFrom impl, smh.
//       inline it into the lexer.
#[derive(Clone, Copy)]
pub struct Indentation(pub usize);

impl Indentation {
    pub const UNIT: Self = Self(1);

    pub const fn to_spaces(self) -> Spaces {
        const INDENTATION_IN_SPACES: usize = 4;

        Spaces(self.0 * INDENTATION_IN_SPACES)
    }
}

impl From<Indentation> for Spaces {
    fn from(indentation: Indentation) -> Self {
        indentation.to_spaces()
    }
}

impl TryFrom<(Ordering, Spaces)> for Indentation {
    type Error = (Indentation, IndentationError);

    fn try_from((change, spaces): (Ordering, Spaces)) -> Result<Self, Self::Error> {
        if spaces.0 % INDENTATION.0 != 0 {
            return Err((
                Indentation(spaces.0.div_ceil(INDENTATION.0)),
                IndentationError::Misaligned,
            ));
        }

        if change == Ordering::Greater && spaces.0 > INDENTATION.0 {
            return Err((Indentation::UNIT, IndentationError::TooDeep));
        }

        Ok(Indentation(spaces.0 / INDENTATION.0))
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum IndentationError {
    Misaligned,
    TooDeep,
}
