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
    //
    // Comment-Like Tokens
    //
    Shebang,
    Comment,
    DocumentationComment,
    //
    // Keywords
    //
    As,
    Case,
    Data,
    Do,
    Extern,
    /// `For`
    ForUpper,
    /// `for`
    ForLower,
    In,
    Let,
    Module,
    Of,
    /// `_`
    Underscore,
    /// `self`
    Self_,
    Super,
    Topmost,
    Use,
    //
    // Reserved Symbols
    //
    At,
    Backslash,
    Colon,
    Dot,
    Equals,
    DoubleAsterisk,
    DoubleColon,
    QuestionMark,
    /// `<-`
    ThinArrowLeft,
    /// `->`
    ThinArrowRight,
    /// `=>`
    WideArrowRight,
    //
    // Punctuation
    //
    Apostrophe,
    ClosingCurlyBracket(Provenance),
    ClosingRoundBracket,
    ClosingSquareBracket,
    OpeningCurlyBracket(Provenance),
    OpeningRoundBracket,
    OpeningSquareBracket,
    Semicolon(Provenance),
    //
    // Other Tokens
    //
    EndOfInput,
    NumberLiteral(Atom),
    Symbol(Atom),
    TextLiteral(Atom),
    Word(Atom),
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
            //
            // Comment-Like Tokens
            //
            Shebang => "shebang",
            Comment => "comment",
            DocumentationComment => "documentation comment",
            //
            // Keywords
            //
            As => keyword!(as),
            Case => keyword!(case),
            Data => keyword!(data),
            Do => keyword!(do),
            Extern => keyword!(extern),
            ForUpper => keyword!(For),
            ForLower => keyword!(for),
            In => keyword!(in),
            Let => keyword!(let),
            Module => keyword!(module),
            Of => keyword!(of),
            Self_ => keyword!(self),
            Super => keyword!(super),
            Topmost => keyword!(topmost),
            Underscore => quoted!("_"),
            Use => keyword!(use),
            //
            // Reserved Symbols
            At => quoted!("@"),
            Backslash => quoted!(r"\"),
            Colon => quoted!(":"),
            Dot => quoted!("."),
            DoubleAsterisk => quoted!("**"),
            DoubleColon => quoted!("::"),
            Equals => quoted!("="),
            QuestionMark => quoted!("?"),
            Semicolon => quoted!(";"),
            ThinArrowLeft => quoted!("<-"),
            ThinArrowRight => quoted!("->"),
            WideArrowRight => quoted!("=>"),
            //
            // Punctuation
            //
            Apostrophe => quoted!("'"),
            ClosingCurlyBracket => quoted!("}"),
            ClosingRoundBracket => quoted!(")"),
            ClosingSquareBracket => quoted!("]"),
            OpeningCurlyBracket => quoted!("{"),
            OpeningRoundBracket => quoted!("("),
            OpeningSquareBracket => quoted!("["),
            //
            // Other Tokens
            //
            EndOfInput => "end of input",
            NumberLiteral => "number literal",
            Symbol => "symbol",
            TextLiteral => "text literal",
            Word => "word",
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
