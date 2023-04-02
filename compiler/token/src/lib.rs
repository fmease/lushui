//! The tokens emitted by the lexer.
#![feature(decl_macro, stmt_expr_attributes, int_roundings)]

use derivation::Str;
use span::Spanned;
use std::{cmp::Ordering, fmt};
use utilities::{quoted, Atom};
pub use word::Word;
use BareToken::*;

mod word;

pub type Token = Spanned<BareToken>;

#[derive(Clone, PartialEq, Eq, Debug)]
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
    ClosingCurlyBracket,
    ClosingRoundBracket,
    ClosingSquareBracket,
    OpeningCurlyBracket,
    OpeningRoundBracket,
    OpeningSquareBracket,
    Semicolon,
    //
    // Other Tokens
    //
    Dedentation,
    EndOfInput,
    Indentation,
    LineBreak,
    NumberLiteral(Atom),
    Symbol(Atom),
    TextLiteral(Atom),
    Word(Atom),
}

impl BareToken {
    // @Task move to lexer
    pub const fn introduces_indented_section(&self) -> bool {
        matches!(self, Do | Of)
    }
}

impl fmt::Display for BareToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            As => quoted!("as"),
            Case => quoted!("case"),
            Data => quoted!("data"),
            Do => quoted!("do"),
            Extern => quoted!("extern"),
            ForUpper => quoted!("For"),
            ForLower => quoted!("for"),
            In => quoted!("in"),
            Let => quoted!("let"),
            Module => quoted!("module"),
            Of => quoted!("of"),
            Self_ => quoted!("self"),
            Super => quoted!("super"),
            Topmost => quoted!("topmost"),
            Underscore => quoted!("_"),
            Use => quoted!("use"),
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
            Dedentation => "dedentation",
            EndOfInput => "end of input",
            Self::Indentation => "indentation",
            LineBreak => "line break",
            NumberLiteral(_) => "number literal",
            Symbol(_) => "symbol",
            TextLiteral(_) => "text literal",
            Self::Word(_) => "word",
        })
    }
}

// @Task move to lexer
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
            Self::Curly => OpeningCurlyBracket,
        }
    }

    pub const fn closing(self) -> BareToken {
        use BareToken::*;

        match self {
            Self::Round => ClosingRoundBracket,
            Self::Square => ClosingSquareBracket,
            Self::Curly => ClosingCurlyBracket,
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
