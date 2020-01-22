//! The lexer.
//!
//! It parses indentation and dedentation intwo two pseudo tokens:
//! [TokenKind::Indentation] and [TokenKind::Dedentation] respectively.
//!
//! Natural number literals are directly converted into [num_bigint::BigUint]
//! and identifiers are interned.

mod error;

use num_bigint::BigUint;
pub(crate) use string_cache::DefaultAtom as Atom;

use std::cmp::Ordering;
use std::fmt;
use std::rc::Rc;
use std::str::FromStr;

use crate::error::Span;
pub use error::Error;
use error::ErrorKind;

// @Task move definition somewhere else
pub type Nat = Rc<BigUint>;

// @Task @Question rename SourceToken|Token to Token|PlainToken to mirror
// Expression|PlainExpression

/// A token with span information [`crate::error::Span`].
///
/// There is no actual reference to the source, we are working with indeces.
#[derive(Debug, Clone)]
pub struct SourceToken {
    pub inner: Token,
    // pub kind: TokenKind,
    // @Task add pub data: Option<TokenData>,
    pub span: Span,
}

impl SourceToken {
    pub fn new(token: Token, span: Span) -> Self {
        Self { inner: token, span }
    }
}

impl std::ops::Deref for SourceToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

/// A token *without* span information.
///
/// The payload contains processed (in constrast to raw source) data.
/// Currently, only identifiers (`self::Token::Identifier`) make use of this extra payload
/// as they get interned.
/// In the future, the lexer may want to store processed string literals as the work of
/// decoding escape characters (and what not) has already been done to check for lexical errors.
/// I don't know about number literals though. They shouldn't be interpreted for as long as possible
/// because they are of arbitrary precision anyways.
///
/// We could think about interning (non-reserved) punctuation.
///
/// The variant `Keyword` should not be understood as token kind on its own but a token class.
///
/// Note: Maybe, this design is over-engineered but I don't know where to elegantly store
/// interned strings.
#[derive(Debug, Clone)]
pub enum Token {
    DocumentationComment,
    Keyword(Keyword),
    Identifier(Atom),
    Punctuation,
    // @Note unused, just for demonstration on how the API is gonna look like
    // TextLiteral(String),
    NatLiteral(Nat),
    VerticalBar,
    Colon,
    Equals,
    Backslash,
    ThinArrow,
    WideArrow,
    Indentation,
    Dedentation,
    LineBreak,
    Semicolon,
    OpeningRoundBracket,
    ClosingRoundBracket,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            Self::DocumentationComment => TokenKind::DocumentationComment,
            Self::Keyword(keyword) => TokenKind::Keyword(*keyword),
            Self::Identifier(_) => TokenKind::Identifier,
            Self::Punctuation => TokenKind::Punctuation,
            Self::NatLiteral(_) => TokenKind::NatLiteral,
            Self::VerticalBar => TokenKind::VerticalBar,
            Self::Colon => TokenKind::Colon,
            Self::Equals => TokenKind::Equals,
            Self::Backslash => TokenKind::Backslash,
            Self::ThinArrow => TokenKind::ThinArrow,
            Self::WideArrow => TokenKind::WideArrow,
            Self::Indentation => TokenKind::Indentation,
            Self::Dedentation => TokenKind::Dedentation,
            Self::LineBreak => TokenKind::LineBreak,
            Self::Semicolon => TokenKind::Semicolon,
            Self::OpeningRoundBracket => TokenKind::OpeningRoundBracket,
            Self::ClosingRoundBracket => TokenKind::ClosingRoundBracket,
        }
    }
}

// @Note quite a lot of code duplication with `Token`. This is a common pitfall though
// known to the Rust community (enum with payloads plus separate enum tags enum)
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
    DocumentationComment,
    Keyword(Keyword),
    Identifier,
    Punctuation,

    // @Note unused
    // TextLiteral,
    NatLiteral,
    VerticalBar,
    Colon,
    Equals,
    Backslash,
    ThinArrow,
    WideArrow,
    Indentation,
    Dedentation,
    LineBreak,
    Semicolon,
    OpeningRoundBracket,
    ClosingRoundBracket,
}

// used for error reporting
impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::DocumentationComment => "documentation comment",
            Self::Keyword(keyword) => return write!(f, "keyword `{}`", keyword),
            Self::Identifier => "identifier",
            Self::Punctuation => "punctuation",
            Self::NatLiteral => "natural number literal",
            Self::VerticalBar => "vertical bar",
            Self::Colon => "colon",
            Self::Equals => "equals sign",
            Self::Backslash => "backslash",
            Self::ThinArrow => "thin arrow",
            Self::WideArrow => "wide arrow",
            Self::Indentation => "indentation",
            Self::Dedentation => "dedentation",
            Self::LineBreak => "line break",
            Self::Semicolon => "semicolon",
            Self::OpeningRoundBracket => "opening round bracket",
            Self::ClosingRoundBracket => "closing round bracket",
        })
    }
}

macro_rules! keywords {
    { $( $keyword:ident $representation:literal, )+ } => {
        #[derive(Debug, PartialEq, Eq, Clone, Copy)]
        pub enum Keyword {
            $( $keyword, )+
        }

        impl fmt::Display for Keyword {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                f.write_str(match self {
                    $( Self::$keyword => $representation, )+
                })
            }
        }

        impl FromStr for Keyword {
            type Err = ();

            fn from_str(source: &str) -> Result<Self, Self::Err> {
                Ok(match source {
                    $( $representation => Self::$keyword, )+
                    _ => return Err(()),
                })
            }
        }
    }
}

keywords! {
    As "as",
    Blank "_",
    Case "case",
    Data "data",
    Foreign "foreign",
    In "in",
    Let "let",
    Module "module",
    Nat "Nat",
    Of "of",
    Super "super",
    Type "Type",
    Use "use",
}

/// Amount of spaces making up one unit of indentation.
pub const INDENTATION_IN_SPACES: usize = 4;

const WHITESPACE: char = ' ';

fn is_punctuation(character: char) -> bool {
    match character {
        '.' | ':' | '+' | '-' | '~' | '=' | '<' | '>' | '*' | '^' | '!' | '?' | '|' | '/'
        | '\\' | '&' | '#' | '%' | '$' | '@' => true,
        _ => false,
    }
}

fn is_identifier_head(character: char) -> bool {
    character.is_ascii_alphabetic() || character == '_'
}

fn is_identifier_body(character: char) -> bool {
    character.is_ascii_alphanumeric() || character == '_'
}

fn is_identifier_tail(character: char) -> bool {
    character == '\''
}

fn extend_with_dedentation(tokens: &mut Vec<SourceToken>, start: usize, amount_of_spaces: usize) {
    if amount_of_spaces == 0 {
        return;
    }
    debug_assert_ne!(start, 0);
    let dedentation = SourceToken::new(Token::Dedentation, Span::new(start, start - 1));
    tokens.extend(std::iter::repeat(dedentation).take(amount_of_spaces / INDENTATION_IN_SPACES));
}

/// Lex source code into an array of tokens.
// @Task keep a bracket stack to report better error messages
pub fn lex(source: &str) -> Result<Vec<SourceToken>, Error> {
    let mut indexed_characters = source.char_indices().peekable();
    let mut tokens = Vec::new();
    let mut indentation_in_spaces = 0;

    // @Task text literal
    // @Bug sometimes we get consecutive dedent&indent, they should be flattened/
    // filtered out
    while let Some(&(index, character)) = indexed_characters.peek() {
        if character == WHITESPACE {
            indexed_characters.next();
            while let Some(&(_, character)) = indexed_characters.peek() {
                if character == WHITESPACE {
                    indexed_characters.next();
                } else {
                    break;
                }
            }
        } else if character == ';' {
            let start = index;
            let mut end = start;

            indexed_characters.next();

            if let Some(&(index, ';')) = indexed_characters.peek() {
                let mut documentation_comment = true;
                end = index;
                indexed_characters.next();

                if let Some(&(index, character)) = indexed_characters.peek() {
                    indexed_characters.next();

                    if character == ';' {
                        end = index;
                        documentation_comment = false;
                    }
                }

                while let Some(&(index, character)) = indexed_characters.peek() {
                    indexed_characters.next();

                    if character != '\n' {
                        if documentation_comment {
                            end = index;
                        }
                    } else {
                        break;
                    }
                }

                if documentation_comment {
                    tokens.push(SourceToken::new(
                        Token::DocumentationComment,
                        Span::new(start, end),
                    ))
                }
            } else {
                tokens.push(SourceToken::new(Token::Semicolon, Span::new(start, end)))
            }
        } else if is_identifier_head(character) {
            let start = index;
            let mut end = start;

            while let Some(&(index, character)) = indexed_characters.peek() {
                if is_identifier_body(character) {
                    end = index;
                    indexed_characters.next();
                } else {
                    if is_identifier_tail(character) {
                        while let Some(&(index, character)) = indexed_characters.peek() {
                            if is_identifier_tail(character) {
                                end = index;
                                indexed_characters.next();
                            } else {
                                break;
                            }
                        }
                    }
                    break;
                }
            }

            let span = Span::new(start, end);

            tokens.push(SourceToken::new(
                if let Ok(keyword) = source[start..=end].parse() {
                    Token::Keyword(keyword)
                } else {
                    Token::Identifier(Atom::from(&source[span.range()]))
                },
                span,
            ));
        } else if character == '\n' {
            tokens.push(SourceToken::new(Token::LineBreak, Span::new(index, index)));

            indexed_characters.next();

            let mut spaces = 0;
            let start = index + 1;
            let mut end = start;

            while let Some(&(index, character)) = indexed_characters.peek() {
                if character == WHITESPACE {
                    end = index;
                    spaces += 1;
                    indexed_characters.next();
                } else {
                    break;
                }
            }

            let change = spaces.cmp(&indentation_in_spaces);

            let absolute_difference = match change {
                Ordering::Greater => spaces - indentation_in_spaces,
                Ordering::Less => indentation_in_spaces - spaces,
                Ordering::Equal => continue,
            };

            let span = Span::new(end - absolute_difference + 1, end);

            if absolute_difference % INDENTATION_IN_SPACES != 0
                || change == Ordering::Greater && absolute_difference > INDENTATION_IN_SPACES
            {
                return Err(Error {
                    kind: ErrorKind::InvalidIndentation(absolute_difference),
                    span,
                });
            }

            match change {
                Ordering::Greater => tokens.push(SourceToken::new(Token::Indentation, span)),
                Ordering::Less => extend_with_dedentation(&mut tokens, start, absolute_difference),
                Ordering::Equal => unreachable!(),
            }

            indentation_in_spaces = spaces;
        } else if is_punctuation(character) {
            let start = index;
            let mut end = start;
            indexed_characters.next();

            while let Some(&(index, character)) = indexed_characters.peek() {
                if is_punctuation(character) {
                    end = index;
                    indexed_characters.next();
                } else {
                    break;
                }
            }

            tokens.push(SourceToken::new(
                match &source[start..=end] {
                    ":" => Token::Colon,
                    "=" => Token::Equals,
                    "|" => Token::VerticalBar,
                    "\\" => Token::Backslash,
                    "->" => Token::ThinArrow,
                    "=>" => Token::WideArrow,
                    _ => Token::Punctuation,
                },
                Span::new(start, end),
            ))
        }
        // @Task verify that it ends with a space instead of any non-digit
        else if character.is_ascii_digit() {
            let start = index;
            let mut end = start;
            indexed_characters.next();

            while let Some(&(index, character)) = indexed_characters.peek() {
                if character.is_ascii_digit() {
                    end = index;
                    indexed_characters.next();
                } else {
                    break;
                }
            }

            tokens.push(SourceToken::new(
                Token::NatLiteral(Rc::new(BigUint::from_str(&source[start..=end]).unwrap())),
                Span::new(start, end),
            ));
        } else {
            indexed_characters.next();

            tokens.push(SourceToken::new(
                match character {
                    '(' => Token::OpeningRoundBracket,
                    ')' => Token::ClosingRoundBracket,
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::IllegalCharacter(character),
                            span: Span::new(index, index),
                        })
                    }
                },
                Span::new(index, index),
            ));
        }
    }

    let last_index = tokens.len() - 1;
    extend_with_dedentation(&mut tokens, last_index, indentation_in_spaces);
    Ok(tokens)
}
