pub(crate) use string_cache::DefaultAtom as Atom;

mod error;

use crate::error::Span;
use std::cmp::Ordering;
use std::str::FromStr;

pub use error::Error;
use error::ErrorKind;

/// A token with span information [`crate::error::Span`].
///
/// There is no actual reference to the source, we are working with indeces.
#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub struct SourceToken {
    pub token: Token,
    // pub kind: TokenKind,
    // @Task add pub data: Option<TokenData>,
    pub span: Span,
}

impl SourceToken {
    pub fn new(token: Token, span: Span) -> Self {
        Self { token, span }
    }
}

impl std::ops::Deref for SourceToken {
    type Target = Token;

    fn deref(&self) -> &Self::Target {
        &self.token
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
#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub enum Token {
    DocumentationComment,
    Keyword(Keyword),
    Identifier(Atom),
    Punctuation,
    // @Note unused, just for demonstration on how the API is gonna look like
    // TextLiteral(String),
    // NumberLiteral,
    Comma,
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
            Self::Comma => TokenKind::Comma,
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
    // NumberLiteral,
    Comma,
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

// @Task implement Copy
/// Enumeration of all Lushui keywords.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Keyword {
    /// The keyword `'as` renaming bindings in certain syntactic constructs.
    As,
    /// The keyword `'_` standing for either an expression that should be infered or an
    /// unnameable identifier.
    Blank,
    /// The keyword `'case` prefixing cases analyses.
    Case,
    /// The keyword `'data` introducing data declarations.
    Data,
    /// The keyword `'foreign` signaling a FFA.
    Foreign,
    /// The keyword `'hole` used for typed holes.
    Hole,
    /// The keyword `'in` being part of let/in-expressions
    In,
    /// The keyword `'let` introducing let-declarations which bind expressions.
    Let,
    /// The keyword `'module` used for module declarations.
    Module,
    /// The keyword `'of` found in case analyses.
    Of,
    /// The keyword `'Parent` refering to a parent module.
    Parent,
    /// The keyword `'Root` standing for the root module.
    Root,
    /// The keyword `'Type` being the type of types.
    Type,
    /// The keyword `'use` prefixing use declarations.
    Use,
    // Unsafe,
}

/// Parsing keywords *without* the sigil `'`.
impl FromStr for Keyword {
    type Err = ();

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Ok(match source {
            "as" => Self::As,
            "_" => Self::Blank,
            "case" => Self::Case,
            "data" => Self::Data,
            "foreign" => Self::Foreign,
            "hole" => Self::Hole,
            "in" => Self::In,
            "let" => Self::Let,
            "module" => Self::Module,
            "of" => Self::Of,
            "Parent" => Self::Parent,
            "Root" => Self::Root,
            "Type" => Self::Type,
            // "unsafe" => Self::Unsafe,
            _ => return Err(()),
        })
    }
}

/// Amount of spaces making up one unit of indentation.
pub const INDENTATION_IN_SPACES: usize = 4;

const SIGIL: char = '\'';
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

/// Lex source code into a vector of tokens.
// @Task keep a bracket stack to report better error messages
pub fn lex(source: &str) -> Result<Vec<SourceToken>, Error> {
    let mut indexed_characters = source.char_indices().peekable();
    let mut tokens = Vec::new();
    let mut indentation_in_spaces = 0;

    // @Task number literals, text literal
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
                    tokens.push(SourceToken::new(Token::DocumentationComment, Span::new(start, end)))
                }
            } else {
                tokens.push(SourceToken::new(Token::Semicolon, Span::new(start, end)))
            }
        }
        // @Task dotted identifiers (need to be lexed, cannot be parsed bc whitespace matters)
        else if character == SIGIL || is_identifier_head(character) {
            let keyword_candidate = character == SIGIL;
            let start = index;
            let mut end = start;

            if keyword_candidate {
                indexed_characters.next();
            }

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
            if keyword_candidate {
                if let Ok(keyword_kind) = source[start + 1..=end].parse() {
                    tokens.push(SourceToken::new(Token::Keyword(keyword_kind), span))
                } else {
                    return Err(Error {
                        kind: ErrorKind::UnknownKeyword(source[start + 1..=end].to_owned()),
                        span,
                    });
                }
            } else {
                tokens.push(SourceToken::new(
                    Token::Identifier(Atom::from(&source[span.range()])),
                    span,
                ))
            }
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
                    "\\" => Token::Backslash,
                    "->" => Token::ThinArrow,
                    "=>" => Token::WideArrow,
                    _ => Token::Punctuation,
                },
                Span::new(start, end),
            ))
        } else {
            indexed_characters.next();

            tokens.push(SourceToken::new(
                match character {
                    '(' => Token::OpeningRoundBracket,
                    ')' => Token::ClosingRoundBracket,
                    ',' => Token::Comma,
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
