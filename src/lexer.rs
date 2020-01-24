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

use std::{cmp::Ordering, fmt, rc::Rc, str::FromStr};

use crate::error::Span;
pub use error::Error;
use error::ErrorKind;

pub type Nat = Rc<BigUint>;

/// A token with span information [`crate::error::Span`].
///
/// There is no actual reference to the source, we are working with indeces.
#[derive(Debug, Clone)]
pub struct SourceToken {
    pub inner: Token,
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

tokens! {
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
    Token, ::kind, TokenKind {
        DocumentationComment "documentation comment",
        Identifier(Atom) "identifier",
        Punctuation "punctuation",
        NatLiteral(Nat) "natural number literal",
        VerticalBar "vertical bar",
        Colon "colon",
        Equals "equals sign",
        Backslash "backslash",
        ThinArrow "thin arrow",
        WideArrow "wide arrow",
        Indentation "indentation",
        Dedentation "dedentation",
        LineBreak "line break",
        OpeningRoundBracket "opening round bracket",
        ClosingRoundBracket "closing round bracket",
        As "keyword `as`",
        Case "keyword `case`",
        Data "keyword `data`",
        Foreign "keyword `foreign`",
        In "keyword `in`",
        Let "keyword `let`",
        Module "keyword `module`",
        Nat "keyword `Nat`",
        Of "keyword `of`",
        Super "keyword `super`",
        Type "keyword `Type`",
        Use "keyword `use`",
    }
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

fn parse_keyword(source: &str) -> Option<Token> {
    Some(match source {
        "as" => Token::As,
        "case" => Token::Case,
        "data" => Token::Data,
        "foreign" => Token::Foreign,
        "in" => Token::In,
        "let" => Token::Let,
        "module" => Token::Module,
        "Nat" => Token::Nat,
        "of" => Token::Of,
        "super" => Token::Super,
        "Type" => Token::Type,
        "Use" => Token::Use,
        _ => return None,
    })
}

fn parse_reserved_punctuation(source: &str) -> Option<Token> {
    Some(match source {
        ":" => Token::Colon,
        "=" => Token::Equals,
        "|" => Token::VerticalBar,
        "\\" => Token::Backslash,
        "->" => Token::ThinArrow,
        "=>" => Token::WideArrow,
        _ => return None,
    })
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

    // @Bug sometimes we get consecutive dedent&indent, they should be flattened/filtered out
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
        }
        // @Task merge consecutive documentation comments to save on memory
        else if character == ';' {
            let start = index;
            let mut end = start;
            let mut documentation = true;

            indexed_characters.next();

            if let Some(&(index, ';')) = indexed_characters.peek() {
                documentation = false;
                end = index;
                indexed_characters.next();
            }

            while let Some(&(index, character)) = indexed_characters.peek() {
                indexed_characters.next();

                if character != '\n' {
                    if documentation {
                        end = index;
                    }
                } else {
                    break;
                }
            }

            if documentation {
                tokens.push(SourceToken::new(
                    Token::DocumentationComment,
                    Span::new(start, end),
                ))
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
                match parse_keyword(&source[start..=end]) {
                    Some(keyword) => keyword,
                    None => Token::Identifier(Atom::from(&source[span.range()])),
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
                parse_reserved_punctuation(&source[start..=end]).unwrap_or(Token::Punctuation),
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
    // @Question maybe also add a artificial line break to simplify parsing or would we still need to handle EOI special?
    Ok(tokens)
}

macro tokens($( #[$attr:meta] )+ $Token:ident, ::$kind:ident, $TokenKind:ident { $( $token:ident $( ($payload:ident) )? $name:literal, )+ }) {
    $( #[$attr] )+
    #[derive(Debug, Clone)]
    pub enum $Token {
        $( $token $( ($payload) )?, )+
    }

    // @Note quite a lot of code duplication with `Token`. This is a common pitfall though
    // known to the Rust community (enum with payloads plus separate enum tags enum)
    // @Note we could instead define Token like
    // `struct Token { kind: TokenKind, data: TokenData, span: Span }` with
    // `union TokenData { identifier: Atom, nat_literal: Nat, none: () }`
    // Using a union to save 8 bytes over the safe version where `TokenData` is an enum
    #[derive(Debug, PartialEq, Eq, Clone, Copy)]
    pub enum $TokenKind {
        $( $token, )+
    }

    impl $Token {
        pub fn $kind(&self) -> $TokenKind {
            match self {
                $( Self::$token $( (discard!($payload)) )? => $TokenKind::$token, )+
            }
        }
    }

    // used for error reporting
    impl fmt::Display for $TokenKind {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            f.write_str(match self {
                $( Self::$token => $name, )+
            })
        }
    }
}

macro discard($id:ident) { _ }
