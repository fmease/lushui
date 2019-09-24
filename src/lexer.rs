use crate::error::Span;
use std::cmp::Ordering;
use std::fmt;
use std::str::FromStr;

#[cfg_attr(test, derive(PartialEq, Eq))]
#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    // @Task add pub data: Option<TokenData>,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}

// @Task move payloads to a `TokenData` enum so the parser can match on
// the kind
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
    DocumentationComment,
    Keyword(Keyword),
    Identifier,
    Punctuation,
    TextLiteral(String),

    Colon,
    Equals,
    Backslash,
    ThinArrow,
    WideArrow,
    Indentation,
    Dedentation,
    LineBreak,
    Semicolon,
    Bracket(Bracket),
}

impl TokenKind {
    pub fn is_text_literal(&self) -> bool {
        match self {
            Self::TextLiteral(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Bracket {
    OpeningRound,
    ClosingRound,
    OpeningCurly,
    ClosingCurly,
}

impl Bracket {
    // @Question impl std::ops::Not?
    pub fn invert(self) -> Self {
        match self {
            Self::OpeningRound => Self::ClosingRound,
            Self::ClosingRound => Self::OpeningRound,
            Self::OpeningCurly => Self::ClosingCurly,
            Self::ClosingCurly => Self::OpeningCurly,
        }
    }
}

// @Task implement Copy
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Keyword {
    Alias,
    As,
    Blank,
    Case,
    Data,
    Foreign,
    Hole,
    In,
    Let,
    Module,
    Of,
    Parent,
    Root,
    Type,
    Unsafe,
}

impl FromStr for Keyword {
    type Err = ();

    fn from_str(source: &str) -> Result<Self, Self::Err> {
        Ok(match source {
            "alias" => Self::Alias,
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
            "unsafe" => Self::Unsafe,
            _ => return Err(()),
        })
    }
}

const INDENTATION_SIZE: usize = 4;

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

fn extend_with_dedentation(tokens: &mut Vec<Token>, start: usize, amount_of_spaces: usize) {
    if amount_of_spaces == 0 {
        return;
    }
    debug_assert_ne!(start, 0);
    let dedentation = Token::new(TokenKind::Dedentation, start..=start - 1);
    tokens.extend(std::iter::repeat(dedentation).take(amount_of_spaces / INDENTATION_SIZE));
}

// @Task keep a bracket stack to report better error messages
pub fn lex(source: &str) -> Result<Vec<Token>, Error> {
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
                    tokens.push(Token::new(TokenKind::DocumentationComment, start..=end))
                }
            } else {
                tokens.push(Token::new(TokenKind::Semicolon, start..=end))
            }
        } else if character == SIGIL || is_identifier_head(character) {
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

            let span = start..=end;
            if keyword_candidate {
                if let Ok(keyword_kind) = source[start + 1..=end].parse() {
                    tokens.push(Token::new(TokenKind::Keyword(keyword_kind), span))
                } else {
                    return Err(Error {
                        kind: ErrorKind::UnknownKeyword(source[start + 1..=end].to_owned()),
                        span,
                    });
                }
            } else {
                tokens.push(Token::new(TokenKind::Identifier, span))
            }
        } else if character == '\n' {
            tokens.push(Token::new(TokenKind::LineBreak, index..=index));

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

            let span = end - absolute_difference + 1..=end;

            if absolute_difference % INDENTATION_SIZE != 0
                || change == Ordering::Greater && absolute_difference > INDENTATION_SIZE
            {
                return Err(Error {
                    kind: ErrorKind::InvalidIndentation(absolute_difference),
                    span,
                });
            }

            match change {
                Ordering::Greater => tokens.push(Token::new(TokenKind::Indentation, span)),
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

            tokens.push(Token::new(
                match &source[start..=end] {
                    ":" => TokenKind::Colon,
                    "=" => TokenKind::Equals,
                    "\\" => TokenKind::Backslash,
                    "->" => TokenKind::ThinArrow,
                    "=>" => TokenKind::WideArrow,
                    _ => TokenKind::Punctuation,
                },
                start..=end,
            ))
        } else {
            indexed_characters.next();

            tokens.push(Token::new(
                match character {
                    '(' => TokenKind::Bracket(Bracket::OpeningRound),
                    ')' => TokenKind::Bracket(Bracket::ClosingRound),
                    '{' => TokenKind::Bracket(Bracket::OpeningCurly),
                    '}' => TokenKind::Bracket(Bracket::ClosingCurly),
                    _ => {
                        return Err(Error {
                            kind: ErrorKind::IllegalCharacter(character),
                            span: index..=index,
                        })
                    }
                },
                index..=index,
            ));
        }
    }

    let last_index = tokens.len() - 1;
    extend_with_dedentation(&mut tokens, last_index, indentation_in_spaces);
    Ok(tokens)
}

#[derive(Debug)] // @Temp
#[cfg_attr(test, derive(PartialEq, Eq))]
pub struct Error {
    pub kind: ErrorKind,
    pub span: Span,
}

#[derive(Debug)] // @Temp
#[cfg_attr(test, derive(PartialEq, Eq))]
pub enum ErrorKind {
    IllegalCharacter(char),
    UnknownKeyword(String),
    InvalidIndentation(usize),
}

impl fmt::Display for ErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::IllegalCharacter(character) => write!(
                f,
                "illegal character U+{:04X} `{}`",
                *character as u32, character,
            ),
            Self::UnknownKeyword(source) => write!(f, "unknown keyword `{}`", source),
            Self::InvalidIndentation(indentation) => write!(
                f,
                "invalid indentation consisting of {} spaces",
                indentation
            ),
        }
    }
}

fn _documentation_comment_inner_span(span: &Span) -> Span {
    const SHIFT: usize = ";;".len();
    span.start() + SHIFT..=*span.end()
}

#[cfg(test)]
mod test {
    use super::{lex, Error, ErrorKind, Keyword, Token, TokenKind};

    #[test]
    fn whitespace() {
        assert_eq!(lex(""), Ok(Vec::new()));
        assert_eq!(lex("        "), Ok(Vec::new()));
    }

    #[test]
    fn identifier() {
        assert_eq!(
            lex("foobar"),
            Ok(vec![Token::new(TokenKind::Identifier, 0..=5)])
        );
        assert_eq!(
            lex("     Oddly_Beautiful "),
            Ok(vec![Token::new(TokenKind::Identifier, 5..=19)]),
        );
        assert_eq!(
            lex("alpha beta' gamma'' delta'''"),
            Ok(vec![
                Token::new(TokenKind::Identifier, 0..=4),
                Token::new(TokenKind::Identifier, 6..=10),
                Token::new(TokenKind::Identifier, 12..=18),
                Token::new(TokenKind::Identifier, 20..=27),
            ]),
        );
        assert_eq!(
            lex("_'_'_"),
            Ok(vec![
                Token::new(TokenKind::Identifier, 0..=1),
                Token::new(TokenKind::Identifier, 2..=3),
                Token::new(TokenKind::Identifier, 4..=4),
            ]),
        );
        {
            static SOURCE: &str = "       QUUX ";
            let left =
                lex(SOURCE).map(|tokens| tokens.get(0).map(|token| SOURCE.get(token.span.clone())));
            assert_eq!(left, Ok(Some(Some("QUUX"))));
        }
    }

    #[test]
    fn keyword() {
        assert_eq!(
            lex("'let"),
            Ok(vec![Token::new(TokenKind::Keyword(Keyword::Let), 0..=6)]),
        );
        assert_eq!(
            lex("'let'let"),
            Err(Error {
                kind: ErrorKind::UnknownKeyword("let'".to_owned()),
                span: 0..=7
            }),
        );
        assert_eq!(
            lex("   'Type Type "),
            Ok(vec![
                Token::new(TokenKind::Keyword(Keyword::Type), 3..=7),
                Token::new(TokenKind::Identifier, 9..=12),
            ]),
        );
    }

    #[test]
    fn illegal_character() {
        assert_eq!(
            lex("§"),
            Err(Error {
                kind: ErrorKind::IllegalCharacter('§'),
                span: 0..=0,
            }),
        );

        assert_eq!(
            lex("      [      @"),
            Err(Error {
                kind: ErrorKind::IllegalCharacter('['),
                span: 6..=6,
            }),
        );
    }

    #[test]
    fn indentation() {
        // @Beacon @Task
    }

    #[test]
    fn punctuation() {
        // @Task
    }
}
