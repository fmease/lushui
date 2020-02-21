//! The lexer.
//!
//! It parses indentation and dedentation intwo two pseudo tokens:
//! [TokenKind::Indentation] and [TokenKind::Dedentation] respectively.
//!
//! Natural number literals are directly converted into [num_bigint::BigUint]
//! and identifiers are interned.

use std::{cmp::Ordering, fmt, rc::Rc, str::FromStr};

use crate::{
    diagnostic::Diagnostic,
    span::{LocalByteIndex, LocalSpan, SourceFile, SourceMap, Span},
    Atom, Nat,
};

/// A token with span information.
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
        TextLiteral(String) "text literal",
        VerticalBar "vertical bar",
        Colon "colon",
        DoubleColon "double colon",
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
        Text "keyword `Text`",
        Of "keyword `of`",
        Super "keyword `super`",
        Type "keyword `Type`",
        Use "keyword `use`",
        EndOfInput "end of input",
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
        "Text" => Token::Text,
        "Type" => Token::Type,
        "use" => Token::Use,
        _ => return None,
    })
}

fn parse_reserved_punctuation(source: &str) -> Option<Token> {
    Some(match source {
        ":" => Token::Colon,
        "::" => Token::DoubleColon,
        "=" => Token::Equals,
        "|" => Token::VerticalBar,
        "\\" => Token::Backslash,
        "->" => Token::ThinArrow,
        "=>" => Token::WideArrow,
        _ => return None,
    })
}

// @Bug implementation seems hacky with illegal case start == 0, @Note this is one ugly function
fn extend_with_dedentation(
    source: &SourceFile,
    tokens: &mut Vec<SourceToken>,
    start: LocalByteIndex,
    amount_of_spaces: usize,
) {
    if amount_of_spaces == 0 {
        return;
    }

    debug_assert_ne!(start, LocalByteIndex::new(0));

    // @Task use better span (it should span 4 spaces if possible) @Note you need to go backwards
    let dedentation = SourceToken::new(
        Token::Dedentation,
        Span::from_local(source, LocalSpan::from(start)),
    );
    tokens.extend(std::iter::repeat(dedentation).take(amount_of_spaces / INDENTATION_IN_SPACES));
}

pub struct Lexer {
    // map: &'m mut SourceMap,
    source: Rc<SourceFile>,
    tokens: Vec<SourceToken>,
}

impl Lexer {
    pub fn load(map: &mut SourceMap, path: std::path::PathBuf) -> Result<Self, crate::span::Error> {
        Ok(Self {
            source: map.load(path)?,
            // map,
            tokens: Vec::new(),
        })
    }

    // @Note bad naming scheme
    pub fn lex(&mut self) -> Result<(), Diagnostic> {
        // @Temporary
        lex(self)
    }

    // @Note the whole Lexer business is bad API-design right now
    pub fn into_tokens(self) -> Vec<SourceToken> {
        self.tokens
    }
}

/// Lex source code into an array of lexer.tokens.
// @Task keep a bracket stack to report better error messages
// @Task refactor this monstrosity of a function into smaller parts! cognitive complexity = 32/25
// @Task move this function (but simplify it first)
fn lex(lexer: &mut Lexer) -> Result<(), Diagnostic> {
    let mut characters = lexer.source.content().chars().peekable();
    let mut index = LocalByteIndex::new(0);
    // @Temporary make it a method, can not even be a closure because of borrowing issues (compound issue)
    // hence it is macro
    macro advance() {
        characters.next();
        index = index + 1;
    };
    let mut indentation_in_spaces = 0;

    // @Bug sometimes we get consecutive dedent&indent, they should be flattened/filtered out
    while let Some(&character) = characters.peek() {
        if character == WHITESPACE {
            advance!();
            while let Some(&character) = characters.peek() {
                if character == WHITESPACE {
                    advance!();
                } else {
                    break;
                }
            }
        } else if character == ';' {
            let mut span = LocalSpan::from(index);
            let mut documentation = true;

            advance!();

            if let Some(';') = characters.peek() {
                documentation = false;
                span.end = index;
                advance!();
            }

            while let Some(&character) = characters.peek() {
                advance!();

                if character != '\n' {
                    if documentation {
                        span.end = index;
                    }
                } else {
                    break;
                }
            }

            if documentation {
                lexer.tokens.push(SourceToken::new(
                    Token::DocumentationComment,
                    Span::from_local(lexer.source.as_ref(), span),
                ))
            }
        } else if is_identifier_head(character) {
            let mut span = LocalSpan::from(index);

            while let Some(&character) = characters.peek() {
                if is_identifier_body(character) {
                    span.end = index;
                    advance!();
                } else {
                    if is_identifier_tail(character) {
                        while let Some(&character) = characters.peek() {
                            if is_identifier_tail(character) {
                                span.end = index;
                                advance!();
                            } else {
                                break;
                            }
                        }
                    }
                    break;
                }
            }

            lexer.tokens.push(SourceToken::new(
                match parse_keyword(&lexer.source[span]) {
                    Some(keyword) => keyword,
                    None => Token::Identifier(Atom::from(&lexer.source[span])),
                },
                Span::from_local(lexer.source.as_ref(), span),
            ));
        } else if character == '\n' {
            lexer.tokens.push(SourceToken::new(
                Token::LineBreak,
                Span::from_local(lexer.source.as_ref(), LocalSpan::from(index)),
            ));

            advance!();

            let mut spaces = 0;
            let mut span = LocalSpan::from(index);

            while let Some(&character) = characters.peek() {
                if character == WHITESPACE {
                    span.end = index;
                    spaces += 1;
                    advance!();
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

            let global_span = Span::from_local(
                lexer.source.as_ref(),
                LocalSpan::new(span.end - absolute_difference + 1, span.end),
            );

            if absolute_difference % INDENTATION_IN_SPACES != 0
                || change == Ordering::Greater && absolute_difference > INDENTATION_IN_SPACES
            {
                return Err(Diagnostic::fatal(
                    format!(
                        "invalid indentation consisting of {} spaces",
                        absolute_difference
                    ),
                    global_span,
                ));
            }

            match change {
                Ordering::Greater => lexer
                    .tokens
                    .push(SourceToken::new(Token::Indentation, global_span)),
                Ordering::Less => extend_with_dedentation(
                    lexer.source.as_ref(),
                    &mut lexer.tokens,
                    span.start,
                    absolute_difference,
                ),
                Ordering::Equal => unreachable!(),
            }

            indentation_in_spaces = spaces;
        } else if is_punctuation(character) {
            let mut span = LocalSpan::from(index);
            advance!();

            while let Some(&character) = characters.peek() {
                if is_punctuation(character) {
                    span.end = index;
                    advance!();
                } else {
                    break;
                }
            }

            lexer.tokens.push(SourceToken::new(
                parse_reserved_punctuation(&lexer.source[span]).unwrap_or(Token::Punctuation),
                Span::from_local(lexer.source.as_ref(), span),
            ))
        } else if character.is_ascii_digit() {
            let mut span = LocalSpan::from(index);
            advance!();

            while let Some(&character) = characters.peek() {
                if character.is_ascii_digit() {
                    span.end = index;
                    advance!();
                } else {
                    break;
                }
            }

            // @Task verify that it ends with a space instead of any non-digit

            lexer.tokens.push(SourceToken::new(
                Token::NatLiteral(Nat::from_str(&lexer.source[span]).unwrap()),
                Span::from_local(lexer.source.as_ref(), span),
            ));
        } else if character == '"' {
            let mut span = LocalSpan::from(index);
            let mut terminated = false;
            advance!();

            while let Some(&character) = characters.peek() {
                // @Task move this into while let part
                // @Task escaping
                advance!();
                span.end = span.end + character.len_utf8();

                if character != '"' {
                } else {
                    terminated = true;
                    break;
                }
            }

            let global_span = Span::from_local(lexer.source.as_ref(), span);

            if !terminated {
                return Err(Diagnostic::fatal(
                    "unterminated text literal".to_owned(),
                    global_span,
                ));
            }

            lexer.tokens.push(SourceToken::new(
                // @Note once we implement escaping, this won't cut it and we need to build our own string
                Token::TextLiteral(
                    lexer.source[LocalSpan::new(span.start + 1, span.end - 1)].to_owned(),
                ),
                global_span,
            ))
        } else {
            let global_span = Span::from_local(lexer.source.as_ref(), LocalSpan::from(index));

            lexer.tokens.push(SourceToken::new(
                match character {
                    '(' => Token::OpeningRoundBracket,
                    ')' => Token::ClosingRoundBracket,
                    _ => {
                        return Err(Diagnostic::fatal(
                            format!(
                                "illegal character U+{:04X} `{}`",
                                character as u32, character
                            ),
                            global_span,
                        ))
                    }
                },
                global_span,
            ));

            advance!();
        }
    }

    let last_index = LocalByteIndex::from_usize(lexer.tokens.len() - 1);
    extend_with_dedentation(
        lexer.source.as_ref(),
        &mut lexer.tokens,
        last_index,
        indentation_in_spaces,
    );
    // @Question maybe also add a artificial line break to simplify parsing? @Bug bad span
    lexer
        .tokens
        .push(SourceToken::new(Token::EndOfInput, Span::dummy()));
    Ok(())
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
