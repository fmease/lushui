//! A collection of *pattern synonyms*.
//!
//! Contrary to methods or arrays, they preserve `match` exhaustiveness & overlap checks at use sites.

use lexer::token::BareToken::*;

/// An [identifier].
///
/// [identifier]: ast::Ident
pub(crate) macro Ident($ident:pat) {
    Word($ident) | Symbol($ident)
}

/// The prefix of an [attribute].
///
/// [attribute]: ast::Attr
pub(crate) macro AttrPrefix() {
    At | DocComment
}

/// The prefix of a lower expression.
pub(crate) macro LowerExprPrefix() {
    //
    // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_expression`.
    //

    AttrPrefix!()
        | WildcardPrefix!()
        | NumLit(_)
        | TextLit(_)
        | ForUpper
        | ForLower
        | Case
        | Do
        | Let
        | Use
        | Trait
        | OpeningRoundBracket
        | OpeningSquareBracket
        | OpeningCurlyBracket
        | PathHead!()
}

/// The prefix of a lower pattern.
pub(crate) macro LowerPatPrefix() {
    //
    // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_pattern`.
    //

    AttrPrefix!()
        | WildcardPrefix!()
        | NumLit(_)
        | TextLit(_)
        | OpeningRoundBracket
        | OpeningSquareBracket
        | OpeningCurlyBracket
        | PathHead!()
}

pub(crate) macro WildcardPrefix() {
    Underscore | QuestionMark
}

/// The head (i.e. prefix) of a [path].
///
/// [path]: ast::Path
pub(crate) macro PathHead() {
    Ident!(_) | PathHanger!()
}

/// The [hanger] of a [path].
///
/// [hanger]: ast::Hanger
/// [path]: ast::Path
pub(crate) macro PathHanger() {
    Extern | Topmost | Super | Self_
}

/// A declaration terminator.
pub(crate) macro Terminator() {
    LineBreak | Dedentation | EndOfInput
}
