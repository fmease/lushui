//! A collection of *pattern synonyms*.
//!
//! Contrary to methods or arrays, they preserve `match` exhaustiveness & overlap checks at use sites.

use lexer::token::BareToken::*;

/// An [identifier]
///
/// [identifier]: ast::Identifier
pub(crate) macro Identifier($identifier:pat) {
    Word($identifier) | Symbol($identifier)
}

/// The prefix of an [attribute].
///
/// [attribute]: ast::Attribute
pub(crate) macro AttributePrefix() {
    At | DocComment
}

/// The prefix of a lower expression.
pub(crate) macro LowerExpressionPrefix() {
    //
    // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_expression`.
    //

    AttributePrefix!()
        | WildcardPrefix!()
        | NumberLiteral(_)
        | TextLiteral(_)
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
pub(crate) macro LowerPatternPrefix() {
    //
    // IMPORTANT: To be kept in sync with `crate::Parser::parse_lower_pattern`.
    //

    AttributePrefix!()
        | WildcardPrefix!()
        | NumberLiteral(_)
        | TextLiteral(_)
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
    Identifier!(_) | PathHanger!()
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
