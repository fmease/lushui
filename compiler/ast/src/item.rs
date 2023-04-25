use crate::{Attributes, Identifier, ParameterKind, Path};
use span::Spanned;
use utility::Atom;

pub type Item<Bare> = span::item::Item<Bare, Attributes>;

/// A wildcard.
///
/// # Examples
///
/// ```lushui
/// main = ?content
/// ```
///
/// * `?content` is the *signaling* wildcard
/// * `content` is the *tag*
///
/// ```lushui
/// main =
///     let result: List _ = compute input
///     in result
/// ```
///
/// * `_` is the *quiet* wildcard
#[derive(Clone, PartialEq, Eq)]
pub enum Wildcard {
    /// A silent wildcard.
    Silent,
    /// A signaling wildcard aka (typed) hole.
    Signaling {
        /// The tag of the signaling wildcard.
        ///
        /// It's *tag* not *binder* or *name* to emphasize that
        /// it's not unique inside of a program.
        tag: Identifier,
    },
}

// @Task docs for all of these!!!

#[derive(Clone, PartialEq, Eq)]
pub struct NumberLiteral {
    pub path: Option<Path>,
    pub literal: Spanned<Atom>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct TextLiteral {
    pub path: Option<Path>,
    pub literal: Spanned<Atom>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct Application<T> {
    pub callee: T,
    pub kind: ParameterKind,
    pub binder: Option<Identifier>,
    pub argument: T,
}

#[derive(Clone, PartialEq, Eq)]
pub struct SequenceLiteral<T> {
    pub path: Option<Path>,
    pub elements: Spanned<Vec<T>>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct RecordLiteral<T> {
    pub path: Option<Path>,
    pub fields: Spanned<Vec<Field<T>>>,
    pub base: Option<T>,
}

// @Task smh. represent `::=` in here! it should only work for expressions, right??

#[derive(Clone, PartialEq, Eq)]

pub struct Field<T> {
    // @Task generalize this to some new kind of path that is delimited by "::" OR "."
    pub name: Identifier,
    pub item: Option<T>,
}
