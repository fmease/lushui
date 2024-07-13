use crate::{Attrs, Ident, ParamKind, Path};
use span::Spanned;
use utility::Atom;

pub type Item<Bare> = span::item::Item<Bare, Attrs>;

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
        tag: Ident,
    },
}

// @Task docs for all of these!!!

/// A number literal.
#[derive(Clone, PartialEq, Eq)]
pub struct NumLit {
    pub path: Option<Path>,
    pub lit: Spanned<Atom>,
}

/// A text literal.
#[derive(Clone, PartialEq, Eq)]
pub struct TextLit {
    pub path: Option<Path>,
    pub lit: Spanned<Atom>,
}

/// A function application.
#[derive(Clone, PartialEq, Eq)]
pub struct App<T> {
    pub callee: T,
    pub kind: ParamKind,
    pub binder: Option<Ident>,
    pub arg: T,
}

/// A sequence literal.
#[derive(Clone, PartialEq, Eq)]
pub struct SeqLit<T> {
    pub path: Option<Path>,
    pub elems: Spanned<Vec<T>>,
}

/// A record literal.
#[derive(Clone, PartialEq, Eq)]
pub struct RecLit<T> {
    pub path: Option<Path>,
    pub fields: Spanned<Vec<Field<T>>>,
    pub base: Option<T>,
}

// @Task smh. represent `::=` in here! it should only work for expressions, right??

#[derive(Clone, PartialEq, Eq)]
pub struct Field<T> {
    // @Task generalize this to some new kind of path that is delimited by "::" OR "."
    pub binder: Ident,
    pub body: Option<T>,
}
