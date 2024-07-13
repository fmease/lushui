use crate::{Expr, Path};
use lexer::{word::Word, CharExt};
use span::{Span, Spanned, Spanning};
use std::hash::Hash;
use utility::Atom;

/// A name to bind something to, together with its source location.
///
/// A valid name is either
///
/// * a [word](Word) or
/// * a symbol
/// * a keyword or a reserved symbol (*)
///
/// (*) In such case the name does not and must not carry any extra semantic
/// information compared to normal words and symbols. This means that there
/// should never arise the need to explicitly check the underlying textual
/// representation to make a decision that influences the outcome of semantic
/// passes (like lowering, name resolution, type checking, code generation).
/// They should simply be used as names that are unnameable by the user in the
/// surface language and where the thing it names might be referenceable by the
/// compiler through other means.
///
/// There are no hard validity requirements for the span but ideally it should
/// refer to the exact location in the source code where the name was found or
/// where it was mapped from in a synthesis operation. Otherwise, it should
/// probably point to the location which triggered the synthesis if applicable.
/// The default span (signifying no location at all) is of course an option
/// albeit a discouraged one.
#[derive(Clone, Copy, Debug)]
pub struct Ident(Spanned<Atom>);

impl Ident {
    /// Create a new identifier without checking for its validity.
    ///
    /// See [identifier](Self) for the preconditions.
    pub const fn new_unchecked(span: Span, atom: Atom) -> Self {
        Self(Spanned::new(span, atom))
    }

    pub fn bare(self) -> Atom {
        self.0.bare
    }

    pub fn to_str(self) -> &'static str {
        self.0.bare.to_str()
    }

    pub fn into_inner(self) -> Spanned<Atom> {
        self.0
    }

    pub fn respan(mut self, span: Span) -> Self {
        self.0.span = span;
        self
    }

    pub fn is_symbol(self) -> bool {
        // Either all characters are symbols or none are.
        self.to_str().chars().next().unwrap().is_symbol()
    }

    pub fn is_word(self) -> bool {
        // Either all characters are letters or none are.
        !self.is_symbol()
    }
}

impl From<Ident> for Expr {
    fn from(ident: Ident) -> Self {
        Expr::common(ident.span(), Path::from(ident).into())
    }
}

impl From<Spanned<Word>> for Ident {
    fn from(name: Spanned<Word>) -> Self {
        Self::new_unchecked(name.span, name.bare.into_inner())
    }
}

impl TryFrom<Ident> for Spanned<Word> {
    type Error = ();

    fn try_from(ident: Ident) -> Result<Self, Self::Error> {
        ident
            .is_word()
            .then(|| Self::new(ident.span(), Word::new_unchecked(ident.bare())))
            .ok_or(())
    }
}

impl Spanning for Ident {
    fn span(&self) -> Span {
        self.0.span
    }
}

impl span::binder::Binder for Ident {
    fn to_str(self) -> &'static str {
        self.to_str()
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.bare() == other.bare()
    }
}

impl Eq for Ident {}

impl Hash for Ident {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.bare().hash(state);
    }
}
