use crate::{Expr, Ident, Item, Params, Path};
use span::{PossiblySpanning, Spanned, SrcFileIdx};
use utility::obtain;

/// A declaration.
///
/// The syntactic category of module-level definitions like functions, data types and modules.
pub type Decl = Item<BareDecl>;

/// A location-less declaration.
#[derive(PartialEq, Eq)]
pub enum BareDecl {
    Func(Box<Func>),
    DataTy(Box<DataTy>),
    Module(Box<Module>),
    ModuleHeader,
    Use(Box<Use>),
    Given(Box<Given>),
}

impl From<Func> for BareDecl {
    fn from(func: Func) -> Self {
        Self::Func(Box::new(func))
    }
}

impl From<DataTy> for BareDecl {
    fn from(ty: DataTy) -> Self {
        Self::DataTy(Box::new(ty))
    }
}

impl From<Module> for BareDecl {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

impl From<Use> for BareDecl {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

impl From<Given> for BareDecl {
    fn from(given: Given) -> Self {
        Self::Given(Box::new(given))
    }
}

/// A function declaration.
///
/// This includes constructors and record & trait fields.
///
/// # Examples
///
/// ```lushui
/// identity 'A (a: A): A = a
/// ```
///
/// * `identity` is the *binder*
/// * `'A` and `(a: A)` are the *parameters*
/// * `A` following the colon is the *type*
/// * `a` at the end is the *body*
// @Task update docs
#[derive(Clone, PartialEq, Eq)]
pub struct Func {
    pub binder: Ident,
    pub params: Params,
    pub ty: Option<Expr>,
    pub body: Option<Expr>,
}

/// A data type declaration.
///
/// # Examples
///
/// ```lushui
/// data Result (A: Type) (E: Type): Type of
///     failure 'A 'E: E -> Result A E
///     success 'A 'E: A -> Result A E
/// ```
///
/// * `Result` is the *binder*
/// * `(A: Type)` and `(E: Type)` are the *parameters*
/// * `Type` at the end of the first line is the *type*
/// * the last two lines contain the *constructors*
// @Task update docs
#[derive(PartialEq, Eq)]
pub struct DataTy {
    pub kind: DataKind,
    pub binder: Ident,
    pub params: Params,
    pub ty: Option<Expr>,
    pub decls: Option<Vec<Decl>>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum DataKind {
    Data,
    Record,
    Trait,
}

/// A module declaration.
///
/// # Examples
///
/// `file.lushui`:
///
/// ```lushui
/// module inner of
///     use extern.core.type.Type
/// ```
///
/// * `inner` is the *binder*
/// * the index of `file.lushui` is the *file* (index)
/// * the last line contains the only *declaration* (or subdeclaration)
#[derive(PartialEq, Eq)]
pub struct Module {
    pub binder: Ident,
    pub file: SrcFileIdx,
    pub decls: Option<Vec<Decl>>,
}

impl TryFrom<BareDecl> for Module {
    type Error = ();

    fn try_from(decl: BareDecl) -> Result<Self, Self::Error> {
        obtain!(decl, BareDecl::Module(module) => *module).ok_or(())
    }
}

/// A use-declaration or a use-statement.
///
/// See [`BareDeclaration::Use`] and [`crate::Statement::Use`].
#[derive(Clone, PartialEq, Eq)]
pub struct Use {
    pub bindings: UsePathTree,
}

pub type UsePathTree = Spanned<BareUsePathTree>;

#[derive(Clone, PartialEq, Eq)]
pub enum BareUsePathTree {
    Single {
        target: Path,
        binder: Option<Ident>,
    },
    Multiple {
        path: Path,
        subpaths: Vec<UsePathTree>,
    },
}

/*
@Task write docs
/// A trait literal.
///
/// # Examples
///
/// ```lushui
/// main = trait of
///     empty = 0
///     append (x: Nat) (y: Nat) = + x y
/// ```
///
/// * `empty = 0` is a *field*
/// * `append (x: Nat) (y: Nat) = + x y` is a *field*
#[derive(Clone, PartialEq, Eq)]
pub struct TraitLiteral {
    pub fields: Vec<Item<crate::Function>>,
}
*/
#[derive(PartialEq, Eq)]
pub struct Given {
    pub binder: Ident,
    pub params: Params,
    pub ty: Option<Expr>,
    pub body: Option<Body>,
}

#[derive(PartialEq, Eq)]
pub enum Body {
    Block { fields: Vec<Decl> },
    Expr { body: Expr },
}

impl PossiblySpanning for Body {
    fn possible_span(&self) -> Option<span::Span> {
        match self {
            Self::Block { fields } => fields.possible_span(),
            Self::Expr { body } => Some(body.span),
        }
    }
}
