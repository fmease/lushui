use crate::{Expression, Identifier, Item, Parameters, Path};
use span::{PossiblySpanning, SourceFileIndex, Spanned};
use utility::obtain;

/// A declaration.
///
/// The syntactic category of module-level definitions like functions, data types and modules.
pub type Declaration = Item<BareDeclaration>;

/// A declaration without an enclosing [`span::Span`].
#[derive(PartialEq, Eq)]
pub enum BareDeclaration {
    Function(Box<Function>),
    Data(Box<Data>),
    Module(Box<Module>),
    ModuleHeader,
    Use(Box<Use>),
    Given(Box<Given>),
}

impl From<Function> for BareDeclaration {
    fn from(function: Function) -> Self {
        Self::Function(Box::new(function))
    }
}

impl From<Data> for BareDeclaration {
    fn from(type_: Data) -> Self {
        Self::Data(Box::new(type_))
    }
}

impl From<Module> for BareDeclaration {
    fn from(module: Module) -> Self {
        Self::Module(Box::new(module))
    }
}

impl From<Use> for BareDeclaration {
    fn from(use_: Use) -> Self {
        Self::Use(Box::new(use_))
    }
}

impl From<Given> for BareDeclaration {
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
pub struct Function {
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<Expression>,
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
pub struct Data {
    pub kind: DataKind,
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub declarations: Option<Vec<Declaration>>,
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
    pub binder: Identifier,
    pub file: SourceFileIndex,
    pub declarations: Option<Vec<Declaration>>,
}

impl TryFrom<BareDeclaration> for Module {
    type Error = ();

    fn try_from(declaration: BareDeclaration) -> Result<Self, Self::Error> {
        obtain!(declaration, BareDeclaration::Module(module) => *module).ok_or(())
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
    Single { target: Path, binder: Option<Identifier> },
    Multiple { path: Path, subpaths: Vec<UsePathTree> },
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
    pub binder: Identifier,
    pub parameters: Parameters,
    pub type_: Option<Expression>,
    pub body: Option<Body>,
}

#[derive(PartialEq, Eq)]
pub enum Body {
    Block { fields: Vec<Declaration> },
    Expression { body: Expression },
}

impl PossiblySpanning for Body {
    fn possible_span(&self) -> Option<span::Span> {
        match self {
            Self::Block { fields } => fields.possible_span(),
            Self::Expression { body } => Some(body.span),
        }
    }
}
