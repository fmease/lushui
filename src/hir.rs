//! The high-level intermediate representation.
//!
//! Parameterized by the type of binder.

mod fmt;

use crate::{
    lexer::Number,
    parser::{Attributes, Explicitness},
    span::{SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
};
use std::{fmt::Display, rc::Rc};

// @Note the marker clone should ideally not be there (it's a marker type) but rustc's derive(Clone)
// is too stupid
pub trait Pass: Clone {
    /// "l-value"/place
    type Binder: Display + Clone;
    /// "r-value"
    type ReferencedBinder: Display + Clone;

    // @Temporary
    /// Use target
    type Target: Display = Self::ReferencedBinder;

    /// "l-value" in patterns
    type PatternBinder: Display + Clone;
    /// "r-value" in foreign applications
    type ForeignApplicationBinder: Display + Clone;
}

pub struct Declaration<P: Pass> {
    pub kind: DeclarationKind<P>,
    pub span: Span,
    pub attributes: Attributes,
}

impl<P: Pass> Declaration<P> {
    pub fn unwrap_constructor(&self) -> &Constructor<P> {
        match &self.kind {
            DeclarationKind::Constructor(constructor) => constructor,
            _ => unreachable!(),
        }
    }
}

impl<P: Pass> Spanning for Declaration<P> {
    fn span(&self) -> Span {
        self.span
    }
}

impl<P: Pass> InvalidFallback for Declaration<P> {
    fn invalid() -> Self {
        decl! {
            Invalid[][Default::default()]
        }
    }
}

pub enum DeclarationKind<P: Pass> {
    Value(Box<Value<P>>),
    Data(Box<Data<P>>),
    Constructor(Box<Constructor<P>>),
    Module(Box<Module<P>>),
    Use(Box<Use<P>>),
    Invalid,
}

pub struct Value<P: Pass> {
    pub binder: P::Binder,
    pub type_annotation: Expression<P>,
    pub expression: Option<Expression<P>>,
}

pub struct Data<P: Pass> {
    pub binder: P::Binder,
    pub type_annotation: Expression<P>,
    pub constructors: Option<Vec<Declaration<P>>>,
}

pub struct Constructor<P: Pass> {
    pub binder: P::Binder,
    pub type_annotation: Expression<P>,
}

pub struct Module<P: Pass> {
    pub binder: P::Binder,
    pub file: Rc<SourceFile>,
    pub declarations: Vec<Declaration<P>>,
}

pub struct Use<P: Pass> {
    pub binder: Option<P::Binder>,
    pub target: P::Target,
}

pub type Expression<P> = Spanned<ExpressionKind<P>>;

#[derive(Clone)]
pub enum ExpressionKind<P: Pass> {
    PiType(Rc<PiType<P>>),
    Application(Rc<Application<P>>),
    Type,
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding<P>>),
    Lambda(Rc<Lambda<P>>),
    UseIn,
    CaseAnalysis(Rc<CaseAnalysis<P>>),
    // @Task move this the documentation below somewhere else (module-level documentation)
    // because this applies to so many types
    /// A sham node emitted when a compiler pass runs into an error state.
    ///
    /// This value is necessary to be able to emit multiple seemingly fatal errors
    /// instead of bailing out early. An HIR value containing an invalid node is
    /// considered invalid itself. Invalid nodes (implicitly) poison the tree.
    /// This means that the pass using an invalid node needs to keep track whether
    /// the tree erroneous. For example by using a `Bag<Diagnostic>` and "emitting"
    /// all collected errors at once at some defined point. "Emitting" here means
    /// returning an `Err` such that overall, the pass/function offically fails and
    /// no invalid nodes ever reach the next pass.
    /// Invalid nodes should be considered local to a pass (the definition of a pass
    /// is left open).
    ///
    /// The alternative design to invalid nodes is more passes (and maybe more intermediate
    /// representations where each successive one contains more information).
    /// This is inconvenient even if we were able to parameterize an existing HIR
    /// instead of defining a completely separate one.
    /// And also maybe, it's more performant than traversing an IR too often (although
    /// micro pass compilers are known to be very fast actually).
    Invalid,
    // @Task move??? this only exists in typer,interpreter
    Substitution(Rc<Substitution<P>>),
    // @Task move??? typer,interpreter
    ForeignApplication(Rc<ForeignApplication<P>>),
}

#[derive(Clone)]
pub struct PiType<P: Pass> {
    pub parameter: Option<P::Binder>,
    pub domain: Expression<P>,
    pub codomain: Expression<P>,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Application<P: Pass> {
    pub callee: Expression<P>,
    pub argument: Expression<P>,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Binding<P: Pass> {
    pub binder: P::ReferencedBinder,
}

#[derive(Clone)]
pub struct Lambda<P: Pass> {
    pub parameter: P::Binder,
    pub parameter_type_annotation: Option<Expression<P>>,
    pub explicitness: Explicitness,
    pub body_type_annotation: Option<Expression<P>>,
    pub body: Expression<P>,
}

#[derive(Clone)]
pub struct CaseAnalysis<P: Pass> {
    pub subject: Expression<P>,
    pub cases: Vec<Case<P>>,
}

#[derive(Clone)]
// @Task move??? this only exists in typer,interpreter
pub struct Substitution<P: Pass> {
    pub substitution: crate::interpreter::Substitution,
    pub expression: Expression<P>,
}

#[derive(Clone)]
// @Task move??? typer,interpreter
pub struct ForeignApplication<P: Pass> {
    pub callee: P::ForeignApplicationBinder,
    pub arguments: Vec<Expression<P>>,
}

impl<P: Pass> InvalidFallback for Expression<P> {
    fn invalid() -> Self {
        expr! { Invalid[] }
    }
}

#[derive(Clone)]
pub struct Case<P: Pass> {
    pub pattern: Pattern<P>,
    pub body: Expression<P>,
}

pub type Pattern<P> = Spanned<PatternKind<P>>;

#[derive(Clone)]
pub enum PatternKind<P: Pass> {
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding<P>>),
    Binder(Rc<Binder<P>>),
    Deapplication(Rc<Deapplication<P>>),
}

/// A binder inside of a pattern.
#[derive(Clone)]
pub struct Binder<P: Pass> {
    pub binder: P::Binder,
}

#[derive(Clone)]
pub struct Deapplication<P: Pass> {
    pub callee: Pattern<P>,
    pub argument: Pattern<P>,
}

pub macro decl {
    ($kind:ident[$( $span:expr )?][$attrs:expr] { $( $body:tt )+ }) => {
        Declaration {
            span: span!($( $span )?),
            attributes: $attrs,
            kind: DeclarationKind::$kind(Box::new(self::$kind { $( $body )+ })),
        }
    },
    ($kind:ident[$( $span:expr )?][$attrs:expr]) => {
        Declaration {
            span: span!($( $span )?),
            attributes: $attrs,
            kind: DeclarationKind::$kind,
        }
    }

}

pub macro expr {
    ($kind:ident[$( $span:expr )?] { $( $body:tt )+ }) => {
        Expression::new(
            span!($( $span )?),
            ExpressionKind::$kind(Rc::new(self::$kind { $( $body )+ })),
        )
    },
    ($kind:ident[$( $span:expr )?]($value:expr)) => {
        Expression::new(
            span!($( $span )?),
            ExpressionKind::$kind(Rc::from($value)),
        )
    },
    ($kind:ident[$( $span:expr )?]) => {
        Expression::new(span!($( $span )?), ExpressionKind::$kind)
    }
}

macro span {
    () => { Span::SHAM },
    ($span:expr) => { $span },
}

pub macro pat {
    ($kind:ident[$( $span:expr )?] { $( $body:tt )+ }) => {
        Pattern::new(
            span!($( $span )?),
            PatternKind::$kind(Rc::new(self::$kind { $( $body )+ })),
        )
    },
    ($kind:ident[$( $span:expr )?]($value:expr)) => {
        Pattern::new(
            span!($( $span )?),
            PatternKind::$kind(Rc::from($value)),
        )
    },
}
