//! The high-level intermediate representation.
//!
//! Parameterized by the type of binder.

mod fmt;

use crate::{
    parser::{Attributes, Explicitness},
    span::{SourceFile, Span, Spanned},
};
use freestanding::freestanding;
use std::{fmt::Display, rc::Rc};

// @Note the marker clone should ideally not be there (it's a marker type) but rustc's derive(Clone)
// is too stupid
pub trait Pass: Clone {
    /// "l-value"/place
    type Binder: Display + Clone;
    /// "r-value"
    type ReferencedBinder: Display + Clone;

    // @Temporary
    type ReferencedBinderInUse: Display = Self::ReferencedBinder;

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

#[freestanding]
#[streamline(Box)]
pub enum DeclarationKind<P: Pass> {
    Value {
        binder: P::Binder,
        type_annotation: Expression<P>,
        expression: Option<Expression<P>>,
    },
    Data {
        binder: P::Binder,
        type_annotation: Expression<P>,
        constructors: Option<Vec<Declaration<P>>>,
    },
    Constructor {
        binder: P::Binder,
        type_annotation: Expression<P>,
    },
    Module {
        binder: P::Binder,
        file: Rc<SourceFile>,
        declarations: Vec<Declaration<P>>,
    },
    Use {
        binder: Option<P::Binder>,
        reference: P::ReferencedBinderInUse,
    },
}

pub type Expression<P> = Spanned<ExpressionKind<P>>;

impl<P: Pass> Expression<P> {
    // @Question by value or by reference?
    pub fn binding(&self) -> Option<&Binding<P>> {
        match &self.kind {
            ExpressionKind::Binding(binding) => Some(binding),
            _ => None,
        }
    }
}

#[freestanding]
#[streamline(Rc)]
#[derive(Clone)]
pub enum ExpressionKind<P: Pass> {
    PiType {
        parameter: Option<P::Binder>,
        domain: Expression<P>,
        codomain: Expression<P>,
        explicitness: Explicitness,
    },
    Application {
        callee: Expression<P>,
        argument: Expression<P>,
        explicitness: Explicitness,
    },
    Type,
    #[parameterless]
    Nat {
        value: crate::Nat,
    },
    #[parameterless]
    Text {
        value: String,
    },
    Binding {
        binder: P::ReferencedBinder,
    },
    Lambda {
        parameter: P::Binder,
        parameter_type_annotation: Option<Expression<P>>,
        explicitness: Explicitness,
        body_type_annotation: Option<Expression<P>>,
        body: Expression<P>,
    },
    UseIn,
    CaseAnalysis {
        subject: Expression<P>,
        cases: Vec<Case<P>>,
    },
    // @Task move??? this only exists in typer,interpreter
    Substitution {
        substitution: crate::interpreter::Substitution,
        expression: Expression<P>,
    },
    // @Task move??? typer,interpreter
    ForeignApplication {
        callee: P::ForeignApplicationBinder,
        arguments: Vec<Expression<P>>,
    },
}

#[derive(Clone)]
pub struct Case<P: Pass> {
    pub pattern: Pattern<P>,
    pub body: Expression<P>,
}

pub type Pattern<P> = Spanned<PatternKind<P>>;

#[freestanding]
#[streamline(Rc)]
#[derive(Clone)]
// @Note naming of variants in unfortunate (necessary because of freestanding
// and bc we don't use submodules here by design)
pub enum PatternKind<P: Pass> {
    #[parameterless]
    NatPattern { value: crate::Nat },
    BindingPattern {
        binder: P::PatternBinder,
        type_annotation: Option<Expression<P>>,
    },
    ApplicationPattern {
        callee: Pattern<P>,
        argument: Pattern<P>,
    },
}

pub macro decl($kind:ident[$span:expr][$attrs:expr] { $( $body:tt )+ }) {{
    let span = $span;
    let attributes = $attrs;

    Declaration {
        kind: DeclarationKind::$kind(Box::new(self::$kind { $( $body )+ })),
        span,
        attributes,
    }
}}

pub macro expr {
    ($kind:ident[$( $span:expr )?] { $( $body:tt )+ }) => {{
        let span = span!($( $span )?);
        Expression::new(
            ExpressionKind::$kind(Rc::new(self::$kind { $( $body )+ })),
            span,
        )
    }},
    ($kind:ident[$( $span:expr )?]) => {{
        let span = span!($( $span )?);
        Expression::new(ExpressionKind::$kind, span)
    }}
}

macro span {
    () => { Span::DUMMY },
    ($span:expr) => { $span },
}

pub macro pat($kind:ident[$span:expr] { $( $body:tt )+ }) {{
    let span = $span;
    Pattern::new(
        PatternKind::$kind(Rc::new(self::$kind { $( $body )+ })),
        span,
    )
}}
