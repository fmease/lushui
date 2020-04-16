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

// @Note this trait is not well designed
pub trait Binder: Display + Clone {
    type Simple: Display;
    type Pattern: Display;
}

pub struct Declaration<B: Binder> {
    pub kind: DeclarationKind<B>,
    pub span: Span,
    pub attributes: Attributes,
}

impl<B: Binder> Declaration<B> {
    pub fn unwrap_constructor(&self) -> &Constructor<B> {
        match &self.kind {
            DeclarationKind::Constructor(constructor) => constructor,
            _ => unreachable!(),
        }
    }
}

#[freestanding]
#[streamline(Box)]
pub enum DeclarationKind<B: Binder> {
    Value {
        binder: B::Simple,
        type_annotation: Expression<B>,
        expression: Option<Expression<B>>,
    },
    Data {
        binder: B::Simple,
        type_annotation: Expression<B>,
        constructors: Option<Vec<Declaration<B>>>,
    },
    Constructor {
        binder: B::Simple,
        type_annotation: Expression<B>,
    },
    Module {
        binder: B::Simple,
        file: Rc<SourceFile>,
        declarations: Option<Vec<Declaration<B>>>,
    },
    Use,
}

pub type Expression<B> = Spanned<ExpressionKind<B>>;

impl<B: Binder> Expression<B> {
    // @Question by value or by reference?
    pub fn binding(&self) -> Option<&Binding<B>> {
        match &self.kind {
            ExpressionKind::Binding(binding) => Some(binding),
            _ => None,
        }
    }
}

#[freestanding]
#[streamline(Rc)]
#[derive(Clone)]
pub enum ExpressionKind<B: Binder> {
    PiType {
        parameter: Option<B::Simple>,
        domain: Expression<B>,
        codomain: Expression<B>,
        explicitness: Explicitness,
    },
    Application {
        callee: Expression<B>,
        argument: Expression<B>,
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
        binder: B,
    },
    Lambda {
        parameter: B::Simple,
        parameter_type_annotation: Option<Expression<B>>,
        explicitness: Explicitness,
        body_type_annotation: Option<Expression<B>>,
        body: Expression<B>,
    },
    UseIn,
    CaseAnalysis {
        subject: Expression<B>,
        cases: Vec<Case<B>>,
    },
    // @Task move???
    Substitution {
        substitution: crate::interpreter::Substitution,
        expression: Expression<B>,
    },
    ForeignApplication {
        callee: B::Simple,
        arguments: Vec<Expression<B>>,
    },
}

#[derive(Clone)]
pub struct Case<B: Binder> {
    pub pattern: Pattern<B>,
    pub body: Expression<B>,
}

pub type Pattern<B> = Spanned<PatternKind<B>>;

#[freestanding]
#[streamline(Rc)]
#[derive(Clone)]
// @Note naming of variants in unfortunate (necessary because of freestanding
// and bc we don't use submodules here by design)
pub enum PatternKind<B: Binder> {
    #[parameterless]
    NatPattern { value: crate::Nat },
    BindingPattern {
        binder: B::Pattern,
        type_annotation: Option<Expression<B>>,
    },
    ApplicationPattern {
        callee: Pattern<B>,
        argument: Pattern<B>,
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
