//! The high-level intermediate representation.
//!
//! Parameterized by the type of binder.

mod fmt;

use crate::{
    parser::{Attributes, Explicitness},
    span::{Span, Spanned},
};
use freestanding::freestanding;
use std::{collections::VecDeque, rc::Rc};

pub trait Binder: std::fmt::Display + Clone {}

pub struct Declaration<B: Binder> {
    pub kind: DeclarationKind<B>,
    pub span: Span,
    pub attributes: Attributes,
}

impl<B: Binder> Declaration<B> {
    pub fn constructor(&self) -> Option<&Constructor<B>> {
        match &self.kind {
            DeclarationKind::Constructor(constructor) => Some(&constructor),
            _ => None,
        }
    }
}

#[freestanding]
#[streamline(Box)]
pub enum DeclarationKind<B: Binder> {
    Value {
        binder: B,
        type_annotation: Expression<B>,
        expression: Option<Expression<B>>,
    },
    Data {
        binder: B,
        type_annotation: Expression<B>,
        constructors: Option<Vec<Declaration<B>>>,
    },
    Constructor {
        binder: B,
        type_annotation: Expression<B>,
    },
    Module {
        declarations: Vec<Declaration<B>>,
    },
    Use,
}

pub type Expression<B> = Spanned<ExpressionKind<B>>;

#[freestanding]
#[streamline(Rc)]
#[derive(Clone)]
pub enum ExpressionKind<B: Binder> {
    PiType {
        parameter: Option<B>,
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
        parameter: B,
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
    UnsaturatedForeignApplication {
        callee: B,
        arguments: VecDeque<Expression<B>>,
    },
}

#[derive(Clone)]
pub struct Case<B: Binder> {
    pub pattern: Pattern<B>,
    pub body: Expression<B>,
}

// @Task @Beacon @Beacon @Beacon @Beacon transform Pattern like we did in the parser
#[derive(Clone)]
pub enum Pattern<B: Binder> {
    Nat(Nat),
    Binding {
        binder: Binding<B>,
        type_annotation: Option<Expression<B>>,
    },
    Application {
        callee: Rc<Pattern<B>>,
        argument: Rc<Pattern<B>>,
    },
}

pub(crate) macro expr {
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

pub(crate) macro decl($kind:ident[$span:expr][$attrs:expr] { $( $body:tt )+ }) {{
    let span = $span;
    let attributes = $attrs;

    Declaration {
        kind: DeclarationKind::$kind(Box::new(self::$kind { $( $body )+ })),
        span,
        attributes,
    }
}}
