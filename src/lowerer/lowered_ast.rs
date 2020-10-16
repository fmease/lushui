//! The lowered AST.

use crate::{
    ast::{Attributes, Explicitness, Identifier, Path},
    lexer::Number,
    span::{SourceFile, Span, Spanned, Spanning},
    support::InvalidFallback,
};
use std::{fmt, rc::Rc};

pub struct Declaration {
    pub kind: DeclarationKind,
    pub span: Span,
    pub attributes: Attributes,
}

impl Declaration {
    pub fn unwrap_constructor(&self) -> &Constructor {
        match &self.kind {
            DeclarationKind::Constructor(constructor) => constructor,
            _ => unreachable!(),
        }
    }
}

impl Spanning for Declaration {
    fn span(&self) -> Span {
        self.span
    }
}

impl InvalidFallback for Declaration {
    fn invalid() -> Self {
        decl! {
            Invalid[][Default::default()]
        }
    }
}

pub enum DeclarationKind {
    Value(Box<Value>),
    Data(Box<Data>),
    Constructor(Box<Constructor>),
    Module(Box<Module>),
    Use(Box<Use>),
    Invalid,
}

pub struct Value {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub expression: Option<Expression>,
}

pub struct Data {
    pub binder: Identifier,
    pub type_annotation: Expression,
    pub constructors: Option<Vec<Declaration>>,
}

pub struct Constructor {
    pub binder: Identifier,
    pub type_annotation: Expression,
}

pub struct Module {
    pub binder: Identifier,
    pub file: Rc<SourceFile>,
    pub declarations: Vec<Declaration>,
}

pub struct Use {
    pub binder: Option<Identifier>,
    pub target: Path,
}

pub type Expression = Spanned<ExpressionKind>;

impl InvalidFallback for Expression {
    fn invalid() -> Self {
        expr! { Invalid[] }
    }
}

#[derive(Clone)]
pub enum ExpressionKind {
    PiType(Rc<PiType>),
    Application(Rc<Application>),
    Type,
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Lambda(Rc<Lambda>),
    UseIn,
    CaseAnalysis(Rc<CaseAnalysis>),
    Invalid,
}

#[derive(Clone)]
pub struct PiType {
    pub parameter: Option<Identifier>,
    pub domain: Expression,
    pub codomain: Expression,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Application {
    pub callee: Expression,
    pub argument: Expression,
    pub explicitness: Explicitness,
}

#[derive(Clone)]
pub struct Binding {
    pub binder: Path,
}

#[derive(Clone)]
pub struct Lambda {
    pub parameter: Identifier,
    pub parameter_type_annotation: Option<Expression>,
    pub explicitness: Explicitness,
    pub body_type_annotation: Option<Expression>,
    pub body: Expression,
}

#[derive(Clone)]
pub struct CaseAnalysis {
    pub subject: Expression,
    pub cases: Vec<Case>,
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Spanned<PatternKind>;

impl InvalidFallback for Pattern {
    fn invalid() -> Self {
        pat! { Invalid[] }
    }
}

#[derive(Clone)]
pub enum PatternKind {
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Binder(Rc<Binder>),
    Deapplication(Rc<Deapplication>),
    Invalid,
}

/// A binder inside of a pattern.
#[derive(Clone)]
pub struct Binder {
    pub binder: Identifier,
}

#[derive(Clone)]
pub struct Deapplication {
    pub callee: Pattern,
    pub argument: Pattern,
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(&self, depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use crate::INDENTATION_IN_SPACES;
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                write!(f, "{}: {}", declaration.binder, declaration.type_annotation)?;
                if let Some(expression) = &declaration.expression {
                    write!(f, " = {}", expression)?;
                }
                writeln!(f)
            }
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder, declaration.type_annotation
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION_IN_SPACES),
                            constructor
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder, declaration.type_annotation
                ),
            },
            Constructor(constructor) => {
                writeln!(f, "{}: {}", constructor.binder, constructor.type_annotation)
            }
            Module(declaration) => {
                writeln!(f, "module {}: =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(depth, f)?;
                }
                Ok(())
            }
            Use(declaration) => match &declaration.binder {
                Some(binder) => writeln!(f, "use {} as {}", declaration.target, binder),
                None => writeln!(f, "use {}", declaration.target),
            },
            Invalid => writeln!(f, "<invalid>"),
        }
    }
}

impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(0, f)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal),
            Application(application) => {
                write!(f, "{} ", application.callee.wrap())?;
                if application.explicitness.is_implicit() {
                    write!(f, "({}{})", application.explicitness, application.argument)
                } else {
                    write!(f, "{}", application.argument.wrap())
                }
            }
            Type => write!(f, "Type"),
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal),
            Binding(binding) => write!(f, "{}", binding.binder),
            Lambda(lambda) => write!(f, "{}", lambda),
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                writeln!(f, "case {} of", analysis.subject)?;
                for case in &analysis.cases {
                    write!(f, "{}", case)?;
                }
                Ok(())
            }
            Invalid => write!(f, "<invalid>"),
        }
    }
}

impl fmt::Display for PiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parameter) = &self.parameter {
            write!(f, "({}{}: {})", self.explicitness, parameter, self.domain)?;
        } else if self.explicitness.is_implicit() {
            write!(f, "({}{})", self.explicitness, self.domain)?;
        } else {
            write!(f, "{}", self.domain.wrap())?;
        }
        write!(f, " -> {}", self.codomain.wrap())
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\\({}{}", self.explicitness, self.parameter)?;
        if let Some(annotation) = &self.parameter_type_annotation {
            write!(f, ": {}", annotation.wrap())?;
        }
        write!(f, ")")?;
        if let Some(annotation) = &self.body_type_annotation {
            write!(f, ": {}", annotation.wrap())?;
        }
        write!(f, " => {}", self.body.wrap())
    }
}

impl fmt::Display for Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} => {}", self.pattern, self.body)
    }
}

// @Task update bracket business
impl fmt::Display for Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(f, "{}", binding.binder),
            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
            Invalid => write!(f, "<invalid>"),
        }
    }
}

trait WrapExpression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a>;
}

impl WrapExpression for Expression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a> {
        PossiblyWrapped(self)
    }
}

struct PossiblyWrapped<'a>(&'a Expression);

impl PossiblyWrapped<'_> {
    fn needs_brackets_conservative(&self) -> bool {
        use ExpressionKind::*;

        !matches!(&self.0.kind, Type | Number(_) | Text(_) | Binding(_) | Invalid)
    }
}

impl fmt::Display for PossiblyWrapped<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.needs_brackets_conservative() {
            write!(f, "({})", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}

// @Beacon @Task generate these macros by a macro

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
    ($kind:ident[$( $span:expr )?]) => {
        Pattern::new(span!($( $span )?), PatternKind::$kind)
    }
}
