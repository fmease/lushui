//! The high-level intermediate representation.
//!
//! Parameterized by the type of binder.

use crate::{
    ast::{Attributes, Explicitness},
    lexer::Number,
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

    /// "r-value" in foreign applications
    type ForeignApplicationBinder: Display + Clone;

    /// Representation of a substitution in this pass.
    type Substitution: Clone;

    /// Information necessary to be able to display values of this pass.
    type ShowLinchpin;

    /// How bindings are formatted in this pass.
    fn format_binding(
        binder: &Self::ReferencedBinder,
        linchpin: &Self::ShowLinchpin,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;

    /// How a substitution is formatted in this pass.
    fn format_substitution(
        substitution: &Self::Substitution,
        linchpin: &Self::ShowLinchpin,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result;
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
    pub target: P::ReferencedBinder,
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
    Substitution(Rc<P::Substitution>),
    // @Task move??? typer,interpreter
    ForeignApplication(Rc<ForeignApplication<P>>),
    // @Task store "next: Expression" and prob  arguments: Vec<Expression>
    IO(Rc<IO<P>>),
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
pub struct Substitution<P: Pass> {
    pub substitution: crate::typer::interpreter::Substitution,
    pub expression: Expression<P>,
}

#[derive(Clone)]
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
pub struct IO<P: Pass> {
    pub index: usize, // @Task IOIndex
    pub arguments: Vec<Expression<P>>,
    // @Task next: Option<Expression<P>>
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

use crate::support::DisplayWith;

// @Beacon @Beacon @Task
// @Task but also have the option to pass some further arguments to show
// (type param) that we can use to configure things like whether we should
// print absolute or relative or intelligently relative paths, whether we
// should print crate indices (not relying on global variables)

// @Beacon @Task replace Display implementations with DisplayWith<P::ShowLinchpin> impls being able to
// print absolute paths!!!

impl<P: Pass> DisplayWith for Declaration<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(linchpin, 0, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
// @Task @Beacon display attributes
impl<P: Pass> Declaration<P> {
    fn format_with_depth(
        &self,
        linchpin: &P::ShowLinchpin,
        depth: usize,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use crate::INDENTATION_IN_SPACES;
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => {
                write!(
                    f,
                    "{}: {}",
                    declaration.binder,
                    declaration.type_annotation.with(linchpin)
                )?;
                if let Some(expression) = &declaration.expression {
                    write!(f, " = {}", expression.with(linchpin))?;
                }
                writeln!(f)
            }
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder,
                        declaration.type_annotation.with(linchpin)
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION_IN_SPACES),
                            constructor.with(linchpin)
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder,
                    declaration.type_annotation.with(linchpin)
                ),
            },
            Constructor(constructor) => writeln!(
                f,
                "{}: {}",
                constructor.binder,
                constructor.type_annotation.with(linchpin)
            ),
            Module(declaration) => {
                writeln!(f, "module {}: =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(linchpin, depth, f)?;
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

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl<P: Pass> DisplayWith for Expression<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use joinery::JoinableIterator;
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal.with(linchpin)),
            Application(application) => {
                write!(f, "{} ", application.callee.wrap().with(linchpin))?;
                if application.explicitness.is_implicit() {
                    write!(
                        f,
                        "({}{})",
                        application.explicitness,
                        application.argument.with(linchpin)
                    )
                } else {
                    write!(f, "{}", application.argument.wrap().with(linchpin))
                }
            }
            Type => write!(f, "Type"),
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal),
            Binding(binding) => P::format_binding(&binding.binder, linchpin, f),
            Lambda(lambda) => write!(f, "{}", lambda.with(linchpin)),
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                writeln!(f, "case {} of", analysis.subject.with(linchpin))?;
                for case in &analysis.cases {
                    write!(f, "{}", case.with(linchpin))?;
                }
                Ok(())
            }
            Invalid => write!(f, "<invalid>"),
            Substitution(substitution) => P::format_substitution(substitution, linchpin, f),
            ForeignApplication(application) => write!(
                f,
                "{} {}",
                application.callee,
                application
                    .arguments
                    .iter()
                    .map(|argument| argument.with(linchpin))
                    .join_with(' ')
            ),
            IO(io) => write!(
                f,
                "<io {} {}>",
                io.index,
                io.arguments
                    .iter()
                    .map(|argument| argument.with(linchpin))
                    .join_with(' ')
            ),
        }
    }
}

use std::fmt;

impl<P: Pass> DisplayWith for PiType<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parameter) = &self.parameter {
            write!(
                f,
                "({}{}: {})",
                self.explicitness,
                parameter,
                self.domain.with(linchpin)
            )?;
        } else if self.explicitness.is_implicit() {
            write!(f, "({}{})", self.explicitness, self.domain.with(linchpin))?;
        } else {
            write!(f, "{}", self.domain.wrap().with(linchpin))?;
        }
        write!(f, " -> {}", self.codomain.wrap().with(linchpin))
    }
}

impl<P: Pass> DisplayWith for Lambda<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\\({}{}", self.explicitness, self.parameter)?;
        if let Some(annotation) = &self.parameter_type_annotation {
            write!(f, ": {}", annotation.wrap().with(linchpin))?;
        }
        write!(f, ")")?;
        if let Some(annotation) = &self.body_type_annotation {
            write!(f, ": {}", annotation.wrap().with(linchpin))?;
        }
        write!(f, " => {}", self.body.wrap().with(linchpin))
    }
}

impl<P: Pass> DisplayWith for Case<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} => {}",
            self.pattern.with(linchpin),
            self.body.with(linchpin)
        )
    }
}

// @Task update bracket business
impl<P: Pass> DisplayWith for Pattern<P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => P::format_binding(&binding.binder, linchpin, f),
            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => write!(
                f,
                "({}) ({})",
                application.callee.with(linchpin),
                application.argument.with(linchpin)
            ),
        }
    }
}

trait WrapExpression<P: Pass> {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a, P>;
}

impl<P: Pass> WrapExpression<P> for Expression<P> {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a, P> {
        PossiblyWrapped(self)
    }
}

struct PossiblyWrapped<'a, P: Pass>(&'a Expression<P>);

impl<P: Pass> PossiblyWrapped<'_, P> {
    fn needs_brackets_conservative(&self) -> bool {
        use ExpressionKind::*;

        !matches!(&self.0.kind, Type | Number(_) | Text(_) | Binding(_) | Invalid | Substitution(_))
    }
}

impl<P: Pass> DisplayWith for PossiblyWrapped<'_, P> {
    type Linchpin = P::ShowLinchpin;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.needs_brackets_conservative() {
            write!(f, "({})", self.0.with(linchpin))
        } else {
            write!(f, "{}", self.0.with(linchpin))
        }
    }
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
