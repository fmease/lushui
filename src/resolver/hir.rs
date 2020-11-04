//! The high-level intermediate representation.

use crate::{
    ast::Explicitness,
    lowered_ast::Item,
    lowered_ast::Number,
    resolver::{CrateScope, FunctionScope, Identifier},
    span::SourceFile,
    support::InvalidFallback,
    typer::interpreter,
};
use std::rc::Rc;

pub type Declaration = Item<DeclarationKind>;

impl Declaration {
    pub fn unwrap_constructor(&self) -> &Constructor {
        match &self.kind {
            DeclarationKind::Constructor(constructor) => constructor,
            _ => unreachable!(),
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

impl InvalidFallback for DeclarationKind {
    fn invalid() -> Self {
        Self::Invalid
    }
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
    pub target: Identifier,
}

pub type Expression = Item<ExpressionKind>;

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
    Substitution(Rc<Substitution>),
    ForeignApplication(Rc<ForeignApplication>),
    IO(Rc<IO>),
    Invalid,
}

impl InvalidFallback for ExpressionKind {
    fn invalid() -> Self {
        Self::Invalid
    }
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
    pub binder: Identifier,
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
pub struct Substitution {
    pub substitution: interpreter::Substitution,
    pub expression: Expression,
}

#[derive(Clone)]
pub struct ForeignApplication {
    pub callee: Identifier,
    pub arguments: Vec<Expression>,
}

#[derive(Clone)]
pub struct IO {
    pub index: usize, // @Task IOIndex
    pub arguments: Vec<Expression>,
    // @Task next: Option<Expression>
}

#[derive(Clone)]
pub struct Case {
    pub pattern: Pattern,
    pub body: Expression,
}

pub type Pattern = Item<PatternKind>;

#[derive(Clone)]
pub enum PatternKind {
    Number(Rc<Number>),
    Text(Rc<String>),
    Binding(Rc<Binding>),
    Binder(Rc<Binder>),
    Deapplication(Rc<Deapplication>),
    Invalid,
}

impl InvalidFallback for PatternKind {
    fn invalid() -> Self {
        Self::Invalid
    }
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

use crate::support::DisplayWith;

impl DisplayWith for Declaration {
    type Linchpin = CrateScope;

    fn format(&self, linchpin: &Self::Linchpin, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(linchpin, 0, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl Declaration {
    fn format_with_depth(
        &self,
        scope: &CrateScope,
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
                    declaration.type_annotation.with(scope)
                )?;
                if let Some(expression) = &declaration.expression {
                    write!(f, " = {}", expression.with(scope))?;
                }
                writeln!(f)
            }
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder,
                        declaration.type_annotation.with(scope)
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(
                            f,
                            "{}{}",
                            " ".repeat(depth * INDENTATION_IN_SPACES),
                            constructor.with(scope)
                        )?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder,
                    declaration.type_annotation.with(scope)
                ),
            },
            Constructor(constructor) => writeln!(
                f,
                "{}: {}",
                constructor.binder,
                constructor.type_annotation.with(scope)
            ),
            Module(declaration) => {
                writeln!(f, "module {}: =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(scope, depth, f)?;
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
impl DisplayWith for Expression {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use joinery::JoinableIterator;
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal.with(scope)),
            Application(application) => {
                write!(f, "{} ", application.callee.wrap().with(scope))?;
                if application.explicitness.is_implicit() {
                    write!(
                        f,
                        "({}{})",
                        application.explicitness,
                        application.argument.with(scope)
                    )
                } else {
                    write!(f, "{}", application.argument.wrap().with(scope))
                }
            }
            Type => write!(f, "Type"),
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal),
            Binding(binding) => write!(
                f,
                "{}",
                FunctionScope::absolute_path(&binding.binder, scope)
            ),
            Lambda(lambda) => write!(f, "{}", lambda.with(scope)),
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                writeln!(f, "case {} of", analysis.subject.with(scope))?;
                for case in &analysis.cases {
                    write!(f, "{}", case.with(scope))?;
                }
                Ok(())
            }
            Invalid => write!(f, "<invalid>"),
            Substitution(substitution) => write!(
                f,
                "<substitution {} {}>",
                substitution.substitution.with(scope),
                substitution.expression.with(scope)
            ),
            ForeignApplication(application) => write!(
                f,
                "{} {}",
                application.callee,
                application
                    .arguments
                    .iter()
                    .map(|argument| argument.with(scope))
                    .join_with(' ')
            ),
            IO(io) => write!(
                f,
                "<io {} {}>",
                io.index,
                io.arguments
                    .iter()
                    .map(|argument| argument.with(scope))
                    .join_with(' ')
            ),
        }
    }
}

use std::fmt;

impl DisplayWith for PiType {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(parameter) = &self.parameter {
            write!(
                f,
                "({}{}: {})",
                self.explicitness,
                parameter,
                self.domain.with(scope)
            )?;
        } else if self.explicitness.is_implicit() {
            write!(f, "({}{})", self.explicitness, self.domain.with(scope))?;
        } else {
            write!(f, "{}", self.domain.wrap().with(scope))?;
        }
        write!(f, " -> {}", self.codomain.wrap().with(scope))
    }
}

impl DisplayWith for Lambda {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "\\({}{}", self.explicitness, self.parameter)?;
        if let Some(annotation) = &self.parameter_type_annotation {
            write!(f, ": {}", annotation.wrap().with(scope))?;
        }
        write!(f, ")")?;
        if let Some(annotation) = &self.body_type_annotation {
            write!(f, ": {}", annotation.wrap().with(scope))?;
        }
        write!(f, " => {}", self.body.wrap().with(scope))
    }
}

impl DisplayWith for Case {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "{} => {}",
            self.pattern.with(scope),
            self.body.with(scope)
        )
    }
}

// @Task update bracket business
impl DisplayWith for Pattern {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(
                f,
                "{}",
                FunctionScope::absolute_path(&binding.binder, scope)
            ),

            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => write!(
                f,
                "({}) ({})",
                application.callee.with(scope),
                application.argument.with(scope)
            ),
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

        !matches!(&self.0.kind, Type | Number(_) | Text(_) | Binding(_) | Invalid | Substitution(_))
    }
}

impl DisplayWith for PossiblyWrapped<'_> {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.needs_brackets_conservative() {
            write!(f, "({})", self.0.with(scope))
        } else {
            write!(f, "{}", self.0.with(scope))
        }
    }
}

pub macro decl($( $tree:tt )+) {
    crate::item::item!(crate::hir, DeclarationKind, Box; $( $tree )+)
}

pub macro expr($( $tree:tt )+) {
    crate::item::item!(crate::hir, ExpressionKind, Rc; $( $tree )+)
}

pub macro pat($( $tree:tt )+) {
    crate::item::item!(crate::hir, PatternKind, Rc; $( $tree )+)
}
