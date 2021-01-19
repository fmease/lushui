//! The definition of the textual representation of the [lowered AST](crate::lowered_ast).

// @Task rewrite all the formatting functions to respect precedence
// just follow what we did for HIR nodes (crate::hir::format)

use crate::ast::{Explicit, Implicit};
use std::{default::default, fmt};

impl fmt::Display for super::AttributeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "@")?;

        match self {
            Self::Allow { lint } => write!(f, "(allow {})", lint),
            Self::Deny { lint } => write!(f, "(deny {})", lint),
            Self::Deprecated {
                reason,
                since,
                until,
                replacement,
            } => write!(
                f,
                "(deprecated (reason {:?}) (since {:?}) (until {:?}) (replacement {:?}))",
                reason, since, until, replacement
            ),
            // Self::Documentation { content } => writeln!(f, ";{:?}", content),
            Self::Documentation { content } => write!(f, "(documentation {:?})", content),
            Self::Forbid { lint } => write!(f, "(forbid {})", lint),
            Self::Foreign => write!(f, "foreign"),
            Self::If { condition } => write!(f, "(if {})", condition),
            Self::Ignore => write!(f, "ignore"),
            Self::Include => write!(f, "include"),
            Self::Inherent => write!(f, "inherent"),
            Self::Int => write!(f, "Int"),
            Self::Int32 => write!(f, "Int32"),
            Self::Int64 => write!(f, "Int64"),
            Self::List => write!(f, "List"),
            Self::Location { path } => write!(f, "(location {})", path),
            Self::Moving => write!(f, "moving"),
            Self::Nat => write!(f, "Nat"),
            Self::Nat32 => write!(f, "Nat32"),
            Self::Nat64 => write!(f, "Nat64"),
            Self::Opaque => write!(f, "opaque"),
            Self::Public { reach } => match reach {
                Some(reach) => write!(f, "(public {})", reach),
                None => write!(f, "public"),
            },
            Self::RecursionLimit { depth } => write!(f, "(recursion-limit {})", depth),
            Self::Rune => write!(f, "Rune"),
            Self::Static => write!(f, "static"),
            Self::Test => write!(f, "test"),
            Self::Text => write!(f, "Text"),
            Self::Unsafe => write!(f, "unsafe"),
            Self::Unstable { feature, reason } => write!(f, "(feature {} {:?})", feature, reason),
            Self::Vector => write!(f, "Vector"),
            Self::Warn { lint } => write!(f, "(warn {})", lint),
        }
    }
}

impl fmt::Display for super::Lint {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl fmt::Display for super::Condition {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl fmt::Display for super::Feature {
    fn fmt(&self, _f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match *self {}
    }
}

impl fmt::Display for super::Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{}", value),
            Self::Nat32(value) => write!(f, "{}", value),
            Self::Nat64(value) => write!(f, "{}", value),
            Self::Int(value) => write!(f, "{}", value),
            Self::Int32(value) => write!(f, "{}", value),
            Self::Int64(value) => write!(f, "{}", value),
        }
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
impl super::Declaration {
    fn format_with_depth(&self, depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::DeclarationKind::*;
        use crate::INDENTATION_IN_SPACES;

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
                writeln!(f, "module {} =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format_with_depth(depth, f)?;
                }
                Ok(())
            }
            Use(declaration) => writeln!(f, "use {} as {}", declaration.target, declaration.binder),
            Invalid => write!(f, "?(invalid)"),
        }
    }
}

impl fmt::Display for super::Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.format_with_depth(0, f)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for super::Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal),
            Application(application) => {
                write!(f, "{} ", application.callee.wrap())?;
                match application.explicitness {
                    Explicit => write!(f, "{}", application.argument.wrap()),
                    Implicit => write!(f, "({}{})", application.explicitness, application.argument),
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
            Invalid => write!(f, "?(invalid)"),
        }
    }
}

impl fmt::Display for super::PiType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let domain_needs_brackets = self.parameter.is_some() || self.aspect != default();

        if domain_needs_brackets {
            write!(f, "(")?;
        }

        write!(f, "{}", self.aspect)?;

        if let Some(parameter) = &self.parameter {
            write!(f, "{parameter}: ")?;
        }

        if domain_needs_brackets {
            write!(f, "{}", self.domain)?;
        } else {
            write!(f, "{}", self.domain.wrap())?;
        }

        if domain_needs_brackets {
            write!(f, ")")?;
        }

        write!(f, " -> {}", self.codomain.wrap())
    }
}

impl fmt::Display for super::Lambda {
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

impl fmt::Display for super::Case {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} => {}", self.pattern, self.body)
    }
}

// @Task update bracket business
impl fmt::Display for super::Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::PatternKind::*;

        match &self.kind {
            Number(number) => write!(f, "{}", number),
            Text(text) => write!(f, "{:?}", text),
            Binding(binding) => write!(f, "{}", binding.binder),
            Binder(binder) => write!(f, "\\{}", binder.binder),
            Deapplication(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
            Invalid => write!(f, "?(invalid)"),
        }
    }
}

trait WrapExpression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a>;
}

impl WrapExpression for super::Expression {
    fn wrap<'a>(&'a self) -> PossiblyWrapped<'a> {
        PossiblyWrapped(self)
    }
}

struct PossiblyWrapped<'a>(&'a super::Expression);

impl PossiblyWrapped<'_> {
    fn needs_brackets_conservative(&self) -> bool {
        use super::ExpressionKind::*;

        !matches!(
            &self.0.kind,
            Type | Number(_) | Text(_) | Binding(_) | Invalid
        )
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
