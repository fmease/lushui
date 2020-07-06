//! Formatted printing of the HIR.

use std::fmt::{Display, Formatter, Result};

use super::{Declaration, DeclarationKind, Expression, ExpressionKind, Pass, Pattern, PatternKind};

impl<P: Pass> Display for Declaration<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, 0)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
// @Task @Beacon display attributes
impl<P: Pass> Declaration<P> {
    fn format(&self, f: &mut Formatter<'_>, depth: usize) -> Result {
        use crate::INDENTATION_IN_SPACES;
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => match &declaration.expression {
                Some(expression) => writeln!(
                    f,
                    "{}: {} = {}",
                    declaration.binder, declaration.type_annotation, expression
                ),
                None => writeln!(f, "{}: {}", declaration.binder, declaration.type_annotation),
            }?,
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "data {}: {} =",
                        declaration.binder, declaration.type_annotation
                    )?;
                    for constructor in constructors {
                        let depth = depth + 1;
                        write!(f, "{}", &" ".repeat(depth * INDENTATION_IN_SPACES))?;
                        constructor.format(f, depth)?;
                    }
                }
                None => writeln!(
                    f,
                    "data {}: {}",
                    declaration.binder, declaration.type_annotation
                )?,
            },
            Constructor(constructor) => {
                writeln!(f, "{}: {}", constructor.binder, constructor.type_annotation)?;
            }
            Module(declaration) => {
                writeln!(f, "module {}: =", declaration.binder)?;
                for declaration in &declaration.declarations {
                    let depth = depth + 1;
                    write!(f, "{}", &" ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format(f, depth)?;
                }
            }
            Use(declaration) => match &declaration.binder {
                Some(binder) => writeln!(f, "use {} as {}", declaration.target, binder)?,
                None => writeln!(f, "use {}", declaration.target)?,
            },
            Invalid => write!(f, "<invalid>")?,
        }

        Ok(())
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl<P: Pass> Display for Expression<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use joinery::JoinableIterator;
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(f, "{}", literal),
            Application(application) => {
                write!(f, "{} ", PossiblyWrapped(&application.callee))?;
                if application.explicitness.is_implicit() {
                    write!(f, "({}{})", application.explicitness, application.argument)
                } else {
                    write!(f, "{}", PossiblyWrapped(&application.argument))
                }
            }
            Type => write!(f, "Type"),
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal.value),
            Binding(path) => write!(f, "{}", path.binder),
            Lambda(lambda) => write!(f, "{}", lambda),
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(case_analysis) => write!(
                f,
                "case {} of\n{}",
                case_analysis.subject,
                case_analysis
                    .cases
                    .iter()
                    .map(|case| format!("    {}", case))
                    .collect::<String>()
            ),
            Invalid => write!(f, "<invalid>"),
            Substitution(substitution) => write!(
                f,
                "<substitution {} {}>",
                substitution.substitution, substitution.expression
            ),
            ForeignApplication(application) => write!(
                f,
                "{} {}",
                application.callee,
                application
                    .arguments
                    .iter()
                    .map(PossiblyWrapped)
                    .join_with(' ')
            ),
        }
    }
}

use std::fmt;

impl<P: Pass> fmt::Debug for Expression<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<P: Pass> Display for super::PiType<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(parameter) = &self.parameter {
            write!(f, "({}{}: {})", self.explicitness, parameter, self.domain)?;
        } else if self.explicitness.is_implicit() {
            write!(f, "({}{})", self.explicitness, self.domain)?;
        } else {
            write!(f, "{}", PossiblyWrapped(&self.domain))?;
        }
        write!(f, " -> {}", PossiblyWrapped(&self.codomain))
    }
}

impl<P: Pass> Display for super::Lambda<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "\\({}{}{})",
            self.explicitness,
            self.parameter,
            self.parameter_type_annotation
                .as_ref()
                .map(|parameter| format!(": {}", parameter))
                .unwrap_or_default()
        )?;
        if let Some(type_annotation) = &self.body_type_annotation {
            write!(f, ": {}", PossiblyWrapped(type_annotation))?;
        }
        write!(f, " => {}", PossiblyWrapped(&self.body))
    }
}

impl<P: Pass> Display for super::Case<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{} => {}\n", self.pattern, self.body)
    }
}

// @Task update bracket business
impl<P: Pass> Display for Pattern<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use PatternKind::*;

        match &self.kind {
            Number(literal) => write!(f, "{}", literal),
            Text(literal) => write!(f, "{:?}", literal.value),
            Binding(path) => write!(f, "{}", path.binder),
            Binder(binding) => write!(f, "\\{}", binding.binder),
            Deapplication(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
        }
    }
}

struct PossiblyWrapped<'a, P: Pass>(&'a Expression<P>);

impl<P: Pass> PossiblyWrapped<'_, P> {
    fn needs_brackets_conservative(&self) -> bool {
        use ExpressionKind::*;

        !matches!(&self.0.kind, Type | Number(_) | Text(_) | Binding(_) | Invalid | Substitution(_))
    }
}

impl<P: Pass> fmt::Display for PossiblyWrapped<'_, P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.needs_brackets_conservative() {
            write!(f, "({})", self.0)
        } else {
            write!(f, "{}", self.0)
        }
    }
}
