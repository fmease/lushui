//! Formatted printing of the HIR.

use std::fmt::{Debug, Display, Formatter, Result};

use super::*;

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
                        f.write_str(&" ".repeat(depth * INDENTATION_IN_SPACES))?;
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
                    f.write_str(&" ".repeat(depth * INDENTATION_IN_SPACES))?;
                    declaration.format(f, depth)?;
                }
            }
            Use(declaration) => match &declaration.binder {
                Some(binder) => writeln!(f, "use {} as {}", declaration.reference, binder)?,
                None => writeln!(f, "use {}", declaration.reference)?,
            },
        }

        Ok(())
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl<P: Pass> Display for Expression<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use ExpressionKind::*;

        match &self.kind {
            PiType(literal) => write!(
                f,
                "({}{}{}) -> ({})",
                literal.explicitness,
                literal
                    .parameter
                    .as_ref()
                    .map(|binder| format!("{}: ", binder))
                    .unwrap_or_default(),
                literal.domain,
                literal.codomain,
            ),
            Application(application) => write!(
                f,
                "({}) ({}{})",
                application.callee, application.explicitness, application.argument,
            ),
            Type => f.write_str("Type"),
            Nat(literal) => write!(f, "{}", literal.value),
            Text(literal) => write!(f, "{:?}", literal.value),
            Binding(path) => write!(f, "{}", path.binder),
            Lambda(lambda) => write!(f, "{}", lambda),
            UseIn => todo!(),
            CaseAnalysis(case_analysis) => write!(
                f,
                "case ({}){}",
                case_analysis.subject,
                case_analysis
                    .cases
                    .iter()
                    .map(|case| format!(" {}", case))
                    .collect::<String>()
            ),
            Substitution(substitution) => write!(
                f,
                "<substitution {} {}>",
                substitution.substitution, substitution.expression
            ),
            // @Task make it look like a normal application because this is user-visible in
            // error messages!!
            ForeignApplication(application) => write!(
                f,
                "<foreign {} {}>",
                application.callee,
                application
                    .arguments
                    .iter()
                    .map(|argument| format!("{},", argument))
                    .collect::<String>()
            ),
        }
    }
}

impl<P: Pass> fmt::Debug for Expression<P> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl<P: Pass> Display for Lambda<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "\\({}{}{}){} => ({})",
            self.explicitness,
            self.parameter,
            self.parameter_type_annotation
                .as_ref()
                .map(|parameter| format!(": {}", parameter))
                .unwrap_or_default(),
            self.body_type_annotation
                .as_ref()
                .map(|type_annotation| format!(": {}", type_annotation))
                .unwrap_or_default(),
            self.body
        )
    }
}

impl<P: Pass> Display for Case<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "of {} => ({})", self.pattern, self.body)
    }
}

impl<P: Pass> Display for Pattern<P> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.kind {
            PatternKind::NatPattern(literal) => write!(f, "{}", literal.value),
            PatternKind::BindingPattern(binding) => match &binding.type_annotation {
                Some(type_annotation) => write!(f, "({}: {})", binding.binder, type_annotation),
                None => write!(f, "{}", binding.binder),
            },
            PatternKind::ApplicationPattern(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
        }
    }
}
