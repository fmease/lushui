//! Formatted printing of the HIR.

use std::fmt::{Display, Formatter, Result};

use super::*;

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
// @Task @Beacon display attributes
impl<B: Binder> Display for Declaration<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        use DeclarationKind::*;

        match &self.kind {
            Value(declaration) => match &declaration.expression {
                Some(expression) => write!(
                    f,
                    "{}: {} = {}",
                    declaration.binder, declaration.type_annotation, expression
                ),
                None => write!(f, "{}: {}", declaration.binder, declaration.type_annotation),
            },
            Data(declaration) => match &declaration.constructors {
                Some(constructors) => write!(
                    f,
                    "data {}: {} =\n{}",
                    declaration.binder,
                    declaration.type_annotation,
                    constructors
                        .iter()
                        .map(|constructor| format!("    {}", constructor))
                        .collect::<Vec<_>>()
                        .join("\n")
                ),
                None => write!(
                    f,
                    "data {}: {}\n",
                    declaration.binder, declaration.type_annotation
                ),
            },
            Module(declaration) => {
                f.write_str("module =\n")?;
                for declaration in &declaration.declarations {
                    writeln!(f, "    {}", declaration)?;
                }
                Ok(())
            }
            Use => todo!(),
        }
    }
}

impl<B: Binder> Display for Constructor<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.binder, self.type_annotation)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl<B: Binder> Display for Expression<B> {
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
            NatType => f.write_str("Nat"),
            Nat(literal) => write!(f, "{}", literal.value),
            TextType => f.write_str("Text"),
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
            UnsaturatedForeignApplication(application) => write!(
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

impl<B: Binder> Display for Lambda<B> {
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

impl<B: Binder> Display for Case<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "of {} => ({})", self.pattern, self.body)
    }
}

impl<B: Binder> Display for Pattern<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Pattern::Nat(literal) => write!(f, "{}", literal.value),
            Pattern::Binding {
                binder,
                type_annotation,
            } => match type_annotation {
                Some(type_annotation) => write!(f, "({}: {})", binder.binder, type_annotation),
                None => write!(f, "{}", binder.binder),
            },
            Pattern::Application { callee, argument } => write!(f, "({}) ({})", callee, argument),
        }
    }
}
