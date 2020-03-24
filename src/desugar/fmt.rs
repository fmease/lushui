//! Formatted printing of the HIR.

use std::fmt::{Display, Formatter, Result};

use super::{
    expression, Binder, Constructor, Declaration, DeclarationKind, Expression, ExpressionKind,
};

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
impl<B: Binder> Display for Declaration<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match &self.kind {
            DeclarationKind::Value(declaration) => write!(
                f,
                "{}: {} = {}",
                declaration.binder, declaration.type_annotation, declaration.expression
            ),
            DeclarationKind::Data(declaration) => write!(
                f,
                "data {}: {} =\n{}",
                declaration.binder,
                declaration.type_annotation,
                declaration
                    .constructors
                    .iter()
                    .map(|constructor| format!("    {}", constructor))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            DeclarationKind::Module(declaration) => {
                f.write_str("module =\n")?;
                for declaration in &declaration.declarations {
                    writeln!(f, "    {}", declaration)?;
                }
                Ok(())
            }
            DeclarationKind::Use => todo!(),
            DeclarationKind::Foreign(declaration) => write!(
                f,
                "foreign {}: {}",
                declaration.binder, declaration.type_annotation
            ),
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
        match &self.kind {
            ExpressionKind::PiType(literal) => write!(
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
            ExpressionKind::Application(application) => write!(
                f,
                "({}) ({}{})",
                application.callee, application.explicitness, application.argument,
            ),
            ExpressionKind::Type => f.write_str("Type"),
            ExpressionKind::NatType => f.write_str("Nat"),
            ExpressionKind::Nat(literal) => write!(f, "{}", literal.value),
            ExpressionKind::TextType => f.write_str("Text"),
            ExpressionKind::Text(literal) => write!(f, "{:?}", literal.value),
            ExpressionKind::Binding(path) => write!(f, "{}", path.binder),
            ExpressionKind::Lambda(lambda) => write!(f, "{}", lambda),
            ExpressionKind::UseIn => todo!(),
            ExpressionKind::CaseAnalysis(case_analysis) => write!(
                f,
                "case ({}){}",
                case_analysis.subject,
                case_analysis
                    .cases
                    .iter()
                    .map(|case| format!(" {}", case))
                    .collect::<String>()
            ),
            ExpressionKind::Substitution(substitution) => write!(
                f,
                "<substitution {} {}>",
                substitution.substitution, substitution.expression
            ),
            ExpressionKind::UnsaturatedForeignApplication(application) => write!(
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

impl<B: Binder> Display for expression::Lambda<B> {
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

impl<B: Binder> Display for expression::Case<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "of {} => ({})", self.pattern, self.body)
    }
}

impl<B: Binder> Display for expression::Pattern<B> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            expression::Pattern::Nat(literal) => write!(f, "{}", literal.value),
            expression::Pattern::Binding {
                binder,
                type_annotation,
            } => match type_annotation {
                Some(type_annotation) => write!(f, "({}: {})", binder.binder, type_annotation),
                None => write!(f, "{}", binder.binder),
            },
            expression::Pattern::Application { callee, argument } => {
                write!(f, "({}) ({})", callee, argument)
            }
        }
    }
}
