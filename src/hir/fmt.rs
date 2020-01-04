use std::fmt::{Display, Formatter, Result};

use super::{expression, Constructor, Declaration, Expression};

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
impl Display for Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Let {
                binder,
                type_annotation,
                expression,
            } => write!(f, "'let {}: {} = {}", binder, type_annotation, expression),
            Self::Data {
                binder,
                type_annotation,
                constructors,
            } => write!(
                f,
                "'data {}: {} =\n{}",
                binder,
                type_annotation,
                constructors
                    .into_iter()
                    .map(|constructor| format!("    {}", constructor))
                    .collect::<Vec<_>>()
                    .join("\n")
            ),
            Self::Module { declarations } => {
                f.write_str("'module =\n")?;
                for declaration in declarations {
                    write!(f, "{}\n", declaration)?;
                }
                Ok(())
            }
            Self::Use => unimplemented!(),
            Self::Foreign => unimplemented!(),
        }
    }
}

impl Display for Constructor {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "{}: {}", self.binder, self.type_annotation)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::PiTypeLiteral(literal, _) => write!(
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
            Self::Application(application, _) => write!(
                f,
                "({}) ({}{})",
                application.expression, application.explicitness, application.argument,
            ),
            Self::TypeLiteral(_, _) => f.write_str("'Type"),
            Self::NatTypeLiteral(_, _) => f.write_str("'Nat"),
            Self::NatLiteral(literal, _) => write!(f, "{}", literal.value),
            Self::Path(path, _) => write!(f, "{}", path.identifier),
            Self::Hole(hole, _) => write!(f, "'hole {}", hole.tag),
            Self::LambdaLiteral(literal, _) => write!(
                f,
                "\\({}{}{}){} => ({})",
                literal.explicitness,
                literal.parameter,
                literal
                    .parameter_type_annotation
                    .as_ref()
                    .map(|parameter| format!(": {}", parameter))
                    .unwrap_or_default(),
                literal
                    .body_type_annotation
                    .as_ref()
                    .map(|type_annotation| format!(": {}", type_annotation))
                    .unwrap_or_default(),
                literal.body
            ),
            Self::UseIn(_, _) => unimplemented!(),
            Self::CaseAnalysis(case_analysis, _) => write!(
                f,
                "'case ({}){}",
                case_analysis.expression,
                case_analysis
                    .cases
                    .iter()
                    .map(|case| format!(" {}", case))
                    .collect::<String>()
            ),
        }
    }
}

impl Display for expression::CaseAnalysisCase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(f, "'of {} => ({})", self.pattern, self.expression)
    }
}

impl Display for expression::Pattern {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            expression::Pattern::NatLiteral(literal) => write!(f, "{}", literal.value),
            expression::Pattern::Path {
                path,
                type_annotation,
            } => match type_annotation {
                Some(type_annotation) => write!(f, "({}: {})", path.identifier, type_annotation),
                None => write!(f, "{}", path.identifier),
            },
            expression::Pattern::Application { callee, argument } => {
                write!(f, "({}) ({})", callee, argument)
            }
        }
    }
}