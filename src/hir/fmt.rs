use std::fmt;

use super::{Constructor, Declaration, Expression};

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled (e.g. an indented data declaration doesn't have its constructors indented)
// @Task implement indentation logic (@Note for now, it's not that relevant because we don*t have modules yet, so a data
// declaration is never actually indented, also expressions which face the same issue when pretty-printing, are printed out in one single line!)
impl fmt::Display for Declaration {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                "'data {}: {}\n{}",
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

// @Question should the line break/indentation be part of the result? I don't think so:
// The parent context should decide (e.g. no line break on the end of output, further indentation)
impl fmt::Display for Constructor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.binder, self.type_annotation)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
            Self::CaseAnalysis(_, _) => unimplemented!(),
        }
    }}
