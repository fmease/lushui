//! Formatting routines for the internal textual representation of the [Lo-AST](super).

// FIXME: If we decide to not get rid of Lo-AST in favor of HIR, color the output again.

use lexer::token::INDENTATION;
use std::fmt;
use utility::Atom;

pub trait Display {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Display for super::Declaration {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_declaration(self, 0, f)
    }
}

// @Task reduce amount of (String) allocations
fn write_declaration(
    declaration: &super::Declaration,
    depth: usize,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareDeclaration::*;

    for attribute in &declaration.attributes.0 {
        writeln!(f, "{}{attribute}", " ".repeat(depth * INDENTATION.0))?;
    }

    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;

    match &declaration.bare {
        Function(function) => {
            write!(f, "{}: ", function.binder)?;
            function.type_.write(f)?;
            if let Some(expression) = &function.body {
                f.write_str(" = ")?;
                expression.write(f)?;
            }
            writeln!(f)
        }
        Data(type_) => match &type_.declarations {
            Some(constructors) => {
                write!(f, "data {}: ", type_.binder)?;
                type_.type_.write(f)?;
                writeln!(f, " of")?;
                for constructor in constructors {
                    write_declaration(constructor, depth + 1, f)?;
                }
                Ok(())
            }
            None => {
                write!(f, "data {}: ", type_.binder)?;
                type_.type_.write(f)?;
                writeln!(f)
            }
        },
        Constructor(constructor) => {
            write!(f, "{}: ", constructor.binder)?;
            constructor.type_.write(f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.declarations {
                write_declaration(declaration, depth + 1, f)?;
            }
            Ok(())
        }
        Use(use_) => writeln!(f, "use {} as {}", use_.target, use_.binder),
        Error(_) => f.write_str("⟨error⟩"),
    }
}

impl Display for super::Expression {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pi_type_literal_or_lower(self, f)
    }
}

// @Task display fewer round brackets by making use of precedence
// @Note many wasted allocations (intermediate Strings)
// @Task add sequences!
fn write_pi_type_literal_or_lower(
    expression: &super::Expression,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    match &expression.bare {
        PiType(pi) => {
            if pi.binder.is_none() && pi.kind == ast::ParameterKind::Explicit {
                format_application_or_lower(&pi.domain, f)?;
            } else {
                f.write_str("For ")?;

                if pi.kind == ast::ParameterKind::Implicit {
                    f.write_str("'")?;
                }

                if pi.kind == ast::ParameterKind::Context {
                    f.write_str("[")?;

                    if let Some(binder) = &pi.binder {
                        write!(f, "{binder}: ")?;
                    }
                    pi.domain.write(f)?;

                    f.write_str("]")?;
                } else {
                    let binder = pi.binder.map_or(Atom::UNDERSCORE, ast::Identifier::bare);

                    write!(f, "({binder}: ")?;
                    pi.domain.write(f)?;
                    f.write_str(")")?;
                }
            }

            f.write_str(" -> ")?;
            write_pi_type_literal_or_lower(&pi.codomain, f)
        }
        Lambda(lambda) => {
            f.write_str("for ")?;

            if lambda.kind == ast::ParameterKind::Implicit {
                write!(f, "'")?;
            }

            if lambda.kind == ast::ParameterKind::Context {
                f.write_str("[")?;

                if let Some(binder) = &lambda.binder {
                    write!(f, "{binder}: ")?;
                }

                // Although it's not statically guaranteed, the domain of context parameters has exist.
                // Let's not unwrap though for robustness.
                if let Some(domain) = &lambda.domain {
                    domain.write(f)?;
                }

                f.write_str("]")?;
            } else {
                let binder = lambda.binder.map_or(Atom::UNDERSCORE, ast::Identifier::bare);

                if let Some(domain) = &lambda.domain {
                    f.write_str("(")?;
                    write!(f, "{binder}: ")?;
                    domain.write(f)?;
                    f.write_str(")")?;
                } else {
                    write!(f, "{binder}")?;
                }
            }

            if let Some(codomain) = &lambda.codomain {
                f.write_str(": ")?;
                codomain.write(f)?;
            }
            f.write_str(" => ")?;
            lambda.body.write(f)
        }
        // @Task get rid of the delimited blocks, they are no longer part of the surface language!
        CaseAnalysis(analysis) => {
            f.write_str("case ")?;
            analysis.scrutinee.write(f)?;
            f.write_str(" of { ")?;
            let mut first = true;
            for case in &analysis.cases {
                if first {
                    first = false;
                } else {
                    write!(f, "; ")?;
                }

                case.pattern.write(f)?;
                f.write_str(" => ")?;
                case.body.write(f)?;
            }
            f.write_str(" }")
        }
        SequenceLiteral(sequence) => {
            if let Some(path) = &sequence.path {
                write!(f, "{path}.")?;
            }

            f.write_str("(")?;
            let mut elements = sequence.elements.bare.iter();
            if let Some(element) = elements.next() {
                element.write(f)?;
            }
            for element in elements {
                f.write_str(", ")?;
                element.write(f)?;
            }
            if sequence.path.is_none() && sequence.elements.bare.len() == 1 {
                f.write_str(",")?;
            }
            f.write_str(")")
        }
        RecordLiteral(record) => {
            if let Some(path) = &record.path {
                write!(f, "{path}.")?;
            }
            f.write_str("{")?;
            let mut fields = record.fields.bare.iter();
            if let Some(field) = fields.next() {
                write!(f, "{} = ", field.binder)?;
                field.body.write(f)?;
            }
            for field in fields {
                write!(f, ", {} = ", field.binder)?;
                field.body.write(f)?;
            }
            f.write_str("}")
        }
        Projection(projection) => {
            format_lower_expression(&projection.basis, f)?;
            write!(f, "::{}", projection.field)
        }
        _ => format_application_or_lower(expression, f),
    }
}

fn format_application_or_lower(
    expression: &super::Expression,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    match &expression.bare {
        Application(application) => {
            format_application_or_lower(&application.callee, f)?;

            f.write_str(" ")?;

            if application.kind == ast::ParameterKind::Implicit {
                f.write_str("'")?;
            }
            if application.kind == ast::ParameterKind::Context {
                f.write_str("[")?;
                application.argument.write(f)?;
                f.write_str("]")
            } else {
                format_lower_expression(&application.argument, f)
            }
        }
        _ => format_lower_expression(expression, f),
    }
}

fn format_lower_expression(
    expression: &super::Expression,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    for attribute in &expression.attributes.0 {
        write!(f, "{attribute} ")?;
    }

    match &expression.bare {
        Type => f.write_str("⟨Type⟩"),
        LocalBinding(_) => f.write_str("⟨_⟩"),
        Wildcard(wildcard) => wildcard.write(f),
        NumberLiteral(number) => write!(f, "{number}"),
        TextLiteral(text) => write!(f, "{text}"),
        Path(path) => write!(f, "{path}"),
        Error(_) => f.write_str("⟨error⟩"),
        _ => {
            write!(f, "(")?;
            expression.write(f)?;
            write!(f, ")")
        }
    }
}

// @Task respect / incorporate precedence just like we do with expressions already
impl Display for super::Pattern {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::BarePattern::*;

        match &self.bare {
            Wildcard(wildcard) => wildcard.write(f),
            NumberLiteral(number) => write!(f, "{number}"),
            TextLiteral(text) => write!(f, "{text}"),
            Path(path) => write!(f, "{path}"),
            LetBinding(binder) => write!(f, "(let {binder})"),
            Application(application) => {
                write!(f, "(")?;
                application.callee.write(f)?;
                write!(f, ") (")?;
                application.argument.write(f)?;
                write!(f, ")")
            }
            // @Task abstract over fmt'ing sequence literals (via a function over Item<_>)
            // once we have format_lower_pattern
            SequenceLiteral(sequence) => {
                if let Some(path) = &sequence.path {
                    write!(f, "{path}.")?;
                }

                write!(f, "[")?;
                let mut elements = sequence.elements.bare.iter();
                if let Some(element) = elements.next() {
                    write!(f, "(")?;
                    element.write(f)?;
                    write!(f, ")")?;
                }
                for element in elements {
                    write!(f, "(")?;
                    element.write(f)?;
                    write!(f, ")")?;
                }
                write!(f, "]")
            }
            Error(_) => f.write_str("⟨error⟩"),
        }
    }
}

impl Display for ast::Wildcard {
    fn write(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Silent => f.write_str("_"),
            Self::Signaling { tag } => write!(f, "?{tag}"),
        }
    }
}
