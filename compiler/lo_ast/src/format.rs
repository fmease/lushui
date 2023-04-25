//! Formatting routines for the internal textual representation of the [Lo-AST](super).

use colored::Colorize;
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
        // @Task get rid of extra alloc
        writeln!(
            f,
            "{}{}",
            " ".repeat(depth * INDENTATION.0),
            attribute.to_string().color(palette::ATTRIBUTE)
        )?;
    }

    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;

    match &declaration.bare {
        Function(function) => {
            write!(f, "{}{} ", function.binder, ":".color(palette::SYMBOL))?;
            function.type_.write(f)?;
            if let Some(expression) = &function.body {
                write!(f, " {} ", "=".color(palette::SYMBOL))?;
                expression.write(f)?;
            }
            writeln!(f)
        }
        Data(type_) => match &type_.declarations {
            Some(constructors) => {
                write!(
                    f,
                    "{} {}{} ",
                    "data".color(palette::KEYWORD),
                    type_.binder,
                    ":".color(palette::SYMBOL),
                )?;
                type_.type_.write(f)?;
                writeln!(f, " {}", "of".color(palette::KEYWORD))?;
                for constructor in constructors {
                    write_declaration(constructor, depth + 1, f)?;
                }
                Ok(())
            }
            None => {
                write!(
                    f,
                    "{} {}{} ",
                    "data".color(palette::KEYWORD),
                    type_.binder,
                    ":".color(palette::SYMBOL),
                )?;
                type_.type_.write(f)?;
                writeln!(f)
            }
        },
        Constructor(constructor) => {
            write!(f, "{}{} ", constructor.binder, ":".color(palette::SYMBOL))?;
            constructor.type_.write(f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(
                f,
                "{module} {binder} {of}",
                module = "module".color(palette::KEYWORD),
                binder = module.binder,
                of = "of".color(palette::KEYWORD)
            )?;
            for declaration in &module.declarations {
                write_declaration(declaration, depth + 1, f)?;
            }
            Ok(())
        }
        Use(use_) => writeln!(
            f,
            "{use_} {target} {as_} {binder}",
            use_ = "use".color(palette::KEYWORD),
            target = use_.target,
            as_ = "as".color(palette::KEYWORD),
            binder = use_.binder,
        ),
        Error(_) => write!(f, "{}", "⟨error⟩".red()),
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
                write!(f, "{} ", "For".color(palette::KEYWORD))?;

                if pi.kind == ast::ParameterKind::Implicit {
                    f.write_str("'")?;
                }

                if pi.kind == ast::ParameterKind::Context {
                    f.write_str("[")?;

                    if let Some(binder) = &pi.binder {
                        write!(f, "{binder}{} ", ":".color(palette::SYMBOL))?;
                    }
                    pi.domain.write(f)?;

                    f.write_str("]")?;
                } else {
                    let binder = pi.binder.map_or(Atom::UNDERSCORE, ast::Identifier::bare);

                    write!(f, "({binder}{} ", ":".color(palette::SYMBOL))?;
                    pi.domain.write(f)?;
                    f.write_str(")")?;
                }
            }

            write!(f, " {arrow} ", arrow = "->".color(palette::SYMBOL))?;
            write_pi_type_literal_or_lower(&pi.codomain, f)
        }
        Lambda(lambda) => {
            write!(f, "{} ", "for".color(palette::KEYWORD))?;

            if lambda.kind == ast::ParameterKind::Implicit {
                write!(f, "'")?;
            }

            if lambda.kind == ast::ParameterKind::Context {
                f.write_str("[")?;

                if let Some(binder) = &lambda.binder {
                    write!(f, "{binder}{} ", ":".color(palette::SYMBOL))?;
                }

                // Although it's not statically guaranteed, the domain of context parameters has exist.
                // Let's not unwrap though for robustness.
                if let Some(domain) = &lambda.domain {
                    domain.write(f)?;
                }

                f.write_str("]")?;
            } else {
                let binder = lambda
                    .binder
                    .map_or(Atom::UNDERSCORE, ast::Identifier::bare);

                if let Some(domain) = &lambda.domain {
                    f.write_str("(")?;
                    write!(f, "{binder}{} ", ":".color(palette::SYMBOL))?;
                    domain.write(f)?;
                    f.write_str(")")?;
                } else {
                    write!(f, "{binder}")?;
                }
            }

            if let Some(codomain) = &lambda.codomain {
                write!(f, "{colon} ", colon = ":".color(palette::SYMBOL))?;
                codomain.write(f)?;
            }
            write!(f, " {arrow} ", arrow = "=>".color(palette::SYMBOL))?;
            lambda.body.write(f)
        }
        // @Task get rid of the delimited blocks, they are no longer part of the surface language!
        CaseAnalysis(analysis) => {
            write!(f, "{case} ", case = "case".color(palette::KEYWORD))?;
            analysis.scrutinee.write(f)?;
            write!(f, " {of} {{ ", of = "of".color(palette::KEYWORD))?;
            let mut first = true;
            for case in &analysis.cases {
                if first {
                    first = false;
                } else {
                    write!(f, "; ")?;
                }

                case.pattern.write(f)?;
                write!(f, " {arrow} ", arrow = "=>".color(palette::SYMBOL))?;
                case.body.write(f)?;
            }
            write!(f, " }}")
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
                write!(f, "{}", field.name)?;
                if let Some(expression) = &field.item {
                    f.write_str(" = ")?;
                    expression.write(f)?;
                }
            }
            for field in fields {
                write!(f, ", {}", field.name)?;
                if let Some(expression) = &field.item {
                    f.write_str(" = ")?;
                    expression.write(f)?;
                }
            }
            f.write_str("}")
        }
        Projection(projection) => {
            format_lower_expression(&projection.basis, f)?;
            write!(f, "{}{}", "::".color(palette::SYMBOL), projection.field)
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
        // @Task get rid of wasted alloc
        write!(f, "{} ", attribute.to_string().color(palette::ATTRIBUTE))?;
    }

    match &expression.bare {
        Type => {
            if colored::control::SHOULD_COLORIZE.should_colorize() {
                write!(f, "{}", "Type".color(palette::KEYWORD))
            } else {
                f.write_str("⟨Type⟩")
            }
        }
        LocalBinding(_) => {
            if colored::control::SHOULD_COLORIZE.should_colorize() {
                write!(f, "{}", "_".color(palette::KEYWORD))
            } else {
                f.write_str("⟨_⟩")
            }
        }
        Wildcard(wildcard) => wildcard.write(f),
        NumberLiteral(number) => write!(f, "{number}"),
        TextLiteral(text) => write!(f, "{text}"),
        Path(path) => write!(f, "{path}"),
        Error(_) => write!(f, "{}", "⟨error⟩".red()),
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
            LetBinding(binder) => {
                write!(f, "({} {binder})", "let ".color(palette::KEYWORD))
            }
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
            Error(_) => write!(f, "{}", "⟨error⟩".red()),
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

mod palette {
    use colored::Color;

    pub(super) const KEYWORD: Color = Color::Cyan;
    pub(super) const SYMBOL: Color = Color::BrightMagenta;
    pub(super) const ATTRIBUTE: Color = Color::BrightWhite;
}
