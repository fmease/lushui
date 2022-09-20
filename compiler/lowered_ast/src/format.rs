//! The definition of the textual representation of the [lowered AST](super).

use colored::{Color, Colorize};
use std::fmt;
use token::INDENTATION;

const KEYWORD_COLOR: Color = Color::Cyan;
const SYMBOL_COLOR: Color = Color::BrightMagenta;
const ATTRIBUTE_COLOR: Color = Color::BrightWhite;

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
            attribute.to_string().color(ATTRIBUTE_COLOR)
        )?;
    }

    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;

    match &declaration.bare {
        Function(function) => {
            write!(
                f,
                "{}{colon} ",
                function.binder,
                colon = ":".color(SYMBOL_COLOR)
            )?;
            function.type_annotation.write(f)?;
            if let Some(expression) = &function.expression {
                write!(f, " {equals} ", equals = "=".color(SYMBOL_COLOR))?;
                expression.write(f)?;
            }
            writeln!(f)
        }
        Data(type_) => match &type_.constructors {
            Some(constructors) => {
                write!(
                    f,
                    "{data} {binder}{colon} ",
                    data = "data".color(KEYWORD_COLOR),
                    binder = type_.binder,
                    colon = ":".color(SYMBOL_COLOR),
                )?;
                type_.type_annotation.write(f)?;
                writeln!(f, " {of}", of = "of".color(KEYWORD_COLOR))?;
                for constructor in constructors {
                    write_declaration(constructor, depth + 1, f)?;
                }
                Ok(())
            }
            None => {
                writeln!(
                    f,
                    "{data} {binder}{colon} ",
                    data = "data".color(KEYWORD_COLOR),
                    binder = type_.binder,
                    colon = ":".color(SYMBOL_COLOR),
                )?;
                type_.type_annotation.write(f)?;
                writeln!(f)
            }
        },
        Constructor(constructor) => {
            write!(
                f,
                "{binder}{colon} ",
                binder = constructor.binder,
                colon = ":".color(SYMBOL_COLOR),
            )?;
            constructor.type_annotation.write(f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(
                f,
                "{module} {binder} {of}",
                module = "module".color(KEYWORD_COLOR),
                binder = module.binder,
                of = "of".color(KEYWORD_COLOR)
            )?;
            for declaration in &module.declarations {
                write_declaration(declaration, depth + 1, f)?;
            }
            Ok(())
        }
        Use(use_) => writeln!(
            f,
            "{use_} {target} {as_} {binder}",
            use_ = "use".color(KEYWORD_COLOR),
            target = use_.target,
            as_ = "as".color(KEYWORD_COLOR),
            binder = use_.binder,
        ),
        Error => write!(f, "{}", "?(error)".red()),
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
            write!(f, "{}", pi.explicitness)?;

            // @Note fragile
            let domain_needs_brackets = pi.parameter.is_some() || pi.laziness.is_some();

            if domain_needs_brackets {
                write!(f, "(")?;
                if pi.laziness.is_some() {
                    write!(f, "lazy ")?;
                }

                if let Some(parameter) = &pi.parameter {
                    write!(f, "{parameter}{colon} ", colon = ":".color(SYMBOL_COLOR))?;
                }

                pi.domain.write(f)?;
                write!(f, ")")?;
            } else {
                format_application_or_lower(&pi.domain, f)?;
            }

            write!(f, " {arrow} ", arrow = "->".color(SYMBOL_COLOR))?;
            write_pi_type_literal_or_lower(&pi.codomain, f)
        }
        Lambda(lambda) => {
            write!(
                f,
                "{backslash}{explicitness}",
                backslash = "\\".color(SYMBOL_COLOR),
                explicitness = lambda.explicitness,
            )?;
            let parameter_needs_brackets =
                lambda.parameter_type_annotation.is_some() || lambda.laziness.is_some();

            if parameter_needs_brackets {
                write!(f, "(")?;
                if lambda.laziness.is_some() {
                    write!(f, "{lazy} ", lazy = "lazy".color(KEYWORD_COLOR))?;
                }
                write!(f, "{}", lambda.parameter)?;
                if let Some(annotation) = &lambda.parameter_type_annotation {
                    write!(f, "{colon} ", colon = ":".color(SYMBOL_COLOR),)?;
                    annotation.write(f)?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(f, "{colon} ", colon = ":".color(SYMBOL_COLOR),)?;
                annotation.write(f)?;
            }
            write!(f, " {arrow} ", arrow = "=>".color(SYMBOL_COLOR))?;
            lambda.body.write(f)
        }
        UseIn => todo!(),
        CaseAnalysis(analysis) => {
            write!(f, "{case} ", case = "case".color(KEYWORD_COLOR),)?;
            analysis.scrutinee.write(f)?;
            write!(f, " {of} {{ ", of = "of".color(KEYWORD_COLOR))?;
            let mut first = true;
            for case in &analysis.cases {
                if first {
                    first = false;
                } else {
                    write!(f, "; ")?;
                }

                case.pattern.write(f)?;
                write!(f, " {arrow} ", arrow = "=>".color(SYMBOL_COLOR),)?;
                case.body.write(f)?;
            }
            write!(f, " }}")
        }
        // @Task abstract over fmting sequence literals (via a function over Item<_>)
        // once we have format_lower_pattern
        SequenceLiteral(sequence) => {
            if let Some(path) = &sequence.path {
                write!(f, "{path}.")?;
            }

            write!(f, "[")?;
            let mut elements = sequence.elements.bare.iter();
            if let Some(element) = elements.next() {
                format_lower_expression(element, f)?;
            }
            for element in elements {
                write!(f, " ")?;
                format_lower_expression(element, f)?;
            }
            write!(f, "]")
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
            write!(f, " {}", application.explicitness)?;
            format_lower_expression(&application.argument, f)
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
        write!(f, "{} ", attribute.to_string().color(ATTRIBUTE_COLOR))?;
    }

    match &expression.bare {
        NumberLiteral(number) => write!(f, "{number}"),
        TextLiteral(text) => write!(f, "{text}"),
        Path(path) => write!(f, "{path}"),
        Error => write!(f, "{}", "?(error)".red()),
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
            NumberLiteral(number) => write!(f, "{number}"),
            TextLiteral(text) => write!(f, "{text}"),
            Path(path) => write!(f, "{path}"),
            Binder(path) => write!(f, "{backslash}{path}", backslash = "\\".color(SYMBOL_COLOR),),
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
            Error => write!(f, "{}", "?(error)".red()),
        }
    }
}
