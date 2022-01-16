//! The definition of the textual representation of the [lowered AST](super).

use colored::{Color, Colorize};

use std::fmt;

use crate::syntax::lexer::INDENTATION;

const KEYWORD_COLOR: Color = Color::Cyan;
const PUNCTUATION_COLOR: Color = Color::BrightMagenta;
const ATTRIBUTE_COLOR: Color = Color::BrightWhite;

impl fmt::Display for super::Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Nat(value) => write!(f, "{value}"),
            Self::Nat32(value) => write!(f, "{value}"),
            Self::Nat64(value) => write!(f, "{value}"),
            Self::Int(value) => write!(f, "{value}"),
            Self::Int32(value) => write!(f, "{value}"),
            Self::Int64(value) => write!(f, "{value}"),
        }
    }
}

// @Task reduce amount of (String) allocations
impl super::Declaration {
    fn format_with_depth(&self, depth: usize, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::DeclarationKind::*;

        for attribute in &self.attributes.0 {
            // @Task get rid of extra alloc
            writeln!(
                f,
                "{}{}",
                " ".repeat(depth * INDENTATION.0),
                attribute.to_string().color(ATTRIBUTE_COLOR)
            )?;
        }

        write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;

        match &self.value {
            Function(function) => {
                write!(
                    f,
                    "{}{colon} {}",
                    function.binder,
                    function.type_annotation,
                    colon = ":".color(PUNCTUATION_COLOR)
                )?;
                if let Some(expression) = &function.expression {
                    write!(
                        f,
                        " {equals} {}",
                        expression,
                        equals = "=".color(PUNCTUATION_COLOR)
                    )?;
                }
                writeln!(f)
            }
            Data(type_) => match &type_.constructors {
                Some(constructors) => {
                    writeln!(
                        f,
                        "{data} {binder}{colon} {type_} {of}",
                        binder = type_.binder,
                        colon = ":".color(PUNCTUATION_COLOR),
                        type_ = type_.type_annotation,
                        data = "data".color(KEYWORD_COLOR),
                        of = "of".color(KEYWORD_COLOR)
                    )?;
                    for constructor in constructors {
                        constructor.format_with_depth(depth + 1, f)?;
                    }
                    Ok(())
                }
                None => writeln!(
                    f,
                    "{data} {binder}{colon} {type_}",
                    data = "data".color(KEYWORD_COLOR),
                    binder = type_.binder,
                    colon = ":".color(PUNCTUATION_COLOR),
                    type_ = type_.type_annotation,
                ),
            },
            Constructor(constructor) => {
                writeln!(
                    f,
                    "{binder}{colon} {type_}",
                    binder = constructor.binder,
                    colon = ":".color(PUNCTUATION_COLOR),
                    type_ = constructor.type_annotation
                )
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
                    declaration.format_with_depth(depth + 1, f)?;
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
        format_pi_type_literal_or_lower(self, f)
    }
}

fn format_pi_type_literal_or_lower(
    expression: &super::Expression,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    match &expression.value {
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
                    write!(
                        f,
                        "{parameter}{colon} ",
                        colon = ":".color(PUNCTUATION_COLOR)
                    )?;
                }

                write!(f, "{}", pi.domain)?;
                write!(f, ")")?;
            } else {
                format_application_or_lower(&pi.domain, f)?;
            }

            write!(f, " {arrow} ", arrow = "->".color(PUNCTUATION_COLOR))?;
            format_pi_type_literal_or_lower(&pi.codomain, f)
        }
        Lambda(lambda) => {
            write!(
                f,
                "{backslash}{explicitness}",
                backslash = "\\".color(PUNCTUATION_COLOR),
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
                    write!(
                        f,
                        "{colon} {type_}",
                        colon = ":".color(PUNCTUATION_COLOR),
                        type_ = annotation
                    )?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(
                    f,
                    "{colon} {type_}",
                    colon = ":".color(PUNCTUATION_COLOR),
                    type_ = annotation,
                )?;
            }
            write!(
                f,
                " {arrow} {body}",
                arrow = "=>".color(PUNCTUATION_COLOR),
                body = lambda.body
            )
        }
        UseIn => todo!(),
        CaseAnalysis(analysis) => {
            write!(
                f,
                "{case} {subject} {of} {{ ",
                case = "case".color(KEYWORD_COLOR),
                subject = analysis.subject,
                of = "of".color(KEYWORD_COLOR)
            )?;
            let mut first = true;
            for case in &analysis.cases {
                if first {
                    first = false;
                } else {
                    write!(f, "; ")?;
                }

                write!(
                    f,
                    "{pattern} {arrow} {body}",
                    pattern = case.pattern,
                    arrow = "=>".color(PUNCTUATION_COLOR),
                    body = case.body
                )?;
            }
            write!(f, " }}")
        }
        _ => format_application_or_lower(expression, f),
    }
}

fn format_application_or_lower(
    expression: &super::Expression,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::ExpressionKind::*;

    match &expression.value {
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
    use super::ExpressionKind::*;

    for attribute in &expression.attributes.0 {
        // @Task get rid of wasted alloc
        write!(f, "{} ", attribute.to_string().color(ATTRIBUTE_COLOR))?;
    }

    match &expression.value {
        Type => write!(f, "{Type}", Type = "Type".blue()),
        NumberLiteral(literal) => write!(f, "{literal}"),
        TextLiteral(literal) => write!(f, "{literal}"),
        Binding(binding) => write!(f, "{}", binding.binder),
        Error => write!(f, "{}", "?(error)".red()),
        _ => write!(f, "({})", expression),
    }
}

// @Task update bracket business
impl fmt::Display for super::Pattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use super::PatternKind::*;

        match &self.value {
            NumberLiteral(number) => write!(f, "{}", number),
            TextLiteral(text) => write!(f, "{}", text),
            Binding(binding) => write!(f, "{}", binding.binder),
            Binder(binder) => write!(
                f,
                "{backslash}{binder}",
                backslash = "\\".color(PUNCTUATION_COLOR),
                binder = binder.binder
            ),
            Deapplication(application) => {
                write!(f, "({}) ({})", application.callee, application.argument)
            }
            Error => write!(f, "{}", "?(error)".red()),
        }
    }
}
