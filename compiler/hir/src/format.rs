//! The definition of the textual representation of the [HIR](crate::hir).

use super::{Declaration, Expression, Pattern};
use joinery::JoinableIterator;
use lushui_utilities::formatted;
use std::{fmt, str::FromStr};

// @Beacon @Beacon @Beacon @Temporary uncomment once the cyclic dep on session is resolved
// #[cfg(test)]
// mod test;

pub trait DisplayContext: Copy {
    fn path_to_string(self, binding: &super::Identifier) -> String;
}

// @Task
pub trait Display<C: DisplayContext> {
    type Output<'a>: fmt::Display
    where
        Self: 'a,
        C: 'a;

    fn display(&self, context: C) -> Self::Output<'_>;
}

impl<C: DisplayContext> Display<C> for Declaration {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| format_declaration(self, 0, context, f))
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
fn format_declaration(
    declaration: &Declaration,
    depth: usize,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareDeclaration::*;
    // use crate::syntax::lexer::INDENTATION;
    // @Beacon @Beacon @Beacon @Temporary use lushui_lexer's def here
    const INDENTATION: (usize,) = (4,);

    match &declaration.bare {
        Function(function) => {
            write!(
                f,
                "{}: {}",
                function.binder,
                function.type_annotation.display(context)
            )?;
            if let Some(expression) = &function.expression {
                write!(f, " = {}", expression.display(context))?;
            }
            writeln!(f)
        }
        Data(type_) => match &type_.constructors {
            Some(constructors) => {
                writeln!(
                    f,
                    "data {}: {} of",
                    type_.binder,
                    type_.type_annotation.display(context)
                )?;
                for constructor in constructors {
                    let depth = depth + 1;
                    write!(
                        f,
                        "{}{}",
                        " ".repeat(depth * INDENTATION.0),
                        constructor.display(context)
                    )?;
                }
                Ok(())
            }
            None => writeln!(
                f,
                "data {}: {}",
                type_.binder,
                type_.type_annotation.display(context)
            ),
        },
        Constructor(constructor) => writeln!(
            f,
            "{}: {}",
            constructor.binder,
            constructor.type_annotation.display(context)
        ),
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.declarations {
                let depth = depth + 1;
                write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                format_declaration(declaration, depth, context, f)?;
            }
            Ok(())
        }
        Use(use_) => match &use_.binder {
            Some(binder) => writeln!(f, "use {} as {}", use_.target, binder),
            None => writeln!(f, "use {}", use_.target),
        },
        Error => writeln!(f, "?(error)"),
    }
}

impl<C: DisplayContext> Display<C> for Expression {
    type Output<'a> = impl fmt::Display + 'a where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| format_pi_type_literal_or_lower(self, context, f))
    }
}

fn format_pi_type_literal_or_lower(
    expression: &Expression,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    // In here, we format `Lambda`, `UseIn` and `CaseAnalysis` as a pi-type-literal-or-lower instead of
    // a lowered expression — which you might have expected from reading through the grammar and the parser.
    // The reason for this is the way we treat bracketed expressions: We do not represent them in the AST
    // (as their own nodes).
    // We could add more checks in the implementation of the pretty-printer since we "lost" information.
    // But actually, we do not need extra checks if we use a different grammar from the parser's.
    // Note that, syntactically, applications in lushui are so flexible that they actually
    // allow bare complex expressions as arguments e.g. `call \x => x` and `call do pure unit`.
    // Haskell adopted this change at some point, too, with the extension `BlockArguments`.
    // Read https://typeclasses.com/ghc/block-arguments and/or
    // https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/block_arguments.html?highlight=xblockarguments.
    // This leads to the phenomenon that `(read \this => this) alpha` and `read (\this => this) alpha`
    // parse to the identical AST modulo spans.
    // However for pretty-printing, we only use the last form — `read (\this => this) alpha` — to avoid
    // extra checks. For the case above, this decision is neither liberal nor conservate resulting in
    // an equal amount of brackets (that being one). This is not the case for `topmost.take (\it => it)` which
    // we *might* want to print as `topmost.take \it => it`. This would probably require passing some flags to
    // the formatting functions and adding more checks.
    //
    // See also `crate::syntax::parser::test::application_lambda_literal_argument_{lax,strict}_grouping` and the
    // comment at the grammar definition of `Pi-Type-Literal-Or-Lower` (in `/misc/grammar/lushui.grammar`)
    // for further details.
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
                    write!(f, "{parameter}: ")?;
                }

                write!(f, "{}", pi.domain.display(context))?;
                write!(f, ")")?;
            } else {
                format_application_or_lower(&pi.domain, context, f)?;
            }
            write!(f, " -> ")?;
            format_pi_type_literal_or_lower(&pi.codomain, context, f)
        }
        Lambda(lambda) => {
            write!(f, r"\{}", lambda.explicitness)?;
            let parameter_needs_brackets =
                lambda.parameter_type_annotation.is_some() || lambda.laziness.is_some();

            if parameter_needs_brackets {
                write!(f, "(")?;
                if lambda.laziness.is_some() {
                    write!(f, "lazy ")?;
                }
                write!(f, "{}", lambda.parameter)?;
                if let Some(annotation) = &lambda.parameter_type_annotation {
                    write!(f, ": {}", annotation.display(context))?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(f, ": {}", annotation.display(context))?;
            }

            write!(f, " => {}", lambda.body.display(context))
        }
        UseIn => todo!(),
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            writeln!(f, "case {} of", analysis.scrutinee.display(context))?;
            for case in &analysis.cases {
                writeln!(
                    f,
                    "{} => {}",
                    case.pattern.display(context),
                    case.body.display(context)
                )?;
            }
            Ok(())
        }
        _ => format_application_or_lower(expression, context, f),
    }
}

// @Task write named arguments
fn format_application_or_lower(
    expression: &Expression,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    match &expression.bare {
        Application(application) => {
            format_application_or_lower(&application.callee, context, f)?;
            write!(f, " {}", application.explicitness)?;
            format_lower_expression(&application.argument, context, f)
        }
        IntrinsicApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                format_lower_expression(argument, context, f)?;
            }

            Ok(())
        }
        _ => format_lower_expression(expression, context, f),
    }
}

fn format_lower_expression(
    expression: &Expression,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BareExpression::*;

    for attribute in &expression.attributes.0 {
        write!(f, "{} ", attribute)?;
    }

    match &expression.bare {
        Type => write!(f, "Type"),
        Number(literal) => write!(f, "{literal}"),
        Text(literal) => write!(f, "{literal}"),
        Binding(binding) => write!(f, "{}", context.path_to_string(&binding.0)),
        // @Beacon @Temporary @Task just write out the path
        Projection(_projection) => write!(f, "?(projection)"),
        IO(io) => write!(
            f,
            "?(io {} {})",
            io.index,
            io.arguments
                .iter()
                .map(|argument| argument.display(context))
                .join_with(' ')
        ),
        Substituted(substituted) => write!(
            f,
            "?(substituted {} {})",
            substituted.substitution.display(context),
            substituted.expression.display(context)
        ),
        Error => write!(f, "?(error)"),
        _ => write!(f, "({})", expression.display(context)),
    }
}

impl<C: DisplayContext> Display<C> for Pattern {
    type Output<'a> = impl fmt::Display + 'a where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| format_pattern(self, context, f))
    }
}

// @Task @Beacon update bracket business
fn format_pattern(
    pattern: &Pattern,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::BarePattern::*;

    match &pattern.bare {
        Number(number) => write!(f, "{number}"),
        Text(text) => write!(f, "{text}"),
        Binding(binding) => write!(f, "{}", context.path_to_string(&binding.0)),
        Binder(binder) => write!(f, "\\{}", binder.0),
        Application(application) => write!(
            f,
            "({}) ({})",
            application.callee.display(context),
            application.argument.display(context)
        ),
        Error => write!(f, "?(error)"),
    }
}

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

impl fmt::Display for super::Text {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // @Task don't use Rust's Debug impl for str!
            super::Text::Text(text) => write!(f, "{text:?}"),
        }
    }
}

impl<C: DisplayContext> Display<C> for super::Substitution {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| format_substitution(self, context, f))
    }
}

fn format_substitution(
    substitution: &super::Substitution,
    context: impl DisplayContext,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use super::Substitution::*;

    match substitution {
        Shift(amount) => write!(f, "shift {}", amount),
        Use(substitution, expression) => write!(
            f,
            "{}[{}]",
            expression.display(context),
            substitution.display(context)
        ),
    }
}

impl fmt::Debug for super::Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            self.binders
                .iter()
                .map(|binding| format!("{binding:?}"))
                .join_with(' ')
        )
    }
}

impl fmt::Debug for super::Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "*"),
            Self::Restricted(reach) => write!(f, "{:?}", reach.lock().unwrap()),
        }
    }
}

impl<C: DisplayContext> Display<C> for super::Exposure {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => write!(f, "‘{}’", reach.lock().unwrap().display(context)),
        })
    }
}

impl fmt::Debug for super::ExposureReach {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
            Self::Resolved(reach) => write!(f, "{reach:?}"),
        }
    }
}

impl<C: DisplayContext> Display<C> for super::ExposureReach {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| match self {
            Self::Resolved(reach) => {
                // write!(f, "{}", context.path_to_string(reach.global(todo!())))
                todo!() // @Beacon @Beacon @Beacon @Task
            }
            // should not be reachable
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
        })
    }
}

impl fmt::Debug for super::PartiallyResolvedPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}.{}", self.namespace, self.path)
    }
}

impl<C: DisplayContext> Display<C> for super::ValueView {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| match self {
            Self::Reducible(expression) => {
                write!(f, "?(reducible {})", expression.display(context))
            }
            Self::Neutral => write!(f, "?(neutral)"),
        })
    }
}
