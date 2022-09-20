//! The definition of the textual representation of the [HIR](hir).
#![feature(default_free_fn, associated_type_defaults)]

use colored::Colorize;
use joinery::JoinableIterator;
use session::{BuildSession, Component, DeclarationIndexExt, LocalDeclarationIndexExt};
use std::{collections::VecDeque, fmt};
use token::INDENTATION;

#[cfg(test)]
mod test;

pub type DefaultContext<'a> = (&'a Component, &'a BuildSession);

pub trait Display {
    type Context<'a> = DefaultContext<'a>;

    fn write(&self, context: Self::Context<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Display for hir::Declaration {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_declaration(self, 0, context, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
fn write_declaration(
    declaration: &hir::Declaration,
    depth: usize,
    context: <hir::Declaration as Display>::Context<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareDeclaration::*;

    match &declaration.bare {
        Function(function) => {
            write!(f, "{}: ", function.binder)?;
            function.type_annotation.write(context, f)?;
            if let Some(expression) = &function.expression {
                write!(f, " = ")?;
                expression.write(context, f)?;
            }
            writeln!(f)
        }
        Data(type_) => {
            write!(f, "data {}: ", type_.binder)?;
            type_.type_annotation.write(context, f)?;

            if let Some(constructors) = &type_.constructors {
                writeln!(f, " of")?;

                for constructor in constructors {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                    constructor.write(context, f)?;
                }
            }

            Ok(())
        }
        Constructor(constructor) => {
            write!(f, "{}: ", constructor.binder)?;
            constructor.type_annotation.write(context, f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.declarations {
                let depth = depth + 1;
                write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                write_declaration(declaration, depth, context, f)?;
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

impl Display for hir::Expression {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pi_type_literal_or_lower(self, context, f)
    }
}

fn write_pi_type_literal_or_lower(
    expression: &hir::Expression,
    context: DefaultContext<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

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

                pi.domain.write(context, f)?;
                write!(f, ")")?;
            } else {
                write_application_or_lower(&pi.domain, context, f)?;
            }
            write!(f, " -> ")?;
            write_pi_type_literal_or_lower(&pi.codomain, context, f)
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
                    write!(f, ": ")?;
                    annotation.write(context, f)?;
                }
                write!(f, ")")?;
            } else {
                write!(f, "{}", lambda.parameter)?;
            }

            if let Some(annotation) = &lambda.body_type_annotation {
                write!(f, ": ")?;
                annotation.write(context, f)?;
            }

            write!(f, " => ")?;
            lambda.body.write(context, f)
        }
        UseIn => todo!(),
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            write!(f, "case ")?;
            analysis.scrutinee.write(context, f)?;
            writeln!(f, " of")?;
            for case in &analysis.cases {
                case.pattern.write(context, f)?;
                writeln!(f, " => ")?;
                case.body.write(context, f)?;
            }
            Ok(())
        }
        _ => write_application_or_lower(expression, context, f),
    }
}

// @Task write named arguments
fn write_application_or_lower(
    expression: &hir::Expression,
    context: DefaultContext<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

    match &expression.bare {
        Application(application) => {
            write_application_or_lower(&application.callee, context, f)?;
            write!(f, " {}", application.explicitness)?;
            write_lower_expression(&application.argument, context, f)
        }
        IntrinsicApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                write_lower_expression(argument, context, f)?;
            }

            Ok(())
        }
        _ => write_lower_expression(expression, context, f),
    }
}

fn write_lower_expression(
    expression: &hir::Expression,
    context @ (component, session): DefaultContext<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

    for attribute in &expression.attributes.0 {
        write!(f, "{} ", attribute)?;
    }

    match &expression.bare {
        Number(literal) => write!(f, "{literal}"),
        Text(literal) => write!(f, "{literal}"),
        Binding(binding) => write!(f, "{}", component.binder_to_path(&binding.0, session)),
        // @Beacon @Temporary @Task just write out the path
        Projection(_projection) => write!(f, "?(projection)"),
        IO(io) => {
            // @Temporary format
            write!(f, "?(io {}", io.index)?;
            for argument in &io.arguments {
                write!(f, " ")?;
                argument.write(context, f)?;
            }
            write!(f, ")")
        }
        Substituted(substituted) => {
            write!(f, "?(substituted ",)?;
            substituted.substitution.write(context, f)?;
            write!(f, " ")?;
            substituted.expression.write(context, f)?;
            write!(f, ")")
        }
        Error => write!(f, "?(error)"),
        _ => {
            write!(f, "(")?;
            expression.write(context, f)?;
            write!(f, ")")
        }
    }
}

// @Beacon @Task respect / incorporate precedence for a prettier output (just like we do with expressions)
impl Display for hir::Pattern {
    fn write(
        &self,
        context @ (component, session): DefaultContext<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        use hir::BarePattern::*;

        match &self.bare {
            Number(number) => write!(f, "{number}"),
            Text(text) => write!(f, "{text}"),
            Binding(binding) => write!(f, "{}", component.binder_to_path(&binding.0, session)),
            Binder(binder) => write!(f, "\\{}", binder.0),
            Application(application) => {
                write!(f, "(")?;
                application.callee.write(context, f)?;
                write!(f, ") (")?;
                application.argument.write(context, f)?;
                write!(f, ")")
            }
            Error => write!(f, "?(error)"),
        }
    }
}

impl Display for hir::Substitution {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shift(amount) => write!(f, "shift {}", amount),
            Self::Use(substitution, expression) => {
                expression.write(context, f)?;
                write!(f, "[")?;
                substitution.write(context, f)?;
                write!(f, "]")
            }
        }
    }
}

impl Display for hir::Exposure {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => {
                write!(f, "‘")?;
                reach.lock().unwrap().write(context, f)?;
                write!(f, "’")
            }
        }
    }
}

impl Display for hir::ExposureReach {
    fn write(
        &self,
        (component, session): DefaultContext<'_>,
        f: &mut fmt::Formatter<'_>,
    ) -> fmt::Result {
        match self {
            Self::Resolved(reach) => {
                let reach = reach.global(component);
                write!(f, "{}", component.index_to_path(reach, session))
            }
            // should not be reachable
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
        }
    }
}

impl Display for hir::ValueView {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reducible(expression) => {
                write!(f, "?(reducible ")?;
                expression.write(context, f)?;
                write!(f, ")")
            }
            Self::Neutral => write!(f, "?(neutral)"),
        }
    }
}

impl Display for hir::Entity {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Task improve output for overly long lines (when the identifier is too long or more
        // importantly when the entity kind (esp. expressions within it) are big)

        let parent = self
            .parent
            .map(|parent| format!("{parent:?}."))
            .unwrap_or_default()
            .bright_black();
        let source = self.source.to_string().bright_red().bold();
        let path = format!("{parent}{source}");
        let exposure = format!("{:?}<", self.exposure).bright_black();
        write!(f, "{exposure:>5}   {path:<40} ↦ ")?;
        self.kind.write(context, f)
    }
}

impl Display for hir::EntityKind {
    fn write(&self, context: DefaultContext<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use hir::EntityKind::*;

        write!(f, "{:>19}   ", self.precise_name().bright_blue())?;

        match self {
            Module { namespace } | UntypedDataType { namespace } => write!(f, "{namespace:?}"),
            Use { target } => write!(f, "{target:?}"),
            Function { type_, expression } => {
                match expression {
                    Some(expression) => expression.write(context, f),
                    None => write!(f, "?(none)"),
                }?;

                write!(f, " : ")?;
                type_.write(context, f)
            }
            DataType {
                type_,
                constructors,
                ..
            } => {
                type_.write(context, f)?;

                if !constructors.is_empty() {
                    write!(f, "; {}", constructors.iter().join_with(' '))?;
                }

                Ok(())
            }
            Constructor { type_ } | IntrinsicFunction { type_, .. } => type_.write(context, f),
            _ => Ok(()),
        }
    }
}

pub trait ComponentExt {
    /// The textual representation of the path of the given binding.
    ///
    /// If the binding is local meaning it represents a function parameter or a binder in a pattern,
    /// this method will return a single identifier that could be used in the corresponding function or
    /// case analysis case to refer to it until the point it gets shadowed (if any) but not outside of
    /// that environment.
    ///
    /// If it's not local, this method will return [the path relative to the root of the current
    /// component][Self::index_to_path].
    ///
    /// # Example Output
    ///
    /// * `x`
    /// * `alpha`
    /// * `topmost`
    /// * `topmost.alpha`
    /// * `topmost.gamma.<?//`
    /// * `extern.core`
    /// * `extern.core.nat.Nat`
    fn binder_to_path(&self, binder: &hir::Identifier, session: &BuildSession) -> String;

    /// The textual representation of the path of the given binding relative to the root of the current component.
    ///
    /// Rephrased it returns a path that could be used in any expression in any module of the current component
    /// to unambiguously (including shadowing) refer to the given binding ignoring exposure.
    /// If the binding is defined in this component, it will always start with the path hanger `topmost`,
    /// otherwise it will start with `extern` followed by the respective name of the external component.
    ///
    /// # Example Output
    ///
    /// * `topmost`
    /// * `topmost.alpha`
    /// * `topmost.gamma.<?//`
    /// * `extern.core`
    /// * `extern.core.nat.Nat`
    fn index_to_path(&self, index: hir::DeclarationIndex, session: &BuildSession) -> String;

    // @Task make this private
    fn index_with_root_to_path(
        &self,
        index: hir::DeclarationIndex,
        root: String,
        session: &BuildSession,
    ) -> String;

    /// The textual representation of the path to the given binding relative to a component root
    /// prefixed with name of the corresponding component.
    ///
    /// Rephrased, it returns a path that could be used in any dependent components (reverse dependencies)
    /// to refer to the binding ignoring exposure as long as one would prepend the path hanger `extern`.
    ///
    /// # Example Output
    ///
    /// * `core.nat.Nat` (`core` referring to a component)
    /// * `json.parse` (`json` referring to a component)
    fn local_index_with_root_to_extern_path(
        &self,
        index: hir::LocalDeclarationIndex,
        root: String,
    ) -> String;

    // @Task add documentation
    fn local_index_to_path_segments(&self, index: hir::LocalDeclarationIndex) -> VecDeque<&str>;
}

impl ComponentExt for Component {
    fn binder_to_path(&self, binder: &hir::Identifier, session: &BuildSession) -> String {
        use hir::Index::*;

        match binder.index {
            Declaration(index) => self.index_to_path(index, session),
            DeBruijn(_) | DeBruijnParameter => binder.to_string(),
        }
    }

    fn index_to_path(&self, index: hir::DeclarationIndex, session: &BuildSession) -> String {
        self.index_with_root_to_path(index, ast::BareHanger::Topmost.name().to_owned(), session)
    }

    fn index_with_root_to_path(
        &self,
        index: hir::DeclarationIndex,
        root: String,
        session: &BuildSession,
    ) -> String {
        match index.local(self) {
            Some(index) => self.local_index_with_root_to_extern_path(index, root),
            None => {
                let component = &session[index.component()];
                let root = format!("{}.{}", ast::BareHanger::Extern.name(), component.name());

                component.index_with_root_to_path(index, root, session)
            }
        }
    }

    fn local_index_with_root_to_extern_path(
        &self,
        index: hir::LocalDeclarationIndex,
        root: String,
    ) -> String {
        let entity = &self[index];
        // @Task rewrite this recursive approach to an iterative one!
        if let Some(parent) = entity.parent {
            let mut parent_path = self.local_index_with_root_to_extern_path(parent, root);

            let parent_is_symbol = token::is_symbol(parent_path.chars().next_back().unwrap());

            if parent_is_symbol {
                parent_path.push(' ');
            }

            parent_path.push('.');

            if entity.source.is_symbol() && parent_is_symbol {
                parent_path.push(' ');
            }

            parent_path += entity.source.as_str();
            parent_path
        } else {
            root
        }
    }

    fn local_index_to_path_segments(
        &self,
        mut index: hir::LocalDeclarationIndex,
    ) -> VecDeque<&str> {
        let mut segments = VecDeque::new();

        while let Some(parent) = self[index].parent {
            segments.push_front(self[index].source.as_str());
            index = parent;
        }

        segments
    }
}

impl Display for Component {
    type Context<'a> = &'a BuildSession;

    fn write(&self, session: &BuildSession, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {} ({:?})", self.type_(), self.name(), self.index())?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in &self.bindings {
            write!(f, "    {}: ", format!("{index:?}").red())?;
            entity.write((self, session), f)?;
            writeln!(f)?;
        }

        Ok(())
    }
}
