//! The definition of the textual representation of the [HIR](hir).
#![feature(associated_type_defaults)]

use colored::Colorize;
use joinery::JoinableIterator;
use lexer::{token::INDENTATION, CharExt};
use session::{
    component::{Component, DeclarationIndexExt, LocalDeclarationIndexExt},
    Session,
};
use std::{collections::VecDeque, fmt};
use utility::Atom;

#[cfg(test)]
mod test;

pub trait Display {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result;
}

impl Display for hir::Declaration {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_declaration(self, 0, session, f)
    }
}

// @Task reduce amount of (String) allocations
// @Bug indentation not correctly handled
// @Task display attributes
fn write_declaration(
    declaration: &hir::Declaration,
    depth: usize,
    session: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareDeclaration::*;

    match &declaration.bare {
        Function(function) => {
            write!(f, "{}: ", function.binder)?;
            function.type_.write(session, f)?;
            if let Some(body) = &function.body {
                f.write_str(" = ")?;
                body.write(session, f)?;
            }
            writeln!(f)
        }
        Data(type_) => {
            write!(f, "data {}: ", type_.binder)?;
            type_.type_.write(session, f)?;

            if let Some(constructors) = &type_.constructors {
                writeln!(f, " of")?;

                for constructor in constructors {
                    let depth = depth + 1;
                    write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                    constructor.write(session, f)?;
                }
            } else {
                writeln!(f)?;
            }

            Ok(())
        }
        Constructor(constructor) => {
            write!(f, "{}: ", constructor.binder)?;
            constructor.type_.write(session, f)?;
            writeln!(f)
        }
        Module(module) => {
            writeln!(f, "module {} of", module.binder)?;
            for declaration in &module.declarations {
                let depth = depth + 1;
                write!(f, "{}", " ".repeat(depth * INDENTATION.0))?;
                write_declaration(declaration, depth, session, f)?;
            }
            Ok(())
        }
        Use(use_) => match &use_.binder {
            Some(binder) => writeln!(f, "use {} as {binder}", use_.target),
            None => writeln!(f, "use {}", use_.target),
        },
        Error(_) => writeln!(f, "⟨error⟩"),
    }
}

impl Display for hir::Expression {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write_pi_type_literal_or_lower(self, session, f)
    }
}

fn write_pi_type_literal_or_lower(
    expression: &hir::Expression,
    session: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

    // In here, we format `Lambda` and `CaseAnalysis` as a pi-type-literal-or-lower instead of
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
            if pi.binder.is_none() && pi.kind == hir::ParameterKind::Explicit {
                write_application_or_lower(&pi.domain, session, f)?;
            } else {
                f.write_str("For ")?;

                if pi.kind == hir::ParameterKind::Implicit {
                    f.write_str("'")?;
                }
                if pi.kind == hir::ParameterKind::Context {
                    f.write_str("[")?;

                    if let Some(binder) = pi.binder {
                        write!(f, "{binder}: ")?;
                    }

                    pi.domain.write(session, f)?;
                    f.write_str("]")?;
                } else {
                    let binder = pi.binder.map_or(Atom::UNDERSCORE, hir::Identifier::bare);

                    write!(f, "({binder}: ")?;
                    pi.domain.write(session, f)?;
                    f.write_str(")")?;
                }
            }

            f.write_str(" -> ")?;
            write_pi_type_literal_or_lower(&pi.codomain, session, f)
        }
        Lambda(lambda) => {
            f.write_str("for ")?;

            if lambda.kind == hir::ParameterKind::Implicit {
                f.write_str("'")?;
            }
            if lambda.kind == hir::ParameterKind::Context {
                f.write_str("[")?;

                if let Some(binder) = lambda.binder {
                    write!(f, "{binder}: ")?;
                }

                // Although it's not statically guaranteed, the domain of context parameters has exist.
                // Let's not unwrap though for robustness.
                if let Some(domain) = &lambda.domain {
                    domain.write(session, f)?;
                }

                f.write_str("]")?;
            } else {
                let binder = lambda
                    .binder
                    .map_or(Atom::UNDERSCORE, hir::Identifier::bare);

                if let Some(domain) = &lambda.domain {
                    write!(f, "({binder}: ")?;
                    domain.write(session, f)?;
                    f.write_str(")")?;
                } else {
                    write!(f, "{binder}")?;
                }
            }

            if let Some(codomain) = &lambda.codomain {
                write!(f, ": ")?;
                codomain.write(session, f)?;
            }

            write!(f, " => ")?;
            lambda.body.write(session, f)
        }
        // @Task fix indentation
        CaseAnalysis(analysis) => {
            write!(f, "case ")?;
            analysis.scrutinee.write(session, f)?;
            writeln!(f, " of")?;
            for case in &analysis.cases {
                case.pattern.write(session, f)?;
                writeln!(f, " => ")?;
                case.body.write(session, f)?;
            }
            Ok(())
        }
        _ => write_application_or_lower(expression, session, f),
    }
}

// @Task write named arguments
fn write_application_or_lower(
    expression: &hir::Expression,
    session: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

    match &expression.bare {
        Application(application) => {
            write_application_or_lower(&application.callee, session, f)?;

            f.write_str(" ")?;

            if application.kind == hir::ParameterKind::Implicit {
                f.write_str("'")?;
            }
            if application.kind == hir::ParameterKind::Context {
                f.write_str("[")?;
                application.argument.write(session, f)?;
                f.write_str("]")
            } else {
                write_lower_expression(&application.argument, session, f)
            }
        }
        IntrinsicApplication(application) => {
            write!(f, "{}", application.callee)?;

            for argument in &application.arguments {
                write!(f, " ")?;
                write_lower_expression(argument, session, f)?;
            }

            Ok(())
        }
        _ => write_lower_expression(expression, session, f),
    }
}

fn write_lower_expression(
    expression: &hir::Expression,
    session: &Session<'_>,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    use hir::BareExpression::*;

    for attribute in &expression.attributes.0 {
        write!(f, "{attribute} ")?;
    }

    match &expression.bare {
        Number(literal) => write!(f, "{literal}"),
        Text(literal) => write!(f, "{literal}"),
        Binding(binding) => write!(f, "{}", session.binder_to_path(binding.0)),
        Projection(projection) => {
            write_lower_expression(&projection.basis, session, f)?;
            write!(f, "::{}", projection.field)
        }
        IO(io) => {
            // @Temporary format
            write!(f, "⟨io {}", io.index)?;
            for argument in &io.arguments {
                f.write_str(" ")?;
                argument.write(session, f)?;
            }
            f.write_str("⟩")
        }
        Substituted(substituted) => {
            f.write_str("⟨subst ")?;
            substituted.substitution.write(session, f)?;
            f.write_str(" ")?;
            substituted.expression.write(session, f)?;
            f.write_str("⟩")
        }
        Error(_) => write!(f, "⟨error⟩"),
        _ => {
            write!(f, "(")?;
            expression.write(session, f)?;
            write!(f, ")")
        }
    }
}

// @Beacon @Task respect / incorporate precedence for a prettier output (just like we do with expressions)
impl Display for hir::Pattern {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use hir::BarePattern::*;

        match &self.bare {
            Number(number) => write!(f, "{number}"),
            Text(text) => write!(f, "{text}"),
            Binding(binding) => write!(f, "{}", session.binder_to_path(binding.0)),
            LetBinding(binder) => write!(f, "(let {binder})"),
            Application(application) => {
                write!(f, "(")?;
                application.callee.write(session, f)?;
                write!(f, ") (")?;
                application.argument.write(session, f)?;
                write!(f, ")")
            }
            Error(_) => write!(f, "⟨error⟩"),
        }
    }
}

impl Display for hir::Substitution {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Shift(amount) => write!(f, "shift {amount}"),
            Self::Use(substitution, expression) => {
                expression.write(session, f)?;
                write!(f, "[")?;
                substitution.write(session, f)?;
                write!(f, "]")
            }
        }
    }
}

impl Display for hir::Exposure {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(reach) => {
                write!(f, "‘")?;
                reach.lock().unwrap().write(session, f)?;
                write!(f, "’")
            }
        }
    }
}

impl Display for hir::ExposureReach {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Resolved(reach) => {
                let reach = reach.global(session);
                write!(f, "{}", session.index_to_path(reach))
            }
            // should not be reachable
            Self::PartiallyResolved(reach) => write!(f, "{reach:?}"),
        }
    }
}

impl Display for hir::ValueView {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Reducible(expression) => {
                write!(f, "⟨reducible ")?;
                expression.write(session, f)?;
                write!(f, "⟩")
            }
            Self::Neutral => write!(f, "⟨neutral⟩"),
        }
    }
}

impl Display for hir::Entity {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
        self.kind.write(session, f)
    }
}

impl Display for hir::EntityKind {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use hir::EntityKind::*;

        write!(f, "{:>19}   ", self.precise_name().bright_blue())?;

        match self {
            Module { namespace } | UntypedDataType { namespace } => write!(f, "{namespace:?}"),
            Use { target } => write!(f, "{target:?}"),
            Function { type_, expression } => {
                match expression {
                    Some(expression) => expression.write(session, f),
                    None => write!(f, "⟨none⟩"),
                }?;

                write!(f, " : ")?;
                type_.write(session, f)
            }
            DataType {
                type_,
                constructors,
                ..
            } => {
                type_.write(session, f)?;

                if !constructors.is_empty() {
                    write!(f, "; {}", constructors.iter().join_with(' '))?;
                }

                Ok(())
            }
            Constructor { type_ } | IntrinsicFunction { type_, .. } => type_.write(session, f),
            _ => Ok(()),
        }
    }
}

pub trait SessionExt {
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
    fn binder_to_path(&self, binder: hir::Identifier) -> String;

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
    fn index_to_path(&self, index: hir::DeclarationIndex) -> String;

    fn local_index_to_path(&self, index: hir::LocalDeclarationIndex) -> String;
}

impl SessionExt for Session<'_> {
    fn binder_to_path(&self, binder: hir::Identifier) -> String {
        use hir::Index::*;

        match binder.index {
            Declaration(index) => self.index_to_path(index),
            DeBruijn(_) | Parameter => binder.to_string(),
        }
    }

    fn index_to_path(&self, index: hir::DeclarationIndex) -> String {
        let root = ast::BareHanger::Topmost.name().to_owned();

        self.component().index_with_root_to_path(index, root, self)
    }

    // @Question can we rewrite this function to not rely on Session?
    fn local_index_to_path(&self, index: hir::LocalDeclarationIndex) -> String {
        self.index_to_path(index.global(self))
    }
}

pub trait ComponentExt {
    fn index_with_root_to_path(
        &self,
        index: hir::DeclarationIndex,
        root: String,
        session: &Session<'_>,
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
    fn local_index_to_path_segments(&self, index: hir::LocalDeclarationIndex) -> VecDeque<Atom>;
}

impl ComponentExt for Component {
    fn index_with_root_to_path(
        &self,
        index: hir::DeclarationIndex,
        root: String,
        session: &Session<'_>,
    ) -> String {
        match index.local(self) {
            Some(index) => self.local_index_with_root_to_extern_path(index, root),
            None => {
                let component = &session.look_up_component(index.component());
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

            let parent_is_symbol = parent_path.chars().next_back().unwrap().is_symbol();

            if parent_is_symbol {
                parent_path.push(' ');
            }

            parent_path.push('.');

            if entity.source.is_symbol() && parent_is_symbol {
                parent_path.push(' ');
            }

            parent_path += entity.source.to_str();
            parent_path
        } else {
            root
        }
    }

    fn local_index_to_path_segments(
        &self,
        mut index: hir::LocalDeclarationIndex,
    ) -> VecDeque<Atom> {
        let mut segments = VecDeque::new();

        while let Some(parent) = self[index].parent {
            segments.push_front(self[index].source.bare());
            index = parent;
        }

        segments
    }
}

impl Display for Component {
    fn write(&self, session: &Session<'_>, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} ({:?})", self.name(), self.index())?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in &self.bindings {
            write!(f, "    {}: ", format!("{index:?}").red())?;
            entity.write(session, f)?;
            writeln!(f)?;
        }

        Ok(())
    }
}
