use super::{
    declaration_id,
    node::{Attributable, Element},
};
use crate::{
    component::Component,
    hir::{self, DeclarationIndex},
    session::BuildSession,
    utility::DisplayWith,
};
use joinery::JoinableIterator;
use std::iter::once;

pub(super) fn format_expression(
    expression: &hir::Expression,
    url_prefix: &str,
    component: &Component,
    session: &BuildSession,
) -> String {
    let mut formatter = Formatter::new(url_prefix, component, session);
    formatter.format_expression(expression);
    formatter.finish()
}

pub(super) fn declaration_url_fragment(
    index: DeclarationIndex,
    component: &Component,
    session: &BuildSession,
) -> String {
    Formatter::new("./", component, session).declaration_url_fragment(index)
}

struct Formatter<'a> {
    url_prefix: &'a str,
    component: &'a Component,
    session: &'a BuildSession,
    output: String,
}

impl<'a> Formatter<'a> {
    fn new(url_prefix: &'a str, component: &'a Component, session: &'a BuildSession) -> Self {
        Self {
            url_prefix,
            component,
            session,
            output: String::new(),
        }
    }

    fn write(&mut self, content: &str) {
        self.output += content;
    }

    fn finish(self) -> String {
        self.output
    }

    fn format_expression(&mut self, expression: &hir::Expression) {
        self.format_pi_type_literal_or_lower(expression);
    }

    fn format_pi_type_literal_or_lower(&mut self, expression: &hir::Expression) {
        use hir::ExpressionKind::*;

        match &expression.bare {
            PiType(pi) => {
                self.write(&pi.explicitness.to_string());

                // @Note fragile
                let domain_needs_brackets = pi.parameter.is_some() || pi.laziness.is_some();

                // @Task add tests to check if parameter aspect is handled correctly
                if domain_needs_brackets {
                    self.write("(");
                    if pi.laziness.is_some() {
                        self.write("lazy ");
                    }

                    if let Some(parameter) = &pi.parameter {
                        self.write(&parameter.to_string());
                        self.write(": ");
                    }

                    self.format_expression(&pi.domain);
                    self.write(")");
                } else {
                    self.format_application_or_lower(&pi.domain);
                }
                self.write(" -&gt; ");
                self.format_pi_type_literal_or_lower(&pi.codomain);
            }
            Lambda(lambda) => {
                self.write(r"\");
                self.write(&lambda.explicitness.to_string());
                let parameter_needs_brackets =
                    lambda.parameter_type_annotation.is_some() || lambda.laziness.is_some();

                if parameter_needs_brackets {
                    self.write("(");
                    if lambda.laziness.is_some() {
                        self.write("lazy ");
                    }
                    self.write(&lambda.parameter.to_string());
                    if let Some(annotation) = &lambda.parameter_type_annotation {
                        self.write(": ");
                        self.format_expression(annotation);
                    }
                    self.write(")");
                } else {
                    self.write(&lambda.parameter.to_string());
                }

                if let Some(annotation) = &lambda.body_type_annotation {
                    self.write(": ");
                    self.format_expression(annotation);
                }

                self.write(" =&gt; ");
                self.format_expression(&lambda.body);
            }
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                self.write("case ");
                self.format_expression(&analysis.scrutinee);
                self.write(" of {");

                // @Task spacing
                for case in &analysis.cases {
                    self.format_pattern(&case.pattern);
                    self.write(" =&gt; ");
                    self.format_expression(&case.body);
                    self.write(";");
                }

                self.write("}");
            }
            _ => self.format_application_or_lower(expression),
        }
    }

    // @Task write named arguments
    fn format_application_or_lower(&mut self, expression: &hir::Expression) {
        use hir::ExpressionKind::*;

        match &expression.bare {
            Application(application) => {
                self.format_application_or_lower(&application.callee);
                self.write(" ");
                self.write(&application.explicitness.to_string());
                self.format_lower_expression(&application.argument);
            }
            IntrinsicApplication(application) => {
                self.write(&application.callee.to_string());

                for argument in &application.arguments {
                    self.write(" ");
                    self.format_lower_expression(argument);
                }
            }
            _ => self.format_lower_expression(expression),
        }
    }

    fn format_lower_expression(&mut self, expression: &hir::Expression) {
        use hir::ExpressionKind::*;

        for attribute in &expression.attributes.0 {
            self.write(&attribute.to_string());
            self.write(" ");
        }

        match &expression.bare {
            Type => {
                Element::new("a")
                    .attribute(
                        "href",
                        format!("{}reserved.html#word.Type", self.url_prefix),
                    )
                    .child("Type")
                    .render(&mut self.output);
            }
            Number(literal) => self.write(&literal.to_string()),
            Text(literal) => self.write(&literal.to_string()),
            Binding(binding) => self.format_binder(&binding.0),
            // @Beacon @Temporary @Task just write out the path
            Projection(_projection) => self.write("?(projection)"),
            IO(io) => {
                self.write("?(io ");
                self.write(&io.index.to_string());
                self.write(")");

                for argument in &io.arguments {
                    self.write(" ");
                    self.format_expression(argument);
                }
            }
            Substitution(substitution) => {
                self.write("?(substitution ");
                self.write(
                    &substitution
                        .substitution
                        .with((self.component, self.session))
                        .to_string(),
                );
                self.format_expression(&substitution.expression);
            }
            Error => self.write("?(error)"),
            _ => {
                self.write("(");
                self.format_expression(expression);
                self.write(")");
            }
        }
    }

    // @Task @Beacon update bracket business
    fn format_pattern(&mut self, pattern: &hir::Pattern) {
        use hir::PatternKind::*;

        match &pattern.bare {
            Number(number) => self.write(&number.to_string()),
            Text(text) => self.write(&text.to_string()),
            Binding(binding) => self.format_binder(&binding.0),
            Binder(binder) => {
                self.write(r"\");
                self.write(&binder.0.to_string());
            }
            Application(application) => {
                self.write("("); // @Temporary
                self.format_pattern(&application.callee);
                self.write(")"); // @Temporary
                self.write("("); // @Temporary
                self.format_pattern(&application.argument);
                self.write(")"); // @Temporary
            }
            Error => self.write("?(error)"),
        }
    }

    fn module_url_fragment(&self, index: DeclarationIndex) -> String {
        let component = self.component(index);

        format!(
            "{}{}/index.html",
            self.url_prefix,
            once(component.folder_id().into())
                .chain(
                    component
                        .local_path_segments(index.local_unchecked())
                        .into_iter()
                        .map(urlencoding::encode)
                )
                .join_with("/")
        )
    }

    fn declaration_url_fragment(&self, index: DeclarationIndex) -> String {
        use crate::entity::EntityKind::*;

        let binder = self.look_up(index).source.as_str();

        match self.look_up(index).kind {
            Use { .. } => "#".to_string(), // @Task
            Module { .. } => self.module_url_fragment(index),
            Function { .. } | IntrinsicFunction { .. } | DataType { .. } => {
                let module_link = self.module_url_fragment(self.parent(index).unwrap());
                format!("{module_link}#{}", declaration_id(binder))
            }
            Constructor { .. } => {
                let data_type = self.parent(index).unwrap();
                let module_link = self.module_url_fragment(self.parent(data_type).unwrap());

                format!(
                    "{module_link}#{}",
                    declaration_id(&format!("{}.{}", self.look_up(data_type).source, binder))
                )
            }
            _ => unreachable!(),
        }
    }

    fn format_binder(&mut self, binder: &hir::Identifier) {
        if let Some(index) = binder.declaration_index() {
            let declaration_url = self.declaration_url_fragment(index);

            // @Task don't use path_to_string for this but don't use extern_path_to_string either
            // (just something without the `extern`/`core` prefix)
            let path = self.component.path_to_string(index, self.session);

            Element::new("a")
                .attribute("href", declaration_url)
                .attribute("title", path)
                .child(binder.to_string())
                .render(&mut self.output);
        } else {
            self.write(&binder.to_string());
        }
    }

    // @Task move look_up{_parent} to some utility module (one which offers functions relying on &Component together with &BuildSession)
    // @Note look_up is copy-pasted from Resolver::_

    fn component(&self, index: DeclarationIndex) -> &Component {
        if index.is_local(self.component) {
            self.component
        } else {
            &self.session[index.component()]
        }
    }

    // @Task dedup with code generator, name resolver, interpreter
    fn look_up(&self, index: DeclarationIndex) -> &crate::entity::Entity {
        match index.local(self.component) {
            Some(index) => &self.component[index],
            None => &self.session[index],
        }
    }

    fn parent(&self, index: DeclarationIndex) -> Option<DeclarationIndex> {
        match index.local(self.component) {
            Some(index) => self.component[index]
                .parent
                .map(|parent| parent.global(self.component)),
            None => self.session[index]
                .parent
                .map(|parent| DeclarationIndex::new(index.component(), parent)),
        }
    }
}
