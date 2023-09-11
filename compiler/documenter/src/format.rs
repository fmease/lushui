use super::{
    declaration_id,
    node::{Attributable, Element},
};
use hir::DeclarationIndex;
use hir_format::{ComponentExt, Display, SessionExt};
use joinery::JoinableIterator;
use session::Session;
use std::fmt::Write;
use utility::{displayed, Atom};

pub(super) fn format_expression(
    expression: &hir::Expression,
    url_prefix: &str,
    session: &Session<'_>,
) -> String {
    let mut formatter = Formatter::new(url_prefix, session);
    formatter.format_expression(expression);
    formatter.finish()
}

pub(super) fn declaration_url_fragment(index: DeclarationIndex, session: &Session<'_>) -> String {
    Formatter::new("./", session).declaration_url_fragment(index)
}

struct Formatter<'a> {
    url_prefix: &'a str,
    session: &'a Session<'a>,
    output: String,
}

impl<'a> Formatter<'a> {
    fn new(url_prefix: &'a str, session: &'a Session<'a>) -> Self {
        Self {
            url_prefix,
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
        self.format_pi_type_or_lower(expression);
    }

    fn format_pi_type_or_lower(&mut self, expression: &hir::Expression) {
        use hir::BareExpression::*;

        match &expression.bare {
            PiType(pi) => {
                if pi.binder.is_none() && pi.kind == hir::ParameterKind::Explicit {
                    self.format_application_or_lower(&pi.domain);
                } else {
                    self.write("For ");

                    if pi.kind == hir::ParameterKind::Implicit {
                        self.write("'");
                    }
                    if pi.kind == hir::ParameterKind::Context {
                        self.write("[");

                        if let Some(binder) = pi.binder {
                            self.write(binder.to_str());
                            self.write(": ");
                        }

                        self.format_expression(&pi.domain);

                        self.write("]");
                    } else {
                        let binder = pi
                            .binder
                            .map_or(Atom::UNDERSCORE, hir::Identifier::bare)
                            .to_str();

                        self.write("(");
                        self.write(binder);
                        self.write(": ");
                        self.format_expression(&pi.domain);
                        self.write(")");
                    }
                }

                self.write(" -&gt; ");
                self.format_pi_type_or_lower(&pi.codomain);
            }
            Lambda(lambda) => {
                self.write("for ");
                if lambda.kind == hir::ParameterKind::Implicit {
                    self.write("'");
                }
                if lambda.kind == hir::ParameterKind::Context {
                    self.write("[");

                    if let Some(binder) = lambda.binder {
                        self.write(binder.to_str());
                        self.write(": ");
                    }

                    if let Some(domain) = &lambda.domain {
                        self.write(": ");
                        self.format_expression(domain);
                    }

                    self.write("]");
                } else {
                    let binder = lambda
                        .binder
                        .map_or(Atom::UNDERSCORE, hir::Identifier::bare)
                        .to_str();

                    if let Some(domain) = &lambda.domain {
                        self.write("(");
                        self.write(binder);
                        self.write(": ");
                        self.format_expression(domain);
                        self.write(")");
                    } else {
                        self.write(binder);
                    }
                }

                if let Some(codomain) = &lambda.codomain {
                    self.write(": ");
                    self.format_expression(codomain);
                }

                self.write(" =&gt; ");
                self.format_expression(&lambda.body);
            }
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
        use hir::BareExpression::*;

        match &expression.bare {
            Application(application) => {
                self.format_application_or_lower(&application.callee);
                self.write(" ");
                if application.kind == hir::ParameterKind::Implicit {
                    self.write("'");
                }
                if application.kind == hir::ParameterKind::Context {
                    self.write("[");
                    self.format_expression(&application.argument);
                    self.write("]");
                } else {
                    self.format_lower_expression(&application.argument);
                }
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
        use hir::BareExpression::*;

        for attribute in &expression.attributes.0 {
            self.write(&attribute.to_string());
            self.write(" ");
        }

        match &expression.bare {
            Number(literal) => self.write(&literal.to_string()),
            Text(literal) => self.write(&literal.to_string()),
            Binding(binding) => self.format_binder(&binding.0),
            // @Task
            Projection(_projection) => self.write("⟨projection⟩"),
            Record(_record) => self.write("⟨record⟩"),
            IO(io) => {
                self.write("⟨io ");
                self.write(&io.index.to_string());
                self.write("⟩");

                for argument in &io.arguments {
                    self.write(" ");
                    self.format_expression(argument);
                }
            }
            Substituted(substituted) => {
                self.write("⟨subst ");
                write!(
                    self.output,
                    "{}",
                    displayed(|f| substituted.substitution.write(self.session, f))
                )
                .unwrap();
                self.format_expression(&substituted.expression);
                self.write("⟩");
            }
            Error(_) => self.write("⟨error⟩"),
            _ => {
                self.write("(");
                self.format_expression(expression);
                self.write(")");
            }
        }
    }

    // @Task @Beacon update bracket business
    fn format_pattern(&mut self, pattern: &hir::Pattern) {
        use hir::BarePattern::*;

        match &pattern.bare {
            Number(number) => self.write(&number.to_string()),
            Text(text) => self.write(&text.to_string()),
            Binding(binding) => self.format_binder(&binding.0),
            LetBinding(binder) => {
                self.write("(let ");
                self.write(binder.to_str());
                self.write(")");
            }
            Application(application) => {
                self.write("("); // @Temporary
                self.format_pattern(&application.callee);
                self.write(")"); // @Temporary
                self.write("("); // @Temporary
                self.format_pattern(&application.argument);
                self.write(")"); // @Temporary
            }
            Error(_) => self.write("⟨error⟩"),
        }
    }

    fn module_url_fragment(&self, index: DeclarationIndex) -> String {
        let component = self.session.component_of(index);

        let mut segments = component.local_index_to_path_segments(index.local_unchecked());
        segments.push_front(component.name().into_inner());

        format!(
            "{}{}/index.html",
            self.url_prefix,
            segments
                .into_iter()
                .map(Atom::to_str)
                .map(urlencoding::encode)
                .join_with("/")
        )
    }

    fn declaration_url_fragment(&self, index: DeclarationIndex) -> String {
        use hir::EntityKind::*;

        let binder = self.session[index].source.to_str();

        match self.session[index].kind {
            Use { .. } => "#".to_string(), // @Task
            Module { .. } => self.module_url_fragment(index),
            Function { .. } | IntrinsicFunction { .. } | DataType { .. } => {
                let module_link = self.module_url_fragment(self.session.parent_of(index).unwrap());
                format!("{module_link}#{}", declaration_id(binder))
            }
            Constructor { .. } => {
                let data_type = self.session.parent_of(index).unwrap();
                let module_link =
                    self.module_url_fragment(self.session.parent_of(data_type).unwrap());

                format!(
                    "{module_link}#{}",
                    declaration_id(&format!("{}.{binder}", self.session[data_type].source))
                )
            }
            _ => unreachable!(),
        }
    }

    fn format_binder(&mut self, binder: &hir::Identifier) {
        if let Some(index) = binder.declaration_index() {
            let declaration_url = self.declaration_url_fragment(index);
            let path = self.session.index_to_path(index);

            Element::anchor(declaration_url, binder.to_str())
                .attribute("title", path)
                .render(&mut self.output);
        } else {
            self.write(&binder.to_string());
        }
    }
}
