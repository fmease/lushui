use crate::{format::DisplayWith, hir, package::BuildSession, resolver::Crate};
use std::default::default;

pub(super) fn format_expression(
    expression: &hir::Expression,
    crate_: &Crate,
    session: &BuildSession,
) -> String {
    let mut formatter = Formatter::new(crate_, session);
    formatter.format_expression(expression);
    formatter.finish()
}

struct Formatter<'a> {
    crate_: &'a Crate,
    session: &'a BuildSession,
    output: String,
}

impl<'a> Formatter<'a> {
    fn new(crate_: &'a Crate, session: &'a BuildSession) -> Self {
        Self {
            crate_,
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
        self.format_pi_type_literal_or_lower(expression)
    }

    fn format_pi_type_literal_or_lower(&mut self, expression: &hir::Expression) {
        use hir::ExpressionKind::*;

        match &expression.value {
            PiType(pi) => {
                self.write(&pi.explicitness.to_string());

                // @Note fragile
                let domain_needs_brackets = pi.parameter.is_some() || pi.aspect != default();

                // @Task add tests to check if parameter aspect is handled correctly
                if domain_needs_brackets {
                    self.write("(");
                    self.write(&pi.aspect.to_string());

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

                self.write(" => ");
                self.format_expression(&lambda.body);
            }
            UseIn => todo!(),
            // @Task fix indentation
            CaseAnalysis(analysis) => {
                self.write("case ");
                self.format_expression(&analysis.subject);
                self.write(" of {");

                // @Task spacing
                for case in &analysis.cases {
                    self.format_pattern(&case.pattern);
                    self.write(" => ");
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

        match &expression.value {
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

        for attribute in expression.attributes.iter() {
            self.write(&attribute.to_string());
            self.write(" ");
        }

        match &expression.value {
            Type => self.write("Type"),
            Number(literal) => self.write(&literal.to_string()),
            // @Task use custom escaping logic
            Text(literal) => self.write(&format!("{literal:?}")),
            Binding(binding) => self.format_binder(&binding.binder),
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
                        .with((self.crate_, self.session))
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

        match &pattern.value {
            Number(number) => self.write(&number.to_string()),
            // @Task write custom escaper
            Text(text) => self.write(&format!("{:?}", text)),
            Binding(binding) => self.format_binder(&binding.binder),
            Binder(binder) => {
                self.write(r"\");
                self.write(&binder.binder.to_string());
            }
            Deapplication(application) => {
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

    fn format_binder(&mut self, binder: &hir::Identifier) {
        if let Some(index) = binder.declaration_index() {
            let path = self.crate_.absolute_path_to_string(index, self.session);

            self.write(r##"<a href="#" class="reference" title=""##);
            self.write(&path);
            self.write(r#"">"#);
            self.write(&binder.to_string());
            self.write("</a>");
        } else {
            self.write(&binder.to_string());
        }
    }
}
