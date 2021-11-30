//! The documentation generator.

use crate::{
    diagnostics::Reporter,
    error::Result,
    hir,
    package::BuildSession,
    resolver::Crate,
    syntax::lowered_ast::{self, AttributeKeys, AttributeKind},
    utility::obtain,
};
use std::{collections::VecDeque, default::default, fs, path::PathBuf};

const DOCUMENTATION_FOLDER_NAME: &str = "documentation";

const STYLE_SHEET_CONTENT: &str = include_str!("documenter/include/style.css");
const STYLE_SHEET_FILE_NAME: &str = "style.css";

pub struct Documenter<'a> {
    crate_: &'a Crate,
    session: &'a BuildSession,
    #[allow(dead_code)]
    reporter: &'a Reporter,
    pages: Vec<Page<'a>>,
    destination: PathBuf,
}

impl<'a> Documenter<'a> {
    pub fn new(crate_: &'a Crate, session: &'a BuildSession, reporter: &'a Reporter) -> Self {
        let destination = crate_
            .package(session)
            .build_path()
            .join(DOCUMENTATION_FOLDER_NAME);

        Self {
            crate_,
            session,
            reporter,
            pages: default(),
            destination,
        }
    }

    // @Question inline?
    fn module_folder_path(&self, path_segments: &VecDeque<&str>) -> PathBuf {
        let mut path = self.destination.clone();
        // @Bug modules names that are punctuation instead of words generally cannot
        // be used as paths on Windows! @Task implement an escaping logic!
        path.extend(path_segments);
        path
    }

    pub fn document(&mut self, declaration: &hir::Declaration) -> Result<()> {
        self.document_declaration(declaration)?;
        self.write()?;

        Ok(())
    }

    // @Task handle the case where two+ crates (goal and/or (transitive) deps)
    // have the same name and are being documented
    #[allow(clippy::unnecessary_wraps)] // @Temporary
    fn write(&self) -> Result<()> {
        // @Beacon @Task handle I/O errors properly

        if !self.destination.exists() {
            fs::create_dir_all(&self.destination).unwrap();
        }

        let style_sheet_path = self.destination.join(STYLE_SHEET_FILE_NAME);

        // if !style_sheet_path.exists() { // not when developing
        fs::write(style_sheet_path, STYLE_SHEET_CONTENT).unwrap();
        // }

        for page in &self.pages {
            let path = self.module_folder_path(&page.module_path_segments);

            if !path.exists() {
                fs::create_dir(&path).unwrap();
            }

            fs::write(path.join("index.html"), &page.content).unwrap();
        }

        Ok(())
    }

    fn document_declaration(&mut self, declaration: &hir::Declaration) -> Result<()> {
        if let hir::DeclarationKind::Module(module) = &declaration.value {
            self.generate_page(module, &declaration.attributes);

            for declaration in &module.declarations {
                self.document_declaration(declaration)?;
            }
        }

        Ok(())
    }

    fn generate_page(&mut self, module: &hir::Module, attributes: &lowered_ast::Attributes) {
        let index = module.binder.local_declaration_index(self.crate_).unwrap();
        let module_path_segments = self.crate_.local_path_segments(index);
        let module_path = self.crate_.display_local_path(index, self.session);

        let mut content = String::from(
            r#"<!doctype html><html lang="en"><head><meta charset="utf-8"><meta name="viewport" content="width=device-width, initial-scale=1.0">"#,
        );
        content += r#"<meta name="generator" content="lushui documenter "#;
        content += env!("CARGO_PKG_VERSION");
        content += r#"">"#;
        content += r#"<link rel="stylesheet" href=""#;
        content += &"../".repeat(module_path_segments.len());
        content += STYLE_SHEET_FILE_NAME;
        content += r#"">"#;
        let description = &self.crate_.package(self.session).description;
        if !description.is_empty() {
            content += r#"<meta name="description" content=""#;
            html_escape::encode_double_quoted_attribute_to_string(description, &mut content);
            content += r#"">"#;
        }
        content += "<title>";
        html_escape::encode_text_to_string(&module_path, &mut content);
        content += "</title>";

        content += "</head><body>";
        // div.content
        {
            content += r#"<div class="content">"#;

            // headline
            {
                content += "<h1>";
                content += if index == self.crate_.root() {
                    "Crate"
                } else {
                    "Module"
                };
                content += " ";
                for (position, path_segment) in module_path_segments.iter().enumerate() {
                    let last_segment = position + 1 == module_path_segments.len();

                    content += r#"<a href=""#;
                    content += &"../".repeat(module_path_segments.len() - 1 - position);
                    content += r#"index.html""#;
                    if last_segment {
                        content += r#" class="module""#;
                    }
                    content += r#">"#;
                    html_escape::encode_text_to_string(&path_segment, &mut content);
                    content += "</a>";

                    if !last_segment {
                        // @Task add extra spacing (left or right, depends) for
                        // non-word module names
                        content += ".<wbr>";
                    }
                }
                content += "</h1>";
            }

            // description
            {
                content += "<p>";
                // unrelated @Task, create a better attribute API! for real!
                // @Beacon @Task combine the lines and send the blob to asciidoctor!
                for line in attributes
                    .filter(AttributeKeys::DOCUMENTATION)
                    .map(|attribute| {
                        obtain!(
                            &attribute.value,
                            AttributeKind::Documentation { content } => content
                        )
                        .unwrap()
                    })
                {
                    html_escape::encode_text_to_string(line, &mut content);
                }
                content += "</p>";
            }

            // @Task re-exports

            // modules
            {
                let submodules: Vec<_> = module
                    .declarations
                    .iter()
                    .filter_map(|declaration| {
                        obtain!(
                            &declaration.value,
                            hir::DeclarationKind::Module(module) => module.binder.as_str()
                        )
                    })
                    .collect();

                if !submodules.is_empty() {
                    content += "<h2>Modules</h2>";
                    content += "<ul>";

                    for submodule in submodules {
                        content += r#"<li><a href=""#;
                        // @Task has to be url-encoded actually!!
                        html_escape::encode_quoted_attribute_to_string(submodule, &mut content);
                        content += r#"/index.html">"#;
                        html_escape::encode_text_to_string(submodule, &mut content);
                        content += "</a></li>";
                    }
                    content += "</ul>";
                }
            }

            // @Task data types

            // @Task functions

            content += "</div>";
        }
        content += "</body></html>";

        self.pages.push(Page {
            module_path_segments,
            content,
        });
    }
}

struct Page<'a> {
    // @Temporary name, bad name
    module_path_segments: VecDeque<&'a str>,
    // @Temporary
    content: String,
}
