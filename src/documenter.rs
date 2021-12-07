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
use node::Document;
use node::{Attributable, Element, Node, VoidElement};
use std::{borrow::Cow, collections::VecDeque, default::default, fs, path::PathBuf};

mod format;
mod node;

const DOCUMENTATION_FOLDER_NAME: &str = "doc";

static STYLE_SHEET_CONTENT: &str = include_str!("documenter/include/style.css");
const STYLE_SHEET_FILE_NAME: &str = "style.min.css";
static SCRIPT_CONTENT: &str = include_str!("documenter/include/script.js");
const SCRIPT_FILE_NAME: &str = "script.min.js";
const SEARCH_INDEX_FILE_NAME: &str = "search-index.min.js";

const DEVELOPING: bool = true;

pub struct Documenter<'a> {
    crate_: &'a Crate,
    session: &'a BuildSession,
    #[allow(dead_code)]
    reporter: &'a Reporter,
    pages: Vec<Page<'a>>,
    bindings: Vec<String>,
    destination: PathBuf,
}

impl<'a> Documenter<'a> {
    pub fn new(crate_: &'a Crate, session: &'a BuildSession, reporter: &'a Reporter) -> Self {
        let destination = session.build_folder().join(DOCUMENTATION_FOLDER_NAME);

        Self {
            crate_,
            session,
            reporter,
            pages: default(),
            bindings: default(),
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
        self.collect_all_bindings(declaration);
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
        let script_path = self.destination.join(SCRIPT_FILE_NAME);

        if DEVELOPING || !style_sheet_path.exists() {
            fs::write(
                style_sheet_path,
                minifier::css::minify(STYLE_SHEET_CONTENT).unwrap(),
            )
            .unwrap();
        }
        if DEVELOPING || !script_path.exists() {
            fs::write(script_path, minifier::js::minify(SCRIPT_CONTENT)).unwrap();
        }
        // @Task use BufWriter
        // @Task put it in self.destination instead once the search index contains all crates
        fs::write(
            self.destination
                .join(self.crate_.name(self.session).as_str())
                .join(SEARCH_INDEX_FILE_NAME),
            {
                let mut search_index = String::from("window.searchIndex=[");
                for binding in &self.bindings {
                    search_index += &format!("{binding:?}");
                    search_index += ",";
                }
                search_index += "];";
                search_index
            },
        )
        .unwrap();

        for page in &self.pages {
            let path = self.module_folder_path(&page.module_path_segments);

            if !path.exists() {
                fs::create_dir(&path).unwrap();
            }

            fs::write(path.join("index.html"), &page.content).unwrap();
        }

        Ok(())
    }

    // @Task merge this with `document_declaration` (once we move out `generate_module_page` out of it!)
    // to avoid traversing the crate graph too often
    fn collect_all_bindings(&mut self, declaration: &hir::Declaration) {
        let local_path = |identifier: &hir::Identifier| {
            let index = identifier.local_declaration_index(self.crate_).unwrap();
            // @Beacon @Task don't use the to_string variant!
            self.crate_.local_path_to_string(index, self.session)
        };

        match &declaration.value {
            hir::DeclarationKind::Function(function) => {
                self.bindings.push(local_path(&function.binder));
            }
            hir::DeclarationKind::Data(type_) => {
                self.bindings.push(local_path(&type_.binder));

                if let Some(constructors) = &type_.constructors {
                    for declaration in constructors {
                        self.collect_all_bindings(declaration);
                    }
                }
            }
            hir::DeclarationKind::Constructor(constructor) => {
                self.bindings.push(local_path(&constructor.binder));
            }
            hir::DeclarationKind::Module(module) => {
                self.bindings.push(local_path(&module.binder));

                for declaration in &module.declarations {
                    self.collect_all_bindings(declaration);
                }
            }
            // @Task re-exports
            // hir::DeclarationKind::Use(_) => todo!(),
            _ => {}
        }
    }

    fn document_declaration(&mut self, declaration: &hir::Declaration) -> Result<()> {
        if let hir::DeclarationKind::Module(module) = &declaration.value {
            // @Task defer generation, only collect into structured data to be able to
            // generate the pages in parallel later on!
            self.generate_module_page(module, &declaration.attributes);

            for declaration in &module.declarations {
                self.document_declaration(declaration)?;
            }
        }

        Ok(())
    }

    fn generate_module_page(&mut self, module: &hir::Module, attributes: &lowered_ast::Attributes) {
        let index = module.binder.local_declaration_index(self.crate_).unwrap();
        let module_path_segments = self.crate_.local_path_segments(index);
        let module_path = self.crate_.local_path_to_string(index, self.session);
        let documentation_folder = "../".repeat(module_path_segments.len());

        let mut head = Element::new("head")
            .child(VoidElement::new("meta").attribute("charset", "utf-8"))
            .child(
                VoidElement::new("meta")
                    .attribute("name", "viewport")
                    .attribute("content", "width=device-width, initial-scale=1.0"),
            )
            .child(Element::new("title").child(module_path))
            .child(
                VoidElement::new("meta")
                    .attribute("name", "generator")
                    .attribute(
                        "content",
                        format!("lushui documenter {}", env!("CARGO_PKG_VERSION")),
                    ),
            )
            .child(
                VoidElement::new("link")
                    .attribute("rel", "stylesheet")
                    .attribute(
                        "href",
                        format!("{documentation_folder}{STYLE_SHEET_FILE_NAME}"),
                    ),
            )
            .child(
                Element::new("script")
                    .attribute("src", format!("{documentation_folder}{SCRIPT_FILE_NAME}"))
                    .boolean_attribute("defer"),
            )
            .child(
                Element::new("script")
                    .attribute(
                        "src",
                        format!(
                            "{}{SEARCH_INDEX_FILE_NAME}",
                            "../".repeat(module_path_segments.len() - 1)
                        ),
                    )
                    .boolean_attribute("defer"),
            );
        let description = &self.crate_.package(self.session).description;
        if !description.is_empty() {
            head.add_child(
                VoidElement::new("meta")
                    .attribute("name", "description")
                    .attribute("content", description),
            );
        }

        let mut body = Element::new("body");

        // ledge
        body.add_child(Node::verbatim(r#"<div class="ledge-container"><div class="ledge"><div><input type="search" id="search" placeholder="Search…" autocomplete="off"><div id="search-results-container"><div id="search-results"></div></div></div></div></div>"#));

        // main content
        {
            let mut main = Element::new("div").attribute("class", "main");

            // section header
            {
                let mut section_header = Element::new("h1");

                section_header.add_child(match index == self.crate_.root() {
                    true => "Crate",
                    false => "Module",
                });

                section_header.add_child(" ");

                for (position, &path_segment) in module_path_segments.iter().enumerate() {
                    let is_last_segment = position + 1 == module_path_segments.len();

                    let mut anchor = Element::new("a").attribute(
                        "href",
                        format!(
                            "{}index.html",
                            "../".repeat(module_path_segments.len() - 1 - position)
                        ),
                    );
                    if is_last_segment {
                        anchor.add_attribute("class", "current");
                    }
                    anchor.add_child(path_segment);

                    section_header.add_child(anchor);

                    if !is_last_segment {
                        // @Task add extra spacing (left or right, depends) for
                        // non-word module names
                        section_header.add_child(Node::verbatim(".<wbr>"));
                    }
                }

                main.add_child(section_header);
            }

            // description
            {
                let mut paragraph = Element::new("p");

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
                    paragraph.add_child(line);
                }

                main.add_child(paragraph);
            }

            let mut subsections = Subsections::default();

            for declaration in &module.declarations {
                match &declaration.value {
                    hir::DeclarationKind::Function(function) => {
                        subsections.functions.push(Function {
                            binder: function.binder.as_str(),
                            type_: format::format_expression(
                                &function.type_annotation,
                                self.crate_,
                                self.session,
                            ),
                        });
                    }
                    // @Task collect constructors
                    hir::DeclarationKind::Data(type_) => {
                        subsections.types.push(Type {
                            binder: type_.binder.as_str(),
                            type_: format::format_expression(
                                &type_.type_annotation,
                                self.crate_,
                                self.session,
                            ),
                        });
                    }
                    hir::DeclarationKind::Module(module) => {
                        subsections.modules.push(Module {
                            binder: module.binder.as_str(),
                            description: "XXX".to_owned(),
                        });
                    }
                    // @Task re-exports, only public//(public crate) for --doc-priv-decls??) ones!
                    // hir::DeclarationKind::Use(_) => todo!(),
                    _ => {}
                }
            }

            fn section_header<'a>(
                level: u8,
                id: impl Into<Cow<'a, str>>,
                content: impl Into<Cow<'a, str>>,
            ) -> Element<'a> {
                fn section_header<'a>(
                    level: u8,
                    id: Cow<'a, str>,
                    content: Cow<'a, str>,
                ) -> Element<'a> {
                    Element::new(format!("h{level}"))
                        .attribute("id", id.clone())
                        .attribute("class", "section-header")
                        .child(
                            Element::new("a")
                                .attribute("href", format!("#{id}"))
                                .child(content),
                        )
                }
                section_header(level, id.into(), content.into())
            }

            if !subsections.modules.is_empty() {
                main.add_child(section_header(2, "modules", "Modules"));

                let mut table = Element::new("table");

                for module in subsections.modules {
                    table.add_child(
                        Element::new("tr")
                            .child(
                                Element::new("td").child(
                                    Element::new("a")
                                        // @Task url-encode the binder (it may be punctuation!)
                                        .attribute("href", format!("{}/index.html", module.binder))
                                        .attribute("class", "identifier")
                                        .child(module.binder),
                                ),
                            )
                            .child(Element::new("td").child(module.description)),
                    );
                }

                main.add_child(table);
            }

            if !subsections.types.is_empty() {
                main.add_child(section_header(2, "types", "Data Types"));

                for type_ in subsections.types {
                    // @Task URL-escape the binder here!
                    let link = format!(
                        "type.{}",
                        html_escape::encode_quoted_attribute(type_.binder),
                    );

                    main.add_child(
                        section_header(3, link, type_.binder)
                            .class("code")
                            .child(": ")
                            .child(Node::verbatim(type_.type_)),
                    );
                }
            }

            if !subsections.functions.is_empty() {
                main.add_child(section_header(2, "functions", "Functions"));

                for function in subsections.functions {
                    // @Task URL-escape the binder here!
                    let link = format!(
                        "function.{}",
                        html_escape::encode_quoted_attribute(function.binder),
                    );

                    main.add_child(
                        section_header(3, link, function.binder)
                            .class("code")
                            .child(": ")
                            .child(Node::verbatim(function.type_)),
                    );
                }
            }

            body.add_child(main);
        }

        let mut html = Element::new("html");
        html.add_attribute("lang", "en");
        html.add_child(head);
        html.add_child(body);

        let mut document = Document::default();
        document.add_child(html);

        let mut content = String::new();
        document.render(&mut content);

        self.pages.push(Page {
            module_path_segments,
            content,
        });
    }
}

#[derive(Default)]
struct Subsections<'a> {
    // re_exports: (),
    modules: Vec<Module<'a>>,
    types: Vec<Type<'a>>,
    functions: Vec<Function<'a>>,
}

struct Module<'a> {
    binder: &'a str,
    description: String,
}

struct Type<'a> {
    binder: &'a str,
    type_: String,
}

struct Function<'a> {
    binder: &'a str,
    type_: String,
}

struct Page<'a> {
    // @Temporary name, bad name
    module_path_segments: VecDeque<&'a str>,
    // @Temporary
    content: String,
}
