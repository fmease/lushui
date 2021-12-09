//! The documentation generator.

// @Tasks:
// * collapsible declaration descriptions
// * "collapse all" functionality
// * descriptions extracted from documentation comments
// * link to source (generate html files per source file)
// * list of all crates (self/goal, direct deps, transitive deps)
// * search functionality using levenshtein
// * description next to search result
// * good looking fonts
// * manually switching between a light and a dark mode + automatic mode detection
// * asciidoc integration
// * print attributes
// * de-lower parameter lists
// * add fonts via @font-face and use them (properly define font-weights of h1 etc)
// * convert ttfs to woffs (1 + 2) and change @font-faces
// * fill the COPYRIGHT file

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

mod fonts;
mod format;
mod node;

const DOCUMENTATION_FOLDER_NAME: &str = "doc";
const MAIN_STYLE_SHEET_FILE_NAME: &str = "style.min.css";
const MAIN_SCRIPT_FILE_NAME: &str = "script.min.js";
const SEARCH_INDEX_FILE_NAME: &str = "search-index.min.js";

// @Temporary
static LOREM_IPSUM: &str = "\
Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. \
Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. \
Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.";

const DEVELOPING: bool = true;

pub struct Documenter<'a> {
    crate_: &'a Crate,
    session: &'a BuildSession,
    #[allow(dead_code)]
    reporter: &'a Reporter,
    pages: Vec<Page<'a>>,
    bindings: Vec<hir::DeclarationIndex>,
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

        {
            let path = self.destination.join(MAIN_STYLE_SHEET_FILE_NAME);
            if DEVELOPING || !path.exists() {
                static STYLE_SHEET: &str = include_str!("documenter/static/css/style.css");
                fs::write(path, minifier::css::minify(STYLE_SHEET).unwrap()).unwrap();
            }
        }

        {
            let path = self.destination.join("fonts.css");
            if !path.exists() {
                static STYLE_SHEET: &str = include_str!("documenter/static/css/fonts.css");
                fs::write(path, minifier::css::minify(STYLE_SHEET).unwrap()).unwrap();
            }
        }

        {
            let path = self.destination.join(MAIN_SCRIPT_FILE_NAME);
            if DEVELOPING || !path.exists() {
                static SCRIPT: &str = include_str!("documenter/static/js/script.js");
                fs::write(path, minifier::js::minify(SCRIPT)).unwrap();
            }
        }

        fonts::copy_over(&self.destination).unwrap();

        // @Task use BufWriter
        // @Task put it in self.destination instead once the search index contains all crates
        fs::write(
            self.destination
                .join(self.crate_.name(self.session).as_str())
                .join(SEARCH_INDEX_FILE_NAME),
            {
                let mut search_index = String::from("window.searchIndex=[");
                for &index in &self.bindings {
                    // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                    // incorrect on top of that!)
                    let path = self.crate_.local_path_to_string(
                        index.local_index(self.crate_).unwrap(),
                        self.session,
                    );

                    search_index += &format!(
                        "[{path:?},{:?}],",
                        format::declaration_url_fragment(index, self.crate_, self.session)
                    );
                }
                search_index += "];";
                search_index
            },
        )
        .unwrap();

        for page in &self.pages {
            let mut path = self.destination.clone();

            for segment in &page.module_path_segments {
                // *URL*-encoding the *folder names* also ensures that we always
                // create a valid Windows file path:
                // https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file#file-and-directory-names
                // @Update @Bug this double-escapes, right??
                path.extend_one(&*urlencoding::encode(segment));
            }

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
        match &declaration.value {
            hir::DeclarationKind::Function(function) => {
                self.bindings
                    .push(function.binder.declaration_index().unwrap());
            }
            hir::DeclarationKind::Data(type_) => {
                self.bindings
                    .push(type_.binder.declaration_index().unwrap());

                if let Some(constructors) = &type_.constructors {
                    for declaration in constructors {
                        self.collect_all_bindings(declaration);
                    }
                }
            }
            hir::DeclarationKind::Constructor(constructor) => {
                self.bindings
                    .push(constructor.binder.declaration_index().unwrap());
            }
            hir::DeclarationKind::Module(module) => {
                self.bindings
                    .push(module.binder.declaration_index().unwrap());

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

    // @Note this is an ugly abstraction
    fn collect_subsections<'m>(
        &mut self,
        module: &'m hir::Module,
        depth: usize,
    ) -> Subsections<'m> {
        let mut subsections = Subsections::default();

        for declaration in &module.declarations {
            match &declaration.value {
                hir::DeclarationKind::Function(function) => {
                    subsections.functions.push(Function {
                        binder: function.binder.as_str(),
                        type_: format::format_expression(
                            &function.type_annotation,
                            depth,
                            self.crate_,
                            self.session,
                        ),
                    });
                }
                // @Task collect constructors
                hir::DeclarationKind::Data(type_) => {
                    let mut formatted_constructors = Vec::new();

                    if let Some(constructors) = &type_.constructors {
                        for declaration in constructors {
                            let constructor = declaration.constructor().unwrap();

                            formatted_constructors.push(Constructor {
                                binder: constructor.binder.as_str(),
                                type_: format::format_expression(
                                    &constructor.type_annotation,
                                    depth,
                                    self.crate_,
                                    self.session,
                                ),
                            });
                        }
                    }

                    subsections.types.push(Type {
                        // @Task write a custom formatter for this which prints links to the documentation
                        // of the specific attributes
                        _attributes: declaration
                            .attributes
                            .iter()
                            .map(ToString::to_string)
                            .intersperse(" ".into())
                            .collect::<String>(),
                        binder: type_.binder.as_str(),
                        type_: format::format_expression(
                            &type_.type_annotation,
                            depth,
                            self.crate_,
                            self.session,
                        ),
                        constructors: formatted_constructors,
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

        subsections
    }

    fn generate_module_page(&mut self, module: &hir::Module, attributes: &lowered_ast::Attributes) {
        let index = module.binder.local_declaration_index(self.crate_).unwrap();
        let module_path_segments = self.crate_.local_path_segments(index);
        let module_path = self.crate_.local_path_to_string(index, self.session);
        // @Note bad name
        let depth = module_path_segments.len();
        let documentation_folder = "../".repeat(depth);

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
                        format!("{documentation_folder}{MAIN_STYLE_SHEET_FILE_NAME}"),
                    ),
            )
            .child(
                Element::new("script")
                    .attribute(
                        "src",
                        format!("{documentation_folder}{MAIN_SCRIPT_FILE_NAME}"),
                    )
                    .boolean_attribute("defer"),
            )
            .child(
                Element::new("script")
                    .attribute(
                        "src",
                        format!("{}{SEARCH_INDEX_FILE_NAME}", "../".repeat(depth - 1)),
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
        body.add_child(
            Element::new("div").class("ledge__positioner").child(
                Element::new("div").class("ledge").child(
                    Element::new("div")
                        .child(
                            VoidElement::new("input")
                                .attribute("type", "search")
                                .attribute("id", "js-searchbar")
                                .attribute("placeholder", "Search…")
                                .attribute("autocomplete", "off")
                                .attribute("data-url-prefix", documentation_folder),
                        )
                        .child(
                            Element::new("div")
                                .attribute("id", "search-results__positioner")
                                .child(Element::new("div").attribute("id", "js-search-results")),
                        ),
                ),
            ),
        );

        let mut container = Element::new("div").class("container");

        let subsections = self.collect_subsections(module, depth);

        // table of contents
        {
            let mut table_of_contents = Element::new("div").class("sidebar");

            if !subsections.modules.is_empty() {
                table_of_contents.add_child(
                    Element::new("a")
                        .attribute("href", format!("#{}", Subsections::MODULES))
                        .class("title")
                        .child(Subsections::HEADING_MODULES),
                );
            }

            if !subsections.types.is_empty() {
                table_of_contents.add_child(
                    Element::new("a")
                        .attribute("href", format!("#{}", Subsections::TYPES))
                        .class("title")
                        .child(Subsections::HEADING_TYPES),
                );

                let mut list = Element::new("ul");

                for type_ in &subsections.types {
                    list.add_child(
                        Element::new("li").child(
                            Element::new("a")
                                .attribute("href", format!("#{}", declaration_id(type_.binder)))
                                .child(type_.binder),
                        ),
                    );
                }

                table_of_contents.add_child(list);
            }

            if !subsections.functions.is_empty() {
                table_of_contents.add_child(
                    Element::new("a")
                        .attribute("href", format!("#{}", Subsections::FUNCTIONS))
                        .class("title")
                        .child(Subsections::HEADING_FUNCTIONS),
                );

                let mut list = Element::new("ul");

                for function in &subsections.functions {
                    list.add_child(
                        Element::new("li").child(
                            Element::new("a")
                                .attribute("href", format!("#{}", declaration_id(function.binder)))
                                .child(function.binder),
                        ),
                    );
                }

                table_of_contents.add_child(list);
            }

            container.add_child(table_of_contents);
        }

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");

                heading.add_child(match index == self.crate_.root() {
                    true => "Crate",
                    false => "Module",
                });

                heading.add_child(" ");

                for (position, &path_segment) in module_path_segments.iter().enumerate() {
                    let is_last_segment = position + 1 == depth;

                    let mut anchor = Element::new("a").attribute(
                        "href",
                        format!("{}index.html", "../".repeat(depth - 1 - position)),
                    );
                    if is_last_segment {
                        anchor.add_class("active");
                    }
                    anchor.add_child(path_segment);

                    heading.add_child(anchor);

                    if !is_last_segment {
                        // @Task add extra spacing (left or right, depends) for
                        // non-word module names
                        heading.add_child(Node::verbatim(".<wbr>"));
                    }
                }

                main.add_child(heading);
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

                // @Temporary
                main.add_child(Element::new("p").child(LOREM_IPSUM));
            }

            fn heading<'a>(
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
                        .class("subheading")
                        .child(
                            Element::new("a")
                                .attribute("href", format!("#{id}"))
                                .child(content),
                        )
                }
                section_header(level, id.into(), content.into())
            }

            if !subsections.modules.is_empty() {
                let mut section = Element::new("section").child(heading(
                    2,
                    Subsections::MODULES,
                    Subsections::HEADING_MODULES,
                ));

                let mut table = Element::new("table").class("indent");

                for module in subsections.modules {
                    table.add_child(
                        Element::new("tr")
                            .child(
                                Element::new("td").child(
                                    Element::new("a")
                                        .attribute(
                                            "href",
                                            format!(
                                                "{}/index.html",
                                                urlencoding::encode(module.binder)
                                            ),
                                        )
                                        .class("reference")
                                        .child(module.binder),
                                ),
                            )
                            .child(Element::new("td").child(module.description)),
                    );
                }

                section.add_child(table);
                main.add_child(section);
            }

            if !subsections.types.is_empty() {
                let mut section = Element::new("section").child(heading(
                    2,
                    Subsections::TYPES,
                    Subsections::HEADING_TYPES,
                ));

                for type_ in subsections.types {
                    let id = declaration_id(type_.binder);

                    section.add_child(
                        Element::new("h3")
                            .attribute("id", id.clone())
                            .class("subheading declaration")
                            .child(
                                Element::new("a")
                                    .class("binder")
                                    .attribute("href", format!("#{id}"))
                                    .child(type_.binder),
                            )
                            .child(": ")
                            .child(Node::verbatim(type_.type_)),
                    );

                    // .child(
                    //     Element::new("div")
                    //         .class("attributes")
                    //         .child(type_.attributes),
                    // )

                    // @Temporary
                    section.add_child(Element::new("p").child(LOREM_IPSUM));

                    // @Task don't do that for abstract or intrinsic types
                    // @Question what if there are no constructors?
                    let mut subsection = Element::new("section").class("indent").child(heading(
                        4,
                        format!("constructors.{}", type_.binder),
                        "Constructors",
                    ));

                    for constructor in type_.constructors {
                        let id =
                            declaration_id(&format!("{}.{}", type_.binder, constructor.binder));

                        subsection.add_child(
                            Element::new("h5")
                                .attribute("id", id.clone())
                                .class("subheading declaration")
                                .child(
                                    Element::new("a")
                                        .class("binder")
                                        .attribute("href", format!("#{id}"))
                                        .child(constructor.binder),
                                )
                                .child(": ")
                                .child(Node::verbatim(constructor.type_)),
                        );

                        // @Temporary
                        subsection.add_child(Element::new("p").child(LOREM_IPSUM));
                    }

                    section.add_child(subsection);
                }

                main.add_child(section)
            }

            if !subsections.functions.is_empty() {
                let mut section = Element::new("section").child(heading(
                    2,
                    Subsections::FUNCTIONS,
                    Subsections::HEADING_FUNCTIONS,
                ));

                for function in subsections.functions {
                    let id = declaration_id(function.binder);

                    section.add_child(
                        Element::new("h3")
                            .attribute("id", id.clone())
                            .class("subheading declaration")
                            .child(
                                Element::new("a")
                                    .class("binder")
                                    .attribute("href", format!("#{id}"))
                                    .child(function.binder),
                            )
                            .child(": ")
                            .child(Node::verbatim(function.type_)),
                    );

                    // @Temporary
                    section.add_child(Element::new("p").child(LOREM_IPSUM));
                }

                main.add_child(section);
            }

            container.add_child(main);
        }

        // crates side bar
        {
            let mut sidebar = Element::new("div").class("sidebar");
            sidebar.add_child(Element::new("div").class("title").child("Crates"));

            sidebar.add_child(Element::new("em").child("not yet implemented"));
            // let mut crate_list = Element::new("ul");

            // for dependency in self.crate_.dependencies(&self.session) {
            //     dbg!();

            //     crate_list.add_child(
            //         Element::new("li").child(self.session[dependency].name(self.session).as_str()),
            //     );
            // }

            // sidebar.add_child(crate_list);
            container.add_child(sidebar);
        }

        body.add_child(container);

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

impl Subsections<'_> {
    const HEADING_MODULES: &'static str = "Modules";
    const MODULES: &'static str = "modules";
    const HEADING_TYPES: &'static str = "Data Types";
    const TYPES: &'static str = "types";
    const HEADING_FUNCTIONS: &'static str = "Functions";
    const FUNCTIONS: &'static str = "functions";
}

struct Module<'a> {
    binder: &'a str,
    description: String,
}

struct Type<'a> {
    _attributes: String,
    binder: &'a str,
    type_: String,
    constructors: Vec<Constructor<'a>>,
}

struct Constructor<'a> {
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

fn declaration_id(binder: &str) -> String {
    format!("decl.{}", urlencoding::encode(binder))
}
