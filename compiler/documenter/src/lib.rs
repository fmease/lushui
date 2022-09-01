//! The documentation generator.
#![feature(decl_macro, default_free_fn)]

// @Task instead of showing all components in the session in the left pane, have two sections:
//     (1) the dependencies of the target(s) (formerly known as "goals")
//     (2) the dependencies of the component currently viewed
// If one currently views the docs of a target (that is most of the time), they are identical (duh)
// and obviously, only show the list once. Don't show empty sections (obviously)

use crossbeam::thread::Scope;
use derivation::Elements;
use error::Result;
use hir_format::ComponentExt;
use joinery::JoinableIterator;
use lowered_ast::{Attribute, AttributeName, Attributes, BareAttribute};
use node::{Attributable, Document, Element, Node, VoidElement};
use session::{BuildSession, Component, ComponentOutline, DeclarationIndexExt, IdentifierExt};
use std::{default::default, fs, io::BufWriter, path::PathBuf};
use text_processor::TextProcessor;
use token::Word;

mod fonts;
mod format;
mod node;
mod subsections;
mod text_processor;

const DOCUMENTATION_FOLDER_NAME: &str = "doc";
const MAIN_STYLE_SHEET_FILE_NAME: &str = "style.min.css";
const MAIN_SCRIPT_FILE_NAME: &str = "script.min.js";
const SEARCH_INDEX_FILE_NAME: &str = "search-index.min.js";

// @Task make this an unstable option instead
const DEVELOPING: bool = true;

/// Document the given component.
///
/// Returns the path to the index page (of the component).
pub fn document(
    component_root: &hir::Declaration,
    options: Options,
    components: &[ComponentOutline],
    component: &Component,
    session: &BuildSession,
) -> Result<PathBuf> {
    crossbeam::scope(|scope| {
        let mut documenter =
            Documenter::new(options, components, component, session, scope).unwrap();

        documenter
            .pages
            .push(documenter.render_reserved_identifiers_page());
        documenter.pages.push(documenter.render_attributes_page());
        documenter.document_declaration(component_root)?;
        documenter.collect_search_items(component_root);
        let index_page_path = documenter.persist().unwrap();
        documenter.text_processor.destruct().unwrap();

        Ok(index_page_path)
    })
    .unwrap()
}

struct Documenter<'a, 'scope> {
    options: Options,
    components: &'a [ComponentOutline],
    component: &'a Component,
    session: &'a BuildSession,
    text_processor: TextProcessor<'scope>,
    pages: Vec<Page>,
    search_items: Vec<SearchItem>,
    path: PathBuf,
}

impl<'a, 'scope> Documenter<'a, 'scope> {
    fn new(
        options: Options,
        components: &'a [ComponentOutline],
        component: &'a Component,
        session: &'a BuildSession,
        scope: &'scope Scope<'a>,
    ) -> Result<Self, std::io::Error> {
        let path = session.build_folder().join(DOCUMENTATION_FOLDER_NAME);

        if !path.exists() {
            fs::create_dir_all(&path)?;
        }

        let text_processor = TextProcessor::new(&path, &options, component, session, scope)?;

        Ok(Self {
            options,
            components,
            component,
            session,
            text_processor,
            pages: default(),
            search_items: default(),
            path,
        })
    }

    /// Persist the generated documentation to disk.
    ///
    /// Returns the path to the index page (of the component).
    fn persist(&self) -> Result<PathBuf, std::io::Error> {
        // @Task handle the case where two+ components (goal and/or (transitive) deps)
        // have the same name and are being documented

        {
            let path = self.path.join(MAIN_STYLE_SHEET_FILE_NAME);
            // @Task instead of `DEVELOPING`, compare hash (maybe)?
            if DEVELOPING || !path.exists() {
                static STYLE_SHEET: &str = include_str!("../include/css/style.css");
                minifier::css::minify(STYLE_SHEET)
                    .unwrap()
                    .write(BufWriter::new(fs::File::create(path)?))?;
            }
        }

        {
            let path = self.path.join("fonts.min.css");
            if !path.exists() {
                static STYLE_SHEET: &str = include_str!("../include/css/fonts.css");
                minifier::css::minify(STYLE_SHEET)
                    .unwrap()
                    .write(BufWriter::new(fs::File::create(path)?))?;
            }
        }

        {
            let path = self.path.join(MAIN_SCRIPT_FILE_NAME);
            // @Task instead of `DEVELOPING`, compare hash (maybe)?
            if DEVELOPING || !path.exists() {
                static SCRIPT: &str = include_str!("../include/js/script.js");
                minifier::js::minify(SCRIPT).write(BufWriter::new(fs::File::create(path)?))?;
            }
        }

        fonts::copy_over(&self.path)?;

        for page in &self.pages {
            let parent = page.path.parent().unwrap();

            if !parent.exists() {
                fs::create_dir(parent)?;
            }

            fs::write(&page.path, &page.content)?;
        }

        let component_path = self.path.join(self.component.name().as_str());

        if !component_path.exists() {
            fs::create_dir(&component_path)?;
        }

        // @Task use BufWriter
        // @Task put it in self.destination instead once the search index contains all components
        fs::write(component_path.join(SEARCH_INDEX_FILE_NAME), {
            let mut search_index = String::from("window.searchIndex=[");

            for search_item in &self.search_items {
                use std::fmt::Write;

                match *search_item {
                    SearchItem::Declaration(index) => {
                        // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                        // incorrect on top of that!)
                        let path = self.component.local_index_with_root_to_extern_path(
                            index.local(self.component).unwrap(),
                            self.component.name().to_string(),
                        );

                        write!(
                            search_index,
                            "[{path:?},{:?}],",
                            format::declaration_url_fragment(index, self.component, self.session)
                        )
                        .unwrap();
                    }
                    SearchItem::Keyword(keyword) => {
                        write!(
                            search_index,
                            "[{keyword:?},{:?}],",
                            // keywords are inherently URL-safe and don't need percent-encoding
                            format!("identifiers.html#word.{keyword}")
                        )
                        .unwrap();
                    }
                    SearchItem::ReservedPunctuation(punctuation) => {
                        write!(
                            search_index,
                            "[{punctuation:?},{:?}],",
                            format!(
                                "identifiers.html#punctuation.{}",
                                urlencoding::encode(punctuation)
                            )
                        )
                        .unwrap();
                    }
                    SearchItem::Attribute(binder) => {
                        write!(
                            search_index,
                            "[{binder:?},{:?}],",
                            // attribute names are inherently URL-safe and don't need percent-encoding
                            format!("attributes.html#attribute.{binder}")
                        )
                        .unwrap();
                    }
                }
            }

            search_index += "];";
            search_index
        })?;

        Ok(component_path.join("index.html"))
    }

    fn collect_search_items(&mut self, component_root: &hir::Declaration) {
        for keyword in KEYWORDS {
            self.search_items.push(SearchItem::Keyword(keyword));
        }

        for punctuation in RESERVED_PUNCTUATION {
            self.search_items
                .push(SearchItem::ReservedPunctuation(punctuation));
        }

        for attribute in AttributeName::elements() {
            self.search_items
                .push(SearchItem::Attribute(attribute.to_str()));
        }

        self.collect_declaration_search_items(component_root);
    }

    fn collect_declaration_search_items(&mut self, declaration: &hir::Declaration) {
        match &declaration.bare {
            hir::BareDeclaration::Function(function) => {
                self.search_items.push(SearchItem::Declaration(
                    function.binder.declaration_index().unwrap(),
                ));
            }
            hir::BareDeclaration::Data(type_) => {
                self.search_items.push(SearchItem::Declaration(
                    type_.binder.declaration_index().unwrap(),
                ));

                if let Some(constructors) = &type_.constructors {
                    for declaration in constructors {
                        self.collect_declaration_search_items(declaration);
                    }
                }
            }
            hir::BareDeclaration::Constructor(constructor) => {
                self.search_items.push(SearchItem::Declaration(
                    constructor.binder.declaration_index().unwrap(),
                ));
            }
            hir::BareDeclaration::Module(module) => {
                self.search_items.push(SearchItem::Declaration(
                    module.binder.declaration_index().unwrap(),
                ));

                for declaration in &module.declarations {
                    self.collect_declaration_search_items(declaration);
                }
            }
            // @Task re-exports
            // hir::DeclarationKind::Use(_) => todo!(),
            _ => {}
        }
    }

    fn document_declaration(&mut self, declaration: &hir::Declaration) -> Result<()> {
        if let hir::BareDeclaration::Module(module) = &declaration.bare {
            // @Task defer generation, only collect into structured data to be able to
            // generate the pages in parallel later on!
            let page = self.render_module_page(module, &declaration.attributes);
            self.pages.push(page);

            for declaration in &module.declarations {
                self.document_declaration(declaration)?;
            }
        }

        Ok(())
    }

    fn collect_module_subsections<'m>(
        &self,
        module: &'m hir::Module,
        url_prefix: &str,
    ) -> subsections::Subsections<'m> {
        let mut subsections = subsections::Subsections::default();

        for declaration in &module.declarations {
            match &declaration.bare {
                hir::BareDeclaration::Function(function) => {
                    subsections.functions.0.push(subsections::Function {
                        attributes: &declaration.attributes,
                        binder: function.binder.as_str(),
                        type_: format::format_expression(
                            &function.type_annotation,
                            url_prefix,
                            self.component,
                            self.session,
                        ),
                    });
                }
                hir::BareDeclaration::Data(type_) => {
                    let mut formatted_constructors = Vec::new();

                    if let Some(constructors) = &type_.constructors {
                        for declaration in constructors {
                            let constructor = declaration.bare.constructor().unwrap();

                            formatted_constructors.push(subsections::Constructor {
                                attributes: &declaration.attributes,
                                binder: constructor.binder.as_str(),
                                type_: format::format_expression(
                                    &constructor.type_annotation,
                                    url_prefix,
                                    self.component,
                                    self.session,
                                ),
                            });
                        }
                    }

                    subsections.types.0.push(subsections::Type {
                        // @Task write a custom formatter for this which prints links to the documentation
                        // of the specific attributes
                        attributes: &declaration.attributes,
                        binder: type_.binder.as_str(),
                        type_: format::format_expression(
                            &type_.type_annotation,
                            url_prefix,
                            self.component,
                            self.session,
                        ),
                        constructors: formatted_constructors,
                    });
                }
                hir::BareDeclaration::Module(module) => {
                    subsections.modules.0.push(subsections::Module {
                        attributes: &declaration.attributes,
                        binder: module.binder.as_str(),
                    });
                }
                // @Task re-exports, only public//(public topmost) for --doc-priv-decls??) ones!
                // hir::DeclarationKind::Use(_) => todo!(),
                _ => {}
            }
        }

        subsections
    }

    fn render_module_page(&self, module: &hir::Module, attributes: &Attributes) -> Page {
        let index = module
            .binder
            .local_declaration_index(self.component)
            .unwrap();
        let mut path_segments = self.component.local_index_to_path_segments(index);
        path_segments.push_front(self.component.name().as_str());
        let page_depth = path_segments.len();
        let url_prefix = format!("./{}", "../".repeat(page_depth));

        let mut body = Element::new("body");
        render_ledge(&mut body, &url_prefix);

        let mut container = Element::new("div").class("container");

        let subsections = self.collect_module_subsections(module, &url_prefix);
        container.add_child(render_sidebar(&subsections, &url_prefix, self.components));

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");

                heading.add_child(match index == self.component.root_local() {
                    true => "Component",
                    false => "Module",
                });

                heading.add_child(" ");

                for (position, &path_segment) in path_segments.iter().enumerate() {
                    let is_last_segment = position + 1 == page_depth;

                    let mut anchor = Element::new("a").attribute(
                        "href",
                        format!("{}index.html", "../".repeat(page_depth - 1 - position)),
                    );
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

            render_declaration_attributes(
                attributes,
                &url_prefix,
                &mut main,
                &self.text_processor,
                &self.options,
            );

            subsections.render(&url_prefix, &mut main, &self.text_processor, &self.options);

            container.add_child(main);
        }

        body.add_child(container);

        let title = self
            .component
            .local_index_with_root_to_extern_path(index, self.component.name().to_string());

        let mut path = self.path.join(self.component.name().as_str());
        path.extend(path_segments.into_iter().skip(1));
        path.push("index.html");

        Page {
            path,
            content: render_page(
                render_head(self.component.name(), &url_prefix, title.into()),
                body,
            ),
        }
    }

    fn render_reserved_identifiers_page(&self) -> Page {
        let url_prefix = "./";

        let mut body = Element::new("body");
        render_ledge(&mut body, url_prefix);

        let mut container = Element::new("div").class("container");

        let subsections = subsections::Subsections::reserved_identifiers();
        container.add_child(render_sidebar(&subsections, url_prefix, self.components));

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");
                heading.add_child("Reserved Identifiers");
                main.add_child(heading);
            }

            subsections.render(url_prefix, &mut main, &self.text_processor, &self.options);

            container.add_child(main);
        }

        body.add_child(container);

        Page {
            path: self.path.join("identifiers.html"),
            content: render_page(
                render_head(
                    self.component.name(),
                    url_prefix,
                    "Reserved Identifiers".into(),
                ),
                body,
            ),
        }
    }

    fn render_attributes_page(&self) -> Page {
        let url_prefix = "./";

        let mut body = Element::new("body");
        render_ledge(&mut body, url_prefix);

        let mut container = Element::new("div").class("container");

        let subsections = subsections::Subsections::attributes();
        container.add_child(render_sidebar(&subsections, url_prefix, self.components));

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");
                heading.add_child("Attributes");
                main.add_child(heading);
            }

            subsections.render(url_prefix, &mut main, &self.text_processor, &self.options);

            container.add_child(main);
        }

        body.add_child(container);

        Page {
            path: self.path.join("attributes.html"),
            content: render_page(
                render_head(self.component.name(), url_prefix, "Attributes".into()),
                body,
            ),
        }
    }
}

fn render_ledge<'a>(body: &mut Element<'a>, url_prefix: &'a str) {
    body.add_child(
        Element::new("div").class("ledge__positioner").child(
            Element::new("div").class("ledge").child(
                Element::new("div")
                    .child(
                        VoidElement::new("input")
                            .attribute("type", "search")
                            .attribute("id", "searchbar")
                            .attribute("placeholder", "Search…")
                            .attribute("autocomplete", "off")
                            .attribute("data-url-prefix", url_prefix),
                    )
                    .child(
                        Element::new("div")
                            .attribute("id", "search-results__positioner")
                            .child(Element::new("div").attribute("id", "search-results")),
                    ),
            ),
        ),
    );
}

fn render_page(head: Element<'_>, body: Element<'_>) -> String {
    let mut html = Element::new("html");
    html.add_attribute("lang", "en");
    html.add_child(head);
    html.add_child(body);

    let mut page = Document::default();
    page.add_child(html);

    let mut content = String::new();
    page.render(&mut content);

    content
}

fn render_head<'a>(component: &Word, url_prefix: &str, title: Node<'a>) -> Element<'a> {
    Element::new("head")
        .child(VoidElement::new("meta").attribute("charset", "utf-8"))
        .child(
            VoidElement::new("meta")
                .attribute("name", "viewport")
                .attribute("content", "width=device-width, initial-scale=1.0"),
        )
        .child(Element::new("title").child(title))
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
                .attribute("href", format!("{url_prefix}{MAIN_STYLE_SHEET_FILE_NAME}")),
        )
        .child(
            Element::new("script")
                .attribute("src", format!("{url_prefix}{MAIN_SCRIPT_FILE_NAME}"))
                .boolean_attribute("defer"),
        )
        .child(
            Element::new("script")
                .attribute(
                    "src",
                    format!("{url_prefix}{component}/{SEARCH_INDEX_FILE_NAME}"),
                )
                .boolean_attribute("defer"),
        )
}

fn render_sidebar<'a>(
    subsections: &subsections::Subsections<'a>,
    url_prefix: &str,
    components: &'a [ComponentOutline],
) -> Element<'a> {
    let mut sidebar = Element::new("div").class("sidebar");

    subsections.render_table_of_contents(&mut sidebar);

    sidebar.add_child(Element::new("div").class("title").child("Components"));

    let mut list = Element::new("ul");

    let mut components: Vec<_> = components.iter().collect();
    components.sort_unstable_by_key(|component| &component.name);

    for component in components {
        let anchor = Element::new("a")
            .attribute("href", format!("{url_prefix}{}/index.html", component.name))
            .child(Node::from(component.name.as_str()));

        list.add_child(Element::new("li").child(anchor));
    }

    sidebar.add_child(list);

    sidebar
}

fn render_declaration_attributes(
    attributes: &Attributes,
    url_prefix: &str,
    parent: &mut Element<'_>,
    text_processor: &TextProcessor<'_>,
    options: &Options,
) {
    let mut labels = Element::new("div").class("labels");

    // @Task sort attributes

    for attribute in &attributes.0 {
        render_declaration_attribute(attribute, &mut labels, url_prefix);
    }

    parent.add_child(labels);

    let mut description = Element::new("div").class("description");

    if let Some(amount) = options.lorem_ipsum {
        for _ in 0..amount {
            description.add_child(Element::new("p").child(LOREM_IPSUM));
        }
    } else if let Some(documentation) = documentation(attributes) {
        // @Task handle errors properly!
        description.add_child(
            text_processor
                .process(documentation, url_prefix.to_owned())
                .unwrap(),
        );
    }

    parent.add_child(description);
}

fn render_declaration_attribute(attribute: &Attribute, parent: &mut Element<'_>, url_prefix: &str) {
    use BareAttribute::*;

    #[allow(clippy::match_same_arms)]
    match &attribute.bare {
        // plain style
        Abstract | Static | If { .. } | Intrinsic | Moving => {
            let name = attribute.bare.name().to_str();

            parent.add_child(
                Element::new("div").class("attribute").child(
                    Element::new("a")
                        .attribute(
                            "href",
                            format!(
                                "{url_prefix}attributes.html#attribute.{}",
                                urlencoding::encode(name)
                            ),
                        )
                        .child(name),
                ),
            );
        }

        // @Task make the path a link; only display this with --document-private-items
        Public(_) => {}

        // @Task incorporate message contain within the attribute etc
        Deprecated(_) => {
            parent.add_child(Element::new("div").class("deprecated").child("deprecated"));
        }
        // @Task incorporate message contain within the attribute etc
        Unstable(_) => parent.add_child(
            Element::new("div")
                .class("experimental")
                .child("experimental"),
        ),
        Unsafe => parent.add_child(Element::new("div").class("unsafe").child("unsafe")),

        // rendered separately
        Doc { .. } => {}

        // not rendered
        Allow { .. }
        | Deny { .. }
        | Forbid { .. }
        | Known
        | Location { .. }
        | RecursionLimit { .. }
        | Statistics
        | Warn { .. } => {}

        // should not exist at this point in time
        Ignore | Include | Test => unreachable!(),
    }
}

fn documentation(attributes: &Attributes) -> Option<String> {
    let content = attributes
        .select::<{ AttributeName::Doc }>()
        .map(|content| content.trim_start_matches(' '))
        .join_with('\n')
        .to_string();

    (!content.is_empty()).then_some(content)
}

#[derive(Default, Clone, Copy)]
pub struct Options {
    pub asciidoc: bool,
    pub lorem_ipsum: Option<usize>,
}

enum SearchItem {
    Declaration(hir::DeclarationIndex),
    Keyword(&'static str),
    ReservedPunctuation(&'static str),
    Attribute(&'static str),
}

struct Page {
    path: PathBuf,
    content: String,
}

// @Task add fast-path / alternative API for words
fn declaration_id(binder: &str) -> String {
    format!("decl.{}", urlencoding::encode(binder))
}

// @Task DRY(lexer, token)

static KEYWORDS: [&str; 16] = [
    "_", "as", "case", "extern", "data", "do", "in", "lazy", "let", "module", "of", "self",
    "super", "topmost", "Type", "use",
];

static RESERVED_PUNCTUATION: [&str; 10] = [".", ":", "=", "\\", "?", "@", "->", "<-", "=>", "::"];

static LOREM_IPSUM: &str = "\
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi \
    ut aliquip ex ea commodo consequat. \
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu \
    fugiat nulla pariatur. \
    Excepteur sint occaecat cupidatat non proident, \
    sunt in culpa qui officia deserunt mollit anim id est laborum.";
