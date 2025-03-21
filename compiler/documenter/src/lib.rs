//! The documentation generator.
#![feature(decl_macro)]

// @Bug We currently store the documentation of each component (be it a target component,
// a direct dependency or a transitive dependency) flat in a single folder meaning we
// don't properly handle components that are distinct but share the same name (which we
// generally want to support contrary to Cargo). Concretely, we overwrite folders in such
// cases leading to a broken links and missing pages.

use crossbeam::thread::Scope;
use derivation::Elements;
use diagnostics::error::Result;
use hir::{Attribute, AttributeName, Attributes, BareAttribute};
use hir_format::ComponentExt;
use joinery::JoinableIterator;
use lexer::word::Word;
use node::{Attributable, Document, Element, Node, VoidElement};
use session::{
    Context, OUTPUT_FOLDER_NAME, Session,
    component::{DeclarationIndexExt, IdentifierExt},
    package::{ManifestPath, Package},
};
use std::{
    fs,
    io::BufWriter,
    path::{Path, PathBuf},
};
use text_processor::TextProcessor;
use utility::{Atom, default};

mod fonts;
mod format;
mod node;
mod subsections;
mod text_processor;

const DOC_FOLDER_NAME: &str = "doc";
const STYLE_SHEET_FILE_NAME: &str = "style.min.css";
const SCRIPT_FILE_NAME: &str = "script.min.js";
const SEARCH_INDEX_FILE_NAME: &str = "search-index.min.js";

// @Task make this an unstable option instead
const DEVELOPING: bool = true;

/// Document the given component.
///
/// Returns the path to the index page (of the component).
pub fn document_component(
    component_root: &hir::Declaration,
    options: Options,
    session: &Session<'_>,
) -> Result<()> {
    crossbeam::scope(|scope| {
        let mut documenter = Documenter::new(options, session, scope).unwrap();

        // @Beacon @Bug this re-creates these special pages with every component!
        //         @Task do it only once!
        documenter.pages.push(documenter.reserved_identifiers_page());
        documenter.pages.push(documenter.attributes_page());

        // @Task don't document the same package twice!
        if let Some(package) = session.package() {
            documenter.pages.push(documenter.package_page(package));
        }

        documenter.document_declaration(component_root)?;
        documenter.collect_search_items(component_root);
        documenter.persist().unwrap();
        documenter.text_processor.destruct().unwrap();

        Ok(())
    })
    .unwrap()
}

pub fn index_page(context: &Context) -> PathBuf {
    let mut path = Documenter::folder(context);
    if context.root_package().is_none() {
        path.push(context.root_component().name.to_str());
    }
    path.push("index.html");
    path
}

struct Documenter<'a, 'scope> {
    options: Options,
    session: &'a Session<'a>,
    text_processor: TextProcessor<'scope>,
    pages: Vec<Page>,
    search_items: Vec<SearchItem>,
    path: PathBuf,
}

impl<'a, 'scope> Documenter<'a, 'scope> {
    fn new(
        options: Options,
        session: &'a Session<'a>,
        scope: &'scope Scope<'a>,
    ) -> Result<Self, std::io::Error> {
        let path = Self::folder(session.context());

        if !path.exists() {
            fs::create_dir_all(&path)?;
        }

        let text_processor = TextProcessor::new(&path, &options, session, scope)?;

        Ok(Self {
            options,
            session,
            text_processor,
            pages: default(),
            search_items: default(),
            path,
        })
    }

    fn folder(context: &Context) -> PathBuf {
        match context.root_package() {
            Some(package) => {
                let package = &context[package];
                let mut path = package.folder().to_path_buf();
                path.push(OUTPUT_FOLDER_NAME);
                path.push(DOC_FOLDER_NAME);
                path.push(package.name.to_str());
                path
            }
            None => {
                Path::new(context.root_component().name.to_str()).with_extension(DOC_FOLDER_NAME)
            }
        }
    }

    /// Persist the generated documentation to disk.
    // @Beacon @Bug this re-creates the CSS, JS and fonts with every component!
    //         @Task do it only once!
    fn persist(&self) -> Result<(), std::io::Error> {
        // @Task handle the case where two+ components (target and/or (transitive) deps)
        // have the same name and are being documented

        {
            let path = self.path.join(STYLE_SHEET_FILE_NAME);
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
            let path = self.path.join(SCRIPT_FILE_NAME);
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

        let component_name = self.session.component().name().to_str();
        let component_path = self.path.join(component_name);

        if !component_path.exists() {
            fs::create_dir(&component_path)?;
        }

        // @Task use BufWriter
        // @Task put it in self.path instead once the search index contains all components
        fs::write(component_path.join(SEARCH_INDEX_FILE_NAME), {
            let mut search_index = String::from("window.searchIndex=[");

            for search_item in &self.search_items {
                use std::fmt::Write;

                match *search_item {
                    SearchItem::Declaration(index) => {
                        // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                        // incorrect on top of that!)
                        let path = self.session.component().local_index_with_root_to_extern_path(
                            index.local(self.session).unwrap(),
                            component_name.to_owned(),
                        );

                        write!(
                            search_index,
                            "[{path:?},{:?}],",
                            format::declaration_url_fragment(index, self.session)
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
                    SearchItem::ReservedSymbol(symbol) => {
                        write!(
                            search_index,
                            "[{symbol:?},{:?}],",
                            format!("identifiers.html#symbol.{}", urlencoding::encode(symbol))
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

        Ok(())
    }

    fn collect_search_items(&mut self, component_root: &hir::Declaration) {
        for keyword in KEYWORDS {
            self.search_items.push(SearchItem::Keyword(keyword));
        }

        for symbol in RESERVED_SYMBOLS {
            self.search_items.push(SearchItem::ReservedSymbol(symbol));
        }

        for attribute in AttributeName::elements() {
            self.search_items.push(SearchItem::Attribute(attribute.to_str()));
        }

        self.collect_declaration_search_items(component_root);
    }

    fn collect_declaration_search_items(&mut self, declaration: &hir::Declaration) {
        match &declaration.bare {
            hir::BareDeclaration::Function(function) => {
                self.search_items
                    .push(SearchItem::Declaration(function.binder.declaration_index().unwrap()));
            }
            hir::BareDeclaration::Data(type_) => {
                self.search_items
                    .push(SearchItem::Declaration(type_.binder.declaration_index().unwrap()));

                if let Some(constructors) = &type_.constructors {
                    for declaration in constructors {
                        self.collect_declaration_search_items(declaration);
                    }
                }
            }
            hir::BareDeclaration::Constructor(constructor) => {
                self.search_items
                    .push(SearchItem::Declaration(constructor.binder.declaration_index().unwrap()));
            }
            hir::BareDeclaration::Module(module) => {
                self.search_items
                    .push(SearchItem::Declaration(module.binder.declaration_index().unwrap()));

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
            let page = self.add_module_page(module, &declaration.attributes);
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
                        binder: function.binder.to_str(),
                        type_: format::format_expression(&function.type_, url_prefix, self.session),
                    });
                }
                hir::BareDeclaration::Data(type_) => {
                    let mut formatted_constructors = Vec::new();

                    if let Some(constructors) = &type_.constructors {
                        for declaration in constructors {
                            let constructor = declaration.bare.constructor().unwrap();

                            formatted_constructors.push(subsections::Constructor {
                                attributes: &declaration.attributes,
                                binder: constructor.binder.to_str(),
                                type_: format::format_expression(
                                    &constructor.type_,
                                    url_prefix,
                                    self.session,
                                ),
                            });
                        }
                    }

                    subsections.types.0.push(subsections::Type {
                        // @Task write a custom formatter for this which prints links to the documentation
                        // of the specific attributes
                        attributes: &declaration.attributes,
                        binder: type_.binder.to_str(),
                        type_: format::format_expression(&type_.type_, url_prefix, self.session),
                        constructors: formatted_constructors,
                    });
                }
                hir::BareDeclaration::Module(module) => {
                    subsections.modules.0.push(subsections::Module {
                        attributes: &declaration.attributes,
                        binder: module.binder.to_str(),
                    });
                }
                // @Task re-exports, only public//(public topmost) for --doc-priv-decls??) ones!
                // hir::DeclarationKind::Use(_) => todo!(),
                _ => {}
            }
        }

        subsections
    }

    fn reserved_identifiers_page(&self) -> Page {
        let url_prefix = "./";
        let mut body = Element::new("body").child(ledge(url_prefix, None, None));
        let mut container = Element::div("container");

        let subsections = subsections::Subsections::reserved_identifiers();
        container.add_child(sidebar(
            &subsections,
            url_prefix,
            default(),
            vec![self.session.root_component().name],
        ));

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
                head(self.session.component().name(), url_prefix, "Reserved Identifiers".into()),
                body,
            ),
        }
    }

    fn attributes_page(&self) -> Page {
        let url_prefix = "./";
        let mut body = Element::new("body").child(ledge(url_prefix, None, None));
        let mut container = Element::div("container");

        let subsections = subsections::Subsections::attributes();
        container.add_child(sidebar(
            &subsections,
            url_prefix,
            default(),
            vec![self.session.root_component().name],
        ));

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
                head(self.session.component().name(), url_prefix, "Attributes".into()),
                body,
            ),
        }
    }

    fn package_page(&self, package: ManifestPath) -> Page {
        let url_prefix = "./";
        let package = &self.session[package];
        let name = &package.name;

        let mut body = Element::new("body").child(ledge(url_prefix, Some(package), None));
        let mut container = Element::div("container");

        container.add_child(sidebar(
            &default(),
            url_prefix,
            default(),
            vec![self.session.root_component().name],
        ));

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");
                heading.add_child(format!("Package {name} {}", package.version.0));
                main.add_child(heading);
            }

            main.add_child(Element::div("description").child(&package.description));

            // @Task add `section.subsection`s!

            {
                main.add_child(Element::new("h2").child("Components"));

                let mut list = Element::new("ul");

                for component in package.components.keys() {
                    list.add_child(Element::new("li").child(Element::anchor(
                        format!("{url_prefix}{component}/index.html"),
                        component.to_str(),
                    )));
                }

                main.add_child(list);
            }

            container.add_child(main);
        }

        body.add_child(container);

        Page {
            path: self.path.join("index.html"),
            content: render_page(
                head(
                    // @Temporary hack
                    Word::new_unchecked(Atom::UNDERSCORE),
                    url_prefix,
                    format!("Package {name}").into(),
                ),
                body,
            ),
        }
    }

    fn add_module_page(&self, module: &hir::Module, attributes: &Attributes) -> Page {
        let index = module.binder.local_declaration_index(self.session).unwrap();
        let component_name = self.session.component().name();

        let mut segments = self.session.component().local_index_to_path_segments(index);
        segments.push_front(component_name.into_inner());
        let page_depth = segments.len();
        let url_prefix = format!("./{}", "../".repeat(page_depth));

        let mut body = Element::new("body").child(ledge(
            &url_prefix,
            self.session.package().map(|package| &self.session[package]),
            Some(component_name),
        ));

        let mut container = Element::div("container");

        let subsections = self.collect_module_subsections(module, &url_prefix);
        container.add_child(sidebar(
            &subsections,
            &url_prefix,
            self.session.component().dependencies().keys().copied().collect(),
            vec![self.session.root_component().name],
        ));

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");

                heading.add_child(match index == self.session.component().root_local() {
                    true => "Component",
                    false => "Module",
                });

                heading.add_child(" ");

                for (position, path_segment) in segments.iter().enumerate() {
                    let is_last_segment = position + 1 == page_depth;

                    heading.add_child(Element::anchor(
                        format!("{}index.html", "../".repeat(page_depth - 1 - position)),
                        path_segment.to_str(),
                    ));

                    if !is_last_segment {
                        // @Task add extra spacing (left or right, depends) for
                        // non-word module names
                        heading.add_child(Node::verbatim(".<wbr>"));
                    }
                }

                main.add_child(heading);
            }

            add_declaration_attributes(
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
            .session
            .component()
            .local_index_with_root_to_extern_path(index, component_name.to_string());

        Page {
            path: segments.into_iter().map(Atom::to_str).chain(Some("index.html")).collect(),
            content: render_page(head(component_name, &url_prefix, title.into()), body),
        }
    }
}

fn ledge<'a>(
    url_prefix: &'a str,
    package: Option<&'a Package>,
    component: Option<Word>,
) -> Element<'a> {
    let mut context = Element::div("name");

    if let Some(package) = package {
        // @Beacon @Task actual link
        context.add_child(
            Element::new("a")
                .attribute("href", format!("{url_prefix}index.html"))
                .child(Element::span("package").child(package.name.to_str()))
                .child(" ")
                .child(&package.version.0),
        );
    }

    if let Some(component) = component {
        if package.is_some() {
            context.add_child(" / ");
        }

        // @Beacon @Task actual link
        context.add_child(
            Element::anchor(format!("{url_prefix}{component}/index.html"), component.to_str())
                .class("component"),
        );
    }

    Element::div("ledge-container")
        .child(Element::div("ledge").child(context).child(search_bar(url_prefix)))
}

fn search_bar(url_prefix: &str) -> Element<'_> {
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
                .attribute("id", "search-results-container")
                .child(Element::new("div").attribute("id", "search-results")),
        )
}

fn render_page(head: Element<'_>, body: Element<'_>) -> String {
    Document::default()
        .child(Element::new("html").attribute("lang", "en").child(head).child(body))
        .render()
}

// @Task remove `component` param
fn head<'a>(component: Word, url_prefix: &str, title: Node<'a>) -> Element<'a> {
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
                // @Task insert version smh
                .attribute("content", "lushui documenter"),
        )
        .child(
            VoidElement::new("link")
                .attribute("rel", "stylesheet")
                .attribute("href", format!("{url_prefix}{STYLE_SHEET_FILE_NAME}")),
        )
        .child(
            Element::new("script")
                .attribute("src", format!("{url_prefix}{SCRIPT_FILE_NAME}"))
                .boolean_attribute("defer"),
        )
        .child(
            Element::new("script")
                .attribute(
                    "src",
                    // @Task only create a single search index
                    format!("{url_prefix}{component}/{SEARCH_INDEX_FILE_NAME}"),
                )
                .boolean_attribute("defer"),
        )
}

fn sidebar<'a>(
    subsections: &subsections::Subsections<'a>,
    url_prefix: &str,
    dependencies: Vec<Word>,
    root_components: Vec<Word>,
) -> Element<'a> {
    let mut sidebar = Element::div("sidebar");

    subsections.render_table_of_contents(&mut sidebar);

    if !dependencies.is_empty() {
        sidebar.add_child(
            Element::div("title").child("Dependencies").attribute("title", "Direct Dependencies"),
        );

        let mut list = Element::new("ul");
        for dependency in dependencies {
            // @Bug this is incorrect for components that share the same name
            let anchor = Element::anchor(
                format!("{url_prefix}{dependency}/index.html"),
                Node::from(dependency.to_str()),
            );

            list.add_child(Element::new("li").child(anchor));
        }

        sidebar.add_child(list);
    }

    {
        sidebar
            .add_child(Element::div("title").child("Roots").attribute("title", "Root Components"));

        let mut list = Element::new("ul");
        for component in root_components {
            let anchor = Element::anchor(
                format!("{url_prefix}{component}/index.html"),
                Node::from(component.to_str()),
            );

            list.add_child(Element::new("li").child(anchor));
        }

        sidebar.add_child(list);
    }

    sidebar
}

fn add_declaration_attributes(
    attributes: &Attributes,
    url_prefix: &str,
    parent: &mut Element<'_>,
    text_processor: &TextProcessor<'_>,
    options: &Options,
) {
    let mut labels = Element::div("labels");

    // @Task sort attributes

    for attribute in &attributes.0 {
        add_declaration_attribute(attribute, &mut labels, url_prefix);
    }

    parent.add_child(labels);

    let mut description = Element::div("description");

    if let Some(amount) = options.lorem_ipsum {
        for _ in 0..amount {
            description.add_child(Element::new("p").child(LOREM_IPSUM));
        }
    } else if let Some(documentation) = documentation(attributes) {
        // @Task handle errors properly!
        description
            .add_child(text_processor.process(documentation, url_prefix.to_owned()).unwrap());
    }

    parent.add_child(description);
}

fn add_declaration_attribute(attribute: &Attribute, parent: &mut Element<'_>, url_prefix: &str) {
    use BareAttribute::*;

    #[allow(clippy::match_same_arms)]
    match &attribute.bare {
        // plain style
        Abstract | Static | If { .. } | Intrinsic { .. } | Moving => {
            let name = attribute.bare.name().to_str();

            parent.add_child(Element::div("attribute").child(Element::anchor(
                format!("{url_prefix}attributes.html#attribute.{}", urlencoding::encode(name)),
                name,
            )));
        }

        // @Task make the path a link; only display this with --document-private-items
        Public(_) => {}

        // @Task incorporate message contain within the attribute etc
        Deprecated(_) => {
            parent.add_child(Element::div("deprecated").child("deprecated"));
        }
        // @Task incorporate message contain within the attribute etc
        Unstable(_) => parent.add_child(Element::div("experimental").child("experimental")),
        Unsafe => parent.add_child(Element::div("unsafe").child("unsafe")),

        // Rendered separately.
        Doc { .. } => {}

        // Not rendered since they are not part of the API.
        Allow { .. }
        | Deny { .. }
        | Forbid { .. }
        | Known { .. }
        | Location { .. }
        | RecursionLimit { .. }
        | Statistics
        | Warn { .. } => {}

        // Not rendered since they are truly internal and not part of the surface language.
        Context | Record | Trait => {}

        // Should not exist at this stage.
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
    ReservedSymbol(&'static str),
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

static KEYWORDS: [&str; 15] = [
    "_", "as", "case", "extern", "data", "do", "in", "lazy", "let", "module", "of", "self",
    "super", "topmost", "use",
];

static RESERVED_SYMBOLS: [&str; 10] = [".", ":", "=", "\\", "?", "@", "->", "<-", "=>", "::"];

static LOREM_IPSUM: &str = "\
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi \
    ut aliquip ex ea commodo consequat. \
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu \
    fugiat nulla pariatur. \
    Excepteur sint occaecat cupidatat non proident, \
    sunt in culpa qui officia deserunt mollit anim id est laborum.";
