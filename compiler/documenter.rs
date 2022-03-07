//! The documentation generator.

// @Task instead of showing all components in the session in the left pane, have two sections:
//     (1) the dependencies of the target(s) (formerly known as "goals")
//     (2) the dependencies of the component currently viewed
// If one currently views the docs of a target (that is most of the time), they are identical (duh)
// and obviously, only show the list once. Don't show empty sections (obviously)

use crate::{
    component::{Component, ComponentMetadata, ComponentType},
    error::Result,
    hir::{self, LocalDeclarationIndex},
    session::BuildSession,
    syntax::{
        ast,
        lowered_ast::{
            attributes::Query as _, Attribute, AttributeKind, AttributeName, Attributes,
        },
    },
    utility::condition,
};
use crossbeam::thread::Scope;
use joinery::JoinableIterator;
use node::{Attributable, Document, Element, Node, VoidElement};
use std::{default::default, fs, path::PathBuf};
use text_processor::TextProcessor;

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

pub fn document(
    declaration: &hir::Declaration,
    options: Options,
    components: &[ComponentMetadata],
    component: &Component,
    session: &BuildSession,
) -> Result<()> {
    crossbeam::scope(|scope| {
        let mut documenter =
            Documenter::new(options, components, component, session, scope).unwrap();

        documenter.document_declaration(declaration)?;
        documenter.collect_search_items(declaration);
        documenter.write().unwrap();
        documenter.text_processor.destruct().unwrap();

        Ok(())
    })
    .unwrap()
}

struct Documenter<'a, 'scope> {
    options: Options,
    components: &'a [ComponentMetadata],
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
        components: &'a [ComponentMetadata],
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

    fn write(&self) -> Result<(), std::io::Error> {
        // @Task handle the case where two+ components (goal and/or (transitive) deps)
        // have the same name and are being documented

        {
            let path = self.path.join(MAIN_STYLE_SHEET_FILE_NAME);
            // @Task instead of `DEVELOPING`, compare hash (maybe)?
            if DEVELOPING || !path.exists() {
                static STYLE_SHEET: &str = include_str!("documenter/static/css/style.css");
                fs::write(path, minifier::css::minify(STYLE_SHEET).unwrap())?;
            }
        }

        {
            let path = self.path.join("fonts.min.css");
            if !path.exists() {
                static STYLE_SHEET: &str = include_str!("documenter/static/css/fonts.css");
                fs::write(path, minifier::css::minify(STYLE_SHEET).unwrap())?;
            }
        }

        {
            let path = self.path.join(MAIN_SCRIPT_FILE_NAME);
            // @Task instead of `DEVELOPING`, compare hash (maybe)?
            if DEVELOPING || !path.exists() {
                static SCRIPT: &str = include_str!("documenter/static/js/script.js");
                fs::write(path, minifier::js::minify(SCRIPT))?;
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
                match search_item {
                    &SearchItem::Declaration(index) => {
                        // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                        // incorrect on top of that!)
                        let path = self
                            .component
                            .extern_path_to_string(index.local(self.component).unwrap());

                        search_index += &format!(
                            "[{path:?},{:?}],",
                            format::declaration_url_fragment(index, self.component, self.session)
                        );
                    }
                    SearchItem::ReservedIdentifier(name) => {
                        let kind =
                            if ast::Identifier::new_unchecked(name.as_str().into(), default())
                                .is_word()
                            {
                                "word"
                            } else {
                                "punctuation"
                            };

                        search_index += &format!(
                            "[{name:?},{:?}],",
                            format!("reserved.html#{kind}.{}", urlencoding::encode(name))
                        );
                    }
                    SearchItem::Attribute(binder) => {
                        search_index += &format!(
                            "[{binder:?},{:?}],",
                            format!("attributes.html#attribute.{}", urlencoding::encode(binder))
                        );
                    }
                }
            }

            search_index += "];";
            search_index
        })?;

        Ok(())
    }

    // @Task merge this with `document_declaration` (once we move out `generate_module_page` out of it!)
    // to avoid traversing the component graph too often
    fn collect_search_items(&mut self, declaration: &hir::Declaration) {
        match &declaration.value {
            hir::DeclarationKind::Function(function) => {
                self.search_items.push(SearchItem::Declaration(
                    function.binder.declaration_index().unwrap(),
                ));
            }
            hir::DeclarationKind::Data(type_) => {
                if let Some(name) = declaration
                    .attributes
                    .get::<{ AttributeName::DocReservedIdentifier }>()
                {
                    self.search_items
                        .push(SearchItem::ReservedIdentifier(name.to_owned()));
                } else if let Some(name) = declaration
                    .attributes
                    .get::<{ AttributeName::DocAttribute }>()
                {
                    self.search_items
                        .push(SearchItem::Attribute(name.to_owned()));
                } else {
                    self.search_items.push(SearchItem::Declaration(
                        type_.binder.declaration_index().unwrap(),
                    ));

                    if let Some(constructors) = &type_.constructors {
                        for declaration in constructors {
                            self.collect_search_items(declaration);
                        }
                    }
                }
            }
            hir::DeclarationKind::Constructor(constructor) => {
                self.search_items.push(SearchItem::Declaration(
                    constructor.binder.declaration_index().unwrap(),
                ));
            }
            hir::DeclarationKind::Module(module) => {
                if !declaration.attributes.contains(
                    AttributeName::DocAttributes.or(AttributeName::DocReservedIdentifiers),
                ) {
                    self.search_items.push(SearchItem::Declaration(
                        module.binder.declaration_index().unwrap(),
                    ));
                }

                for declaration in &module.declarations {
                    self.collect_search_items(declaration);
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
            self.render_module_page(module, &declaration.attributes);

            for declaration in &module.declarations {
                self.document_declaration(declaration)?;
            }
        }

        Ok(())
    }

    fn collect_subsections<'m>(
        &mut self,
        module: &'m hir::Module,
        url_prefix: &str,
    ) -> subsections::Subsections<'m> {
        let mut subsections = subsections::Subsections::default();

        for declaration in &module.declarations {
            match &declaration.value {
                hir::DeclarationKind::Function(function) => {
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
                hir::DeclarationKind::Data(type_) => {
                    if let Some(name) = declaration
                        .attributes
                        .get::<{ AttributeName::DocReservedIdentifier }>()
                    {
                        if ast::Identifier::new_unchecked(name.into(), default()).is_word() {
                            subsections.keywords.0.push(subsections::Keyword { name });
                        } else {
                            subsections
                                .reserved_punctuation
                                .0
                                .push(subsections::SingleReservedPunctuation { name });
                        }
                    } else if let Some(name) = declaration
                        .attributes
                        .get::<{ AttributeName::DocAttribute }>()
                    {
                        subsections
                            .attributes
                            .0
                            .push(subsections::Attribute { name });
                    } else {
                        let mut formatted_constructors = Vec::new();

                        if let Some(constructors) = &type_.constructors {
                            for declaration in constructors {
                                let constructor = declaration.constructor().unwrap();

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
                }
                hir::DeclarationKind::Module(module) => {
                    if declaration.attributes.contains(
                        AttributeName::DocAttributes.or(AttributeName::DocReservedIdentifiers),
                    ) {
                        continue;
                    }

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

    fn render_module_page(&mut self, module: &hir::Module, attributes: &Attributes) {
        let content_type = condition! {
            attributes.contains(AttributeName::DocAttributes) => PageContentType::Attributes,
            attributes.contains(AttributeName::DocReservedIdentifiers) => PageContentType::ReservedIdentifiers,
            else => PageContentType::Module,
        };

        let index = module
            .binder
            .local_declaration_index(self.component)
            .unwrap();
        let path_segments = self.component.extern_path_segments(index);
        let page_depth = match content_type {
            PageContentType::Module => path_segments.len(),
            PageContentType::Attributes | PageContentType::ReservedIdentifiers => 0,
        };
        let url_prefix = format!("./{}", "../".repeat(page_depth));

        let mut body = Element::new("body");

        // ledge
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
                                .attribute("data-url-prefix", &url_prefix),
                        )
                        .child(
                            Element::new("div")
                                .attribute("id", "search-results__positioner")
                                .child(Element::new("div").attribute("id", "search-results")),
                        ),
                ),
            ),
        );

        let mut container = Element::new("div").class("container");

        let subsections = self.collect_subsections(module, &url_prefix);

        // sidebar
        {
            let mut sidebar = Element::new("div").class("sidebar");

            subsections.render_table_of_contents(&mut sidebar);

            sidebar.add_child(Element::new("div").class("title").child("Components"));

            let mut list = Element::new("ul");

            let mut components: Vec<_> = self.components.iter().collect();
            components.sort_unstable_by_key(|component| &component.name);

            for component in components {
                let anchor = Element::new("a")
                    .attribute(
                        "href",
                        format!(
                            "{url_prefix}{}{}/index.html",
                            component.name,
                            // @Note this does not scale to multiple executables per package
                            if component.type_ == ComponentType::Executable
                                && component.is_ambiguously_named_within_package
                            {
                                ".exec"
                            } else {
                                ""
                            }
                        ),
                    )
                    .child(
                        if component.package == self.session.goal_package()
                            && component.is_ambiguously_named_within_package
                        {
                            Node::from(format!("{} ({})", component.name, component.type_))
                        } else {
                            Node::from(component.name.as_str())
                        },
                    );

                list.add_child(Element::new("li").child(anchor));
            }

            sidebar.add_child(list);
            container.add_child(sidebar);
        }

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");

                match content_type {
                    PageContentType::Module => {
                        heading.add_child(match index == self.component.local_root() {
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
                    }
                    PageContentType::Attributes => heading.add_child("Attributes"),
                    PageContentType::ReservedIdentifiers => {
                        heading.add_child("Reserved Identifiers")
                    }
                }

                main.add_child(heading);
            }

            render_declaration_attributes(
                attributes,
                &url_prefix,
                &mut main,
                &mut self.text_processor,
                &self.options,
            );

            subsections.render(
                &url_prefix,
                &mut main,
                &mut self.text_processor,
                &self.options,
            );

            container.add_child(main);
        }

        body.add_child(container);

        let mut html = Element::new("html");
        html.add_attribute("lang", "en");
        html.add_child(self.render_document_head(index, &url_prefix, content_type));
        html.add_child(body);

        let mut document = Document::default();
        document.add_child(html);

        let mut content = String::new();
        document.render(&mut content);

        let path = match content_type {
            PageContentType::Module => {
                let mut path = self.path.clone();
                let mut path_segments = path_segments.into_iter();

                if self.component.is_executable()
                    && self.component.metadata.is_ambiguously_named_within_package
                {
                    path_segments.next();
                    // @Note does not scale to multiple binaries per package
                    path.push(format!("{}.exec", self.component.name()));
                }

                for segment in path_segments {
                    path.push(segment);
                }

                path.join("index.html")
            }
            PageContentType::Attributes => self.path.join("attributes.html"),
            PageContentType::ReservedIdentifiers => self.path.join("reserved.html"),
        };

        self.pages.push(Page { path, content });
    }

    fn render_document_head(
        &self,
        index: LocalDeclarationIndex,
        url_prefix: &str,
        content_type: PageContentType,
    ) -> Element<'_> {
        let mut head = Element::new("head")
            .child(VoidElement::new("meta").attribute("charset", "utf-8"))
            .child(
                VoidElement::new("meta")
                    .attribute("name", "viewport")
                    .attribute("content", "width=device-width, initial-scale=1.0"),
            )
            .child(Element::new("title").child(match content_type {
                // @Task respect self.component.is_ambiguously_named_within_package
                PageContentType::Module => self.component.extern_path_to_string(index),
                PageContentType::Attributes => "Attributes".into(),
                PageContentType::ReservedIdentifiers => "Reserved Identifiers".into(),
            }))
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
                        // @Temporary
                        format!(
                            "{url_prefix}/{}/{SEARCH_INDEX_FILE_NAME}",
                            self.component.name()
                        ),
                    )
                    .boolean_attribute("defer"),
            );
        let description = &self.component.package(self.session).description;
        if !description.is_empty() {
            head.add_child(
                VoidElement::new("meta")
                    .attribute("name", "description")
                    .attribute("content", description),
            );
        }

        head
    }
}

fn render_declaration_attributes(
    attributes: &Attributes,
    url_prefix: &str,
    parent: &mut Element<'_>,
    text_processor: &mut TextProcessor<'_>,
    options: &Options,
) {
    let mut labels = Element::new("div").class("labels");

    // @Task sort attributes

    for attribute in &attributes.0 {
        render_declaration_attribute(attribute, &mut labels, url_prefix);
    }

    // @Task currently only for some stuff
    // labels.add_child(Element::new("div").class("internal").child("internal"));

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
    use AttributeKind::*;

    #[allow(clippy::match_same_arms)]
    match &attribute.value {
        // plain style
        Abstract | Static | If { .. } | Intrinsic | Moving => {
            let name = attribute.value.name().to_str();

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
            parent.add_child(Element::new("div").class("deprecated").child("deprecated"))
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
        | DocAttributes
        | DocAttribute { .. }
        | DocReservedIdentifier { .. }
        | DocReservedIdentifiers
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

    (!content.is_empty()).then(|| content)
}

#[derive(Default)]
pub struct Options {
    pub asciidoc: bool,
    pub lorem_ipsum: Option<usize>,
}

#[derive(Clone, Copy)]
enum PageContentType {
    Module,
    Attributes,
    ReservedIdentifiers,
}

enum SearchItem {
    Declaration(hir::DeclarationIndex),
    ReservedIdentifier(String),
    Attribute(String),
}

struct Page {
    path: PathBuf,
    content: String,
}

fn declaration_id(binder: &str) -> String {
    format!("decl.{}", urlencoding::encode(binder))
}

static LOREM_IPSUM: &str = "\
    Lorem ipsum dolor sit amet, consectetur adipiscing elit, \
    sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. \
    Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi \
    ut aliquip ex ea commodo consequat. \
    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu \
    fugiat nulla pariatur. \
    Excepteur sint occaecat cupidatat non proident, \
    sunt in culpa qui officia deserunt mollit anim id est laborum.";
