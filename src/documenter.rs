//! The documentation generator.

// @Tasks:
// * add descriptions extracted from documentation comments
// * link to source (generate html files per source file)
// * search functionality using levenshtein
// * description next to search result
// * manually switching between a light and a dark mode + automatic mode detection
// * asciidoc integration
// * print attributes (and link them)
// * de-lower parameter lists
// * add fonts via @font-face and use them (properly define font-weights of h1 etc)
// * convert ttfs to woffs (1 + 2) and change @font-faces
// * fill the COPYRIGHT file
// * render unstable bindings special
// * render deprecated bindings struck-through
// * add filter feature (only show @abstract, @public, …)

use crate::{
    diagnostics::Reporter,
    error::Result,
    hir,
    package::{BuildSession, CrateType, PackageIndex},
    resolver::Crate,
    syntax::{
        ast,
        lowered_ast::{self, AttributeKeys, AttributeKind},
        CrateName,
    },
    utility::obtain,
};
use node::{Attributable, Document, Element, Node, VoidElement};
use std::{borrow::Cow, default::default, fs, path::PathBuf};

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

pub fn document(
    declaration: &hir::Declaration,
    options: Options,
    crates: &[CrateSketch],
    crate_: &Crate,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<()> {
    let mut documenter = Documenter::new(options, crates, crate_, session, reporter);

    documenter.document_declaration(declaration)?;
    documenter.collect_search_items(declaration);
    documenter.write()?;

    Ok(())
}

struct Documenter<'a> {
    options: Options,
    crates: &'a [CrateSketch],
    crate_: &'a Crate,
    session: &'a BuildSession,
    #[allow(dead_code)]
    reporter: &'a Reporter,
    pages: Vec<Page>,
    search_items: Vec<SearchItem>,
    destination: PathBuf,
}

impl<'a> Documenter<'a> {
    fn new(
        options: Options,
        crate_names: &'a [CrateSketch],
        crate_: &'a Crate,
        session: &'a BuildSession,
        reporter: &'a Reporter,
    ) -> Self {
        let destination = session.build_folder().join(DOCUMENTATION_FOLDER_NAME);

        Self {
            options,
            crates: crate_names,
            crate_,
            session,
            reporter,
            pages: default(),
            search_items: default(),
            destination,
        }
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

        for page in &self.pages {
            let parent = page.path.parent().unwrap();

            if !parent.exists() {
                fs::create_dir(parent).unwrap();
            }

            fs::write(&page.path, &page.content).unwrap();
        }

        // @Task use BufWriter
        // @Task put it in self.destination instead once the search index contains all crates
        fs::write(
            self.destination
                .join(self.crate_.name.as_str())
                .join(SEARCH_INDEX_FILE_NAME),
            {
                let mut search_index = String::from("window.searchIndex=[");

                for search_item in &self.search_items {
                    match search_item {
                        &SearchItem::Declaration(index) => {
                            // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                            // incorrect on top of that!)
                            let path = self
                                .crate_
                                .local_path_to_string(index.local_index(self.crate_).unwrap());

                            search_index += &format!(
                                "[{path:?},{:?}],",
                                format::declaration_url_fragment(index, self.crate_, self.session)
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
                                format!(
                                    "attributes.html#attribute.{}",
                                    urlencoding::encode(binder)
                                )
                            );
                        }
                    }
                }

                search_index += "];";
                search_index
            },
        )
        .unwrap();

        Ok(())
    }

    // @Task merge this with `document_declaration` (once we move out `generate_module_page` out of it!)
    // to avoid traversing the crate graph too often
    fn collect_search_items(&mut self, declaration: &hir::Declaration) {
        match &declaration.value {
            hir::DeclarationKind::Function(function) => {
                self.search_items.push(SearchItem::Declaration(
                    function.binder.declaration_index().unwrap(),
                ));
            }
            hir::DeclarationKind::Data(type_) => {
                if declaration
                    .attributes
                    .has(AttributeKeys::DOC_RESERVED_IDENTIFIER)
                {
                    self.search_items.push(
                        SearchItem::ReservedIdentifier(
                            declaration.attributes
                                .get(|attribute| obtain!(attribute, AttributeKind::DocReservedIdentifier { name } => name))
                                .clone()
                        )
                    );
                } else if declaration.attributes.has(AttributeKeys::DOC_ATTRIBUTE) {
                    self.search_items.push(
                        SearchItem::Attribute(
                            declaration.attributes
                                .get(|attribute| obtain!(attribute, AttributeKind::DocAttribute { name } => name))
                                .clone()
                        )
                    );
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
                if !declaration.attributes.has(AttributeKeys::DOC_ATTRIBUTES)
                    && !declaration
                        .attributes
                        .has(AttributeKeys::DOC_RESERVED_IDENTIFIERS)
                {
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
                        binder: function.binder.as_str(),
                        type_: format::format_expression(
                            &function.type_annotation,
                            url_prefix,
                            &self.options,
                            self.crate_,
                            self.session,
                        ),
                    });
                }
                hir::DeclarationKind::Data(type_) => {
                    if declaration
                        .attributes
                        .has(AttributeKeys::DOC_RESERVED_IDENTIFIER)
                    {
                        let binder = declaration.attributes
                            .get(|attribute| obtain!(attribute, AttributeKind::DocReservedIdentifier { name } => name));

                        if ast::Identifier::new_unchecked(binder.as_str().into(), default())
                            .is_word()
                        {
                            subsections
                                .keywords
                                .0
                                .push(subsections::Keyword { name: binder });
                        } else {
                            subsections
                                .reserved_punctuation
                                .0
                                .push(subsections::SingleReservedPunctuation { name: binder });
                        }
                    } else if declaration.attributes.has(AttributeKeys::DOC_ATTRIBUTE) {
                        let binder = declaration.attributes
                            .get(|attribute| obtain!(attribute, AttributeKind::DocAttribute { name } => name));

                        subsections
                            .attributes
                            .0
                            .push(subsections::Attribute { binder });
                    } else {
                        let mut formatted_constructors = Vec::new();

                        if let Some(constructors) = &type_.constructors {
                            for declaration in constructors {
                                let constructor = declaration.constructor().unwrap();

                                formatted_constructors.push(subsections::Constructor {
                                    binder: constructor.binder.as_str(),
                                    type_: format::format_expression(
                                        &constructor.type_annotation,
                                        url_prefix,
                                        &self.options,
                                        self.crate_,
                                        self.session,
                                    ),
                                });
                            }
                        }

                        subsections.types.0.push(subsections::Type {
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
                                url_prefix,
                                &self.options,
                                self.crate_,
                                self.session,
                            ),
                            constructors: formatted_constructors,
                        });
                    }
                }
                hir::DeclarationKind::Module(module) => {
                    subsections.modules.0.push(subsections::Module {
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

    fn render_module_page(&mut self, module: &hir::Module, attributes: &lowered_ast::Attributes) {
        let is_attribute_page = attributes.has(AttributeKeys::DOC_ATTRIBUTES);
        let is_reserved_identifier_page = attributes.has(AttributeKeys::DOC_RESERVED_IDENTIFIERS);

        let index = module.binder.local_declaration_index(self.crate_).unwrap();
        let path_segments = self.crate_.local_path_segments(index);
        let page_depth = if is_attribute_page || is_reserved_identifier_page {
            0
        } else {
            path_segments.len()
        };

        let url_prefix = format!("./{}", "../".repeat(page_depth));

        let mut head = Element::new("head")
            .child(VoidElement::new("meta").attribute("charset", "utf-8"))
            .child(
                VoidElement::new("meta")
                    .attribute("name", "viewport")
                    .attribute("content", "width=device-width, initial-scale=1.0"),
            )
            // @Task respect self.crate_.is_ambiguously_named_within_package
            .child(Element::new("title").child(if is_attribute_page {
                "Attributes".into()
            } else if is_reserved_identifier_page {
                "Reserved Identifiers".into()
            } else {
                self.crate_.local_path_to_string(index)
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
                        format!("{url_prefix}/{}/{SEARCH_INDEX_FILE_NAME}", self.crate_.name),
                    )
                    .boolean_attribute("defer"),
            );
        let description = &self.session[self.crate_.package].description;
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
                                .attribute("data-url-prefix", &url_prefix),
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

        let subsections = self.collect_subsections(module, &url_prefix);

        {
            let mut table_of_contents = Element::new("div").class("sidebar");
            subsections.render_table_of_contents(&mut table_of_contents);
            container.add_child(table_of_contents);
        }

        // main content
        {
            let mut main = Element::new("section").class("main");

            // main heading
            {
                let mut heading = Element::new("h1");

                if is_attribute_page {
                    heading.add_child("Attributes");
                } else if is_reserved_identifier_page {
                    heading.add_child("Reserved Identifiers");
                } else {
                    heading.add_child(match index == self.crate_.root() {
                        true => "Crate",
                        false => "Module",
                    });

                    heading.add_child(" ");

                    for (position, &path_segment) in path_segments.iter().enumerate() {
                        let is_last_segment = position + 1 == page_depth;

                        let mut anchor = Element::new("a").attribute(
                            "href",
                            format!("{}index.html", "../".repeat(page_depth - 1 - position)),
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

            subsections.render(&mut main);

            container.add_child(main);
        }

        // crates side bar
        {
            let mut sidebar = Element::new("div").class("sidebar");
            sidebar.add_child(Element::new("div").class("title").child("Crates"));

            let mut list = Element::new("ul");

            for crate_ in self.crates {
                let anchor = Element::new("a")
                    .attribute(
                        "href",
                        format!(
                            "{url_prefix}{}{}/index.html",
                            crate_.name,
                            // @Note this does not scale to multiple binaries per package
                            if crate_.type_ == CrateType::Binary
                                && crate_.is_ambiguously_named_within_package
                            {
                                ".binary"
                            } else {
                                ""
                            }
                        ),
                    )
                    .child(
                        if crate_.package == self.session.goal_package()
                            && crate_.is_ambiguously_named_within_package
                        {
                            Node::from(format!("{} ({})", crate_.name, crate_.type_))
                        } else {
                            Node::from(crate_.name.as_str())
                        },
                    );

                list.add_child(Element::new("li").child(anchor));
            }

            sidebar.add_child(list);
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

        let path = if is_attribute_page {
            self.destination.join("attributes.html")
        } else if is_reserved_identifier_page {
            self.destination.join("reserved.html")
        } else {
            let mut path = self.destination.clone();
            let mut path_segments = path_segments.into_iter();

            if self.crate_.is_binary() && self.crate_.is_ambiguously_named_within_package {
                path_segments.next();
                // @Note does not scale to multiple binaries per package
                path.push(format!("{}.binary", self.crate_.name));
            }

            for segment in path_segments {
                path.push(segment);
            }

            path.join("index.html")
        };

        self.pages.push(Page { path, content });
    }
}

#[derive(Default)]
pub struct Options {
    pub no_core: bool,
}

// @Note ideally, we wouldn't need this extra boiled-down version of Crate
// but unfortunately, BuildSession currently cannot store unbuilt crates
pub struct CrateSketch {
    name: CrateName,
    type_: CrateType,
    package: PackageIndex,
    is_ambiguously_named_within_package: bool,
}

impl CrateSketch {
    pub fn new(crate_: &Crate) -> Self {
        Self {
            name: crate_.name.clone(),
            type_: crate_.type_,
            package: crate_.package,
            is_ambiguously_named_within_package: crate_.is_ambiguously_named_within_package,
        }
    }
}

enum SearchItem {
    Declaration(hir::DeclarationIndex),
    ReservedIdentifier(String),
    Attribute(String),
}

fn anchored_heading<'a>(
    level: u8,
    id: impl Into<Cow<'a, str>>,
    content: impl Into<Cow<'a, str>>,
) -> Element<'a> {
    fn anchored_heading<'a>(level: u8, id: Cow<'a, str>, content: Cow<'a, str>) -> Element<'a> {
        Element::new(format!("h{level}"))
            .attribute("id", id.clone())
            .class("subheading")
            .child(
                Element::new("a")
                    .attribute("href", format!("#{id}"))
                    .child(content),
            )
    }
    anchored_heading(level, id.into(), content.into())
}

mod subsections {
    use super::{
        anchored_heading, declaration_id,
        node::{Attributable, Element, Node},
        LOREM_IPSUM,
    };

    #[derive(Default)]
    pub(super) struct Subsections<'a> {
        // re_exports: (),
        pub(super) modules: Modules<'a>,
        pub(super) types: Types<'a>,
        pub(super) functions: Functions<'a>,
        pub(super) attributes: Attributes<'a>,
        pub(super) keywords: Keywords<'a>,
        pub(super) reserved_punctuation: ReservedPunctuation<'a>,
    }

    impl<'a> Subsections<'a> {
        pub(super) fn render_table_of_contents(&self, parent: &mut Element<'a>) {
            self.modules.render_table_of_contents(parent);
            self.types.render_table_of_contents(parent);
            self.functions.render_table_of_contents(parent);
            self.attributes.render_table_of_contents(parent);
            self.keywords.render_table_of_contents(parent);
            self.reserved_punctuation.render_table_of_contents(parent);
        }

        pub fn render(self, parent: &mut Element<'a>) {
            self.modules.render(parent);
            self.types.render(parent);
            self.functions.render(parent);
            self.attributes.render(parent);
            self.keywords.render(parent);
            self.reserved_punctuation.render(parent);
        }
    }

    pub(super) trait Subsection<'a> {
        const ID: &'static str;
        const TITLE: &'static str;

        type Subsubsection: Subsubsection<'a>;

        fn subsections(&self) -> &[Self::Subsubsection];

        fn render_table_of_contents(&self, parent: &mut Element<'a>) {
            if self.subsections().is_empty() {
                return;
            }

            parent.add_child(
                Element::new("a")
                    .attribute("href", format!("#{}", Self::ID))
                    .class("title")
                    .child(Self::TITLE),
            );

            let mut list = Element::new("ul");

            for subsubsection in self.subsections() {
                list.add_child(
                    Element::new("li").child(
                        Element::new("a")
                            .attribute("href", format!("#{}", subsubsection.id()))
                            .child(subsubsection.title()),
                    ),
                );
            }

            parent.add_child(list);
        }
    }

    pub(super) trait Subsubsection<'a> {
        fn id(&self) -> String;
        fn title(&self) -> &'a str;
    }

    #[derive(Default)]
    pub(super) struct Modules<'a>(pub(super) Vec<Module<'a>>);

    impl<'a> Subsection<'a> for Modules<'a> {
        const ID: &'static str = "modules";
        const TITLE: &'static str = "Modules";

        type Subsubsection = Module<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }

        fn render_table_of_contents(&self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            parent.add_child(
                Element::new("a")
                    .attribute("href", format!("#{}", Self::ID))
                    .class("title")
                    .child(Self::TITLE),
            );
        }
    }

    impl<'a> Modules<'a> {
        fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            let mut table = Element::new("table").class("indent");

            for module in self.0 {
                table.add_child(
                    Element::new("tr")
                        .child(
                            Element::new("td").child(
                                Element::new("a")
                                    .attribute("href", module.id())
                                    .child(module.binder),
                            ),
                        )
                        .child(Element::new("td").child(module.description)),
                );
            }

            section.add_child(table);
            parent.add_child(section);
        }
    }

    pub(super) struct Module<'a> {
        pub(super) binder: &'a str,
        pub(super) description: String,
    }

    impl<'a> Subsubsection<'a> for Module<'a> {
        fn id(&self) -> String {
            format!("{}/index.html", urlencoding::encode(self.binder))
        }

        fn title(&self) -> &'a str {
            self.binder
        }
    }

    #[derive(Default)]
    pub(super) struct Types<'a>(pub(super) Vec<Type<'a>>);

    impl<'a> Subsection<'a> for Types<'a> {
        const ID: &'static str = "types";
        const TITLE: &'static str = "Data Types";

        type Subsubsection = Type<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }
    }

    impl<'a> Types<'a> {
        fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            for type_ in self.0 {
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

                // @Temporary
                section.add_child(Element::new("p").child(LOREM_IPSUM));

                // @Task don't do that for abstract or intrinsic types
                // @Question what if there are no constructors?
                let mut subsection =
                    Element::new("section")
                        .class("indent")
                        .child(anchored_heading(
                            4,
                            format!("constructors.{}", type_.binder),
                            "Constructors",
                        ));

                for constructor in type_.constructors {
                    let id = declaration_id(&format!("{}.{}", type_.binder, constructor.binder));

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

            parent.add_child(section);
        }
    }

    pub(super) struct Type<'a> {
        pub(super) _attributes: String,
        pub(super) binder: &'a str,
        pub(super) type_: String,
        pub(super) constructors: Vec<Constructor<'a>>,
    }

    impl<'a> Subsubsection<'a> for Type<'a> {
        fn id(&self) -> String {
            declaration_id(self.binder)
        }

        fn title(&self) -> &'a str {
            self.binder
        }
    }

    pub(super) struct Constructor<'a> {
        pub(super) binder: &'a str,
        pub(super) type_: String,
    }

    #[derive(Default)]
    pub(super) struct Functions<'a>(pub(super) Vec<Function<'a>>);

    impl<'a> Subsection<'a> for Functions<'a> {
        const ID: &'static str = "functions";
        const TITLE: &'static str = "Functions";

        type Subsubsection = Function<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }
    }

    impl<'a> Functions<'a> {
        fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            for function in self.0 {
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

            parent.add_child(section);
        }
    }

    pub(super) struct Function<'a> {
        pub(super) binder: &'a str,
        pub(super) type_: String,
    }

    impl<'a> Subsubsection<'a> for Function<'a> {
        fn id(&self) -> String {
            declaration_id(self.binder)
        }

        fn title(&self) -> &'a str {
            self.binder
        }
    }

    #[derive(Default)]
    pub(super) struct Keywords<'a>(pub(super) Vec<Keyword<'a>>);

    impl<'a> Subsection<'a> for Keywords<'a> {
        const ID: &'static str = "keywords";
        const TITLE: &'static str = "Keywords";

        type Subsubsection = Keyword<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }
    }

    impl<'a> Keywords<'a> {
        fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            for keyword in self.0 {
                let id = keyword.id();

                section.add_child(
                    Element::new("h3")
                        .attribute("id", id.clone())
                        .class("subheading declaration")
                        .child(
                            Element::new("a")
                                .class("binder")
                                .attribute("href", format!("#{id}"))
                                .child(keyword.name),
                        ),
                );

                // @Temporary
                section.add_child(Element::new("p").child(LOREM_IPSUM));
            }

            parent.add_child(section);
        }
    }

    pub(super) struct Keyword<'a> {
        pub(super) name: &'a str,
    }

    impl<'a> Subsubsection<'a> for Keyword<'a> {
        fn id(&self) -> String {
            format!("word.{}", urlencoding::encode(self.name))
        }

        fn title(&self) -> &'a str {
            self.name
        }
    }

    #[derive(Default)]
    pub(super) struct ReservedPunctuation<'a>(pub(super) Vec<SingleReservedPunctuation<'a>>);

    impl<'a> Subsection<'a> for ReservedPunctuation<'a> {
        const ID: &'static str = "punctuation";
        const TITLE: &'static str = "Reserved Punctuation";

        type Subsubsection = SingleReservedPunctuation<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }
    }

    impl<'a> ReservedPunctuation<'a> {
        fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            for reserved_punctuation in self.0 {
                let id = reserved_punctuation.id();

                section.add_child(
                    Element::new("h3")
                        .attribute("id", id.clone())
                        .class("subheading declaration")
                        .child(
                            Element::new("a")
                                .class("binder")
                                .attribute("href", format!("#{id}"))
                                .child(reserved_punctuation.name),
                        ),
                );

                // @Temporary
                section.add_child(Element::new("p").child(LOREM_IPSUM));
            }

            parent.add_child(section);
        }
    }

    pub(super) struct SingleReservedPunctuation<'a> {
        pub(super) name: &'a str,
    }

    impl<'a> Subsubsection<'a> for SingleReservedPunctuation<'a> {
        fn id(&self) -> String {
            format!("punctuation.{}", urlencoding::encode(self.name))
        }

        fn title(&self) -> &'a str {
            self.name
        }
    }

    #[derive(Default)]
    pub(super) struct Attributes<'a>(pub(super) Vec<Attribute<'a>>);

    impl<'a> Subsection<'a> for Attributes<'a> {
        const ID: &'static str = "attributes";
        const TITLE: &'static str = "Attributes";

        type Subsubsection = Attribute<'a>;

        fn subsections(&self) -> &[Self::Subsubsection] {
            &self.0
        }
    }

    impl<'a> Attributes<'a> {
        pub(super) fn render(self, parent: &mut Element<'a>) {
            if self.0.is_empty() {
                return;
            }

            let mut section =
                Element::new("section").child(anchored_heading(2, Self::ID, Self::TITLE));

            for attribute in self.0 {
                let id = attribute.id();

                section.add_child(
                    Element::new("h3")
                        .attribute("id", id.clone())
                        .class("subheading declaration")
                        .child(
                            Element::new("a")
                                .class("binder")
                                .attribute("href", format!("#{id}"))
                                .child(attribute.binder),
                        ),
                );

                // @Temporary
                section.add_child(Element::new("p").child(LOREM_IPSUM));
            }

            parent.add_child(section);
        }
    }

    pub(super) struct Attribute<'a> {
        pub(super) binder: &'a str,
    }

    impl<'a> Subsubsection<'a> for Attribute<'a> {
        fn id(&self) -> String {
            format!("attribute.{}", urlencoding::encode(self.binder))
        }

        fn title(&self) -> &'a str {
            self.binder
        }
    }
}

struct Page {
    path: PathBuf,
    // @Temporary
    content: String,
}

fn declaration_id(binder: &str) -> String {
    format!("decl.{}", urlencoding::encode(binder))
}
