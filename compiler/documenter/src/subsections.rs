use super::{
    LOREM_IPSUM, Options, add_declaration_attributes, declaration_id, documentation,
    node::{Attributable, Element, Node},
    text_processor::TextProcessor,
};
use derivation::Elements;
use hir::{AttributeName, attribute::Query as _};
use std::borrow::Cow;
use unicode_segmentation::UnicodeSegmentation;

// @Task get rid of this rigid non-extensible struct!
#[derive(Default)]
pub(crate) struct Subsections<'a> {
    // pub(crate) uses: Uses<'a>, // @Task
    pub(crate) modules: Modules<'a>,
    pub(crate) types: Types<'a>,
    pub(crate) functions: Functions<'a>,
    pub(crate) attributes: Attributes,
    pub(crate) keywords: Keywords<'a>,
    pub(crate) reserved_symbols: ReservedSymbols<'a>,
}

impl<'a> Subsections<'a> {
    pub(crate) fn reserved_identifiers() -> Self {
        let mut subsections = Self::default();

        subsections
            .keywords
            .0
            .extend(crate::KEYWORDS.into_iter().map(|keyword| Keyword { name: keyword }));

        subsections.reserved_symbols.0.extend(
            crate::RESERVED_SYMBOLS.into_iter().map(|symbol| ReservedSymbol { name: symbol }),
        );

        subsections
    }

    pub(crate) fn attributes() -> Self {
        let mut subsections = Self::default();
        subsections.attributes.0.extend(AttributeName::elements());
        subsections
    }

    pub(crate) fn render_table_of_contents(&self, parent: &mut Element<'a>) {
        self.modules.render_table_of_contents(parent);
        self.types.render_table_of_contents(parent);
        self.functions.render_table_of_contents(parent);
        self.attributes.render_table_of_contents(parent);
        self.keywords.render_table_of_contents(parent);
        self.reserved_symbols.render_table_of_contents(parent);
    }

    pub(crate) fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        self.modules.render(parent, options);
        self.types.render(url_prefix, parent, text_processor, options);
        self.functions.render(url_prefix, parent, text_processor, options);
        self.attributes.render(parent);
        self.keywords.render(parent);
        self.reserved_symbols.render(parent);
    }
}

pub(crate) trait Subsection<'a> {
    const ID: &'static str;
    const TITLE: &'static str;

    type Subsubsection: Subsubsection<'a>;

    fn subsections(&self) -> &[Self::Subsubsection];

    fn render_table_of_contents(&self, parent: &mut Element<'a>) {
        if self.subsections().is_empty() {
            return;
        }

        parent.add_child(Element::anchor(format!("#{}", Self::ID), Self::TITLE).class("title"));

        let mut list = Element::new("ul");

        let mut subsubsections: Vec<_> = self.subsections().iter().collect();
        // @Task shm sort upper case before lower case per letter (e.g. A > a but M /> a)
        subsubsections.sort_by_key(|subsubsection| subsubsection.title().to_lowercase());

        for subsubsection in subsubsections {
            list.add_child(
                Element::new("li").child(Element::anchor(
                    format!("#{}", subsubsection.id()),
                    subsubsection.title(),
                )),
            );
        }

        parent.add_child(list);
    }
}

pub(crate) trait Subsubsection<'a> {
    fn id(&self) -> String;
    fn title(&self) -> &'a str;
}

#[derive(Default)]
pub(crate) struct Modules<'a>(pub(crate) Vec<Module<'a>>);

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

        parent.add_child(Element::anchor(format!("#{}", Self::ID), Self::TITLE).class("title"));
    }
}

impl<'a> Modules<'a> {
    fn render(self, parent: &mut Element<'a>, options: &Options) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        let mut table = Element::new("table").class("modules indented");

        for module in self.0 {
            let mut table_definition = Element::new("td");

            // @Task actually, don't take the first *sentence* but the first *paragraph*
            // (once we use asciidoc)
            if let Some(amount) = options.lorem_ipsum {
                if amount != 0 {
                    table_definition
                        .add_child(LOREM_IPSUM.unicode_sentences().next().unwrap_or_default());
                }
            } else if let Some(documentation) = documentation(module.attributes) {
                let first_sentence = documentation
                    .unicode_sentences()
                    .map(ToOwned::to_owned)
                    .next()
                    .unwrap_or_default();
                table_definition.add_child(first_sentence);
            }

            let mut anchor = Element::anchor(module.id(), module.binder).class("binder");

            if module.attributes.has(AttributeName::Deprecated) {
                anchor.add_class("deprecated");
            }

            table.add_child(
                Element::new("tr").child(Element::new("td").child(anchor)).child(table_definition),
            );
        }

        section.add_child(table);
        parent.add_child(section);
    }
}

pub(crate) struct Module<'a> {
    pub(crate) attributes: &'a hir::Attributes,
    pub(crate) binder: &'a str,
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
pub(crate) struct Types<'a>(pub(crate) Vec<Type<'a>>);

impl<'a> Subsection<'a> for Types<'a> {
    const ID: &'static str = "types";
    const TITLE: &'static str = "Data Types";

    type Subsubsection = Type<'a>;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl<'a> Types<'a> {
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for type_ in self.0 {
            let id = declaration_id(type_.binder);

            let mut anchor = Element::anchor(format!("#{id}"), type_.binder).class("binder");

            if type_.attributes.has(AttributeName::Deprecated) {
                anchor.add_class("deprecated");
            }

            // @Task render with explicit parameters and without (hiding one of them)
            let mut subheading = Element::new("h3")
                .attribute("id", id.clone())
                .class("subheading declaration")
                .child(anchor)
                .child(": ")
                .child(Node::verbatim(type_.type_));

            subheading.add_child(source_link());

            section.add_child(subheading);

            add_declaration_attributes(
                type_.attributes,
                url_prefix,
                &mut section,
                text_processor,
                options,
            );

            // @Beacon @Task for abstract data types, only continue if this type is not local
            // and if --doc-priv-decls was not specified
            if type_.constructors.is_empty()
                || type_.attributes.has(AttributeName::Abstract.or(AttributeName::Intrinsic))
            {
                continue;
            }

            let mut subsection = Element::new("section").class("indented").child(
                anchored_subheading(4, format!("constructors.{}", type_.binder), "Constructors"),
            );

            for constructor in type_.constructors {
                let id = declaration_id(&format!("{}.{}", type_.binder, constructor.binder));

                // @Task render with explicit parameters and without (hiding one of them)
                subsection.add_child(
                    Element::new("h5")
                        .attribute("id", id.clone())
                        .class("subheading declaration")
                        .child(
                            Element::anchor(format!("#{id}"), constructor.binder).class("binder"),
                        )
                        .child(": ")
                        .child(Node::verbatim(constructor.type_)),
                );

                add_declaration_attributes(
                    constructor.attributes,
                    url_prefix,
                    &mut subsection,
                    text_processor,
                    options,
                );
            }

            section.add_child(subsection);
        }

        parent.add_child(section);
    }
}

pub(crate) struct Type<'a> {
    pub(crate) attributes: &'a hir::Attributes,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expression?
    pub(crate) type_: String,
    pub(crate) constructors: Vec<Constructor<'a>>,
}

impl<'a> Subsubsection<'a> for Type<'a> {
    fn id(&self) -> String {
        declaration_id(self.binder)
    }

    fn title(&self) -> &'a str {
        self.binder
    }
}

pub(crate) struct Constructor<'a> {
    pub(crate) attributes: &'a hir::Attributes,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expression?
    pub(crate) type_: String,
}

#[derive(Default)]
pub(crate) struct Functions<'a>(pub(crate) Vec<Function<'a>>);

impl<'a> Subsection<'a> for Functions<'a> {
    const ID: &'static str = "functions";
    const TITLE: &'static str = "Functions";

    type Subsubsection = Function<'a>;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl<'a> Functions<'a> {
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for function in self.0 {
            let id = declaration_id(function.binder);

            let mut anchor = Element::anchor(format!("#{id}"), function.binder).class("binder");

            if function.attributes.has(AttributeName::Deprecated) {
                anchor.add_class("deprecated");
            }

            // @Task render with explicit parameters and without (hiding one of them)
            section.add_child(
                Element::new("h3")
                    .attribute("id", id)
                    .class("subheading declaration")
                    .child(anchor)
                    .child(": ")
                    .child(Node::verbatim(function.type_))
                    .child(source_link()),
            );

            add_declaration_attributes(
                function.attributes,
                url_prefix,
                &mut section,
                text_processor,
                options,
            );
        }

        parent.add_child(section);
    }
}

pub(crate) struct Function<'a> {
    pub(crate) attributes: &'a hir::Attributes,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expression?
    pub(crate) type_: String,
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
pub(crate) struct Keywords<'a>(pub(crate) Vec<Keyword<'a>>);

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
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for keyword in self.0 {
            let id = keyword.id();

            section.add_child(
                Element::new("h3")
                    .attribute("id", id.clone())
                    .class("subheading declaration")
                    .child(Element::anchor(format!("#{id}"), keyword.name).class("binder")),
            );

            // @Task include the documentation of each keyword here at compile-time
        }

        parent.add_child(section);
    }
}

pub(crate) struct Keyword<'a> {
    pub(crate) name: &'a str,
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
pub(crate) struct ReservedSymbols<'a>(pub(crate) Vec<ReservedSymbol<'a>>);

impl<'a> Subsection<'a> for ReservedSymbols<'a> {
    const ID: &'static str = "symbols";
    const TITLE: &'static str = "Reserved Symbols";

    type Subsubsection = ReservedSymbol<'a>;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl<'a> ReservedSymbols<'a> {
    fn render(self, parent: &mut Element<'a>) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for reserved_symbol in self.0 {
            let id = reserved_symbol.id();

            section.add_child(
                Element::new("h3")
                    .attribute("id", id.clone())
                    .class("subheading declaration")
                    .child(Element::anchor(format!("#{id}"), reserved_symbol.name).class("binder")),
            );

            // @Task include the documentation of each reserved symbol here at compile-time
        }

        parent.add_child(section);
    }
}

pub(crate) struct ReservedSymbol<'a> {
    pub(crate) name: &'a str,
}

impl<'a> Subsubsection<'a> for ReservedSymbol<'a> {
    fn id(&self) -> String {
        format!("symbol.{}", urlencoding::encode(self.name))
    }

    fn title(&self) -> &'a str {
        self.name
    }
}

#[derive(Default)]
pub(crate) struct Attributes(pub(crate) Vec<AttributeName>);

impl Subsection<'_> for Attributes {
    const ID: &'static str = "attributes";
    const TITLE: &'static str = "Attributes";

    type Subsubsection = AttributeName;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl Attributes {
    pub(crate) fn render(self, parent: &mut Element<'_>) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for attribute in self.0 {
            let id = attribute.id();

            section.add_child(
                Element::new("h3")
                    .attribute("id", id.clone())
                    .class("subheading declaration")
                    .child(Element::anchor(format!("#{id}"), attribute.to_str()).class("binder")),
            );

            {
                let mut labels = Element::div("labels");

                if attribute.is_internal() {
                    labels.add_child(Element::div("internal").child("internal"));
                }

                if !attribute.is_implemented() {
                    labels.add_child(Element::div("unsupported").child("not supported yet"));
                }

                section.add_child(labels);
            }
            // @Task include the documentation of each attribute here at compile-time
        }

        parent.add_child(section);
    }
}

impl<'a> Subsubsection<'a> for AttributeName {
    fn id(&self) -> String {
        format!("attribute.{}", urlencoding::encode(self.to_str()))
    }

    fn title(&self) -> &'a str {
        self.to_str()
    }
}

fn source_link() -> Element<'static> {
    // @Task create an actual link!
    Element::anchor("#", "source").class("source").attribute("title", "Go to the source code")
}

fn anchored_subheading<'a>(
    level: u8,
    id: impl Into<Cow<'a, str>>,
    content: impl Into<Cow<'a, str>>,
) -> Element<'a> {
    fn anchored_subheading<'a>(level: u8, id: Cow<'a, str>, content: Cow<'a, str>) -> Element<'a> {
        let href = format!("#{id}");

        Element::new(format!("h{level}"))
            .attribute("id", id)
            .class("subheading")
            .child(Element::anchor(href, content))
    }

    anchored_subheading(level, id.into(), content.into())
}
