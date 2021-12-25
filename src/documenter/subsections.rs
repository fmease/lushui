use super::{
    declaration_id, documentation,
    node::{Attributable, Element, Node},
    render_declaration_attributes,
    text_processor::{self, TextProcessor},
    Options, LOREM_IPSUM,
};
use crate::syntax::lowered_ast::{self, attributes::Query as _, AttributeName};
use std::{borrow::Cow, default::default};
use unicode_segmentation::UnicodeSegmentation;

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

    pub(crate) fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut TextProcessor,
        options: &Options,
    ) {
        self.modules.render(parent, options);
        self.types
            .render(url_prefix, parent, text_processor, options);
        self.functions
            .render(url_prefix, parent, text_processor, options);
        self.attributes
            .render(url_prefix, parent, text_processor, options);
        self.keywords
            .render(url_prefix, parent, text_processor, options);
        self.reserved_punctuation
            .render(url_prefix, parent, text_processor, options);
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

        let mut subsubsections: Vec<_> = self.subsections().iter().collect();
        // @Task shm sort upper case before lower case per letter (e.g. A > a but M /> a)
        subsubsections.sort_by_key(|subsubsection| subsubsection.title().to_lowercase());

        for subsubsection in subsubsections {
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
            } else {
                let first_sentence = documentation(module.attributes)
                    .unicode_sentences()
                    .map(ToOwned::to_owned)
                    .next()
                    .unwrap_or_default();
                table_definition.add_child(first_sentence);
            }

            let mut anchor = Element::new("a")
                .attribute("href", module.id())
                .class("binder")
                .child(module.binder);

            if module.attributes.contains(AttributeName::Deprecated) {
                anchor.add_class("deprecated");
            }

            table.add_child(
                Element::new("tr")
                    .child(Element::new("td").child(anchor))
                    .child(table_definition),
            );
        }

        section.add_child(table);
        parent.add_child(section);
    }
}

pub(super) struct Module<'a> {
    pub(super) attributes: &'a lowered_ast::attributes::Attributes,
    pub(super) binder: &'a str,
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
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut text_processor::TextProcessor,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for type_ in self.0 {
            let id = declaration_id(type_.binder);

            let mut anchor = Element::new("a")
                .class("binder")
                .attribute("href", format!("#{id}"))
                .child(type_.binder);

            if type_.attributes.contains(AttributeName::Deprecated) {
                anchor.add_class("deprecated");
            }

            // @Task render with explicit parameters and without (hiding one of them)
            let mut subheading = Element::new("h3")
                .attribute("id", id.clone())
                .class("subheading declaration")
                .child(anchor)
                .child(": ")
                .child(Node::verbatim(type_.type_));

            if !type_
                .attributes
                .contains(AttributeName::DocAttribute.or(AttributeName::DocReservedIdentifier))
            {
                subheading.add_child(source_link());
            }

            section.add_child(subheading);

            render_declaration_attributes(
                type_.attributes,
                url_prefix,
                &mut section,
                text_processor,
                options,
            );

            // @Beacon @Beacon @Task for abstract data types, only continue if this type is not local
            // and if --doc-priv-decls was not specified
            if type_.constructors.is_empty()
                || type_
                    .attributes
                    .contains(AttributeName::Abstract.or(AttributeName::Intrinsic))
            {
                continue;
            }

            let mut subsection =
                Element::new("section")
                    .class("indented")
                    .child(anchored_subheading(
                        4,
                        format!("constructors.{}", type_.binder),
                        "Constructors",
                    ));

            for constructor in type_.constructors {
                let id = declaration_id(&format!("{}.{}", type_.binder, constructor.binder));

                // @Task render with explicit parameters and without (hiding one of them)
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

                render_declaration_attributes(
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

pub(super) struct Type<'a> {
    pub(super) attributes: &'a lowered_ast::attributes::Attributes,
    pub(super) binder: &'a str,
    // @Question store a hir::Expression?
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
    pub(super) attributes: &'a lowered_ast::attributes::Attributes,
    pub(super) binder: &'a str,
    // @Question store a hir::Expression?
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
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut text_processor::TextProcessor,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for function in self.0 {
            let id = declaration_id(function.binder);

            let mut anchor = Element::new("a")
                .class("binder")
                .attribute("href", format!("#{id}"))
                .child(function.binder);

            if function.attributes.contains(AttributeName::Deprecated) {
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

            render_declaration_attributes(
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

pub(super) struct Function<'a> {
    pub(super) attributes: &'a lowered_ast::attributes::Attributes,
    pub(super) binder: &'a str,
    // @Question store a hir::Expression?
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
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut text_processor::TextProcessor,
        options: &Options,
    ) {
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
                    .child(
                        Element::new("a")
                            .class("binder")
                            .attribute("href", format!("#{id}"))
                            .child(keyword.name),
                    ),
            );

            render_declaration_attributes(
                &default(),
                url_prefix,
                &mut section,
                text_processor,
                options,
            );
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
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut text_processor::TextProcessor,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section =
            Element::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

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

            render_declaration_attributes(
                &default(),
                url_prefix,
                &mut section,
                text_processor,
                options,
            );
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
    pub(super) fn render(
        self,
        url_prefix: &str,
        parent: &mut Element<'a>,
        text_processor: &mut text_processor::TextProcessor,
        options: &Options,
    ) {
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
                    .child(
                        Element::new("a")
                            .class("binder")
                            .attribute("href", format!("#{id}"))
                            .child(attribute.name),
                    ),
            );

            render_declaration_attributes(
                &default(),
                url_prefix,
                &mut section,
                text_processor,
                options,
            );
        }

        parent.add_child(section);
    }
}

pub(super) struct Attribute<'a> {
    pub(super) name: &'a str,
}

impl<'a> Subsubsection<'a> for Attribute<'a> {
    fn id(&self) -> String {
        format!("attribute.{}", urlencoding::encode(self.name))
    }

    fn title(&self) -> &'a str {
        self.name
    }
}

fn source_link() -> Element<'static> {
    // @Task create an actual link!
    Element::new("a")
        .class("source")
        .attribute("href", "#")
        .attribute("title", "Go to the source code")
        .child("source")
}

fn anchored_subheading<'a>(
    level: u8,
    id: impl Into<Cow<'a, str>>,
    content: impl Into<Cow<'a, str>>,
) -> Element<'a> {
    fn anchored_subheading<'a>(level: u8, id: Cow<'a, str>, content: Cow<'a, str>) -> Element<'a> {
        Element::new(format!("h{level}"))
            .attribute("id", id.clone())
            .class("subheading")
            .child(
                Element::new("a")
                    .attribute("href", format!("#{id}"))
                    .child(content),
            )
    }

    anchored_subheading(level, id.into(), content.into())
}
