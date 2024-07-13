use super::{
    add_decl_attrs, decl_id, documentation,
    node::{Attributable, Elem, Node},
    text_processor::TextProcessor,
    Options, LOREM_IPSUM,
};
use derivation::Elements;
use hir::{attr::Query as _, AttrName};
use std::borrow::Cow;
use unicode_segmentation::UnicodeSegmentation;

// @Task get rid of this rigid non-extensible struct!
#[derive(Default)]
pub(crate) struct Subsections<'a> {
    // pub(crate) uses: Uses<'a>, // @Task
    pub(crate) modules: Modules<'a>,
    pub(crate) types: Types<'a>,
    pub(crate) funcs: Funcs<'a>,
    pub(crate) attrs: Attrs,
    pub(crate) keywords: Keywords<'a>,
    pub(crate) reserved_symbols: ReservedSymbols<'a>,
}

impl<'a> Subsections<'a> {
    pub(crate) fn reserved_idents() -> Self {
        let mut subsections = Self::default();

        subsections.keywords.0.extend(
            crate::KEYWORDS
                .into_iter()
                .map(|keyword| Keyword { name: keyword }),
        );

        subsections.reserved_symbols.0.extend(
            crate::RESERVED_SYMBOLS
                .into_iter()
                .map(|symbol| ReservedSymbol { name: symbol }),
        );

        subsections
    }

    pub(crate) fn attrs() -> Self {
        let mut subsections = Self::default();
        subsections.attrs.0.extend(AttrName::elements());
        subsections
    }

    pub(crate) fn render_table_of_contents(&self, parent: &mut Elem<'a>) {
        self.modules.render_table_of_contents(parent);
        self.types.render_table_of_contents(parent);
        self.funcs.render_table_of_contents(parent);
        self.attrs.render_table_of_contents(parent);
        self.keywords.render_table_of_contents(parent);
        self.reserved_symbols.render_table_of_contents(parent);
    }

    pub(crate) fn render(
        self,
        url_prefix: &str,
        parent: &mut Elem<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        self.modules.render(parent, options);
        self.types
            .render(url_prefix, parent, text_processor, options);
        self.funcs
            .render(url_prefix, parent, text_processor, options);
        self.attrs.render(parent);
        self.keywords.render(parent);
        self.reserved_symbols.render(parent);
    }
}

pub(crate) trait Subsection<'a> {
    const ID: &'static str;
    const TITLE: &'static str;

    type Subsubsection: Subsubsection<'a>;

    fn subsections(&self) -> &[Self::Subsubsection];

    fn render_table_of_contents(&self, parent: &mut Elem<'a>) {
        if self.subsections().is_empty() {
            return;
        }

        parent.add_child(Elem::anchor(format!("#{}", Self::ID), Self::TITLE).class("title"));

        let mut list = Elem::new("ul");

        let mut subsubsections: Vec<_> = self.subsections().iter().collect();
        // @Task shm sort upper case before lower case per letter (e.g. A > a but M /> a)
        subsubsections.sort_by_key(|subsubsection| subsubsection.title().to_lowercase());

        for subsubsection in subsubsections {
            list.add_child(Elem::new("li").child(Elem::anchor(
                format!("#{}", subsubsection.id()),
                subsubsection.title(),
            )));
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

    fn render_table_of_contents(&self, parent: &mut Elem<'a>) {
        if self.0.is_empty() {
            return;
        }

        parent.add_child(Elem::anchor(format!("#{}", Self::ID), Self::TITLE).class("title"));
    }
}

impl<'a> Modules<'a> {
    fn render(self, parent: &mut Elem<'a>, options: &Options) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        let mut table = Elem::new("table").class("modules indented");

        for module in self.0 {
            let mut table_definition = Elem::new("td");

            // @Task actually, don't take the first *sentence* but the first *paragraph*
            // (once we use asciidoc)
            if let Some(amount) = options.lorem_ipsum {
                if amount != 0 {
                    table_definition
                        .add_child(LOREM_IPSUM.unicode_sentences().next().unwrap_or_default());
                }
            } else if let Some(documentation) = documentation(module.attrs) {
                let first_sentence = documentation
                    .unicode_sentences()
                    .map(ToOwned::to_owned)
                    .next()
                    .unwrap_or_default();
                table_definition.add_child(first_sentence);
            }

            let mut anchor = Elem::anchor(module.id(), module.binder).class("binder");

            if module.attrs.has(AttrName::Deprecated) {
                anchor.add_class("deprecated");
            }

            table.add_child(
                Elem::new("tr")
                    .child(Elem::new("td").child(anchor))
                    .child(table_definition),
            );
        }

        section.add_child(table);
        parent.add_child(section);
    }
}

pub(crate) struct Module<'a> {
    pub(crate) attrs: &'a hir::Attrs,
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
        parent: &mut Elem<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for ty in self.0 {
            let id = decl_id(ty.binder);

            let mut anchor = Elem::anchor(format!("#{id}"), ty.binder).class("binder");

            if ty.attrs.has(AttrName::Deprecated) {
                anchor.add_class("deprecated");
            }

            // @Task render with explicit parameters and without (hiding one of them)
            let mut subheading = Elem::new("h3")
                .attr("id", id.clone())
                .class("subheading declaration")
                .child(anchor)
                .child(": ")
                .child(Node::verbatim(ty.ty));

            subheading.add_child(source_link());

            section.add_child(subheading);

            add_decl_attrs(ty.attrs, url_prefix, &mut section, text_processor, options);

            // @Beacon @Task for abstract data types, only continue if this type is not local
            // and if --doc-priv-decls was not specified
            if ty.ctors.is_empty() || ty.attrs.has(AttrName::Abstract.or(AttrName::Intrinsic)) {
                continue;
            }

            let mut subsection = Elem::new("section")
                .class("indented")
                .child(anchored_subheading(
                    4,
                    format!("constructors.{}", ty.binder),
                    "Constructors",
                ));

            for constructor in ty.ctors {
                let id = decl_id(&format!("{}.{}", ty.binder, constructor.binder));

                // @Task render with explicit parameters and without (hiding one of them)
                subsection.add_child(
                    Elem::new("h5")
                        .attr("id", id.clone())
                        .class("subheading declaration")
                        .child(Elem::anchor(format!("#{id}"), constructor.binder).class("binder"))
                        .child(": ")
                        .child(Node::verbatim(constructor.ty)),
                );

                add_decl_attrs(
                    constructor.attrs,
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
    pub(crate) attrs: &'a hir::Attrs,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expr?
    pub(crate) ty: String,
    pub(crate) ctors: Vec<Constructor<'a>>,
}

impl<'a> Subsubsection<'a> for Type<'a> {
    fn id(&self) -> String {
        decl_id(self.binder)
    }

    fn title(&self) -> &'a str {
        self.binder
    }
}

pub(crate) struct Constructor<'a> {
    pub(crate) attrs: &'a hir::Attrs,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expr?
    pub(crate) ty: String,
}

#[derive(Default)]
pub(crate) struct Funcs<'a>(pub(crate) Vec<Func<'a>>);

impl<'a> Subsection<'a> for Funcs<'a> {
    const ID: &'static str = "functions";
    const TITLE: &'static str = "Functions";

    type Subsubsection = Func<'a>;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl<'a> Funcs<'a> {
    fn render(
        self,
        url_prefix: &str,
        parent: &mut Elem<'a>,
        text_processor: &TextProcessor<'_>,
        options: &Options,
    ) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for func in self.0 {
            let id = decl_id(func.binder);

            let mut anchor = Elem::anchor(format!("#{id}"), func.binder).class("binder");

            if func.attrs.has(AttrName::Deprecated) {
                anchor.add_class("deprecated");
            }

            // @Task render with explicit parameters and without (hiding one of them)
            section.add_child(
                Elem::new("h3")
                    .attr("id", id)
                    .class("subheading declaration")
                    .child(anchor)
                    .child(": ")
                    .child(Node::verbatim(func.ty))
                    .child(source_link()),
            );

            add_decl_attrs(
                func.attrs,
                url_prefix,
                &mut section,
                text_processor,
                options,
            );
        }

        parent.add_child(section);
    }
}

pub(crate) struct Func<'a> {
    pub(crate) attrs: &'a hir::Attrs,
    pub(crate) binder: &'a str,
    // @Question store a hir::Expr?
    pub(crate) ty: String,
}

impl<'a> Subsubsection<'a> for Func<'a> {
    fn id(&self) -> String {
        decl_id(self.binder)
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
    fn render(self, parent: &mut Elem<'a>) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for keyword in self.0 {
            let id = keyword.id();

            section.add_child(
                Elem::new("h3")
                    .attr("id", id.clone())
                    .class("subheading declaration")
                    .child(Elem::anchor(format!("#{id}"), keyword.name).class("binder")),
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
    fn render(self, parent: &mut Elem<'a>) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for reserved_symbol in self.0 {
            let id = reserved_symbol.id();

            section.add_child(
                Elem::new("h3")
                    .attr("id", id.clone())
                    .class("subheading declaration")
                    .child(Elem::anchor(format!("#{id}"), reserved_symbol.name).class("binder")),
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
pub(crate) struct Attrs(pub(crate) Vec<AttrName>);

impl<'a> Subsection<'a> for Attrs {
    const ID: &'static str = "attributes";
    const TITLE: &'static str = "Attrs";

    type Subsubsection = AttrName;

    fn subsections(&self) -> &[Self::Subsubsection] {
        &self.0
    }
}

impl Attrs {
    pub(crate) fn render(self, parent: &mut Elem<'_>) {
        if self.0.is_empty() {
            return;
        }

        let mut section = Elem::new("section").child(anchored_subheading(2, Self::ID, Self::TITLE));

        for attr in self.0 {
            let id = attr.id();

            section.add_child(
                Elem::new("h3")
                    .attr("id", id.clone())
                    .class("subheading declaration")
                    .child(Elem::anchor(format!("#{id}"), attr.to_str()).class("binder")),
            );

            {
                let mut labels = Elem::div("labels");

                if attr.is_internal() {
                    labels.add_child(Elem::div("internal").child("internal"));
                }

                if !attr.is_implemented() {
                    labels.add_child(Elem::div("unsupported").child("not supported yet"));
                }

                section.add_child(labels);
            }
            // @Task include the documentation of each attribute here at compile-time
        }

        parent.add_child(section);
    }
}

impl<'a> Subsubsection<'a> for AttrName {
    fn id(&self) -> String {
        format!("attribute.{}", urlencoding::encode(self.to_str()))
    }

    fn title(&self) -> &'a str {
        self.to_str()
    }
}

fn source_link() -> Elem<'static> {
    // @Task create an actual link!
    Elem::anchor("#", "source")
        .class("source")
        .attr("title", "Go to the source code")
}

fn anchored_subheading<'a>(
    level: u8,
    id: impl Into<Cow<'a, str>>,
    content: impl Into<Cow<'a, str>>,
) -> Elem<'a> {
    fn anchored_subheading<'a>(level: u8, id: Cow<'a, str>, content: Cow<'a, str>) -> Elem<'a> {
        let href = format!("#{id}");

        Elem::new(format!("h{level}"))
            .attr("id", id)
            .class("subheading")
            .child(Elem::anchor(href, content))
    }

    anchored_subheading(level, id.into(), content.into())
}
