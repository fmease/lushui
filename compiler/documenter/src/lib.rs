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
use hir::{Attr, AttrName, Attrs, BareAttr};
use hir_format::ComponentExt;
use joinery::JoinableIterator;
use lexer::word::Word;
use node::{Attributable, Document, Elem, Node, VoidElem};
use session::{
    component::{DeclIdxExt, IdentExt},
    package::{ManifestPath, Package},
    Context, Session, OUTPUT_FOLDER_NAME,
};
use std::{
    fs,
    io::BufWriter,
    path::{Path, PathBuf},
};
use text_processor::TextProcessor;
use utility::{default, Atom};

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
pub fn document_comp(comp_root: &hir::Decl, opts: Options, sess: &Session<'_>) -> Result<()> {
    crossbeam::scope(|scope| {
        let mut d = Documenter::new(opts, sess, scope).unwrap();

        // @Beacon @Bug this re-creates these special pages with every component!
        //         @Task do it only once!
        d.pages.push(d.reserved_idents_page());
        d.pages.push(d.attrs_page());

        // @Task don't document the same package twice!
        if let Some(package) = sess.pkg() {
            d.pages.push(d.package_page(package));
        }

        d.document_decl(comp_root)?;
        d.collect_search_items(comp_root);
        d.persist().unwrap();
        d.text_processor.destruct().unwrap();

        Ok(())
    })
    .unwrap()
}

pub fn index_page(cx: &Context) -> PathBuf {
    let mut path = Documenter::folder(cx);
    if cx.root_pkg().is_none() {
        path.push(cx.root_comp().name.to_str());
    }
    path.push("index.html");
    path
}

struct Documenter<'a, 'scope> {
    opts: Options,
    sess: &'a Session<'a>,
    text_processor: TextProcessor<'scope>,
    pages: Vec<Page>,
    search_items: Vec<SearchItem>,
    path: PathBuf,
}

impl<'a, 'scope> Documenter<'a, 'scope> {
    fn new(
        opts: Options,
        sess: &'a Session<'a>,
        scope: &'scope Scope<'a>,
    ) -> Result<Self, std::io::Error> {
        let path = Self::folder(sess.cx());

        if !path.exists() {
            fs::create_dir_all(&path)?;
        }

        let text_processor = TextProcessor::new(&path, &opts, sess, scope)?;

        Ok(Self {
            opts,
            sess,
            text_processor,
            pages: default(),
            search_items: default(),
            path,
        })
    }

    fn folder(cx: &Context) -> PathBuf {
        match cx.root_pkg() {
            Some(package) => {
                let package = &cx[package];
                let mut path = package.folder().to_path_buf();
                path.push(OUTPUT_FOLDER_NAME);
                path.push(DOC_FOLDER_NAME);
                path.push(package.name.to_str());
                path
            }
            None => Path::new(cx.root_comp().name.to_str()).with_extension(DOC_FOLDER_NAME),
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

        let component_name = self.sess.comp().name().to_str();
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
                    SearchItem::Decl(index) => {
                        // @Beacon @Task don't use the to_string variant, so we don't need to split() JS (which would be
                        // incorrect on top of that!)
                        let path = self.sess.comp().local_idx_with_root_to_extern_path(
                            index.local(self.sess).unwrap(),
                            component_name.to_owned(),
                        );

                        write!(
                            search_index,
                            "[{path:?},{:?}],",
                            format::decl_url_fragment(index, self.sess)
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
                    SearchItem::Attr(binder) => {
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

    fn collect_search_items(&mut self, comp_root: &hir::Decl) {
        for keyword in KEYWORDS {
            self.search_items.push(SearchItem::Keyword(keyword));
        }

        for symbol in RESERVED_SYMBOLS {
            self.search_items.push(SearchItem::ReservedSymbol(symbol));
        }

        for attr in AttrName::elements() {
            self.search_items.push(SearchItem::Attr(attr.to_str()));
        }

        self.collect_decl_search_items(comp_root);
    }

    fn collect_decl_search_items(&mut self, decl: &hir::Decl) {
        match &decl.bare {
            hir::BareDecl::Func(func) => {
                self.search_items
                    .push(SearchItem::Decl(func.binder.decl_idx().unwrap()));
            }
            hir::BareDecl::DataTy(ty) => {
                self.search_items
                    .push(SearchItem::Decl(ty.binder.decl_idx().unwrap()));

                if let Some(ctors) = &ty.ctors {
                    for ctor in ctors {
                        self.collect_decl_search_items(ctor);
                    }
                }
            }
            hir::BareDecl::Ctor(ctor) => {
                self.search_items
                    .push(SearchItem::Decl(ctor.binder.decl_idx().unwrap()));
            }
            hir::BareDecl::Module(module) => {
                self.search_items
                    .push(SearchItem::Decl(module.binder.decl_idx().unwrap()));

                for declaration in &module.decls {
                    self.collect_decl_search_items(declaration);
                }
            }
            // @Task re-exports
            // hir::DeclarationKind::Use(_) => todo!(),
            _ => {}
        }
    }

    fn document_decl(&mut self, decl: &hir::Decl) -> Result<()> {
        if let hir::BareDecl::Module(module) = &decl.bare {
            // @Task defer generation, only collect into structured data to be able to
            // generate the pages in parallel later on!
            let page = self.add_module_page(module, &decl.attrs);
            self.pages.push(page);

            for declaration in &module.decls {
                self.document_decl(declaration)?;
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

        for decl in &module.decls {
            match &decl.bare {
                hir::BareDecl::Func(func) => {
                    subsections.funcs.0.push(subsections::Func {
                        attrs: &decl.attrs,
                        binder: func.binder.to_str(),
                        ty: format::fmt_expr(&func.ty, url_prefix, self.sess),
                    });
                }
                hir::BareDecl::DataTy(ty) => {
                    let mut fmted_ctors = Vec::new();

                    if let Some(ctors) = &ty.ctors {
                        for decl in ctors {
                            let ctor = decl.bare.ctor().unwrap();

                            fmted_ctors.push(subsections::Constructor {
                                attrs: &decl.attrs,
                                binder: ctor.binder.to_str(),
                                ty: format::fmt_expr(&ctor.ty, url_prefix, self.sess),
                            });
                        }
                    }

                    subsections.types.0.push(subsections::Type {
                        // @Task write a custom formatter for this which prints links to the documentation
                        // of the specific attributes
                        attrs: &decl.attrs,
                        binder: ty.binder.to_str(),
                        ty: format::fmt_expr(&ty.ty, url_prefix, self.sess),
                        ctors: fmted_ctors,
                    });
                }
                hir::BareDecl::Module(module) => {
                    subsections.modules.0.push(subsections::Module {
                        attrs: &decl.attrs,
                        binder: module.binder.to_str(),
                    });
                }
                // @Task re-exports, only public//(public topmost) for --doc-priv-decls??) ones!
                // hir::BareDecl::Use(_) => todo!(),
                _ => {}
            }
        }

        subsections
    }

    fn reserved_idents_page(&self) -> Page {
        let url_prefix = "./";
        let mut body = Elem::new("body").child(ledge(url_prefix, None, None));
        let mut container = Elem::div("container");

        let subsections = subsections::Subsections::reserved_idents();
        container.add_child(sidebar(
            &subsections,
            url_prefix,
            default(),
            vec![self.sess.root_comp().name],
        ));

        // main content
        {
            let mut main = Elem::new("section").class("main");

            // main heading
            {
                let mut heading = Elem::new("h1");
                heading.add_child("Reserved Identifiers");
                main.add_child(heading);
            }

            subsections.render(url_prefix, &mut main, &self.text_processor, &self.opts);

            container.add_child(main);
        }

        body.add_child(container);

        Page {
            path: self.path.join("identifiers.html"),
            content: render_page(
                head(
                    self.sess.comp().name(),
                    url_prefix,
                    "Reserved Identifiers".into(),
                ),
                body,
            ),
        }
    }

    fn attrs_page(&self) -> Page {
        let url_prefix = "./";
        let mut body = Elem::new("body").child(ledge(url_prefix, None, None));
        let mut container = Elem::div("container");

        let subsections = subsections::Subsections::attrs();
        container.add_child(sidebar(
            &subsections,
            url_prefix,
            default(),
            vec![self.sess.root_comp().name],
        ));

        // main content
        {
            let mut main = Elem::new("section").class("main");

            // main heading
            {
                let mut heading = Elem::new("h1");
                heading.add_child("Attrs");
                main.add_child(heading);
            }

            subsections.render(url_prefix, &mut main, &self.text_processor, &self.opts);

            container.add_child(main);
        }

        body.add_child(container);

        Page {
            path: self.path.join("attributes.html"),
            content: render_page(
                head(self.sess.comp().name(), url_prefix, "Attrs".into()),
                body,
            ),
        }
    }

    fn package_page(&self, package: ManifestPath) -> Page {
        let url_prefix = "./";
        let package = &self.sess[package];
        let name = &package.name;

        let mut body = Elem::new("body").child(ledge(url_prefix, Some(package), None));
        let mut container = Elem::div("container");

        container.add_child(sidebar(
            &default(),
            url_prefix,
            default(),
            vec![self.sess.root_comp().name],
        ));

        // main content
        {
            let mut main = Elem::new("section").class("main");

            // main heading
            {
                let mut heading = Elem::new("h1");
                heading.add_child(format!("Package {name} {}", package.version.0));
                main.add_child(heading);
            }

            main.add_child(Elem::div("description").child(&package.description));

            // @Task add `section.subsection`s!

            {
                main.add_child(Elem::new("h2").child("Components"));

                let mut list = Elem::new("ul");

                for component in package.components.keys() {
                    list.add_child(Elem::new("li").child(Elem::anchor(
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

    fn add_module_page(&self, module: &hir::Module, attrs: &Attrs) -> Page {
        let index = module.binder.local_decl_idx(self.sess).unwrap();
        let component_name = self.sess.comp().name();

        let mut segments = self.sess.comp().local_idx_to_path_segments(index);
        segments.push_front(component_name.into_inner());
        let page_depth = segments.len();
        let url_prefix = format!("./{}", "../".repeat(page_depth));

        let mut body = Elem::new("body").child(ledge(
            &url_prefix,
            self.sess.pkg().map(|package| &self.sess[package]),
            Some(component_name),
        ));

        let mut container = Elem::div("container");

        let subsections = self.collect_module_subsections(module, &url_prefix);
        container.add_child(sidebar(
            &subsections,
            &url_prefix,
            self.sess.comp().deps().keys().copied().collect(),
            vec![self.sess.root_comp().name],
        ));

        // main content
        {
            let mut main = Elem::new("section").class("main");

            // main heading
            {
                let mut heading = Elem::new("h1");

                heading.add_child(match index == self.sess.comp().root_local() {
                    true => "Component",
                    false => "Module",
                });

                heading.add_child(" ");

                for (position, path_segment) in segments.iter().enumerate() {
                    let is_last_segment = position + 1 == page_depth;

                    heading.add_child(Elem::anchor(
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

            add_decl_attrs(
                attrs,
                &url_prefix,
                &mut main,
                &self.text_processor,
                &self.opts,
            );

            subsections.render(&url_prefix, &mut main, &self.text_processor, &self.opts);

            container.add_child(main);
        }

        body.add_child(container);

        let title = self
            .sess
            .comp()
            .local_idx_with_root_to_extern_path(index, component_name.to_string());

        Page {
            path: segments
                .into_iter()
                .map(Atom::to_str)
                .chain(Some("index.html"))
                .collect(),
            content: render_page(head(component_name, &url_prefix, title.into()), body),
        }
    }
}

fn ledge<'a>(url_prefix: &'a str, pkg: Option<&'a Package>, comp: Option<Word>) -> Elem<'a> {
    let mut context = Elem::div("name");

    if let Some(pkg) = pkg {
        // @Beacon @Task actual link
        context.add_child(
            Elem::new("a")
                .attr("href", format!("{url_prefix}index.html"))
                .child(Elem::span("package").child(pkg.name.to_str()))
                .child(" ")
                .child(&pkg.version.0),
        );
    }

    if let Some(comp) = comp {
        if pkg.is_some() {
            context.add_child(" / ");
        }

        // @Beacon @Task actual link
        context.add_child(
            Elem::anchor(format!("{url_prefix}{comp}/index.html"), comp.to_str())
                .class("component"),
        );
    }

    Elem::div("ledge-container").child(
        Elem::div("ledge")
            .child(context)
            .child(search_bar(url_prefix)),
    )
}

fn search_bar(url_prefix: &str) -> Elem<'_> {
    Elem::new("div")
        .child(
            VoidElem::new("input")
                .attr("type", "search")
                .attr("id", "searchbar")
                .attr("placeholder", "Search…")
                .attr("autocomplete", "off")
                .attr("data-url-prefix", url_prefix),
        )
        .child(
            Elem::new("div")
                .attr("id", "search-results-container")
                .child(Elem::new("div").attr("id", "search-results")),
        )
}

fn render_page(head: Elem<'_>, body: Elem<'_>) -> String {
    Document::default()
        .child(Elem::new("html").attr("lang", "en").child(head).child(body))
        .render()
}

// @Task remove `component` param
fn head<'a>(component: Word, url_prefix: &str, title: Node<'a>) -> Elem<'a> {
    Elem::new("head")
        .child(VoidElem::new("meta").attr("charset", "utf-8"))
        .child(
            VoidElem::new("meta")
                .attr("name", "viewport")
                .attr("content", "width=device-width, initial-scale=1.0"),
        )
        .child(Elem::new("title").child(title))
        .child(
            VoidElem::new("meta")
                .attr("name", "generator")
                // @Task insert version smh
                .attr("content", "lushui documenter"),
        )
        .child(
            VoidElem::new("link")
                .attr("rel", "stylesheet")
                .attr("href", format!("{url_prefix}{STYLE_SHEET_FILE_NAME}")),
        )
        .child(
            Elem::new("script")
                .attr("src", format!("{url_prefix}{SCRIPT_FILE_NAME}"))
                .bool_attr("defer"),
        )
        .child(
            Elem::new("script")
                .attr(
                    "src",
                    // @Task only create a single search index
                    format!("{url_prefix}{component}/{SEARCH_INDEX_FILE_NAME}"),
                )
                .bool_attr("defer"),
        )
}

fn sidebar<'a>(
    subsections: &subsections::Subsections<'a>,
    url_prefix: &str,
    deps: Vec<Word>,
    root_comps: Vec<Word>,
) -> Elem<'a> {
    let mut sidebar = Elem::div("sidebar");

    subsections.render_table_of_contents(&mut sidebar);

    if !deps.is_empty() {
        sidebar.add_child(
            Elem::div("title")
                .child("Dependencies")
                .attr("title", "Direct Dependencies"),
        );

        let mut list = Elem::new("ul");
        for dep in deps {
            // @Bug this is incorrect for components that share the same name
            let anchor = Elem::anchor(
                format!("{url_prefix}{dep}/index.html"),
                Node::from(dep.to_str()),
            );

            list.add_child(Elem::new("li").child(anchor));
        }

        sidebar.add_child(list);
    }

    {
        sidebar.add_child(
            Elem::div("title")
                .child("Roots")
                .attr("title", "Root Components"),
        );

        let mut list = Elem::new("ul");
        for comp in root_comps {
            let anchor = Elem::anchor(
                format!("{url_prefix}{comp}/index.html"),
                Node::from(comp.to_str()),
            );

            list.add_child(Elem::new("li").child(anchor));
        }

        sidebar.add_child(list);
    }

    sidebar
}

fn add_decl_attrs(
    attrs: &Attrs,
    url_prefix: &str,
    parent: &mut Elem<'_>,
    text_processor: &TextProcessor<'_>,
    options: &Options,
) {
    let mut labels = Elem::div("labels");

    // @Task sort attributes

    for attr in &attrs.0 {
        add_decl_attr(attr, &mut labels, url_prefix);
    }

    parent.add_child(labels);

    let mut descr = Elem::div("description");

    if let Some(amount) = options.lorem_ipsum {
        for _ in 0..amount {
            descr.add_child(Elem::new("p").child(LOREM_IPSUM));
        }
    } else if let Some(doc) = documentation(attrs) {
        // @Task handle errors properly!
        descr.add_child(text_processor.process(doc, url_prefix.to_owned()).unwrap());
    }

    parent.add_child(descr);
}

fn add_decl_attr(attr: &Attr, parent: &mut Elem<'_>, url_prefix: &str) {
    use BareAttr::*;

    #[allow(clippy::match_same_arms)]
    match &attr.bare {
        // plain style
        Abstract | Static | If { .. } | Intrinsic { .. } | Moving => {
            let name = attr.bare.name().to_str();

            parent.add_child(Elem::div("attribute").child(Elem::anchor(
                format!(
                    "{url_prefix}attributes.html#attribute.{}",
                    urlencoding::encode(name)
                ),
                name,
            )));
        }

        // @Task make the path a link; only display this with --document-private-items
        Public(_) => {}

        // @Task incorporate message contain within the attribute etc
        Deprecated(_) => {
            parent.add_child(Elem::div("deprecated").child("deprecated"));
        }
        // @Task incorporate message contain within the attribute etc
        Unstable(_) => parent.add_child(Elem::div("experimental").child("experimental")),
        Unsafe => parent.add_child(Elem::div("unsafe").child("unsafe")),

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

fn documentation(attrs: &Attrs) -> Option<String> {
    let content = attrs
        .select::<{ AttrName::Doc }>()
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
    Decl(hir::DeclIdx),
    Keyword(&'static str),
    ReservedSymbol(&'static str),
    Attr(&'static str),
}

struct Page {
    path: PathBuf,
    content: String,
}

// @Task add fast-path / alternative API for words
fn decl_id(binder: &str) -> String {
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
