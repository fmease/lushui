//! The definition of the textual representation(s) of the AST.

use super::{Ident, ParamKind};
use index_map::Index as _;
use joinery::JoinableIterator;
use span::{Span, Spanned, Spanning, SrcFileIdx};
use std::{
    fmt::{self, Formatter},
    io::{self, Write},
};
use struct_::Struct;
use utility::{
    paint::{AnsiColor, Painter},
    SmallVec,
};

pub use indent::Indent;

mod indent {
    use std::fmt;

    const INDENT_IN_SPACES: Repr = 4;
    type Repr = usize;

    #[derive(Clone, Copy, Default)]
    pub struct Indent(Repr);

    impl Indent {
        fn map(self, mapper: impl FnOnce(Repr) -> Repr) -> Self {
            Self(mapper(self.0))
        }

        pub(super) fn increased(self) -> Self {
            self.map(|indent| indent + INDENT_IN_SPACES)
        }
    }

    impl fmt::Display for Indent {
        fn fmt(&self, p: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(p, "{}", " ".repeat(self.0))
        }
    }
}

mod struct_ {
    use super::{palette, Indent, Render};
    use std::io::{self, Write};
    use utility::paint::Painter;

    pub(super) struct Struct<'p> {
        p: &'p mut Painter,
        indent: Indent,
        has_fields: bool,
        inline: bool,
        result: io::Result<()>,
    }

    impl<'p> Struct<'p> {
        pub(super) fn new(indent: Indent, p: &'p mut Painter) -> Self {
            Self {
                p,
                indent,
                has_fields: false,
                inline: false,
                result: Ok(()),
            }
        }

        pub(super) fn name(mut self, name: &str) -> Self {
            self.result = self.result.and_then(|()| {
                self.p.set(palette::NAME.on_default().bold())?;
                write!(self.p, "{name}")?;
                self.p.unset()
            });

            self
        }

        pub(super) fn field(mut self, name: &str, field: &impl Render) -> Self {
            if !self.has_fields {
                self.has_fields = true;
                self.indent = self.indent.increased();
            }

            self.result = self.result.and_then(|()| {
                if self.inline {
                    write!(self.p, " ")
                } else {
                    writeln!(self.p)
                }
            });

            self.result = self.result.and_then(|()| {
                if !self.inline {
                    write!(self.p, "{}", self.indent)?;
                }

                self.p.set(palette::FIELD)?;
                write!(self.p, "{name}")?;
                self.p.unset()?;
                write!(self.p, ": ")?;
                field.render(self.indent, self.p)
            });

            self
        }

        pub(super) fn finish(self) -> io::Result<()> {
            self.result
        }
    }
}

pub trait Render {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()>;
}

impl<T: Render> Render for Option<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Some(value) => value.render(indent, p),
            None => {
                p.set(palette::SPECIAL_SYMBOL)?;
                write!(p, "none")?;
                p.unset()
            }
        }
    }
}

impl<T: Render> Render for [T] {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        if self.is_empty() {
            p.set(palette::SPECIAL_SYMBOL)?;
            write!(p, "empty")?;
            return p.unset();
        }

        let mut struct_ = Struct::new(indent, p);

        for (index, decl) in self.iter().enumerate() {
            struct_ = struct_.field(&index.to_string(), decl);
        }

        struct_.finish()
    }
}

impl<T: Render> Render for Vec<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        (**self).render(indent, p)
    }
}

impl<T: Render, const N: usize> Render for SmallVec<T, N> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        (**self).render(indent, p)
    }
}

impl<T: Render, U: Render> Render for (T, U) {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .field("0", &self.0)
            .field("1", &self.1)
            .finish()
    }
}

impl Render for Span {
    fn render(&self, _: Indent, p: &mut Painter) -> io::Result<()> {
        p.set(palette::SPAN)?;
        write!(p, "{self:?}")?;
        p.unset()
    }
}

impl<K: Render> Render for Spanned<K> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        self.span.render(indent, p)?;
        write!(p, " ")?;
        self.bare.render(indent, p)
    }
}

impl<I: Render> Render for super::Item<I> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        self.span.render(indent, p)?;
        write!(p, " ")?;
        self.bare.render(indent, p)?;

        if !self.attrs.is_empty() {
            let indent = indent.increased();
            writeln!(p)?;
            write!(p, "{indent}")?;
            p.set(palette::FIELD)?;
            write!(p, "attributes")?;
            p.unset()?;
            write!(p, ":")?;
            self.attrs.render(indent, p)?;
        }

        Ok(())
    }
}

impl Render for super::BareDecl {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::Func(func) => func.render(indent, p),
            Self::DataTy(ty) => ty.render(indent, p),
            Self::Module(module) => module.render(indent, p),
            Self::ModuleHeader => Struct::new(indent, p).name("Decl.Module-Header").finish(),
            Self::Use(use_) => use_.render(indent, p),
            Self::Given(given) => given.render(indent, p),
        }
    }
}

impl Render for super::Func {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Decl.Fn")
            .field("binder", &self.binder)
            .field("params", &self.params)
            .field("type", &self.ty)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::DataTy {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Decl.Data")
            .field("binder", &self.binder)
            .field("params", &self.params)
            .field("type", &self.ty)
            .field("constructors", &self.decls)
            .finish()
    }
}

impl Render for super::Module {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Decl.Module")
            .field("binder", &self.binder)
            .field("file", &self.file)
            .field("decls", &self.decls)
            .finish()
    }
}

impl Render for SrcFileIdx {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p).name("SrcFileIdx").finish()?;
        p.set(palette::VERBATIM)?;
        write!(p, " {}", self.value())?;
        p.unset()
    }
}

impl Render for super::Use {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Decl.Use")
            .field("bindings", &self.bindings)
            .finish()
    }
}

impl Render for super::BareUsePathTree {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indent, p);

        match self {
            Self::Single { target, binder } => struct_
                .name("UsePathTree.Single")
                .field("target", target)
                .field("binder", binder),
            Self::Multiple { path, subpaths } => struct_
                .name("UsePathTree.Multiple")
                .field("path", path)
                .field("subpaths", subpaths),
        }
        .finish()
    }
}

impl Render for super::Given {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Decl.Given")
            .field("binder", &self.binder)
            .field("params", &self.params)
            .field("type", &self.ty)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::Body {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indent, p);

        match self {
            Self::Block { fields } => struct_.name("Block").field("fields", fields),
            Self::Expr { body } => struct_.name("Expr").field("body", body),
        }
        .finish()
    }
}

impl Render for super::BareExpr {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::QuantifiedTy(ty) => ty.render(indent, p),
            Self::App(app) => app.render(indent, p),
            Self::NumLit(num) => num.render(indent, p),
            Self::TextLit(text) => text.render(indent, p),
            Self::Wildcard(hole) => hole.render(indent, p),
            Self::Path(path) => path.render(indent, p),
            Self::Proj(field) => field.render(indent, p),
            Self::LamLit(lamda) => lamda.render(indent, p),
            Self::LetBinding(binding) => binding.render(indent, p),
            Self::UseBinding(binding) => binding.render(indent, p),
            Self::CaseAnalysis(analysis) => analysis.render(indent, p),
            Self::DoBlock(do_) => do_.render(indent, p),
            Self::SeqLit(sequence) => sequence.render(indent, p),
            Self::RecLit(record) => record.render(indent, p),
            Self::Error(_) => {
                p.set(palette::INVALID)?;
                write!(p, "invalid")?;
                p.unset()
            }
        }
    }
}

impl Render for super::QuantifiedTy {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("QuantifiedTy")
            .field("quantifier", &self.quantifier)
            .field("params", &self.params)
            .field("codomain", &self.codomain)
            .finish()
    }
}

impl Render for super::Quantifier {
    fn render(&self, _: Indent, p: &mut Painter) -> io::Result<()> {
        p.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Pi => write!(p, "pi")?,
            Self::Sigma => write!(p, "sigma")?,
        }
        p.unset()
    }
}

impl<T: Render> Render for super::App<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("App")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("callee", &self.callee)
            .field("arg", &self.arg)
            .finish()
    }
}

impl Render for super::NumLit {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("NumLit")
            .field("path", &self.path)
            .field(
                "literal",
                // FIXME: Render the span of the literal as well.
                &AdHoc(|_, p| {
                    p.set(palette::INVALID)?;
                    write!(p, "{}", self.lit.bare)?;
                    p.unset()
                }),
            )
            .finish()
    }
}

impl fmt::Display for super::NumLit {
    fn fmt(&self, p: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(p, "{path}.{}", self.lit),
            None => write!(p, "{}", self.lit),
        }
    }
}

impl Render for super::TextLit {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("TextLit")
            .field("path", &self.path)
            .field(
                "literal",
                // FIXME: Render the span of the literal as well.
                &AdHoc(|_, p| render_text_lit(self.lit.bare.to_str(), p)),
            )
            .finish()
    }
}

impl fmt::Display for super::TextLit {
    fn fmt(&self, p: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Bug this doesn't escape anything at all!
        match &self.path {
            Some(path) => write!(p, "{path}.\"{}\"", self.lit),
            None => write!(p, "\"{}\"", self.lit),
        }
    }
}

impl Render for super::Wildcard {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indent, p);

        match self {
            Self::Silent => struct_.name("Wildcard.Silent"),
            Self::Signaling { tag } => struct_.name("Wildcard.Signaling").field("tag", tag),
        }
        .finish()
    }
}

impl Render for super::Path {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Path")
            .field("hanger", &self.hanger)
            .field("segments", &self.segments)
            .finish()
    }
}

impl Render for super::Proj {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Proj")
            .field("basis", &self.basis)
            .field("field", &self.field)
            .finish()
    }
}

impl Render for super::BareHanger {
    fn render(&self, _: Indent, p: &mut Painter) -> io::Result<()> {
        // FIXME: Add the color to the palette.
        p.set(AnsiColor::BrightYellow)?;
        write!(p, "{self}")?;
        p.unset()
    }
}

impl Render for super::LamLit {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("LamLit")
            .field("params", &self.params)
            .field("codomain", &self.codomain)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::LetBinding {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("LetBinding")
            .field("binder", &self.binder)
            .field("params", &self.params)
            .field("type", &self.ty)
            .field("body", &self.body)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Render for super::UseBinding {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("UseBinding")
            .field("bindings", &self.bindings)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Render for super::CaseAnalysis {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("CaseAnalysis")
            .field("scrutinee", &self.scrutinee)
            .field("cases", &self.cases)
            .finish()
    }
}

impl Render for super::Case {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Case")
            .field("pattern", &self.pattern)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::DoBlock {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Do-Block")
            .field("statements", &self.statements)
            .finish()
    }
}

impl<T: Render> Render for super::SeqLit<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("SeqLit")
            .field("path", &self.path)
            .field("elems", &self.elems)
            .finish()
    }
}

impl<T: Render> Render for super::RecLit<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("RecLit")
            .field("path", &self.path)
            .field("fields", &self.fields)
            .field("base", &self.base)
            .finish()
    }
}

impl<T: Render> Render for super::Field<T> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Field")
            .field("binder", &self.binder)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::BareParam {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("Param")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("type", &self.ty)
            .finish()
    }
}

impl Render for super::LocalBinder {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::Named(binder) => binder.render(indent, p),
            Self::Discarded(span) => {
                span.render(indent, p)?;
                p.set(palette::SPECIAL_SYMBOL)?;
                write!(p, " discarded")?;
                p.unset()
            }
        }
    }
}

// @Bug this does not properly handle symbols in paths
// @Task take a look at `Component::index_to_path`
impl fmt::Display for super::Path {
    fn fmt(&self, p: &mut Formatter<'_>) -> fmt::Result {
        if let Some(hanger) = &self.hanger {
            write!(p, "{hanger}")?;

            if !self.segments.is_empty() {
                write!(p, ".")?;
            }
        }

        write!(p, "{}", self.segments.iter().join_with("."))
    }
}

impl fmt::Display for super::BareHanger {
    fn fmt(&self, p: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(p, "{}", self.name())
    }
}

impl Render for super::Statement {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::Let(let_) => let_.render(indent, p),
            Self::Use(use_) => use_.render(indent, p),
            Self::Expr(expression) => expression.render(indent, p),
        }
    }
}

impl Render for super::LetStatement {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("LetStatement")
            .field("binder", &self.binder)
            .field("params", &self.params)
            .field("type", &self.ty)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::BindingMode {
    fn render(&self, _: Indent, p: &mut Painter) -> io::Result<()> {
        p.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Plain => write!(p, "plain")?,
            Self::Effectful => write!(p, "effectful")?,
        }
        p.unset()
    }
}

impl Render for super::BarePat {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::Wildcard(wildcard) => wildcard.render(indent, p),
            Self::NumLit(num) => num.render(indent, p),
            Self::TextLit(text) => text.render(indent, p),
            Self::SeqLit(sequence) => sequence.render(indent, p),
            Self::RecLit(record) => record.render(indent, p),
            Self::Path(path) => path.render(indent, p),
            Self::LetBinding(binder) => binder.render(indent, p),
            Self::App(app) => app.render(indent, p),
        }
    }
}

impl Render for Ident {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        self.span().render(indent, p)?;
        write!(p, " ")?;
        Struct::new(indent, p).name("Ident").finish()?;
        p.set(palette::VERBATIM)?;
        write!(p, " {self}")?;
        p.unset()
    }
}

impl fmt::Display for Ident {
    fn fmt(&self, p: &mut Formatter<'_>) -> fmt::Result {
        write!(
            p,
            "{:width$}",
            self.to_str(),
            width = p.width().unwrap_or_default()
        )
    }
}

impl Render for ParamKind {
    fn render(&self, _: Indent, p: &mut Painter) -> io::Result<()> {
        p.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Explicit => write!(p, "explicit")?,
            Self::Implicit => write!(p, "implicit")?,
            Self::Context => write!(p, "context")?,
        }
        p.unset()
    }
}

impl Render for super::BareAttr {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indent, p);
        match self {
            Self::Reg { binder, args } => struct_
                .name("Attr")
                .field("binder", binder)
                .field("args", args),
            Self::Doc => struct_.name("Doc"),
        }
        .finish()
    }
}

impl Render for super::BareAttrArg {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        match self {
            Self::NumLit(num) => {
                Struct::new(indent, p).name("NumLit").finish()?;
                p.set(palette::VERBATIM)?;
                write!(p, " {num}")?;
                p.unset()
            }
            Self::TextLit(text) => {
                Struct::new(indent, p).name("TextLit").finish()?;
                write!(p, " ")?;
                render_text_lit(text.to_str(), p)
            }
            Self::Path(path) => path.render(indent, p),
            Self::Named(named) => named.render(indent, p),
        }
    }
}

impl Render for super::NamedAttrArg {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        Struct::new(indent, p)
            .name("NamedAttrArg")
            .field("binder", &self.binder)
            .field("value", &self.value)
            .finish()
    }
}

// FIXME: Only shorten the text literal if we are printing to a TTY.
// FIXME: Consider making whitespace visible.
fn render_text_lit(content: &str, p: &mut Painter) -> io::Result<()> {
    const MAXIMUM_LENGTH: usize = 20;
    #[allow(clippy::assertions_on_constants)]
    const _: () = assert!(MAXIMUM_LENGTH % 2 == 0);

    let length = content.len();

    let (left, right) = if length <= MAXIMUM_LENGTH {
        (content, None)
    } else {
        let left = &content[..MAXIMUM_LENGTH / 2];
        let right = &content[length - MAXIMUM_LENGTH / 2..];

        (left, Some(right))
    };

    p.set(palette::VERBATIM)?;
    render_text_literal_part(left, p)?;
    if let Some(right) = right {
        p.set(AnsiColor::Black.on(palette::SPECIAL_SYMBOL_INSIDE_VERBATIM))?;
        write!(p, "{ELLIPSIS}")?;
        p.unset()?;

        render_text_literal_part(right, p)?;
    }
    p.unset()
}

fn render_text_literal_part(content: &str, p: &mut Painter) -> io::Result<()> {
    let mut lines = content.split('\n');

    if let Some(line) = lines.next() {
        write!(p, "{line}")?;
    }

    for line in lines {
        p.set(palette::SPECIAL_SYMBOL_INSIDE_VERBATIM)?;
        write!(p, "{LINE_BREAK}")?;
        p.unset()?;

        write!(p, "{line}")?;
    }

    Ok(())
}

struct AdHoc<P: Fn(Indent, &mut Painter) -> io::Result<()>>(P);

impl<P: Fn(Indent, &mut Painter) -> io::Result<()>> Render for AdHoc<P> {
    fn render(&self, indent: Indent, p: &mut Painter) -> io::Result<()> {
        (self.0)(indent, p)
    }
}

const LINE_BREAK: &str = "↵";
const ELLIPSIS: &str = "...";

mod palette {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;

    pub(super) const FIELD: AnsiColor = AnsiColor::BrightWhite;
    pub(super) const INVALID: AnsiColor = AnsiColor::BrightRed;
    pub(super) const NAME: AnsiColor = AnsiColor::BrightCyan;
    pub(super) const SPAN: AnsiColor = AnsiColor::BrightBlack;
    pub(super) const SPECIAL_SYMBOL_INSIDE_VERBATIM: AnsiColor = AnsiColor::BrightRed;
    pub(super) const SPECIAL_SYMBOL: AnsiColor = AnsiColor::BrightCyan;
    pub(super) const VERBATIM: AnsiColor = AnsiColor::Yellow;
}
