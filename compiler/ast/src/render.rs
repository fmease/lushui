//! The definition of the textual representation(s) of the AST.

use super::{Identifier, ParameterKind};
use index_map::Index as _;
use joinery::JoinableIterator;
use span::{SourceFileIndex, Span, Spanned, Spanning};
use std::{
    fmt::{self, Formatter},
    io::{self, Write},
};
use struct_::Struct;
use utility::{
    paint::{AnsiColor, Painter},
    SmallVec,
};

pub use indentation::Indentation;

mod indentation {
    use std::fmt;

    const INDENTATION_IN_SPACES: Representation = 4;
    type Representation = usize;

    #[derive(Clone, Copy, Default)]
    pub struct Indentation(Representation);

    impl Indentation {
        fn map(self, mapper: impl FnOnce(Representation) -> Representation) -> Self {
            Self(mapper(self.0))
        }

        pub(super) fn increased(self) -> Self {
            self.map(|indentation| indentation + INDENTATION_IN_SPACES)
        }
    }

    impl fmt::Display for Indentation {
        fn fmt(&self, painter: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(painter, "{}", " ".repeat(self.0))
        }
    }
}

mod struct_ {
    use super::{palette, Indentation, Render};
    use std::io::{self, Write};
    use utility::paint::Painter;

    pub(super) struct Struct<'p> {
        painter: &'p mut Painter,
        indentation: Indentation,
        has_fields: bool,
        inline: bool,
        result: io::Result<()>,
    }

    impl<'p> Struct<'p> {
        pub(super) fn new(indentation: Indentation, painter: &'p mut Painter) -> Self {
            Self {
                painter,
                indentation,
                has_fields: false,
                inline: false,
                result: Ok(()),
            }
        }

        pub(super) fn name(mut self, name: &str) -> Self {
            self.result = self.result.and_then(|()| {
                self.painter.set(palette::NAME.on_default().bold())?;
                write!(self.painter, "{name}")?;
                self.painter.unset()
            });

            self
        }

        pub(super) fn field(mut self, name: &str, field: &impl Render) -> Self {
            if !self.has_fields {
                self.has_fields = true;
                self.indentation = self.indentation.increased();
            }

            self.result = self.result.and_then(|()| {
                if self.inline {
                    write!(self.painter, " ")
                } else {
                    writeln!(self.painter)
                }
            });

            self.result = self.result.and_then(|()| {
                if !self.inline {
                    write!(self.painter, "{}", self.indentation)?;
                }

                self.painter.set(palette::FIELD)?;
                write!(self.painter, "{name}")?;
                self.painter.unset()?;
                write!(self.painter, ": ")?;
                field.render(self.indentation, self.painter)
            });

            self
        }

        pub(super) fn finish(self) -> io::Result<()> {
            self.result
        }
    }
}

pub trait Render {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()>;
}

impl<T: Render> Render for Option<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Some(value) => value.render(indentation, painter),
            None => {
                painter.set(palette::SPECIAL_SYMBOL)?;
                write!(painter, "none")?;
                painter.unset()
            }
        }
    }
}

impl<T: Render> Render for [T] {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        if self.is_empty() {
            painter.set(palette::SPECIAL_SYMBOL)?;
            write!(painter, "empty")?;
            return painter.unset();
        }

        let mut struct_ = Struct::new(indentation, painter);

        for (index, declaration) in self.iter().enumerate() {
            struct_ = struct_.field(&index.to_string(), declaration);
        }

        struct_.finish()
    }
}

impl<T: Render> Render for Vec<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        (**self).render(indentation, painter)
    }
}

impl<T: Render, const N: usize> Render for SmallVec<T, N> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        (**self).render(indentation, painter)
    }
}

impl<T: Render, U: Render> Render for (T, U) {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .field("0", &self.0)
            .field("1", &self.1)
            .finish()
    }
}

impl Render for Span {
    fn render(&self, _: Indentation, painter: &mut Painter) -> io::Result<()> {
        painter.set(palette::SPAN)?;
        write!(painter, "{self:?}")?;
        painter.unset()
    }
}

impl<K: Render> Render for Spanned<K> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        self.span.render(indentation, painter)?;
        write!(painter, " ")?;
        self.bare.render(indentation, painter)
    }
}

impl<I: Render> Render for super::Item<I> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        self.span.render(indentation, painter)?;
        write!(painter, " ")?;
        self.bare.render(indentation, painter)?;

        if !self.attributes.is_empty() {
            let indentation = indentation.increased();
            writeln!(painter)?;
            write!(painter, "{indentation}")?;
            painter.set(palette::FIELD)?;
            write!(painter, "attributes")?;
            painter.unset()?;
            write!(painter, ":")?;
            self.attributes.render(indentation, painter)?;
        }

        Ok(())
    }
}

impl Render for super::BareDeclaration {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::Function(function) => function.render(indentation, painter),
            Self::Data(type_) => type_.render(indentation, painter),
            Self::Module(module) => module.render(indentation, painter),
            Self::ModuleHeader => Struct::new(indentation, painter)
                .name("Module-Header")
                .finish(),
            Self::Use(use_) => use_.render(indentation, painter),
            Self::Given(given) => given.render(indentation, painter),
        }
    }
}

impl Render for super::Function {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Function-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::Data {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Data-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("constructors", &self.declarations)
            .finish()
    }
}

impl Render for super::Module {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Module-Declaration")
            .field("binder", &self.binder)
            .field("file", &self.file)
            .field("declarations", &self.declarations)
            .finish()
    }
}

impl Render for SourceFileIndex {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Source-File-Index")
            .finish()?;
        painter.set(palette::VERBATIM)?;
        write!(painter, " {}", self.value())?;
        painter.unset()
    }
}

impl Render for super::Use {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Use-Declaration")
            .field("bindings", &self.bindings)
            .finish()
    }
}

impl Render for super::BareUsePathTree {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indentation, painter);

        match self {
            Self::Single { target, binder } => struct_
                .name("Use-Path-Tree.Single")
                .field("target", target)
                .field("binder", binder),
            Self::Multiple { path, subpaths } => struct_
                .name("Use-Path-Tree.Multiple")
                .field("path", path)
                .field("subpaths", subpaths),
        }
        .finish()
    }
}

impl Render for super::Given {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Given-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::Body {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indentation, painter);

        match self {
            Self::Block { fields } => struct_.name("Block").field("fields", fields),
            Self::Expression { body } => struct_.name("Expression").field("body", body),
        }
        .finish()
    }
}

impl Render for super::BareExpression {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::QuantifiedType(type_) => type_.render(indentation, painter),
            Self::Application(application) => application.render(indentation, painter),
            Self::NumberLiteral(number) => number.render(indentation, painter),
            Self::TextLiteral(text) => text.render(indentation, painter),
            Self::Wildcard(hole) => hole.render(indentation, painter),
            Self::Path(path) => path.render(indentation, painter),
            Self::Projection(field) => field.render(indentation, painter),
            Self::LambdaLiteral(lambda) => lambda.render(indentation, painter),
            Self::LetBinding(binding) => binding.render(indentation, painter),
            Self::UseBinding(binding) => binding.render(indentation, painter),
            Self::CaseAnalysis(analysis) => analysis.render(indentation, painter),
            Self::DoBlock(do_) => do_.render(indentation, painter),
            Self::SequenceLiteral(sequence) => sequence.render(indentation, painter),
            Self::RecordLiteral(record) => record.render(indentation, painter),
            Self::Error(_) => {
                painter.set(palette::INVALID)?;
                write!(painter, "invalid")?;
                painter.unset()
            }
        }
    }
}

impl Render for super::QuantifiedType {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Quantified-Type")
            .field("quantifier", &self.quantifier)
            .field("parameters", &self.parameters)
            .field("codomain", &self.codomain)
            .finish()
    }
}

impl Render for super::Quantifier {
    fn render(&self, _: Indentation, painter: &mut Painter) -> io::Result<()> {
        painter.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Pi => write!(painter, "pi")?,
            Self::Sigma => write!(painter, "sigma")?,
        }
        painter.unset()
    }
}

impl<T: Render> Render for super::Application<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Application")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("callee", &self.callee)
            .field("argument", &self.argument)
            .finish()
    }
}

impl Render for super::NumberLiteral {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Number-Literal")
            .field("path", &self.path)
            .field(
                "literal",
                // FIXME: Render the span of the literal as well.
                &AdHoc(|_, painter| {
                    painter.set(palette::INVALID)?;
                    write!(painter, "{}", self.literal.bare)?;
                    painter.unset()
                }),
            )
            .finish()
    }
}

impl fmt::Display for super::NumberLiteral {
    fn fmt(&self, painter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(painter, "{path}.{}", self.literal),
            None => write!(painter, "{}", self.literal),
        }
    }
}

impl Render for super::TextLiteral {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Text-Literal")
            .field("path", &self.path)
            .field(
                "literal",
                // FIXME: Render the span of the literal as well.
                &AdHoc(|_, painter| render_text_literal(self.literal.bare.to_str(), painter)),
            )
            .finish()
    }
}

impl fmt::Display for super::TextLiteral {
    fn fmt(&self, painter: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Bug this doesn't escape anything at all!
        match &self.path {
            Some(path) => write!(painter, "{path}.\"{}\"", self.literal),
            None => write!(painter, "\"{}\"", self.literal),
        }
    }
}

impl Render for super::Wildcard {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indentation, painter);

        match self {
            Self::Silent => struct_.name("Wildcard.Silent"),
            Self::Signaling { tag } => struct_.name("Wildcard.Signaling").field("tag", tag),
        }
        .finish()
    }
}

impl Render for super::Path {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Path")
            .field("hanger", &self.hanger)
            .field("segments", &self.segments)
            .finish()
    }
}

impl Render for super::Projection {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Projection")
            .field("basis", &self.basis)
            .field("field", &self.field)
            .finish()
    }
}

impl Render for super::BareHanger {
    fn render(&self, _: Indentation, painter: &mut Painter) -> io::Result<()> {
        // FIXME: Add the color to the palette.
        painter.set(AnsiColor::BrightYellow)?;
        write!(painter, "{self}")?;
        painter.unset()
    }
}

impl Render for super::LambdaLiteral {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Lambda-Literal")
            .field("parameters", &self.parameters)
            .field("codomain", &self.codomain)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::LetBinding {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Let-Binding")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Render for super::UseBinding {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Use-Binding")
            .field("bindings", &self.bindings)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Render for super::CaseAnalysis {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Case-Analysis")
            .field("scrutinee", &self.scrutinee)
            .field("cases", &self.cases)
            .finish()
    }
}

impl Render for super::Case {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Case")
            .field("pattern", &self.pattern)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::DoBlock {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Do-Block")
            .field("statements", &self.statements)
            .finish()
    }
}

impl<T: Render> Render for super::SequenceLiteral<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Sequence-Literal")
            .field("path", &self.path)
            .field("elements", &self.elements)
            .finish()
    }
}

impl<T: Render> Render for super::RecordLiteral<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Record-Literal")
            .field("path", &self.path)
            .field("fields", &self.fields)
            .field("base", &self.base)
            .finish()
    }
}

impl<T: Render> Render for super::Field<T> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Field")
            .field("binder", &self.binder)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::BareParameter {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Parameter")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("type", &self.type_)
            .finish()
    }
}

impl Render for super::LocalBinder {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::Named(binder) => binder.render(indentation, painter),
            Self::Discarded(span) => {
                span.render(indentation, painter)?;
                painter.set(palette::SPECIAL_SYMBOL)?;
                write!(painter, " discarded")?;
                painter.unset()
            }
        }
    }
}

// @Bug this does not properly handle symbols in paths
// @Task take a look at `Component::index_to_path`
impl fmt::Display for super::Path {
    fn fmt(&self, painter: &mut Formatter<'_>) -> fmt::Result {
        if let Some(hanger) = &self.hanger {
            write!(painter, "{hanger}")?;

            if !self.segments.is_empty() {
                write!(painter, ".")?;
            }
        }

        write!(painter, "{}", self.segments.iter().join_with("."))
    }
}

impl fmt::Display for super::BareHanger {
    fn fmt(&self, painter: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(painter, "{}", self.name())
    }
}

impl Render for super::Statement {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::Let(let_) => let_.render(indentation, painter),
            Self::Use(use_) => use_.render(indentation, painter),
            Self::Expression(expression) => expression.render(indentation, painter),
        }
    }
}

impl Render for super::LetStatement {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Let-Statement")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Render for super::BindingMode {
    fn render(&self, _: Indentation, painter: &mut Painter) -> io::Result<()> {
        painter.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Plain => write!(painter, "plain")?,
            Self::Effectful => write!(painter, "effectful")?,
        }
        painter.unset()
    }
}

impl Render for super::BarePattern {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::Wildcard(wildcard) => wildcard.render(indentation, painter),
            Self::NumberLiteral(number) => number.render(indentation, painter),
            Self::TextLiteral(text) => text.render(indentation, painter),
            Self::SequenceLiteral(sequence) => sequence.render(indentation, painter),
            Self::RecordLiteral(record) => record.render(indentation, painter),
            Self::Path(path) => path.render(indentation, painter),
            Self::LetBinding(binder) => binder.render(indentation, painter),
            Self::Application(application) => application.render(indentation, painter),
        }
    }
}

impl Render for Identifier {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        self.span().render(indentation, painter)?;
        write!(painter, " ")?;
        Struct::new(indentation, painter)
            .name("Identifier")
            .finish()?;
        painter.set(palette::VERBATIM)?;
        write!(painter, " {self}")?;
        painter.unset()
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, painter: &mut Formatter<'_>) -> fmt::Result {
        write!(
            painter,
            "{:width$}",
            self.to_str(),
            width = painter.width().unwrap_or_default()
        )
    }
}

impl Render for ParameterKind {
    fn render(&self, _: Indentation, painter: &mut Painter) -> io::Result<()> {
        painter.set(palette::SPECIAL_SYMBOL)?;
        match self {
            Self::Explicit => write!(painter, "explicit")?,
            Self::Implicit => write!(painter, "implicit")?,
            Self::Context => write!(painter, "context")?,
        }
        painter.unset()
    }
}

impl Render for super::BareAttribute {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        let struct_ = Struct::new(indentation, painter);
        match self {
            Self::Regular { binder, arguments } => struct_
                .name("Attribute")
                .field("binder", binder)
                .field("arguments", arguments),
            Self::Documentation => struct_.name("Documentation"),
        }
        .finish()
    }
}

impl Render for super::BareAttributeArgument {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        match self {
            Self::NumberLiteral(number) => {
                Struct::new(indentation, painter)
                    .name("Number-Literal")
                    .finish()?;
                painter.set(palette::VERBATIM)?;
                write!(painter, " {number}")?;
                painter.unset()
            }
            Self::TextLiteral(text) => {
                Struct::new(indentation, painter)
                    .name("Text-Literal")
                    .finish()?;
                write!(painter, " ")?;
                render_text_literal(text.to_str(), painter)
            }
            Self::Path(path) => path.render(indentation, painter),
            Self::Named(named) => named.render(indentation, painter),
        }
    }
}

impl Render for super::NamedAttributeArgument {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        Struct::new(indentation, painter)
            .name("Named-Attribute-Argument")
            .field("binder", &self.binder)
            .field("value", &self.value)
            .finish()
    }
}

// FIXME: Only shorten the text literal if we are printing to a TTY.
// FIXME: Consider making whitespace visible.
fn render_text_literal(content: &str, painter: &mut Painter) -> io::Result<()> {
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

    painter.set(palette::VERBATIM)?;
    render_text_literal_part(left, painter)?;
    if let Some(right) = right {
        painter.set(AnsiColor::Black.on(palette::SPECIAL_SYMBOL_INSIDE_VERBATIM))?;
        write!(painter, "{ELLIPSIS}")?;
        painter.unset()?;

        render_text_literal_part(right, painter)?;
    }
    painter.unset()
}

fn render_text_literal_part(content: &str, painter: &mut Painter) -> io::Result<()> {
    let mut lines = content.split('\n');

    if let Some(line) = lines.next() {
        write!(painter, "{line}")?;
    }

    for line in lines {
        painter.set(palette::SPECIAL_SYMBOL_INSIDE_VERBATIM)?;
        write!(painter, "{LINE_BREAK}")?;
        painter.unset()?;

        write!(painter, "{line}")?;
    }

    Ok(())
}

struct AdHoc<P: Fn(Indentation, &mut Painter) -> io::Result<()>>(P);

impl<P: Fn(Indentation, &mut Painter) -> io::Result<()>> Render for AdHoc<P> {
    fn render(&self, indentation: Indentation, painter: &mut Painter) -> io::Result<()> {
        (self.0)(indentation, painter)
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
