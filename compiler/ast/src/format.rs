//! The definition of the textual representation(s) of the AST.

use super::{Identifier, ParameterKind};
use colored::{Color, Colorize};
use format_struct::FormatStruct;
use index_map::Index as _;
use joinery::JoinableIterator;
use span::{SourceFileIndex, Span, Spanned, Spanning};
use std::fmt::{self, Formatter, Result};
use utility::{default, SmallVec};

pub use indentation::Indentation;

mod palette {
    use colored::Color;

    pub(super) const NAME: Color = Color::BrightCyan;
    pub(super) const INVALID: Color = Color::BrightRed;
    pub(super) const SPECIAL_SYMBOL: Color = Color::BrightCyan;
    pub(super) const VERBATIM: Color = Color::Yellow;
    pub(super) const ELLIPSIS: Color = Color::BrightRed;
    pub(super) const SPAN: Color = Color::BrightBlack;
    pub(super) const FIELD: Color = Color::BrightWhite;
}

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
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", " ".repeat(self.0))
        }
    }
}

mod format_struct {
    use super::{palette, Format, Formatter, Indentation, Result};
    use colored::Colorize;

    pub(super) struct FormatStruct<'f, 'v> {
        formatter: &'f mut Formatter<'v>,
        indentation: Indentation,
        has_fields: bool,
        inline: bool,
        result: Result,
    }

    impl<'f, 'v> FormatStruct<'f, 'v> {
        pub(super) fn new(formatter: &'f mut Formatter<'v>, indentation: Indentation) -> Self {
            Self {
                formatter,
                indentation,
                has_fields: false,
                inline: false,
                result: Ok(()),
            }
        }

        pub(super) fn name(mut self, name: &str) -> Self {
            self.result = self
                .result
                .and_then(|_| write!(self.formatter, "{}", name.color(palette::NAME).bold()));

            self
        }

        pub(super) fn field(mut self, name: &str, field: &impl Format) -> Self {
            if !self.has_fields {
                self.has_fields = true;
                self.indentation = self.indentation.increased();
            }

            self.result = self.result.and_then(|_| {
                if self.inline {
                    write!(self.formatter, " ")
                } else {
                    writeln!(self.formatter)
                }
            });

            self.result = self.result.and_then(|_| {
                if !self.inline {
                    write!(self.formatter, "{}", self.indentation)?;
                }

                write!(self.formatter, "{}: ", name.color(palette::FIELD))?;
                field.format(self.formatter, self.indentation)
            });

            self
        }

        pub(super) fn finish(self) -> Result {
            self.result
        }
    }
}

// @Task only shorten if we are printing to a tty but how can we check that? even with `atty::is` that is
// impossible since we are so disconnected from printing, we are only writing to an arbitrary writer.
// @Task make whitespace visible
fn format_text_literal(content: &str, f: &mut Formatter<'_>) -> Result {
    const MAXIMUM_LENGTH: usize = 20;
    #[allow(clippy::assertions_on_constants)]
    const _: () = assert!(MAXIMUM_LENGTH % 2 == 0);

    let length = content.len();

    let content = if length <= MAXIMUM_LENGTH {
        content.color(palette::VERBATIM).to_string()
    } else {
        let length_left = MAXIMUM_LENGTH / 2;
        let length_right = length - MAXIMUM_LENGTH / 2;

        let left = &content[..length_left];
        let ellipsis = "....".black().on_color(palette::ELLIPSIS);
        let right = &content[length_right..];

        format!("{left}{ellipsis}{right}")
    };

    // `str::replace` would not work here since it breaks the fragile `ColoredString`s
    let content = content
        .split('\n')
        .map(|segment| segment.color(palette::VERBATIM))
        .join_with("\u{21b5}".black().on_color(palette::ELLIPSIS));

    write!(f, "{content}")
}

pub trait Format {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result;
}

impl<T: Format> Format for Option<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Some(value) => value.format(f, indentation),
            None => write!(f, "{}", "none".color(palette::SPECIAL_SYMBOL)),
        }
    }
}

impl<T: Format> Format for [T] {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        if self.is_empty() {
            return write!(f, "{}", "empty".color(palette::SPECIAL_SYMBOL));
        }

        let mut struct_ = FormatStruct::new(f, indentation);

        for (index, declaration) in self.iter().enumerate() {
            struct_ = struct_.field(&index.to_string(), declaration);
        }

        struct_.finish()
    }
}

impl<T: Format> Format for Vec<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        (**self).format(f, indentation)
    }
}

impl<T: Format, const N: usize> Format for SmallVec<T, N> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        (**self).format(f, indentation)
    }
}

impl<T: Format, U: Format> Format for (T, U) {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .field("0", &self.0)
            .field("1", &self.1)
            .finish()
    }
}

impl Format for Span {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        // @Beacon @Task don't use `colored` here, too many wasted allocations!!
        write!(f, "{}", format!("{self:?}").color(palette::SPAN))
    }
}

impl<K: Format> Format for Spanned<K> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.bare.format(f, indentation)
    }
}

impl<I: Format> Format for super::Item<I> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.bare.format(f, indentation)?;

        if !self.attributes.is_empty() {
            let indentation = indentation.increased();
            write!(f, "\n{indentation}{}:", "attributes".color(palette::FIELD))?;
            self.attributes.format(f, indentation)?;
        }

        Ok(())
    }
}

pub trait Debug {
    fn write(&self, f: &mut Formatter<'_>) -> fmt::Result;
}

impl<BareItem: Format> Debug for super::Item<BareItem> {
    fn write(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.format(f, default())
    }
}

impl Format for super::BareDeclaration {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Function(function) => function.format(f, indentation),
            Self::Data(type_) => type_.format(f, indentation),
            Self::Module(module) => module.format(f, indentation),
            Self::ModuleHeader => FormatStruct::new(f, indentation)
                .name("Module-Header")
                .finish(),
            Self::Use(use_) => use_.format(f, indentation),
            Self::Given(given) => given.format(f, indentation),
        }
    }
}

impl fmt::Debug for super::BareDeclaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, default())
    }
}

impl Format for super::Function {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Function-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::Data {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Data-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("constructors", &self.declarations)
            .finish()
    }
}

impl Format for super::Module {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Module-Declaration")
            .field("binder", &self.binder)
            .field("file", &self.file)
            .field("declarations", &self.declarations)
            .finish()
    }
}

impl Format for SourceFileIndex {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Source-File-Index")
            .finish()?;
        write!(f, " {}", self.value().to_string().color(palette::VERBATIM))
    }
}

impl Format for super::Use {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Use-Declaration")
            .field("bindings", &self.bindings)
            .finish()
    }
}

impl Format for super::BareUsePathTree {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        let struct_ = FormatStruct::new(f, indentation);

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

impl Format for super::Given {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Given-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::Body {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        let struct_ = FormatStruct::new(f, indentation);

        match self {
            Self::Block { fields } => struct_.name("Block").field("fields", fields),
            Self::Expression { body } => struct_.name("Expression").field("body", body),
        }
        .finish()
    }
}

impl Format for super::BareExpression {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::QuantifiedType(type_) => type_.format(f, indentation),
            Self::Application(application) => application.format(f, indentation),
            Self::NumberLiteral(number) => number.format(f, indentation),
            Self::TextLiteral(text) => text.format(f, indentation),
            Self::Wildcard(hole) => hole.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::Projection(field) => field.format(f, indentation),
            Self::LambdaLiteral(lambda) => lambda.format(f, indentation),
            Self::LetBinding(binding) => binding.format(f, indentation),
            Self::UseBinding(binding) => binding.format(f, indentation),
            Self::CaseAnalysis(analysis) => analysis.format(f, indentation),
            Self::DoBlock(do_) => do_.format(f, indentation),
            Self::SequenceLiteral(sequence) => sequence.format(f, indentation),
            Self::RecordLiteral(record) => record.format(f, indentation),
            Self::Error(_) => write!(f, "{}", "invalid".color(palette::INVALID)),
        }
    }
}

impl fmt::Debug for super::BareExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, default())
    }
}

impl Format for super::QuantifiedType {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Quantified-Type")
            .field("quantifier", &self.quantifier)
            .field("parameters", &self.parameters)
            .field("codomain", &self.codomain)
            .finish()
    }
}

impl Format for super::Quantifier {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Pi => "pi",
                Self::Sigma => "sigma",
            }
            .color(palette::SPECIAL_SYMBOL)
        )
    }
}

impl<T: Format> Format for super::Application<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Application")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("callee", &self.callee)
            .field("argument", &self.argument)
            .finish()
    }
}

impl Format for super::NumberLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Number-Literal")
            .field("path", &self.path)
            .field(
                "literal",
                // @Task also print span of literal
                &AdHoc(|f, _| write!(f, "{}", self.literal.bare.to_str().color(palette::INVALID))),
            )
            .finish()
    }
}

impl fmt::Display for super::NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{path}.{}", self.literal),
            None => write!(f, "{}", self.literal),
        }
    }
}

impl Format for super::TextLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Text-Literal")
            .field("path", &self.path)
            .field(
                "literal",
                // @Task also print span of literal
                &AdHoc(|f, _| format_text_literal(self.literal.bare.to_str(), f)),
            )
            .finish()
    }
}

impl fmt::Display for super::TextLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // @Bug this doesn't escape anything at all!
        match &self.path {
            Some(path) => write!(f, "{path}.\"{}\"", self.literal),
            None => write!(f, "\"{}\"", self.literal),
        }
    }
}

impl Format for super::Wildcard {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        let struct_ = FormatStruct::new(f, indentation);

        match self {
            Self::Silent => struct_.name("Wildcard.Silent"),
            Self::Signaling { tag } => struct_.name("Wildcard.Signaling").field("tag", tag),
        }
        .finish()
    }
}

impl Format for super::Path {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Path")
            .field("hanger", &self.hanger)
            .field("segments", &self.segments)
            .finish()
    }
}

impl Format for super::Projection {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Projection")
            .field("basis", &self.basis)
            .field("field", &self.field)
            .finish()
    }
}

impl Format for super::BareHanger {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(f, "{}", self.to_string().color(Color::BrightYellow))
    }
}

impl Format for super::LambdaLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Lambda-Literal")
            .field("parameters", &self.parameters)
            .field("codomain", &self.codomain)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::LetBinding {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Let-Binding")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Format for super::UseBinding {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Use-Binding")
            .field("bindings", &self.bindings)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Format for super::CaseAnalysis {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Case-Analysis")
            .field("scrutinee", &self.scrutinee)
            .field("cases", &self.cases)
            .finish()
    }
}

impl Format for super::Case {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Case")
            .field("pattern", &self.pattern)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::DoBlock {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Do-Block")
            .field("statements", &self.statements)
            .finish()
    }
}

impl<T: Format> Format for super::SequenceLiteral<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Sequence-Literal")
            .field("path", &self.path)
            .field("elements", &self.elements)
            .finish()
    }
}

impl<T: Format> Format for super::RecordLiteral<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Record-Literal")
            .field("path", &self.path)
            .field("fields", &self.fields)
            .field("base", &self.base)
            .finish()
    }
}

impl<T: Format> Format for super::Field<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Field")
            .field("name", &self.name)
            .field("value", &self.item)
            .finish()
    }
}

impl Format for super::BareParameter {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Parameter")
            .field("kind", &self.kind)
            .field("binder", &self.binder)
            .field("type", &self.type_)
            .finish()
    }
}

impl Format for super::LocalBinder {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Named(binder) => binder.format(f, indentation),
            Self::Discarded(span) => {
                span.format(f, indentation)?;
                write!(f, " {}", "discarded".color(palette::SPECIAL_SYMBOL))
            }
        }
    }
}

// @Bug this does not properly handle symbols in paths
// @Task take a look at `Component::index_to_path`
impl fmt::Display for super::Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(hanger) = &self.hanger {
            write!(f, "{hanger}")?;

            if !self.segments.is_empty() {
                write!(f, ".")?;
            }
        }

        write!(f, "{}", self.segments.iter().join_with("."))
    }
}

impl fmt::Display for super::BareHanger {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Format for super::Statement {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Let(let_) => let_.format(f, indentation),
            Self::Use(use_) => use_.format(f, indentation),
            Self::Expression(expression) => expression.format(f, indentation),
        }
    }
}

impl Format for super::LetStatement {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Let-Statement")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type", &self.type_)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::BindingMode {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Plain => "plain",
                Self::Effectful => "effectful",
            }
            .color(palette::SPECIAL_SYMBOL)
        )
    }
}

impl Format for super::BarePattern {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Wildcard(wildcard) => wildcard.format(f, indentation),
            Self::NumberLiteral(number) => number.format(f, indentation),
            Self::TextLiteral(text) => text.format(f, indentation),
            Self::SequenceLiteral(sequence) => sequence.format(f, indentation),
            Self::RecordLiteral(record) => record.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::LetBinding(binder) => binder.format(f, indentation),
            Self::Application(application) => application.format(f, indentation),
        }
    }
}

impl Format for Identifier {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span().format(f, indentation)?;
        write!(f, " ")?;
        FormatStruct::new(f, indentation)
            .name("Identifier")
            .finish()?;
        write!(f, " {}", self.to_str().color(palette::VERBATIM))
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{:width$}",
            self.to_str(),
            width = f.width().unwrap_or_default()
        )
    }
}

impl Format for ParameterKind {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Explicit => "explicit",
                Self::Implicit => "implicit",
                Self::Context => "context",
            }
            .color(palette::SPECIAL_SYMBOL)
        )
    }
}

impl Format for super::BareAttribute {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        let struct_ = FormatStruct::new(f, indentation);
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

impl Format for super::BareAttributeArgument {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::NumberLiteral(number) => {
                FormatStruct::new(f, indentation)
                    .name("Number-Literal")
                    .finish()?;
                write!(f, " {}", number.to_str().color(palette::VERBATIM))
            }
            Self::TextLiteral(text) => {
                FormatStruct::new(f, indentation)
                    .name("Text-Literal")
                    .finish()?;
                write!(f, " ")?;
                format_text_literal(text.to_str(), f)
            }
            Self::Path(path) => path.format(f, indentation),
            Self::Named(named) => named.format(f, indentation),
        }
    }
}

impl Format for super::NamedAttributeArgument {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Named-Attribute-Argument")
            .field("binder", &self.binder)
            .field("value", &self.value)
            .finish()
    }
}

struct AdHoc<F: Fn(&mut Formatter<'_>, Indentation) -> Result>(F);

impl<F: Fn(&mut Formatter<'_>, Indentation) -> Result> Format for AdHoc<F> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        (self.0)(f, indentation)
    }
}
