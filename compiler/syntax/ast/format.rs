//! The definition of the textual representation(s) of the [AST](crate::syntax::ast).

use crate::{
    span::{SourceFileIndex, Span, Spanned, Spanning},
    utility::SmallVec,
};
use colored::{Color, Colorize};
use format_struct::FormatStruct;
pub use indentation::Indentation;
use index_map::Index as _;
use joinery::JoinableIterator;
use std::fmt::{self, Debug, Formatter, Result};

use super::{Explicitness, Identifier};

mod color {
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
    use super::{color, Format, Formatter, Indentation, Result};
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
                .and_then(|_| write!(self.formatter, "{}", name.color(color::NAME).bold()));

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

                write!(self.formatter, "{}: ", name.color(color::FIELD))?;
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
        content.color(color::VERBATIM).to_string()
    } else {
        let length_left = MAXIMUM_LENGTH / 2;
        let length_right = length - MAXIMUM_LENGTH / 2;

        let left = &content[..length_left];
        let ellipsis = "....".black().on_color(color::ELLIPSIS);
        let right = &content[length_right..];

        format!("{left}{ellipsis}{right}")
    };

    // `str::replace` would not work here since it breaks the fragile `ColoredString`s
    let content = content
        .split('\n')
        .map(|segment| segment.color(color::VERBATIM))
        .join_with("\u{21b5}".black().on_color(color::ELLIPSIS));

    write!(f, "{}", content)
}

// @Task make this pub(crate) once the type-leak rules are relaxed
pub trait Format {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result;
}

impl<T: Format> Format for Option<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Some(value) => value.format(f, indentation),
            None => write!(f, "{}", "none".color(color::SPECIAL_SYMBOL)),
        }
    }
}

impl<T: Format> Format for [T] {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        if self.is_empty() {
            return write!(f, "{}", "empty".color(color::SPECIAL_SYMBOL));
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

impl Format for Span {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        // @Beacon @Task don't use `colored` here, too many wasted allocations!!
        write!(f, "{}", format!("{self:?}").color(color::SPAN))
    }
}

impl<K: Format> Format for Spanned<K> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.value.format(f, indentation)
    }
}

impl<I: Format> Format for super::Item<I> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.value.format(f, indentation)?;

        if !self.attributes.is_empty() {
            let indentation = indentation.increased();
            write!(f, "\n{indentation}{}:", "attributes".color(color::FIELD))?;
            self.attributes.format(f, indentation)?;
        }

        Ok(())
    }
}

impl<I: Format> Debug for super::Item<I> {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, Indentation::default())
    }
}

impl Format for super::DeclarationKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Function(function) => function.format(f, indentation),
            Self::Data(type_) => type_.format(f, indentation),
            Self::Constructor(constructor) => constructor.format(f, indentation),
            Self::Module(module) => module.format(f, indentation),
            Self::ModuleHeader => FormatStruct::new(f, indentation)
                .name("Module-Header")
                .finish(),
            Self::Group(group) => group.format(f, indentation),
            Self::Use(use_) => use_.format(f, indentation),
        }
    }
}

impl Debug for super::DeclarationKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, Indentation::default())
    }
}

impl Format for super::Function {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Function-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type-annotation", &self.type_annotation)
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
            .field("type-annotation", &self.type_annotation)
            .field("constructors", &self.constructors)
            .finish()
    }
}

impl Format for super::Constructor {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Constructor-Declaration")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type-annotation", &self.type_annotation)
            .field("body", &self.body)
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
        write!(f, " {}", self.value().to_string().color(color::VERBATIM))
    }
}

impl Format for super::Group {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Group-Declaration")
            .field("declarations", &self.declarations)
            .finish()
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

impl Format for super::UsePathTreeKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        let struct_ = FormatStruct::new(f, indentation);

        match self {
            Self::Single { target, binder } => struct_
                .name("Single-Use-Path-Tree")
                .field("target", target)
                .field("binder", binder),
            Self::Multiple { path, bindings } => struct_
                .name("Multiple-Use-Path-Tree")
                .field("path", path)
                .field("bindings", bindings),
        }
        .finish()
    }
}

impl Format for super::ExpressionKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::PiTypeLiteral(pi) => pi.format(f, indentation),
            Self::Application(application) => application.format(f, indentation),
            Self::TypeLiteral => FormatStruct::new(f, indentation)
                .name("Type-Literal")
                .finish(),
            Self::NumberLiteral(number) => number.format(f, indentation),
            Self::TextLiteral(text) => text.format(f, indentation),
            Self::TypedHole(hole) => hole.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::Field(field) => field.format(f, indentation),
            Self::LambdaLiteral(lambda) => lambda.format(f, indentation),
            Self::LetIn(let_in) => let_in.format(f, indentation),
            Self::UseIn(use_in) => use_in.format(f, indentation),
            Self::CaseAnalysis(analysis) => analysis.format(f, indentation),
            Self::DoBlock(do_) => do_.format(f, indentation),
            Self::SequenceLiteral(sequence) => sequence.format(f, indentation),
            Self::Error => write!(f, "{}", "invalid".color(color::INVALID)),
        }
    }
}

impl Debug for super::ExpressionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, Indentation::default())
    }
}

impl Format for super::PiTypeLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Pi-Type-Literal")
            .field("domain", &self.domain)
            .field("codomain", &self.codomain)
            .finish()
    }
}

impl Format for super::Domain {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Domain")
            .field("explicitness", &self.explicitness)
            .field("laziness", &self.laziness)
            .field("binder", &self.binder)
            .field("expression", &self.expression)
            .finish()
    }
}

impl<T: Format> Format for super::Application<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Application")
            .field("explicitness", &self.explicitness)
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
                &AdHoc(|f, _| write!(f, "{}", self.literal.value.color(color::INVALID))),
            )
            .finish()
    }
}

impl fmt::Display for super::NumberLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}.{}", path, self.literal),
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
                &AdHoc(|f, _| format_text_literal(&self.literal.value, f)),
            )
            .finish()
    }
}

impl fmt::Display for super::TextLiteral {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.path {
            Some(path) => write!(f, "{}.{:?}", path, self.literal),
            None => write!(f, "{:?}", self.literal),
        }
    }
}

impl Format for super::TypedHole {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Typed-Hole")
            .field("tag", &self.tag)
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

impl Format for super::Field {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Field")
            .field("base", &self.base)
            .field("member", &self.member)
            .finish()
    }
}

impl Format for super::HangerKind {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(f, "{}", self.to_string().color(Color::BrightYellow))
    }
}

impl Format for super::LambdaLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Lambda-Literal")
            .field("parameters", &self.parameters)
            .field("body-type-annotation", &self.body_type_annotation)
            .field("body", &self.body)
            .finish()
    }
}

impl Format for super::LetIn {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Let-In")
            .field("binder", &self.binder)
            .field("parameters", &self.parameters)
            .field("type-annotation", &self.type_annotation)
            .field("expression", &self.expression)
            .field("scope", &self.scope)
            .finish()
    }
}

impl Format for super::UseIn {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Use-In")
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
            .field("elements", &self.elements)
            .finish()
    }
}

impl Format for super::ParameterKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Parameter")
            .field("explicitness", &self.explicitness)
            .field("laziness", &self.laziness)
            .field("binder", &self.binder)
            .field("type-annotation", &self.type_annotation)
            .finish()
    }
}

impl fmt::Display for super::Path {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if let Some(hanger) = &self.hanger {
            write!(f, "{}", hanger)?;

            if !self.segments.is_empty() {
                write!(f, ".")?;
            }
        }

        write!(f, "{}", self.segments.iter().join_with("."))
    }
}

impl fmt::Display for super::HangerKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name())
    }
}

impl Format for super::Statement {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::Let(let_) => let_.format(f, indentation),
            Self::Use(use_) => use_.format(f, indentation),
            Self::Bind(bind) => bind.format(f, indentation),
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
            .field("type-annotation", &self.type_annotation)
            .field("expression", &self.expression)
            .finish()
    }
}

impl Format for super::BindStatement {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Bind-Statement")
            .field("binder", &self.binder)
            .field("type-annotation", &self.type_annotation)
            .field("expression", &self.expression)
            .finish()
    }
}

impl Format for super::PatternKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::NumberLiteral(number) => number.format(f, indentation),
            Self::TextLiteral(text) => text.format(f, indentation),
            Self::SequenceLiteral(sequence) => sequence.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::Binder(binder) => binder.format(f, indentation),
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
        write!(f, " {}", self.as_str().color(color::VERBATIM))
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        write!(
            f,
            "{:width$}",
            self.as_str(),
            width = f.width().unwrap_or_default()
        )
    }
}

impl Format for Explicitness {
    fn format(&self, f: &mut Formatter<'_>, _: Indentation) -> Result {
        write!(
            f,
            "{}",
            match self {
                Self::Implicit => "implicit",
                Self::Explicit => "explicit",
            }
            .color(color::SPECIAL_SYMBOL)
        )
    }
}

impl fmt::Display for Explicitness {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        match self {
            Self::Implicit => write!(f, "'"),
            Self::Explicit => write!(f, ""),
        }
    }
}

impl Format for super::AttributeKind {
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

impl Format for super::AttributeArgumentKind {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Self::NumberLiteral(number) => {
                FormatStruct::new(f, indentation)
                    .name("Number-Literal")
                    .finish()?;
                write!(f, " {}", number.color(color::VERBATIM))
            }
            Self::TextLiteral(text) => {
                FormatStruct::new(f, indentation)
                    .name("Text-Literal")
                    .finish()?;
                write!(f, " ")?;
                format_text_literal(text, f)
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