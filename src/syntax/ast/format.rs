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

const TOKEN_NAME_COLOR: Color = Color::BrightCyan;
const INVALID_COLOR: Color = Color::BrightRed;
const SPECIAL_SYMBOL_COLOR: Color = Color::BrightCyan;
const VERBATIM_COLOR: Color = Color::Yellow;
const ELLIPSIS_COLOR: Color = Color::BrightRed;
const SPAN_COLOR: Color = Color::BrightBlack;
const FIELD_COLOR: Color = Color::BrightWhite;

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
    use super::{Format, Formatter, Indentation, Result, FIELD_COLOR};
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

        pub(super) fn inline(mut self) -> Self {
            self.inline = true;
            self
        }

        pub(super) fn name(mut self, name: &str) -> Self {
            self.result = self.result.and_then(|()| {
                write!(
                    self.formatter,
                    "{}",
                    name.color(super::TOKEN_NAME_COLOR).bold()
                )
            });

            self
        }

        pub(super) fn field(mut self, name: &str, field: &impl Format) -> Self {
            if !self.has_fields {
                self.has_fields = true;
                self.indentation = self.indentation.increased();
            }

            self.result = self.result.and_then(|()| {
                if self.inline {
                    write!(self.formatter, " ")
                } else {
                    writeln!(self.formatter)
                }
            });

            self.result = self.result.and_then(|()| {
                if !self.inline {
                    write!(self.formatter, "{}", self.indentation)?;
                }

                write!(self.formatter, "{}: ", name.color(FIELD_COLOR))?;
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
    const _: () = assert!(MAXIMUM_LENGTH % 2 == 0);

    let length = content.len();

    let content = if length <= MAXIMUM_LENGTH {
        content.color(VERBATIM_COLOR).to_string()
    } else {
        let length_left = MAXIMUM_LENGTH / 2;
        let length_right = length - MAXIMUM_LENGTH / 2;

        let left = &content[..length_left];
        let ellipsis = "....".black().on_color(ELLIPSIS_COLOR);
        let right = &content[length_right..];

        format!("{left}{ellipsis}{right}")
    };

    // `str::replace` would not work here since it breaks the fragile `ColoredString`s
    let content = content
        .split('\n')
        .map(|segment| segment.color(VERBATIM_COLOR))
        .join_with("\u{21b5}".black().on_color(ELLIPSIS_COLOR));

    write!(f, "{}", content)
}

pub trait Format {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result;
}

impl<T: Format> Format for Option<T> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        match self {
            Some(value) => value.format(f, indentation),
            None => write!(f, "{}", "none".color(SPECIAL_SYMBOL_COLOR)),
        }
    }
}

impl<T: Format> Format for [T] {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        if self.is_empty() {
            return write!(f, "{}", "empty".color(SPECIAL_SYMBOL_COLOR));
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
        write!(f, "{}", format!("{self:?}").color(SPAN_COLOR))
    }
}

impl<K: Format> Format for Spanned<K> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.data.format(f, indentation)
    }
}

impl<I: Format> Format for super::Item<I> {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        self.data.format(f, indentation)?;

        if !self.attributes.is_empty() {
            let indentation = indentation.increased();
            write!(f, "\n{indentation}{}:", "attributes".color(FIELD_COLOR))?;
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
            Self::Value(declaration) => declaration.format(f, indentation),
            Self::Data(declaration) => declaration.format(f, indentation),
            Self::Constructor(declaration) => declaration.format(f, indentation),
            Self::Module(declaration) => declaration.format(f, indentation),
            Self::Header => FormatStruct::new(f, indentation).name("Header").finish(),
            Self::Group(declaration) => declaration.format(f, indentation),
            Self::Use(declaration) => declaration.format(f, indentation),
        }
    }
}

impl Debug for super::DeclarationKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        self.format(f, Indentation::default())
    }
}

impl Format for super::Value {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Value-Declaration")
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
        write!(f, " {}", self.value().to_string().color(VERBATIM_COLOR))
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
            Self::NumberLiteral(number) => {
                FormatStruct::new(f, indentation)
                    .name("Number-Literal")
                    .finish()?;
                write!(f, " {}", number.color(VERBATIM_COLOR))
            }
            Self::TextLiteral(text) => {
                FormatStruct::new(f, indentation)
                    .name("Text-Literal")
                    .finish()?;
                write!(f, " ")?;
                format_text_literal(text, f)
            }
            Self::TypedHole(hole) => hole.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::Field(field) => field.format(f, indentation),
            Self::LambdaLiteral(lambda) => lambda.format(f, indentation),
            Self::LetIn(let_in) => let_in.format(f, indentation),
            Self::UseIn(use_in) => use_in.format(f, indentation),
            Self::CaseAnalysis(analysis) => analysis.format(f, indentation),
            Self::DoBlock(do_) => do_.format(f, indentation),
            Self::SequenceLiteral(sequence) => sequence.format(f, indentation),
            Self::Error => write!(f, "{}", "invalid".color(INVALID_COLOR)),
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
            .field("aspect", &self.aspect)
            .field("binder", &self.binder)
            .field("expression", &self.expression)
            .finish()
    }
}

impl Format for super::ParameterAspect {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Parameter-Aspect")
            .inline()
            .field("laziness", &self.laziness)
            .field("fieldness", &self.fieldness)
            .finish()
    }
}

/// Potentially with trailing whitespace.
impl fmt::Display for super::ParameterAspect {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result {
        if self.is_lazy() {
            write!(f, "lazy ")?;
        }

        if self.is_field() {
            write!(f, "::")?;
        }

        Ok(())
    }
}

impl Format for super::Application {
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

impl Format for super::SequenceLiteral {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Sequence-Literal")
            .field("elements", &self.elements)
            .finish()
    }
}

impl Format for super::ParameterGroup {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;
        FormatStruct::new(f, indentation)
            .name("Parameter-Group")
            .field("explicitness", &self.explicitness)
            .field("aspect", &self.aspect)
            .field("parameters", &self.parameters)
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
            Self::NumberLiteral(number) => {
                FormatStruct::new(f, indentation)
                    .name("Number-Literal")
                    .finish()?;
                write!(f, " {}", number.color(VERBATIM_COLOR))
            }
            Self::TextLiteral(text) => {
                FormatStruct::new(f, indentation)
                    .name("Text-Literal")
                    .finish()?;
                write!(f, " ")?;
                format_text_literal(text, f)
            }
            Self::SequenceLiteralPattern(sequence) => sequence.format(f, indentation),
            Self::Path(path) => path.format(f, indentation),
            Self::Binder(binder) => binder.format(f, indentation),
            Self::Deapplication(deapplication) => deapplication.format(f, indentation),
        }
    }
}

impl Format for super::SequenceLiteralPattern {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Sequence-Literal")
            .field("elements", &self.elements)
            .finish()
    }
}

impl Format for super::Binder {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Pattern-Binder")
            .inline()
            .field("binder", &self.binder)
            .finish()
    }
}

impl Format for super::Deapplication {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        FormatStruct::new(f, indentation)
            .name("Deapplication")
            .field("explicitness", &self.explicitness)
            .field("binder", &self.binder)
            .field("callee", &self.callee)
            .field("argument", &self.argument)
            .finish()
    }
}

impl Format for Identifier {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span().format(f, indentation)?;
        write!(f, " ")?;
        FormatStruct::new(f, indentation)
            .name("Identifier")
            .finish()?;
        write!(f, " {}", self.as_str().color(VERBATIM_COLOR))
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
            .color(SPECIAL_SYMBOL_COLOR)
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

impl Format for super::Attribute {
    fn format(&self, f: &mut Formatter<'_>, indentation: Indentation) -> Result {
        self.span.format(f, indentation)?;
        write!(f, " ")?;

        FormatStruct::new(f, indentation)
            .name("Attribute")
            .field("binder", &self.binder)
            .field("arguments", &self.arguments)
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
                write!(f, " {}", number.color(VERBATIM_COLOR))
            }
            Self::TextLiteral(text) => {
                FormatStruct::new(f, indentation)
                    .name("Text-Literal")
                    .finish()?;
                write!(f, " ")?;
                format_text_literal(text, f)
            }
            Self::TextEncodedInSpan => FormatStruct::new(f, indentation)
                .name("Text-Encoded-In-Span")
                .finish(),
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
