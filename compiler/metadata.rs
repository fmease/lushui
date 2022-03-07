//! A custom human-readable metadata file format.
//!
//! # Missing Features
//!
//! * raw text
//! * multi-line text with indentation awareness
//! * text escape sequences
//! * negative numbers
//! * keywords as keys (e.g. `false: false`)

use crate::{
    diagnostics::{reporter::ErrorReported, Code, Diagnostic, Reporter},
    error::Result,
    span::{SourceFileIndex, SourceMap, Span, Spanned, Spanning, WeaklySpanned},
    utility::{obtain, HashMap},
};
use derivation::Discriminant;
use std::{fmt, sync::RwLock};

mod lexer;
mod parser;

pub type Value = Spanned<ValueKind>;
pub type Record<K = String, V = Value> = HashMap<WeaklySpanned<K>, V>;

pub fn parse(
    file_index: SourceFileIndex,
    map: &RwLock<SourceMap>,
    reporter: &Reporter,
) -> Result<Value> {
    let tokens = lexer::Lexer::new(&map.read().unwrap()[file_index])
        .lex()
        .value;
    parser::Parser::new(file_index, &tokens, map, reporter).parse()
}

#[derive(Debug, Discriminant)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[discriminant(type_: Type)]
pub enum ValueKind {
    Boolean(bool),
    Integer(i64),
    Text(String),
    List(Vec<Value>),
    Record(Record),
}

impl From<bool> for ValueKind {
    fn from(bool: bool) -> Self {
        Self::Boolean(bool)
    }
}

impl TryFrom<ValueKind> for bool {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Boolean(value) => value).ok_or(TypeError {
            expected: Type::Boolean,
            actual: type_,
        })
    }
}

impl From<i64> for ValueKind {
    fn from(integer: i64) -> Self {
        Self::Integer(integer)
    }
}

impl TryFrom<ValueKind> for i64 {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Integer(value) => value).ok_or(TypeError {
            expected: Type::Integer,
            actual: type_,
        })
    }
}

impl From<String> for ValueKind {
    fn from(text: String) -> Self {
        Self::Text(text)
    }
}

impl From<&str> for ValueKind {
    fn from(text: &str) -> Self {
        Self::Text(text.into())
    }
}

impl TryFrom<ValueKind> for String {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Text(value) => value).ok_or(TypeError {
            expected: Type::Text,
            actual: type_,
        })
    }
}

impl From<Vec<Value>> for ValueKind {
    fn from(array: Vec<Value>) -> Self {
        Self::List(array)
    }
}

impl<const N: usize> From<[Value; N]> for ValueKind {
    fn from(array: [Value; N]) -> Self {
        Self::List(array.into())
    }
}

impl TryFrom<ValueKind> for Vec<Value> {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::List(value) => value).ok_or(TypeError {
            expected: Type::List,
            actual: type_,
        })
    }
}

impl From<Record> for ValueKind {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl TryFrom<ValueKind> for Record {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Record(value) => value).ok_or(TypeError {
            expected: Type::Record,
            actual: type_,
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::Text => write!(f, "text"),
            Self::List => write!(f, "list"),
            Self::Record => write!(f, "record"),
        }
    }
}

pub struct TypeError {
    pub(crate) expected: Type,
    pub(crate) actual: Type,
}

/// The span of the key excluding quotes if there are any.
pub(crate) fn key_content_span(key: impl Spanning, map: &SourceMap) -> Span {
    fn key_content_span(span: Span, map: &SourceMap) -> Span {
        let is_quoted = map.snippet(span).starts_with('"');

        if is_quoted {
            span.trim(1)
        } else {
            span
        }
    }

    key_content_span(key.span(), map)
}

pub(crate) fn convert<T: TryFrom<ValueKind, Error = TypeError>>(
    value: Value,
    reporter: &Reporter,
) -> Result<Spanned<T>> {
    Ok(Spanned::new(
        value.span,
        value
            .value
            .try_into()
            .map_err(|TypeError { expected, actual }| {
                Diagnostic::error()
                    .code(Code::E800)
                    .message(format!(
                        "expected type `{expected}` but got type `{actual}`",
                    ))
                    .labeled_primary_span(value.span, "has the wrong type")
                    .report(reporter)
            })?,
    ))
}

pub(crate) struct RecordWalker<'r> {
    record: Spanned<Record>,
    reporter: &'r Reporter,
}

impl<'r> RecordWalker<'r> {
    pub(crate) fn new(record: Spanned<Record>, reporter: &'r Reporter) -> Self {
        Self { record, reporter }
    }

    pub(crate) fn take<T>(&mut self, key: &str) -> Result<Spanned<T>>
    where
        T: TryFrom<ValueKind, Error = TypeError>,
    {
        match self.record.value.remove(key) {
            Some(value) => convert(value, self.reporter),
            None => Err(Diagnostic::error()
                .code(Code::E802)
                .message(format!("the record does not contain the entry `{key}`"))
                .primary_span(&self.record)
                .report(self.reporter)),
        }
    }

    pub(crate) fn take_optional<T>(&mut self, key: &str) -> Result<Option<Spanned<T>>>
    where
        T: TryFrom<ValueKind, Error = TypeError>,
    {
        match self.record.value.remove(key) {
            Some(value) => convert(value, self.reporter).map(Some),
            None => Ok(None),
        }
    }

    pub(crate) fn exhaust(self) -> Result<Span> {
        if !self.record.value.is_empty() {
            for key in self.record.value.into_keys() {
                // @Question should we use the "unknown" terminology?
                Diagnostic::error()
                    .code(Code::E801)
                    .message(format!("the record contains the unknown entry `{key}`"))
                    .primary_span(key)
                    .report(self.reporter);
            }

            return Err(ErrorReported::new_unchecked());
        }

        Ok(self.record.span)
    }
}
