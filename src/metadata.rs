//! A custom human-readable metadata file format.
//!
//! # Missing Features
//!
//! * raw text
//! * multi-line text with indentation awareness
//! * text escape sequences
//! * negative numbers
//! * keywords as keys (e.g. `false: false`)
#![allow(clippy::implicit_hasher)] // false positive

use crate::{
    diagnostics::{reporter::ErrorReported, Code, Diagnostic, Reporter},
    error::Result,
    span::{SharedSourceMap, SourceFileIndex, SourceMap, Span, Spanned, Spanning, WeaklySpanned},
    utility::obtain,
};
use discriminant::Discriminant;
use std::{collections::BTreeMap, fmt};

mod format;
mod lexer;
mod parser;

pub(crate) type Value = Spanned<ValueKind>;

pub(crate) fn parse(
    source_file_index: SourceFileIndex,
    map: SharedSourceMap,
    reporter: &Reporter,
) -> Result<Value> {
    let source_file = &map.borrow()[source_file_index];
    let tokens = lexer::Lexer::new(source_file).lex().value;
    parser::Parser::new(source_file_index, &tokens, map.clone(), reporter).parse()
}

#[derive(Debug, Discriminant)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[discriminant(Type::type_)]
pub(crate) enum ValueKind {
    Bool(bool),
    Integer(i64),
    Text(String),
    Array(Vec<Value>),
    Map(BTreeMap<WeaklySpanned<String>, Value>),
}

impl From<bool> for ValueKind {
    fn from(bool: bool) -> Self {
        Self::Bool(bool)
    }
}

impl TryFrom<ValueKind> for bool {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Bool(value) => value).ok_or(TypeError {
            expected: Type::Bool,
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
        Self::Array(array)
    }
}

impl<const N: usize> From<[Value; N]> for ValueKind {
    fn from(array: [Value; N]) -> Self {
        Self::Array(array.into())
    }
}

impl TryFrom<ValueKind> for Vec<Value> {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Array(value) => value).ok_or(TypeError {
            expected: Type::Array,
            actual: type_,
        })
    }
}

impl From<BTreeMap<WeaklySpanned<String>, Value>> for ValueKind {
    fn from(map: BTreeMap<WeaklySpanned<String>, Value>) -> Self {
        Self::Map(map)
    }
}

impl TryFrom<ValueKind> for BTreeMap<WeaklySpanned<String>, Value> {
    type Error = TypeError;

    fn try_from(value: ValueKind) -> Result<Self, Self::Error> {
        let type_ = value.type_();

        obtain!(value, ValueKind::Map(value) => value).ok_or(TypeError {
            expected: Type::Map,
            actual: type_,
        })
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::Text => write!(f, "text"),
            Self::Array => write!(f, "array"),
            Self::Map => write!(f, "map"),
        }
    }
}

pub(crate) fn content_span_of_key(key: impl Spanning, map: &SourceMap) -> Span {
    fn content_span_of_key(span: Span, map: &SourceMap) -> Span {
        let is_quoted = map.snippet(span).starts_with('"');

        if is_quoted {
            span.trim(1)
        } else {
            span
        }
    }

    content_span_of_key(key.span(), map)
}

// @Beacon @Temporary signature
pub(crate) fn remove_map_entry<T: TryFrom<ValueKind, Error = TypeError>>(
    map: Spanned<&mut BTreeMap<WeaklySpanned<String>, Value>>,
    key: &str,
    path: Option<String>,
    reporter: &Reporter,
) -> Result<Spanned<T>> {
    match map.value.remove(key) {
        Some(value) => convert(key, value, reporter),
        None => Err(Diagnostic::error()
            .code(Code::E802)
            .message(format!(
                "the {} is missing the key `{key}`",
                path.map_or_else(|| "root map".into(), |path| format!("map `{path}`"))
            ))
            .primary_span(map)
            .report(reporter)),
    }
}

pub(crate) fn remove_optional_map_entry<T: TryFrom<ValueKind, Error = TypeError>>(
    map: &mut BTreeMap<WeaklySpanned<String>, Value>,
    key: &str,
    reporter: &Reporter,
) -> Result<Option<Spanned<T>>> {
    match map.remove(key) {
        Some(value) => convert(key, value, reporter).map(Some),
        None => Ok(None),
    }
}

// @Temporary signature
pub(crate) fn check_map_is_empty(
    map: BTreeMap<WeaklySpanned<String>, Value>,
    path: Option<String>,
    reporter: &Reporter,
) -> Result {
    if !map.is_empty() {
        let location = path.map_or_else(|| "root map".into(), |path| format!("map `{path}`"));

        for key in map.into_keys() {
            // @Task improve message
            Diagnostic::error()
                .code(Code::E801)
                .message(format!("unknown key `{key}` in {location}"))
                .primary_span(key.span)
                .report(reporter);
        }

        return Err(ErrorReported::error_will_be_reported_unchecked());
    }

    Ok(())
}

// @Temporary name
pub(crate) fn convert<T: TryFrom<ValueKind, Error = TypeError>>(
    key: &str,
    value: Value,
    reporter: &Reporter,
) -> Result<Spanned<T>> {
    let span = value.span;
    let value = value.value.try_into().map_err(|error: TypeError| {
        Diagnostic::error()
            .code(Code::E800)
            .message(format!(
                "the type of key `{key}` should be {} but it is {}",
                error.expected, error.actual
            ))
            .labeled_primary_span(span, "has the wrong type")
            .report(reporter)
    })?;

    Ok(Spanned::new(span, value))
}

pub(crate) struct TypeError {
    pub(crate) expected: Type,
    pub(crate) actual: Type,
}
