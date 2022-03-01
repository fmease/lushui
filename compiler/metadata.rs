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
    span::{SourceFileIndex, SourceMap, SourceMapCell, Span, Spanned, Spanning, WeaklySpanned},
    utility::{obtain, HashMap},
};
use derivation::Discriminant;
use std::fmt;

mod lexer;
mod parser;

pub(crate) type Value = Spanned<ValueKind>;
pub(crate) type Map<K = String, V = Value> = HashMap<WeaklySpanned<K>, V>;

pub(crate) fn parse(
    source_file_index: SourceFileIndex,
    map: SourceMapCell,
    reporter: &Reporter,
) -> Result<Value> {
    let source_file = &map.borrow()[source_file_index];
    let tokens = lexer::Lexer::new(source_file).lex().value;
    parser::Parser::new(source_file_index, &tokens, map.clone(), reporter).parse()
}

#[derive(Debug, Discriminant)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[discriminant(type_: Type)]
pub(crate) enum ValueKind {
    Boolean(bool),
    Integer(i64),
    Text(String),
    List(Vec<Value>),
    Map(Map),
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

impl From<Map> for ValueKind {
    fn from(map: Map) -> Self {
        Self::Map(map)
    }
}

impl TryFrom<ValueKind> for Map {
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
            Self::Boolean => write!(f, "boolean"),
            Self::Integer => write!(f, "integer"),
            Self::Text => write!(f, "text"),
            Self::List => write!(f, "list"),
            Self::Map => write!(f, "map"),
        }
    }
}

pub(crate) struct TypeError {
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

pub(crate) struct MapWalker<'r> {
    map: Spanned<Map>,
    reporter: &'r Reporter,
}

impl<'r> MapWalker<'r> {
    pub(crate) fn new(map: Spanned<Map>, reporter: &'r Reporter) -> Self {
        Self { map, reporter }
    }

    pub(crate) fn take<T>(&mut self, key: &str) -> Result<Spanned<T>>
    where
        T: TryFrom<ValueKind, Error = TypeError>,
    {
        match self.map.value.remove(key) {
            Some(value) => convert(value, self.reporter),
            None => Err(Diagnostic::error()
                .code(Code::E802)
                .message(format!("the map is missing the key `{key}`"))
                .primary_span(&self.map)
                .report(self.reporter)),
        }
    }

    pub(crate) fn take_optional<T>(&mut self, key: &str) -> Result<Option<Spanned<T>>>
    where
        T: TryFrom<ValueKind, Error = TypeError>,
    {
        match self.map.value.remove(key) {
            Some(value) => convert(value, self.reporter).map(Some),
            None => Ok(None),
        }
    }

    pub(crate) fn exhaust(self) -> Result<Span> {
        if !self.map.value.is_empty() {
            for key in self.map.value.into_keys() {
                // @Task improve message
                Diagnostic::error()
                    .code(Code::E801)
                    .message(format!("the map contains the unknown key `{key}`"))
                    .primary_span(key)
                    .report(self.reporter);
            }

            return Err(ErrorReported::new_unchecked());
        }

        Ok(self.map.span)
    }
}
