//! Deserialization support for Recnot, a human-readable configuration file format.
//!
//! Short for *record notation*.
//!
//! # Missing Features
//!
//! * raw text
//! * multi-line text with indentation awareness
//! * text escape sequences
//! * negative numbers
use derivation::Discriminant;
use diagnostics::{error::Result, reporter::ErasedReportedError, Diag, ErrorCode, Reporter};
use span::{SourceMap, Span, Spanned, Spanning, SrcFileIdx, WeaklySpanned};
use std::{fmt, sync::RwLock};
use utility::{obtain, HashMap};

mod parser;

pub mod lexer;

pub type Value = Spanned<BareValue>;
pub type Record<K = String, V = Value> = HashMap<WeaklySpanned<K>, V>;

pub fn parse(file: SrcFileIdx, map: &RwLock<SourceMap>, rep: &Reporter) -> Result<Value> {
    let tokens = lexer::lex(&map.read().unwrap()[file], &lexer::Options::default());
    parser::parse(tokens, file, map, rep)
}

#[derive(Debug, Discriminant)]
#[cfg_attr(test, derive(PartialEq, Eq))]
#[discriminant(ty: Type)]
pub enum BareValue {
    Boolean(bool),
    Integer(i64),
    Text(String),
    List(Vec<Value>),
    Record(Record),
}

impl From<bool> for BareValue {
    fn from(bool: bool) -> Self {
        Self::Boolean(bool)
    }
}

impl TryFrom<BareValue> for bool {
    type Error = TypeError;

    fn try_from(value: BareValue) -> Result<Self, Self::Error> {
        let ty = value.ty();

        obtain!(value, BareValue::Boolean(value) => value).ok_or(TypeError {
            expected: Type::Boolean,
            actual: ty,
        })
    }
}

impl From<i64> for BareValue {
    fn from(integer: i64) -> Self {
        Self::Integer(integer)
    }
}

impl TryFrom<BareValue> for i64 {
    type Error = TypeError;

    fn try_from(value: BareValue) -> Result<Self, Self::Error> {
        let ty = value.ty();

        obtain!(value, BareValue::Integer(value) => value).ok_or(TypeError {
            expected: Type::Integer,
            actual: ty,
        })
    }
}

impl From<String> for BareValue {
    fn from(text: String) -> Self {
        Self::Text(text)
    }
}

impl From<&str> for BareValue {
    fn from(text: &str) -> Self {
        Self::Text(text.into())
    }
}

impl TryFrom<BareValue> for String {
    type Error = TypeError;

    fn try_from(value: BareValue) -> Result<Self, Self::Error> {
        let ty = value.ty();

        obtain!(value, BareValue::Text(value) => value).ok_or(TypeError {
            expected: Type::Text,
            actual: ty,
        })
    }
}

impl From<Vec<Value>> for BareValue {
    fn from(array: Vec<Value>) -> Self {
        Self::List(array)
    }
}

impl<const N: usize> From<[Value; N]> for BareValue {
    fn from(array: [Value; N]) -> Self {
        Self::List(array.into())
    }
}

impl TryFrom<BareValue> for Vec<Value> {
    type Error = TypeError;

    fn try_from(value: BareValue) -> Result<Self, Self::Error> {
        let ty = value.ty();

        obtain!(value, BareValue::List(value) => value).ok_or(TypeError {
            expected: Type::List,
            actual: ty,
        })
    }
}

impl From<Record> for BareValue {
    fn from(record: Record) -> Self {
        Self::Record(record)
    }
}

impl TryFrom<BareValue> for Record {
    type Error = TypeError;

    fn try_from(value: BareValue) -> Result<Self, Self::Error> {
        let ty = value.ty();

        obtain!(value, BareValue::Record(value) => value).ok_or(TypeError {
            expected: Type::Record,
            actual: ty,
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
    expected: Type,
    actual: Type,
}

trait TextContentSpanExt {
    fn text_content_span(&self, map: &SourceMap) -> Span;
}

impl<T: Spanning> TextContentSpanExt for T {
    fn text_content_span(&self, map: &SourceMap) -> Span {
        fn text_content_span(span: Span, map: &SourceMap) -> Span {
            let is_quoted = map.snippet(span).starts_with('"');

            if is_quoted {
                span.trim(1)
            } else {
                span
            }
        }

        text_content_span(self.span(), map)
    }
}

pub trait WithTextContentSpanExt {
    fn with_text_content_span(self, map: &SourceMap) -> Self;
}

impl<T> WithTextContentSpanExt for Spanned<T> {
    fn with_text_content_span(self, map: &SourceMap) -> Self {
        self.transform(|span| span.text_content_span(map))
    }
}

pub fn convert<T: TryFrom<BareValue, Error = TypeError>>(
    value: Value,
    rep: &Reporter,
) -> Result<Spanned<T>> {
    Ok(Spanned::new(
        value.span,
        value
            .bare
            .try_into()
            .map_err(|TypeError { expected, actual }| {
                Diag::error()
                    .code(ErrorCode::E800)
                    .message(format!(
                        "expected type ‘{expected}’ but got type ‘{actual}’",
                    ))
                    .span(value.span, "has the wrong type")
                    .report(rep)
            })?,
    ))
}

pub struct RecordWalker<'r> {
    record: Spanned<Record>,
    rep: &'r Reporter,
}

impl<'r> RecordWalker<'r> {
    pub fn new(record: Spanned<Record>, rep: &'r Reporter) -> Self {
        Self { record, rep }
    }

    pub fn take<T>(&mut self, key: &str) -> Result<Spanned<T>>
    where
        T: TryFrom<BareValue, Error = TypeError>,
    {
        match self.record.bare.remove(key) {
            Some(value) => convert(value, self.rep),
            None => Err(Diag::error()
                .code(ErrorCode::E802)
                .message(format!("the record does not contain the entry ‘{key}’"))
                .unlabeled_span(&self.record)
                .report(self.rep)),
        }
    }

    pub fn take_optional<T>(&mut self, key: &str) -> Result<Option<Spanned<T>>>
    where
        T: TryFrom<BareValue, Error = TypeError>,
    {
        match self.record.bare.remove(key) {
            Some(value) => convert(value, self.rep).map(Some),
            None => Ok(None),
        }
    }

    pub fn exhaust(self) -> Result<Span> {
        if !self.record.bare.is_empty() {
            for key in self.record.bare.into_keys() {
                // @Question should we use the "unknown" terminology?
                Diag::error()
                    .code(ErrorCode::E801)
                    .message(format!("the record contains the unknown entry ‘{key}’"))
                    .unlabeled_span(key)
                    .report(self.rep);
            }

            return Err(ErasedReportedError::new_unchecked());
        }

        Ok(self.record.span)
    }
}
