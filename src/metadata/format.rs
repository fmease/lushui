use super::{Value, ValueKind};
use crate::syntax::Word;
use std::fmt;

const INDENTATION: usize = 4;

fn format(
    value: &ValueKind,
    indentation: usize,
    is_root: bool,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    match value {
        ValueKind::Bool(bool) => write!(f, "{bool}"),
        // @Question numeric separators?
        ValueKind::Integer(integer) => write!(f, "{integer}"),
        // @Task don't use Rust's escaping logic but our own!
        ValueKind::Text(text) => write!(f, "{text:?}"),
        ValueKind::Array(array) => {
            // very naive and error-prone heuristic
            let is_multi_line = array.len() > 2
                || array.first().map_or(false, |first| {
                    matches!(first.value, ValueKind::Array(_) | ValueKind::Map(_))
                });

            write!(f, "[")?;

            if is_multi_line {
                writeln!(f)?;

                for element in array {
                    let indentation = indentation + INDENTATION;

                    write!(f, "{}", " ".repeat(indentation))?;
                    format(&element.value, indentation, false, f)?;
                    writeln!(f, ",")?;
                }

                write!(f, "{}", " ".repeat(indentation))?;
            } else {
                let mut elements = array.iter();
                // @Beacon @Beacon @Question what happens with a multi-line thingy inside a single-line thingy???
                if let Some(element) = elements.next() {
                    format(&element.value, indentation + INDENTATION, false, f)?;
                }
                for element in elements {
                    write!(f, ", ")?;
                    format(&element.value, indentation + INDENTATION, false, f)?;
                }
            }

            write!(f, "]")
        }
        ValueKind::Map(map) => {
            if is_root {
                for (key, value) in map {
                    format_pair(&key.value, value, indentation, f)?;
                    // @Task don't place a \n after the last comma
                    writeln!(f, ",")?;
                }

                return Ok(());
            }

            if map.is_empty() {
                return write!(f, "{{}}");
            }

            // very naive and error-prone heuristic
            let is_multi_line = map.len() > 2
                || map.values().next().map_or(false, |first| {
                    matches!(first.value, ValueKind::Array(_) | ValueKind::Map(_))
                });

            write!(f, "{{")?;

            if is_multi_line {
                writeln!(f)?;

                for (key, value) in map {
                    let indentation = indentation + INDENTATION;

                    write!(f, "{}", " ".repeat(indentation))?;
                    format_pair(&key.value, value, indentation, f)?;
                    writeln!(f, ",")?;
                }

                write!(f, "{}", " ".repeat(indentation))?;
            } else {
                write!(f, " ")?;

                let mut pairs = map.iter();
                // @Beacon @Beacon @Question what happens with a multi-line thingy inside a single-line thingy???
                if let Some((key, value)) = pairs.next() {
                    format_pair(&key.value, value, indentation + INDENTATION, f)?;
                }
                for (key, value) in pairs {
                    write!(f, ", ")?;
                    format_pair(&key.value, value, indentation + INDENTATION, f)?;
                }

                write!(f, " ")?;
            }

            write!(f, "}}")
        }
    }
}

fn format_pair(
    key: &str,
    value: &Value,
    indentation: usize,
    f: &mut fmt::Formatter<'_>,
) -> fmt::Result {
    if Word::parse(key.to_owned()).is_ok() {
        write!(f, "{key}")?;
    } else {
        // @Task don't use Rust's escaping logic but our own!
        write!(f, "{key:?}")?;
    }
    write!(f, ": ")?;
    format(&value.value, indentation, false, f)
}

impl fmt::Display for ValueKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        format(self, 0, true, f)
    }
}

#[cfg(test)]
mod test {
    #[allow(unused_imports)]
    use crate::utility::no_std_assert as assert_eq;
    use crate::{
        metadata::{Value, ValueKind},
        span::WeaklySpanned,
        utility::difference,
    };
    use std::{collections::BTreeMap, default::default};

    fn assert_eq(actual: &str, expected: &str) {
        if actual != expected {
            panic!(
                "the actual formatted metadata does not match the expected one:\n{}",
                difference(expected, actual, "\n"),
            );
        }
    }

    fn value(value: impl Into<ValueKind>) -> Value {
        Value::new(default(), value.into())
    }

    fn key(key: impl Into<String>) -> WeaklySpanned<String> {
        WeaklySpanned::new(default(), key.into())
    }

    #[test]
    fn array() {
        assert_eq(
            &value([
                value([]),
                value([value("it")]),
                value([value(false), value(true)]),
                value([value(0), value(1), value(2)]),
                value(23_000),
            ])
            .to_string(),
            "\
[
    [],
    [\"it\"],
    [false, true],
    [
        0,
        1,
        2,
    ],
    23000,
]",
        );
    }

    #[test]
    fn map() {
        assert_eq(
            &value(BTreeMap::from_iter([
                (
                    key("complex"),
                    value(BTreeMap::from_iter([
                        (key("alpha"), value(BTreeMap::default())),
                        (
                            key("beta"),
                            value(BTreeMap::from_iter([
                                (key("inner0"), value(23)),
                                (key("inner1"), value("val")),
                            ])),
                        ),
                        (
                            key("not a word"),
                            value([value(BTreeMap::from_iter([
                                (key("one"), value(false)),
                                (key("2"), value(-1)),
                                (key("three:"), value("")),
                            ]))]),
                        ),
                    ])),
                ),
                (key("trail"), value(0)),
            ]))
            .to_string(),
            "\
complex: {
    alpha: {},
    beta: { inner0: 23, inner1: \"val\" },
    \"not a word\": [
        {
            \"2\": -1,
            one: false,
            \"three:\": \"\",
        },
    ],
},
trail: 0,
",
        );
    }
}
