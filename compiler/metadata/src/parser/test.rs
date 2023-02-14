//! Positive behavior tests for parsing the custom metadata format.
//!
//! Negative behavior tests are UI tests and found in `/test/ui/tests/metadata/`.

use super::{Record, Value};
use diagnostics::{error::Result, Reporter};
use span::{span, FileName, SourceMap, Spanned};
use std::{
    default::default,
    sync::{Arc, RwLock},
};
use utilities::difference;

fn parse(source: &str) -> Result<Value> {
    let map: Arc<RwLock<SourceMap>> = default();
    let file = map
        .write()
        .unwrap()
        .add(FileName::Anonymous, Arc::new(source.to_owned()), None);
    let reporter = Reporter::stderr().with_map(map.clone());
    super::super::parse(file, &map, &reporter)
}

fn assert_eq(actual: Result<Value>, expected: Value) {
    match actual {
        Ok(actual) => {
            // @Note for some reason, despite the `#`, large Records are not formatted with multiple line breaks
            assert!(
                actual == expected,
                "the actual value outputted by the parser does not match the expected one:\n{}",
                difference(&format!("{expected:#?}"), &format!("{actual:#?}"), "")
            );
        }
        _ => panic!("expected the value ‘{expected:?}’ but an error was (silently) reported"),
    }
}

#[allow(unused_imports)]
use utilities::no_std_assert as assert_eq;
#[allow(unused_imports)]
use utilities::no_std_assert as assert_ne;

#[test]
fn empty() {
    assert_eq(parse(""), Value::new(span(1, 1), Record::default().into()));
}

#[test]
fn sole_line_break() {
    assert_eq(
        parse("\n"),
        Value::new(span(1, 2), Record::default().into()),
    );
}

#[test]
fn comment() {
    assert_eq(
        parse("# there it is"),
        Value::new(span(1, 14), Record::default().into()),
    );
}

#[test]
fn comments() {
    assert_eq(
        parse(
            "\
# one
##two  
# \"three",
        ),
        Value::new(span(1, 23), Record::default().into()),
    );
}

#[test]
fn bool() {
    assert_eq(parse("true"), Value::new(span(1, 5), true.into()));
}

#[test]
fn padded_bool() {
    assert_eq(parse("  true "), Value::new(span(3, 7), true.into()));
}

#[test]
fn commented_number() {
    assert_eq(
        parse(
            "\
# description
0",
        ),
        Value::new(span(15, 16), 0.into()),
    );
}

#[test]
fn number() {
    assert_eq(parse("1001"), Value::new(span(1, 5), 1001.into()));
}

#[test]
fn negative_number() {
    assert_eq(parse("-89210"), Value::new(span(1, 7), (-89_210).into()));
}

#[test]
fn number_with_separators() {
    assert_eq(
        parse("9'999'998"),
        Value::new(span(1, 10), 9_999_998.into()),
    );
}

#[test]
fn text() {
    assert_eq(
        parse(r#""filler""#),
        Value::new(span(1, 9), "filler".into()),
    );
}

#[test]
fn multi_line_text() {
    assert_eq(
        parse("\"split\n across\n  lines\""),
        Value::new(span(1, 24), "split\n across\n  lines".into()),
    );
}

#[test]
fn identifier() {
    assert_eq(parse("raw"), Value::new(span(1, 4), "raw".into()));
}

#[test]
fn dashed_identifier() {
    assert_eq(parse("split-up"), Value::new(span(1, 9), "split-up".into()));
}

#[test]
fn array() {
    assert_eq(
        parse(r#"[[], "it",23'000, spacer ,{  }]"#),
        Value::new(
            span(1, 32),
            [
                Value::new(span(2, 4), [].into()),
                Value::new(span(6, 10), "it".into()),
                Value::new(span(11, 17), 23_000.into()),
                Value::new(span(19, 25), "spacer".into()),
                Value::new(span(27, 31), Record::default().into()),
            ]
            .into(),
        ),
    );
}

#[test]
fn array_trailing_comma() {
    assert_eq(
        parse("[false,]"),
        Value::new(span(1, 9), [Value::new(span(2, 7), false.into())].into()),
    );
}

#[test]
fn record() {
    assert_eq(
        parse(
            r#"{uno: ".","dos:" :19
,   tres:{}}"#,
        ),
        Value::new(
            span(1, 34),
            Record::from_iter([
                (
                    Spanned::weak(span(2, 5), "uno".into()),
                    Value::new(span(7, 10), ".".into()),
                ),
                (
                    Spanned::weak(span(11, 17), "dos:".into()),
                    Value::new(span(19, 21), 19.into()),
                ),
                (
                    Spanned::weak(span(26, 30), "tres".into()),
                    Value::new(span(31, 33), Record::default().into()),
                ),
            ])
            .into(),
        ),
    );
}

#[test]
fn record_trailing_comma() {
    assert_eq(
        parse("{x:1,}"),
        Value::new(
            span(1, 7),
            Record::from_iter([(
                Spanned::weak(span(2, 3), "x".into()),
                Value::new(span(4, 5), 1.into()),
            )])
            .into(),
        ),
    );
}

#[test]
fn top_level_bracketless_record() {
    assert_eq(
        parse(
            r#"alpha: 234,
# parenthesis
 beta :false ,
left:right,
"gam ma"
: {"":[]}
"#,
        ),
        Value::new(
            span(1, 73),
            Record::from_iter([
                (
                    Spanned::weak(span(1, 6), "alpha".into()),
                    Value::new(span(8, 11), 234.into()),
                ),
                (
                    Spanned::weak(span(28, 32), "beta".into()),
                    Value::new(span(34, 39), false.into()),
                ),
                (
                    Spanned::weak(span(42, 46), "left".into()),
                    Value::new(span(47, 52), "right".into()),
                ),
                (
                    Spanned::weak(span(54, 62), "gam ma".into()),
                    Value::new(
                        span(65, 72),
                        Record::from_iter([(
                            Spanned::weak(span(66, 68), String::new()),
                            Value::new(span(69, 71), [].into()),
                        )])
                        .into(),
                    ),
                ),
            ])
            .into(),
        ),
    );
}

#[test]
fn record_key_in_dash_case() {
    assert_eq(
        parse(
            "\
fun_ky: \"\",
k-eys: \"\",
",
        ),
        Value::new(
            span(1, 24),
            Record::from_iter([
                (
                    Spanned::weak(span(1, 7), "fun_ky".into()),
                    Value::new(span(9, 11), "".into()),
                ),
                (
                    Spanned::weak(span(13, 18), "k-eys".into()),
                    Value::new(span(20, 22), "".into()),
                ),
            ])
            .into(),
        ),
    );
}

#[test]
fn keyword_record_key() {
    assert_eq(
        parse("{ true : 0 }"),
        Value::new(
            span(1, 13),
            Record::from_iter([(
                Spanned::weak(span(3, 7), "true".into()),
                Value::new(span(10, 11), 0.into()),
            )])
            .into(),
        ),
    );
}

#[test]
fn top_level_keyword_record_key() {
    assert_eq(
        parse("false:true"),
        Value::new(
            span(1, 11),
            Record::from_iter([(
                Spanned::weak(span(1, 6), "false".into()),
                Value::new(span(7, 11), true.into()),
            )])
            .into(),
        ),
    );
}
