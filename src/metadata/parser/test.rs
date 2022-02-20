//! Positive behavior tests for parsing the custom metadata format.
//!
//! Negative tests are yet to be written. In any case, they have to be UI tests.

use std::collections::BTreeMap;

use super::Value;
use crate::{
    diagnostics::reporter::SilentReporter,
    error::Result,
    span::{span, SourceMap, WeaklySpanned},
    utility::difference,
};

// @Task don't use a silent reporter!
// @Task don't use the assert_eq macro but a diff'ing assert function!

fn parse(source: &str) -> Result<Value> {
    let map = SourceMap::shared();
    let file = map.borrow_mut().add(None, source.to_owned());
    super::super::parse(file, map, &SilentReporter.into())
}

fn assert_eq(actual: Result<Value>, expected: Value) {
    match actual {
        Ok(actual) => {
            if actual != expected {
                // @Note for some reason, despite the `#`, large BTreeMaps are not formatted with multiple line breaks
                panic!(
                    "the actual value outputted by the parser does not match the expected one:\n{}",
                    difference(&format!("{expected:#?}"), &format!("{actual:#?}"), ""),
                );
            }
        }
        _ => panic!("expected the value `{expected:?}` but an error was (silently) reported"),
    }
}

#[allow(unused_imports)]
use crate::utility::no_std_assert as assert_eq;
#[allow(unused_imports)]
use crate::utility::no_std_assert as assert_ne;

#[test]
fn empty() {
    assert_eq(
        parse(""),
        Value::new(span(1, 1), BTreeMap::default().into()),
    );
}

#[test]
fn sole_line_break() {
    assert_eq(
        parse("\n"),
        Value::new(span(1, 2), BTreeMap::default().into()),
    );
}

#[test]
fn comment() {
    assert_eq(
        parse("# there it is"),
        Value::new(span(1, 14), BTreeMap::default().into()),
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
        Value::new(span(1, 23), BTreeMap::default().into()),
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
#[ignore = "unimplemented"]
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
fn array() {
    assert_eq(
        parse(r#"[[], "it",23'000  ,{  }]"#),
        Value::new(
            span(1, 25),
            [
                Value::new(span(2, 4), [].into()),
                Value::new(span(6, 10), "it".into()),
                Value::new(span(11, 17), 23_000.into()),
                Value::new(span(20, 24), BTreeMap::default().into()),
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
fn map() {
    assert_eq(
        parse(
            r#"{uno: ".","dos:" :19
,   tres:{}}"#,
        ),
        Value::new(
            span(1, 34),
            BTreeMap::from_iter([
                (
                    WeaklySpanned::new(span(2, 5), "uno".into()),
                    Value::new(span(7, 10), ".".into()),
                ),
                (
                    WeaklySpanned::new(span(11, 17), "dos:".into()),
                    Value::new(span(19, 21), 19.into()),
                ),
                (
                    WeaklySpanned::new(span(26, 30), "tres".into()),
                    Value::new(span(31, 33), BTreeMap::default().into()),
                ),
            ])
            .into(),
        ),
    );
}

#[test]
fn map_trailing_comma() {
    assert_eq(
        parse("{x:1,}"),
        Value::new(
            span(1, 7),
            BTreeMap::from_iter([(
                WeaklySpanned::new(span(2, 3), "x".into()),
                Value::new(span(4, 5), 1.into()),
            )])
            .into(),
        ),
    );
}

#[test]
fn top_level_bracketless_map() {
    assert_eq(
        parse(
            r#"alpha: 234,
# parenthesis
 beta :false ,
"gam ma"
: {"":[]}
"#,
        ),
        Value::new(
            span(1, 61),
            BTreeMap::from_iter([
                (
                    WeaklySpanned::new(span(1, 6), "alpha".into()),
                    Value::new(span(8, 11), 234.into()),
                ),
                (
                    WeaklySpanned::new(span(28, 32), "beta".into()),
                    Value::new(span(34, 39), false.into()),
                ),
                (
                    WeaklySpanned::new(span(42, 50), "gam ma".into()),
                    Value::new(
                        span(53, 60),
                        BTreeMap::from_iter([(
                            WeaklySpanned::new(span(54, 56), "".into()),
                            Value::new(span(57, 59), [].into()),
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
fn dash_case_map_key() {
    assert_eq(
        parse(
            "\
fun_ky: \"\",
k-eys: \"\",
",
        ),
        Value::new(
            span(1, 24),
            BTreeMap::from_iter([
                (
                    WeaklySpanned::new(span(1, 7), "fun_ky".into()),
                    Value::new(span(9, 11), "".into()),
                ),
                (
                    WeaklySpanned::new(span(13, 18), "k-eys".into()),
                    Value::new(span(20, 22), "".into()),
                ),
            ])
            .into(),
        ),
    );
}
