use super::{Code, Diagnostic};
use crate::{
    span::{span, SourceMap},
    utility::difference,
};

#[track_caller]
fn assert_format(diagnostic: &Diagnostic, map: Option<&SourceMap>, expected: &str) {
    colored::control::set_override(false);
    let actual = diagnostic.format_for_terminal(map);
    // colored::control::unset_override(); // conflicts with parallel test execution

    if actual != expected {
        // @Beacon @Bug this diff now isn't colored because of the global setting
        // @Task replace colored with something more flexible

        panic!(
            "the terminal format differs:\n{}",
            difference(expected, &actual, "\n"),
        );
    }
}

#[test]
fn teminal_format_no_highlights() {
    let diagnostic = Diagnostic::error().code(Code::E000).message("summary");

    assert_format(&diagnostic, None, "error[E000]: summary");
}

#[test]
fn terminal_format_single_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\ngamma\n".into());

    let diagnostic = Diagnostic::error()
        .message("message")
        .primary_span(span(8, 11));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: message
 --> :2:2
  |
2 | beta
  |  ^^^
  |",
    );
}

#[test]
fn terminal_format_two_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n".into());

    let diagnostic = Diagnostic::error().primary_span(span(1, 9));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error
 --> :1:1
  |
1 |   alpha
  |  _^
2 | | beta
  | |__^
  |",
    );
}

#[test]
fn terminal_format_multi_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\ngamma\ndelta\nepsilon".into());

    let diagnostic = Diagnostic::error()
        .code(Code::E000)
        .message("explanation")
        .primary_span(span(9, 23));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error[E000]: explanation
 --> :2:3
  |
2 |   beta
 ... ___^
4 | | delta
  | |_____^
  |",
    );
}

#[test]
fn terminal_format_triple_digit_line_number() {
    let mut map = SourceMap::default();
    map.add(None, {
        let mut content = "\n".repeat(120);
        content += "这是一个句子";
        content
    });

    let diagnostic = Diagnostic::warning()
        .message("this is a sentence")
        .primary_span(span(124, 133));

    // @Bug ":4" should be ":2" (#chars, not #bytes)
    assert_format(
        &diagnostic,
        Some(&map),
        "\
warning: this is a sentence
   --> :121:4
    |
121 | 这是一个句子
    |   ^^^^^^
    |",
    );
}

#[test]
fn terminal_format_primary_secondary_highlights() {
    let mut map = SourceMap::default();
    map.add(None, "2ndry\nPRIM\n2ndry\n".into());

    let diagnostic = Diagnostic::error()
        .code(Code::E001)
        .message("important")
        .primary_span(span(7, 11))
        .secondary_span(span(1, 4))
        .secondary_span(span(15, 17));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error[E001]: important
 --> :1:1
  |
1 | 2ndry
  | ---
  |
 --> :2:1
  |
2 | PRIM
  | ^^^^
  |
 --> :3:4
  |
3 | 2ndry
  |    --
  |",
    );
}

#[test]
fn terminal_format_primary_secondary_highlight_differing_line_number_widths() {
    let mut map = SourceMap::default();
    map.add(None, "\nprimary\n\n\n\n\n\n\n\n\n\nsecondary\n".into());

    let diagnostic = Diagnostic::bug()
        .message("placeholder")
        .primary_span(span(3, 7))
        .secondary_span(span(19, 28));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal compiler error: placeholder
  --> :2:2
   |
 2 | primary
   |  ^^^^
   |
  --> :12:1
   |
12 | secondary
   | ---------
   |",
    );
}

#[test]
fn terminal_format_highlights_in_different_files() {
    let mut map = SourceMap::default();
    map.add(Some("ONE".into()), "a\nbc\ndef\n".into());
    map.add(Some("TWO".into()), "zyx".into());

    let diagnostic = Diagnostic::debug()
        .primary_span(span(4, 5))
        .secondary_span(span(11, 13));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal debugging message
 --> ONE:2:2
  |
2 | bc
  |  ^
  |
 --> TWO:1:1
  |
1 | zyx
  | --
  |",
    );
}

#[test]
fn terminal_format_highlights_same_line() {
    let mut map = SourceMap::default();
    map.add(Some("identity".into()), "sequence\n".into());

    let diagnostic = Diagnostic::error()
        .message("tag")
        .primary_span(span(1, 6))
        .secondary_span(span(5, 9));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: tag
 --> identity:1:1
  |
1 | sequence
  | ^^^^^
  |
 --> identity:1:5
  |
1 | sequence
  |     ----
  |",
    );
}

#[test]
fn terminal_format_labeled_highlights() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta".into());

    let diagnostic = Diagnostic::error()
        .message("labels")
        .labeled_primary_span(span(2, 4), "pointer")
        .labeled_secondary_span(span(7, 11), "content")
        .labeled_primary_span(span(12, 23), "explanation")
        .labeled_secondary_span(span(28, 33), "message");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: labels
 --> :1:2
  |
1 | alpha
  |  ^^ pointer
  |
 --> :2:1
  |
2 | beta
  | ---- content
  |
 --> :3:1
  |
3 |   gamma
  |  _^
4 | | delta
  | |_____^ explanation
  |
 --> :5:5
  |
5 |   epsilon
  |  _____-
6 | | zeta
  | |_- message
  |",
    );
}

#[test]
fn terminal_format_multi_line_labeled_highlights() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta\n".into());

    let diagnostic = Diagnostic::error()
        .message("multi-line labels")
        .labeled_primary_span(span(2, 4), "pointer\ncontext\nfiller")
        .labeled_secondary_span(span(7, 11), "content\n  indented")
        .labeled_primary_span(span(12, 23), "explanation\naddendum")
        .labeled_secondary_span(span(28, 33), "message\n\nbottom");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: multi-line labels
 --> :1:2
  |
1 | alpha
  |  ^^ pointer
  |     context
  |     filler
  |
 --> :2:1
  |
2 | beta
  | ---- content
  |        indented
  |
 --> :3:1
  |
3 |   gamma
  |  _^
4 | | delta
  | |_____^ explanation
  |         addendum
  |
 --> :5:5
  |
5 |   epsilon
  |  _____-
6 | | zeta
  | |_- message
  |
  |     bottom
  |",
    );
}

#[test]
fn terminal_format_multi_line_labeled_highlights_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta".into());

    let diagnostic = Diagnostic::error()
        .message("multi-line labels")
        .labeled_primary_span(span(2, 4), "pointer\ncontext\nfiller")
        .labeled_secondary_span(span(7, 11), "content\n  indented")
        .labeled_primary_span(span(12, 23), "explanation\naddendum")
        .labeled_secondary_span(span(28, 33), "message\n\nbottom");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: multi-line labels
 --> :1:2
  |
1 | alpha
  |  ^^ pointer
  |     context
  |     filler
  |
 --> :2:1
  |
2 | beta
  | ---- content
  |        indented
  |
 --> :3:1
  |
3 |   gamma
  |  _^
4 | | delta
  | |_____^ explanation
  |         addendum
  |
 --> :5:5
  |
5 |   epsilon
  |  _____-
6 | | zeta
  | |_- message
  |
  |     bottom
  |",
    );
}

#[test]
fn terminal_format_subdiagnostics_no_highlights() {
    let diagnostic = Diagnostic::error()
        .code(Code::E004)
        .message("summary")
        .note("clarification")
        .note("other clarification")
        .help("hint")
        .subdebug("mark");

    assert_format(
        &diagnostic,
        None,
        "\
error[E004]: summary
 note: clarification
 note: other clarification
 help: hint
 debug: mark",
    );
}

#[test]
fn terminal_format_subdiagnostics() {
    let mut map = SourceMap::default();
    map.add(None, "****  ****".into());

    let diagnostic = Diagnostic::warning()
        .message("it")
        .primary_span(span(5, 7))
        .help("helpful")
        .help("less helpful");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
warning: it
 --> :1:5
  |
1 | ****  ****
  |     ^^
  |
 help: helpful
 help: less helpful",
    );
}

#[test]
fn terminal_format_subdiagnostics_two_digit_line_numbers() {
    let mut map = SourceMap::default();
    map.add(None, "****  ****\n".repeat(10));

    let diagnostic = Diagnostic::warning()
        .message("it")
        .primary_span(span(104, 106))
        .help("helpful")
        .help("less helpful");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
warning: it
  --> :10:5
   |
10 | ****  ****
   |     ^^
   |
  help: helpful
  help: less helpful",
    );
}

#[test]
fn terminal_format_multi_line_subdiagnostics() {
    let mut map = SourceMap::default();
    map.add(None, "****  ****".into());

    let diagnostic = Diagnostic::warning()
        .message("it")
        .primary_span(span(5, 7))
        .help("helpful\ntip\nhopefully")
        .subdebug("alpha\nbeta")
        .help("less helpful\ntip\n");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
warning: it
 --> :1:5
  |
1 | ****  ****
  |     ^^
  |
 help: helpful
       tip
       hopefully
 debug: alpha
        beta
 help: less helpful
       tip",
    )
}

#[test]
fn terminal_format_multiple_primary_highlights() {
    let mut map = SourceMap::default();
    map.add(None, "gamma\n".into());

    let diagnostic = Diagnostic::error().primary_spans([span(1, 2), span(3, 4), span(5, 6)]);

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error
 --> :1:1
  |
1 | gamma
  | ^
  |
 --> :1:3
  |
1 | gamma
  |   ^
  |
 --> :1:5
  |
1 | gamma
  |     ^
  |",
    );
}

#[test]
fn terminal_format_zero_length_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "sample\n".into());

    let diagnostic = Diagnostic::debug().message("nil").primary_span(span(3, 3));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal debugging message: nil
 --> :1:3
  |
1 | sample
  |  ><
  |",
    );
}

#[test]
fn terminal_format_zero_length_highlight_start_of_line() {
    let mut map = SourceMap::default();
    map.add(None, "sample\n".into());

    let diagnostic = Diagnostic::debug().message("nil").primary_span(span(1, 1));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal debugging message: nil
 --> :1:1
  |
1 |  sample
  | ><
  |",
    );
}

#[test]
fn terminal_format_highlight_line_break() {
    let mut map = SourceMap::default();
    map.add(
        None,
        "This is a sentence.\nThis is a follow-up sentence.\n".into(),
    );

    let diagnostic = Diagnostic::error().labeled_primary_span(span(20, 20), "EOL");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error
 --> :1:20
  |
1 | This is a sentence.
  |                   >< EOL
  |",
    );
}

#[test]
fn terminal_format_highlight_end_of_input() {
    let mut map = SourceMap::default();
    map.add(None, "This is a sentence.".into());

    let diagnostic = Diagnostic::error().labeled_primary_span(span(20, 20), "EOI");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error
 --> :1:20
  |
1 | This is a sentence.
  |                   >< EOI
  |",
    );
}

/// The trailing line break should not move the highlighted end of input
/// to a new line. It should look as if there was no trailing line break.
#[test]
fn terminal_format_highlight_end_of_input_with_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "This is a sentence.\n".into());

    let diagnostic = Diagnostic::error().labeled_primary_span(span(21, 21), "EOI");

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error
 --> :1:21
  |
1 | This is a sentence.
  |                   >< EOI
  |",
    );
}

/// A highlight dontaining a trailing line break should not make it
/// a multi-line highlight. It should look as if there was no
/// trailing line break. Optimally, a highlight should not contain
/// a trailing line break.
#[test]
fn terminal_format_highlight_containing_final_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "This is a sentence.\n".into());

    let diagnostic = Diagnostic::warning()
        .message("weird corner case")
        .primary_span(span(1, 21));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
warning: weird corner case
 --> :1:1
  |
1 | This is a sentence.
  | ^^^^^^^^^^^^^^^^^^^
  |",
    );
}

/// A highlight that includes the end of input virtual location
/// should look as if the end of input was not included.
/// Preferably, highlights should not contain the trailing end of input.
/// We might want to change this to panic.
#[test]
fn terminal_format_highlight_containing_final_end_of_input() {
    let mut map = SourceMap::default();
    map.add(None, "EVERYTHING\n".into());

    let diagnostic = Diagnostic::bug().primary_span(span(1, 13));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal compiler error
 --> :1:1
  |
1 | EVERYTHING
  | ^^^^^^^^^^
  |",
    );
}

#[test]
fn terminal_format_highlight_in_empty_file() {
    let mut map = SourceMap::default();
    map.add(Some("empty.txt".into()), String::new());

    let diagnostic = Diagnostic::error()
        .message("this file has to contain something reasonable")
        .primary_span(span(1, 1));

    assert_format(
        &diagnostic,
        Some(&map),
        "\
error: this file has to contain something reasonable
 --> empty.txt:1:1
  |
1 |  
  | ><
  |",
    );
}

// @Task Fix this!
#[test]
#[ignore = "weird corner case"]
fn terminal_format_two_line_break_highlight_containing_first() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n\ngamma".into());

    let diagnostic = Diagnostic::bug().primary_span(span(1, 12));

    // @Note I don't actually know how it should be rendered
    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal compiler error
 --> :1:1
  |
1 |   alpha
  |  _^
2 | | beta
  | |____^
  |",
    );
}

// @Task Fix this!
#[test]
#[ignore = "weird corner case"]
fn terminal_format_two_line_breaks_highlight_containing_second() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\n\n".into());

    let diagnostic = Diagnostic::bug().primary_span(span(1, 8));

    // @Note I don't actually know how it should be rendered
    assert_format(
        &diagnostic,
        Some(&map),
        "\
internal compiler error
 --> :1:1
  |
1 | alpha
  | ^^^^^
  |",
    );
}
