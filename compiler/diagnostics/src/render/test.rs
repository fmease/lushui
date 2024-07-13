use crate::{Diag, ErrorCode, LintCode, UnboxedUntaggedDiag};
use span::{span, FileName::Anon, SourceMap};
use std::sync::Arc;
use utility::{
    paint::{epaint, paint_to_string, ColorChoice},
    Changeset, ChangesetExt,
};

#[track_caller]
// FIXME: rename
fn assert_format(diag: &UnboxedUntaggedDiag, map: Option<&SourceMap>, expected: &str) {
    let actual = paint_to_string(
        |painter| diag.render(map, painter),
        // We are not interested in checking the coloring.
        ColorChoice::Never,
    )
    .unwrap();

    if actual != expected {
        // We also lock stdout since the test runner would otherwise interfere.
        let stdout = std::io::stdout().lock();
        epaint(
            |painter| Changeset::new(expected, &actual, "\n").render_with_ledge(painter),
            ColorChoice::Auto,
        )
        .unwrap();
        drop(stdout);

        panic!("the output differs");
    }
}

#[test]
fn format_no_highlights() {
    let diag = Diag::error().code(ErrorCode::E000).message("summary");

    assert_format(&diag, None, "error[E000]: summary");
}

#[test]
fn format_single_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\ngamma\n");

    let diag = Diag::error().message("message").unlabeled_span(span(8, 11));

    assert_format(
        &diag,
        Some(&map),
        "\
error: message
  ┌─ ⟨anonymous⟩:2:2
  │
2 │ beta
  │  ═══",
    );
}

#[test]
fn format_two_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\n");

    let diag = Diag::error().unlabeled_span(span(1, 9));

    assert_format(
        &diag,
        Some(&map),
        "\
error
  ┌─ ⟨anonymous⟩:1:1
  │
1 │   alpha
  │ ╔═╝
2 │ ║ beta
  │ ╚══╝",
    );
}

#[test]
fn format_multi_line_primary_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\ngamma\ndelta\nepsilon");

    let diag = Diag::error()
        .code(ErrorCode::E000)
        .message("explanation")
        .unlabeled_span(span(9, 23));

    assert_format(
        &diag,
        Some(&map),
        "\
error[E000]: explanation
  ┌─ ⟨anonymous⟩:2:3
  │
2 │   beta
  · ╔═══╝
4 │ ║ delta
  │ ╚═════╝",
    );
}

#[test]
fn format_triple_digit_line_number() {
    let mut map = SourceMap::default();
    map.add(
        Anon,
        {
            let mut content = "\n".repeat(120);
            content += "这是一个句子";
            Arc::new(content)
        },
        None,
    );

    let diag = Diag::warning()
        .message("this is a sentence")
        .unlabeled_span(span(124, 133));

    assert_format(
        &diag,
        Some(&map),
        "\
warning: this is a sentence
    ┌─ ⟨anonymous⟩:121:2
    │
121 │ 这是一个句子
    │   ══════",
    );
}

#[test]
fn format_primary_secondary_highlights() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "2ndry\nPRIM\n2ndry\n");

    let diag = Diag::error()
        .code(ErrorCode::E001)
        .message("important")
        .unlabeled_span(span(7, 11))
        .unlabeled_secondary_span(span(1, 4))
        .unlabeled_secondary_span(span(15, 17));

    assert_format(
        &diag,
        Some(&map),
        "\
error[E001]: important
  ┌─ ⟨anonymous⟩:1:1
  │
1 │ 2ndry
  │ ───
  │
  ├─ ⟨anonymous⟩:2:1
  │
2 │ PRIM
  │ ════
  │
  ├─ ⟨anonymous⟩:3:4
  │
3 │ 2ndry
  │    ──",
    );
}

#[test]
fn format_primary_secondary_highlight_differing_line_number_widths() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "\nprimary\n\n\n\n\n\n\n\n\n\nsecondary\n");

    let diag = Diag::bug()
        .message("placeholder")
        .unlabeled_span(span(3, 7))
        .unlabeled_secondary_span(span(19, 28));

    assert_format(
        &diag,
        Some(&map),
        "\
internal compiler error: placeholder
   ┌─ ⟨anonymous⟩:2:2
   │
 2 │ primary
   │  ════
   │
   ├─ ⟨anonymous⟩:12:1
   │
12 │ secondary
   │ ─────────",
    );
}

#[test]
fn format_highlights_in_different_files() {
    let mut map = SourceMap::default();
    map.add_str("ONE", "a\nbc\ndef\n");
    map.add_str("TWO", "zyx");

    let diag = Diag::debug()
        .unlabeled_span(span(4, 5))
        .unlabeled_secondary_span(span(11, 13));

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message
  ┌─ ONE:2:2
  │
2 │ bc
  │  ═
  │
  ├─ TWO:1:1
  │
1 │ zyx
  │ ──",
    );
}

#[test]
fn format_highlights_same_line() {
    let mut map = SourceMap::default();
    map.add_str("identity", "sequence\n");

    let diag = Diag::error()
        .message("tag")
        .unlabeled_span(span(1, 6))
        .unlabeled_secondary_span(span(5, 9));

    assert_format(
        &diag,
        Some(&map),
        "\
error: tag
  ┌─ identity:1:1
  │
1 │ sequence
  │ ═════
  │
  ├─ identity:1:5
  │
1 │ sequence
  │     ────",
    );
}

#[test]
fn format_labeled_highlights() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta");

    let diag = Diag::error()
        .message("labels")
        .span(span(2, 4), "pointer")
        .label(span(7, 11), "content")
        .span(span(12, 23), "explanation")
        .label(span(28, 33), "message");

    assert_format(
        &diag,
        Some(&map),
        "\
error: labels
  ┌─ ⟨anonymous⟩:1:2
  │
1 │ alpha
  │  ══ pointer
  │
  ├─ ⟨anonymous⟩:2:1
  │
2 │ beta
  │ ──── content
  │
  ├─ ⟨anonymous⟩:3:1
  │
3 │   gamma
  │ ╔═╝
4 │ ║ delta
  │ ╚═════╝ explanation
  │
  ├─ ⟨anonymous⟩:5:5
  │
5 │   epsilon
  │ ┌─────┘
6 │ │ zeta
  │ └─┘ message",
    );
}

#[test]
fn format_multi_line_labeled_highlights() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta\n");

    let diag = Diag::error()
        .message("multi-line labels")
        .span(span(2, 4), "pointer\ncontext\nfiller")
        .label(span(7, 11), "content\n  indented")
        .span(span(12, 23), "explanation\naddendum")
        .label(span(28, 33), "message\n\nbottom");

    assert_format(
        &diag,
        Some(&map),
        "\
error: multi-line labels
  ┌─ ⟨anonymous⟩:1:2
  │
1 │ alpha
  │  ══ pointer
  │     context
  │     filler
  │
  ├─ ⟨anonymous⟩:2:1
  │
2 │ beta
  │ ──── content
  │        indented
  │
  ├─ ⟨anonymous⟩:3:1
  │
3 │   gamma
  │ ╔═╝
4 │ ║ delta
  │ ╚═════╝ explanation
  │         addendum
  │
  ├─ ⟨anonymous⟩:5:5
  │
5 │   epsilon
  │ ┌─────┘
6 │ │ zeta
  │ └─┘ message
  │
  │     bottom",
    );
}

#[test]
fn format_multi_line_labeled_highlights_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\ngamma\ndelta\nepsilon\nzeta");

    let diag = Diag::error()
        .message("multi-line labels")
        .span(span(2, 4), "pointer\ncontext\nfiller")
        .label(span(7, 11), "content\n  indented")
        .span(span(12, 23), "explanation\naddendum")
        .label(span(28, 33), "message\n\nbottom");

    assert_format(
        &diag,
        Some(&map),
        "\
error: multi-line labels
  ┌─ ⟨anonymous⟩:1:2
  │
1 │ alpha
  │  ══ pointer
  │     context
  │     filler
  │
  ├─ ⟨anonymous⟩:2:1
  │
2 │ beta
  │ ──── content
  │        indented
  │
  ├─ ⟨anonymous⟩:3:1
  │
3 │   gamma
  │ ╔═╝
4 │ ║ delta
  │ ╚═════╝ explanation
  │         addendum
  │
  ├─ ⟨anonymous⟩:5:5
  │
5 │   epsilon
  │ ┌─────┘
6 │ │ zeta
  │ └─┘ message
  │
  │     bottom",
    );
}

#[test]
fn format_subdiagnostics_no_highlights() {
    let diag = Diag::error()
        .code(ErrorCode::E004)
        .message("summary")
        .note("clarification")
        .note("other clarification")
        .help("hint");

    assert_format(
        &diag,
        None,
        "\
error[E004]: summary
 note: clarification
 note: other clarification
 help: hint",
    );
}

#[test]
fn format_subdiagnostics() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "****  ****");

    let diag = Diag::warning()
        .message("it")
        .unlabeled_span(span(5, 7))
        .help("helpful")
        .help("less helpful");

    assert_format(
        &diag,
        Some(&map),
        "\
warning: it
  ┌─ ⟨anonymous⟩:1:5
  │
1 │ ****  ****
  │     ══
  │
 help: helpful
 help: less helpful",
    );
}

#[test]
fn format_subdiagnostics_two_digit_line_numbers() {
    let mut map = SourceMap::default();
    map.add(Anon, Arc::new("****  ****\n".repeat(10)), None);

    let diag = Diag::warning()
        .message("it")
        .unlabeled_span(span(104, 106))
        .help("helpful")
        .help("less helpful");

    assert_format(
        &diag,
        Some(&map),
        "\
warning: it
   ┌─ ⟨anonymous⟩:10:5
   │
10 │ ****  ****
   │     ══
   │
  help: helpful
  help: less helpful",
    );
}

#[test]
fn format_multi_line_subdiagnostics() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "****  ****");

    let diag = Diag::warning()
        .message("it")
        .unlabeled_span(span(5, 7))
        .help("helpful\ntip\nhopefully")
        .help("less helpful\ntip\n");

    assert_format(
        &diag,
        Some(&map),
        "\
warning: it
  ┌─ ⟨anonymous⟩:1:5
  │
1 │ ****  ****
  │     ══
  │
 help: helpful
       tip
       hopefully
 help: less helpful
       tip",
    );
}

#[test]
fn format_multiple_primary_highlights() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "gamma\n");

    let diag = Diag::error().unlabeled_spans([span(1, 2), span(3, 4), span(5, 6)]);

    assert_format(
        &diag,
        Some(&map),
        "\
error
  ┌─ ⟨anonymous⟩:1:1
  │
1 │ gamma
  │ ═
  │
  ├─ ⟨anonymous⟩:1:3
  │
1 │ gamma
  │   ═
  │
  ├─ ⟨anonymous⟩:1:5
  │
1 │ gamma
  │     ═",
    );
}

#[test]
fn format_zero_length_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "sample\n");

    let diag = Diag::debug().message("nil").unlabeled_span(span(3, 3));

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message: nil
  ┌─ ⟨anonymous⟩:1:3
  │
1 │ sample
  │  ⟫⟪",
    );
}

#[test]
fn format_zero_length_highlight_start_of_line() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "sample\n");

    let diag = Diag::debug().message("nil").unlabeled_span(span(1, 1));

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message: nil
  ┌─ ⟨anonymous⟩:1:1
  │
1 │  sample
  │ ⟫⟪",
    );
}

#[test]
fn format_zero_length_secondary_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "sample\n");

    let diag = Diag::debug()
        .message("nil")
        .unlabeled_secondary_span(span(3, 3));

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message: nil
  ┌─ ⟨anonymous⟩:1:3
  │
1 │ sample
  │  ⟩⟨",
    );
}

#[test]
fn format_highlight_line_break() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "This is a sentence.\nThis is a follow-up sentence.\n");

    let diag = Diag::error().span(span(20, 20), "EOL");

    assert_format(
        &diag,
        Some(&map),
        "\
error
  ┌─ ⟨anonymous⟩:1:20
  │
1 │ This is a sentence.
  │                   ⟫⟪ EOL",
    );
}

#[test]
fn format_highlight_end_of_input() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "This is a sentence.");

    let diag = Diag::error().span(span(20, 20), "EOI");

    assert_format(
        &diag,
        Some(&map),
        "\
error
  ┌─ ⟨anonymous⟩:1:20
  │
1 │ This is a sentence.
  │                   ⟫⟪ EOI",
    );
}

/// The trailing line break should not move the highlighted end of input
/// to a new line. It should look as if there was no trailing line break.
#[test]
fn format_highlight_end_of_input_with_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "This is a sentence.\n");

    let diag = Diag::error().span(span(21, 21), "EOI");

    assert_format(
        &diag,
        Some(&map),
        "\
error
  ┌─ ⟨anonymous⟩:1:21
  │
1 │ This is a sentence.
  │                   ⟫⟪ EOI",
    );
}

/// A highlight dontaining a trailing line break should not make it
/// a multi-line highlight. It should look as if there was no
/// trailing line break. Optimally, a highlight should not contain
/// a trailing line break.
#[test]
fn format_highlight_containing_final_line_break() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "This is a sentence.\n");

    let diag = Diag::warning()
        .message("weird corner case")
        .unlabeled_span(span(1, 21));

    assert_format(
        &diag,
        Some(&map),
        "\
warning: weird corner case
  ┌─ ⟨anonymous⟩:1:1
  │
1 │ This is a sentence.
  │ ═══════════════════",
    );
}

/// A highlight that includes the end of input virtual location
/// should look as if the end of input was not included.
/// Preferably, highlights should not contain the trailing end of input.
/// We might want to change this to panic.
#[test]
fn format_highlight_containing_final_end_of_input() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "EVERYTHING\n");

    let diag = Diag::bug().unlabeled_span(span(1, 13));

    assert_format(
        &diag,
        Some(&map),
        "\
internal compiler error
  ┌─ ⟨anonymous⟩:1:1
  │
1 │ EVERYTHING
  │ ══════════",
    );
}

#[test]
fn format_highlight_in_empty_file() {
    let mut map = SourceMap::default();
    map.add_str("empty.txt", "");

    let diag = Diag::error()
        .message("this file has to contain something reasonable")
        .unlabeled_span(span(1, 1));

    assert_format(
        &diag,
        Some(&map),
        "\
error: this file has to contain something reasonable
  ┌─ empty.txt:1:1
  │
1 │  
  │ ⟫⟪",
    );
}

#[test]
fn format_warning_with_lint_code() {
    let diag = Diag::warning()
        .code(LintCode::PermanentlyUnassigned)
        .message("no man's land");

    assert_format(
        &diag,
        None,
        "\
warning[permanently-unassigned]: no man's land",
    );
}

#[test]
fn format_path_no_highlights() {
    let diag = Diag::error()
        .message("there is something wrong with this file")
        .path("path/to/file.ext".into());

    assert_format(
        &diag,
        None,
        "\
error: there is something wrong with this file
  ── path/to/file.ext",
    );
}

#[test]
fn format_path_together_with_highlight() {
    let mut map = SourceMap::default();
    map.add_str("root.cfg", "allow_plain_text = false\n");

    let diag = Diag::error()
        .message("this file is not acceptable")
        .path("problematic.txt".into())
        .span(span(20, 25), "because you set this");

    assert_format(
        &diag,
        Some(&map),
        "\
error: this file is not acceptable
  ┌─ problematic.txt
  │
  ├─ root.cfg:1:20
  │
1 │ allow_plain_text = false
  │                    ═════ because you set this",
    );
}

#[test]
fn format_path_together_with_subdiagnostic() {
    let diag = Diag::warning()
        .message("this file looks spooky")
        .path("scary.exe".into())
        .help("better delete it");

    assert_format(
        &diag,
        None,
        "\
warning: this file looks spooky
  ┌─ scary.exe
  │
 help: better delete it",
    );
}

#[test]
fn format_suggestion() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "Would you like some more tea?");

    let diag = Diag::debug()
        .message("this phrasing is too euphemistic")
        .unlabeled_span(span(16, 25))
        .suggest(span(16, 25), "be more business-minded", "to buy a pot of");

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message: this phrasing is too euphemistic
  ┌─ ⟨anonymous⟩:1:16
  │
1 │ Would you like some more tea?
  │                ═════════
  │
 help: be more business-minded
  ┌─ ⟨anonymous⟩:1:16
  │
1 │ Would you like to buy a pot of tea?
  │                ~~~~~~~~~~~~~~~",
    );
}

// @Beacon @Task don't "omit" the file location (file:line:col) if the file differs from the one of the
// primary highlight (and if there is no (primary/secondary) highlight at all, always show)

/// If there is no preceeding primary or secondary highlight, show the file information (path, line, column)
/// accociated with the suggestion to give the needed context.
#[test]
fn format_suggestion_no_preceeding_highlight() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "Would you like some more tea?");

    let diag = Diag::debug().suggest(span(16, 25), "be more business-minded", "to buy a pot of");

    assert_format(
        &diag,
        Some(&map),
        "\
internal debugging message
 help: be more business-minded
  ┌─ ⟨anonymous⟩:1:16
  │
1 │ Would you like to buy a pot of tea?
  │                ~~~~~~~~~~~~~~~",
    );
}

#[test]
fn format_suggestion_removal() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "This is the the best!");

    let diag = Diag::error()
        .message("duplicate consecutive word ‘the’")
        .suggest(span(13, 17), "remove the second occurrence of the word", "");

    assert_format(
        &diag,
        Some(&map),
        "\
error: duplicate consecutive word ‘the’
 help: remove the second occurrence of the word
  ┌─ ⟨anonymous⟩:1:13
  │
1 │ This is the best!
  │            ⟩⟨",
    );
}

// @Task Fix this!
#[test]
#[ignore = "weird corner case"]
fn format_two_line_break_highlight_containing_first() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\nbeta\n\ngamma");

    let diag = Diag::bug().unlabeled_span(span(1, 12));

    // @Note I don't actually know how it should be rendered
    assert_format(
        &diag,
        Some(&map),
        "\
internal compiler error
  ┌─ ⟨anonymous⟩:1:1
  │
1 │   alpha
  │ ╔═╝
2 │ ║ beta
  │ ╚════╝",
    );
}

// @Task Fix this!
#[test]
#[ignore = "weird corner case"]
fn format_two_line_breaks_highlight_containing_second() {
    let mut map = SourceMap::default();
    map.add_str(Anon, "alpha\n\n");

    let diag = Diag::bug().unlabeled_span(span(1, 8));

    // @Note I don't actually know how it should be rendered
    assert_format(
        &diag,
        Some(&map),
        "\
internal compiler error
  ┌─ ⟨anonymous⟩:1:1
  │
1 │ alpha
  │ ═════",
    );
}
