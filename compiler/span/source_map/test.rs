use super::{Line, Lines, SourceMap};
use crate::span::{span, ByteIndex};

/// Letting the first proper offset be `1` frees up `0` to mean _unknown location_ in [`Span::default`].
#[test]
fn first_next_offset_is_one() {
    let map = SourceMap::default();

    assert_eq!(map.next_offset(), ByteIndex::new(1));
}

#[test]
fn spacing_between_files() {
    let mut map = SourceMap::default();

    let file0 = map.add(None, "abc".into());
    let file1 = map.add(None, "defgh".into());

    assert_eq!(map[file0].span, span(1, 4));
    assert_eq!(&map[file0][map[file0].span.local(&map[file0])], "abc");
    assert_eq!(map[file1].span, span(5, 10));
    assert_eq!(&map[file1][map[file1].span.local(&map[file1])], "defgh");
}

#[test]
fn lines_single_line_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghijklmnopq\n".into());

    assert_eq!(
        map.lines(span(4, 7)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "abcdefghijklmnopq",
                highlight_width: 3,
                highlight_padding_width: 3,
                highlight_start_column: 4,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_single_line_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghijklmnopq".into());

    assert_eq!(
        map.lines(span(4, 7)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "abcdefghijklmnopq",
                highlight_width: 3,
                highlight_padding_width: 3,
                highlight_start_column: 4,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_single_character_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "空#\n".into());

    assert_eq!(
        map.lines(span(4, 5)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "空#",
                highlight_width: 1,
                highlight_padding_width: 2,
                // @Bug use #chars, not #bytes (@Task change to 2)
                highlight_start_column: 4,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_single_wide_character_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "#空\n".into());

    assert_eq!(
        map.lines(span(2, 5)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "#空",
                highlight_width: 2,
                highlight_padding_width: 1,
                highlight_start_column: 2,
            },
            last_line: None,
        }
    );
}
#[test]
fn lines_single_wide_character_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "#空".into());

    assert_eq!(
        map.lines(span(2, 5)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "#空",
                highlight_width: 2,
                highlight_padding_width: 1,
                highlight_start_column: 2,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_single_line_highlight_multi_line_source() {
    let mut map = SourceMap::default();
    map.add(None, "buffer\n空\n空it__\nbuffer\n".into());

    assert_eq!(
        map.lines(span(15, 17)),
        Lines {
            path: None,
            first_line: Line {
                number: 3,
                content: "空it__",
                highlight_width: 2,
                highlight_padding_width: 2,
                // @Bug use #chars, not #bytes (@Task change to 2)
                highlight_start_column: 4,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_multi_line_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n第三\ndelta\nepsilon\n".into());

    assert_eq!(
        map.lines(span(8, 21)),
        Lines {
            path: None,
            first_line: Line {
                number: 2,
                content: "beta",
                highlight_width: 3,
                highlight_padding_width: 1,
                highlight_start_column: 2,
            },
            last_line: Some(Line {
                number: 4,
                content: "delta",
                highlight_width: 2,
                highlight_padding_width: 0,
                highlight_start_column: 1,
            }),
        }
    );
}

#[test]
fn lines_multi_line_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n第三\ndelta\nepsilon".into());

    assert_eq!(
        map.lines(span(10, 15)),
        Lines {
            path: None,
            first_line: Line {
                number: 2,
                content: "beta",
                highlight_width: 1,
                highlight_padding_width: 3,
                highlight_start_column: 4,
            },
            last_line: Some(Line {
                number: 3,
                content: "第三",
                highlight_width: 2,
                highlight_padding_width: 0,
                highlight_start_column: 1,
            }),
        }
    );
}

#[test] // @Beacon @Beacon @Beacon
fn lines_highlight_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "buffer\n".into());

    assert_eq!(
        map.lines(span(7, 8)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "buffer",
                highlight_width: 0,
                highlight_padding_width: 6,
                highlight_start_column: 7,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_zero_length_highlight() {
    let mut map = SourceMap::default();
    map.add(None, ".:.:.:\n".into());

    assert_eq!(
        map.lines(span(2, 2)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: ".:.:.:",
                highlight_width: 0,
                highlight_padding_width: 1,
                highlight_start_column: 2,
            },
            last_line: None,
        }
    );
}

/// The trailing line break should not move the highlighted end of input
/// to a new line. It should look as if there was no trailing line break.
#[test]
fn lines_end_of_input_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "content\n".into());

    assert_eq!(
        map.lines(span(9, 9)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "content",
                highlight_width: 0,
                highlight_padding_width: 7,
                highlight_start_column: 9,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_end_of_input_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "content".into());

    assert_eq!(
        map.lines(span(8, 8)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "content",
                highlight_width: 0,
                highlight_padding_width: 7,
                highlight_start_column: 8,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_end_of_input_highlight_empty_file() {
    let mut map = SourceMap::default();
    map.add(None, String::new());

    assert_eq!(
        map.lines(span(1, 1)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "",
                highlight_width: 0,
                highlight_padding_width: 0,
                highlight_start_column: 1,
            },
            last_line: None,
        }
    );
}

#[test]
fn lines_highlight_containing_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n".into());

    assert_eq!(
        map.lines(span(1, 12)),
        Lines {
            path: None,
            first_line: Line {
                number: 1,
                content: "alpha",
                highlight_width: 5,
                highlight_padding_width: 0,
                highlight_start_column: 1,
            },
            last_line: Some(Line {
                number: 2,
                content: "beta",
                highlight_width: 4,
                highlight_padding_width: 0,
                highlight_start_column: 1,
            }),
        }
    );
}

#[test]
#[should_panic]
#[ignore = "panicking on invalid spans is currently not guaranteed"]
fn lines_span_out_of_bounds_single_line_source() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghi\n".into());

    map.lines(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_single_line_source_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghi".into());

    map.lines(span(6, 20));
}

#[test]
#[should_panic]
#[ignore = "panicking on invalid spans is currently not guaranteed"]
fn lines_span_out_of_bounds_multi_line_source() {
    let mut map = SourceMap::default();
    map.add(None, "abc\ndefghi\n".into());

    map.lines(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_multi_line_source_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abc\ndefghi".into());

    map.lines(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_empty_source() {
    let mut map = SourceMap::default();
    map.add(None, String::new());

    map.lines(span(1, 2));
}

// @Task implement this
#[test]
#[should_panic]
#[ignore = "unimplemented"]
fn lines_span_does_not_start_at_char_boundary() {
    todo!()
}

// @Task implement this
#[test]
#[should_panic]
#[ignore = "unimplemented"]
fn lines_span_does_not_end_at_char_boundary() {
    todo!()
}
