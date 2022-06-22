use super::{Highlight, LineWithHighlight, LinesWithHighlight, SourceMap};
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
        map.lines_with_highlight(span(4, 7)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "abcdefghijklmnopq",
                highlight: Highlight {
                    start: 4,
                    end: 7,
                    width: 3,
                    prefix_width: 3
                },
            },
            last: None,
        }
    );
}

#[test]
fn lines_single_line_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghijklmnopq".into());

    assert_eq!(
        map.lines_with_highlight(span(4, 7)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "abcdefghijklmnopq",
                highlight: Highlight {
                    start: 4,
                    end: 7,
                    width: 3,
                    prefix_width: 3,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_single_character_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "空#\n".into());

    assert_eq!(
        map.lines_with_highlight(span(4, 5)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "空#",
                highlight: Highlight {
                    start: 2,
                    end: 3,
                    width: 1,
                    prefix_width: 2,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_single_wide_character_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "#空\n".into());

    assert_eq!(
        map.lines_with_highlight(span(2, 5)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "#空",
                highlight: Highlight {
                    start: 2,
                    end: 3,
                    width: 2,
                    prefix_width: 1,
                }
            },
            last: None,
        }
    );
}
#[test]
fn lines_single_wide_character_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "#空".into());

    assert_eq!(
        map.lines_with_highlight(span(2, 5)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "#空",
                highlight: Highlight {
                    start: 2,
                    end: 3,
                    width: 2,
                    prefix_width: 1,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_single_line_highlight_multi_line_source() {
    let mut map = SourceMap::default();
    map.add(None, "buffer\n空\n空it__\nbuffer\n".into());

    assert_eq!(
        map.lines_with_highlight(span(15, 17)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 3,
                content: "空it__",
                highlight: Highlight {
                    start: 2,
                    end: 4,
                    width: 2,
                    prefix_width: 2,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_multi_line_highlight() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n第三\ndelta\nepsilon\n".into());

    assert_eq!(
        map.lines_with_highlight(span(8, 21)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 2,
                content: "beta",
                highlight: Highlight {
                    start: 2,
                    end: 5,
                    width: 3,
                    prefix_width: 1,
                }
            },
            last: Some(LineWithHighlight {
                number: 4,
                content: "delta",
                highlight: Highlight {
                    start: 1,
                    end: 3,
                    width: 2,
                    prefix_width: 0,
                }
            }),
        }
    );
}

#[test]
fn lines_multi_line_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n第三\ndelta\nepsilon".into());

    assert_eq!(
        map.lines_with_highlight(span(10, 15)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 2,
                content: "beta",
                highlight: Highlight {
                    start: 4,
                    end: 5,
                    width: 1,
                    prefix_width: 3,
                },
            },
            last: Some(LineWithHighlight {
                number: 3,
                content: "第三",
                highlight: Highlight {
                    start: 1,
                    end: 2,
                    width: 2,
                    prefix_width: 0,
                }
            }),
        }
    );
}

#[test]
fn lines_highlight_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "buffer\n".into());

    assert_eq!(
        map.lines_with_highlight(span(7, 8)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "buffer",
                highlight: Highlight {
                    start: 7,
                    end: 7,
                    width: 0,
                    prefix_width: 6,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_zero_length_highlight() {
    let mut map = SourceMap::default();
    map.add(None, ".:.:.:\n".into());

    assert_eq!(
        map.lines_with_highlight(span(2, 2)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: ".:.:.:",
                highlight: Highlight {
                    start: 2,
                    end: 2,
                    width: 0,
                    prefix_width: 1,
                }
            },
            last: None,
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
        map.lines_with_highlight(span(9, 9)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "content",
                highlight: Highlight {
                    start: 9,
                    end: 9,
                    width: 0,
                    prefix_width: 7,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_end_of_input_highlight_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "content".into());

    assert_eq!(
        map.lines_with_highlight(span(8, 8)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "content",
                highlight: Highlight {
                    start: 8,
                    end: 8,
                    width: 0,
                    prefix_width: 7,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_end_of_input_highlight_empty_file() {
    let mut map = SourceMap::default();
    map.add(None, String::new());

    assert_eq!(
        map.lines_with_highlight(span(1, 1)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "",
                highlight: Highlight {
                    start: 1,
                    end: 1,
                    width: 0,
                    prefix_width: 0,
                }
            },
            last: None,
        }
    );
}

#[test]
fn lines_highlight_containing_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "alpha\nbeta\n".into());

    assert_eq!(
        map.lines_with_highlight(span(1, 12)),
        LinesWithHighlight {
            path: None,
            first: LineWithHighlight {
                number: 1,
                content: "alpha",
                highlight: Highlight {
                    start: 1,
                    end: 6,
                    width: 5,
                    prefix_width: 0,
                }
            },
            last: Some(LineWithHighlight {
                number: 2,
                content: "beta",
                highlight: Highlight {
                    start: 1,
                    end: 6,
                    width: 4,
                    prefix_width: 0,
                }
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

    map.lines_with_highlight(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_single_line_source_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abcdefghi".into());

    map.lines_with_highlight(span(6, 20));
}

#[test]
#[should_panic]
#[ignore = "panicking on invalid spans is currently not guaranteed"]
fn lines_span_out_of_bounds_multi_line_source() {
    let mut map = SourceMap::default();
    map.add(None, "abc\ndefghi\n".into());

    map.lines_with_highlight(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_multi_line_source_no_trailing_line_break() {
    let mut map = SourceMap::default();
    map.add(None, "abc\ndefghi".into());

    map.lines_with_highlight(span(6, 20));
}

#[test]
#[should_panic]
fn lines_span_out_of_bounds_empty_source() {
    let mut map = SourceMap::default();
    map.add(None, String::new());

    map.lines_with_highlight(span(1, 2));
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
