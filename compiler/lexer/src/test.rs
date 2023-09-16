// @Task add tests for invalid indentation (esp. verifying the recovery logic)
// @Task smh create a Stack<Section> of the form [TopLevel, Continued, Indented]
// @Task smh create a Vec<Section> of the form [TopLevel, Indented, Continued]
// @Task add a lot of tests of the interaction between line breaks, indentation and *comments*!

use crate::{
    token::{IndentationError, Spaces},
    BareError,
    BareToken::*,
    Error, Outcome, Token,
};
use span::span;
use utility::{
    paint::{epaint, ColorChoice},
    Changeset, ChangesetExt,
};

fn lex(source: &'static str) -> Outcome {
    super::lex_string(source.to_owned())
}

macro assert_lex_eq {
    ($source:literal, $tokens:expr $(,)?) => {
        assert_lex_eq!($source, $tokens, Vec::new())
    },
    ($source:literal, $tokens:expr, $errors:expr $(,)?) => {
        assert_eq(lex($source), Outcome { tokens: $tokens, errors: $errors })
    }
}

#[track_caller]
#[allow(clippy::needless_pass_by_value)] // more legible call sites, flexibility doesn't matter here
fn assert_eq(actual: Outcome, expected: Outcome) {
    if actual != expected {
        // We also lock stdout since the test runner would otherwise interfere.
        let stdout = std::io::stdout().lock();
        epaint(
            |painter| {
                Changeset::new(&format!("{expected:#?}"), &format!("{actual:#?}"), "\n")
                    .render_with_ledge(painter)
            },
            ColorChoice::Auto,
        )
        .unwrap();
        drop(stdout);

        panic!("the output by the lexer does not match the expected one");
    }
}

#[test]
fn comments() {
    assert_lex_eq!(
        "
;;; bland commentary ensues
;;; a filler line
;;; and an end
",
        vec![
            Token::new(span(1, 2), LineBreak),
            Token::new(span(2, 29), Comment),
            Token::new(span(30, 47), Comment),
            Token::new(span(48, 62), Comment),
            Token::new(span(63, 63), EndOfInput),
        ],
    );
}

#[test]
fn documentation_comments() {
    assert_lex_eq!(
        "\
alpha;;;文本
0401 ;; stray documentation comment
;; next one
;;有意思的信",
        vec![
            Token::new(span(1, 6), Word("alpha".into())),
            Token::new(span(6, 15), Comment),
            Token::new(span(16, 20), NumberLiteral("0401".into())),
            Token::new(span(21, 51), DocumentationComment),
            Token::new(span(52, 63), DocumentationComment),
            Token::new(span(64, 81), DocumentationComment),
            Token::new(span(81, 81), EndOfInput),
        ],
    );
}

#[test]
fn empty_documentation_comment_followed_by_non_empty_one() {
    assert_lex_eq!(
        "\
;;
;; non-empty
",
        vec![
            Token::new(span(1, 3), DocumentationComment),
            Token::new(span(4, 16), DocumentationComment),
            Token::new(span(17, 17), EndOfInput),
        ],
    );
}

#[test]
fn shebang() {
    assert_lex_eq!(
        "\
#!/usr/bin/lushui run
it",
        vec![
            Token::new(span(1, 23), Shebang),
            Token::new(span(23, 25), Word("it".into())),
            Token::new(span(25, 25), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shebang_but_a_hashtag() {
    assert_lex_eq!(
        "\
#hashtag",
        vec![
            Token::new(span(1, 2), Symbol("#".into())),
            Token::new(span(2, 9), Word("hashtag".into())),
            Token::new(span(9, 9), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shabang_but_a_hash() {
    assert_lex_eq!(
        "\
#",
        vec![
            Token::new(span(1, 2), Symbol("#".into())),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shebang_but_a_symbol() {
    assert_lex_eq!(
        "\
#?/WEIRD",
        vec![
            Token::new(span(1, 4), Symbol("#?/".into())),
            Token::new(span(4, 9), Word("WEIRD".into())),
            Token::new(span(9, 9), EndOfInput),
        ],
    );
}

#[test]
fn shebang_lookalike_not_first_line() {
    assert_lex_eq!(
        "
#!/usr/bin/lushui run
",
        vec![
            Token::new(span(1, 2), LineBreak),
            Token::new(span(2, 5), Symbol("#!/".into())),
            Token::new(span(5, 8), Word("usr".into())),
            Token::new(span(8, 9), Symbol("/".into())),
            Token::new(span(9, 12), Word("bin".into())),
            Token::new(span(12, 13), Symbol("/".into())),
            Token::new(span(13, 19), Word("lushui".into())),
            Token::new(span(20, 23), Word("run".into())),
            Token::new(span(23, 24), LineBreak),
            Token::new(span(24, 24), EndOfInput),
        ],
    );
}

#[test]
fn identifiers() {
    assert_lex_eq!(
        "alpha alpha0 _alpha al6ha_beta_",
        vec![
            Token::new(span(1, 6), Word("alpha".into())),
            Token::new(span(7, 13), Word("alpha0".into())),
            Token::new(span(14, 20), Word("_alpha".into())),
            Token::new(span(21, 32), Word("al6ha_beta_".into())),
            Token::new(span(32, 32), EndOfInput),
        ],
    );
}

#[test]
fn dashed_identifiers() {
    assert_lex_eq!(
        "ALPH4-G4MM4 alpha-gamma _-_ alpha-0",
        vec![
            Token::new(span(1, 12), Word("ALPH4-G4MM4".into())),
            Token::new(span(13, 24), Word("alpha-gamma".into())),
            Token::new(span(25, 28), Word("_-_".into())),
            Token::new(span(29, 36), Word("alpha-0".into())),
            Token::new(span(36, 36), EndOfInput),
        ],
    );
}

#[test]
fn weird_dashed_identifiers() {
    assert_lex_eq!(
        "alpha- alpha-: alpha--beta--- -no --no-no",
        vec![
            Token::new(span(1, 7), Word("alpha-".into())),
            Token::new(span(8, 14), Word("alpha-".into())),
            Token::new(span(14, 15), Colon),
            Token::new(span(16, 30), Word("alpha--beta---".into())),
            Token::new(span(31, 32), Symbol("-".into())),
            Token::new(span(32, 34), Word("no".into())),
            Token::new(span(35, 37), Symbol("--".into())),
            Token::new(span(37, 42), Word("no-no".into())),
            Token::new(span(42, 42), EndOfInput),
        ],
    );
}

#[test]
fn keywords_and_lookalikes() {
    assert_lex_eq!(
        "    self self_ self-self in _",
        vec![
            Token::new(span(5, 9), Self_),
            Token::new(span(10, 15), Word("self_".into())),
            Token::new(span(16, 25), Word("self-self".into())),
            Token::new(span(26, 28), In),
            Token::new(span(29, 30), Underscore),
            Token::new(span(30, 30), EndOfInput),
        ],
    );
}

#[test]
fn symbols() {
    assert_lex_eq!(
        "+ +>alpha//$~%  #0 . ..",
        vec![
            Token::new(span(1, 2), Symbol("+".into())),
            Token::new(span(3, 5), Symbol("+>".into())),
            Token::new(span(5, 10), Word("alpha".into())),
            Token::new(span(10, 15), Symbol("//$~%".into())),
            Token::new(span(17, 18), Symbol("#".into())),
            Token::new(span(18, 19), NumberLiteral("0".into())),
            Token::new(span(20, 21), Dot),
            Token::new(span(22, 24), Symbol("..".into())),
            Token::new(span(24, 24), EndOfInput),
        ],
    );
}

#[test]
fn identifier_with_trailing_dot() {
    assert_lex_eq!(
        "namespace.",
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 11), EndOfInput),
        ],
    );
}

#[test]
fn identifier_dot_symbol() {
    assert_lex_eq!(
        "namespace.+>!",
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 14), Symbol("+>!".into())),
            Token::new(span(14, 14), EndOfInput),
        ],
    );
}

#[test]
fn lex_identifier_dot_dotted_symbol() {
    assert_lex_eq!(
        "namespace.$.?!.",
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 16), Symbol("$.?!.".into())),
            Token::new(span(16, 16), EndOfInput),
        ],
    );
}

#[test]
fn lex_identifier_and_dotted_symbol_after_space() {
    assert_lex_eq!(
        "namespace .$.?!.",
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(11, 17), Symbol(".$.?!.".into())),
            Token::new(span(17, 17), EndOfInput),
        ],
    );
}

#[test]
fn lex_keyword_dot_symbol() {
    assert_lex_eq!(
        "data.#",
        vec![
            Token::new(span(1, 5), Data),
            Token::new(span(5, 6), Dot),
            Token::new(span(6, 7), Symbol("#".into())),
            Token::new(span(7, 7), EndOfInput),
        ],
    );
}

#[test]
fn lex_number_literals() {
    assert_lex_eq!(
        "1001409409220293022239833211 01",
        vec![
            Token::new(
                span(1, 29),
                NumberLiteral("1001409409220293022239833211".into()),
            ),
            Token::new(span(30, 32), NumberLiteral("01".into())),
            Token::new(span(32, 32), EndOfInput),
        ],
    );
}

#[test]
fn lex_number_literals_with_separators() {
    assert_lex_eq!(
        r#"334 1'000what 3'2'2'1"" 500 10"" -23 3''100 10' 1'"#,
        vec![
            Token::new(span(1, 4), NumberLiteral("334".into())),
            Token::new(span(5, 10), NumberLiteral("1000".into())),
            Token::new(span(10, 14), Word("what".into())),
            Token::new(span(15, 22), NumberLiteral("3221".into())),
            Token::new(span(22, 24), TextLiteral("".into())),
            Token::new(span(25, 28), NumberLiteral("500".into())),
            Token::new(span(29, 31), NumberLiteral("10".into())),
            Token::new(span(31, 33), TextLiteral("".into())),
            Token::new(span(34, 37), NumberLiteral("-23".into())),
            Token::new(span(38, 44), NumberLiteral("3100".into())),
            Token::new(span(45, 48), NumberLiteral("10".into())),
            Token::new(span(49, 51), NumberLiteral("1".into())),
            Token::new(span(51, 51), EndOfInput),
        ],
    );
}

#[test]
fn lex_text_literal() {
    assert_lex_eq!(
        r#""
    al
  pha""#,
        vec![
            Token::new(
                span(1, 16),
                TextLiteral(
                    "
    al
  pha"
                    .into()
                ),
            ),
            Token::new(span(16, 16), EndOfInput),
        ],
    );
}

#[test]
fn lex_unterminated_text_literal() {
    assert_lex_eq!(
        r#""text message"#,
        vec![
            Token::new(span(1, 14), TextLiteral("text message".into())),
            Token::new(span(14, 14), EndOfInput)
        ],
        vec![Error::new(span(1, 14), BareError::UnterminatedTextLiteral)],
    );
}

#[test]
fn lex_single_quote() {
    assert_lex_eq!(
        "'",
        vec![
            Token::new(span(1, 2), Apostrophe),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn lex_brackets() {
    assert_lex_eq!(
        "(( )( ))",
        vec![
            Token::new(span(1, 2), OpeningRoundBracket),
            Token::new(span(2, 3), OpeningRoundBracket),
            Token::new(span(4, 5), ClosingRoundBracket),
            Token::new(span(5, 6), OpeningRoundBracket),
            Token::new(span(7, 8), ClosingRoundBracket),
            Token::new(span(8, 9), ClosingRoundBracket),
            Token::new(span(9, 9), EndOfInput),
        ],
    );
}

#[test]
fn bare_non_ascii_is_invalid() {
    assert_lex_eq!(
        "函数",
        vec![Token::new(span(7, 7), EndOfInput),],
        vec![
            Error::new(span(1, 4), BareError::InvalidToken('\u{51FD}')),
            Error::new(span(4, 7), BareError::InvalidToken('\u{6570}')),
        ],
    );
}

#[test]
fn bare_non_ascii_are_invalid_but_non_fatal() {
    assert_lex_eq!(
        "    函数 function",
        vec![
            Token::new(span(12, 20), Word("function".into())),
            Token::new(span(20, 20), EndOfInput),
        ],
        vec![
            Error::new(span(5, 8), BareError::InvalidToken('函')),
            Error::new(span(8, 11), BareError::InvalidToken('数')),
        ]
    );
}

#[test]
fn backticks_are_invalid() {
    assert_lex_eq!(
        "`",
        vec![Token::new(span(2, 2), EndOfInput),],
        vec![Error::new(span(1, 2), BareError::InvalidToken('`')),]
    );
}

#[test]
fn backticks_are_invalid_right_after_number_literal() {
    assert_lex_eq!(
        "1`",
        vec![
            Token::new(span(1, 2), NumberLiteral("1".into())),
            Token::new(span(3, 3), EndOfInput),
        ],
        vec![Error::new(span(2, 3), BareError::InvalidToken('`')),]
    );
}

#[test]
fn tabs_are_invalid() {
    assert_lex_eq!(
        "\t\t",
        vec![Token::new(span(3, 3), EndOfInput),],
        vec![
            Error::new(span(1, 2), BareError::InvalidToken('\t')),
            Error::new(span(2, 3), BareError::InvalidToken('\t')),
        ]
    );
}

#[test]
fn line_breaks_are_terminators_at_the_toplevel() {
    assert_lex_eq!(
        "\
alpha #?
100 it

\"moot\"",
        vec![
            Token::new(span(1, 6), Word("alpha".into())),
            Token::new(span(7, 9), Symbol("#?".into())),
            Token::new(span(9, 10), LineBreak),
            Token::new(span(10, 13), NumberLiteral("100".into())),
            Token::new(span(14, 16), Word("it".into())),
            Token::new(span(16, 18), LineBreak),
            Token::new(span(18, 24), TextLiteral("moot".into())),
            Token::new(span(24, 24), EndOfInput),
        ],
    );
}

/// Indentation means line continuation unless it follows the keyword `of`
/// or `do` (in which case it creates a “proper”/reified section, namely an
/// indented section; not in this test).
#[test]
fn indentation_means_line_continuation() {
    assert_lex_eq!(
        "\
start middle
    end
\"anything
    really\"
    3291238
        module
            (
                )

$%&~~
    .!^  \\/",
        vec![
            Token::new(span(1, 6), Word("start".into())),
            Token::new(span(7, 13), Word("middle".into())),
            Token::new(span(18, 21), Word("end".into())),
            Token::new(span(21, 22), LineBreak),
            Token::new(span(22, 43), TextLiteral("anything\n    really".into())),
            Token::new(span(48, 55), NumberLiteral("3291238".into())),
            Token::new(span(64, 70), Module),
            Token::new(span(83, 84), OpeningRoundBracket),
            Token::new(span(101, 102), ClosingRoundBracket),
            Token::new(span(102, 104), LineBreak),
            Token::new(span(104, 109), Symbol("$%&~~".into())),
            Token::new(span(114, 117), Symbol(".!^".into())),
            Token::new(span(119, 121), Symbol(r"\/".into())),
            Token::new(span(121, 121), EndOfInput),
        ],
    );
}

#[test]
fn line_breaks_are_not_terminators_in_continued_sections() {
    assert_lex_eq!(
        "\
-0
    off
    side
    \"\"
@@@ @


    lvl1
        lvl2
        lvl2
    lvl1
    1
",
        vec![
            Token::new(span(1, 3), NumberLiteral("-0".into())),
            Token::new(span(8, 11), Word("off".into())),
            Token::new(span(16, 20), Word("side".into())),
            Token::new(span(25, 27), TextLiteral("".into())),
            Token::new(span(27, 28), LineBreak),
            Token::new(span(28, 31), Symbol("@@@".into())),
            Token::new(span(32, 33), At),
            Token::new(span(40, 44), Word("lvl1".into())),
            Token::new(span(53, 57), Word("lvl2".into())),
            Token::new(span(66, 70), Word("lvl2".into())),
            Token::new(span(75, 79), Word("lvl1".into())),
            Token::new(span(84, 85), NumberLiteral("1".into())),
            Token::new(span(85, 86), LineBreak),
            Token::new(span(86, 86), EndOfInput),
        ],
    );
}

#[test]
fn keyword_of_introduces_indented_sections() {
    // @Task test sth similar with no trailing line break at the end ("early" EOI)
    assert_lex_eq!(
        "\
of
    something
    more
of

    1980

>of
    module of
        CONTENT
    of
        >>!<<
",
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), Indentation),
            Token::new(span(8, 17), Word("something".into())),
            Token::new(span(17, 18), LineBreak),
            Token::new(span(22, 26), Word("more".into())),
            Token::new(span(27, 27), Dedentation),
            Token::new(span(27, 27), LineBreak),
            Token::new(span(27, 29), Of),
            Token::new(span(31, 35), Indentation),
            Token::new(span(35, 39), NumberLiteral("1980".into())),
            Token::new(span(41, 41), Dedentation),
            Token::new(span(41, 41), LineBreak),
            Token::new(span(41, 42), Symbol(">".into())),
            Token::new(span(42, 44), Of),
            Token::new(span(45, 49), Indentation),
            Token::new(span(49, 55), Module),
            Token::new(span(56, 58), Of),
            Token::new(span(59, 67), Indentation),
            Token::new(span(67, 74), Word("CONTENT".into())),
            Token::new(span(75, 79), Dedentation),
            // @Task this span should be empty, right?
            Token::new(span(75, 79), LineBreak),
            Token::new(span(79, 81), Of),
            Token::new(span(82, 90), Indentation),
            Token::new(span(90, 95), Symbol(">>!<<".into())),
            Token::new(span(96, 96), Dedentation),
            Token::new(span(96, 96), LineBreak),
            Token::new(span(96, 96), Dedentation),
            Token::new(span(96, 96), LineBreak),
            Token::new(span(96, 96), EndOfInput),
        ],
    );
}

#[test]
fn no_superfluous_line_break_before_dedentation_token_with_continued_section() {
    assert_lex_eq!(
        "\
of
    a
        b
",
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), Indentation),
            Token::new(span(8, 9), Word("a".into())),
            Token::new(span(18, 19), Word("b".into())),
            Token::new(span(20, 20), Dedentation),
            Token::new(span(20, 20), LineBreak),
            Token::new(span(20, 20), EndOfInput),
        ],
    );
}

#[test]
fn empty_indented_section_does_not_create_indentation_tokens() {
    assert_lex_eq!(
        "\
of
do

of
    do
",
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(3, 4), LineBreak),
            Token::new(span(4, 6), Do),
            Token::new(span(6, 8), LineBreak),
            Token::new(span(8, 10), Of),
            Token::new(span(11, 15), Indentation),
            Token::new(span(15, 17), Do),
            Token::new(span(18, 18), Dedentation),
            Token::new(span(18, 18), LineBreak),
            Token::new(span(18, 18), EndOfInput),
        ],
    );
}

#[test]
fn keyword_do_and_of_and_no_block_follows() {
    assert_lex_eq!(
        r#"
do it
of"it"
"#,
        vec![
            Token::new(span(1, 2), LineBreak),
            Token::new(span(2, 4), Do),
            Token::new(span(5, 7), Word("it".into())),
            Token::new(span(7, 8), LineBreak),
            Token::new(span(8, 10), Of),
            Token::new(span(10, 14), TextLiteral("it".into())),
            Token::new(span(14, 15), LineBreak),
            Token::new(span(15, 15), EndOfInput),
        ],
    );
}

#[test]
fn round_bracket_closes_indented_section() {
    assert_lex_eq!(
        "\
(of
    fo)
(of
    fo
    )
",
        vec![
            Token::new(span(1, 2), OpeningRoundBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), Indentation),
            Token::new(span(9, 11), Word("fo".into())),
            // @Question better span?
            Token::new(span(11, 12), Dedentation),
            Token::new(span(11, 12), ClosingRoundBracket),
            Token::new(span(12, 13), LineBreak),
            Token::new(span(13, 14), OpeningRoundBracket),
            Token::new(span(14, 16), Of),
            Token::new(span(17, 21), Indentation),
            Token::new(span(21, 23), Word("fo".into())),
            // @Question better span?
            Token::new(span(28, 29), Dedentation),
            Token::new(span(28, 29), ClosingRoundBracket),
            Token::new(span(29, 30), LineBreak),
            Token::new(span(30, 30), EndOfInput),
        ],
    );
}

#[test]
fn square_bracket_closes_indented_section() {
    assert_lex_eq!(
        "\
[of
    fo]
#",
        vec![
            Token::new(span(1, 2), OpeningSquareBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), Indentation),
            Token::new(span(9, 11), Word("fo".into())),
            // @Question better span?
            Token::new(span(11, 12), Dedentation),
            Token::new(span(11, 12), ClosingSquareBracket),
            Token::new(span(12, 13), LineBreak),
            Token::new(span(13, 14), Symbol("#".into())),
            Token::new(span(14, 14), EndOfInput),
        ],
    );
}

#[test]
fn pair_of_brackets_does_not_close_indented_section() {
    assert_lex_eq!(
        "\
of
    (f [])
    inside
",
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), Indentation),
            Token::new(span(8, 9), OpeningRoundBracket),
            Token::new(span(9, 10), Word("f".into())),
            Token::new(span(11, 12), OpeningSquareBracket),
            Token::new(span(12, 13), ClosingSquareBracket),
            Token::new(span(13, 14), ClosingRoundBracket),
            Token::new(span(14, 15), LineBreak),
            Token::new(span(19, 25), Word("inside".into())),
            Token::new(span(26, 26), Dedentation),
            Token::new(span(26, 26), LineBreak),
            Token::new(span(26, 26), EndOfInput),
        ],
    );
}

/// `=>` and `=` are indeed visually aligned *but* the `)` depends the first indentation
/// and as such, the `=` should be considered (further) indented relative to the line with
/// the closing bracket.
#[test]
fn brackets_reset_indentation() {
    assert_lex_eq!(
        "\
(of
    =>)
    = .
",
        vec![
            Token::new(span(1, 2), OpeningRoundBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), Indentation),
            Token::new(span(9, 11), WideArrowRight),
            Token::new(span(11, 12), Dedentation),
            Token::new(span(11, 12), ClosingRoundBracket),
            Token::new(span(17, 18), Equals),
            Token::new(span(19, 20), Dot),
            Token::new(span(20, 21), LineBreak),
            Token::new(span(21, 21), EndOfInput),
        ],
    );
}

#[test]
fn dedented_closing_bracket_does_not_create_line_break() {
    assert_lex_eq!(
        "\
hook = (
    element
)",
        vec![
            Token::new(span(1, 5), Word("hook".into())),
            Token::new(span(6, 7), Equals),
            Token::new(span(8, 9), OpeningRoundBracket),
            Token::new(span(14, 21), Word("element".into())),
            Token::new(span(22, 23), ClosingRoundBracket),
            Token::new(span(23, 23), EndOfInput),
        ]
    );
}

#[test]
fn indentation_at_start_of_input() {
    assert_lex_eq!(
        "    data
data",
        vec![
            Token::new(span(5, 9), Data),
            Token::new(span(9, 10), LineBreak),
            Token::new(span(10, 14), Data),
            Token::new(span(14, 14), EndOfInput),
        ]
    );
}

#[test]
fn invalid_indentation_at_start_of_input() {
    assert_lex_eq!(
        " !",
        vec![
            Token::new(span(2, 3), Symbol("!".into())),
            Token::new(span(3, 3), EndOfInput),
        ],
        vec![Error::new(
            span(1, 2),
            BareError::InvalidIndentation(Spaces(1), IndentationError::Misaligned)
        )]
    );
}
