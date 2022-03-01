//! (Mostly) positive behavior tests of the lexer.
//!
//! Negative behavior tests are UI tests to be found in `parsing/`.

use super::{Provenance, Token, TokenKind::*, UnterminatedTextLiteral};
use crate::{
    error::{Health, Outcome, Result},
    span::span,
    utility::difference,
};
use std::default::default;

fn lex(source: &'static str) -> Result<Outcome<Vec<Token>>, ()> {
    super::lex_string(source.to_owned())
}

fn assert_eq(actual: Result<Outcome<Vec<Token>>, ()>, expected: Vec<Token>) {
    assert_eq_with_health(actual, expected, Health::Untainted)
}

fn assert_eq_tainted(actual: Result<Outcome<Vec<Token>>, ()>, expected: Vec<Token>) {
    assert_eq_with_health(actual, expected, Health::Tainted)
}

fn assert_eq_with_health(
    actual: Result<Outcome<Vec<Token>>, ()>,
    expected: Vec<Token>,
    expected_health: Health,
) {
    match actual {
        Ok(Outcome!(actual, health)) => {
            if health != expected_health {
                panic!(
                    "expected the tokens `{actual:?}` to be \
                     {expected_health} but they are {health}",
                );
            }

            if actual != expected {
                panic!(
                    "the actual tokens outputted by the lexer do not match the expected ones:\n{}",
                    difference(&format!("{expected:#?}"), &format!("{actual:#?}"), "\n"),
                );
            }
        }
        _ => panic!("expected the tokens `{expected:?}` but an error was (silently) reported"),
    }
}

#[allow(unused_imports)]
use crate::utility::no_std_assert as assert_eq;
#[allow(unused_imports)]
use crate::utility::no_std_assert as assert_ne;

#[test]
fn comments() {
    assert_eq(
        lex("
;;; bland commentary ensues
;;; a filler line
;;; and an end
"),
        vec![
            Token::new(span(1, 2), Semicolon(Provenance::Lexer)),
            Token::new(span(63, 63), EndOfInput),
        ],
    );
}

#[test]
fn documentation_comments() {
    assert_eq(
        lex("\
alpha;;;文本
0401 ;; stray documentation comment
;; next one
;;有意思的信"),
        vec![
            Token::new(span(1, 6), Word("alpha".into())),
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
    assert_eq(
        lex("\
;;
;; non-empty
"),
        vec![
            Token::new(span(1, 3), DocumentationComment),
            Token::new(span(4, 16), DocumentationComment),
            Token::new(span(17, 17), EndOfInput),
        ],
    );
}

#[test]
fn shebang() {
    assert_eq(
        lex("\
#!/usr/bin/lushui run
it"),
        vec![
            Token::new(span(23, 25), Word("it".into())),
            Token::new(span(25, 25), EndOfInput),
        ],
    )
}

#[test]
fn not_a_shebang_but_a_hashtag() {
    assert_eq(
        lex("\
#hashtag"),
        vec![
            Token::new(span(1, 2), Punctuation("#".into())),
            Token::new(span(2, 9), Word("hashtag".into())),
            Token::new(span(9, 9), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shabang_but_a_hash() {
    assert_eq(
        lex("\
#"),
        vec![
            Token::new(span(1, 2), Punctuation("#".into())),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shebang_but_punctuation() {
    assert_eq(
        lex("\
#?/WEIRD"),
        vec![
            Token::new(span(1, 4), Punctuation("#?/".into())),
            Token::new(span(4, 9), Word("WEIRD".into())),
            Token::new(span(9, 9), EndOfInput),
        ],
    );
}

#[test]
fn shebang_lookalike_not_first_line() {
    assert_eq(
        lex("
#!/usr/bin/lushui run
"),
        vec![
            Token::new(span(1, 2), Semicolon(Provenance::Lexer)),
            Token::new(span(2, 5), Punctuation("#!/".into())),
            Token::new(span(5, 8), Word("usr".into())),
            Token::new(span(8, 9), Punctuation("/".into())),
            Token::new(span(9, 12), Word("bin".into())),
            Token::new(span(12, 13), Punctuation("/".into())),
            Token::new(span(13, 19), Word("lushui".into())),
            Token::new(span(20, 23), Word("run".into())),
            Token::new(span(23, 24), Semicolon(Provenance::Lexer)),
            Token::new(span(24, 24), EndOfInput),
        ],
    )
}

#[test]
fn identifiers() {
    assert_eq(
        lex("alpha alpha0 _alpha al6ha_beta_"),
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
    assert_eq(
        lex("ALPH4-G4MM4 alpha-gamma _-_"),
        vec![
            Token::new(span(1, 12), Word("ALPH4-G4MM4".into())),
            Token::new(span(13, 24), Word("alpha-gamma".into())),
            Token::new(span(25, 28), Word("_-_".into())),
            Token::new(span(28, 28), EndOfInput),
        ],
    );
}

#[test]
fn keywords_and_lookalikes() {
    assert_eq(
        lex("self   Type Type_ Type-Type in _"),
        vec![
            Token::new(span(1, 5), Self_),
            Token::new(span(8, 12), Type),
            Token::new(span(13, 18), Word("Type_".into())),
            Token::new(span(19, 28), Word("Type-Type".into())),
            Token::new(span(29, 31), In),
            Token::new(span(32, 33), Underscore),
            Token::new(span(33, 33), EndOfInput),
        ],
    );
}

// @Task make this a UI test
#[test]
fn do_not_lex_identifier_with_trailing_dash() {
    assert!(lex("alpha-").is_err());
}

// @Task make this a UI test
#[test]
fn do_not_lex_identifier_with_trailing_dash_punctuation() {
    assert!(lex("alpha-:").is_err());
}

// @Task make this a UI test
#[test]
fn do_not_lex_identifier_with_trailing_dash_number_literal() {
    assert!(lex("alpha-0").is_err());
}

// @Task make this a UI test
#[test]
fn do_not_lex_identifier_with_consecutive_dashes() {
    assert!(lex("alpha--gamma").is_err());
}

#[test]
fn punctuation() {
    assert_eq(
        lex("+ +>alpha//$~%  #0 . .."),
        vec![
            Token::new(span(1, 2), Punctuation("+".into())),
            Token::new(span(3, 5), Punctuation("+>".into())),
            Token::new(span(5, 10), Word("alpha".into())),
            Token::new(span(10, 15), Punctuation("//$~%".into())),
            Token::new(span(17, 18), Punctuation("#".into())),
            Token::new(span(18, 19), NumberLiteral("0".into())),
            Token::new(span(20, 21), Dot),
            Token::new(span(22, 24), Punctuation("..".into())),
            Token::new(span(24, 24), EndOfInput),
        ],
    );
}

#[test]
fn identifier_with_trailing_dot() {
    assert_eq(
        lex("namespace."),
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 11), EndOfInput),
        ],
    )
}

#[test]
fn identifier_dot_punctuation() {
    assert_eq(
        lex("namespace.+>!"),
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 14), Punctuation("+>!".into())),
            Token::new(span(14, 14), EndOfInput),
        ],
    )
}

#[test]
fn lex_identifier_dot_dotted_punctuation() {
    assert_eq(
        lex("namespace.$.?!."),
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(10, 11), Dot),
            Token::new(span(11, 16), Punctuation("$.?!.".into())),
            Token::new(span(16, 16), EndOfInput),
        ],
    )
}

#[test]
fn lex_identifier_and_dotted_punctuation_after_space() {
    assert_eq(
        lex("namespace .$.?!."),
        vec![
            Token::new(span(1, 10), Word("namespace".into())),
            Token::new(span(11, 17), Punctuation(".$.?!.".into())),
            Token::new(span(17, 17), EndOfInput),
        ],
    )
}

#[test]
fn lex_keyword_dot_punctuation() {
    assert_eq(
        lex("data.#"),
        vec![
            Token::new(span(1, 5), Data),
            Token::new(span(5, 6), Dot),
            Token::new(span(6, 7), Punctuation("#".into())),
            Token::new(span(7, 7), EndOfInput),
        ],
    )
}

#[test]
fn lex_number_literals() {
    assert_eq(
        lex("1001409409220293022239833211 01"),
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
    assert_eq(
        lex(r#"334 1'000what 3'2'2'1"" 500 10"" -23"#),
        vec![
            Token::new(span(1, 4), NumberLiteral("334".into())),
            Token::new(span(5, 10), NumberLiteral("1000".into())),
            Token::new(span(10, 14), Word("what".into())),
            Token::new(span(15, 22), NumberLiteral("3221".into())),
            Token::new(span(22, 24), TextLiteral(Ok(default()))),
            Token::new(span(25, 28), NumberLiteral("500".into())),
            Token::new(span(29, 31), NumberLiteral("10".into())),
            Token::new(span(31, 33), TextLiteral(Ok(default()))),
            Token::new(span(34, 37), NumberLiteral("-23".into())),
            Token::new(span(37, 37), EndOfInput),
        ],
    );
}

// @Task make this a UI test
#[test]
fn do_no_lex_number_literal_with_consecutive_separators() {
    assert!(lex("3''100").is_err());
}

// @Task make this a UI test
#[test]
fn do_not_lex_number_literal_with_trailing_separator() {
    assert!(lex("10' ").is_err());
}

// @Task make this a UI test
#[test]
fn do_not_lex_number_literal_with_trailing_separator_right_before_eoi() {
    assert!(lex("10'").is_err());
}

#[test]
fn lex_text_literal() {
    assert_eq(
        lex(r#""
    al
  pha""#),
        vec![
            Token::new(
                span(1, 16),
                TextLiteral(Ok("
    al
  pha"
                .into())),
            ),
            Token::new(span(16, 16), EndOfInput),
        ],
    );
}

#[test]
fn lex_unterminated_text_literal() {
    assert_eq(
        lex(r#""text message"#),
        vec![
            Token::new(span(1, 14), TextLiteral(Err(UnterminatedTextLiteral))),
            Token::new(span(14, 14), EndOfInput),
        ],
    );
}

#[test]
fn lex_single_quote() {
    assert_eq(
        lex("'"),
        vec![
            Token::new(span(1, 2), Apostrophe),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn lex_brackets() {
    assert_eq(
        lex("(( )( ))"),
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
fn bare_non_ascii_is_illegal() {
    assert_eq_tainted(
        lex("函数"),
        vec![
            Token::new(span(1, 4), Illegal('\u{51FD}')),
            Token::new(span(4, 7), Illegal('\u{6570}')),
            Token::new(span(7, 7), EndOfInput),
        ],
    );
}

#[test]
fn bare_non_ascii_are_illegal_but_non_fatal() {
    assert_eq_tainted(
        lex(" 函数 function"),
        vec![
            Token::new(span(2, 5), Illegal('函')),
            Token::new(span(5, 8), Illegal('数')),
            Token::new(span(9, 17), Word("function".into())),
            Token::new(span(17, 17), EndOfInput),
        ],
    );
}

#[test]
fn backticks_are_illegal() {
    assert_eq_tainted(
        lex("`"),
        vec![
            Token::new(span(1, 2), Illegal('`')),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn backticks_are_illegal_right_after_number_literal() {
    assert_eq_tainted(
        lex("1`"),
        vec![
            Token::new(span(1, 2), NumberLiteral("1".into())),
            Token::new(span(2, 3), Illegal('`')),
            Token::new(span(3, 3), EndOfInput),
        ],
    );
}

#[test]
fn tabs_are_illegal() {
    assert_eq_tainted(
        lex("\t\t"),
        vec![
            Token::new(span(1, 2), Illegal('\t')),
            Token::new(span(2, 3), Illegal('\t')),
            Token::new(span(3, 3), EndOfInput),
        ],
    );
}

#[test]
fn line_breaks_are_terminators_at_the_toplevel() {
    assert_eq(
        lex("\
alpha #?
100 it

\"moot\""),
        vec![
            Token::new(span(1, 6), Word("alpha".into())),
            Token::new(span(7, 9), Punctuation("#?".into())),
            Token::new(span(9, 10), Semicolon(Provenance::Lexer)),
            Token::new(span(10, 13), NumberLiteral("100".into())),
            Token::new(span(14, 16), Word("it".into())),
            Token::new(span(16, 18), Semicolon(Provenance::Lexer)),
            Token::new(span(18, 24), TextLiteral(Ok("moot".into()))),
            Token::new(span(24, 24), EndOfInput),
        ],
    );
}

// @Beacon @Task add a lot of tests of the interaction between
// line breaks, indentation and *comments*!

/// Indentation means line continuation unless it follows the keyword `of`
/// or `do` (in which case it creates a “proper”/reified section, namely an
/// indented section; not in this test).
#[test]
fn indentation_means_line_continuation() {
    assert_eq(
        lex("\
start middle
    end
\"anything
    really\"
    3291238
        module
            (
                )

$%&~~
    .!^  \\/"),
        vec![
            Token::new(span(1, 6), Word("start".into())),
            Token::new(span(7, 13), Word("middle".into())),
            Token::new(span(18, 21), Word("end".into())),
            Token::new(span(21, 22), Semicolon(Provenance::Lexer)),
            Token::new(span(22, 43), TextLiteral(Ok("anything\n    really".into()))),
            Token::new(span(48, 55), NumberLiteral("3291238".into())),
            Token::new(span(64, 70), Module),
            Token::new(span(83, 84), OpeningRoundBracket),
            Token::new(span(101, 102), ClosingRoundBracket),
            Token::new(span(102, 104), Semicolon(Provenance::Lexer)),
            Token::new(span(104, 109), Punctuation("$%&~~".into())),
            Token::new(span(114, 117), Punctuation(".!^".into())),
            Token::new(span(119, 121), Punctuation(r"\/".into())),
            Token::new(span(121, 121), EndOfInput),
        ],
    );
}

#[test]
fn line_breaks_are_not_terminators_in_continued_sections() {
    assert_eq(
        lex("\
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
"),
        vec![
            Token::new(span(1, 3), NumberLiteral("-0".into())),
            Token::new(span(8, 11), Word("off".into())),
            Token::new(span(16, 20), Word("side".into())),
            Token::new(span(25, 27), TextLiteral(Ok(default()))),
            Token::new(span(27, 28), Semicolon(Provenance::Lexer)),
            Token::new(span(28, 31), Punctuation("@@@".into())),
            Token::new(span(32, 33), At),
            Token::new(span(40, 44), Word("lvl1".into())),
            Token::new(span(53, 57), Word("lvl2".into())),
            Token::new(span(66, 70), Word("lvl2".into())),
            Token::new(span(75, 79), Word("lvl1".into())),
            Token::new(span(84, 85), NumberLiteral("1".into())),
            Token::new(span(85, 86), Semicolon(Provenance::Lexer)),
            Token::new(span(86, 86), EndOfInput),
        ],
    );
}

#[test]
fn keyword_of_introduces_indented_sections() {
    // @Task test sth similar with no trailing line break at the end ("early" EOI)
    assert_eq(
        lex("\
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
"),
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 17), Word("something".into())),
            Token::new(span(17, 18), Semicolon(Provenance::Lexer)),
            Token::new(span(22, 26), Word("more".into())),
            Token::new(span(27, 27), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(27, 27), Semicolon(Provenance::Lexer)),
            Token::new(span(27, 29), Of),
            Token::new(span(31, 35), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(35, 39), NumberLiteral("1980".into())),
            Token::new(span(41, 41), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(41, 41), Semicolon(Provenance::Lexer)),
            Token::new(span(41, 42), Punctuation(">".into())),
            Token::new(span(42, 44), Of),
            Token::new(span(45, 49), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(49, 55), Module),
            Token::new(span(56, 58), Of),
            Token::new(span(59, 67), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(67, 74), Word("CONTENT".into())),
            Token::new(span(75, 79), ClosingCurlyBracket(Provenance::Lexer)),
            // @Task this span should be empty, right?
            Token::new(span(75, 79), Semicolon(Provenance::Lexer)),
            Token::new(span(79, 81), Of),
            Token::new(span(82, 90), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(90, 95), Punctuation(">>!<<".into())),
            Token::new(span(96, 96), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(96, 96), Semicolon(Provenance::Lexer)),
            Token::new(span(96, 96), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(96, 96), Semicolon(Provenance::Lexer)),
            Token::new(span(96, 96), EndOfInput),
        ],
    );
}

#[test]
fn no_superfluous_virtual_semicolon_before_virtual_curly_bracket_with_continued_section() {
    assert_eq(
        lex("\
of
    a
        b
"),
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 9), Word("a".into())),
            Token::new(span(18, 19), Word("b".into())),
            Token::new(span(20, 20), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(20, 20), Semicolon(Provenance::Lexer)),
            Token::new(span(20, 20), EndOfInput),
        ],
    );
}

#[test]
#[ignore]
fn keyword_do_introduces_indented_sections() {
    todo!() // @Task
}

#[test]
fn empty_indented_section_does_not_create_virtual_curly_brackets() {
    assert_eq(
        lex("\
of
do

of
    do
"),
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(3, 4), Semicolon(Provenance::Lexer)),
            Token::new(span(4, 6), Do),
            Token::new(span(6, 8), Semicolon(Provenance::Lexer)),
            Token::new(span(8, 10), Of),
            Token::new(span(11, 15), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(15, 17), Do),
            Token::new(span(18, 18), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(18, 18), Semicolon(Provenance::Lexer)),
            Token::new(span(18, 18), EndOfInput),
        ],
    )
}

// @Task smh create a Stack<Section> of the form [TopLevel, Continued, Indented]

// @Task smh create a Vec<Section> of the form [TopLevel, Indented, Continued]

#[test]
fn keyword_do_and_of_and_no_block_follows() {
    assert_eq(
        lex(r#"
do it
of"it"
"#),
        vec![
            Token::new(span(1, 2), Semicolon(Provenance::Lexer)),
            Token::new(span(2, 4), Do),
            Token::new(span(5, 7), Word("it".into())),
            Token::new(span(7, 8), Semicolon(Provenance::Lexer)),
            Token::new(span(8, 10), Of),
            Token::new(span(10, 14), TextLiteral(Ok("it".into()))),
            Token::new(span(14, 15), Semicolon(Provenance::Lexer)),
            Token::new(span(15, 15), EndOfInput),
        ],
    );
}

// #[test]
// fn _() {
//     let _ = lex("\n    \n");
//     todo!(); // @Task
// }

#[test]
fn round_bracket_closes_indented_section() {
    assert_eq(
        lex("\
(of
    fo)
(of
    fo
    )
"),
        vec![
            Token::new(span(1, 2), OpeningRoundBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(9, 11), Word("fo".into())),
            // @Question better span?
            Token::new(span(11, 12), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(11, 12), ClosingRoundBracket),
            Token::new(span(12, 13), Semicolon(Provenance::Lexer)),
            Token::new(span(13, 14), OpeningRoundBracket),
            Token::new(span(14, 16), Of),
            Token::new(span(17, 21), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(21, 23), Word("fo".into())),
            Token::new(span(23, 24), Semicolon(Provenance::Lexer)),
            // @Question better span?
            Token::new(span(28, 29), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(28, 29), ClosingRoundBracket),
            Token::new(span(29, 30), Semicolon(Provenance::Lexer)),
            Token::new(span(30, 30), EndOfInput),
        ],
    );
}

#[test]
fn square_bracket_closes_indented_section() {
    assert_eq(
        lex("\
[of
    fo]
#"),
        vec![
            Token::new(span(1, 2), OpeningSquareBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(9, 11), Word("fo".into())),
            // @Question better span?
            Token::new(span(11, 12), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(11, 12), ClosingSquareBracket),
            Token::new(span(12, 13), Semicolon(Provenance::Lexer)),
            Token::new(span(13, 14), Punctuation("#".into())),
            Token::new(span(14, 14), EndOfInput),
        ],
    );
}

#[test]
fn pair_of_brackets_does_not_close_indented_section() {
    assert_eq(
        lex("\
of
    (f [])
    inside
"),
        vec![
            Token::new(span(1, 3), Of),
            Token::new(span(4, 8), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 9), OpeningRoundBracket),
            Token::new(span(9, 10), Word("f".into())),
            Token::new(span(11, 12), OpeningSquareBracket),
            Token::new(span(12, 13), ClosingSquareBracket),
            Token::new(span(13, 14), ClosingRoundBracket),
            Token::new(span(14, 15), Semicolon(Provenance::Lexer)),
            Token::new(span(19, 25), Word("inside".into())),
            Token::new(span(26, 26), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(26, 26), Semicolon(Provenance::Lexer)),
            Token::new(span(26, 26), EndOfInput),
        ],
    );
}

/// Yes, `=>` and `=` are aligned *but* the `)` “outdents” the first indentation and
/// and such, the `=` should be considered (more) indented relative to the line with
/// the closing bracket.
#[test]
fn brackets_reset_indentation() {
    assert_eq(
        lex("\
(of
    =>)
    = .
"),
        vec![
            Token::new(span(1, 2), OpeningRoundBracket),
            Token::new(span(2, 4), Of),
            Token::new(span(5, 9), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(9, 11), WideArrowRight),
            Token::new(span(11, 12), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(11, 12), ClosingRoundBracket),
            Token::new(span(17, 18), Equals),
            // @Question should this be Punctuation?
            Token::new(span(19, 20), Dot),
            Token::new(span(20, 21), Semicolon(Provenance::Lexer)),
            Token::new(span(21, 21), EndOfInput),
        ],
    )
}
