use super::{Token, TokenKind::*};
use crate::{
    error::{Health, Outcome, Result},
    span::span,
};

fn lex(source: &'static str) -> Result<Outcome<Vec<Token>>> {
    super::lex(source.to_owned())
}

fn assert_ok_token(actual: Result<Outcome<Vec<Token>>>, expected: Vec<Token>) {
    match actual {
        Ok(Outcome {
            value: actual,
            health: Health::Untainted,
        }) => {
            if actual != expected {
                // @Beacon @Temporary
                std::env::set_var("NO_COLOR", "");

                panic!(
                    "the actual tokens outputted by the lexer do not match the expected ones:\n{}",
                    difference::Changeset::new(
                        &format!("{:#?}", expected),
                        &format!("{:#?}", actual),
                        "\n"
                    )
                );
            }
        }
        _ => panic!("expected the tokens `{:?}` but got an `Err`", expected),
    }
}

#[allow(unused_macros)]
macro no_std_assert($( $anything:tt )*) {
    compile_error!(
        "use function `assert_ok_token` or `assert_err` instead of macro `assert_eq` and similar"
    )
}

#[allow(unused_imports)]
use no_std_assert as assert_eq;
#[allow(unused_imports)]
use no_std_assert as assert_ne;

#[test]
fn comments() {
    assert_ok_token(
        lex("
;;; bland commentary ensues
;;; a filler line
;;; and an end
"),
        vec![
            Token::new_virtual(Semicolon, span(1, 1)),
            Token::new(EndOfInput, span(62, 62)),
        ],
    );
}

#[test]
fn documentation_comments() {
    assert_ok_token(
        lex("\
alpha;;;文本
0401 ;; stray documentation comment
;; next one
;;有意思的信"),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_number_literal("0401".into(), span(16, 19)),
            Token::new(DocumentationComment, span(21, 51)),
            Token::new(DocumentationComment, span(52, 63)),
            Token::new(DocumentationComment, span(64, 80)),
            Token::new(EndOfInput, span(80, 80)),
        ],
    );
}

#[test]
fn identifiers() {
    assert_ok_token(
        lex("alpha alpha0 _alpha al6ha_beta_"),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_identifier("alpha0".into(), span(7, 12)),
            Token::new_identifier("_alpha".into(), span(14, 19)),
            Token::new_identifier("al6ha_beta_".into(), span(21, 31)),
            Token::new(EndOfInput, span(31, 31)),
        ],
    );
}

#[test]
fn dashed_identifiers() {
    assert_ok_token(
        lex("ALPH4-G4MM4 alpha-gamma _-_"),
        vec![
            Token::new_identifier("ALPH4-G4MM4".into(), span(1, 11)),
            Token::new_identifier("alpha-gamma".into(), span(13, 23)),
            Token::new_identifier("_-_".into(), span(25, 27)),
            Token::new(EndOfInput, span(27, 27)),
        ],
    );
}

#[test]
fn keywords_and_lookalikes() {
    assert_ok_token(
        lex("self   Type Type_ Type-Type in _"),
        vec![
            Token::new(Self_, span(1, 4)),
            Token::new(Type, span(8, 11)),
            Token::new_identifier("Type_".into(), span(13, 17)),
            Token::new_identifier("Type-Type".into(), span(19, 27)),
            Token::new(In, span(29, 30)),
            Token::new(Underscore, span(32, 32)),
            Token::new(EndOfInput, span(32, 32)),
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
    assert_ok_token(
        lex("+ +>alpha//$~%  #0 . .."),
        vec![
            Token::new_punctuation("+".into(), span(1, 1)),
            Token::new_punctuation("+>".into(), span(3, 4)),
            Token::new_identifier("alpha".into(), span(5, 9)),
            Token::new_punctuation("//$~%".into(), span(10, 14)),
            Token::new_punctuation("#".into(), span(17, 17)),
            Token::new_number_literal("0".into(), span(18, 18)),
            Token::new(Dot, span(20, 20)),
            Token::new_punctuation("..".into(), span(22, 23)),
            Token::new(EndOfInput, span(23, 23)),
        ],
    );
}

#[test]
fn identifier_with_trailing_dot() {
    assert_ok_token(
        lex("namespace."),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new(EndOfInput, span(10, 10)),
        ],
    )
}

#[test]
fn identifier_dot_punctuation() {
    assert_ok_token(
        lex("namespace.+>!"),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new_punctuation("+>!".into(), span(11, 13)),
            Token::new(EndOfInput, span(13, 13)),
        ],
    )
}

#[test]
fn lex_identifier_dot_dotted_punctuation() {
    assert_ok_token(
        lex("namespace.$.?!."),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new(Dot, span(10, 10)),
            Token::new_punctuation("$.?!.".into(), span(11, 15)),
            Token::new(EndOfInput, span(15, 15)),
        ],
    )
}

#[test]
fn lex_identifier_and_dotted_punctuation_after_space() {
    assert_ok_token(
        lex("namespace .$.?!."),
        vec![
            Token::new_identifier("namespace".into(), span(1, 9)),
            Token::new_punctuation(".$.?!.".into(), span(11, 16)),
            Token::new(EndOfInput, span(16, 16)),
        ],
    )
}

#[test]
fn lex_keyword_dot_punctuation() {
    assert_ok_token(
        lex("data.#"),
        vec![
            Token::new(Data, span(1, 4)),
            Token::new(Dot, span(5, 5)),
            Token::new_punctuation("#".into(), span(6, 6)),
            Token::new(EndOfInput, span(6, 6)),
        ],
    )
}

#[test]
fn lex_number_literals() {
    assert_ok_token(
        lex("1001409409220293022239833211 01"),
        vec![
            Token::new_number_literal("1001409409220293022239833211".into(), span(1, 28)),
            Token::new_number_literal("01".into(), span(30, 31)),
            Token::new(EndOfInput, span(31, 31)),
        ],
    );
}

#[test]
fn lex_number_literals_with_separators() {
    assert_ok_token(
        lex(r#"334 1'000what 3'2'2'1"" 500 10"" -23"#),
        vec![
            Token::new_number_literal("334".into(), span(1, 3)),
            Token::new_number_literal("1000".into(), span(5, 9)),
            Token::new_identifier("what".into(), span(10, 13)),
            Token::new_number_literal("3221".into(), span(15, 21)),
            Token::new_text_literal(String::new(), span(22, 23), true),
            Token::new_number_literal("500".into(), span(25, 27)),
            Token::new_number_literal("10".into(), span(29, 30)),
            Token::new_text_literal(String::new(), span(31, 32), true),
            Token::new_number_literal("-23".into(), span(34, 36)),
            Token::new(EndOfInput, span(36, 36)),
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
    assert_ok_token(
        lex(r#""
    al
  pha""#),
        vec![
            Token::new_text_literal(
                "
    al
  pha"
                .into(),
                span(1, 15),
                true,
            ),
            Token::new(EndOfInput, span(15, 15)),
        ],
    );
}

#[test]
fn lex_unterminated_text_literal() {
    assert_ok_token(
        lex(r#""text message"#),
        vec![
            Token::new_text_literal("text message".into(), span(1, 13), false),
            Token::new(EndOfInput, span(13, 13)),
        ],
    );
}

#[test]
fn lex_single_quote() {
    assert_ok_token(
        lex("'"),
        vec![
            Token::new(SingleQuote, span(1, 1)),
            Token::new(EndOfInput, span(1, 1)),
        ],
    );
}

#[test]
fn lex_brackets() {
    assert_ok_token(
        lex("(( )( ))"),
        vec![
            Token::new(OpeningRoundBracket, span(1, 1)),
            Token::new(OpeningRoundBracket, span(2, 2)),
            Token::new(ClosingRoundBracket, span(4, 4)),
            Token::new(OpeningRoundBracket, span(5, 5)),
            Token::new(ClosingRoundBracket, span(7, 7)),
            Token::new(ClosingRoundBracket, span(8, 8)),
            Token::new(EndOfInput, span(8, 8)),
        ],
    );
}

// @Task update to the new system
#[test]
#[ignore]
fn do_not_lex_unbalanced_round_brackets_too_few_closing() {
    assert!(lex("((").is_err());
}

#[test]
#[ignore]
fn do_not_lex_unbalanced_round_brackets_too_few_opening() {
    assert!(lex(")))").is_err());
}

#[test]
fn bare_non_ascii_is_illegal() {
    assert_ok_token(
        lex("函数"),
        vec![
            Token::new_illegal('\u{51FD}', span(1, 3)),
            Token::new_illegal('\u{6570}', span(4, 6)),
            Token::new(EndOfInput, span(6, 6)),
        ],
    );
}

#[test]
fn bare_non_ascii_are_illegal_but_non_fatal() {
    assert_ok_token(
        lex(" 函数 function"),
        vec![
            Token::new_illegal('\u{51FD}', span(2, 4)),
            Token::new_illegal('\u{6570}', span(5, 7)),
            Token::new_identifier("function".into(), span(9, 16)),
            Token::new(EndOfInput, span(16, 16)),
        ],
    );
}

#[test]
fn backticks_are_illegal() {
    assert_ok_token(
        lex("`"),
        vec![
            Token::new_illegal('`', span(1, 1)),
            Token::new(EndOfInput, span(1, 1)),
        ],
    );
}

#[test]
fn backticks_are_illegal_right_after_number_literal() {
    assert_ok_token(
        lex("1`"),
        vec![
            Token::new_number_literal("1".into(), span(1, 1)),
            Token::new_illegal('`', span(2, 2)),
            Token::new(EndOfInput, span(2, 2)),
        ],
    );
}

#[test]
fn tabs_are_illegal() {
    assert_ok_token(
        lex("\t\t"),
        vec![
            Token::new_illegal('\t', span(1, 1)),
            Token::new_illegal('\t', span(2, 2)),
            Token::new(EndOfInput, span(2, 2)),
        ],
    );
}

#[test]
fn line_breaks_are_terminators_at_the_toplevel() {
    assert_ok_token(
        lex("\
alpha #?
100 it

\"moot\"\
"),
        vec![
            Token::new_identifier("alpha".into(), span(1, 5)),
            Token::new_punctuation("#?".into(), span(7, 8)),
            Token::new_virtual(Semicolon, span(9, 9)),
            Token::new_number_literal("100".into(), span(10, 12)),
            Token::new_identifier("it".into(), span(14, 15)),
            Token::new_virtual(Semicolon, span(16, 17)),
            Token::new_text_literal("moot".into(), span(18, 23), true),
            Token::new(EndOfInput, span(23, 23)),
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
    assert_ok_token(
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
            Token::new_identifier("start".into(), span(1, 5)),
            Token::new_identifier("middle".into(), span(7, 12)),
            Token::new_identifier("end".into(), span(18, 20)),
            Token::new_virtual(Semicolon, span(21, 21)),
            Token::new_text_literal("anything\n    really".into(), span(22, 42), true),
            Token::new_number_literal("3291238".into(), span(48, 54)),
            Token::new(Module, span(64, 69)),
            Token::new(OpeningRoundBracket, span(83, 83)),
            Token::new(ClosingRoundBracket, span(101, 101)),
            Token::new_virtual(Semicolon, span(102, 103)),
            Token::new_punctuation("$%&~~".into(), span(104, 108)),
            Token::new_punctuation(".!^".into(), span(114, 116)),
            Token::new_punctuation(r"\/".into(), span(119, 120)),
            Token::new(EndOfInput, span(120, 120)),
        ],
    );
}

#[test]
fn line_breaks_are_not_terminators_in_continued_sections() {
    assert_ok_token(
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
            Token::new_number_literal("-0".into(), span(1, 2)),
            Token::new_identifier("off".into(), span(8, 10)),
            Token::new_identifier("side".into(), span(16, 19)),
            Token::new_text_literal(String::new(), span(25, 26), true),
            Token::new_virtual(Semicolon, span(27, 27)),
            Token::new_punctuation("@@@".into(), span(28, 30)),
            Token::new(At, span(32, 32)),
            Token::new_identifier("lvl1".into(), span(40, 43)),
            Token::new_identifier("lvl2".into(), span(53, 56)),
            Token::new_identifier("lvl2".into(), span(66, 69)),
            Token::new_identifier("lvl1".into(), span(75, 78)),
            Token::new_number_literal("1".into(), span(84, 84)),
            Token::new_virtual(Semicolon, span(85, 85)),
            Token::new(EndOfInput, span(85, 85)),
        ],
    );
}

#[test]
fn keyword_of_introduces_indented_sections() {
    // @Task test sth similar with no trailing line break at the end ("early" EOI)
    assert_ok_token(
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
            Token::new(Of, span(1, 2)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(OpeningCurlyBracket, span(3, 7)),
            Token::new_identifier("something".into(), span(8, 16)),
            Token::new_virtual(Semicolon, span(17, 17)),
            Token::new_identifier("more".into(), span(22, 25)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(ClosingCurlyBracket, span(26, 26)),
            Token::new_virtual(Semicolon, span(26, 26)),
            Token::new(Of, span(27, 28)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(OpeningCurlyBracket, span(29, 34)),
            Token::new_number_literal("1980".into(), span(35, 38)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(ClosingCurlyBracket, span(39, 40)),
            Token::new_virtual(Semicolon, span(39, 40)),
            Token::new_punctuation(">".into(), span(41, 41)),
            Token::new(Of, span(42, 43)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(OpeningCurlyBracket, span(44, 48)),
            Token::new(Module, span(49, 54)),
            Token::new(Of, span(56, 57)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(OpeningCurlyBracket, span(58, 66)),
            Token::new_identifier("CONTENT".into(), span(67, 73)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(ClosingCurlyBracket, span(74, 78)),
            // @Bug wrong span?
            Token::new_virtual(Semicolon, span(74, 78)),
            Token::new(Of, span(79, 80)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(OpeningCurlyBracket, span(81, 89)),
            Token::new_punctuation(">>!<<".into(), span(90, 94)),
            // @Task we need to associate the token with a more useful span
            Token::new_virtual(ClosingCurlyBracket, span(95, 95)),
            Token::new_virtual(Semicolon, span(95, 95)),
            // // @Task we need to associate the token with a more useful span
            Token::new_virtual(ClosingCurlyBracket, span(95, 95)),
            Token::new_virtual(Semicolon, span(95, 95)),
            Token::new(EndOfInput, span(95, 95)),
        ],
    );
}

#[test]
fn no_superfluous_virtual_semicolon_before_virtual_curly_bracket_with_continued_section() {
    assert_ok_token(
        lex("\
of
    a
        b
"),
        vec![
            Token::new(Of, span(1, 2)),
            Token::new_virtual(OpeningCurlyBracket, span(3, 7)),
            Token::new_identifier("a".into(), span(8, 8)),
            Token::new_identifier("b".into(), span(18, 18)),
            Token::new_virtual(ClosingCurlyBracket, span(19, 19)),
            Token::new_virtual(Semicolon, span(19, 19)),
            Token::new(EndOfInput, span(19, 19)),
        ],
    );
}

#[test]
#[ignore]
fn keyword_do_introduces_indented_sections() {
    todo!()
}

#[test]
fn empty_indented_section_does_not_create_virtual_curly_brackets() {
    assert_ok_token(
        lex("\
of
do

of
    do
"),
        vec![
            Token::new(Of, span(1, 2)),
            Token::new_virtual(Semicolon, span(3, 3)),
            Token::new(Do, span(4, 5)),
            Token::new_virtual(Semicolon, span(6, 7)),
            Token::new(Of, span(8, 9)),
            Token::new_virtual(OpeningCurlyBracket, span(10, 14)),
            Token::new(Do, span(15, 16)),
            // Token::new_virtual(Semicolon, span(17, 17)), // @Beacon hmm
            Token::new_virtual(ClosingCurlyBracket, span(17, 17)),
            Token::new_virtual(Semicolon, span(17, 17)),
            Token::new(EndOfInput, span(17, 17)),
        ],
    )
}

// @Beacon @Task smh create a Stack<Section> of the form [TopLevel,Continued,Indented]
// #[test]
// fn yyyy() {
//     todo!()
// }

// @Beacon @Task smh create a Vec<Section> of the form [TopLevel,Indented,Continued]
// #[test]
// fn yyyy() {
//     todo!()
// }

#[test]
fn keyword_do_and_of_and_no_block_follows() {
    assert_ok_token(
        lex(r#"
do it
of"it"
"#),
        vec![
            Token::new_virtual(Semicolon, span(1, 1)),
            Token::new(Do, span(2, 3)),
            Token::new_identifier("it".into(), span(5, 6)),
            Token::new_virtual(Semicolon, span(7, 7)),
            Token::new(Of, span(8, 9)),
            Token::new_text_literal("it".into(), span(10, 13), true),
            Token::new_virtual(Semicolon, span(14, 14)),
            Token::new(EndOfInput, span(14, 14)),
        ],
    );
}

// @Task

// #[test]
// fn xxxxxx() {
//     let _ = lex("\n    \n");
//     todo!();
// }

#[test]
fn round_bracket_closes_indented_section() {
    assert_ok_token(
        lex("\
(of
    fo)
(of
    fo
    )
"),
        vec![
            Token::new(OpeningRoundBracket, span(1, 1)),
            Token::new(Of, span(2, 3)),
            // @Bug wrong span
            Token::new_virtual(OpeningCurlyBracket, span(4, 8)),
            Token::new_identifier("fo".into(), span(9, 10)),
            // @Question better span?
            Token::new_virtual(ClosingCurlyBracket, span(11, 11)),
            Token::new(ClosingRoundBracket, span(11, 11)),
            Token::new_virtual(Semicolon, span(12, 12)),
            Token::new(OpeningRoundBracket, span(13, 13)),
            Token::new(Of, span(14, 15)),
            // @Bug wrong span
            Token::new_virtual(OpeningCurlyBracket, span(16, 20)),
            Token::new_identifier("fo".into(), span(21, 22)),
            Token::new_virtual(Semicolon, span(23, 23)),
            // @Question better span?
            Token::new_virtual(ClosingCurlyBracket, span(28, 28)),
            Token::new(ClosingRoundBracket, span(28, 28)),
            Token::new_virtual(Semicolon, span(29, 29)),
            Token::new(EndOfInput, span(29, 29)),
        ],
    );
}

// @Task
#[test]
#[ignore]
fn square_bracket_closes_indented_section() {}

#[test]
fn pair_of_brackets_does_not_close_indented_section() {
    assert_ok_token(
        lex("\
of
    (f [])
    inside
"),
        vec![
            Token::new(Of, span(1, 2)),
            // @Bug wrong span
            Token::new_virtual(OpeningCurlyBracket, span(3, 7)),
            Token::new(OpeningRoundBracket, span(8, 8)),
            Token::new_identifier("f".into(), span(9, 9)),
            Token::new(OpeningSquareBracket, span(11, 11)),
            Token::new(ClosingSquareBracket, span(12, 12)),
            Token::new(ClosingRoundBracket, span(13, 13)),
            Token::new_virtual(Semicolon, span(14, 14)),
            Token::new_identifier("inside".into(), span(19, 24)),
            Token::new_virtual(ClosingCurlyBracket, span(25, 25)),
            Token::new_virtual(Semicolon, span(25, 25)),
            Token::new(EndOfInput, span(25, 25)),
        ],
    );
}

// @Question should the single dot really be a Dot? shouldn't it be
// punctuation?
/// Yes, `=>` and `=` are aligned *but* the `)` outdents the first indentation and
/// and such, the `=` should be considered (more) indented relative to the line with
/// the closing bracket.
// @Task rephrase the above
#[test]
fn brackets_reset_indentation() {
    assert_ok_token(
        lex("\
(of
    =>)
    = .
"),
        vec![
            Token::new(OpeningRoundBracket, span(1, 1)),
            Token::new(Of, span(2, 3)),
            Token::new_virtual(OpeningCurlyBracket, span(4, 8)),
            Token::new(WideArrow, span(9, 10)),
            Token::new_virtual(ClosingCurlyBracket, span(11, 11)),
            Token::new(ClosingRoundBracket, span(11, 11)),
            Token::new(Equals, span(17, 17)),
            Token::new(Dot, span(19, 19)),
            Token::new_virtual(Semicolon, span(20, 20)),
            Token::new(EndOfInput, span(20, 20)),
        ],
    )
}

// @Task make this a UI test (maybe?)
#[test]
#[ignore]
fn do_not_lex_too_shallow_indentation() {
    todo!()
}

// @Task make this a UI test (maybe?)
#[test]
#[ignore]
fn do_not_lex_too_deep_indentation() {
    todo!()
}
