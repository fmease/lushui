use super::{Provenance, Token, TokenKind::*, UnterminatedTextLiteral};
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
            Token::new(span(1, 1), Semicolon(Provenance::Lexer)),
            Token::new(span(62, 62), EndOfInput),
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
            Token::new(span(1, 5), Identifier("alpha".into())),
            Token::new(span(16, 19), NumberLiteral("0401".into())),
            Token::new(span(21, 51), DocumentationComment),
            Token::new(span(52, 63), DocumentationComment),
            Token::new(span(64, 80), DocumentationComment),
            Token::new(span(80, 80), EndOfInput),
        ],
    );
}

#[test]
fn shebang() {
    assert_ok_token(
        lex("\
#!/usr/bin/lushui run
it"),
        vec![
            Token::new(span(23, 24), Identifier("it".into())),
            Token::new(span(24, 24), EndOfInput),
        ],
    )
}

#[test]
fn not_a_shebang_but_a_hashtag() {
    assert_ok_token(
        lex("\
#hashtag"),
        vec![
            Token::new(span(1, 1), Punctuation("#".into())),
            Token::new(span(2, 8), Identifier("hashtag".into())),
            Token::new(span(8, 8), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shabang_but_a_hash() {
    assert_ok_token(
        lex("\
#"),
        vec![
            Token::new(span(1, 1), Punctuation("#".into())),
            Token::new(span(1, 1), EndOfInput),
        ],
    );
}

#[test]
fn not_a_shebang_but_punctuation() {
    assert_ok_token(
        lex("\
#?/WEIRD"),
        vec![
            Token::new(span(1, 3), Punctuation("#?/".into())),
            Token::new(span(4, 8), Identifier("WEIRD".into())),
            Token::new(span(8, 8), EndOfInput),
        ],
    );
}

#[test]
fn shebang_lookalike_not_first_line() {
    assert_ok_token(
        lex("
#!/usr/bin/lushui run
"),
        vec![
            Token::new(span(1, 1), Semicolon(Provenance::Lexer)),
            Token::new(span(2, 4), Punctuation("#!/".into())),
            Token::new(span(5, 7), Identifier("usr".into())),
            Token::new(span(8, 8), Punctuation("/".into())),
            Token::new(span(9, 11), Identifier("bin".into())),
            Token::new(span(12, 12), Punctuation("/".into())),
            Token::new(span(13, 18), Identifier("lushui".into())),
            Token::new(span(20, 22), Identifier("run".into())),
            Token::new(span(23, 23), Semicolon(Provenance::Lexer)),
            Token::new(span(23, 23), EndOfInput),
        ],
    )
}

#[test]
fn identifiers() {
    assert_ok_token(
        lex("alpha alpha0 _alpha al6ha_beta_"),
        vec![
            Token::new(span(1, 5), Identifier("alpha".into())),
            Token::new(span(7, 12), Identifier("alpha0".into())),
            Token::new(span(14, 19), Identifier("_alpha".into())),
            Token::new(span(21, 31), Identifier("al6ha_beta_".into())),
            Token::new(span(31, 31), EndOfInput),
        ],
    );
}

#[test]
fn dashed_identifiers() {
    assert_ok_token(
        lex("ALPH4-G4MM4 alpha-gamma _-_"),
        vec![
            Token::new(span(1, 11), Identifier("ALPH4-G4MM4".into())),
            Token::new(span(13, 23), Identifier("alpha-gamma".into())),
            Token::new(span(25, 27), Identifier("_-_".into())),
            Token::new(span(27, 27), EndOfInput),
        ],
    );
}

#[test]
fn keywords_and_lookalikes() {
    assert_ok_token(
        lex("self   Type Type_ Type-Type in _"),
        vec![
            Token::new(span(1, 4), Self_),
            Token::new(span(8, 11), Type),
            Token::new(span(13, 17), Identifier("Type_".into())),
            Token::new(span(19, 27), Identifier("Type-Type".into())),
            Token::new(span(29, 30), In),
            Token::new(span(32, 32), Underscore),
            Token::new(span(32, 32), EndOfInput),
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
            Token::new(span(1, 1), Punctuation("+".into())),
            Token::new(span(3, 4), Punctuation("+>".into())),
            Token::new(span(5, 9), Identifier("alpha".into())),
            Token::new(span(10, 14), Punctuation("//$~%".into())),
            Token::new(span(17, 17), Punctuation("#".into())),
            Token::new(span(18, 18), NumberLiteral("0".into())),
            Token::new(span(20, 20), Dot),
            Token::new(span(22, 23), Punctuation("..".into())),
            Token::new(span(23, 23), EndOfInput),
        ],
    );
}

#[test]
fn identifier_with_trailing_dot() {
    assert_ok_token(
        lex("namespace."),
        vec![
            Token::new(span(1, 9), Identifier("namespace".into())),
            Token::new(span(10, 10), Dot),
            Token::new(span(10, 10), EndOfInput),
        ],
    )
}

#[test]
fn identifier_dot_punctuation() {
    assert_ok_token(
        lex("namespace.+>!"),
        vec![
            Token::new(span(1, 9), Identifier("namespace".into())),
            Token::new(span(10, 10), Dot),
            Token::new(span(11, 13), Punctuation("+>!".into())),
            Token::new(span(13, 13), EndOfInput),
        ],
    )
}

#[test]
fn lex_identifier_dot_dotted_punctuation() {
    assert_ok_token(
        lex("namespace.$.?!."),
        vec![
            Token::new(span(1, 9), Identifier("namespace".into())),
            Token::new(span(10, 10), Dot),
            Token::new(span(11, 15), Punctuation("$.?!.".into())),
            Token::new(span(15, 15), EndOfInput),
        ],
    )
}

#[test]
fn lex_identifier_and_dotted_punctuation_after_space() {
    assert_ok_token(
        lex("namespace .$.?!."),
        vec![
            Token::new(span(1, 9), Identifier("namespace".into())),
            Token::new(span(11, 16), Punctuation(".$.?!.".into())),
            Token::new(span(16, 16), EndOfInput),
        ],
    )
}

#[test]
fn lex_keyword_dot_punctuation() {
    assert_ok_token(
        lex("data.#"),
        vec![
            Token::new(span(1, 4), Data),
            Token::new(span(5, 5), Dot),
            Token::new(span(6, 6), Punctuation("#".into())),
            Token::new(span(6, 6), EndOfInput),
        ],
    )
}

#[test]
fn lex_number_literals() {
    assert_ok_token(
        lex("1001409409220293022239833211 01"),
        vec![
            Token::new(
                span(1, 28),
                NumberLiteral("1001409409220293022239833211".into()),
            ),
            Token::new(span(30, 31), NumberLiteral("01".into())),
            Token::new(span(31, 31), EndOfInput),
        ],
    );
}

#[test]
fn lex_number_literals_with_separators() {
    assert_ok_token(
        lex(r#"334 1'000what 3'2'2'1"" 500 10"" -23"#),
        vec![
            Token::new(span(1, 3), NumberLiteral("334".into())),
            Token::new(span(5, 9), NumberLiteral("1000".into())),
            Token::new(span(10, 13), Identifier("what".into())),
            Token::new(span(15, 21), NumberLiteral("3221".into())),
            Token::new(span(22, 23), TextLiteral(Ok(String::new()))),
            Token::new(span(25, 27), NumberLiteral("500".into())),
            Token::new(span(29, 30), NumberLiteral("10".into())),
            Token::new(span(31, 32), TextLiteral(Ok(String::new()))),
            Token::new(span(34, 36), NumberLiteral("-23".into())),
            Token::new(span(36, 36), EndOfInput),
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
            Token::new(
                span(1, 15),
                TextLiteral(Ok("
    al
  pha"
                .into())),
            ),
            Token::new(span(15, 15), EndOfInput),
        ],
    );
}

#[test]
fn lex_unterminated_text_literal() {
    assert_ok_token(
        lex(r#""text message"#),
        vec![
            Token::new(span(1, 13), TextLiteral(Err(UnterminatedTextLiteral))),
            Token::new(span(13, 13), EndOfInput),
        ],
    );
}

#[test]
fn lex_single_quote() {
    assert_ok_token(
        lex("'"),
        vec![
            Token::new(span(1, 1), SingleQuote),
            Token::new(span(1, 1), EndOfInput),
        ],
    );
}

#[test]
fn lex_brackets() {
    assert_ok_token(
        lex("(( )( ))"),
        vec![
            Token::new(span(1, 1), OpeningRoundBracket),
            Token::new(span(2, 2), OpeningRoundBracket),
            Token::new(span(4, 4), ClosingRoundBracket),
            Token::new(span(5, 5), OpeningRoundBracket),
            Token::new(span(7, 7), ClosingRoundBracket),
            Token::new(span(8, 8), ClosingRoundBracket),
            Token::new(span(8, 8), EndOfInput),
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
            Token::new(span(1, 3), Illegal('\u{51FD}')),
            Token::new(span(4, 6), Illegal('\u{6570}')),
            Token::new(span(6, 6), EndOfInput),
        ],
    );
}

#[test]
fn bare_non_ascii_are_illegal_but_non_fatal() {
    assert_ok_token(
        lex(" 函数 function"),
        vec![
            Token::new(span(2, 4), Illegal('\u{51FD}')),
            Token::new(span(5, 7), Illegal('\u{6570}')),
            Token::new(span(9, 16), Identifier("function".into())),
            Token::new(span(16, 16), EndOfInput),
        ],
    );
}

#[test]
fn backticks_are_illegal() {
    assert_ok_token(
        lex("`"),
        vec![
            Token::new(span(1, 1), Illegal('`')),
            Token::new(span(1, 1), EndOfInput),
        ],
    );
}

#[test]
fn backticks_are_illegal_right_after_number_literal() {
    assert_ok_token(
        lex("1`"),
        vec![
            Token::new(span(1, 1), NumberLiteral("1".into())),
            Token::new(span(2, 2), Illegal('`')),
            Token::new(span(2, 2), EndOfInput),
        ],
    );
}

#[test]
fn tabs_are_illegal() {
    assert_ok_token(
        lex("\t\t"),
        vec![
            Token::new(span(1, 1), Illegal('\t')),
            Token::new(span(2, 2), Illegal('\t')),
            Token::new(span(2, 2), EndOfInput),
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
            Token::new(span(1, 5), Identifier("alpha".into())),
            Token::new(span(7, 8), Punctuation("#?".into())),
            Token::new(span(9, 9), Semicolon(Provenance::Lexer)),
            Token::new(span(10, 12), NumberLiteral("100".into())),
            Token::new(span(14, 15), Identifier("it".into())),
            Token::new(span(16, 17), Semicolon(Provenance::Lexer)),
            Token::new(span(18, 23), TextLiteral(Ok("moot".into()))),
            Token::new(span(23, 23), EndOfInput),
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
            Token::new(span(1, 5), Identifier("start".into())),
            Token::new(span(7, 12), Identifier("middle".into())),
            Token::new(span(18, 20), Identifier("end".into())),
            Token::new(span(21, 21), Semicolon(Provenance::Lexer)),
            Token::new(span(22, 42), TextLiteral(Ok("anything\n    really".into()))),
            Token::new(span(48, 54), NumberLiteral("3291238".into())),
            Token::new(span(64, 69), Module),
            Token::new(span(83, 83), OpeningRoundBracket),
            Token::new(span(101, 101), ClosingRoundBracket),
            Token::new(span(102, 103), Semicolon(Provenance::Lexer)),
            Token::new(span(104, 108), Punctuation("$%&~~".into())),
            Token::new(span(114, 116), Punctuation(".!^".into())),
            Token::new(span(119, 120), Punctuation(r"\/".into())),
            Token::new(span(120, 120), EndOfInput),
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
            Token::new(span(1, 2), NumberLiteral("-0".into())),
            Token::new(span(8, 10), Identifier("off".into())),
            Token::new(span(16, 19), Identifier("side".into())),
            Token::new(span(25, 26), TextLiteral(Ok(String::new()))),
            Token::new(span(27, 27), Semicolon(Provenance::Lexer)),
            Token::new(span(28, 30), Punctuation("@@@".into())),
            Token::new(span(32, 32), At),
            Token::new(span(40, 43), Identifier("lvl1".into())),
            Token::new(span(53, 56), Identifier("lvl2".into())),
            Token::new(span(66, 69), Identifier("lvl2".into())),
            Token::new(span(75, 78), Identifier("lvl1".into())),
            Token::new(span(84, 84), NumberLiteral("1".into())),
            Token::new(span(85, 85), Semicolon(Provenance::Lexer)),
            Token::new(span(85, 85), EndOfInput),
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
            Token::new(span(1, 2), Of),
            // @Task we need to associate the token with a more useful span
            Token::new(span(3, 7), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 16), Identifier("something".into())),
            Token::new(span(17, 17), Semicolon(Provenance::Lexer)),
            Token::new(span(22, 25), Identifier("more".into())),
            // @Task we need to associate the token with a more useful span
            Token::new(span(26, 26), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(26, 26), Semicolon(Provenance::Lexer)),
            Token::new(span(27, 28), Of),
            // @Task we need to associate the token with a more useful span
            Token::new(span(29, 34), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(35, 38), NumberLiteral("1980".into())),
            // @Task we need to associate the token with a more useful span
            Token::new(span(39, 40), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(39, 40), Semicolon(Provenance::Lexer)),
            Token::new(span(41, 41), Punctuation(">".into())),
            Token::new(span(42, 43), Of),
            // @Task we need to associate the token with a more useful span
            Token::new(span(44, 48), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(49, 54), Module),
            Token::new(span(56, 57), Of),
            // @Task we need to associate the token with a more useful span
            Token::new(span(58, 66), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(67, 73), Identifier("CONTENT".into())),
            // @Task we need to associate the token with a more useful span
            Token::new(span(74, 78), ClosingCurlyBracket(Provenance::Lexer)),
            // @Bug wrong span?
            Token::new(span(74, 78), Semicolon(Provenance::Lexer)),
            Token::new(span(79, 80), Of),
            // @Task we need to associate the token with a more useful span
            Token::new(span(81, 89), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(90, 94), Punctuation(">>!<<".into())),
            // @Task we need to associate the token with a more useful span
            Token::new(span(95, 95), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(95, 95), Semicolon(Provenance::Lexer)),
            // // @Task we need to associate the token with a more useful span
            Token::new(span(95, 95), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(95, 95), Semicolon(Provenance::Lexer)),
            Token::new(span(95, 95), EndOfInput),
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
            Token::new(span(1, 2), Of),
            Token::new(span(3, 7), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 8), Identifier("a".into())),
            Token::new(span(18, 18), Identifier("b".into())),
            Token::new(span(19, 19), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(19, 19), Semicolon(Provenance::Lexer)),
            Token::new(span(19, 19), EndOfInput),
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
            Token::new(span(1, 2), Of),
            Token::new(span(3, 3), Semicolon(Provenance::Lexer)),
            Token::new(span(4, 5), Do),
            Token::new(span(6, 7), Semicolon(Provenance::Lexer)),
            Token::new(span(8, 9), Of),
            Token::new(span(10, 14), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(15, 16), Do),
            // Token::new(span(17, 17), Semicolon(Provenance::Lexer)), // @Beacon hmm
            Token::new(span(17, 17), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(17, 17), Semicolon(Provenance::Lexer)),
            Token::new(span(17, 17), EndOfInput),
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
            Token::new(span(1, 1), Semicolon(Provenance::Lexer)),
            Token::new(span(2, 3), Do),
            Token::new(span(5, 6), Identifier("it".into())),
            Token::new(span(7, 7), Semicolon(Provenance::Lexer)),
            Token::new(span(8, 9), Of),
            Token::new(span(10, 13), TextLiteral(Ok("it".into()))),
            Token::new(span(14, 14), Semicolon(Provenance::Lexer)),
            Token::new(span(14, 14), EndOfInput),
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
            Token::new(span(1, 1), OpeningRoundBracket),
            Token::new(span(2, 3), Of),
            // @Bug wrong span
            Token::new(span(4, 8), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(9, 10), Identifier("fo".into())),
            // @Question better span?
            Token::new(span(11, 11), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(11, 11), ClosingRoundBracket),
            Token::new(span(12, 12), Semicolon(Provenance::Lexer)),
            Token::new(span(13, 13), OpeningRoundBracket),
            Token::new(span(14, 15), Of),
            // @Bug wrong span
            Token::new(span(16, 20), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(21, 22), Identifier("fo".into())),
            Token::new(span(23, 23), Semicolon(Provenance::Lexer)),
            // @Question better span?
            Token::new(span(28, 28), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(28, 28), ClosingRoundBracket),
            Token::new(span(29, 29), Semicolon(Provenance::Lexer)),
            Token::new(span(29, 29), EndOfInput),
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
            Token::new(span(1, 2), Of),
            // @Bug wrong span
            Token::new(span(3, 7), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(8, 8), OpeningRoundBracket),
            Token::new(span(9, 9), Identifier("f".into())),
            Token::new(span(11, 11), OpeningSquareBracket),
            Token::new(span(12, 12), ClosingSquareBracket),
            Token::new(span(13, 13), ClosingRoundBracket),
            Token::new(span(14, 14), Semicolon(Provenance::Lexer)),
            Token::new(span(19, 24), Identifier("inside".into())),
            Token::new(span(25, 25), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(25, 25), Semicolon(Provenance::Lexer)),
            Token::new(span(25, 25), EndOfInput),
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
            Token::new(span(1, 1), OpeningRoundBracket),
            Token::new(span(2, 3), Of),
            Token::new(span(4, 8), OpeningCurlyBracket(Provenance::Lexer)),
            Token::new(span(9, 10), WideArrowRight),
            Token::new(span(11, 11), ClosingCurlyBracket(Provenance::Lexer)),
            Token::new(span(11, 11), ClosingRoundBracket),
            Token::new(span(17, 17), Equals),
            Token::new(span(19, 19), Dot),
            Token::new(span(20, 20), Semicolon(Provenance::Lexer)),
            Token::new(span(20, 20), EndOfInput),
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
