#!/usr/bin/sh
//usr/bin/env rustc $0 ; ./generate ; rm ./generate ; exit
//! Extract the EBNF snippets from the parser.

// @Task either make this part of `build.rs` (accepting a flag) or another
// binary of the Cargo package lushui for improved portability and stability.

use std::{fs, path::Path};

const PARSER_PATH: &str = "../../src/parser.rs";
const OUTPUT_PATH: &str = "lushui.grammar";

const PREAMBLE: &str = "\
; The grammar of the Lushui programming language.
;
; Terminals are not plain text but lexems, those are tokens outputted by the lexer.
; This document is written in an EBNF flavor and
; generated from the documentation comments of the parser. Therefore, do not edit this file directly.
;
";

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let program_directory_path = Path::new(file!()).parent().unwrap();
    let parser = fs::read_to_string(program_directory_path.join(PARSER_PATH))?;

    let mut snippet_state = OutsideCodeSnippet;
    let mut ebnf_snippets = 0;
    let mut other_snippets = 0;
    let mut ebnf_definition_state = NotCollectedYet;

    let mut grammar = String::from(PREAMBLE);

    for line in parser.lines() {
        let line = line.trim_start();
        if let Some(line) = line.strip_prefix("///") {
            let line = line.strip_prefix(" ").unwrap_or(line);

            match line.strip_prefix("```") {
                Some(line) => {
                    snippet_state = if line.contains("ebnf") {
                        match snippet_state {
                            OutsideCodeSnippet => InsideEbnfSnippet,
                            InsideOtherCodeSnippet | InsideEbnfSnippet => unreachable!(),
                        }
                    } else {
                        match snippet_state {
                            OutsideCodeSnippet => InsideOtherCodeSnippet,
                            InsideOtherCodeSnippet => {
                                other_snippets += 1;
                                OutsideCodeSnippet
                            }
                            InsideEbnfSnippet => {
                                grammar.push('\n');
                                ebnf_snippets += 1;
                                OutsideCodeSnippet
                            }
                        }
                    };
                }
                None => {
                    if matches!(snippet_state, OutsideCodeSnippet | InsideOtherCodeSnippet) {
                        continue;
                    }

                    grammar += line;
                    grammar.push('\n');
                }
            }
        } else if let Some(line) = line.strip_prefix("//!") {
            if line.contains("| Notation") && line.contains("| Definition") {
                grammar.push(';');
                grammar += line;
                grammar.push('\n');
                match ebnf_definition_state {
                    NotCollectedYet => ebnf_definition_state = Collecting,
                    Collecting | Collected => unreachable!(),
                }
            } else {
                match ebnf_definition_state {
                    NotCollectedYet | Collected => {}
                    Collecting => {
                        grammar.push(';');
                        grammar += &line
                            .replace("&vert;", "|")
                            .replace("<code>", "`")
                            .replace("</code>", "`");
                        grammar.push('\n');
                    }
                }
            }
        } else {
            match ebnf_definition_state {
                Collected => {}
                NotCollectedYet => unreachable!(),
                Collecting => {
                    grammar += "\n";
                    ebnf_definition_state = Collected
                }
            }
        }
    }

    if !matches!(snippet_state, OutsideCodeSnippet) {
        unreachable!()
    }

    if !matches!(ebnf_definition_state, Collected) {
        unreachable!()
    };

    fs::write(program_directory_path.join(OUTPUT_PATH), grammar)?;

    println!(
        "processed {} EBNF snippets and skipped {} other snippets",
        ebnf_snippets, other_snippets
    );

    Ok(())
}

use SnippetState::*;

#[derive(Clone, Copy)]
enum SnippetState {
    OutsideCodeSnippet,
    InsideOtherCodeSnippet,
    InsideEbnfSnippet,
}

use EbnfDefinitionState::*;

#[derive(Clone, Copy)]
enum EbnfDefinitionState {
    NotCollectedYet,
    Collecting,
    Collected,
}
