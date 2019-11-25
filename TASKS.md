# TASKS

## IMPORTANT

* drastically improve scopes, how we store bindings. we cannot just deep-copy hashmaps and expressions around!!!
  that's soooooooo expensive!!
* complete instance checking

## Lexer

* keep a bracket stack for early error reporting
* re-model `TokenKind` without any payloads for uniform pattern matching inside the parser
* Implement text literals
* Implement basic number literals
* Implement number literals fully (Nat, Int, Frac, Float, Complex)
* Improve indentation logic (issue: consecutive dedent/linebreak/indent)
* Write unit tests

## Parser

* If possible, get rid of the clones in `Context::token` (and thus also `expect` and `consume`)
* "Fully" define optional line breaks (+ indentation) and try to implement them
  e.g. line break after sigle parameter
* parse documentation comments
* try reducing the amount of `Context::reflect`s
* propagate span information from lexer and use it for error reporting
* annotate each parser fn with grammar rules (almost done)
* parse constructors
* later, parse those niche language features:
  * `'of` in parameter list (inline exhaustive pattern match)
  * symbols, symbolic application

## HIR

## Error Reporting

* implement a more solid code snippet preview
* refactor the `error.rs` module: better abstractions, structure and API
