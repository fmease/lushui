# TASKS

Meta note: This file is not really used and up to date with my current ever-changing plans.

## Roadmap (One of Many I Made)

* parse optional indentation in let-declarations, more specifically, right after `=`
* parse case/of (or a temporary version of it)
* implement some basic form of case/of (goal: be able to write `not: Bool -> Bool` in Lushui!)
* implement basic recursion (and out-of-order declarations if it goes hand-in-hand) (goal: be able to write: `length: (A: 'Type) -> List A -> 'Nat` in Lushui!)
* if not implemented when implementing recursion, implement out-of-order (module local) declarations now!
* (**can be done anytime**) improve how we store scopes/environments and how we substitute (use debruijn-indeces or so)

## IMPORTANT

* drastically improve scopes, how we store bindings. we cannot just deep-copy hashmaps and expressions around!!!
  that's soooooooo expensive!!
* complete instance checking (done, but does not check whether stuff is specialized or contains existentials)

## Lexer

* keep a bracket stack for early error reporting
* Implement text literals
* Implement number literals fully (Nat, Int, Frac, Float, Complex)
* Improve indentation logic (issue: consecutive dedent/linebreak/indent)
* Write unit tests

## Parser

* If possible, get rid of the clones in `Context::token` (and thus also `expect` and `consume`)
* "Fully" define optional line breaks (+ indentation) and try to implement them
  e.g. line break after sigle parameter
* parse documentation comments
* try reducing the amount of `Context::reflect`s
* annotate each parser fn with grammar rules

## Error Reporting

* implement a more solid code snippet preview
* refactor the `error.rs` module: better abstractions, structure and API
