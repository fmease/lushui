# TASKS

## Grammar

* Write it down "fully"

## Lexer

* Implement text literals
* Implement basic number literals
* Implement number literals fully (nat, int, frac, float, complex)
* Improve indentation logic (issue: consecutive dedent/linebreak/indent)
* Write unit tests for indentation, punctuation, reserved punctuation,
  literals, documentation comments, comments

## Parser

* If possible, get rid of the clones in `Context::token` (and thus also `expect` and `consume`)
* "Fully" define optional line breaks (+ indentation) and try to implement them
  e.g. line break after sigle parameter
* effin parse expressions
