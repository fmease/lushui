# Language Updates/Proposals

## 12019-10-13

* Implicit parameters/arguments no longer embraced in `{` and `}` but in `(,` and `)`
* Built-in types (text, int, ..) are keywords (`'Text`, `'Int`, ...) because they have
  literals. The literals could not exist without the types

## 12019-10-07

* This is a proposal: Syntactically, we are able to allow **type annotation expressions**
  which are notated with an infix colon. The thing is, it gets unintuitive with the syntax
  of pi literals. We might accept this cost:
  `(A: 'Type) -> Int` does **not** mean type annotation inside a pi literal.
  To express the latter, you need to add extra parenthesis:
  `((A: 'Type))`-> Int`. For now, the parsing order is kept different

## 12019-06-03

* Alternative function application syntax with different precedence than the normal
  one using the semicolon `;`. E.g. `foo (not; bar x); ban 8` instead of `foo (not (bar x)) (ban 8)`
* Documentation comment prefixed by `;;`, normal comment by `;;;`
* Naming convention: Snake case e.g. `a_long_identifier`. Types, type constructors and modules use
  strict title-cased snake-case, e.g. `A_Long_Identifier` where the tail of acronyms don't have to be lower case,
  e.g. `XML_HTTP_Request`. Some exceptions maybe, e.g. `Nat32` instead of `Nat_32`?
  Also, optionally primes at the end.
* A new proposal for named arguments: By default, cannot be named. If `'let` added in front of
  parameter list, it gets public e.g. `'let f ('let x: Int)` vs. `'let f (x: Int)`
  Drawbacks: Another datum to assciate with the function type; diverging API designs: Some people are going
  to prefer unnamed, some named. Syntactic clutter. Complexity.
