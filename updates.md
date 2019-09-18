# Language Updates

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
