# Imminent Tasks

* rewrite FFI logic using middleware/abstractions
* add effect infrastructure
  * add foreign data type `Effect: Type -> Type` with foreign bindings `pure-effect`,
    `bind-effect`, `read-line` and `print-line`
  * recognize `main: Effect Unit`
  * define effect runner
  * have effect identifiers
* work on case analysis
  * port to debruijn
  * the rest
* work on order-independence and recursion
* modules
* advanced type inference

## Smaller

* span information for type errors (very likely needs a second error type)
* implement `Option` as inherent type, add `substract: Nat -> Nat -> Option Nat` and
  `divide: Nat -> Nat -> Option Nat`
* implement polymorphic foreign bindings to allow `panic` again
* be able to format multi-line spans
* pretty-print HIR (way fewer parens)
* refactor HIR's patterns to new system
* command line arguments
* pass a context to more and more functions containing command line arguments, source map
  and stuff
* fix let/in (parsing)
* update VS Code grammar
* fix the ugly functions in the parser which lead to bad error messages
* finally check what early EOI do to the parser instead of line breaks and the like
* figure out how to handle multiple errors better
* count total number of thrown errors in main
* use program flags to control whether to print tokens, AST, HIR, …
* test dedentation/indentation-filtering
* correctly parse any kind of indented expressions
* parse more possible indentations (eg. after parameter groups)

## FFI

* in `Expression::evaluate` under `Application`, if `callee` is a `Binding` and foreign,
  return a `ForeignApplication`. do **not** try to evaluate this. just let it be like
  explicit substitutions `Substitution`
* in `evaluate` under `Application`, if `callee` is a `Foreign Application`, add the `argument`
  to the foreign application
* in `evaluate` under `ForeignApplication`, if the arity does not match, return it as it
  (it stays neutral for a bit), otherwise check if all arguments (of type `Expression`) can be
  converted into a `ffi::Value`. if there is exists **one** argument which cannot, return the
  whole foreign application as-is (stuff should progress nonetheless), otherwise apply the
  foreign function `ForeignFunction` to the argument list
* in `Declaration::type_check_and_evaluate` under `Value`, if the declaration is foreign,
  **verify** that its (evaluated!) type is _ffi-compatible_
* a type is _ffi-compatible_ (`is_ffi_compatible`) if
  * it is a pi-type or `Type` or
  * foreign or
  * inherent
  unevaluated `case/of` **will** throw for example
  same with parameters (from pi-types)
* in `try_apply_foreign_binding` (or however we will rename/call it), skip arguments of type `Type`,
  maybe do some other stuff related to "monomorphization"/parameterization

## Style

* replace as many signatures taking `Expression`s with `&Expression`s. this will reduce clones I
  think because most functions don't need ownership and if it does, we can just cheaply clone the Rc.
  this is useful for lifetime stuff e.g. `ModuleScope::constructors` should take an `&Expression` to
  be able to return `&[Identifier]` instead of an expensive clone for `Vec<Identifier>`
