# Imminent Tasks

3. rewrite FFI logic using middleware/abstractions
  * dedicated `enum ffi::Value` e.g. containing `Bool(bool)` or `Nat(crate::Nat)`
  * middleware looks for a `Bool` declaration marked with the attribute `language` and
    reports a compiletime/runtime error if a foreign function returns a bool but it has not
    been registered
5. add effect infrastructure
7. work on case analysis
8. work on order-independence and recursion
10. modules

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
