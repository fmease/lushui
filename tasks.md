# Imminent Tasks

1. swap to new syntax (mainly identifiers)
2. parse attributes and improve parser, replace foreign declarations
3. rewrite FFI logic using middleware/abstractions
  * dedicated `enum ffi::Value` e.g. containing `Bool(bool)` or `Nat(crate::Nat)`
  * middleware looks for a `Bool` declaration marked with the attribute `language` and
    reports a compiletime/runtime error if a foreign function returns a bool but it has not
    been registered
4. add foreign data declarations
5. add effect infrastructure
6. improve structure of lexer
7. work on case analysis
8. work on order-independence and recursion
