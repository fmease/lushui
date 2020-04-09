# Imminent Tasks

3. rewrite FFI logic using middleware/abstractions
  * dedicated `enum ffi::Value` e.g. containing `Bool(bool)` or `Nat(crate::Nat)`
  * middleware looks for a `Bool` declaration marked with the attribute `language` and
    reports a compiletime/runtime error if a foreign function returns a bool but it has not
    been registered
5. add effect infrastructure
7. work on case analysis
8. work on order-independence and recursion
