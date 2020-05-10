# Imminent Tasks

* documentation!
* test infrastructure: testing `test/*.lushui`-files for certain criteria specifiable
  in the first line of the respective files
* add constructor `|`-syntax
* add effect infrastructure
  * add foreign data type `Effect: Type -> Type` with foreign bindings `pure`,
    `bind`, `read-line` and `print-line`
  * recognize `main: Effect Unit`
  * define effect runner
  * have effect identifiers
* work on case analysis
  * update syntax of patterns and case analysis
  * port to debruijn
  * the rest
* modules
  * implement more complicated use-declarations (multiple binders, self, as)
    * implement symbols as identifiers 
  * work on order-independence and recursion
  * implement exposure lists and privacy
  * design and implement namespaced constructors
  * improve error messages of the resolver
* advanced type inference (!!!)
* design and implement records
* FFI
  * add infrastructure for polymorphic foreign functions (necessary for `panic`, effects, …)
  * check for FFI-compability at foreign declarations
  * implement higher-order foreign functions
* fix all the bugs related to substitutions (!!!)
* fix `(a: A) -> B` smh not equalling `A -> B` anymore
* print full path on type mismatch (and the like)
* pretty-print HIR (way fewer parens)
* spans
  * span information for type errors (very likely needs a second error type)
  * be able to format multi-line spans
* pass a context to more and more functions containing command line arguments, source map
  and stuff
* fix let/in (parsing)
* update VS Code grammar
* fix the ugly functions in the parser which lead to bad error messages
* drastically improve error message for missing type annotations for declarations in the parser
* finally check what early EOI do to the parser instead of line breaks and the like
* figure out how to handle multiple errors better
* correctly parse any kind of indented expressions
  * there are still some bugs
* parse more possible indentations (eg. after parameter groups)
* replace as many signatures taking `Expression`s with `&Expression`s. this will reduce clones I
  think because most functions don't need ownership and if it does, we can just cheaply clone the Rc.
  this is useful for lifetime stuff e.g. `ModuleScope::constructors` should take an `&Expression` to
  be able to return `&[Identifier]` instead of an expensive clone for `Vec<Identifier>`
* split `interpreter.rs` into type checker and interpreter
* CLI
  * add subcommands:
    * `check` (type check only)
    * `run` (type check and run program entry)
    * `highlight`
      * format the AST to HTML with CSS classes
      * add lexer option to keep comments
      * the first versions won't stay true to source and will act like a very simple
        code formatter but that's actually very good!
      * advanced highlighting: mark binders ("binding occurrens") different from referenced binders ("applied occurrens")
      * maybe generalize this to also be able to print JSON, so we can inspect a complex ast with firefox's JSON viewer
