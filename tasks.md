# Tasks

## Minimum Amount of Features for (Public) 0.2.0?

* bidirectional type-checking (with unification)
* implicit arguments
* pattern deapplication (if GADTs don't work that's fine for this version)
* compilation to bytecode
* VM with garbage collection
* polishment: no obvious crashes
  * one is in the parser for invalid paths
  * one is in the typer: some out-of-order declarations

## For 0.3.0

* simple effects with `Effect: Type -> Type`

## Imminent / Mixed Bag

* add mode where the whole language+ecosystem builds upon a object-capability model
* if requested output how many in-order and how many out-of-order declarations where
  processed by the name resolver and separately by the typer
* implement implicit arguments
* implement bidirectional type-checking
* compilation
  * compile expressions and declarations to bytecode
  * define the bytecode (stack-based, has disk/file representation not only memory representation)
  * erase types
  * write a vm for the bytecode
* parse underscores as binders
* parse rune literals (e.g. `"X"#Rune`)
* make registration of foreign functions module-aware (on the Rust side)
* then, add several functions for the newly added number types
* bytecode vm
  * create bytecode
  * compile to bytecode
  * create vm
* add lushui to SCC's `languages.json` and build custom scc against it,
  provide a script to add the entry to the file (to be included in the repository)
  alternative: switch to cloc where you can just add a `xxx.cloc` file in you project root
* implement records
* allow "trailing dashes" in identifiers
* documentation of the interpreter!
* add effect infrastructure
  * add foreign data type `Effect: Type -> Type` with foreign bindings `pure`,
    `bind`, `read-line` and `print-line`
  * recognize `main: Effect Unit`
  * define effect runner
  * have effect identifiers
* work on case analysis (!!!)
* modules
  * implement exposure lists and privacy
* advanced type inference (!!!)
* FFI
  * add infrastructure for polymorphic foreign functions (necessary for `panic`, effects, …)
  * check for FFI-compability at foreign declarations
  * implement higher-order foreign functions
* fix all the bugs related to substitutions (!!!)
* pretty-print HIR (way fewer parens)
* fix the ugly functions in the parser which lead to bad error messages
* indentation
  * allow indentation by a multiple of 4 (e.g. 8), the parser should handle invalid stuff
    this solves issues where there are empty lines at the beginning of a module body
    * update: maybe the lexer can handle it (e.g. not adding anymore indenation(dedent) in some
      special cases)
  * correctly parse any kind of indented expressions
    * there are still some bugs
  * parse more possible indentations (eg. after parameter groups)
* replace as many signatures taking `Expression`s with `&Expression`s. this will reduce clones I
  think because most functions don't need ownership and if it does, we can just cheaply clone the Rc.
  this is useful for lifetime stuff e.g. `ModuleScope::constructors` should take an `&Expression` to
  be able to return `&[Identifier]` instead of an expensive clone for `Vec<Identifier>`
* CLI
  * add subcommands:
    * `highlight`
      * format the AST to HTML with CSS classes
      * need a losless AST first
      * the first versions won't stay true to source and will act like a very simple
        code formatter but that's actually very good!
      * advanced highlighting: mark binders ("binding occurrens") different from referenced binders ("applied occurrens")
      * maybe generalize this to also be able to print JSON, so we can inspect a complex ast with firefox's JSON viewer
