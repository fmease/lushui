//! Name resolver.
//!
//! This module is not yet used. It is planned to run between the parser and the type checker/interpreter.
//! The question remains whether it should operate on the AST produced by the parser and output the HIR (which
//! will differ from the current one) or whether it should act on a DesugaredAST: Today's HIR.
//!
//! ## Future Features
//!
//! * resolve package imports (package declarations)
//! * resolve module imports (file system module declarations)
//! * resolve paths to an identifier consisting of a package-unique definiton identifier
//!   (which is mappable to the definition-site span) and a usage-site span
//! * resolve out-of-order declarations
//! * resolve recursive (value) declarations and prepare the resulting bindings
//!   for the type checker which should be able to type-check recursive functions
//!   and (!) equi-recursive types)
//! * gracefully handle cyclic dependencies (illegal module recursion)
//! * handle module privacy (notably restricted exposure and good error messages)
