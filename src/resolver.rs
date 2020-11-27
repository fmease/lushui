//! The name resolver.
//!
//! It traverses the desugared AST (aka desugar-HIR) and registers bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to resolver identifiers [Identifier] which
//! either links to a crate-local index [CrateIndex] or a Debruijn-index [DebruijnIndex]
//! respectively.
//!
//! It does two main passes and a (hopefully) small one for use declarations to support
//! out of order declarations.
//!
//! ## Future Features
//!
//! * handle module privacy (notably restricted exposure and good error messages)
//! * handle crate declarations

pub mod hir;
mod scope;

use crate::{
    ast,
    diagnostic::{Code, Diagnostic, Diagnostics, Results, Warn},
    entity::EntityKind,
    lowered_ast,
    support::{accumulate_errors, InvalidFallback, ManyErrExt, TransposeExt, TryIn},
};
use hir::{decl, expr, pat};
pub use scope::{
    CrateIndex, CrateScope, DebruijnIndex, FunctionScope, Identifier, Index, Namespace,
};
use scope::{OnlyValue, ValueOrModule};
use std::rc::Rc;

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

/// The state of the resolver.
pub struct Resolver<'a> {
    scope: &'a mut CrateScope,
    warnings: &'a mut Diagnostics,
    // @Task move into separate `Context` struct again to pass as an argument
    parent_data_binding: Option<CrateIndex>,
}

impl<'a> Resolver<'a> {
    pub fn new(scope: &'a mut CrateScope, warnings: &'a mut Diagnostics) -> Self {
        Self {
            scope,
            warnings,
            parent_data_binding: None,
        }
    }

    /// Resolve the names of a declaration.
    ///
    /// It performs three passes to resolve all possible out of order declarations.
    /// If the declaration passed is not a module, this function will panic as it
    /// requires a crate root which is defined through the root module.
    pub fn resolve_declaration(
        &mut self,
        declaration: lowered_ast::Declaration,
    ) -> Results<hir::Declaration> {
        self.start_resolve_declaration(&declaration, None)?;
        // @Bug creates fatal errors for use stuff (see tests/multiple-undefined1)
        self.scope.resolve_unresolved_uses()?;
        self.finish_resolve_declaration(declaration, None)
    }

    /// Partially resolve a declaration merely registering declarations.
    ///
    /// This traverses all declarations and registers module-level bindings
    /// checking that they are only defined once per namespace.
    /// Use bindings which refer to an unknown binding are marked as such
    /// to be resolved in the second, a minor pass.
    ///
    /// This also searches the program entry and stores it when it finds it.
    ///
    /// In contrast to [Self::finish_resolve_declaration], this does not actually return a
    /// new intermediate HIR because of too much mapping and type-system boilerplate
    /// and it's just not worth it memory-wise.
    fn start_resolve_declaration(
        &mut self,
        declaration: &lowered_ast::Declaration,
        module: Option<CrateIndex>,
    ) -> Results<()> {
        use lowered_ast::DeclarationKind::*;

        match &declaration.kind {
            Value(value) => {
                let module = module.unwrap();

                let index = self
                    .scope
                    .register_binding(value.binder.clone(), EntityKind::UntypedValue, Some(module))
                    .many_err()?;

                if self.scope.program_entry.is_none() && module == self.scope.root() {
                    if value.binder.as_str() == PROGRAM_ENTRY_IDENTIFIER {
                        self.scope.program_entry =
                            Some(Identifier::new(index, value.binder.clone()));
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                let namespace = self
                    .scope
                    .register_binding(
                        data.binder.clone(),
                        EntityKind::untyped_data_type(),
                        Some(module),
                    )
                    .many_err()?;

                if let Some(constructors) = &data.constructors {
                    constructors
                        .iter()
                        .map(|constructor| {
                            self.parent_data_binding = Some(namespace);
                            let declaration =
                                self.start_resolve_declaration(constructor, Some(module))?;
                            self.parent_data_binding = None;
                            Ok(declaration)
                        })
                        .collect::<Vec<_>>()
                        .transpose()?;
                }
            }
            // @Task register fields
            Constructor(constructor) => {
                let module = self.parent_data_binding.unwrap();

                // @Task don't return early, see analoguous code for modules
                let namespace = self
                    .scope
                    .register_binding(
                        constructor.binder.clone(),
                        EntityKind::untyped_constructor(),
                        Some(module),
                    )
                    .many_err()?;

                let mut errors = Diagnostics::default();

                let mut type_annotation = &constructor.type_annotation;

                while let lowered_ast::ExpressionKind::PiType(pi) = &type_annotation.kind {
                    if pi.is_field {
                        let parameter = pi.parameter.as_ref().unwrap();

                        if let Err(error) = self.scope.register_binding(
                            parameter.clone(),
                            EntityKind::UntypedValue,
                            Some(namespace),
                        ) {
                            errors.insert(error);
                        }
                    }
                    type_annotation = &pi.codomain;
                }

                errors.err_or(())?;
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self
                    .scope
                    .register_binding(submodule.binder.clone(), EntityKind::module(), module)
                    .many_err()?;

                submodule
                    .declarations
                    .iter()
                    .map(|declaration| self.start_resolve_declaration(declaration, Some(index)))
                    .collect::<Vec<_>>()
                    .transpose()?;
            }
            Use(use_) => {
                let module = module.unwrap();

                let binder = use_
                    .binder
                    .as_ref()
                    .ok_or_else(|| {
                        Diagnostic::error()
                            .with_code(Code::E025)
                            .with_message("`use` of bare `super` and `crate` disallowed")
                            .with_span(declaration)
                            .with_help("add a name to it with `as`")
                    })
                    .many_err()?;

                let index = self
                    .scope
                    .register_binding(binder.clone(), EntityKind::UnresolvedUse, Some(module))
                    .many_err()?;

                self.scope
                    .register_unresolved_use(index, use_.target.clone(), module);
            }
            Invalid => {}
        }

        Ok(())
    }

    /// Completely resolve a desugar-HIR declaration to a resolver-HIR declaration.
    ///
    /// Tries to resolve all expressions and patterns contained within all declarations
    /// and actually builds the new HIR.
    ///
    /// This is the second major pass (but the third in total including the middle minor
    /// use-declaration resolving one).
    fn finish_resolve_declaration(
        &mut self,
        declaration: lowered_ast::Declaration,
        module: Option<CrateIndex>,
    ) -> Results<hir::Declaration> {
        use lowered_ast::DeclarationKind::*;

        Ok(match declaration.kind {
            Value(value) => {
                let module = module.unwrap();

                let type_annotation =
                    self.resolve_expression(value.type_annotation, &FunctionScope::Module(module));

                let expression = value
                    .expression
                    .map(|expression| {
                        self.resolve_expression(expression, &FunctionScope::Module(module))
                    })
                    .transpose();

                let binder = self
                    .scope
                    .resolve_identifier::<OnlyValue>(&value.binder, module)
                    .many_err()?;

                let (type_annotation, expression) =
                    accumulate_errors!(type_annotation, expression)?;

                decl! {
                    Value {
                        declaration.attributes,
                        declaration.span;
                        binder,
                        type_annotation,
                        expression,
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                let type_annotation =
                    self.resolve_expression(data.type_annotation, &FunctionScope::Module(module));

                let binder = self
                    .scope
                    .resolve_identifier::<OnlyValue>(&data.binder, module)
                    .many_err()?;

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.parent_data_binding = Some(binder.crate_index().unwrap());
                            let declaration =
                                self.finish_resolve_declaration(constructor, Some(module))?;
                            self.parent_data_binding = None;
                            Ok(declaration)
                        })
                        .collect()
                });

                let (type_annotation, constructors) =
                    accumulate_errors!(type_annotation, constructors.transpose())?;

                decl! {
                    Data {
                        declaration.attributes,
                        declaration.span;
                        binder,
                        constructors,
                        type_annotation,
                    }
                }
            }
            Constructor(constructor) => {
                let module = module.unwrap();

                let type_annotation = self.resolve_expression(
                    constructor.type_annotation,
                    &FunctionScope::Module(module),
                )?;

                let binder = self
                    .scope
                    .resolve_identifier::<OnlyValue>(
                        &constructor.binder,
                        self.parent_data_binding.unwrap(),
                    )
                    .many_err()?;

                decl! {
                    Constructor {
                        declaration.attributes,
                        declaration.span;
                        binder,
                        type_annotation,
                    }
                }
            }
            Module(submodule) => {
                // @Note ValueOrModule too general @Bug this might lead to
                // values used as modules!! we should create OnlyModule
                let index = match module {
                    Some(module) => self
                        .scope
                        .resolve_identifier::<ValueOrModule>(&submodule.binder, module)
                        .many_err()?,
                    None => self.scope.root(),
                };

                let declarations = submodule
                    .declarations
                    .into_iter()
                    .map(|declaration| self.finish_resolve_declaration(declaration, Some(index)))
                    .collect::<Vec<_>>()
                    .transpose()?;

                decl! {
                    Module {
                        declaration.attributes,
                        declaration.span;
                        binder: Identifier::new(index, submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                }
            }
            Use(use_) => {
                let module = module.unwrap();
                let binder = use_.binder.unwrap();

                let index = self
                    .scope
                    .resolve_identifier::<ValueOrModule>(&binder, module)
                    .many_err()?;

                decl! {
                    Use {
                        declaration.attributes,
                        declaration.span;
                        binder: Some(Identifier::new(index, binder.clone())),
                        // @Temporary
                        target: Identifier::new(index, binder),
                    }
                }
            }
            Invalid => InvalidFallback::invalid(),
        })
    }

    // @Task @Beacon use Rc::try_unwrap more instead of clone
    fn resolve_expression(
        &mut self,
        expression: lowered_ast::Expression,
        scope: &FunctionScope<'_>,
    ) -> Results<hir::Expression> {
        use lowered_ast::ExpressionKind::*;

        let mut errors = Diagnostics::default();

        let expression = match expression.kind {
            PiType(pi) => {
                let (domain, codomain) = accumulate_errors!(
                    self.resolve_expression(pi.domain.clone(), scope),
                    match pi.parameter.clone() {
                        Some(parameter) => self.resolve_expression(
                            pi.codomain.clone(),
                            &scope.extend_with_parameter(parameter),
                        ),
                        None => self.resolve_expression(pi.codomain.clone(), scope),
                    },
                )?;

                return Ok(expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DebruijnParameter, parameter.clone())),
                        domain,
                        codomain,
                        explicitness: pi.explicitness,
                    }
                });
            }
            Application(application) => {
                let (callee, argument) = accumulate_errors!(
                    self.resolve_expression(application.callee.clone(), scope),
                    self.resolve_expression(application.argument.clone(), scope),
                )?;

                return Ok(expr! {
                    Application {
                        expression.attributes,
                        expression.span;
                        callee,
                        argument,
                        explicitness: application.explicitness,
                    }
                });
            }
            Type => expr! { Type { expression.attributes, expression.span } },
            Number(number) => expr! { Number(expression.attributes, expression.span; number) },
            Text(text) => expr! { Text(expression.attributes, expression.span; text) },
            Binding(binding) => expr! {
                Binding {
                    expression.attributes,
                    expression.span;
                    binder: scope.resolve_binding(&binding.binder, &self.scope).many_err()?,
                }
            },
            // @Task @Beacon @Beacon don't use try_in here: you don't need to: use
            // accumulate_err, the stuff here is independent! right??
            Lambda(lambda) => expr! {
                Lambda {
                    expression.attributes,
                    expression.span;
                    parameter: Identifier::new(Index::DebruijnParameter, lambda.parameter.clone()),
                    parameter_type_annotation: lambda.parameter_type_annotation.clone()
                        .map(|type_| self.resolve_expression(type_, scope)
                            .try_in(&mut errors)),
                    body_type_annotation: lambda.body_type_annotation.clone()
                        .map(|type_| self.resolve_expression(type_,
                            &scope.extend_with_parameter(lambda.parameter.clone()))
                            .try_in(&mut errors)),
                    body: self.resolve_expression(lambda.body.clone(),
                        &scope.extend_with_parameter(lambda.parameter.clone()))
                        .try_in(&mut errors),
                    explicitness: lambda.explicitness,
                }
            },
            UseIn => {
                return Err(Diagnostic::bug()
                    .with_message("use/in expression not fully implemented yet")
                    .with_span(&expression))
                .many_err()
            }
            CaseAnalysis(analysis) => {
                let subject = self.resolve_expression(analysis.subject.clone(), scope)?;
                let mut cases = Vec::new();

                for case in &analysis.cases {
                    let (pattern, binders) = self.resolve_pattern(case.pattern.clone(), scope)?;
                    let body = self.resolve_expression(
                        case.body.clone(),
                        &scope.extend_with_pattern_binders(binders),
                    )?;

                    cases.push(hir::Case { pattern, body });
                }

                expr! {
                    CaseAnalysis {
                        expression.attributes,
                        expression.span;
                        subject,
                        cases,
                    }
                }
            }
            Invalid => InvalidFallback::invalid(),
        };

        errors.err_or(expression)
    }

    fn resolve_pattern(
        &mut self,
        pattern: lowered_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Results<(hir::Pattern, Vec<ast::Identifier>)> {
        use lowered_ast::PatternKind::*;

        let mut binders = Vec::new();

        let pattern = match pattern.kind.clone() {
            Number(number) => pat! { Number(pattern.attributes, pattern.span; number) },
            Text(text) => pat! { Text(pattern.attributes, pattern.span; text) },
            Binding(binding) => pat! {
                Binding {
                    pattern.attributes,
                    pattern.span;
                    binder: scope.resolve_binding(&binding.binder, &self.scope).many_err()?,
                }
            },
            Binder(binder) => {
                binders.push(binder.binder.clone());
                pat! {
                    Binder {
                        pattern.attributes,
                        pattern.span;
                        binder: Identifier::new(Index::DebruijnParameter, unrc!(binder.binder)),
                    }
                }
            }
            Deapplication(deapplication) => {
                let ((callee, mut callee_binders), (argument, mut argument_binders)) = accumulate_errors!(
                    self.resolve_pattern(deapplication.callee.clone(), scope),
                    self.resolve_pattern(deapplication.argument.clone(), scope),
                )?;

                binders.append(&mut callee_binders);
                binders.append(&mut argument_binders);

                pat! {
                    Deapplication {
                        pattern.attributes,
                        pattern.span;
                        callee,
                        argument,
                    }
                }
            }
            Invalid => InvalidFallback::invalid(),
        };

        Ok((pattern, binders))
    }
}

impl Warn for Resolver<'_> {
    fn diagnostics(&mut self) -> &mut Diagnostics {
        &mut self.warnings
    }
}

macro unrc($compound:ident.$projection:ident) {
    Rc::try_unwrap($compound)
        .map(|compound| compound.$projection)
        .unwrap_or_else(|compound| compound.$projection.clone())
}
