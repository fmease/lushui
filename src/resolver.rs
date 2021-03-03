//! The name resolver.
//!
//! It traverses the [lowered AST](lowered_ast) and registers bindings
//! defined both at module-level using declarations and at function
//! and pattern level as parameters. Furthermore, it resolves all paths inside
//! expressions and patterns to [resolved identifiers](Identifier) which
//! links to a [crate-local index](CrateIndex) or a [de Bruijn index](DeBruijnIndex)
//! respectively.
//!
//! It does two main passes and a (hopefully) small one for use-declarations to support
//! out of order declarations.
//!
//! ## Future Features
//!
//! * crate system

pub mod hir;
mod scope;

use crate::{
    ast,
    diagnostics::{Diagnostic, Diagnostics, Results, Warn},
    entity::EntityKind,
    error::{accumulate_errors, obtain, ManyErrExt, PossiblyErroneous, TransposeExt, TryIn},
    lowered_ast::{self, AttributeKeys, AttributeKind},
};
use hir::{decl, expr, pat};
pub use scope::{
    CrateIndex, CrateScope, DeBruijnIndex, Exposure, FunctionScope, Identifier, Index, Namespace,
};
use scope::{OnlyModule, OnlyValue, RegistrationError, RestrictedExposure, ValueOrModule};
use std::{mem, rc::Rc};

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

/// Additional context for name resolution.
// @Task split context for start|finish resolve second does not store info about opacity
#[derive(Default)]
struct Context {
    // @Note if we were to rewrite/refactor the lowered AST to use indices for nested
    // declarations, then we could simply loop up the AST node and wouldn't need
    // `Opacity`
    parent_data_binding: Option<(CrateIndex, Option<Opacity>)>,
}

enum Opacity {
    Transparent,
    Opaque,
}

/// The state of the resolver.
pub struct Resolver<'a> {
    scope: &'a mut CrateScope,
    warnings: &'a mut Diagnostics,
}

impl<'a> Resolver<'a> {
    pub fn new(scope: &'a mut CrateScope, warnings: &'a mut Diagnostics) -> Self {
        Self { scope, warnings }
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
        // @Note awkward manual error propagation, very error prone
        // topic: horrible error handling APIs
        self.start_resolve_declaration(&declaration, None, Context::default())
            .map_err(|error| error.diagnostics(mem::take(&mut self.scope.duplicate_definitions)))?;

        self.scope.resolve_use_bindings();

        // @Task @Beacon don't return early here
        self.scope.resolve_exposure_reaches()?;

        let declaration = self
            .finish_resolve_declaration(declaration, None, Context::default())
            .try_in(&mut self.scope.errors);

        self.scope.errors.take().err_or(declaration)
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
    /// For more on this, see [CrateScope::resolve_identifier].
    fn start_resolve_declaration(
        &mut self,
        declaration: &lowered_ast::Declaration,
        module: Option<CrateIndex>,
        context: Context,
    ) -> Result<(), RegistrationError> {
        use lowered_ast::DeclarationKind::*;

        let exposure = if declaration.attributes.has(AttributeKeys::PUBLIC) {
            match declaration
                .attributes
                .get(|kind| obtain!(kind, AttributeKind::Public { reach } => reach))
            {
                Some(reach) => RestrictedExposure::Unresolved {
                    reach: reach.clone(),
                }
                .into(),
                None => Exposure::Unrestricted,
            }
        } else {
            match module {
                // no `@public` means private i.e. restricted to `self` i.e. `@(public self)`
                Some(module) => RestrictedExposure::Resolved { reach: module }.into(),
                None => Exposure::Unrestricted,
            }
        };

        match &declaration.kind {
            Value(value) => {
                let module = module.unwrap();

                let index = self.scope.register_binding(
                    value.binder.clone(),
                    exposure,
                    EntityKind::UntypedValue,
                    Some(module),
                )?;

                if self.scope.program_entry.is_none() && module == self.scope.root() {
                    if value.binder.as_str() == PROGRAM_ENTRY_IDENTIFIER {
                        self.scope.program_entry =
                            Some(Identifier::new(index, value.binder.clone()));
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let namespace = self.scope.register_binding(
                    data.binder.clone(),
                    exposure,
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                if let Some(constructors) = &data.constructors {
                    constructors
                        .iter()
                        .map(|constructor| {
                            let opacity = if declaration.attributes.has(AttributeKeys::OPAQUE) {
                                Opacity::Opaque
                            } else {
                                Opacity::Transparent
                            };

                            self.start_resolve_declaration(
                                constructor,
                                Some(module),
                                Context {
                                    parent_data_binding: Some((namespace, Some(opacity))),
                                },
                            )
                        })
                        .transpose()?;
                }
            }
            Constructor(constructor) => {
                let module = module.unwrap();
                let (namespace, module_opacity) = context.parent_data_binding.unwrap();

                let exposure = match module_opacity.unwrap() {
                    Opacity::Transparent => self.scope.bindings[namespace].exposure.clone(),
                    // as if a @(public super) was attached to the constructor
                    Opacity::Opaque => RestrictedExposure::Resolved { reach: module }.into(),
                };

                // @Task don't return early, see analoguous code for modules
                let namespace = self.scope.register_binding(
                    constructor.binder.clone(),
                    exposure,
                    EntityKind::untyped_constructor(),
                    Some(namespace),
                )?;

                let mut overall_result = Ok(());

                let mut type_annotation = &constructor.type_annotation;

                while let lowered_ast::ExpressionKind::PiType(pi) = &type_annotation.kind {
                    if pi.aspect.is_field() {
                        let parameter = pi.parameter.as_ref().unwrap();

                        overall_result = self
                            .scope
                            .register_binding(
                                parameter.clone(),
                                Exposure::Unrestricted, // @Temporary
                                EntityKind::UntypedValue,
                                Some(namespace),
                            )
                            .map(|_| ());
                    }
                    type_annotation = &pi.codomain;
                }

                overall_result?;
            }
            Module(submodule) => {
                // @Task @Beacon don't return early on error
                // @Note you need to create a fake index for this (an index which points to
                // a fake, nameless binding)
                let index = self.scope.register_binding(
                    submodule.binder.clone(),
                    exposure,
                    EntityKind::module(),
                    module,
                )?;

                submodule
                    .declarations
                    .iter()
                    .map(|declaration| {
                        self.start_resolve_declaration(declaration, Some(index), Context::default())
                    })
                    .transpose()?;
            }
            Use(use_) => {
                let module = module.unwrap();

                let index = self.scope.register_binding(
                    use_.binder.clone(),
                    exposure,
                    EntityKind::UnresolvedUse,
                    Some(module),
                )?;

                self.scope
                    .register_use_binding(index, use_.target.clone(), module);
            }
            Error => {}
        }

        Ok(())
    }

    /// Completely resolve a lowered declaration to a declaration of the HIR.
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
        context: Context,
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
                    .reobtain_resolved_identifier::<OnlyValue>(&value.binder, module);

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
                    .reobtain_resolved_identifier::<OnlyValue>(&data.binder, module);

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context {
                                    parent_data_binding: Some((
                                        binder.crate_index().unwrap(),
                                        None,
                                    )),
                                },
                            )
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

                let binder = self.scope.reobtain_resolved_identifier::<OnlyValue>(
                    &constructor.binder,
                    context.parent_data_binding.unwrap().0,
                );

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
                let index = match module {
                    Some(module) => self
                        .scope
                        .reobtain_resolved_identifier::<OnlyModule>(&submodule.binder, module),
                    None => self.scope.root(),
                };

                let declarations = submodule
                    .declarations
                    .into_iter()
                    .map(|declaration| {
                        self.finish_resolve_declaration(
                            declaration,
                            Some(index),
                            Context::default(),
                        )
                    })
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

                let binder = Identifier::new(
                    self.scope
                        .reobtain_resolved_identifier::<ValueOrModule>(&use_.binder, module),
                    use_.binder,
                );

                decl! {
                    Use {
                        declaration.attributes,
                        declaration.span;
                        binder: Some(binder.clone()),
                        target: binder,
                    }
                }
            }
            Error => PossiblyErroneous::error(),
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
                        explicitness: pi.explicitness,
                        aspect: pi.aspect,
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DeBruijnParameter, parameter.clone())),
                        domain,
                        codomain,
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
                    parameter: Identifier::new(Index::DeBruijnParameter, lambda.parameter.clone()),
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
                    laziness: lambda.laziness,
                }
            },
            UseIn => {
                return Err(
                    Diagnostic::unimplemented("use/in expression").with_primary_span(&expression)
                )
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
            Error => PossiblyErroneous::error(),
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
                        binder: Identifier::new(Index::DeBruijnParameter, unrc!(binder.binder)),
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
            Error => PossiblyErroneous::error(),
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
