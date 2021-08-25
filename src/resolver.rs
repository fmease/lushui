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

pub mod hir;
mod scope;

use crate::{
    ast,
    diagnostics::{Diagnostic, Reporter},
    entity::EntityKind,
    error::{Health, PossiblyErroneous, Result},
    lowered_ast::{self, AttributeKeys, AttributeKind},
    package::CrateStore,
    util::obtain,
};
use hir::{decl, expr, pat};
pub use scope::{
    CrateScope, DeBruijnIndex, DeclarationIndex, Exposure, FunctionScope, Identifier, Index,
    LocalDeclarationIndex, Namespace,
};
use scope::{OnlyModule, OnlyValue, RegistrationError, RestrictedExposure, ValueOrModule};
use std::rc::Rc;

const PROGRAM_ENTRY_IDENTIFIER: &str = "main";

/// Additional context for name resolution.
// @Task split context for start|finish resolve second does not store info about opacity
#[derive(Default, Debug)]
struct Context {
    // @Note if we were to rewrite/refactor the lowered AST to use indices for nested
    // declarations, then we could simply loop up the AST node and wouldn't need
    // `Opacity`
    parent_data_binding: Option<(LocalDeclarationIndex, Option<Opacity>)>,
}

#[derive(Debug)]
enum Opacity {
    Transparent,
    Opaque,
}

/// The state of the resolver.
pub struct Resolver<'a> {
    scope: &'a mut CrateScope,
    crates: &'a CrateStore,
    reporter: &'a Reporter,
}

impl<'a> Resolver<'a> {
    pub fn new(scope: &'a mut CrateScope, crates: &'a CrateStore, reporter: &'a Reporter) -> Self {
        Self {
            scope,
            crates,
            reporter,
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
    ) -> Result<hir::Declaration> {
        self.start_resolve_declaration(&declaration, None, Context::default())
            .map_err(|_| {
                std::mem::take(&mut self.scope.duplicate_definitions)
                    .into_iter()
                    .for_each(|(_, error)| Diagnostic::from(error).report(self.reporter));
            })?;

        self.scope.resolve_use_bindings(self.crates, self.reporter);

        // @Task @Beacon don't return early here
        self.scope
            .resolve_exposure_reaches(self.crates, self.reporter)?;

        let declaration = self.finish_resolve_declaration(declaration, None, Context::default());

        Result::from(self.scope.health).and(declaration)
    }

    /// Partially resolve a declaration merely registering declarations.
    ///
    /// This traverses all declarations and registers module-level bindings
    /// checking that they are only defined once per namespace.
    /// Use-bindings which refer to an unknown binding are marked as such
    /// to be resolved in the second, a minor pass.
    ///
    /// This also searches the program entry and stores it when it finds it.
    ///
    /// In contrast to [Self::finish_resolve_declaration], this does not actually return a
    /// new intermediate HIR because of too much mapping and type-system boilerplate
    /// and it's just not worth it memory-wise.
    /// For more on this, see [CrateScope::reobtain_resolved_identifier].
    fn start_resolve_declaration(
        &mut self,
        declaration: &lowered_ast::Declaration,
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Result<(), RegistrationError> {
        use lowered_ast::DeclarationKind::*;

        let exposure = match declaration.attributes.has(AttributeKeys::PUBLIC) {
            true => match declaration
                .attributes
                .get(|kind| obtain!(kind, AttributeKind::Public { reach } => reach))
            {
                Some(reach) => RestrictedExposure::Unresolved {
                    reach: reach.clone(),
                }
                .into(),
                None => Exposure::Unrestricted,
            },
            false => match module {
                // a lack of `@public` means private i.e. restricted to `self` i.e. `@(public self)`
                Some(module) => RestrictedExposure::Resolved { reach: module }.into(),
                None => Exposure::Unrestricted,
            },
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
                        self.scope.program_entry = Some(Identifier::new(
                            self.scope.global_index(index),
                            value.binder.clone(),
                        ));
                    }
                }
            }
            Data(data) => {
                // there is always a root module
                let module = module.unwrap();

                // @Task don't return early, see analoguous code for modules
                let namespace = self.scope.register_binding(
                    data.binder.clone(),
                    exposure,
                    EntityKind::untyped_data_type(),
                    Some(module),
                )?;

                if let Some(constructors) = &data.constructors {
                    // @Task awkward API: Result <-> Result<(), RegistrationError>
                    let health =
                        constructors
                            .iter()
                            .fold(Health::Untainted, |health, constructor| {
                                let opacity =
                                    match declaration.attributes.has(AttributeKeys::OPAQUE) {
                                        true => Opacity::Opaque,
                                        false => Opacity::Transparent,
                                    };

                                health
                                    & self
                                        .start_resolve_declaration(
                                            constructor,
                                            Some(module),
                                            Context {
                                                parent_data_binding: Some((
                                                    namespace,
                                                    Some(opacity),
                                                )),
                                            },
                                        )
                                        .map_err(|_| ())
                            });
                    return Result::from(health).map_err(|()| RegistrationError::Unrecoverable);
                }
            }
            Constructor(constructor) => {
                // there is always a root module
                let module = module.unwrap();
                let (namespace, module_opacity) = context.parent_data_binding.unwrap();

                let exposure = match module_opacity.unwrap() {
                    Opacity::Transparent => self.scope.get(namespace).exposure.clone(),
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

                // @Note awkward API
                let health =
                    submodule
                        .declarations
                        .iter()
                        .fold(Health::Untainted, |health, declaration| {
                            health
                                & self
                                    .start_resolve_declaration(
                                        declaration,
                                        Some(index),
                                        Context::default(),
                                    )
                                    .map_err(|_| ())
                        });
                return Result::from(health).map_err(|()| RegistrationError::Unrecoverable);
            }
            Use(use_) => {
                // there is always a root module
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
        module: Option<LocalDeclarationIndex>,
        context: Context,
    ) -> Result<hir::Declaration> {
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

                let binder = self.scope.reobtain_resolved_identifier::<OnlyValue>(
                    &value.binder,
                    module,
                    self.crates,
                );

                decl! {
                    Value {
                        declaration.attributes,
                        declaration.span;
                        type_annotation: type_annotation?,
                        expression: expression?,
                        binder,
                    }
                }
            }
            Data(data) => {
                let module = module.unwrap();

                let type_annotation =
                    self.resolve_expression(data.type_annotation, &FunctionScope::Module(module));

                // @Beacon @Question wouldn't it be great if that method returned a
                // LocalDeclarationIndex instead of an Identifier?
                // or maybe even a *LocalIdentifier?
                let binder = self.scope.reobtain_resolved_identifier::<OnlyValue>(
                    &data.binder,
                    module,
                    self.crates,
                );

                let constructors = data.constructors.map(|constructors| {
                    constructors
                        .into_iter()
                        .map(|constructor| {
                            self.finish_resolve_declaration(
                                constructor,
                                Some(module),
                                Context {
                                    parent_data_binding: Some((
                                        self.scope
                                            .local_index(binder.declaration_index().unwrap())
                                            .unwrap(),
                                        None,
                                    )),
                                },
                            )
                        })
                        .collect()
                });

                decl! {
                    Data {
                        declaration.attributes,
                        declaration.span;
                        constructors: constructors.transpose()?,
                        type_annotation: type_annotation?,
                        binder,
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
                    self.crates,
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
                    // unwrap: could only ever be non-local if the binder was a use-binding
                    // but it is module binding
                    Some(module) => self
                        .scope
                        .local_index(self.scope.reobtain_resolved_identifier::<OnlyModule>(
                            &submodule.binder,
                            module,
                            self.crates,
                        ))
                        .unwrap(),
                    None => self.scope.root(),
                };

                let mut health = Health::Untainted;

                // @Note awkward API
                let declarations = submodule
                    .declarations
                    .into_iter()
                    .flat_map(|declaration| {
                        let declaration = self.finish_resolve_declaration(
                            declaration,
                            Some(index),
                            Context::default(),
                        );
                        if declaration.is_err() {
                            health.taint();
                        }
                        declaration
                    })
                    .collect();

                Result::from(health)?;

                decl! {
                    Module {
                        declaration.attributes,
                        declaration.span;
                        binder: Identifier::new(self.scope.global_index(index), submodule.binder),
                        file: submodule.file,
                        declarations,
                    }
                }
            }
            Use(use_) => {
                let module = module.unwrap();

                let binder = Identifier::new(
                    self.scope.reobtain_resolved_identifier::<ValueOrModule>(
                        &use_.binder,
                        module,
                        self.crates,
                    ),
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
    ) -> Result<hir::Expression> {
        use lowered_ast::ExpressionKind::*;

        let expression = match expression.kind {
            PiType(pi) => {
                let domain = self.resolve_expression(pi.domain.clone(), scope);
                let codomain = match pi.parameter.clone() {
                    Some(parameter) => self.resolve_expression(
                        pi.codomain.clone(),
                        &scope.extend_with_parameter(parameter),
                    ),
                    None => self.resolve_expression(pi.codomain.clone(), scope),
                };

                return Ok(expr! {
                    PiType {
                        expression.attributes,
                        expression.span;
                        domain: domain?,
                        codomain: codomain?,
                        explicitness: pi.explicitness,
                        aspect: pi.aspect,
                        parameter: pi.parameter.clone()
                            .map(|parameter| Identifier::new(Index::DeBruijnParameter, parameter.clone())),
                    }
                });
            }
            Application(application) => {
                let callee = self.resolve_expression(application.callee.clone(), scope);
                let argument = self.resolve_expression(application.argument.clone(), scope);

                return Ok(expr! {
                    Application {
                        expression.attributes,
                        expression.span;
                        callee: callee?,
                        argument: argument?,
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
                    binder: scope.resolve_binding(&binding.binder, self.scope, self.crates,  self.reporter)?,
                }
            },
            Lambda(lambda) => {
                let parameter_type_annotation = lambda
                    .parameter_type_annotation
                    .clone()
                    .map(|type_| self.resolve_expression(type_, scope));
                let body_type_annotation = lambda.body_type_annotation.clone().map(|type_| {
                    self.resolve_expression(
                        type_,
                        &scope.extend_with_parameter(lambda.parameter.clone()),
                    )
                });
                let body = self.resolve_expression(
                    lambda.body.clone(),
                    &scope.extend_with_parameter(lambda.parameter.clone()),
                );

                expr! {
                    Lambda {
                        expression.attributes,
                        expression.span;
                        parameter: Identifier::new(Index::DeBruijnParameter, lambda.parameter.clone()),
                        parameter_type_annotation: parameter_type_annotation.transpose()?,
                        body_type_annotation: body_type_annotation.transpose()?,
                        body: body?,
                        explicitness: lambda.explicitness,
                        laziness: lambda.laziness,
                    }
                }
            }
            UseIn => {
                return Err(Diagnostic::unimplemented("use/in expression")
                    .primary_span(&expression)
                    .report(self.reporter));
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

        Ok(expression)
    }

    fn resolve_pattern(
        &mut self,
        pattern: lowered_ast::Pattern,
        scope: &FunctionScope<'_>,
    ) -> Result<(hir::Pattern, Vec<ast::Identifier>)> {
        use lowered_ast::PatternKind::*;

        let mut binders = Vec::new();

        let pattern = match pattern.kind.clone() {
            Number(number) => pat! { Number(pattern.attributes, pattern.span; number) },
            Text(text) => pat! { Text(pattern.attributes, pattern.span; text) },
            Binding(binding) => pat! {
                Binding {
                    pattern.attributes,
                    pattern.span;
                    binder: scope.resolve_binding(&binding.binder, self.scope, self.crates,  self.reporter)?,
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
                let callee = self.resolve_pattern(deapplication.callee.clone(), scope);
                let argument = self.resolve_pattern(deapplication.argument.clone(), scope);

                let (callee, mut callee_binders) = callee?;
                let (argument, mut argument_binders) = argument?;

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

macro unrc($compound:ident.$projection:ident) {
    Rc::try_unwrap($compound)
        .map(|compound| compound.$projection)
        .unwrap_or_else(|compound| compound.$projection.clone())
}
