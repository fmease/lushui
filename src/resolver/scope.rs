//! Scope data structures used for name resolution.
//!
//! There are two types of scopes:
//!
//! * [CrateScope] for module-level bindings (may be out of order and recursive)
//!   i.e. bindings defined by declarations in modules, data types and constructors
//! * [FunctionScope] for bindings defined inside of expressions or functions (order matters)
//!   like parameters, let/in binders and case analysis binders
//!
//! The most important functions are [FunctionScope::resolve_binding] and [CrateScope::resolve_path]
//! (used by `resolve_binding`).
//!
//! The equivalent in the type checker is [crate::typer::interpreter::scope].

// @Task don't report similarily named *private* bindings!
// @Task recognize leaks of private types!

use crate::{
    ast::{self, Path},
    diagnostics::{Code, Diagnostic, Diagnostics, Result},
    entity::{Entity, EntityKind},
    format::{unordered_listing, AsDebug, Conjunction, DisplayWith, QuoteExt},
    smallvec,
    span::{Span, Spanning},
    typer::interpreter::{ffi, scope::Registration},
    HashMap, HashSet, SmallVec,
};
use indexed_vec::IndexVec;
use joinery::JoinableIterator;
use std::{cell::RefCell, cmp::Ordering, default::default, fmt, iter::once, mem::take};

type Bindings = IndexVec<CrateIndex, Entity>;

/// The crate scope for module-level bindings.
///
/// This structure is used not only by the name resolver but also the type checker.
#[derive(Default)]
pub struct CrateScope {
    pub(crate) program_entry: Option<Identifier>,
    /// All bindings inside of a crate.
    ///
    /// The first element must always be the root module.
    pub(crate) bindings: Bindings,
    /// For resolving out of order use-declarations.
    partially_resolved_use_bindings: HashMap<CrateIndex, PartiallyResolvedUseBinding>,
    /// Used for grouping circular bindings in diagnostics
    // @Beacon @Question can we smh unify this with `unresolved_uses`?
    partially_resolved_use_bindings_grouped: Vec<HashSet<CrateIndex>>,
    // @Temporary
    // set of the "owners"
    partially_resolved_exposure_reaches: HashSet<CrateIndex>,
    // @Temporary
    partially_resolved_exposure_reaches_grouped: Vec<HashSet<CrateIndex>>,
    /// For error reporting.
    pub(super) duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
    pub(super) errors: Diagnostics,
    pub(crate) ffi: ffi::Scope,
    // @Note this is very coarse-grained: as soon as we cannot resolve EITHER type annotation (for example)
    // OR actual value(s), we bail out and add this here. This might be too conversative (leading to more
    // "circular type" errors or whatever), we can just discriminate by creating sth like
    // UnresolvedThingy/WorlistItem { index: CrateIndex, expression: TypeAnnotation|Value|Both|... }
    // for the typer only!
    pub(crate) out_of_order_bindings: Vec<Registration>,
}

impl CrateScope {
    /// The crate root.
    ///
    /// Unbeknownst to the subdeclarations of the crate, it takes the name
    /// given by external sources, the crate name. Inside the crate, the root
    /// can only ever be referenced through the keyword `crate` (unless renamed).
    pub(super) fn root(&self) -> CrateIndex {
        CrateIndex(0)
    }

    /// Build a textual representation of the absolute path of a binding.
    ///
    /// This procedure always prepends `crate.` to every path.
    pub fn absolute_path(&self, index: CrateIndex) -> String {
        use crate::lexer::is_punctuation;

        let entity = &self.bindings[index];

        if let Some(parent) = entity.parent {
            let mut parent = self.absolute_path(parent);

            let parent_is_punctuation = is_punctuation(parent.chars().next_back().unwrap());

            if parent_is_punctuation {
                parent.push(' ');
            }

            parent.push('.');

            if entity.source.is_punctuation() && parent_is_punctuation {
                parent.push(' ');
            }

            parent += entity.source.as_str();
            parent
        } else {
            "crate".into()
        }
    }

    /// Register a binding to a given entity.
    ///
    /// Apart from actually registering, it mainly checks for name duplication.
    // @Note the way we handle errors here is a mess but it takes time and understanding
    // to design a better API meeting all/most of our requirements!
    pub(super) fn register_binding(
        &mut self,
        binder: ast::Identifier,
        exposure: Exposure,
        binding: EntityKind,
        namespace: Option<CrateIndex>,
    ) -> Result<CrateIndex, RegistrationError> {
        if let Some(namespace) = namespace {
            if let Some(&index) = self.bindings[namespace]
                .namespace()
                .unwrap()
                .bindings
                .iter()
                .find(|&&index| self.bindings[index].source == binder)
            {
                let previous = &self.bindings[index].source;

                self.duplicate_definitions
                    .entry(index)
                    .or_insert(DuplicateDefinition {
                        binder: previous.to_string(),
                        occurrences: smallvec![previous.span],
                    })
                    .occurrences
                    .push(binder.span);

                return Err(RegistrationError::DuplicateDefinition);
            }
        }

        let index = self.bindings.push(Entity {
            source: binder,
            kind: binding,
            exposure,
            parent: namespace,
        });

        if let Some(namespace) = namespace {
            self.bindings[namespace]
                .namespace_mut()
                .unwrap()
                .bindings
                .push(index);
        }

        Ok(index)
    }

    pub(super) fn register_use_binding(
        &mut self,
        index: CrateIndex,
        reference: Path,
        module: CrateIndex,
    ) {
        let previous = self
            .partially_resolved_use_bindings
            .insert(index, PartiallyResolvedUseBinding { reference, module });

        debug_assert!(previous.is_none());
    }

    /// Resolve a syntactic path given a namespace.
    fn resolve_path<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
    ) -> Result<Target::Output, ResolutionError> {
        self.resolve_path_with_origin::<Target>(
            path,
            namespace,
            namespace,
            IdentifierUsage::Unqualified,
        )
    }

    /// Resolve a syntactic path given a namespace with an explicit origin.
    fn resolve_path_with_origin<Target: ResolutionTarget>(
        &self,
        path: &Path,
        namespace: CrateIndex,
        origin_namespace: CrateIndex,
        usage: IdentifierUsage,
    ) -> Result<Target::Output, ResolutionError> {
        use ast::HangerKind::*;

        if let Some(hanger) = &path.hanger {
            let namespace = match hanger.kind {
                Crate => self.root(),
                Super => self.resolve_super(hanger, namespace)?,
                Self_ => namespace,
            };

            return if path.segments.is_empty() {
                Target::handle_bare_super_and_crate(hanger.span, namespace).map_err(Into::into)
            } else {
                self.resolve_path_with_origin::<Target>(
                    &path.tail(),
                    namespace,
                    origin_namespace,
                    IdentifierUsage::Qualified,
                )
            };
        }

        let index =
            self.resolve_first_segment(&path.segments[0], namespace, origin_namespace, usage)?;

        match path.to_simple() {
            Some(identifier) => {
                Target::resolve_simple_path(identifier, &self.bindings[index].kind, index)
                    .map_err(Into::into)
            }
            None => match self.bindings[index].namespace() {
                Some(_) => self.resolve_path_with_origin::<Target>(
                    &path.tail(),
                    index,
                    origin_namespace,
                    IdentifierUsage::Qualified,
                ),
                None if self.bindings[index].is_error() => {
                    // @Task add rationale
                    Ok(Target::output(index, path.last_identifier().unwrap()))
                }
                None => Err(value_used_as_a_namespace(&path.segments[0], &path.segments[1]).into()),
            },
        }
    }

    fn resolve_super(&self, hanger: &ast::Hanger, module: CrateIndex) -> Result<CrateIndex> {
        module.parent(self).ok_or_else(|| {
            Diagnostic::error()
                .with_code(Code::E021) // @Question use a dedicated code?
                .with_message("the crate root does not have a parent")
                .with_primary_span(hanger)
        })
    }

    /// Resolve the first identifier segment of a path.
    fn resolve_first_segment(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
        origin_namespace: CrateIndex,
        usage: IdentifierUsage,
    ) -> Result<CrateIndex, ResolutionError> {
        let index = self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .ok_or_else(|| ResolutionError::UnresolvedBinding {
                identifier: identifier.clone(),
                namespace,
                usage,
            })?;

        self.handle_exposure(index, identifier, origin_namespace)?;
        self.collapse_use_chain(index)
    }

    fn handle_exposure(
        &self,
        index: CrateIndex,
        identifier: &ast::Identifier,
        origin_namespace: CrateIndex,
    ) -> Result<(), ResolutionError> {
        eprintln!("handle_exposure index={index} ident={identifier}");

        let binding = &self.bindings[index];

        if let Exposure::Restricted(exposure) = &binding.exposure {
            // unwrap: root always has Exposure::Unrestricted
            let definition_site_namespace = binding.parent.unwrap();
            // @Beacon @Beacon @Task don't resolve here!! (no recursive call plz)
            // throw some new ResolutionError::UnresolvedExposureReach that
            // resolve_exposure_reaches can handle (to successfully detect circular
            // exposure reaches!)
            // let reach = exposure.resolve(definition_site_namespace, self)?;
            // @Bug this not fine-graned enough, I guess
            // @Note we need to differenciate between failed to resolve
            // exposure reach (for good) and not-yet-resolved
            let reach = match *exposure.borrow() {
                RestrictedExposure::Resolved { reach } => reach,
                RestrictedExposure::Unresolved { reach: _ } => {
                    return Err(ResolutionError::UnresolvedExposureReach { inquirer: index })
                }
            };

            if !self.is_allowed_to_access(origin_namespace, definition_site_namespace, reach) {
                // @Task add more information
                return Err(Diagnostic::error()
                    .with_code(Code::E029)
                    .with_message(format!(
                        "binding `{}` is private",
                        self.absolute_path(index)
                    ))
                    .with_primary_span(identifier)
                    .into());
            }
        }

        Ok(())
    }

    /// Indicate if the definition-site namespace can be accessed from the given namespace.
    fn is_allowed_to_access(
        &self,
        namespace: CrateIndex,
        definition_site_namespace: CrateIndex,
        reach: CrateIndex,
    ) -> bool {
        let access_from_same_namespace_or_below =
            namespace.some_ancestor_equals(definition_site_namespace, self);

        let access_from_above_in_reach = || namespace.some_ancestor_equals(reach, self);

        access_from_same_namespace_or_below || access_from_above_in_reach()
    }

    pub(super) fn resolve_exposure_reaches(&mut self) {}

    pub(super) fn _resolve_exposure_reaches(&mut self) {
        // @Note it's not necessary to store this as a field
        self.partially_resolved_exposure_reaches = self.bindings.indices().collect();
        self.partially_resolved_exposure_reaches_grouped = default();

        while !self.partially_resolved_exposure_reaches.is_empty() {
            let mut partially_resolved_exposure_reaches = HashSet::default();

            for &index in &self.partially_resolved_exposure_reaches {
                let binding = &self.bindings[index];
                // eprintln!(
                //     "resolve_exposure_reaches {{for}} binding.source={}",
                //     binding.source
                // );

                if let Exposure::Restricted(exposure) = &binding.exposure {
                    // unwrap: root always has Exposure::Unrestricted
                    let definition_site_namespace = binding.parent.unwrap();

                    use ResolutionError::*;

                    match exposure.resolve(definition_site_namespace, self) {
                        Ok(_) => {}
                        Err(UnresolvedExposureReach { inquirer }) => {
                            eprintln!("inserting partially resolved exposure reach index={index} inquirer={inquirer}");
                            partially_resolved_exposure_reaches.insert(index);

                            if let Some(indices) = self
                                .partially_resolved_exposure_reaches_grouped
                                .iter_mut()
                                .filter(|indices| indices.contains(&inquirer))
                                .next()
                            {
                                indices.insert(index);
                            } else {
                                let mut indices = HashSet::default();
                                indices.insert(index);
                                self.partially_resolved_exposure_reaches_grouped
                                    .push(indices);
                            }
                        }
                        Err(Unrecoverable(error)) => {
                            self.errors.insert(error);
                            continue;
                        }
                        Err(UnresolvedBinding { .. }) => todo!("unresolved binding"),
                        Err(UnresolvedUseBinding { .. }) => todo!("unresolved use binding"),
                    }
                }

                if let EntityKind::Use { reference } = binding.kind {
                    let reference = &self.bindings[reference];

                    if binding.exposure.compare(&reference.exposure, self)
                        == Some(Ordering::Greater)
                    {
                        self.errors.insert(
                        Diagnostic::error()
                            .with_code(Code::E009)
                            // @Question use absolute path?
                            .with_message(format!("re-export of the more private binding `{}`", reference.source))
                            .with_labeled_primary_span(&binding.source, "re-exporting binding with greater exposure")
                            .with_labeled_secondary_span(&reference.source, "re-exported binding with lower exposure")
                            .with_note(format!(
                                "expected the exposure of `{}` to be at most {} but it actually is {}",
                                binding.source,
                                reference.exposure.with(self),
                                binding.exposure.with(self),
                            )),
                    );
                    }
                }
            }

            // resolition stalled; therefore tere are circular exposure reaches
            if partially_resolved_exposure_reaches.len()
                == self.partially_resolved_exposure_reaches.len()
            {
                // self.errors.insert(
                //     Diagnostic::error()
                //         .with_message("found circular exposure reaches")
                //         .with_debug("only maybe"),
                // );
                let circular_bindings = take(&mut self.partially_resolved_exposure_reaches_grouped)
                    .into_iter()
                    .filter(|indices| {
                        indices
                            .iter()
                            .any(|index| partially_resolved_exposure_reaches.contains(index))
                    });

                let errors = circular_bindings
                    .map(|indices| -> Diagnostic {
                        indices
                            .iter()
                            .for_each(|&index| self.bindings[index].mark_as_error());

                        let reaches = unordered_listing(
                            indices
                                .iter()
                                .map(|&index| self.absolute_path(index).quote()),
                            Conjunction::And,
                        );

                        // @Task don't use the def-site spans, use the use-site spans
                        let spans = indices
                            .into_iter()
                            .map(|index| self.bindings[index].source.span);

                        // @Note on unresolved reaches we can still get the span but then
                        // we don't know which one...which of the segments is actually circular?
                        // in general, it isn't always the last one, right?
                        // let spans = indices.into_iter().map(|index| {
                        //     self.bindings[self.bindings[index].exposure.resolved_reach().unwrap()]
                        //         .source
                        //         .span
                        // });

                        Diagnostic::error()
                            // .with_code(Code::E024)
                            .with_message(format!("exposure reaches {reaches} are circular"))
                            .with_primary_spans(spans)
                            .with_debug("merely temporary")
                    })
                    .collect::<Vec<_>>();

                self.errors.extend(errors);
                return;
            }

            self.partially_resolved_exposure_reaches = partially_resolved_exposure_reaches;
        }

        // dbg!(&self.partially_resolved_exposure_reaches);
    }

    /// Find a similarly named binding in the same namespace.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier.
    fn find_similarly_named(&self, identifier: &str, namespace: &Namespace) -> Option<&str> {
        namespace
            .bindings
            .iter()
            .map(|&index| self.bindings[index].source.as_str())
            .find(|&other_identifier| is_similar(identifier, other_identifier))
    }

    /// Collapse chain of use bindings aka indirect uses.
    ///
    /// This is an invariant established to make things easier to reason about during resolution.
    fn collapse_use_chain(&self, index: CrateIndex) -> Result<CrateIndex, ResolutionError> {
        use EntityKind::*;

        match self.bindings[index].kind {
            UntypedValue | Module(_) | UntypedDataType(_) | UntypedConstructor(_) => Ok(index),
            Use { reference } => Ok(reference),
            UnresolvedUse => Err(ResolutionError::UnresolvedUseBinding { inquirer: index }),
            Error => Ok(index),
            _ => unreachable!(),
        }
    }

    /// Reobtain the resolved identifier.
    ///
    /// Used in [super::Resolver::finish_resolve_declaration], the last pass of the
    /// name resolver, to re-gain some information (the [Identifier]s) collected during the first pass.
    ///
    /// This way, [super::Resolver::start_resolve_declaration] does not need to return
    /// a new intermediate representation being a representation between the
    /// lowered AST and the HIR where all the _binders_ of declarations are resolved
    /// (i.e. are [Identifier]s) but all _bindings_ (in type annotations, expressions, …)
    /// are still unresolved (i.e. are [crate::ast::Identifier]s).
    ///
    /// Such an IR would imply writing a lot of boilerplate if we were to duplicate
    /// definitions & mappings or – if even possible – creating a totally complicated
    /// parameterized lowered AST with complicated traits having many associated types
    /// (painfully learned through previous experiences).
    // @Task add to documentation that this panics on unresolved and does not check exposure,
    // also it does not check the resolution target etc
    // @Task add that it may only fail if circular use bindings were found or a use
    // binding could not be resolved since `Self::resolve_use_binding` is treated non-fatally
    // in Resolver::resolve_declaration
    pub(super) fn reobtain_resolved_identifier<Target: ResolutionTarget>(
        &self,
        identifier: &ast::Identifier,
        namespace: CrateIndex,
    ) -> Target::Output {
        let index = self.bindings[namespace]
            .namespace()
            .unwrap()
            .bindings
            .iter()
            .copied()
            .find(|&index| &self.bindings[index].source == identifier)
            .unwrap();

        let index = self
            .collapse_use_chain(index)
            .unwrap_or_else(|_| unreachable!());

        Target::output(index, identifier)
    }

    /// Resolve use bindings.
    ///
    /// This is the second pass of three of the name resolver.
    ///
    /// This uses a queue/worklist to resolve use bindings over and over until
    /// all out of order use bindings are successfully resolved or until
    /// no progress can be made anymore in which case all remaining
    /// use bindings are actually circular and are thus reported.
    // @Task update docs in regards to number of phases
    // @Task update docs regarding errors
    // @Task clear partially_resolved_use_bindings at the end both if Err or Ok
    pub(super) fn resolve_use_bindings(&mut self) {
        while !self.partially_resolved_use_bindings.is_empty() {
            let mut partially_resolved_use_bindings = HashMap::default();

            for (&index, item) in self.partially_resolved_use_bindings.iter() {
                use ResolutionError::*;

                match self.resolve_path::<ValueOrModule>(&item.reference, item.module) {
                    Ok(reference) => {
                        self.bindings[index].kind = EntityKind::Use { reference };
                    }
                    Err(error @ (UnresolvedBinding { .. } | Unrecoverable(_))) => {
                        self.bindings[index].mark_as_error();
                        self.errors.insert(error.diagnostic(self).unwrap());
                    }
                    Err(
                        UnresolvedUseBinding { inquirer } | UnresolvedExposureReach { inquirer },
                    ) => {
                        partially_resolved_use_bindings.insert(
                            index,
                            PartiallyResolvedUseBinding {
                                reference: item.reference.clone(),
                                module: item.module,
                            },
                        );
                        // @Beacon @Beacon @Question would it change if we were
                        // to add it to a intially empty local definition `unresolved_uses_grouped`
                        // just like `unresolved_uses` (c.f. `self.unresolved_uses`)
                        // and adding to it and updating it after the "stalling" check
                        // does it remove non-circular bindings over time? or is it just like
                        // today's version?? check this with playground/somecirc.lushui
                        if let Some(indices) = self
                            .partially_resolved_use_bindings_grouped
                            .iter_mut()
                            .filter(|indices| indices.contains(&inquirer))
                            .next()
                        {
                            indices.insert(index);
                        } else {
                            let mut indices = HashSet::default();
                            indices.insert(index);
                            self.partially_resolved_use_bindings_grouped.push(indices);
                        }
                    } // @Note UnresolvedExposureReach does not differentiate
                      // between unresolved and erroneous exposure reach
                      // @Task we probably should do that
                      // @Temporary
                      // Err(UnresolvedExposureReach) => {
                      //     partially_resolved_use_bindings.insert(
                      //         index,
                      //         PartiallyResolvedUseBinding {
                      //             reference: item.reference.clone(),
                      //             module: item.module,
                      //         },
                      //     );
                      // }
                }
            }

            // resolution stalled; therefore there are circular use-bindings
            if partially_resolved_use_bindings.len() == self.partially_resolved_use_bindings.len() {
                let circular_bindings = take(&mut self.partially_resolved_use_bindings_grouped)
                    .into_iter()
                    .filter(|indices| {
                        indices
                            .iter()
                            .any(|index| partially_resolved_use_bindings.contains_key(index))
                    });

                let errors = circular_bindings
                    .map(|indices| -> Diagnostic {
                        indices
                            .iter()
                            .for_each(|&index| self.bindings[index].mark_as_error());

                        let declarations = unordered_listing(
                            indices
                                .iter()
                                .map(|&index| self.absolute_path(index).quote()),
                            Conjunction::And,
                        );

                        let spans = indices
                            .into_iter()
                            .map(|index| self.bindings[index].source.span);

                        Diagnostic::error()
                            .with_code(Code::E024)
                            .with_message(format!("declarations {declarations} are circular"))
                            .with_primary_spans(spans)
                    })
                    .collect::<Vec<_>>();

                self.errors.extend(errors);
                return;
            }

            self.partially_resolved_use_bindings = partially_resolved_use_bindings;
        }
    }
}

// @Task improve!
impl fmt::Debug for CrateScope {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("CrateScope")
            .field(
                "bindings",
                &self
                    .bindings
                    .iter_enumerated()
                    .map(|(index, binding)| format!("\n    {:?}: {}", index, binding.with(self)))
                    .collect::<String>()
                    .as_debug(),
            )
            // @Bug debug out broken: missing newline: we need to do it manually without debug_struct
            .field(
                "partially_resolved_use_bindings",
                &self.partially_resolved_use_bindings,
            )
            .field(
                "partially_resolved_use_bindings_grouped",
                &self.partially_resolved_use_bindings_grouped,
            )
            .field(
                "partially_resolved_exposure_reaches",
                &self.partially_resolved_exposure_reaches,
            )
            .field(
                "partially_resolved_exposure_reaches_grouped",
                &self.partially_resolved_exposure_reaches_grouped,
            )
            // .field("out_of_order_bindings", &self.out_of_order_bindings)
            .finish()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum Exposure {
    Unrestricted,
    Restricted(RefCell<RestrictedExposure>),
}

impl Exposure {
    fn compare(&self, other: &Self, scope: &CrateScope) -> Option<Ordering> {
        use Exposure::*;

        match (self, other) {
            (Unrestricted, Unrestricted) => Some(Ordering::Equal),
            (Unrestricted, Restricted(_)) => Some(Ordering::Greater),
            (Restricted(_), Unrestricted) => Some(Ordering::Less),
            (Restricted(this), Restricted(other)) => this.borrow().compare(&other.borrow(), scope),
        }
    }
}

impl fmt::Debug for Exposure {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "{}", "unrestricted"),
            Self::Restricted(exposure) => {
                write!(f, "restricted(")?;
                match &*exposure.borrow() {
                    RestrictedExposure::Unresolved { reach } => write!(f, "{reach}"),
                    RestrictedExposure::Resolved { reach } => write!(f, "{reach}"),
                }?;
                write!(f, ")")
            }
        }
    }
}

impl DisplayWith for Exposure {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unrestricted => write!(f, "unrestricted"),
            Self::Restricted(exposure) => write!(f, "`{}`", exposure.borrow().with(scope)),
        }
    }
}

#[derive(Clone, PartialEq, Eq)]
pub enum RestrictedExposure {
    Unresolved { reach: Path },
    Resolved { reach: CrateIndex },
}

impl RestrictedExposure {
    fn compare(&self, other: &Self, scope: &CrateScope) -> Option<Ordering> {
        use RestrictedExposure::*;

        Some(match (self, other) {
            (&Resolved { reach: this }, &Resolved { reach: other }) => {
                if this == other {
                    Ordering::Equal
                } else if other.some_ancestor_equals(this, scope) {
                    Ordering::Greater
                } else if this.some_ancestor_equals(other, scope) {
                    Ordering::Less
                } else {
                    return None;
                }
            }
            _ => return None,
        })
    }
}

trait ResolveExt {
    fn resolve(
        &self,
        definition_site_namespace: CrateIndex,
        scope: &CrateScope,
    ) -> Result<CrateIndex, ResolutionError>;
}

impl ResolveExt for RefCell<RestrictedExposure> {
    fn resolve(
        &self,
        definition_site_namespace: CrateIndex,
        scope: &CrateScope,
    ) -> Result<CrateIndex, ResolutionError> {
        let exposure = self.borrow();

        Ok(match &*exposure {
            RestrictedExposure::Unresolved {
                reach: unresolved_reach,
            } => {
                let reach = scope
                    .resolve_path::<OnlyModule>(unresolved_reach, definition_site_namespace)?;
                // unwrap: all use bindings are already resolved @Update @Bug not anymore...UnresExpRe
                // .map_err(|error| error.diagnostic(scope).unwrap())?;

                let reach_is_ancestor =
                    definition_site_namespace.some_ancestor_equals(reach, scope);

                if !reach_is_ancestor {
                    return Err(Diagnostic::error()
                        .with_code(Code::E000)
                        .with_message("exposure can only be restricted to ancestor modules")
                        .with_primary_span(unresolved_reach)
                        .into());
                }

                drop(exposure);
                *self.borrow_mut() = RestrictedExposure::Resolved { reach };

                reach
            }
            &RestrictedExposure::Resolved { reach } => reach,
        })
    }
}

impl DisplayWith for RestrictedExposure {
    type Linchpin = CrateScope;

    fn format(&self, scope: &CrateScope, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Unresolved { reach } => write!(f, "{}", reach),
            &Self::Resolved { reach } => write!(f, "{}", scope.absolute_path(reach)),
        }
    }
}

impl From<RestrictedExposure> for Exposure {
    fn from(exposure: RestrictedExposure) -> Self {
        Self::Restricted(RefCell::new(exposure))
    }
}

/// Duplicate definition diagnostic.
pub(super) struct DuplicateDefinition {
    binder: String,
    occurrences: SmallVec<Span, 2>,
}

impl From<DuplicateDefinition> for Diagnostic {
    fn from(definition: DuplicateDefinition) -> Self {
        Diagnostic::error()
            .with_code(Code::E020)
            .with_message(format!(
                "`{}` is defined multiple times in this scope",
                definition.binder
            ))
            .with_labeled_primary_spans(
                definition.occurrences.into_iter(),
                "conflicting definition",
            )
    }
}

impl From<HashMap<CrateIndex, DuplicateDefinition>> for Diagnostics {
    fn from(definitions: HashMap<CrateIndex, DuplicateDefinition>) -> Self {
        definitions.into_values().map(Into::into).collect()
    }
}

/// Specifies behavior for resolving different sorts of entities.
///
/// Right now, it's only about the difference between values and modules
/// since modules are not values (non-first-class). As such, this trait
/// allows to implementors to define what should happen with the resolved entity
/// if it appears in a specific location
pub(super) trait ResolutionTarget {
    type Output;

    fn output(index: CrateIndex, identifier: &ast::Identifier) -> Self::Output;

    fn handle_bare_super_and_crate(span: Span, namespace: CrateIndex) -> Result<Self::Output>;

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output>;
}

/// Marker to specify it's okay to resolve to either value or module.
pub(super) enum ValueOrModule {}

impl ResolutionTarget for ValueOrModule {
    type Output = CrateIndex;

    fn output(index: CrateIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_super_and_crate(_: Span, namespace: CrateIndex) -> Result<Self::Output> {
        Ok(namespace)
    }

    fn resolve_simple_path(
        _: &ast::Identifier,
        _: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        Ok(index)
    }
}

/// Marker to specify to only resolve to values.
pub(super) enum OnlyValue {}

impl ResolutionTarget for OnlyValue {
    type Output = Identifier;

    fn output(index: CrateIndex, identifier: &ast::Identifier) -> Self::Output {
        Identifier::new(index, identifier.clone())
    }

    fn handle_bare_super_and_crate(span: Span, _: CrateIndex) -> Result<Self::Output> {
        Err(module_used_as_a_value(span))
    }

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            UntypedValue | UntypedDataType(_) | UntypedConstructor(_) | Error => {
                Ok(Self::output(index, identifier))
            }
            Module(_) => Err(module_used_as_a_value(identifier.span)),
            _ => unreachable!(),
        }
    }
}

pub(super) enum OnlyModule {}

impl ResolutionTarget for OnlyModule {
    type Output = CrateIndex;

    fn output(index: CrateIndex, _: &ast::Identifier) -> Self::Output {
        index
    }

    fn handle_bare_super_and_crate(_: Span, namespace: CrateIndex) -> Result<Self::Output> {
        Ok(namespace)
    }

    fn resolve_simple_path(
        identifier: &ast::Identifier,
        binding: &EntityKind,
        index: CrateIndex,
    ) -> Result<Self::Output> {
        use EntityKind::*;

        match binding {
            // @Beacon @Task use a distinct code!
            UntypedValue | UntypedDataType(_) | UntypedConstructor(_) => Err(Diagnostic::error()
                .with_code(Code::E022)
                .with_message(format!("value `{}` is not a module", identifier))
                .with_primary_span(identifier)),
            Module(_) | Error => Ok(index),
            _ => unreachable!(),
        }
    }
}

/// If an identifier is used unqualified or qualified.
///
/// Exclusively used for error reporting.
#[derive(Clone, Copy)]
enum IdentifierUsage {
    Qualified,
    Unqualified,
}

struct PartiallyResolvedUseBinding {
    reference: Path,
    module: CrateIndex,
}

impl fmt::Debug for PartiallyResolvedUseBinding {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("PartiallyResolvedUseBinding")
            .field("reference", &self.reference.as_debug())
            .field("module", &self.module)
            .finish()
    }
}

/// A namespace can either be a module or a data type.
/// A module contains any declarations (except constructors) and
/// a data type their constructors.
// @Task update documentation
#[derive(Clone, Default)]
pub struct Namespace {
    bindings: Vec<CrateIndex>,
}

impl fmt::Debug for Namespace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.bindings.iter().join_with(" "))
    }
}

// @Question move to resolver/hir.rs?
/// A name-resolved identifier.
#[derive(Clone, PartialEq)]
pub struct Identifier {
    /// Source at the use-site/call-site or def-site if definition.
    pub(crate) source: ast::Identifier,
    pub(crate) index: Index,
}

impl Identifier {
    pub(super) fn new(index: impl Into<Index>, source: ast::Identifier) -> Self {
        Self {
            index: index.into(),
            source,
        }
    }

    pub(crate) fn parameter(name: &str) -> Self {
        Identifier::new(
            Index::DeBruijnParameter,
            ast::Identifier::new(name.into(), Span::SHAM),
        )
    }

    pub fn as_str(&self) -> &str {
        self.source.as_str()
    }

    pub fn to_expression(self) -> crate::hir::Expression {
        crate::hir::expr! {
            Binding {
                crate::lowered_ast::Attributes::default(), self.span();
                binder: self
            }
        }
    }

    // @Note bad name
    pub fn as_innermost(&self) -> Self {
        Self::new(DeBruijnIndex(0), self.source.clone())
    }

    pub fn stripped(self) -> Self {
        Self {
            source: self.source.stripped(),
            ..self
        }
    }

    pub fn is_innermost(&self) -> bool {
        self.index == DeBruijnIndex(0).into()
    }

    pub fn shift(self, amount: usize) -> Self {
        Self {
            index: self.index.shift(amount),
            ..self
        }
    }

    pub fn unshift(self) -> Self {
        Self {
            index: self.index.unshift(),
            ..self
        }
    }

    pub fn crate_index(&self) -> Option<CrateIndex> {
        self.index.crate_()
    }

    pub fn de_bruijn_index(&self) -> Option<DeBruijnIndex> {
        self.index.de_bruijn()
    }
}

impl Spanning for Identifier {
    fn span(&self) -> Span {
        self.source.span
    }
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.source)?;
        if crate::OPTIONS
            .get()
            .map_or(false, |options| options.display_crate_indices)
        {
            // @Note does not work well with punctuation..
            write!(f, "#{:?}", self.index)?;
        }
        Ok(())
    }
}

impl fmt::Debug for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

#[derive(Clone, Copy, PartialEq)]
pub enum Index {
    Crate(CrateIndex),
    DeBruijn(DeBruijnIndex),
    DeBruijnParameter,
}

impl Index {
    fn shift(self, amount: usize) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0 + amount).into(),
            Self::Crate(_) | Self::DeBruijnParameter => self,
        }
    }

    fn unshift(self) -> Self {
        match self {
            Self::DeBruijn(index) => DeBruijnIndex(index.0.saturating_sub(1)).into(),
            Self::Crate(_) | Self::DeBruijnParameter => self,
        }
    }

    pub fn crate_(self) -> Option<CrateIndex> {
        match self {
            Self::Crate(index) => Some(index),
            _ => None,
        }
    }

    pub fn de_bruijn(self) -> Option<DeBruijnIndex> {
        match self {
            Self::DeBruijn(index) => Some(index),
            _ => None,
        }
    }
}

impl fmt::Debug for Index {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Crate(index) => write!(f, "{:?}", index),
            Self::DeBruijn(index) => write!(f, "{:?}", index),
            Self::DeBruijnParameter => write!(f, "P"),
        }
    }
}

/// Crate-local identifier for bindings defined through declarations.
// @Note name is kind of misleading, we should think about renaming it
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct CrateIndex(pub(crate) usize);

impl CrateIndex {
    fn parent(self, scope: &CrateScope) -> Option<Self> {
        scope.bindings[self].parent
    }

    fn some_ancestor_equals(mut self, namespace: CrateIndex, scope: &CrateScope) -> bool {
        loop {
            if self == namespace {
                break true;
            }

            self = match self.parent(scope) {
                Some(parent) => parent,
                None => break false,
            }
        }
    }
}

impl fmt::Debug for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}C", self.0)
    }
}

impl fmt::Display for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

impl From<CrateIndex> for Index {
    fn from(index: CrateIndex) -> Self {
        Self::Crate(index)
    }
}

impl indexed_vec::Idx for CrateIndex {
    fn new(index: usize) -> Self {
        Self(index)
    }
    fn index(self) -> usize {
        self.0
    }
}

/// De Bruijn index — index for bindings defined by function parameters.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct DeBruijnIndex(pub usize);

impl fmt::Debug for DeBruijnIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}D", self.0)
    }
}

impl From<DeBruijnIndex> for Index {
    fn from(index: DeBruijnIndex) -> Self {
        Self::DeBruijn(index)
    }
}

pub enum FunctionScope<'a> {
    Module(CrateIndex),
    FunctionParameter {
        parent: &'a Self,
        binder: ast::Identifier,
    },
    PatternBinders {
        parent: &'a Self,
        binders: Vec<ast::Identifier>,
    },
}

impl<'a> FunctionScope<'a> {
    pub(super) fn extend_with_parameter(&'a self, binder: ast::Identifier) -> Self {
        Self::FunctionParameter {
            parent: self,
            binder,
        }
    }

    pub(super) fn extend_with_pattern_binders(&'a self, binders: Vec<ast::Identifier>) -> Self {
        Self::PatternBinders {
            parent: self,
            binders,
        }
    }

    pub fn absolute_path(binder: &Identifier, scope: &CrateScope) -> String {
        match binder.index {
            Index::Crate(index) => scope.absolute_path(index),
            Index::DeBruijn(_) | Index::DeBruijnParameter => binder.as_str().into(),
        }
    }

    /// Resolve a binding in a function scope given a depth.
    pub(super) fn resolve_binding(&self, query: &Path, scope: &CrateScope) -> Result<Identifier> {
        self.resolve_binding_with_depth(query, scope, 0, self)
    }

    /// Resolve a binding in a function scope given a depth.
    ///
    /// The `depth` is necessary for the recursion to successfully create DeBruijn-indices.
    ///
    /// The `origin` signifies the innermost function scope from where the resolution was first requested.
    /// This information is used for diagnostics, namely typo flagging where we once again start at the origin
    /// and walk back out.
    fn resolve_binding_with_depth(
        &self,
        query: &Path,
        scope: &CrateScope,
        depth: usize,
        origin: &Self,
    ) -> Result<Identifier> {
        use FunctionScope::*;

        match self {
            &Module(module) => scope
                .resolve_path::<OnlyValue>(query, module)
                .map_err(|error| {
                    error
                        .diagnostic_with(scope, |identifier, _| {
                            origin.find_similarly_named(identifier, scope)
                        })
                        .unwrap()
                }),
            // @Note this looks ugly/complicated, use helper functions
            FunctionParameter { parent, binder } => {
                if let Some(identifier) = query.identifier_head() {
                    if binder == identifier {
                        if query.segments.len() > 1 {
                            return Err(value_used_as_a_namespace(identifier, &query.segments[1]));
                        }

                        Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                    } else {
                        parent.resolve_binding_with_depth(query, scope, depth + 1, origin)
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module())
                        .map_err(|error| error.diagnostic(scope).unwrap())
                }
            }
            // @Note this looks ugly/complicated, use helper functions
            PatternBinders { parent, binders } => {
                if let Some(identifier) = query.identifier_head() {
                    match binders
                        .iter()
                        .rev()
                        .zip(depth..)
                        .find(|(binder, _)| binder == &identifier)
                    {
                        Some((_, depth)) => {
                            if query.segments.len() > 1 {
                                return Err(value_used_as_a_namespace(
                                    identifier,
                                    &query.segments[1],
                                ));
                            }

                            Ok(Identifier::new(DeBruijnIndex(depth), identifier.clone()))
                        }
                        None => parent.resolve_binding_with_depth(
                            query,
                            scope,
                            depth + binders.len(),
                            origin,
                        ),
                    }
                } else {
                    scope
                        .resolve_path::<OnlyValue>(query, parent.module())
                        .map_err(|error| error.diagnostic(scope).unwrap())
                }
            }
        }
    }

    /// Find a similarly named binding in the scope.
    ///
    /// With "scope", it is meant to include parent scopes, too.
    ///
    /// Used for error reporting when an undefined binding was encountered.
    /// In the future, we might decide to find not one but several similar names
    /// but that would be computationally heavier and we would need to be careful
    /// and consider the effects of shadowing.
    fn find_similarly_named(&self, identifier: &str, scope: &'a CrateScope) -> Option<&str> {
        use FunctionScope::*;

        match self {
            &Module(module) => {
                scope.find_similarly_named(identifier, scope.bindings[module].namespace().unwrap())
            }
            FunctionParameter { parent, binder } => {
                if is_similar(identifier, binder.as_str()) {
                    Some(binder.as_str())
                } else {
                    parent.find_similarly_named(identifier, scope)
                }
            }
            PatternBinders { parent, binders } => {
                if let Some(binder) = binders
                    .iter()
                    .rev()
                    .find(|binder| is_similar(identifier, binder.as_str()))
                {
                    Some(binder.as_str())
                } else {
                    parent.find_similarly_named(identifier, scope)
                }
            }
        }
    }

    fn module(&self) -> CrateIndex {
        use FunctionScope::*;

        match self {
            Module(index) => *index,
            FunctionParameter { parent, .. } => parent.module(),
            PatternBinders { parent, .. } => parent.module(),
        }
    }
}

fn is_similar(queried_identifier: &str, other_identifier: &str) -> bool {
    strsim::levenshtein(other_identifier, queried_identifier)
        <= std::cmp::max(queried_identifier.len(), 3) / 3
}

// @Task levenshtein-search for similar named bindings which are in fact a namespace and suggest the first one
// @Question rewrite to set the focus on the illegal access to a "subdeclaration"
fn value_used_as_a_namespace(
    non_namespace: &ast::Identifier,
    subbinder: &ast::Identifier,
) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E022)
        .with_message(format!("value `{}` is not a namespace", non_namespace))
        .with_labeled_primary_span(subbinder, "reference to a subdeclaration")
        .with_labeled_secondary_span(non_namespace, "a value, not a namespace")
}

// @Task levenshtein-search for similar named bindings which are in fact values and suggest the first one
// @Task print absolute path of the module in question and use highlight the entire path, not just the last
// segment
fn module_used_as_a_value(span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_code(Code::E023)
        .with_message("module used as if it was a value")
        .with_primary_span(span)
        .with_help("modules are not first-class citizens, consider utilizing records for such cases instead")
}

/// A possibly recoverable error that can emerge during resolution.
enum ResolutionError {
    Unrecoverable(Diagnostic),
    UnresolvedBinding {
        identifier: ast::Identifier,
        namespace: CrateIndex,
        usage: IdentifierUsage,
    },
    UnresolvedUseBinding {
        inquirer: CrateIndex,
    },
    // @Temporary
    UnresolvedExposureReach {
        inquirer: CrateIndex,
    },
}

impl ResolutionError {
    fn diagnostic(self, scope: &CrateScope) -> Option<Diagnostic> {
        self.diagnostic_with(scope, |identifier, namespace| {
            scope.find_similarly_named(identifier, scope.bindings[namespace].namespace().unwrap())
        })
    }

    fn diagnostic_with<'s>(
        self,
        scope: &CrateScope,
        find_lookalike: impl FnOnce(&str, CrateIndex) -> Option<&'s str>,
    ) -> Option<Diagnostic> {
        match self {
            Self::Unrecoverable(error) => Some(error),
            Self::UnresolvedBinding {
                identifier,
                namespace,
                usage,
            } => {
                // @Question should we use the terminology "field" when the namespace is a record?
                let mut message = format!("binding `{}` is not defined in ", identifier);

                match usage {
                    IdentifierUsage::Unqualified => message += "this scope",
                    IdentifierUsage::Qualified => {
                        message += match scope.bindings[namespace].kind {
                            EntityKind::Module(_) => "module",
                            EntityKind::UntypedDataType(_) | EntityKind::UntypedConstructor(_) => {
                                "namespace"
                            }
                            _ => unreachable!(),
                        };
                        message += " `";
                        message += &scope.absolute_path(namespace);
                        message += "`";
                    }
                }

                Some(
                    Diagnostic::error()
                        .with_code(Code::E021)
                        .with_message(message)
                        .with_primary_span(&identifier)
                        .when_some(
                            find_lookalike(identifier.as_str(), namespace),
                            |diagnostic, binding| {
                                diagnostic.with_help(format!(
                                    "a binding with a similar name exists in scope: `{}`",
                                    binding
                                ))
                            },
                        ),
                )
            }
            _ => None,
        }
    }
}

impl From<Diagnostic> for ResolutionError {
    fn from(error: Diagnostic) -> Self {
        Self::Unrecoverable(error)
    }
}

pub(super) enum RegistrationError {
    Unrecoverable(Diagnostics),
    /// Duplicate definitions found.
    ///
    /// Details about this error are **not stored here** but as
    /// [DuplicateDefinition]s in the [super::Resolver] so they can
    /// be easily grouped.
    DuplicateDefinition,
}

impl RegistrationError {
    pub(super) fn diagnostics(
        self,
        duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
    ) -> Diagnostics {
        duplicate_definitions
            .into_values()
            .map(Into::into)
            .chain(self)
            .collect()
    }
}

impl From<Diagnostic> for RegistrationError {
    fn from(diagnostic: Diagnostic) -> Self {
        Self::Unrecoverable(once(diagnostic).collect())
    }
}

// @Note not entirely happy that we need to implement this
// this undermines the safety of this type: We only want you
// to be able to access the unrecoverable diagnostics if you
// give us duplicate_definitions: HashMap<CrateIndex, DuplicateDefinition>,
// but now, this is not true anymore
impl IntoIterator for RegistrationError {
    type Item = Diagnostic;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            Self::Unrecoverable(diagnostics) => diagnostics.into_iter(),
            Self::DuplicateDefinition => Diagnostics::default().into_iter(),
        }
    }
}

// @Note uggllyyy!!!
impl Extend<Diagnostic> for RegistrationError {
    fn extend<T: IntoIterator<Item = Diagnostic>>(&mut self, iter: T) {
        if let Self::DuplicateDefinition = self {
            *self = Self::Unrecoverable(Diagnostics::default());
        };

        match self {
            Self::Unrecoverable(diagnostics) => diagnostics.extend(iter),
            Self::DuplicateDefinition => unreachable!(),
        };
    }
}
