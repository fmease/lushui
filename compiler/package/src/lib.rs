//! The package and component resolver.
#![feature(let_chains, try_trait_v2)]

use diagnostics::{
    error::{Health, Outcome, Result},
    reporter::ErasedReportedError,
    Diag, ErrorCode, Reporter,
};
use index_map::IndexMap;
use lexer::word::Word;
use manifest::{DependencyDeclaration, DependencyProvider, PackageManifest, PackageProfile};
use recnot::Record;
use session::{
    package::{ManifestPath, Package, PossiblyUnresolvedComponent::*, CORE_PACKAGE_NAME},
    unit::{BuildUnit, CompTy},
    Context,
};
use span::{SourceMap, Spanned, WeaklySpanned};
use std::{
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use utility::{
    cycle::find_cycles_by_key, default, path::CanonicalPathBuf, pluralize, CompIdx, Conjunction,
    FormatError, HashMap, ListingExt, QuoteExt, FILE_EXTENSION,
};

mod error;
mod manifest;

pub fn find_package(path: &Path) -> Option<&Path> {
    let manifest_path = path.join(ManifestPath::FILE_NAME);

    // Using `exists` over `try_exists` is fine here since any suppressed errors will surface later
    // when we try to load the manifest without degradation in the quality of the error message.
    if manifest_path.exists() {
        Some(path)
    } else {
        find_package(path.parent()?)
    }
}

/// Resolve all components and package dependencies of a package given the path to its folder without building anything.
pub fn resolve_package(
    path: &Path,
    filter: &ComponentFilter,
    map: &Arc<RwLock<SourceMap>>,
    rep: Reporter,
) -> Result<(IndexMap<CompIdx, BuildUnit>, Context)> {
    let mut queue = BuildQueue::new(map, rep);
    queue.resolve_package(path, filter)?;
    Ok(queue.finalize())
}

/// Resolve the components and dependencies of a file given its path without building anything.
pub fn resolve_file(
    path: &Path,
    content: Option<Arc<String>>,
    ty: CompTy,
    no_core: bool,
    map: &Arc<RwLock<SourceMap>>,
    rep: Reporter,
) -> Result<(IndexMap<CompIdx, BuildUnit>, Context)> {
    let mut queue = BuildQueue::new(map, rep);
    queue.resolve_file(path, content, ty, no_core)?;
    Ok(queue.finalize())
}

trait PackageExt {
    fn from_manifest(profile: PackageProfile, path: ManifestPath) -> Self;
}

impl PackageExt for Package {
    fn from_manifest(profile: PackageProfile, path: ManifestPath) -> Self {
        Package {
            name: profile.name.bare,
            path,
            version: profile.version.bare,
            description: profile
                .description
                .map(|description| description.bare)
                .unwrap_or_default(),
            components: HashMap::default(),
        }
    }
}

struct BuildQueue {
    /// The components which have not been built yet.
    comps: IndexMap<CompIdx, BuildUnit>,
    pkgs: HashMap<ManifestPath, Package>,
    /// The mapping from component to corresponding package.
    comp_pkgs: HashMap<CompIdx, ManifestPath>,
    map: Arc<RwLock<SourceMap>>,
    rep: Reporter,
}

impl BuildQueue {
    fn new(map: &Arc<RwLock<SourceMap>>, rep: Reporter) -> Self {
        Self {
            comps: default(),
            pkgs: default(),
            comp_pkgs: default(),
            map: map.clone(),
            rep,
        }
    }
}

impl BuildQueue {
    fn resolve_package(&mut self, folder: &Path, filter: &ComponentFilter) -> Result {
        let manifest_path = folder.join(ManifestPath::FILE_NAME);
        let manifest = self.map().load(&manifest_path, None);
        let manifest = match manifest {
            Ok(file) => file,
            Err(error) => {
                return Err(Diag::error()
                    .message("could not load the package")
                    .path(manifest_path)
                    .note(format!(
                        "failed to open the package manifest:\n{}",
                        error.format()
                    ))
                    .report(&self.rep));
            }
        };

        let manifest_path = ManifestPath::from(self.map()[manifest].name().path().unwrap());
        let manifest = PackageManifest::parse(manifest, self)?;

        let package = Package::from_manifest(manifest.profile, manifest_path);
        self.pkgs.insert(manifest_path, package);

        let mut health = Health::Untainted;

        if !filter.is_empty() {
            return Err(Diag::error()
                .message("component filters are not supported yet")
                .report(&self.rep));
        }

        // @Task apply the ComponentFilter here!
        // @Note it's not that simple. we also need to check (local) components
        //       that are (local component) dependencies of the
        //       filtered / included components.
        if let Some(component_worklist) = manifest.components {
            // @Task don't use a HashMap here, @Bug this leads to non-determistic
            // ordering of diagnostics in case of cycle errors.
            let mut component_worklist: HashMap<_, _> = component_worklist
                .bare
                .into_iter()
                .map(|(name, component)| (name, (component, None)))
                .collect();

            self[manifest_path].components.extend(
                component_worklist
                    .keys()
                    .map(|name| (name.bare, Unresolved)),
            );

            while !component_worklist.is_empty() {
                let amount_unresolved_components = component_worklist.len();
                let mut unresolved_components = HashMap::default();

                for (name, (component, _)) in component_worklist {
                    use error::DependencyResolutionError::*;
                    let dependencies = match self.resolve_dependencies(
                        name.bare,
                        manifest_path,
                        component.bare.dependencies.as_ref(),
                    ) {
                        Ok(dependencies) => dependencies,
                        // @Temporary Cycle case
                        Err(UnresolvedLocalComponent(dependency_name) | Cycle(dependency_name)) => {
                            unresolved_components.insert(name, (component, Some(dependency_name)));
                            continue;
                        }
                        Err(ErasedFatal(error) | ErasedNonFatal(error)) => {
                            health.taint(error);
                            continue;
                        }
                    };

                    let ty = component.bare.ty;

                    if ty.bare != CompTy::Library && ty.bare != CompTy::Executable {
                        let error = Diag::error()
                            .message(format!("the component type ‘{ty}’ is not supported yet"))
                            .unlabeled_span(ty)
                            .report(&self.rep);
                        health.taint(error);
                    }

                    // @Task add UI test
                    if let Some(public) = component.bare.public {
                        let error = Diag::error()
                            .message("setting the component exposure is not supported yet")
                            .unlabeled_span(public)
                            .report(&self.rep);
                        health.taint(error);
                    }

                    let component = self.comps.insert_with(|index| {
                        BuildUnit {
                            name: name.bare,
                            index,
                            // @Beacon @Temporary new_unchecked @Bug its use is incorrect! @Task canonicalize (I guess?)
                            path: component.bare.path.as_ref().map(|relative_path| {
                                CanonicalPathBuf::new_unchecked(
                                    manifest_path.folder().join(relative_path),
                                )
                            }),
                            ty: ty.bare,
                            dependencies,
                        }
                    });

                    self.register_package_component(manifest_path, name.bare, component);
                }

                // Resolution stalled; therefore there are cyclic package-local components.
                if unresolved_components.len() == amount_unresolved_components {
                    // @Task if the cycle is of size one, add a note that it is referencing itself (which might be
                    // non-obvious from the span we currently highlight).
                    // @Task if there are other local components with
                    // the same name but with a differing type, add a note that only library components are being looked
                    // at during dependency resolution (clarifying that one cannot depend on non-library components)

                    for cycle in find_cycles_by_key::<Word, Spanned<Word>>(
                        &unresolved_components
                            .iter()
                            .map(|(dependent, (_, dependency))| {
                                (dependent.bare, dependency.unwrap())
                            })
                            .collect(),
                        |name| &name.bare,
                    ) {
                        let components = cycle.iter().map(QuoteExt::quote).list(Conjunction::And);

                        // @Task if the circular components are local, i.e. all come from the same package,
                        //       add “in package ‘<package>’ [is/are circular]”
                        Diag::error()
                            .message(format!(
                                "the library {} {components} {} circular",
                                pluralize!(cycle.len(), "component"),
                                pluralize!(cycle.len(), "is", "are"),
                            ))
                            .unlabeled_spans(cycle)
                            .report(&self.rep);
                    }

                    return Err(ErasedReportedError::new_unchecked());
                }

                component_worklist = unresolved_components;
            }
        }

        health.into()
    }

    fn resolve_file(
        &mut self,
        file_path: &Path,
        _: Option<Arc<String>>,
        ty: CompTy,
        no_core: bool,
    ) -> Result {
        let file_path = match CanonicalPathBuf::new(file_path) {
            Ok(path) => path,
            Err(error) => {
                // @Task better message
                return Err(Diag::error()
                    .message("could not load the file")
                    .path(file_path.into())
                    .note(error.format())
                    .report(&self.rep));
            }
        };

        let name = parse_comp_name_from_file_path(&file_path, &self.rep)?;
        let mut dependencies = HashMap::default();

        if !no_core {
            let core_manifest_path = ManifestPath::core();
            let core_package_name = session::package::core_package_name();
            let library = self
                .resolve_dependency_by_manifest(
                    Ok(core_manifest_path),
                    Spanned::bare(core_package_name),
                    None,
                    Box::new(|| {
                        Diag::error()
                            .message(format!("could not load the package ‘{CORE_PACKAGE_NAME}’"))
                            .path(core_manifest_path.0.to_path_buf())
                    }),
                )
                .map_err(|error| {
                    use error::DependencyResolutionError::*;
                    match error {
                        ErasedNonFatal(error) | ErasedFatal(error) => error,
                        UnresolvedLocalComponent(..) | Cycle(_) => todo!(), // @Temporary
                    }
                })?;

            dependencies.insert(core_package_name, library);
        }

        self.comps.insert_with(|index| {
            BuildUnit {
                name,
                index,
                // @Task don't use bare
                path: Spanned::bare(file_path),
                ty,
                dependencies,
            }
        });

        Ok(())
    }

    fn resolve_dependencies(
        &mut self,
        dependent_component_name: Word,
        dependent_path: ManifestPath,
        dependencies: Option<&Spanned<Record<Word, Spanned<DependencyDeclaration>>>>,
    ) -> Result<HashMap<Word, CompIdx>, error::DependencyResolutionError> {
        let Some(dependencies) = dependencies else {
            return Ok(HashMap::default());
        };

        let mut resolved_dependencies = HashMap::default();
        let mut health = Health::Untainted;

        for (&dependency_exonym, dependency_declaration) in &dependencies.bare {
            match self.resolve_dependency(
                dependent_component_name,
                dependent_path,
                dependency_exonym,
                dependency_declaration,
            ) {
                Ok(dependency) => {
                    resolved_dependencies.insert(dependency_exonym.bare, dependency);
                }
                Err(error::DependencyResolutionError::ErasedNonFatal(error)) => {
                    health.taint(error);
                }
                Err(error) => return Err(error),
            }
        }

        Result::from(Outcome::new(resolved_dependencies, health))
            .map_err(error::DependencyResolutionError::ErasedFatal)
    }

    fn resolve_dependency(
        &mut self,
        dependent_component_name: Word,
        dependent_path: ManifestPath,
        component_exonym: WeaklySpanned<Word>,
        declaration: &Spanned<DependencyDeclaration>,
    ) -> Result<CompIdx, error::DependencyResolutionError> {
        let dependency = match self.resolve_dependency_declaration(
            declaration,
            component_exonym.bare,
            dependent_path,
        ) {
            Ok(dependency) => dependency,
            Err(error) => return Err(error.into()),
        };

        // @Task add a UI test
        if let Some(version) = &declaration.bare.version {
            return Err(Diag::error()
                .message("version requirements are not supported yet")
                .unlabeled_span(version)
                .report(&self.rep)
                .into());
        }

        // @Task add a UI test
        if let Some(public) = &declaration.bare.public {
            return Err(Diag::error()
                .message("setting the dependency exposure is not supported yet")
                .unlabeled_span(public)
                .report(&self.rep)
                .into());
        }

        let component_endonym = declaration
            .bare
            .component
            .unwrap_or_else(|| component_exonym.strong());

        // @Question do we want to a allow declarations of the form ‘<secondary-lib>: { … }’ w/o an explicit ‘component: <secondary-lib>’?

        let folder_path = match dependency {
            Dependency::ForeignPackage(path) => path,
            Dependency::LocalComponent => {
                let package = &self[dependent_path];

                return match package.components.get(&component_endonym.bare) {
                    Some(&Resolved(component)) if self[component].ty == CompTy::Library => {
                        Ok(component)
                    }
                    // @Task add a UI test for this
                    Some(&Resolved(component)) => Err(error::non_library_dependency_error(
                        component_endonym,
                        self[component].ty,
                        package.name,
                    )
                    .report(&self.rep)
                    .into()),
                    Some(Unresolved) => {
                        Err(error::DependencyResolutionError::UnresolvedLocalComponent(
                            component_endonym,
                        ))
                    }
                    None => Err(
                        error::undefined_component_error(component_endonym, package.name)
                            .report(&self.rep)
                            .into(),
                    ),
                };
            }
        };

        let manifest_path_unchecked = folder_path.join(ManifestPath::FILE_NAME);
        let manifest_path = CanonicalPathBuf::new(&manifest_path_unchecked).map(ManifestPath::from);

        // Deduplicate packages by absolute path and check for circular components.
        if let Ok(manifest_path) = manifest_path
            && let Some(package) = self.pkgs.get(&manifest_path)
        {
            // @Beacon @Task add a diag note for size-one cycles of the form `{ path: "." }` (etc) and
            //               recommend the dep-provider `package` (…)

            // @Beacon @Note this probably won't scale to our new order-independent component resolver (maybe)
            // if so, consider not throwing a cycle error (here / unconditionally)
            // @Beacon @Task handle component privacy here
            return match package.components.get(&component_endonym.bare) {
                Some(&Resolved(component)) if self[component].ty == CompTy::Library => {
                    Ok(component)
                }
                // @Bug this does not fire when we want to since the it is apparently unresolved at this stage for some reason
                // @Task test this, is this reachable?
                Some(&Resolved(component)) => Err(error::non_library_dependency_error(
                    component_endonym,
                    self[component].ty,
                    package.name,
                )
                .report(&self.rep)
                .into()),
                Some(Unresolved) => {
                    // @Temporary
                    // @Note this does not scale to more complex cycles (e.g. a cycle of three components)
                    // @Task provide more context for transitive dependencies of the root component
                    Err(error::DependencyResolutionError::ErasedFatal(
                        Diag::error()
                            .message(format!(
                                "the components ‘{dependent_component_name}’ and ‘{component_endonym}’ are circular",
                            ))
                            // @Task add the span of "the" counterpart component
                            .unlabeled_span(component_exonym)
                            .report(&self.rep),
                    ))
                    // @Task this should definitely not be fatal fatal since we wanna catch separate cycles (eg. {{a,b,c}, {a,sep}})
                    // Err(error::DependencyResolutionError::Cycle(Spanned::bare(dependent_component_name.clone())))
                }
                None => Err(
                    error::undefined_component_error(component_endonym, package.name)
                        .report(&self.rep)
                        .into(),
                ),
            };
        }

        self.resolve_dependency_by_manifest(
            manifest_path,
            component_endonym,
            declaration.bare.package,
            Box::new(|| {
                Diag::error()
                    .message(format!(
                        "could not load the dependency ‘{component_exonym}’",
                    ))
                    .path(manifest_path_unchecked)
                    .unlabeled_span(match &declaration.bare.path {
                        Some(path) => path.span,
                        None => component_exonym.span,
                    })
            }),
        )
    }

    fn resolve_dependency_by_manifest(
        &mut self,
        manifest_path: std::io::Result<ManifestPath>,
        component_endonym: Spanned<Word>,
        declared_package_name: Option<Spanned<Word>>,
        load_error: Box<dyn FnOnce() -> Diag + '_>,
    ) -> Result<CompIdx, error::DependencyResolutionError> {
        let manifest =
            manifest_path.and_then(|path| Ok((path, self.map().read((*path.0).clone(), None)?)));
        let (manifest_path, manifest) = match manifest {
            Ok(manifest) => manifest,
            Err(error) => {
                // The dependency provider is most likely `filesystem` or `distribution`.
                // If the dependency provider is of the kind that downloads remote resources
                // and stores them locally, this probably means the local resources were
                // tampered with or a bug occurred.

                // @Task provide more context for transitive dependencies of the root component
                // @Question code?
                // @Task provide more information when provider==distribution
                // @Question use endonym here instead? or use both?
                return Err(load_error().note(error.format()).report(&self.rep).into());
            }
        };

        let manifest = PackageManifest::parse(manifest, self)?;

        if let Some(package_name) = declared_package_name
            && package_name.bare != manifest.profile.name.bare
        {
            // @Task message, @Task add UI test
            return Err(Diag::error()
                .message("declared package name does not match actual one")
                .unlabeled_span(package_name)
                .report(&self.rep)
                .into());
        }

        // @Task assert version requirement (if any) is fulfilled

        let package = Package::from_manifest(manifest.profile, manifest_path);
        let package_name = package.name;
        self.pkgs.insert(manifest_path, package);

        // @Task handle component privacy
        let library = match manifest
            .components
            .as_ref()
            .and_then(|components| components.bare.get(&component_endonym.weak()))
        {
            Some(component) if component.bare.ty.bare == CompTy::Library => &component.bare,
            Some(component) => {
                return Err(error::non_library_dependency_error(
                    component_endonym,
                    component.bare.ty.bare,
                    package_name,
                )
                .report(&self.rep)
                .into())
            }
            None => {
                return Err(
                    error::undefined_component_error(component_endonym, package_name)
                        .report(&self.rep)
                        .into(),
                )
            }
        };

        let name = component_endonym.bare;

        // @Beacon @Temporary new_unchecked @Bug its use is incorrect! @Task re-canonicalize (I guess?)
        let library_component_path = library.path.as_ref().map(|relative_path| {
            CanonicalPathBuf::new_unchecked(manifest_path.folder().join(relative_path))
        });

        // @Question should we prefill all components here too?
        self[manifest_path].components.insert(name, Unresolved);

        // Transitive dependencies from the perspective of the dependent package.
        let dependencies =
            self.resolve_dependencies(name, manifest_path, library.dependencies.as_ref())?;

        let library = self.comps.insert_with(|index| BuildUnit {
            name,
            index,
            path: library_component_path,
            ty: library.ty.bare,
            dependencies,
        });

        self.register_package_component(manifest_path, name, library);

        Ok(library)
    }

    fn register_package_component(
        &mut self,
        package: ManifestPath,
        name: Word,
        component: CompIdx,
    ) {
        self[package].components.insert(name, Resolved(component));
        self.comp_pkgs.insert(component, package);
    }

    fn resolve_dependency_declaration(
        &self,
        Spanned!(span, declaration): &Spanned<DependencyDeclaration>,
        exonym: Word,
        manifest_path: ManifestPath,
    ) -> Result<Dependency> {
        // @Beacon @Task create a DependencyDeclaration' that's an enum not a struct with provider
        // being the discrimant ("parse, don't validate") and return it

        let provider = match declaration.provider {
            Some(provider) => provider.bare,
            // Infer the provider from the entries.
            None => {
                if declaration.path.is_some() {
                    DependencyProvider::Filesystem
                } else if declaration.version.is_some() {
                    DependencyProvider::Registry
                } else {
                    DependencyProvider::Package
                }
            }
        };

        if provider == DependencyProvider::Package
            && (declaration.version.is_some() || declaration.package.is_some())
        {
            // @Beacon @Task report an error that those things are incompatible
            // @Task add test
            return Err(Diag::error()
                .message("@Task")
                .with(|ot| match declaration.provider {
                    Some(provider) => ot.unlabeled_span(provider),
                    None => ot,
                })
                .report(&self.rep));
        }

        if provider != DependencyProvider::Filesystem
            && let Some(path) = &declaration.path
        {
            // @Beacon @Task report an error that those things are incompatible
            // @Task add test
            // @Task add a 2nd *primary* span that highlights the provider (if available otherwise do sth. else)
            // @Question can we point at the field/key `path` instead of its value?
            return Err(Diag::error()
                .message("@Task")
                .unlabeled_span(path)
                .with(|it| match declaration.provider {
                    Some(provider) => it.unlabeled_span(provider),
                    None => it,
                })
                .report(&self.rep));
        }

        match provider {
            DependencyProvider::Filesystem => match &declaration.path {
                Some(path) => Ok(Dependency::ForeignPackage(
                    manifest_path.folder().join(&path.bare),
                )),
                // @Task improve message
                None => Err(Diag::error()
                    .message("dependency declaration does not have entry ‘path’")
                    .unlabeled_span(span)
                    .with(|it| match declaration.provider.as_ref() {
                        // Currently always present in this branch.
                        Some(provider) => it.label(provider, "required by this"),
                        None => it,
                    })
                    .report(&self.rep)),
            },
            DependencyProvider::Distribution => {
                let component = declaration
                    .component
                    .as_ref()
                    .map_or(exonym, |name| name.bare);
                let path = session::package::distributed_packages_path().join(component.to_str());
                Ok(Dependency::ForeignPackage(path))
            }
            DependencyProvider::Package => Ok(Dependency::LocalComponent),
            DependencyProvider::Git | DependencyProvider::Registry => Err(Diag::error()
                .message(format!(
                    "the dependency provider ‘{provider}’ is not supported yet",
                ))
                // @Task better label! say how it was inferred!!
                .with(|it| match declaration.provider {
                    Some(provider) => it.unlabeled_span(provider),
                    None => it.span(span, format!("implies provider ‘{provider}’")),
                })
                .report(&self.rep)),
        }
    }

    fn finalize(self) -> (IndexMap<CompIdx, BuildUnit>, Context) {
        // @Task Support the existence of more than one root component
        //       dependending on a component filter provided by the user
        let root = self.comps.last().unwrap();

        let context = Context::new(
            self.pkgs,
            self.comp_pkgs,
            root.outline(),
            &self.map,
            self.rep,
        );

        (self.comps, context)
    }

    fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.map.read().unwrap()
    }

    fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.map.write().unwrap()
    }
}

#[derive(Default)]
pub struct ComponentFilter {
    pub names: Vec<Word>,
    pub types: Vec<CompTy>,
}

impl ComponentFilter {
    fn is_empty(&self) -> bool {
        self.names.is_empty() && self.types.is_empty()
    }
}

fn parse_comp_name_from_file_path(path: &Path, rep: &Reporter) -> Result<Word> {
    if !utility::has_file_extension(path, FILE_EXTENSION) {
        Diag::warning()
            .message(format!(
                "the source file does not have the file extension ‘{FILE_EXTENSION}’"
            ))
            .report(rep);
    }

    // @Question can the file stem ever be empty in our case?
    let name = path.file_stem().unwrap();
    // @Task do not unwrap! provide custom error
    let name = name.to_str().unwrap();

    Word::parse(name.into()).map_err(|()| {
        // @Task DRY @Question is the common code justified?
        // @Question isn't this function used in such a way that it's
        //     "component and package name"?
        Diag::error()
            .code(ErrorCode::E036)
            .message(format!("the component name ‘{name}’ is not a valid word"))
            .report(rep)
    })
}

impl Index<CompIdx> for BuildQueue {
    type Output = BuildUnit;

    fn index(&self, index: CompIdx) -> &Self::Output {
        &self.comps[index]
    }
}

impl IndexMut<CompIdx> for BuildQueue {
    fn index_mut(&mut self, index: CompIdx) -> &mut Self::Output {
        &mut self.comps[index]
    }
}

impl Index<ManifestPath> for BuildQueue {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.pkgs[&path]
    }
}

impl IndexMut<ManifestPath> for BuildQueue {
    fn index_mut(&mut self, path: ManifestPath) -> &mut Self::Output {
        self.pkgs.get_mut(&path).unwrap()
    }
}

enum Dependency {
    ForeignPackage(PathBuf),
    LocalComponent,
}
