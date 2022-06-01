//! The package and component resolver.

use crate::{
    component::{Component, ComponentIndex, ComponentMetadata, ComponentType, Components},
    diagnostics::{reporter::ErasedReportedError, Code, Diagnostic, Reporter},
    error::Result,
    metadata::Record,
    session::BuildSession,
    span::{SourceMap, Spanned, WeaklySpanned},
    syntax::Word,
    utility::{
        cycle::find_cycles_by_key, pluralize, Conjunction, HashMap, IOError, ListingExt, QuoteExt,
    },
};
use index_map::IndexMap;
pub use manifest::FILE_NAME as MANIFEST_FILE_NAME;
use manifest::{
    ComponentKey, ComponentManifest, DependencyDeclaration, DependencyProvider, PackageManifest,
    PackageProfile, Version,
};
use std::{
    default::default,
    fmt,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};

mod manifest;

pub fn find_package(path: &Path) -> Option<&Path> {
    let manifest_path = path.join(manifest::FILE_NAME);

    if manifest_path.exists() {
        Some(path)
    } else {
        find_package(path.parent()?)
    }
}

/// Resolve all components and package dependencies of a package given the path to its folder without building anything.
pub fn resolve_package(
    path: &Path,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result<(Components, BuildSession)> {
    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            // @Task better message e.g. mention manifest
            return Err(Diagnostic::error()
                .message("could not load the package")
                .note(IOError(error, path).to_string())
                .report(&reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_package(&path)?;
    Ok(queue.finalize())
}

/// Resolve the components and dependencies of a file given its path without building anything.
pub fn resolve_file(
    path: &Path,
    component_type: ComponentType,
    no_core: bool,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result<(Components, BuildSession)> {
    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            // @Task better message
            return Err(Diagnostic::error()
                .message("could not load the file")
                .note(IOError(error, path).to_string())
                .report(&reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_file(path, component_type, no_core)?;
    Ok(queue.finalize())
}

/// A collection of [components](Component) and some metadata.
#[derive(Debug)]
pub struct Package {
    pub name: Word,
    /// The file or folder path of the package.
    ///
    /// For single-file packages, this points to a file.
    /// For normal packages, it points to the package folder
    /// which contains the package manifest.
    // @Beacon @Task make this of type PackagePath,
    // enum PackageLocation { NormalPackage(PathBuf), SingleFilePackage(PathBuf) }
    pub path: PathBuf,
    #[allow(dead_code)]
    version: Version,
    pub(crate) description: String,
    components: HashMap<(Word, ComponentType), PossiblyUnresolved<ComponentIndex>>,
}

impl Package {
    fn from_manifest(profile: PackageProfile, path: PathBuf) -> Self {
        Package {
            name: profile.name.value,
            path,
            version: profile.version.value,
            description: profile
                .description
                .map(|description| description.value)
                .unwrap_or_default(),
            components: HashMap::default(),
        }
    }

    fn file(name: Word, path: PathBuf) -> Self {
        Self {
            name,
            path,
            version: Version("0.0.0".to_owned()),
            description: String::new(),
            components: HashMap::default(),
        }
    }

    /// Test if this package is the standard library `core`.
    pub(crate) fn is_core(&self) -> bool {
        self.path == core_package_path()
    }
}

#[derive(Debug, Clone, Copy)]
enum PossiblyUnresolved<T> {
    Unresolved,
    Resolved(T),
}

#[derive(PartialEq, Eq, Clone, Copy, index_map::Index)]
pub struct PackageIndex(usize);

impl PackageIndex {
    #[cfg(test)]
    pub(crate) const fn new_unchecked(index: usize) -> Self {
        Self(index)
    }
}

impl fmt::Debug for PackageIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}p", self.0)
    }
}

struct BuildQueue {
    /// The components which have not been built yet.
    components: Components,
    packages: IndexMap<PackageIndex, Package>,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildQueue {
    fn new(map: &Arc<RwLock<SourceMap>>, reporter: Reporter) -> Self {
        Self {
            components: default(),
            packages: default(),
            map: map.clone(),
            reporter,
        }
    }
}

impl BuildQueue {
    fn resolve_package(&mut self, package_path: &Path) -> Result {
        let manifest_path = package_path.join(manifest::FILE_NAME);
        let manifest_file = self.map().load(manifest_path.clone());
        let manifest_file = match manifest_file {
            Ok(file) => file,
            Err(error) => {
                return Err(Diagnostic::error()
                    // @Question code?
                    .message("could not load the package")
                    .note(IOError(error, &manifest_path).to_string())
                    .report(&self.reporter));
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self)?;
        let package = Package::from_manifest(manifest.profile, package_path.to_owned());
        let package = self.packages.insert(package);

        // @Task use Health "2.0" instead
        let mut health = None::<ErasedReportedError> /* Untainted */;

        // @Beacon @Beacon @Beacon @Task we don't to unconditionally loop through all those components,
        // I *think*, only those that are relevant: this is driven by the CLI: Esp. if the users passes
        // certain flag (cf. --exe, --lib etc. flags passed to cargo build)
        // @Note but maybe we should still check for correctness of irrelevant components, check what cargo does!
        if let Some(Spanned!(component_worklist, _)) = manifest.components {
            let mut component_worklist: HashMap<_, _> = component_worklist
                .into_iter()
                .map(|(key, component)| {
                    // @Beacon #primary_components_default_to_package_name
                    // @Beacon @Beacon @Beacon @Bug don't default to the package_name !!!!!
                    // `ext: { path: "..." }` should NOT expand to
                    // `ext: { path: "...", component: ext }` the comp should just stay None / the dependenc_
                    let name = key
                        .name
                        .as_ref()
                        .map_or_else(|| self[package].name.clone(), |name| name.value.clone());

                    ((name, key.type_), (component, None))
                })
                .collect();

            self[package]
                .components
                .extend(component_worklist.iter().map(|((name, type_), _)| {
                    ((name.clone(), type_.value), PossiblyUnresolved::Unresolved)
                }));

            while !component_worklist.is_empty() {
                let amount_unresolved_components = component_worklist.len();
                let mut unresolved_components = HashMap::default();

                for ((name, type_), (component, _)) in component_worklist {
                    use DependencyResolutionError::*;
                    let dependencies = match self.resolve_dependencies(
                        &name,
                        package,
                        package_path,
                        component.value.dependencies.as_ref(),
                    ) {
                        Ok(dependencies) => dependencies,
                        Err(UnresolvedLocalComponent(dependency_name)) => {
                            unresolved_components
                                .insert((name, type_), (component, Some(dependency_name)));
                            continue;
                        }
                        Err(ErasedFatal(error) | ErasedNonFatal(error)) => {
                            // @Task use taint() "2.0" instead
                            health = Some(error);
                            continue;
                        }
                    };

                    if !matches!(
                        type_.value,
                        ComponentType::Library | ComponentType::Executable
                    ) {
                        // @Task use health.taint(...) "2.0" instead
                        health = Some(
                            Diagnostic::error()
                                .message(format!(
                                    "the component type `{type_}` is not supported yet"
                                ))
                                .primary_span(type_)
                                .report(&self.reporter),
                        );
                    }

                    let component = self.components.insert_with(|index| {
                        Component::new(
                            ComponentMetadata::new(
                                name.clone(),
                                index,
                                package,
                                component
                                    .value
                                    .path
                                    .as_ref()
                                    .map(|relative_path| package_path.join(relative_path)),
                                type_.value,
                            ),
                            dependencies,
                        )
                    });

                    self[package]
                        .components
                        .insert((name, type_.value), PossiblyUnresolved::Resolved(component));
                }

                // resolution stalled; therefore there are cyclic components
                if unresolved_components.len() == amount_unresolved_components {
                    // @Task if the cycle is of size one, add a note that it is referencing itself (which might be
                    // non-obvious from the span we currently highlight).
                    // @Task if there are other local components with
                    // the same name but with a differing type, add a note that only library components are being looked
                    // at during dependency resolution (clarifying that one cannot depend on non-library components)

                    for cycle in find_cycles_by_key(
                        &unresolved_components
                            .iter()
                            // @Question is dropping the component type here unsound? @Task craft an example
                            .map(|((dependent, _), (_, dependency))| {
                                (dependent, dependency.as_ref().unwrap().as_ref())
                            })
                            .collect::<HashMap<&Word, Spanned<&Word>>>(),
                        |name| &name.value,
                    ) {
                        let components = cycle.iter().map(QuoteExt::quote).list(Conjunction::And);

                        Diagnostic::error()
                            .message(format!(
                                "the library {} {components} {} circular",
                                pluralize!(cycle.len(), "component"),
                                pluralize!(cycle.len(), "is", "are"),
                            ))
                            .primary_spans(cycle)
                            .report(&self.reporter);
                    }

                    return Err(ErasedReportedError::new_unchecked());
                }

                component_worklist = unresolved_components;
            }
        }

        // @Task use Result::ok_if_untainted "2.0" instead
        match health {
            None => Ok(()),
            Some(error) => Err(error),
        }
    }

    fn resolve_file(&mut self, file_path: PathBuf, type_: ComponentType, no_core: bool) -> Result {
        // package *and* component name
        let name = parse_component_name_from_file_path(&file_path, &self.reporter)?;

        let package = Package::file(name.clone(), file_path.clone());
        let package = self.packages.insert(package);

        self[package]
            .components
            .insert((name.clone(), type_), PossiblyUnresolved::Unresolved);

        let mut dependencies = HashMap::default();

        if !no_core {
            // @Note this currently duplicates a lot of stuff from Self::resolve_dependencies
            // in fact, all the logic was copied over from there and manually adjusted
            // @Task abstract over this

            let core_manifest_path = core_package_path().join(manifest::FILE_NAME);
            let core_manifest_file = self.map().load(core_manifest_path.clone());
            let core_manifest_file = match core_manifest_file {
                Ok(file) => file,
                Err(error) => {
                    return Err(Diagnostic::error()
                        // @Question code?
                        .message("could not load the package `core`")
                        .note(IOError(error, &core_manifest_path).to_string())
                        .report(&self.reporter));
                }
            };

            let core_manifest = PackageManifest::parse(core_manifest_file, self)?;
            let core_package = Package::from_manifest(core_manifest.profile, core_package_path());
            let core_package = self.packages.insert(core_package);

            let core_package_name = core_package_name();
            let (_, Spanned!(library, _)) =
                self.resolve_primary_library(core_manifest.components.as_ref())?;

            self[core_package].components.insert(
                (core_package_name.clone(), ComponentType::Library),
                PossiblyUnresolved::Unresolved,
            );

            let transitive_dependencies = self
                .resolve_dependencies(
                    &core_package_name,
                    core_package,
                    &core_package_path(),
                    library.dependencies.as_ref(),
                )
                .map_err(|error| {
                    use DependencyResolutionError::*;
                    match error {
                        ErasedNonFatal(error) | ErasedFatal(error) => error,
                        UnresolvedLocalComponent(..) => unreachable!(), // ugly
                    }
                })?;

            let library = self.components.insert_with(|index| {
                Component::new(
                    ComponentMetadata::new(
                        core_package_name.clone(),
                        index,
                        core_package,
                        library
                            .path
                            .as_ref()
                            .map(|relative_path| core_package_path().join(relative_path)),
                        ComponentType::Library,
                    ),
                    transitive_dependencies,
                )
            });

            self[core_package].components.insert(
                (core_package_name.clone(), ComponentType::Library),
                PossiblyUnresolved::Resolved(library),
            );

            dependencies.insert(core_package_name, library);
        }

        let component = self.components.insert_with(|index| {
            Component::new(
                ComponentMetadata::new(
                    name.clone(),
                    index,
                    package,
                    Spanned::new(default(), file_path),
                    type_,
                ),
                dependencies,
            )
        });

        self[package]
            .components
            .insert((name, type_), PossiblyUnresolved::Resolved(component));

        Ok(())
    }

    fn resolve_dependencies(
        &mut self,
        dependent_component_name: &Word,
        dependent_package_index: PackageIndex,
        dependent_package_path: &Path,
        dependencies: Option<&Spanned<Record<Word, Spanned<DependencyDeclaration>>>>,
    ) -> Result<HashMap<Word, ComponentIndex>, DependencyResolutionError> {
        let Some(dependencies) = dependencies else {
            return Ok(HashMap::default());
        };

        let mut resolved_dependencies = HashMap::default();
        // @Task use Health once its API allows this
        let mut health = None::<ErasedReportedError> /* Untainted */;

        for (dependency_exonym, dependency_declaration) in &dependencies.value {
            match self.resolve_dependency(
                dependent_component_name,
                dependent_package_index,
                dependent_package_path,
                dependency_exonym,
                dependency_declaration,
            ) {
                Ok(dependency) => {
                    resolved_dependencies.insert(dependency_exonym.value.clone(), dependency);
                }
                Err(DependencyResolutionError::ErasedNonFatal(error)) => {
                    // @Task use health.taint() once it can accept ErasedReportedErrors
                    health = Some(error);
                }
                Err(error) => return Err(error),
            }
        }

        // @Task use Result::ok_if_untainted once the API is modernized
        match health {
            None => Ok(resolved_dependencies),
            Some(error) => Err(DependencyResolutionError::ErasedFatal(error)),
        }
    }

    fn resolve_dependency(
        &mut self,
        dependent_component_name: &Word,
        dependent_package: PackageIndex,
        dependent_package_path: &Path,
        component_exonym: &WeaklySpanned<Word>,
        declaration: &Spanned<DependencyDeclaration>,
    ) -> Result<ComponentIndex, DependencyResolutionError> {
        let dependency = match self.resolve_dependency_declaration(
            declaration,
            &component_exonym.value,
            dependent_package_path,
        ) {
            Ok(dependency) => dependency,
            Err(error) => return Err(error.into()),
        };

        if let Some(version) = &declaration.value.version {
            // @Task message, @Task test
            return Err(Diagnostic::error()
                .message("[version requirement field not supported yet]")
                .primary_span(version)
                .report(&self.reporter)
                .into());
        }

        let component_endonym = declaration
            .value
            .component
            .as_ref()
            .map_or(component_exonym.as_ref().strong(), Spanned::as_ref);

        let mut package_path = match dependency {
            Dependency::ForeignPackage(path) => path,
            Dependency::LocalComponent => {
                let package = &self[dependent_package];

                return match package
                    .components
                    .get(&(component_endonym.value.clone(), ComponentType::Library))
                {
                    Some(&PossiblyUnresolved::Resolved(index)) => Ok(index),
                    Some(PossiblyUnresolved::Unresolved) => {
                        Err(DependencyResolutionError::UnresolvedLocalComponent(
                            component_endonym.cloned(),
                        ))
                    }
                    None => Err(Diagnostic::undefined_library_component(
                        component_endonym,
                        &package.name,
                    )
                    .report(&self.reporter)
                    .into()),
                };
            }
        };

        // deduplicate packages by absolute path and check for circular components
        if let Ok(canonical_package_path) = package_path.canonicalize() {
            // error case handled later via `SourceMap::load` for uniform error messages and to achieve DRY
            // @Update ^^^ we no longer have that formatting code in place, can we improve this?
            package_path = canonical_package_path;

            if let Some(package) = self
                .packages
                .values()
                .find(|package| package.path == package_path)
            {
                // @Question handle component privacy here?
                let library = package
                    .components
                    .iter()
                    .find(|((name, type_), _)| {
                        name == component_endonym.value && *type_ == ComponentType::Library
                    })
                    .map(|(_, &library)| library);

                // @Beacon @Task add a diag note for size-one cycles of the form `{ path: "." }` (etc) and
                //               recommend the dep-provider `package` (…)

                // @Beacon @Beacon @Note this probably won't scale to our new order-independent component resolver (maybe)
                // if so, consider not throwing a cycle error (here / unconditionally)
                return match library {
                    Some(PossiblyUnresolved::Resolved(library)) => Ok(library),
                    // @Beacon @Beacon @Beacon @Task instead of reporting an error here, return a "custom"
                    //                               DepRepError::UnresolvedComponent here (add a param/flag to differentiate
                    //                               it from the case above) and @Task smh accumulate "inquirer"s so we can
                    //                               find_cycles later on for scalable diagnostics (> 3 packages that are cyclic)
                    Some(PossiblyUnresolved::Unresolved) => {
                        // @Note the message does not scale to more complex cycles (e.g. a cycle of three components)
                        // @Task provide more context for transitive dependencies of the goal component
                        // @Task code

                        // @Question does this need to be fatal?
                        Err(DependencyResolutionError::ErasedFatal(
                            Diagnostic::error()
                                .message(format!(
                                    "the components `{dependent_component_name}` and `{component_endonym}` are circular",
                                ))
                                // @Task add the span of "the" counterpart component
                                .primary_span(component_exonym)
                                .report(&self.reporter),
                        ))
                    }
                    None => Err(Diagnostic::undefined_library_component(
                        component_endonym,
                        &package.name,
                    )
                    .report(&self.reporter)
                    .into()),
                };
            }
        }

        let manifest_path = package_path.join(manifest::FILE_NAME);
        let manifest_file = self.map().load(manifest_path.clone());
        let manifest_file = match manifest_file {
            Ok(file) => file,
            Err(error) => {
                // The dependency provider is most likely `filesystem` or `distribution`.
                // If the dependency provider is of the kind that downloads remote resources
                // and stores them locally, this probably means the local resources were
                // tampered with or a bug occurred.

                // @Task provide more context for transitive dependencies of the goal component
                // @Question code?
                // @Task provide more information when provider==distribution
                // @Question use endonym here instead? or use both?
                return Err(Diagnostic::error()
                    .message(format!(
                        "could not load the dependency `{component_exonym}`",
                    ))
                    .note(IOError(error, &manifest_path).to_string())
                    .primary_span(match &declaration.value.path {
                        Some(path) => path.span,
                        None => component_exonym.span,
                    })
                    .report(&self.reporter)
                    .into());
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self)?;

        if let Some(package_name) = &declaration.value.package
        && package_name.value != manifest.profile.name.value
        {
            // @Task message, @Task test
            return Err(
                Diagnostic::error()
                    .message("[declared package name does not match actual one]")
                    .primary_span(package_name)
                    .report(&self.reporter)
                    .into()
            );
        }

        // @Task assert version requirement (if any) is fulfilled

        let package = Package::from_manifest(manifest.profile, package_path.clone());
        let package_name = package.name.clone();
        let package = self.packages.insert(package);

        // @Task handle component privacy
        let Some((library_key, Spanned!(library, _))) = manifest.components.as_ref().and_then(|components| {
            components.value.iter().find_map(|(key, library)| {
                // @Beacon #primary_components_default_to_package_name
                // @Beacon @Beacon @Beacon @Bug don't default to the package_name !!!!!
                // `ext: { path: "..." }` should NOT expand to
                // `ext: { path: "...", component: ext }` the comp should just stay None / the dependency
                let name = key
                    .name
                    .as_ref()
                    .map_or_else(|| package_name.clone(), |name| name.value.clone());
                let type_ = key.type_.value;

                (name == *component_endonym.value && type_ == ComponentType::Library)
                    .then(|| ((name, type_), library))
            })
        }) else {
            return Err(
                Diagnostic::undefined_library_component(component_endonym, &package_name)
                    .report(&self.reporter)
                    .into(),
            );
        };

        // @Question should we prefill all components here too?
        self[package]
            .components
            .insert(library_key.clone(), PossiblyUnresolved::Unresolved);

        // transitive dependencies from the perspective of the dependent package
        let dependencies = self.resolve_dependencies(
            &library_key.0,
            package,
            &package_path,
            library.dependencies.as_ref(),
        )?;

        let library = self.components.insert_with(|index| {
            Component::new(
                ComponentMetadata::new(
                    library_key.0.clone(),
                    index,
                    package,
                    library
                        .path
                        .as_ref()
                        .map(|relative_path| package_path.join(relative_path)),
                    library_key.1,
                ),
                dependencies,
            )
        });

        self[package]
            .components
            .insert(library_key, PossiblyUnresolved::Resolved(library));

        Ok(library)
    }

    fn resolve_dependency_declaration(
        &self,
        Spanned!(declaration, span): &Spanned<DependencyDeclaration>,
        exonym: &Word,
        package_path: &Path,
    ) -> Result<Dependency> {
        // @Beacon @Task create a DependencyDeclaration' that's an enum not a struct with provider
        // being the discrimant ("parse, don't validate") and return it

        let provider = match declaration.provider {
            Some(provider) => provider.value,
            // infer the provider from the entries
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
            panic!();
        }

        if provider != DependencyProvider::Filesystem && declaration.path.is_some() {
            // @Beacon @Task report an error that those things are incompatible
            // @Task add test
            panic!();
        }

        match provider {
            DependencyProvider::Filesystem => match &declaration.path {
                Some(path) => Ok(Dependency::ForeignPackage(package_path.join(&path.value))),
                // @Task improve message
                None => Err(Diagnostic::error()
                    .message("dependency declaration does not have entry `path`")
                    .primary_span(span)
                    .with(|error| match declaration.provider.as_ref() {
                        // currently always present in this branch
                        Some(provider) => {
                            error.labeled_secondary_span(provider, "required by this")
                        }
                        None => error,
                    })
                    .report(&self.reporter)),
            },
            DependencyProvider::Distribution => {
                let component = declaration
                    .component
                    .as_ref()
                    .map_or(exonym, |name| &name.value);
                let path = distributed_packages_path().join(component.as_str());
                Ok(Dependency::ForeignPackage(path))
            }
            DependencyProvider::Package => Ok(Dependency::LocalComponent),
            DependencyProvider::Git | DependencyProvider::Registry => Err(Diagnostic::error()
                .message(format!(
                    "the dependency provider `{provider}` is not supported yet",
                ))
                // @Task better label! say how it was inferred!!
                .with(|error| match declaration.provider {
                    Some(provider) => error.primary_span(provider),
                    None => {
                        error.labeled_primary_span(span, format!("implies provider `{provider}`"))
                    }
                })
                .report(&self.reporter)),
        }
    }

    // @Task remove / replace
    fn resolve_primary_library<'m>(
        &self,
        components: Option<&'m Spanned<manifest::Components>>,
    ) -> Result<(&'m ComponentKey, &'m Spanned<ComponentManifest>)> {
        components
            .and_then(|components| {
                components.value.iter().find(|(key, _)| {
                    key.name.is_none() && key.type_.value == ComponentType::Library
                })
            })
            .ok_or_else(|| {
                // @Temporary diagnostic
                Diagnostic::error()
                    .message("no <primary> library found in package <name>")
                    .report(&self.reporter)
            })
    }

    fn finalize(/*mut*/ self) -> (Components, BuildSession) {
        // @Beacon @Beacon @Beacon @Bug no longer correct!
        let goal_component = self.components.last().unwrap();
        let goal_component_index = goal_component.index();
        // let _goal_package = &self.packages[goal_component.metadata.package];
        let goal_package_index = goal_component.metadata.package;
        // let _is_homonymous = |&component: &ComponentIndex| {
        //     goal_component.name() == self.components[component].name()
        // };

        // @Beacon @Beacon @Beacon @Task
        // let library_lookalike = goal_package
        //     .library
        //     .filter(|_| goal_component.is_executable())
        //     .filter(is_homonymous);
        // // @Note this is not extensible to multiple executable components
        // let executable_lookalike = goal_package
        //     .executables
        //     .get(0)
        //     .copied()
        //     .filter(|_| goal_component.is_library())
        //     .filter(is_homonymous);

        // if let Some(lookalike) = library_lookalike.or(executable_lookalike) {
        //     self.components[goal_component_index]
        //         .metadata
        //         .is_ambiguously_named_within_package = true;
        //     self.components[lookalike]
        //         .metadata
        //         .is_ambiguously_named_within_package = true;
        // }

        let session = BuildSession::new(
            self.packages,
            goal_component_index,
            goal_package_index,
            &self.map,
            self.reporter,
        );

        (self.components, session)
    }

    fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.map.read().unwrap()
    }

    fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.map.write().unwrap()
    }
}

enum DependencyResolutionError {
    ErasedNonFatal(ErasedReportedError),
    ErasedFatal(ErasedReportedError),
    // @Note component exists, not fully resolved yet
    UnresolvedLocalComponent(Spanned<Word>),
}

impl From<ErasedReportedError> for DependencyResolutionError {
    fn from(error: ErasedReportedError) -> Self {
        Self::ErasedNonFatal(error)
    }
}

fn parse_component_name_from_file_path(path: &Path, reporter: &Reporter) -> Result<Word> {
    if !crate::utility::has_file_extension(path, crate::FILE_EXTENSION) {
        Diagnostic::warning()
            .message("the source file does not have the file extension `lushui`")
            .report(reporter);
    }

    // @Question can the file stem ever be empty in our case?
    let name = path.file_stem().unwrap();
    // @Beacon @Beacon @Beacon @Task do not unwrap! provide custom error
    let name = name.to_str().unwrap();

    Word::parse(name.into()).map_err(|_| {
        // @Task DRY @Question is the common code justified?
        // @Question isn't this function used in such a way that it's
        //     "component and package name"?
        Diagnostic::error()
            .code(Code::E036)
            .message(format!("the component name `{name}` is not a valid word"))
            .report(reporter)
    })
}

impl Index<ComponentIndex> for BuildQueue {
    type Output = Component;

    fn index(&self, index: ComponentIndex) -> &Self::Output {
        &self.components[index]
    }
}

impl IndexMut<ComponentIndex> for BuildQueue {
    fn index_mut(&mut self, index: ComponentIndex) -> &mut Self::Output {
        &mut self.components[index]
    }
}

impl Index<PackageIndex> for BuildQueue {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildQueue {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.packages[index]
    }
}

/// The path to the folder of packages shipped with the compiler.
pub(crate) fn distributed_packages_path() -> PathBuf {
    // @Task make this configurable via CLI option & env var & config file

    const DISTRIBUTED_LIBRARIES_FOLDER: &str = "libraries";

    Path::new(env!("CARGO_MANIFEST_DIR")).join(DISTRIBUTED_LIBRARIES_FOLDER)
}

pub(crate) fn core_package_path() -> PathBuf {
    distributed_packages_path().join(CORE_PACKAGE_NAME)
}

pub(crate) const CORE_PACKAGE_NAME: &str = "core";

pub(crate) fn core_package_name() -> Word {
    Word::new_unchecked("core".into())
}

enum Dependency {
    ForeignPackage(PathBuf),
    LocalComponent,
}

impl Diagnostic {
    fn undefined_library_component(name: Spanned<&Word>, package: &Word) -> Self {
        // @Task improve message, should we special-case component name = package name?
        // @Task add note if component(s) with the given library name exist and that they cannot
        //       be used as a dependency to another component
        Self::error()
            .message(format!(
                "the library component `{name}` is not defined in package `{package}`"
            ))
            .primary_span(name)
    }
}
