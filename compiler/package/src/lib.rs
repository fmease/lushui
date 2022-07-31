//! The package and component resolver.
#![feature(default_free_fn, let_else, try_trait_v2)]

use diagnostics::{reporter::ErasedReportedError, Diagnostic, ErrorCode, Reporter};
use error::Result;
use index_map::IndexMap;
use lexer::WordExt;
pub use manifest::FILE_NAME as MANIFEST_FILE_NAME;
use manifest::{DependencyDeclaration, DependencyProvider, PackageManifest, PackageProfile};
use metadata::Record;
use session::{
    BuildSession, Component, ComponentType, Components, Package, PackageIndex,
    PossiblyUnresolvedComponent::*,
};
use span::{SourceMap, Spanned, WeaklySpanned};
use std::{
    default::default,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use token::Word;
use utilities::{
    cycle::find_cycles_by_key, pluralize, ComponentIndex, Conjunction, FormatWithPathExt, HashMap,
    ListingExt, QuoteExt, FILE_EXTENSION,
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
    filter: ComponentFilter,
    map: &Arc<RwLock<SourceMap>>,
    reporter: Reporter,
) -> Result<(Components, BuildSession)> {
    let path = match path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            // @Task better message e.g. mention manifest
            return Err(Diagnostic::error()
                .message("could not load the package")
                .note(error.format(Some(path)))
                .report(&reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_package(&path, filter)?;
    Ok(queue.finalize())
}

/// Resolve the components and dependencies of a file given its path without building anything.
pub fn resolve_file(
    path: &Path,
    content: Option<Arc<String>>,
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
                .note(error.format(Some(path)))
                .report(&reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_file(path, content, component_type, no_core)?;
    Ok(queue.finalize())
}

trait PackageExt {
    fn from_manifest(profile: PackageProfile, path: PathBuf) -> Self;
}

impl PackageExt for Package {
    fn from_manifest(profile: PackageProfile, path: PathBuf) -> Self {
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
    components: Components,
    packages: IndexMap<PackageIndex, Package>,
    /// The corresponding package of the components.
    component_packages: HashMap<ComponentIndex, PackageIndex>,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildQueue {
    fn new(map: &Arc<RwLock<SourceMap>>, reporter: Reporter) -> Self {
        Self {
            components: default(),
            packages: default(),
            component_packages: default(),
            map: map.clone(),
            reporter,
        }
    }
}

impl BuildQueue {
    fn resolve_package(&mut self, package_path: &Path, filter: ComponentFilter) -> Result {
        let manifest_path = package_path.join(manifest::FILE_NAME);
        let manifest_file = self.map().load(manifest_path.clone(), None);
        let manifest_file = match manifest_file {
            Ok(file) => file,
            Err(error) => {
                return Err(Diagnostic::error()
                    // @Question code?
                    .message("could not load the package")
                    .note(error.format(Some(&manifest_path)))
                    .report(&self.reporter));
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self)?;
        let package = Package::from_manifest(manifest.profile, package_path.to_owned());
        let package = self.packages.insert(package);

        // @Task use Health "2.0" instead
        let mut health = None::<ErasedReportedError> /* Untainted */;

        // @Temporary
        if !filter.is_empty() {
            return Err(Diagnostic::error()
                .message("component filters are not supported yet")
                .report(&self.reporter));
        }

        // @Task apply the ComponentFilter here!
        // @Note it's not that simple. we also need to check (local) components
        //       that are (local component) dependencies of the
        //       filtered / included components.
        if let Some(component_worklist) = manifest.components {
            let mut component_worklist: HashMap<_, _> = component_worklist
                .bare
                .into_iter()
                .map(|(name, component)| (name, (component, None)))
                .collect();

            self[package].components.extend(
                component_worklist
                    .iter()
                    .map(|(name, _)| (name.bare.clone(), Unresolved)),
            );

            while !component_worklist.is_empty() {
                let amount_unresolved_components = component_worklist.len();
                let mut unresolved_components = HashMap::default();

                for (name, (component, _)) in component_worklist {
                    use DependencyResolutionError::*;
                    let dependencies = match self.resolve_dependencies(
                        &name.bare,
                        package,
                        package_path,
                        component.bare.dependencies.as_ref(),
                    ) {
                        Ok(dependencies) => dependencies,
                        Err(UnresolvedLocalComponent(dependency_name)) => {
                            unresolved_components.insert(name, (component, Some(dependency_name)));
                            continue;
                        }
                        Err(ErasedFatal(error) | ErasedNonFatal(error)) => {
                            // @Task use taint() "2.0" instead
                            health = Some(error);
                            continue;
                        }
                    };

                    let type_ = component.bare.type_;

                    if type_.bare != ComponentType::Library
                        && type_.bare != ComponentType::Executable
                    {
                        // @Task use health.taint(...) "2.0" instead
                        health = Some(
                            Diagnostic::error()
                                .message(format!(
                                    "the component type ‘{type_}’ is not supported yet",
                                ))
                                .primary_span(type_)
                                .report(&self.reporter),
                        );
                    }

                    let component = self.components.insert_with(|index| {
                        Component::new(
                            name.bare.clone(),
                            index,
                            component
                                .bare
                                .path
                                .as_ref()
                                .map(|relative_path| package_path.join(relative_path)),
                            None,
                            type_.bare,
                            dependencies,
                        )
                    });

                    self.register_package_component(package, name.bare, component);
                }

                // resolution stalled; therefore there are cyclic components
                if unresolved_components.len() == amount_unresolved_components {
                    // @Task if the cycle is of size one, add a note that it is referencing itself (which might be
                    // non-obvious from the span we currently highlight).
                    // @Task if there are other local components with
                    // the same name but with a differing type, add a note that only library components are being looked
                    // at during dependency resolution (clarifying that one cannot depend on non-library components)

                    for cycle in find_cycles_by_key::<&Word, Spanned<&Word>>(
                        &unresolved_components
                            .iter()
                            .map(|(dependent, (_, dependency))| {
                                (&dependent.bare, dependency.as_ref().unwrap().as_ref())
                            })
                            .collect(),
                        |name| &name.bare,
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

    fn resolve_file(
        &mut self,
        file_path: PathBuf,
        content: Option<Arc<String>>,
        type_: ComponentType,
        no_core: bool,
    ) -> Result {
        let name = parse_component_name_from_file_path(&file_path, &self.reporter)?;
        let mut dependencies = HashMap::default();

        if !no_core {
            // @Note this currently duplicates a lot of stuff from Self::resolve_dependencies
            // in fact, all the logic was copied over from there and manually adjusted
            // @Task abstract over this
            // @Update @Task don't touch packages here at all! just go straight to the
            //               component (and assume that it doesn't have any deps)

            let core_package_path = session::core_package_path();
            let core_manifest_path = core_package_path.join(manifest::FILE_NAME);
            let core_manifest_file = self.map().load(core_manifest_path.clone(), None);
            let core_manifest_file = match core_manifest_file {
                Ok(file) => file,
                Err(error) => {
                    return Err(Diagnostic::error()
                        // @Question code?
                        .message("could not load the package ‘core’")
                        .note(error.format(Some(&core_manifest_path)))
                        .report(&self.reporter));
                }
            };

            let core_manifest = PackageManifest::parse(core_manifest_file, self)?;
            let core_package =
                Package::from_manifest(core_manifest.profile, core_package_path.to_owned());
            let core_package = self.packages.insert(core_package);

            let core_package_name = session::core_package_name();
            // @Task handle errors properly here!
            let library = core_manifest
                .components
                .as_ref()
                .unwrap()
                .bare
                .get(&core_package_name)
                .unwrap()
                .as_ref();
            let library = library.bare;

            let core_library_component_path = library
                .path
                .as_ref()
                .map(|relative_path| core_package_path.join(relative_path));

            self[core_package]
                .components
                .insert(core_package_name.clone(), Unresolved);

            let transitive_dependencies = self
                .resolve_dependencies(
                    &core_package_name,
                    core_package,
                    core_package_path,
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
                    core_package_name.clone(),
                    index,
                    core_library_component_path,
                    None,
                    ComponentType::Library,
                    transitive_dependencies,
                )
            });

            self.register_package_component(core_package, core_package_name.clone(), library);

            dependencies.insert(core_package_name, library);
        }

        self.components.insert_with(|index| {
            Component::new(
                name.clone(),
                index,
                Spanned::new(default(), file_path),
                content,
                type_,
                dependencies,
            )
        });

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

        for (dependency_exonym, dependency_declaration) in &dependencies.bare {
            match self.resolve_dependency(
                dependent_component_name,
                dependent_package_index,
                dependent_package_path,
                dependency_exonym,
                dependency_declaration,
            ) {
                Ok(dependency) => {
                    resolved_dependencies.insert(dependency_exonym.bare.clone(), dependency);
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
            &component_exonym.bare,
            dependent_package_path,
        ) {
            Ok(dependency) => dependency,
            Err(error) => return Err(error.into()),
        };

        if let Some(version) = &declaration.bare.version {
            // @Task message, @Task test
            return Err(Diagnostic::error()
                .message("[version requirement field not supported yet]")
                .primary_span(version)
                .report(&self.reporter)
                .into());
        }

        let component_endonym = declaration
            .bare
            .component
            .as_ref()
            .map_or(component_exonym.as_ref().strong(), Spanned::as_ref);

        let mut package_path = match dependency {
            Dependency::ForeignPackage(path) => path,
            Dependency::LocalComponent => {
                let package = &self[dependent_package];

                return match package.components.get(component_endonym.bare) {
                    // @Task don't use a guard: it muddies/worsens the error cause. instead, report a more specific error
                    Some(&Resolved(component)) if self[component].is_library() => Ok(component),
                    Some(Unresolved) => Err(DependencyResolutionError::UnresolvedLocalComponent(
                        component_endonym.cloned(),
                    )),
                    _ => Err(
                        undefined_library_component_error(component_endonym, &package.name)
                            .report(&self.reporter)
                            .into(),
                    ),
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
                // @Beacon @Task add a diag note for size-one cycles of the form `{ path: "." }` (etc) and
                //               recommend the dep-provider `package` (…)

                // @Beacon @Note this probably won't scale to our new order-independent component resolver (maybe)
                // if so, consider not throwing a cycle error (here / unconditionally)
                // @Beacon @Task handle component privacy here
                return match package.components.get(component_endonym.bare) {
                    // @Task don't use a guard: it muddies/worsens the error cause. instead, report a more specific error
                    Some(&Resolved(component)) if self[component].is_library() => Ok(component),
                    // @Beacon @Task instead of reporting an error here, return a "custom"
                    //               DepRepError::UnresolvedComponent here (add a param/flag to differentiate
                    //               it from the case above) and @Task smh accumulate "inquirer"s so we can
                    //               find_cycles later on for scalable diagnostics (> 3 packages that are cyclic)
                    Some(Unresolved) => {
                        // @Note the message does not scale to more complex cycles (e.g. a cycle of three components)
                        // @Task provide more context for transitive dependencies of the goal component
                        // @Task code

                        // @Question does this need to be fatal?
                        Err(DependencyResolutionError::ErasedFatal(
                            Diagnostic::error()
                                .message(format!(
                                    "the components ‘{dependent_component_name}’ and ‘{component_endonym}’ are circular",
                                ))
                                // @Task add the span of "the" counterpart component
                                .primary_span(component_exonym)
                                .report(&self.reporter),
                        ))
                    }
                    _ => Err(
                        undefined_library_component_error(component_endonym, &package.name)
                            .report(&self.reporter)
                            .into(),
                    ),
                };
            }
        }

        let manifest_path = package_path.join(manifest::FILE_NAME);
        let manifest_file = self.map().load(manifest_path.clone(), None);
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
                        "could not load the dependency ‘{component_exonym}’",
                    ))
                    .note(error.format(Some(&manifest_path)))
                    .primary_span(match &declaration.bare.path {
                        Some(path) => path.span,
                        None => component_exonym.span,
                    })
                    .report(&self.reporter)
                    .into());
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self)?;

        if let Some(package_name) = &declaration.bare.package
        && package_name.bare != manifest.profile.name.bare
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

        let Some(library) = manifest.components.as_ref().and_then(|components| {
            // @Task handle component privacy
            components.bare.get(&component_endonym.cloned().weak())
                .filter(|component| component.bare.type_.bare == ComponentType::Library)
        }) else {
            return Err(
                undefined_library_component_error(component_endonym, &package_name)
                    .report(&self.reporter)
                    .into(),
            );
        };

        let library = &library.bare;
        let name = component_endonym.bare;

        let library_component_path = library
            .path
            .as_ref()
            .map(|relative_path| package_path.join(relative_path));

        // @Question should we prefill all components here too?
        self[package].components.insert(name.clone(), Unresolved);

        // transitive dependencies from the perspective of the dependent package
        let dependencies =
            self.resolve_dependencies(name, package, &package_path, library.dependencies.as_ref())?;

        let library = self.components.insert_with(|index| {
            Component::new(
                name.clone(),
                index,
                library_component_path,
                None,
                library.type_.bare,
                dependencies,
            )
        });

        self.register_package_component(package, name.clone(), library);

        Ok(library)
    }

    fn register_package_component(
        &mut self,
        package: PackageIndex,
        name: Word,
        component: ComponentIndex,
    ) {
        self[package].components.insert(name, Resolved(component));
        self.component_packages.insert(component, package);
    }

    fn resolve_dependency_declaration(
        &self,
        Spanned!(span, declaration): &Spanned<DependencyDeclaration>,
        exonym: &Word,
        package_path: &Path,
    ) -> Result<Dependency> {
        // @Beacon @Task create a DependencyDeclaration' that's an enum not a struct with provider
        // being the discrimant ("parse, don't validate") and return it

        let provider = match declaration.provider {
            Some(provider) => provider.bare,
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
                Some(path) => Ok(Dependency::ForeignPackage(package_path.join(&path.bare))),
                // @Task improve message
                None => Err(Diagnostic::error()
                    .message("dependency declaration does not have entry ‘path’")
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
                    .map_or(exonym, |name| &name.bare);
                let path = session::distributed_packages_path().join(component.as_str());
                Ok(Dependency::ForeignPackage(path))
            }
            DependencyProvider::Package => Ok(Dependency::LocalComponent),
            DependencyProvider::Git | DependencyProvider::Registry => Err(Diagnostic::error()
                .message(format!(
                    "the dependency provider ‘{provider}’ is not supported yet",
                ))
                // @Task better label! say how it was inferred!!
                .with(|error| match declaration.provider {
                    Some(provider) => error.primary_span(provider),
                    None => {
                        error.labeled_primary_span(span, format!("implies provider ‘{provider}’"))
                    }
                })
                .report(&self.reporter)),
        }
    }

    fn finalize(self) -> (Components, BuildSession) {
        // @Bug no longer correct!
        // @Task introduce proper concept of target components replacing the
        // current concept of a goal component
        let goal_component = self.components.last().unwrap();
        let goal_component_index = goal_component.index();

        let session = BuildSession::new(
            self.packages,
            self.component_packages,
            goal_component_index,
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

#[derive(Default)]
pub struct ComponentFilter {
    pub names: Vec<Word>,
    pub types: Vec<ComponentType>,
}

impl ComponentFilter {
    fn is_empty(&self) -> bool {
        self.names.is_empty() && self.types.is_empty()
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
    if !utilities::has_file_extension(path, FILE_EXTENSION) {
        Diagnostic::warning()
            .message("the source file does not have the file extension ‘lushui’")
            .report(reporter);
    }

    // @Question can the file stem ever be empty in our case?
    let name = path.file_stem().unwrap();
    // @Beacon @Task do not unwrap! provide custom error
    let name = name.to_str().unwrap();

    Word::parse(name.into()).map_err(|_| {
        // @Task DRY @Question is the common code justified?
        // @Question isn't this function used in such a way that it's
        //     "component and package name"?
        Diagnostic::error()
            .code(ErrorCode::E036)
            .message(format!("the component name ‘{name}’ is not a valid word"))
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

enum Dependency {
    ForeignPackage(PathBuf),
    LocalComponent,
}

fn undefined_library_component_error(name: Spanned<&Word>, package: &Word) -> Diagnostic {
    // @Task improve message, should we special-case component name = package name?
    // @Task add note if component(s) with the given library name exist and that they cannot
    //       be used as a dependency to another component
    Diagnostic::error()
        .message(format!(
            "the library component ‘{name}’ is not defined in package ‘{package}’"
        ))
        .primary_span(name)
}
