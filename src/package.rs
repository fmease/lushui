//! The package and component resolver.

use crate::{
    component::{Component, ComponentIndex, ComponentMetadata, ComponentType, Components},
    diagnostics::{reporter::ErrorReported, Code, Diagnostic, Reporter},
    error::{Health, OkIfUntaintedExt, Result},
    metadata::{key_content_span, Map},
    session::BuildSession,
    span::{SourceMapCell, Spanned},
    syntax::Word,
    utility::{HashMap, IOError},
};
use index_map::IndexMap;
pub use manifest::PackageManifest;
use manifest::{ComponentManifest, PackageProfile, Provider};
pub(crate) use manifest::{DependencyDeclaration, Version};
use std::{
    default::default,
    fmt,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
};

mod manifest;

/// Resolve all components and package dependencies of a package given the path to its folder without building anything.
pub fn resolve_package(
    package_path: &Path,
    map: SourceMapCell,
    reporter: &Reporter,
) -> Result<(Components, BuildSession)> {
    let package_path = match package_path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            // @Task better message e.g. mention manifest
            return Err(Diagnostic::error()
                .message("could not load the package")
                .note(IOError(error, package_path).to_string())
                .report(reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_package(&package_path)?;
    Ok(queue.finalize())
}

/// Resolve the components and dependencies of a file given its path without building anything.
pub fn resolve_file(
    file_path: &Path,
    component_type: ComponentType,
    no_core: bool,
    map: SourceMapCell,
    reporter: &Reporter,
) -> Result<(Components, BuildSession)> {
    let file_path = match file_path.canonicalize() {
        Ok(path) => path,
        Err(error) => {
            // @Task better message
            return Err(Diagnostic::error()
                .message("could not load the file")
                .note(IOError(error, file_path).to_string())
                .report(reporter));
        }
    };

    let mut queue = BuildQueue::new(map, reporter);
    queue.resolve_file(file_path, component_type, no_core)?;
    Ok(queue.finalize())
}

/// A collection of components and some metadata.
///
/// More concretely, it consists of zero or more executable components
/// and of zero or one library component but of always at least one component.
/// The most important metadatum is the list of dependencies (external components).
#[derive(Debug)]
pub struct Package {
    /// The name of the package.
    ///
    /// The library and the default executable component share this name
    /// unless overwritten in their manifests.
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
    pub(crate) version: Version,
    pub(crate) description: String,
    components: HashMap<Word, PossiblyUnresolved<ComponentIndex>>,
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

struct BuildQueue<'r> {
    /// The components which have not been built yet.
    components: Components,
    packages: IndexMap<PackageIndex, Package>,
    map: SourceMapCell,
    reporter: &'r Reporter,
}

impl<'r> BuildQueue<'r> {
    fn new(map: SourceMapCell, reporter: &'r Reporter) -> Self {
        Self {
            components: default(),
            packages: default(),
            map,
            reporter,
        }
    }
}

impl BuildQueue<'_> {
    fn resolve_package(&mut self, package_path: &Path) -> Result {
        let manifest_path = package_path.join(PackageManifest::FILE_NAME);
        let manifest_file = match self.map.borrow_mut().load(manifest_path.clone()) {
            Ok(file) => file,
            Err(error) => {
                return Err(Diagnostic::error()
                    // @Question code?
                    .message("could not load the package")
                    .note(IOError(error, &manifest_path).to_string())
                    .report(self.reporter));
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self.map.clone(), self.reporter)?;
        let package = Package::from_manifest(manifest.profile, package_path.to_owned());
        let package = self.packages.insert(package);

        let mut health = Health::Untainted;

        // @Beacon @Beacon @Beacon @Task we don't to unconditionally loop through all those components,
        // I *think*, only those that are relevant: this is driven by the CLI: Esp. if the users passes
        // certain flag (cf. --exe, --lib etc. flags passed to cargo build)
        // @Note but maybe we should still check for correctness of irrelevant components, check what cargo does!
        if let Some(components) = manifest.components {
            for component in components.value {
                let name = component
                    .name
                    .map_or_else(|| self[package].name.clone(), |name| name.value);

                self[package]
                    .components
                    .insert(name.clone(), PossiblyUnresolved::Unresolved);

                let dependencies = match self.resolve_dependencies(
                    &name,
                    package_path,
                    component.dependencies.as_ref(),
                ) {
                    Ok(dependencies) => dependencies,
                    Err(_) => {
                        health.taint();
                        continue;
                    }
                };

                let component = self.components.insert_with(|index| {
                    Component::new(
                        ComponentMetadata::new(
                            name.clone(),
                            index,
                            package,
                            component
                                .path
                                .map(|relative_path| package_path.join(relative_path)),
                            component.type_.value,
                        ),
                        dependencies,
                    )
                });

                self[package]
                    .components
                    .insert(name, PossiblyUnresolved::Resolved(component));
            }
        }

        Result::ok_if_untainted((), health)
    }

    fn resolve_file(
        &mut self,
        file_path: PathBuf,
        component_type: ComponentType,
        no_core: bool,
    ) -> Result {
        // package *and* component name
        let name = parse_component_name_from_file_path(&file_path, self.reporter)?;

        let package = Package::file(name.clone(), file_path.clone());
        let package = self.packages.insert(package);

        self[package]
            .components
            .insert(name.clone(), PossiblyUnresolved::Unresolved);

        let mut dependencies = HashMap::default();

        if !no_core {
            // @Note this currently duplicates a lot of stuff from Self::resolve_dependencies
            // in fact, all the logic was copied over from there and manually adjusted
            // @Task abstract over this

            let core_package_path = core_package_path();
            let core_manifest_path = core_package_path.join(PackageManifest::FILE_NAME);
            let core_manifest_file = match self.map.borrow_mut().load(core_manifest_path.clone()) {
                Ok(file) => file,
                Err(error) => {
                    return Err(Diagnostic::error()
                        // @Question code?
                        .message("could not load the package `core`")
                        .note(IOError(error, &core_manifest_path).to_string())
                        .report(self.reporter));
                }
            };

            let core_manifest =
                PackageManifest::parse(core_manifest_file, self.map.clone(), self.reporter)?;
            let core_package =
                Package::from_manifest(core_manifest.profile, core_package_path.clone());
            let core_package = self.packages.insert(core_package);

            let core_package_name = core_package_name();
            let library = self.resolve_primary_library(core_manifest.components.as_ref())?;

            self[core_package]
                .components
                .insert(core_package_name.clone(), PossiblyUnresolved::Unresolved);

            let transitive_dependencies = match self.resolve_dependencies(
                &core_package_name,
                &core_package_path,
                library.dependencies.as_ref(),
            ) {
                Ok(dependencies) => dependencies,
                Err(_) => {
                    return Err(ErrorReported::new_unchecked());
                }
            };

            let library = self.components.insert_with(|index| {
                Component::new(
                    ComponentMetadata::new(
                        core_package_name.clone(),
                        index,
                        core_package,
                        library
                            .path
                            .as_ref()
                            .map(|relative_path| core_package_path.join(relative_path)),
                        library.type_.value,
                    ),
                    transitive_dependencies,
                )
            });

            self[core_package].components.insert(
                core_package_name.clone(),
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
                    component_type,
                ),
                dependencies,
            )
        });

        self[package]
            .components
            .insert(name, PossiblyUnresolved::Resolved(component));

        Ok(())
    }

    // @Beacon @Task implement sublibraries!
    fn resolve_dependencies(
        &mut self,
        component_name: &Word,
        package_path: &Path,
        dependencies: Option<&Spanned<Map<Word, Spanned<DependencyDeclaration>>>>,
    ) -> Result<HashMap<Word, ComponentIndex>> {
        let Some(dependencies) = dependencies else {
            return Ok(HashMap::default());
        };

        let mut resolved_dependencies = HashMap::default();
        let mut health = Health::Untainted;

        for (dependency_exonym, dependency_declaration) in &dependencies.value {
            let Ok(mut dependency_path) = self.resolve_dependency_declaration(
                &dependency_declaration,
                &dependency_exonym.value,
                &package_path,
            ) else {
                health.taint();
                continue;
            };

            // deduplicate packages by absolute path and check for circular components
            if let Ok(path) = dependency_path.canonicalize() {
                // error case handled later via `SourceMap::load` for uniform error messages and to achieve DRY
                // @Update ^^^ we no longer have that formatting code in place, can we improve this?
                dependency_path = path;

                if let Some(dependency) = self
                    .packages
                    .values()
                    .find(|package| package.path == dependency_path)
                {
                    // @Note we cannot handle sublibraries yet (secondary libraries)
                    // only find the primary library for now
                    // @Beacon @Beacon @Beacon @Bug very much not correct! @Task properly find the (primary)
                    // library in the already-resolved package `dependency`
                    // @Beacon @Temporary panic
                    let (_, &primary_library) = dbg!(&dependency)
                        .components
                        .iter()
                        .find(|&(name, _)| name == &dependency.name)
                        .unwrap_or_else(|| {
                            panic!(
                                "already resolved package `{}` has no library",
                                dependency.name
                            )
                        });

                    match primary_library {
                        PossiblyUnresolved::Resolved(primary_library) => {
                            resolved_dependencies
                                .insert(dependency_exonym.value.clone(), primary_library);
                            continue;
                        }
                        PossiblyUnresolved::Unresolved => {
                            let map = self.map.borrow();

                            // @Note the message does not scale to more complex cycles (e.g. a cycle of three components)
                            // @Task provide more context for transitive dependencies of the goal component
                            // @Task code
                            return Err(Diagnostic::error()
                                .message(format!(
                                    "the components `{component_name}` and `{}` are circular",
                                    // @Task don't use the package's name but the (library) component's one!
                                    dependency.name
                                ))
                                .primary_span(key_content_span(dependency_exonym, &map))
                                // @Beacon @Beacon @Beacon @Task
                                // .primary_span(
                                //     // @Beacon @Bug probably does not work if exonym != endonym (but that can be fixed easily!)
                                //     key_content_span(
                                //         dependency
                                //             .dependency_manifest
                                //             .as_ref()
                                //             .unwrap()
                                //             .value
                                //             .keys()
                                //             .find(|key| key.value == self[package_index].name)
                                //             .unwrap(),
                                //         &map,
                                //     ),
                                // )
                                .report(self.reporter));
                        }
                    }
                }
            }

            let dependency_manifest_path = dependency_path.join(PackageManifest::FILE_NAME);
            let dependency_manifest_file =
                match self.map.borrow_mut().load(dependency_manifest_path.clone()) {
                    Ok(file) => file,
                    Err(error) => {
                        // @Task provide more context for transitive dependencies of the goal component
                        // @Question code?
                        let diagnostic = Diagnostic::error()
                            .message(format!(
                                "could not load the dependency `{dependency_exonym}`",
                            ))
                            .note(IOError(error, &dependency_manifest_path).to_string());
                        let diagnostic = match &dependency_declaration.value.path {
                            Some(path) => diagnostic.primary_span(path.span.trim(1)), // trimming quotes
                            None => diagnostic.primary_span(key_content_span(
                                dependency_exonym,
                                &self.map.borrow(),
                            )),
                        };
                        diagnostic.report(self.reporter);
                        health.taint();
                        continue;
                    }
                };

            let dependency_manifest =
                PackageManifest::parse(dependency_manifest_file, self.map.clone(), self.reporter)?;

            let alleged_dependency_endonym = dependency_declaration
                .value
                .name
                .as_ref()
                .map_or(&dependency_exonym.value, |name| &name.value);

            if alleged_dependency_endonym != &dependency_manifest.profile.name.value {
                // @Task improve error message, add highlight
                // @Task use primary span (endonym here (key `name` or map key)) &
                // secondary span (endonym in the dependency's package manifest file)
                // @Task branch on existence of the key `name`
                // @Task maybe instead of returning early, we could just go forwards with the the exonym
                return Err(Diagnostic::error()
                    .message("library name does not match")
                    .report(self.reporter));
            }

            // @Task assert version requirement (if any) is fulfilled
            // @Update just report an "unimplemented" error for now

            let dependency_package =
                Package::from_manifest(dependency_manifest.profile, dependency_path.clone());
            let dependency_package_name = dependency_package.name.clone();
            let dependency_package = self.packages.insert(dependency_package);

            // @Note we cannot handle sublibraries yet (secondary libraries)
            // only find the primary library for now
            let Ok(library) =
                self.resolve_primary_library(dependency_manifest.components.as_ref()) else {
                    health.taint();
                    continue;
                };

            // @Temporary procedure here (DRY! see resolve_package):
            // @Beacon @Beacon @Beacon @Question the name of the primary library is ALWAYS
            // the same as the package name, what are we doing here???
            let library_name = library
                .name
                .as_ref()
                .map_or(dependency_package_name, |name| name.value.clone());

            self[dependency_package]
                .components
                .insert(library_name.clone(), PossiblyUnresolved::Unresolved);

            let transitive_dependencies = match self.resolve_dependencies(
                &library_name,
                &dependency_path,
                library.dependencies.as_ref(),
            ) {
                Ok(dependencies) => dependencies,
                Err(_) => {
                    health.taint();
                    continue;
                }
            };

            let library = self.components.insert_with(|index| {
                Component::new(
                    ComponentMetadata::new(
                        library_name.clone(),
                        index,
                        dependency_package,
                        library
                            .path
                            .as_ref()
                            .map(|relative_path| dependency_path.join(relative_path)),
                        library.type_.value,
                    ),
                    transitive_dependencies,
                )
            });

            self[dependency_package]
                .components
                .insert(library_name, PossiblyUnresolved::Resolved(library));

            resolved_dependencies.insert(dependency_exonym.value.clone(), library);
        }

        Result::ok_if_untainted(resolved_dependencies, health)
    }

    fn resolve_dependency_declaration(
        &self,
        Spanned!(declaration, span): &Spanned<DependencyDeclaration>,
        exonym: &Word,
        package_path: &Path,
    ) -> Result<PathBuf> {
        let provider = match &declaration.provider {
            Some(provider) => provider.value,
            // infer the provider from the other fields
            None => {
                if declaration.path.is_some() {
                    Provider::Filesystem
                } else if declaration.version.is_some() {
                    Provider::Registry
                } else {
                    // @Question should we infer Package if no other fields + the current package
                    // contains a sublibrary with the name?

                    // @Task flesh out the message
                    return Err(Diagnostic::error()
                        .message("invalid dependency declaration")
                        .primary_span(span)
                        .report(self.reporter));
                }
            }
        };

        match provider {
            Provider::Filesystem => match &declaration.path {
                Some(path) => Ok(package_path.join(&path.value)),
                // @Task improve message
                None => Err(Diagnostic::error()
                    .message("dependency declaration is missing field `path`")
                    .primary_span(span)
                    // currently always present in this branch
                    .if_present(declaration.provider.as_ref(), |this, provider| {
                        this.labeled_secondary_span(provider, "required by this")
                    })
                    .report(self.reporter)),
            },
            Provider::Distribution => {
                let name = declaration.name.as_ref().map_or(exonym, |name| &name.value);
                Ok(distributed_packages_path().join(name.as_str()))
            }
            Provider::Package | Provider::Git | Provider::Registry => Err(Diagnostic::error()
                .message(format!(
                    "dependency provider `{provider}` is not supported yet"
                ))
                // @Task better label! say how it was inferred!!
                .if_(declaration.provider.is_none(), |this| {
                    this.labeled_primary_span(span, format!("implies provider `{provider}`"))
                })
                .if_present(declaration.provider.as_ref(), |this, provider| {
                    this.primary_span(provider)
                })
                .report(self.reporter)),
        }
    }

    // @Task generalize to primary+secondary libraries
    fn resolve_primary_library<'c>(
        &self,
        components: Option<&'c Spanned<Vec<ComponentManifest>>>,
    ) -> Result<&'c ComponentManifest> {
        components
            .and_then(|components| {
                components
                    .value
                    .iter()
                    .find(|component| component.name.is_none())
            })
            .ok_or_else(|| {
                // @Temporary diagnostic
                Diagnostic::error()
                    .message("no (primary) library found in package XXX")
                    .report(self.reporter)
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

        let session = BuildSession::new(self.packages, goal_component_index, goal_package_index);

        (self.components, session)
    }
}

// impl Diagnostic {
//     fn dependent_package_does_not_contain_a_library(dependency: &str, dependent: &str) -> Self {
//         // @Task provide more context for transitive dependencies of the goal component
//         // @Task code
//         // @Beacon @Task span
//         // @Update better message: dependent (?) package does not contain a library
//         Self::error().message(format!(
//             "dependency `{dependency}` of `{dependent}` is not a library"
//         ))
//     }
// }

impl Index<ComponentIndex> for BuildQueue<'_> {
    type Output = Component;

    fn index(&self, index: ComponentIndex) -> &Self::Output {
        &self.components[index]
    }
}

impl IndexMut<ComponentIndex> for BuildQueue<'_> {
    fn index_mut(&mut self, index: ComponentIndex) -> &mut Self::Output {
        &mut self.components[index]
    }
}

impl Index<PackageIndex> for BuildQueue<'_> {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildQueue<'_> {
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

pub fn find_package(path: &Path) -> Option<&Path> {
    let manifest_path = path.join(PackageManifest::FILE_NAME);

    if manifest_path.exists() {
        Some(path)
    } else {
        find_package(path.parent()?)
    }
}

pub(crate) fn parse_component_name_from_file_path(
    path: &Path,
    reporter: &Reporter,
) -> Result<Word> {
    if !crate::utility::has_file_extension(path, crate::FILE_EXTENSION) {
        Diagnostic::warning()
            .message("missing or non-standard file extension")
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
