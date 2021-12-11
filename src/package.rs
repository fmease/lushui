//! The package and crate resolver and future package manager.

use crate::{
    diagnostics::{Diagnostic, Reporter},
    error::{Health, ReportedExt, Result},
    format::IOError,
    metadata::content_span_of_key,
    resolver::Crate,
    span::{SharedSourceMap, Spanned, WeaklySpanned},
    syntax::CrateName,
    utility::HashMap,
    FILE_EXTENSION,
};
use index_map::IndexMap;
use manifest::PackageProfile;
pub use manifest::{BinaryManifest, DependencyManifest, LibraryManifest, PackageManifest, Version};
pub use session::BuildSession;
use std::{
    default::default,
    fmt,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
    str::FromStr,
};

pub mod session;

#[derive(PartialEq, Eq, Clone, Copy, index_map::Index)]
pub struct PackageIndex(pub usize);

impl fmt::Debug for PackageIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}p", self.0)
    }
}

pub struct BuildQueue<'r> {
    /// The crates which have not been built yet.
    crates: IndexMap<CrateIndex, Crate>,
    packages: IndexMap<PackageIndex, Package>,
    map: SharedSourceMap,
    reporter: &'r Reporter,
}

impl<'r> BuildQueue<'r> {
    pub fn new(map: SharedSourceMap, reporter: &'r Reporter) -> Self {
        Self {
            crates: default(),
            packages: default(),
            map,
            reporter,
        }
    }
}

impl BuildQueue<'_> {
    // @Note this does not scale to single-file packages with --extern dependencies
    fn enqueue_dependencies(
        &mut self,
        package_index: PackageIndex,
    ) -> Result<HashMap<CrateName, CrateIndex>> {
        let package_path = &self[package_index].path.clone();
        let mut resolved_dependencies = HashMap::default();
        let mut health = Health::Untainted;

        let dependencies = match self[package_index].dependency_manifest.clone() {
            Some(dependencies) => dependencies.value,
            None => return Ok(resolved_dependencies),
        };

        for (dependency_exonym, unresolved_dependency) in &dependencies {
            let mut dependency_path = match &unresolved_dependency.value.path {
                // @Note this won't scale to `--extern=NAME:REL_PATH` (for single-file packages)
                // since the package_path points to a file, not a folder and the REL_PATH
                // should be relative to CWD
                Some(path) => package_path.join(&path.value),
                None => distributed_packages_path().join(dependency_exonym.value.as_str()),
            };

            if let Ok(path) = dependency_path.canonicalize() {
                // error case handled later via `SourceMap::load` for uniform error messages and to achieve DRY
                // @Update ^^^ we no longer have that formatting code in place, can we improve this?
                dependency_path = path;

                if let Some(dependency) = self
                    .packages
                    .values()
                    .find(|package| package.path == dependency_path)
                {
                    if !dependency.is_fully_resolved {
                        let map = self.map.borrow();

                        // @Note the message does not scale to more complex cycles (e.g. a cycle of three packages)
                        // @Task provide more context for transitive dependencies of the goal crate
                        // @Task code
                        Diagnostic::error()
                            .message(format!(
                                "the packages `{}` and `{}` are circular",
                                self[package_index].name, dependency.name
                            ))
                            .primary_span(content_span_of_key(dependency_exonym, &map))
                            .primary_span(
                                // @Beacon @Bug probably does not work if exonym != endonym (but that can be fixed easily!)
                                content_span_of_key(
                                    dependency
                                        .dependency_manifest
                                        .as_ref()
                                        .unwrap()
                                        .value
                                        .keys()
                                        .find(|key| key.value == self[package_index].name)
                                        .unwrap(),
                                    &map,
                                ),
                            )
                            .report(self.reporter);
                        return Err(());
                    }

                    // deduplicating packages by absolute path
                    resolved_dependencies.insert(
                        dependency_exonym.value.clone(),
                        dependency.library.ok_or_else(|| {
                            // @Question is this reachable?
                            // my past self wrote yes if some specific (?) could not be loaded
                            // since we just continue in this case
                            Diagnostic::dependent_package_does_not_contain_a_library(
                                dependency.name.as_str(),
                                self[package_index].name.as_str(),
                            )
                            .report(self.reporter);
                        })?,
                    );
                    continue;
                }
            }

            let dependency_manifest_path = dependency_path.join(PackageManifest::FILE_NAME);
            let file = self.map.borrow_mut().load(dependency_manifest_path.clone());
            let dependency_manifest_file = match file {
                Ok(file) => file,
                Err(error) => {
                    // @Task provide more context for transitive dependencies of the goal crate
                    // @Question code?
                    let diagnostic = Diagnostic::error()
                        .message(format!(
                            "could not load the dependency `{dependency_exonym}`",
                        ))
                        .note(IOError(error, &dependency_manifest_path).to_string());
                    let diagnostic = match &unresolved_dependency.value.path {
                        Some(path) => diagnostic.primary_span(path.span.trim(1)), // trimming quotes
                        None => diagnostic.primary_span(content_span_of_key(
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

            let alleged_dependency_endonym = unresolved_dependency
                .value
                .name
                .as_ref()
                .map_or(&dependency_exonym.value, |name| &name.value);

            if alleged_dependency_endonym != &dependency_manifest.profile.name.value {
                // @Task @Beacon @Beacon @Beacon improve error message
                // @Beacon @Beacon @Task span information
                // @Task use primary span (endonym here (key `name` or map key)) &
                // secondary span (endonym in the dependency's package manifest file)
                // @Task branch on existence of the key `name`
                Diagnostic::error()
                    .message("library name does not match")
                    .report(self.reporter);
                // @Note we could just go forwards with the the exonym, theoretically
                return Err(());
            }

            // @Task assert version requirement (if any) is fulfilled

            let dependency_package = Package::from_manifest(
                dependency_manifest.profile,
                dependency_manifest.crates.dependencies,
                dependency_path,
            );
            let dependency_package = self.packages.insert(dependency_package);

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-file
            let resolved_transitive_dependencies = self.enqueue_dependencies(dependency_package)?;

            self.resolve_library_manifest(dependency_package, dependency_manifest.crates.library);
            self[dependency_package].dependencies = resolved_transitive_dependencies;
            self[dependency_package].is_fully_resolved = true;

            resolved_dependencies.insert(
                dependency_exonym.value.clone(),
                self[dependency_package].library.ok_or_else(|| {
                    Diagnostic::dependent_package_does_not_contain_a_library(
                        self[dependency_package].name.as_str(),
                        self[package_index].name.as_str(),
                    )
                    .report(self.reporter);
                })?,
            );
        }

        health.of(resolved_dependencies).into()
    }

    fn resolve_library_manifest(
        &mut self,
        package: PackageIndex,
        library: Option<Spanned<LibraryManifest>>,
    ) -> bool {
        let path = &self[package].path;
        let default_library_path = path.join(CrateType::Library.default_root_file_path());

        let path = match library {
            Some(library) => Some(
                library
                    .value
                    .path
                    .map_or(default_library_path, |path| path.value),
            ),
            None => default_library_path.exists().then(|| default_library_path),
        };

        // @Task use `library.name` if available once implemented
        let name = self[package].name.clone();

        let package_contains_library = path.is_some();

        if let Some(path) = path {
            let index = self
                .crates
                .insert_with(|index| Crate::new(name, index, package, path, CrateType::Library));
            self[package].library = Some(index);
        }

        package_contains_library
    }

    fn resolve_binary_manifest(
        &mut self,
        package: PackageIndex,
        binary: Option<Spanned<BinaryManifest>>,
    ) -> bool {
        let path = &self[package].path;
        let default_binary_path = path.join(CrateType::Binary.default_root_file_path());

        // @Beacon @Task also find other binaries in source/binaries/
        let paths = match binary {
            Some(binary) => vec![binary
                .value
                .path
                .map_or(default_binary_path, |path| path.value)],
            None => match default_binary_path.exists() {
                true => vec![default_binary_path],
                false => Vec::new(),
            },
        };

        // @Task use `binary.name` if available once implemented
        let mut name = Some(self[package].name.clone());

        let package_contains_binaries = !paths.is_empty();

        for path in paths {
            let index = self.crates.insert_with(|index| {
                Crate::new(
                    name.take()
                        .expect("multiple binaries in a single not yet implemented"),
                    index,
                    package,
                    path,
                    CrateType::Binary,
                )
            });
            self[package].binaries.push(index);
        }

        package_contains_binaries
    }

    fn resolve_library_and_binary_manifests(
        &mut self,
        package: PackageIndex,
        library: Option<Spanned<LibraryManifest>>,
        binary: Option<Spanned<BinaryManifest>>,
    ) -> Result {
        let package_contains_library = self.resolve_library_manifest(package, library);
        let package_contains_binaries = self.resolve_binary_manifest(package, binary);

        if !(package_contains_library || package_contains_binaries) {
            // @Task provide more context for transitive dependencies of the goal crate
            // @Task code
            Diagnostic::error()
                .message(format!(
                    "the package `{}` does not contain a library or any binaries",
                    self[package].name,
                ))
                .report(self.reporter);
            return Err(());
        }

        Ok(())
    }

    pub fn process_package(&mut self, path: &Path) -> Result {
        let manifest_path = path.join(PackageManifest::FILE_NAME);
        let manifest_file = match self.map.borrow_mut().load(manifest_path.clone()) {
            Ok(file) => file,
            Err(error) => {
                Diagnostic::error()
                    // @Question code?
                    .message("could not load the package")
                    .note(IOError(error, &manifest_path).to_string())
                    .report(self.reporter);
                return Err(());
            }
        };

        let manifest = PackageManifest::parse(manifest_file, self.map.clone(), self.reporter)?;
        let package = Package::from_manifest(
            manifest.profile,
            manifest.crates.dependencies,
            path.to_owned(),
        );
        let package = self.packages.insert(package);

        // @Note we probably need to disallow referencing the same package through different
        // names from the same package to be able to generate a correct lock-file
        let resolved_dependencies = self.enqueue_dependencies(package)?;

        self.resolve_library_and_binary_manifests(
            package,
            manifest.crates.library,
            manifest.crates.binary,
        )?;
        self[package].dependencies = resolved_dependencies;
        self[package].is_fully_resolved = true;

        Ok(())
    }

    pub fn process_single_file_package(
        &mut self,
        path: PathBuf,
        crate_type: CrateType,
        no_core: bool,
    ) -> Result {
        // package and crate name
        let name = parse_crate_name_from_file_path(&path, self.reporter)?;

        let package = Package::single_file_package(name.clone(), path.clone());
        let package = self.packages.insert(package);

        let mut resolved_dependencies = HashMap::default();

        if !no_core {
            let core_path = core_package_path();

            let core_manifest_path = core_path.join(PackageManifest::FILE_NAME);
            let core_manifest_file = match self.map.borrow_mut().load(core_manifest_path.clone()) {
                Ok(file) => file,
                Err(error) => {
                    Diagnostic::error()
                        // @Question code?
                        .message("could not load the package `core`")
                        .note(IOError(error, &core_manifest_path).to_string())
                        .report(self.reporter);
                    return Err(());
                }
            };

            let core_manifest =
                PackageManifest::parse(core_manifest_file, self.map.clone(), self.reporter)?;
            let core_package = Package::from_manifest(
                core_manifest.profile,
                core_manifest.crates.dependencies,
                core_path,
            );
            let core_package = self.packages.insert(core_package);

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-fil
            let resolved_transitive_core_dependencies = self.enqueue_dependencies(core_package)?;

            self.resolve_library_manifest(core_package, core_manifest.crates.library);
            self[core_package].dependencies = resolved_transitive_core_dependencies;
            self[core_package].is_fully_resolved = true;

            resolved_dependencies.insert(
                CrateName::core_package_name(),
                self[core_package].library.unwrap(),
            );
        }

        let crate_ = self
            .crates
            .insert_with(|index| Crate::new(name, index, package, path, crate_type));
        let package = &mut self[package];
        package.binaries.push(crate_);
        package.dependencies = resolved_dependencies;
        package.is_fully_resolved = true;

        Ok(())
    }

    pub fn finalize(mut self) -> (BuildSession, IndexMap<CrateIndex, Crate>) {
        // @Note this is not extensible to multiple binary crates
        let goal_crate = self.crates.last().unwrap();
        let goal_crate_index = goal_crate.index;
        let goal_package = &self.packages[goal_crate.package];
        let goal_package_index = goal_crate.package;
        let homonymous = |&crate_: &CrateIndex| goal_crate.name == self.crates[crate_].name;

        let library_lookalike = goal_package
            .library
            .filter(|_| goal_crate.is_binary())
            .filter(homonymous);
        // @Note this is not extensible to multiple binary crates
        let binary_lookalike = goal_package
            .binaries
            .get(0)
            .copied()
            .filter(|_| goal_crate.is_library())
            .filter(homonymous);

        if let Some(lookalike) = library_lookalike.or(binary_lookalike) {
            self.crates[goal_crate_index].is_ambiguously_named_within_package = true;
            self.crates[lookalike].is_ambiguously_named_within_package = true;
        }

        (
            BuildSession::new(self.packages, goal_crate_index, goal_package_index),
            self.crates,
        )
    }
}

impl Diagnostic {
    fn dependent_package_does_not_contain_a_library(dependency: &str, dependent: &str) -> Self {
        // @Task provide more context for transitive dependencies of the goal crate
        // @Task code
        // @Beacon @Task span
        // @Update better message: dependent (?) package does not contain a library
        Self::error().message(format!(
            "dependency `{dependency}` of `{dependent}` is not a library"
        ))
    }
}

impl Index<CrateIndex> for BuildQueue<'_> {
    type Output = Crate;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.crates[index]
    }
}

impl IndexMut<CrateIndex> for BuildQueue<'_> {
    fn index_mut(&mut self, index: CrateIndex) -> &mut Self::Output {
        &mut self.crates[index]
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

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CrateIndex(pub u16);

impl fmt::Debug for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}c", self.0)
    }
}

impl index_map::Index for CrateIndex {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn value(self) -> usize {
        self.0 as _
    }
}

/// The path to the folder of packages shipped with the compiler.
pub fn distributed_packages_path() -> PathBuf {
    // @Task make this configurable via CLI option & env var & config file

    const DISTRIBUTED_LIBRARIES_FOLDER: &str = "libraries";

    Path::new(env!("CARGO_MANIFEST_DIR")).join(DISTRIBUTED_LIBRARIES_FOLDER)
}

pub fn core_package_path() -> PathBuf {
    distributed_packages_path().join(CrateName::core_package_name().as_str())
}

pub const DEFAULT_SOURCE_FOLDER_NAME: &str = "source";

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum CrateType {
    Library,
    Binary,
}

impl CrateType {
    pub const fn default_root_file_stem(self) -> &'static str {
        match self {
            Self::Library => "library",
            Self::Binary => "main",
        }
    }

    pub fn default_root_file_path(self) -> PathBuf {
        Path::new(DEFAULT_SOURCE_FOLDER_NAME)
            .join(self.default_root_file_stem())
            .with_extension(FILE_EXTENSION)
    }
}

impl fmt::Display for CrateType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Library => write!(f, "library"),
            Self::Binary => write!(f, "binary"),
        }
    }
}

impl FromStr for CrateType {
    type Err = ();

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        Ok(match input {
            "binary" => Self::Binary,
            "library" => Self::Library,
            _ => return Err(()),
        })
    }
}

/// A collection of crates and some metadata.
///
/// More concretely, it consists of zero or more binary (executable) crates
/// and of zero or one library crate but of always at least one crate.
/// The most important metadatum is the list of dependencies (external crates).
#[derive(Debug)]
pub struct Package {
    /// The name of the package.
    ///
    /// The library and the default binary crate share this name
    /// unless overwritten in their manifests.
    pub name: CrateName,
    /// The file or folder path of the package.
    ///
    /// For single-file packages, this points to a file.
    /// For normal packages, it points to the package folder
    /// which contains the package manifest.
    // @Beacon @Task make this of type PackagePath,
    // enum PackageLocation { NormalPackage(PathBuf), SingleFilePackage(PathBuf) }
    pub path: PathBuf,
    pub version: Option<Version>,
    pub description: String,
    /// States if the package is allowed to be published to a package repository.
    pub is_private: bool,
    pub library: Option<CrateIndex>,
    pub binaries: Vec<CrateIndex>,
    pub dependencies: HashMap<CrateName, CrateIndex>,
    /// Indicates if the library, binary and dependency crates are fully resolved.
    ///
    /// Packages are resolved in two steps to allow the library crate and the binary
    /// crates to keep an [index to the owning package](PackageIndex) and the package to
    /// keep [indices to its crates](CrateIndex).
    pub is_fully_resolved: bool,
    pub dependency_manifest:
        Option<Spanned<HashMap<WeaklySpanned<CrateName>, Spanned<DependencyManifest>>>>,
}

impl Package {
    pub fn from_manifest(
        profile: PackageProfile,
        dependency_manifest: Option<
            Spanned<HashMap<WeaklySpanned<CrateName>, Spanned<DependencyManifest>>>,
        >,
        path: PathBuf,
    ) -> Self {
        Package {
            name: profile.name.value,
            path,
            version: profile.version.map(|version| version.value),
            description: profile
                .description
                .map(|description| description.value)
                .unwrap_or_default(),
            is_private: profile
                .is_private
                .map(|is_private| is_private.value)
                .unwrap_or_default(),
            library: None,
            binaries: Vec::new(),
            dependencies: default(),
            is_fully_resolved: false,
            dependency_manifest,
        }
    }

    pub fn single_file_package(name: CrateName, path: PathBuf) -> Self {
        Self {
            name,
            path,
            version: Some(Version("0.0.0".to_owned())),
            description: String::new(),
            is_private: true,
            library: None,
            binaries: Vec::new(),
            dependencies: default(),
            is_fully_resolved: false,
            dependency_manifest: None,
        }
    }

    /// Test if this package is the standard library `core`.
    pub fn is_core(&self) -> bool {
        self.path == core_package_path()
    }
}

pub fn find_package(path: &Path) -> Option<&Path> {
    let manifest_path = path.join(PackageManifest::FILE_NAME);

    if manifest_path.exists() {
        Some(path)
    } else {
        find_package(path.parent()?)
    }
}

pub fn parse_crate_name_from_file_path(path: &Path, reporter: &Reporter) -> Result<CrateName> {
    if !crate::utility::has_file_extension(path, crate::FILE_EXTENSION) {
        Diagnostic::warning()
            .message("missing or non-standard file extension")
            .report(reporter);
    }

    // @Question can the file stem ever be empty in our case?
    let name = path.file_stem().unwrap();

    // @Beacon @Beacon @Beacon @Task do not unwrap! provide custom error
    CrateName::parse(name.to_str().unwrap()).reported(reporter)
}

pub mod manifest {
    use crate::{
        diagnostics::{Code, Diagnostic, Reporter},
        error::{Health, ReportedExt, Result},
        metadata::{self, content_span_of_key, convert, TypeError},
        span::{SharedSourceMap, SourceFileIndex, Spanned, WeaklySpanned},
        syntax::CrateName,
        utility::{try_all, HashMap},
    };
    use std::path::PathBuf;

    // @Note missing span of PackageManifest itself
    pub struct PackageManifest {
        pub profile: PackageProfile,
        pub crates: PackageCrates,
    }

    impl PackageManifest {
        pub const FILE_NAME: &'static str = "package.metadata";

        pub fn parse(
            file: SourceFileIndex,
            map: SharedSourceMap,
            reporter: &Reporter,
        ) -> Result<Self> {
            let manifest = metadata::parse(file, map.clone(), reporter)?;

            let manifest_span = manifest.span;
            let mut manifest: HashMap<_, _> =
                manifest.value.try_into().map_err(|error: TypeError| {
                    Diagnostic::error()
                        .code(Code::E800)
                        .message(format!(
                            "the type of the root should be {} but it is {}",
                            error.expected, error.actual
                        ))
                        .labeled_primary_span(manifest_span, "has the wrong type")
                        .report(reporter);
                })?;

            let name = metadata::remove_map_entry::<String>(
                Spanned::new(manifest_span, &mut manifest),
                "name",
                None,
                reporter,
            )
            .and_then(|name| {
                // trimming quotes
                CrateName::parse_spanned(name.map_span(|span| span.trim(1)).as_deref())
                    .reported(reporter)
            });
            let version = metadata::remove_optional_map_entry(&mut manifest, "version", reporter);
            let description =
                metadata::remove_optional_map_entry(&mut manifest, "description", reporter);
            let is_private =
                metadata::remove_optional_map_entry(&mut manifest, "private", reporter);

            let library = match metadata::remove_optional_map_entry::<HashMap<_, _>>(
                &mut manifest,
                "library",
                reporter,
            ) {
                Ok(Some(mut library)) => {
                    let path = metadata::remove_optional_map_entry::<String>(
                        &mut library.value,
                        "path",
                        reporter,
                    );
                    let exhaustion = metadata::check_map_is_empty(
                        library.value,
                        Some("library".into()),
                        reporter,
                    );

                    try_all! { path, exhaustion; return Err(()) };
                    Ok(Some(Spanned::new(
                        library.span,
                        LibraryManifest {
                            path: path.map(|path| path.map(PathBuf::from)),
                        },
                    )))
                }
                Ok(None) => Ok(None),
                Err(()) => Err(()),
            };

            let binary = match metadata::remove_optional_map_entry::<HashMap<_, _>>(
                &mut manifest,
                "binary",
                reporter,
            ) {
                Ok(Some(mut binary)) => {
                    let path = metadata::remove_optional_map_entry::<String>(
                        &mut binary.value,
                        "path",
                        reporter,
                    );
                    let exhaustion =
                        metadata::check_map_is_empty(binary.value, Some("binary".into()), reporter);

                    try_all! { path, exhaustion; return Err(()) };
                    Ok(Some(Spanned::new(
                        binary.span,
                        BinaryManifest {
                            path: path.map(|path| path.map(PathBuf::from)),
                        },
                    )))
                }
                Ok(None) => Ok(None),
                Err(()) => Err(()),
            };

            let dependencies = match metadata::remove_optional_map_entry::<HashMap<_, _>>(
                &mut manifest,
                "dependencies",
                reporter,
            ) {
                Ok(Some(dependencies)) => {
                    let mut health = Health::Untainted;
                    let mut parsed_dependencies = HashMap::default();

                    for (dependency_name, dependency_manifest) in dependencies.value {
                        // @Task generalize parse_spanned over I: Influence on future param on Spanned
                        let dependency_name = match CrateName::parse_spanned(
                            dependency_name
                                .map_span(|span| content_span_of_key(span, &map.borrow()))
                                .as_deref()
                                .strong(),
                        ) {
                            Ok(name) => name.weak(),
                            Err(error) => {
                                error.report(reporter);
                                health.taint();
                                continue;
                            }
                        };

                        // @Task custom error message (maybe)
                        let Ok(mut dependency_manifest) = convert::<HashMap<_, _>>(
                            dependency_name.value.as_str(),
                            dependency_manifest,
                            reporter,
                        ) else {
                            health.taint();
                            continue;
                        };

                        let version = metadata::remove_optional_map_entry::<String>(
                            &mut dependency_manifest.value,
                            "version",
                            reporter,
                        );
                        let name: Result<Option<_>> =
                            metadata::remove_optional_map_entry::<String>(
                                &mut dependency_manifest.value,
                                "name",
                                reporter,
                            )
                            .and_then(|name| {
                                // trimming quotes
                                name.map(|name| {
                                    CrateName::parse_spanned(
                                        name.map_span(|span| span.trim(1)).as_deref(),
                                    )
                                    .reported(reporter)
                                })
                                .transpose()
                            });
                        let path = metadata::remove_optional_map_entry::<String>(
                            &mut dependency_manifest.value,
                            "path",
                            reporter,
                        );

                        // @Temporary path
                        let exhaustion = metadata::check_map_is_empty(
                            dependency_manifest.value,
                            Some("dependency".into()),
                            reporter,
                        );

                        try_all! {
                            version, name, path, exhaustion;
                            health.taint(); continue
                        };

                        let dependency_manifest = Spanned::new(
                            dependency_manifest.span,
                            DependencyManifest {
                                version: version.map(|version| version.map(VersionRequirement)),
                                name,
                                path: path.map(|path| path.map(PathBuf::from)),
                            },
                        );

                        parsed_dependencies.insert(dependency_name, dependency_manifest);
                    }

                    health
                        .of(Some(Spanned::new(dependencies.span, parsed_dependencies)))
                        .into()
                }
                Ok(None) => Ok(None),
                Err(()) => Err(()),
            };

            metadata::check_map_is_empty(manifest, None, reporter)?;

            try_all! {
                name, version, description, is_private,
                library, binary, dependencies;
                return Err(())
            };

            Ok(PackageManifest {
                profile: PackageProfile {
                    name,
                    version: version.map(|version| version.map(Version)),
                    description,
                    is_private,
                },
                crates: PackageCrates {
                    library,
                    binary,
                    dependencies,
                },
            })
        }
    }

    /// Crate-indepedent package information.
    pub struct PackageProfile {
        pub name: Spanned<CrateName>,
        pub version: Option<Spanned<Version>>,
        pub description: Option<Spanned<String>>,
        pub is_private: Option<Spanned<bool>>,
    }

    /// Information about the crates of a package.
    pub struct PackageCrates {
        pub library: Option<Spanned<LibraryManifest>>,
        // @Task Vec<_>
        pub binary: Option<Spanned<BinaryManifest>>,
        pub dependencies:
            Option<Spanned<HashMap<WeaklySpanned<CrateName>, Spanned<DependencyManifest>>>>,
    }

    #[derive(Default)]
    pub struct LibraryManifest {
        // @Task pub name: Option<Spanned<CrateName>>,
        pub path: Option<Spanned<PathBuf>>,
    }

    #[derive(Default)]
    pub struct BinaryManifest {
        // @Task pub name: Option<Spanned<CrateName>>,
        pub path: Option<Spanned<PathBuf>>,
    }

    #[derive(Clone, Debug)]
    pub struct DependencyManifest {
        pub version: Option<Spanned<VersionRequirement>>,
        pub name: Option<Spanned<CrateName>>,
        pub path: Option<Spanned<PathBuf>>,
    }

    #[derive(Debug)]
    pub struct Version(pub String);

    #[derive(Clone, Debug)]
    pub struct VersionRequirement(pub String);
}
