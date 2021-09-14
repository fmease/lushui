use crate::{
    diagnostics::{Diagnostic, Reporter},
    entity::Entity,
    error::Result,
    format::DisplayWith,
    lexer::parse_crate_name,
    resolver::{CrateScope, DeclarationIndex, Identifier},
    span::SharedSourceMap,
    util::HashMap,
    FILE_EXTENSION,
};
use index_map::IndexMap;
pub use manifest::{
    BinaryManifest, DependencyManifest, LibraryManifest, PackageManifest, PackageManifestCrates,
    PackageManifestDetails, Version,
};
use std::{
    convert::TryInto,
    default::default,
    fmt,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
};

#[derive(Default)]
pub struct BuildSession {
    // @Question BTreeSet<CrateScope> (CrateScope.index) ?
    built_crates: HashMap<CrateIndex, CrateScope>,
    built_packages: IndexMap<PackageIndex, Package>,
}

impl BuildSession {
    pub fn with_packages(packages: IndexMap<PackageIndex, Package>) -> Self {
        Self {
            built_crates: HashMap::default(),
            built_packages: packages,
        }
    }

    pub fn add(&mut self, crate_: CrateScope) {
        self.built_crates.insert(crate_.index, crate_);
    }

    pub fn foreign_type(&self, binder: &'static str) -> Option<&Identifier> {
        // @Task don't just search through all crates (linearly) but
        // respect the (not yet existing) dependency graph:
        // crates farther away have higher priority meaning
        // crates trying to (re-)define the foreign type is illegal
        // equally far crates (e.g. `core` and a no-core library) trying
        // to define the same foreign type leads to an error
        // (a different one)
        for crate_ in self.built_crates.values() {
            match crate_.ffi.foreign_types.get(binder) {
                Some(Some(binder)) => return Some(binder),
                Some(None) => continue,
                None => unreachable!(),
            }
        }

        None
    }
}

impl Index<DeclarationIndex> for BuildSession {
    type Output = Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        &self.built_crates[&index.crate_index()][index.local_index()]
    }
}

impl Index<CrateIndex> for BuildSession {
    type Output = CrateScope;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.built_crates[&index]
    }
}

impl Index<PackageIndex> for BuildSession {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.built_packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildSession {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.built_packages[index]
    }
}

// @Temporary
#[derive(PartialEq, Eq, Clone, Copy, index_map::Index)]
pub struct PackageIndex(usize);

impl fmt::Debug for PackageIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}p", self.0)
    }
}
pub struct BuildQueue<'r> {
    unbuilt_crates: IndexMap<CrateIndex, CrateScope>,
    unbuilt_packages: IndexMap<PackageIndex, Package>,
    map: SharedSourceMap,
    reporter: &'r Reporter,
}

impl<'r> BuildQueue<'r> {
    pub fn new(map: SharedSourceMap, reporter: &'r Reporter) -> Self {
        Self {
            unbuilt_crates: default(),
            unbuilt_packages: default(),
            map,
            reporter,
        }
    }
}

impl BuildQueue<'_> {
    fn enqueue_dependencies(
        &mut self,
        package_index: PackageIndex,
        dependencies: &HashMap<String, DependencyManifest>,
    ) -> Result<HashMap<String, CrateIndex>> {
        let package_path = self[package_index].path.clone();
        let mut resolved_dependencies = HashMap::default();

        for (dependency_exonym, unresolved_dependency) in dependencies {
            let mut dependency_path = match &unresolved_dependency.path {
                Some(path) => package_path.join(&path.data),
                None => distributed_libraries_path().join(dependency_exonym),
            };

            if let Ok(path) = dependency_path.canonicalize() {
                // error case handled later via `SourceMap::load` for uniform error messages and to anchieve DRY
                dependency_path = path;

                if let Some(package) = self
                    .unbuilt_packages
                    .values()
                    .find(|package| package.path == dependency_path)
                {
                    if !package.is_fully_resolved {
                        // @Beacon @Beacon @Beacon @Task embellish
                        // @Beacon @Beacon @Task span info
                        Diagnostic::error()
                            .message("circular crates")
                            .report(self.reporter);
                        return Err(());
                    }

                    // deduplicating packages by absolute path
                    // @Note the error case can only be reached if we don't bail out early (which we don't)
                    resolved_dependencies.insert(
                        dependency_exonym.clone(),
                        package.library.ok_or_else(|| {
                            dependency_is_not_a_library(&package.name, &self[package_index].name)
                                .report(self.reporter)
                        })?,
                    );
                    continue;
                }
            }

            let dependency_manifest_path = dependency_path.join(PackageManifest::FILE_NAME);
            let dependency_manifest_file =
                match self.map.borrow_mut().load(dependency_manifest_path.clone()) {
                    Ok(file) => file,
                    Err(error) => {
                        // @Task "could not find a package manifest for the package XY
                        // specified as a path dependency" (sth sim to this)
                        // @Beacon @Beacon @Beacon @Task
                        Diagnostic::error()
                            .message("could not load package manifest [[[dependency package]]]")
                            .note(error.with(&dependency_manifest_path).to_string())
                            // @Task point to `dependency_name` if not available and adjust message ^
                            // @Note we need to thread through the Span first, np tho
                            .if_present(unresolved_dependency.path.as_ref(), |this, path| {
                                this.primary_span(path)
                            })
                            .report(self.reporter);
                        // @Task don't return early here!
                        return Err(());
                    }
                };

            let dependency_manifest =
                PackageManifest::parse(dependency_manifest_file, self.map.clone(), self.reporter)?;

            let alleged_dependency_endonym = unresolved_dependency
                .name
                .as_ref()
                .map(|name| &name.data)
                .unwrap_or(dependency_exonym);

            if alleged_dependency_endonym != &dependency_manifest.details.name.data {
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

            let dependency_package =
                Package::from_manifest_details(dependency_manifest.details, dependency_path);
            let dependency_package = self.unbuilt_packages.insert(dependency_package);

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-file
            let resolved_transitive_dependencies = self.enqueue_dependencies(
                dependency_package,
                &dependency_manifest
                    .crates
                    .dependencies
                    .map(|dependencies| dependencies.data)
                    .unwrap_or_default()
                    .iter()
                    .map(|(key, value)| (key.data.clone(), value.data.clone()))
                    .collect(),
            )?;

            self.resolve_library_manifest(
                dependency_package,
                dependency_manifest
                    .crates
                    .library
                    .map(|library| library.data),
            );
            self[dependency_package].dependencies = resolved_transitive_dependencies;
            self[dependency_package].is_fully_resolved = true;

            resolved_dependencies.insert(
                dependency_exonym.clone(),
                // @Temporary try op
                self[dependency_package].library.ok_or_else(|| {
                    dependency_is_not_a_library(
                        &self[dependency_package].name,
                        &self[package_index].name,
                    )
                    .report(self.reporter)
                })?,
            );
        }

        // @Temporary
        fn dependency_is_not_a_library(dependency: &str, dependent: &str) -> Diagnostic {
            // @Beacon @Beacon @Beacon @Task message, span!!
            Diagnostic::error().message(format!(
                "dependency `{dependency}` of `{dependent}` is not a library"
            ))
        }

        Ok(resolved_dependencies)
    }

    fn resolve_library_manifest(
        &mut self,
        package_index: PackageIndex,
        library: Option<LibraryManifest>,
    ) -> bool {
        let path = &self[package_index].path;
        let default_library_path = path.join(CrateType::Library.default_root_file_path());

        let path = match library {
            Some(library) => Some(
                library
                    .path
                    .map(|path| path.data)
                    .unwrap_or(default_library_path),
            ),
            None => default_library_path.exists().then(|| default_library_path),
        };

        let package_contains_library = path.is_some();

        if let Some(path) = path {
            let index = self.unbuilt_crates.insert_with(|index| {
                CrateScope::new(index, package_index, path, CrateType::Library)
            });
            self[package_index].library = Some(index);
        }

        package_contains_library
    }

    fn resolve_binary_manifest(
        &mut self,
        package_index: PackageIndex,
        binary: Option<BinaryManifest>,
    ) -> bool {
        let path = &self[package_index].path;
        let default_binary_path = path.join(CrateType::Binary.default_root_file_path());

        // @Beacon @Task also find other binaries in source/binaries/
        let paths = match binary {
            Some(binary) => vec![binary
                .path
                .map(|path| path.data)
                .unwrap_or(default_binary_path)],
            None => match default_binary_path.exists() {
                true => vec![default_binary_path],
                false => Vec::new(),
            },
        };

        let package_contains_binaries = !paths.is_empty();

        for path in paths {
            let index = self.unbuilt_crates.insert_with(|index| {
                CrateScope::new(index, package_index, path, CrateType::Binary)
            });
            self[package_index].binaries.push(index);
        }

        package_contains_binaries
    }

    fn resolve_library_and_binary_manifests(
        &mut self,
        package_index: PackageIndex,
        library: Option<LibraryManifest>,
        binary: Option<BinaryManifest>,
    ) -> Result {
        let package_contains_library = self.resolve_library_manifest(package_index, library);
        let package_contains_binaries = self.resolve_binary_manifest(package_index, binary);

        if !(package_contains_library || package_contains_binaries) {
            // @Task embellish
            // @Beacon @Task span info
            Diagnostic::error()
                .message("package does neither contain a library nor any binaries")
                .report(self.reporter);
            return Err(());
        }

        Ok(())
    }

    pub fn process_package(&mut self, path: PathBuf) -> Result {
        let package_path = match find_package(&path) {
            Some(path) => path,
            None => {
                // @Beacon @Task span info
                Diagnostic::error()
                    .message("neither the current directory nor any of its parents is a package")
                    .note(format!(
                        "none of the directories contain a package manifest file named `{}`",
                        PackageManifest::FILE_NAME
                    ))
                    .report(self.reporter);
                return Err(());
            }
        };

        let manifest_path = package_path.join(PackageManifest::FILE_NAME);
        let manifest_file = match self.map.borrow_mut().load(manifest_path.clone()) {
            Ok(file) => file,
            Err(error) => {
                // @Beacon @Beacon @Beacon @Task
                Diagnostic::error()
                    .message("could not load package manifest [[[goal package]]]")
                    .note(error.with(&manifest_path).to_string())
                    .report(self.reporter);
                return Err(());
            }
        };

        // @Task verify name and version matches (unless overwritten!)
        // @Task don't return early here!
        // @Task don't bubble up with `?` but once `open` returns a proper error type,
        // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
        // specified as a path dependency" (sth sim to this)
        let manifest = PackageManifest::parse(manifest_file, self.map.clone(), self.reporter)?;

        let package = Package::from_manifest_details(manifest.details, package_path.to_owned());
        let package = self.unbuilt_packages.insert(package);

        // @Note we probably need to disallow referencing the same package through different
        // names from the same package to be able to generate a correct lock-file
        let resolved_dependencies = self.enqueue_dependencies(
            package,
            &manifest
                .crates
                .dependencies
                .map(|dependencies| dependencies.data)
                .unwrap_or_default()
                .iter()
                .map(|(key, value)| (key.data.clone(), value.data.clone()))
                .collect(),
        )?;

        self.resolve_library_and_binary_manifests(
            package,
            manifest.crates.library.map(|library| library.data),
            manifest.crates.binary.map(|binary| binary.data),
        )?;
        self[package].dependencies = resolved_dependencies;
        self[package].is_fully_resolved = true;

        Ok(())
    }

    pub fn process_single_file_package(
        &mut self,
        source_file_path: PathBuf,
        unlink_core: bool,
    ) -> Result {
        let crate_name = parse_crate_name(source_file_path.clone(), self.reporter)?;

        // @Task dont unwrap, handle error case
        // joining with "." since it might return "" which would fail to canonicalize
        let path = source_file_path
            .parent()
            .unwrap()
            .join(".")
            .canonicalize()
            .unwrap();

        // @Note wasteful name cloning
        let package = Package::single_file_package(crate_name.as_str().to_owned(), path);
        let package = self.unbuilt_packages.insert(package);

        let mut resolved_dependencies = HashMap::default();

        if !unlink_core {
            let core_path = distributed_libraries_path().join(CORE_PACKAGE_NAME);

            let core_manifest_path = core_path.join(PackageManifest::FILE_NAME);
            let core_manifest_file = match self.map.borrow_mut().load(core_manifest_path.clone()) {
                Ok(file) => file,
                Err(error) => {
                    // @Beacon @Beacon @Beacon @Task
                    Diagnostic::error()
                        .message("could not load package manifest [[[core package]]]")
                        .note(error.with(&core_manifest_path).to_string())
                        .report(self.reporter);
                    return Err(());
                }
            };

            let core_manifest =
                PackageManifest::parse(core_manifest_file, self.map.clone(), self.reporter)?;

            let core_package = Package::from_manifest_details(core_manifest.details, core_path);
            let core_package = self.unbuilt_packages.insert(core_package);

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-fil
            let resolved_transitive_core_dependencies = self.enqueue_dependencies(
                core_package,
                &core_manifest
                    .crates
                    .dependencies
                    .map(|dependencies| dependencies.data)
                    .unwrap_or_default()
                    .iter()
                    .map(|(key, value)| (key.data.clone(), value.data.clone()))
                    .collect(),
            )?;

            self.resolve_library_manifest(
                core_package,
                core_manifest.crates.library.map(|library| library.data),
            );
            self[core_package].dependencies = resolved_transitive_core_dependencies;
            self[core_package].is_fully_resolved = true;

            resolved_dependencies.insert(
                CORE_PACKAGE_NAME.to_string(),
                self[core_package].library.unwrap(),
            );
        }

        let binary = self.unbuilt_crates.insert_with(|index| {
            CrateScope::new(index, package, source_file_path, CrateType::Binary)
        });
        let package = &mut self[package];
        package.binaries.push(binary);
        package.dependencies = resolved_dependencies;
        package.is_fully_resolved = true;

        Ok(())
    }

    pub fn into_unbuilt_and_built(self) -> (IndexMap<CrateIndex, CrateScope>, BuildSession) {
        (
            self.unbuilt_crates,
            BuildSession::with_packages(self.unbuilt_packages),
        )
    }
}

impl Index<CrateIndex> for BuildQueue<'_> {
    type Output = CrateScope;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.unbuilt_crates[index]
    }
}

impl IndexMut<CrateIndex> for BuildQueue<'_> {
    fn index_mut(&mut self, index: CrateIndex) -> &mut Self::Output {
        &mut self.unbuilt_crates[index]
    }
}

impl Index<PackageIndex> for BuildQueue<'_> {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.unbuilt_packages[index]
    }
}

impl IndexMut<PackageIndex> for BuildQueue<'_> {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.unbuilt_packages[index]
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

// @Temporary location
/// The path to the folder of libraries shipped with the compiler/interpreter.
// @Task make this configurable via CLI option & env var & config file
pub fn distributed_libraries_path() -> PathBuf {
    const DISTRIBUTED_LIBRARIES_FOLDER: &str = "libraries";

    Path::new(env!("CARGO_MANIFEST_DIR")).join(DISTRIBUTED_LIBRARIES_FOLDER)
}

pub const CORE_PACKAGE_NAME: &str = "core";

pub const DEFAULT_SOURCE_FOLDER_NAME: &str = "source";

#[derive(Clone, Copy, Debug)]
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

#[derive(Debug)]
pub struct Package {
    pub name: String,
    pub path: PathBuf,
    pub version: Option<Version>,
    pub description: String,
    pub is_private: bool,
    pub library: Option<CrateIndex>,
    pub binaries: Vec<CrateIndex>,
    pub dependencies: HashMap<String, CrateIndex>,
    pub is_fully_resolved: bool,
}

impl Package {
    pub fn from_manifest_details(manifest: PackageManifestDetails, path: PathBuf) -> Self {
        Package {
            name: manifest.name.data,
            path,
            version: manifest.version.map(|version| version.data),
            description: manifest
                .description
                .map(|description| description.data)
                .unwrap_or_default(),
            is_private: manifest
                .is_private
                .map(|is_private| is_private.data)
                .unwrap_or_default(),
            library: None,
            binaries: Vec::new(),
            dependencies: HashMap::default(),
            is_fully_resolved: false,
        }
    }

    pub fn single_file_package(name: String, path: PathBuf) -> Self {
        Self {
            name,
            path,
            version: Some(Version("0.0.0".to_owned())),
            description: String::new(),
            is_private: true,
            library: None,
            binaries: Vec::new(),
            dependencies: HashMap::default(),
            is_fully_resolved: false,
        }
    }
}

pub fn find_package(path: &Path) -> Option<&Path> {
    let manifest_path = path.join(PackageManifest::FILE_NAME);

    // `try_exists` not suitable here
    if manifest_path.exists() {
        Some(path)
    } else {
        find_package(path.parent()?)
    }
}

pub mod manifest {
    use crate::{
        diagnostics::{Code, Diagnostic, Reporter},
        error::{Health, Result},
        metadata::{self, convert, Map, TypeError},
        span::{SharedSourceMap, SourceFileIndex, Spanned},
        util::{spanned_key_map::SpannedKeyMap, try_all},
    };
    use std::{convert::TryInto, path::PathBuf};

    pub struct PackageManifest {
        pub details: PackageManifestDetails,
        pub crates: PackageManifestCrates,
    }

    impl PackageManifest {
        pub const FILE_NAME: &'static str = "package.metadata";

        pub fn parse(
            source_file: SourceFileIndex,
            source_map: SharedSourceMap,
            reporter: &Reporter,
        ) -> Result<Self> {
            let manifest = metadata::parse(source_file, source_map.clone(), reporter)?;

            let manifest_span = manifest.span;
            let mut manifest: Map = manifest.data.try_into().map_err(|error: TypeError| {
                Diagnostic::error()
                    .code(Code::E800)
                    .message(format!(
                        "the type of the root should be {} but it is {}",
                        error.expected, error.actual
                    ))
                    .labeled_primary_span(manifest_span, "has the wrong type")
                    .report(reporter)
            })?;

            let name = manifest.remove("name", None, manifest_span, reporter);
            let version = manifest.remove_optional("version", reporter);
            let description = manifest.remove_optional("description", reporter);
            let is_private = manifest.remove_optional("private", reporter);

            let library = match manifest.remove_optional::<Map>("library", reporter) {
                Ok(Some(mut library)) => {
                    let path = library.data.remove_optional::<String>("path", reporter);
                    let exhaustion = library
                        .data
                        .check_exhaustion(Some("library".into()), reporter);

                    try_all! { path, exhaustion => return Err(()) };
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

            let binary = match manifest.remove_optional::<Map>("binary", reporter) {
                Ok(Some(mut binary)) => {
                    let path = binary.data.remove_optional::<String>("path", reporter);
                    let exhaustion = binary
                        .data
                        .check_exhaustion(Some("binary".into()), reporter);

                    try_all! { path, exhaustion => return Err(()) };
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

            let dependencies = match manifest.remove_optional::<Map>("dependencies", reporter) {
                Ok(Some(dependencies)) => {
                    let mut health = Health::Untainted;
                    let mut parsed_dependencies = SpannedKeyMap::default();

                    for (dependency_name, dependency_manifest) in dependencies.data.into_iter() {
                        // @Task custom error message (maybe)
                        let dependency_manifest =
                            convert::<Map>(&dependency_name.data, dependency_manifest, reporter);

                        try_all! { dependency_manifest => health.taint(); continue };
                        let mut dependency_manifest = dependency_manifest;

                        let version = dependency_manifest
                            .data
                            .remove_optional::<String>("version", reporter);
                        let name = dependency_manifest.data.remove_optional("name", reporter);
                        let path = dependency_manifest
                            .data
                            .remove_optional::<String>("path", reporter);

                        // @Temporary path
                        let exhaustion = dependency_manifest
                            .data
                            .check_exhaustion(Some("dependency".into()), reporter);

                        try_all! {
                            version, name, path, exhaustion =>
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

            manifest.check_exhaustion(None, reporter)?;

            try_all! {
                name, version, description, is_private,
                library, binary, dependencies =>
                return Err(())
            };

            let manifest = PackageManifest {
                details: PackageManifestDetails {
                    name,
                    version: version.map(|version| version.map(Version)),
                    description,
                    is_private,
                },
                crates: PackageManifestCrates {
                    library,
                    binary,
                    dependencies,
                },
            };

            Ok(manifest)
        }
    }

    // @Beacon @Task make this a newtype and enforce that it's a valid
    // alphanumeric identifier
    pub type CrateName = String;

    pub struct PackageManifestDetails {
        pub name: Spanned<CrateName>,
        pub version: Option<Spanned<Version>>,
        pub description: Option<Spanned<String>>,
        pub is_private: Option<Spanned<bool>>,
    }

    pub struct PackageManifestCrates {
        pub library: Option<Spanned<LibraryManifest>>,
        // @Task Vec<_>
        pub binary: Option<Spanned<BinaryManifest>>,
        pub dependencies: Option<Spanned<SpannedKeyMap<CrateName, Spanned<DependencyManifest>>>>,
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

    // @Beacon @Temporary clone
    #[derive(Clone)]
    pub struct DependencyManifest {
        pub version: Option<Spanned<VersionRequirement>>,
        pub name: Option<Spanned<CrateName>>,
        pub path: Option<Spanned<PathBuf>>,
        // pub url: Option<String>,
        // pub branch: Option<String>,
        // pub tag: Option<String>,
        // pub revision: Option<String>,
    }

    // @Temporary
    #[derive(Debug)]
    pub struct Version(pub String);

    // @Temporary
    #[derive(Clone)]
    pub struct VersionRequirement(pub String);
}
