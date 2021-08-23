use crate::{
    diagnostics::{Diagnostic, Reporter},
    entity::Entity,
    error::Result,
    resolver::{CrateScope, DeclarationIndex, Identifier},
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
    fmt,
    ops::{Index, IndexMut},
    path::{Path, PathBuf},
};

#[derive(Default, Debug)] // @Temporary Debug
pub struct CrateStore {
    // @Question BTreeSet<CrateScope> (CrateScope.index) ?
    crates: HashMap<CrateIndex, CrateScope>,
    packages: IndexMap<PackageIndex, Package>,
}

impl CrateStore {
    pub fn with_packages(packages: IndexMap<PackageIndex, Package>) -> Self {
        Self {
            crates: HashMap::default(),
            packages,
        }
    }

    pub fn add(&mut self, crate_: CrateScope) {
        self.crates.insert(crate_.index, crate_);
    }

    pub fn entity(&self, index: DeclarationIndex) -> &Entity {
        self.crates[&index.crate_index()].get(index.local_index())
    }

    pub fn foreign_type(&self, binder: &'static str) -> Option<&Identifier> {
        // @Task don't just search through all crates (linearly) but
        // respect the (not yet existing) dependency graph:
        // crates farther away have higher priority meaning
        // crates trying to (re-)define the foreign type is illegal
        // equally far crates (e.g. `core` and a no-core library) trying
        // to define the same foreign type leads to an error
        // (a different one)
        for crate_ in self.crates.values() {
            match crate_.ffi.foreign_types.get(binder) {
                Some(Some(binder)) => return Some(binder),
                Some(None) => continue,
                None => unreachable!(),
            }
        }

        None
    }
}

impl Index<CrateIndex> for CrateStore {
    type Output = CrateScope;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.crates[&index]
    }
}

impl Index<PackageIndex> for CrateStore {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl IndexMut<PackageIndex> for CrateStore {
    fn index_mut(&mut self, index: PackageIndex) -> &mut Self::Output {
        &mut self.packages[index]
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

#[derive(Default)]
pub struct CrateBuildQueue {
    pub crates: IndexMap<CrateIndex, CrateScope>,
    pub packages: IndexMap<PackageIndex, Package>,
}

impl CrateBuildQueue {
    // @Question are some errors non-fatal??
    pub fn enqueue_dependencies(
        &mut self,
        package_path: &Path,
        dependencies: &HashMap<String, DependencyManifest>,
        reporter: &Reporter,
    ) -> Result<HashMap<String, CrateIndex>> {
        let mut resolved_dependencies = HashMap::default();

        for (dependency_name, dependency_manifest) in dependencies {
            let path = match &dependency_manifest.path {
                // @Task handle error
                Some(path) => package_path.join(path).canonicalize().unwrap(),
                // @Beacon @Task we need to emit a custom diagnostic if stuff cannot be
                // found in the distributies libraries path
                None => distributed_libraries_path().join(dependency_name),
            };

            if let Some(package) = self.packages.values().find(|package| package.path == path) {
                if !package.is_fully_resolved {
                    // @Task embellish
                    Diagnostic::error()
                        .message("circular crates")
                        .report(reporter);
                    return Err(());
                }

                // deduplicating packages by absolute path
                // @Task handle unwrap: "error: crate does not contain a library"
                resolved_dependencies.insert(dependency_name.clone(), package.library.unwrap());
                continue;
            }

            // @Task don't return early here!
            // @Task don't bubble up with `?` but once `open` returns a proper error type,
            // report a custom diagnostic saying ~ "could not find a package manifest for the package XY
            // specified as a path dependency" (sth sim to this)
            let manifest = PackageManifest::from_package_path(&path, &reporter)?;

            // @Task proper error
            assert_eq!(
                &manifest.details.name,
                dependency_manifest.name.as_ref().unwrap_or(dependency_name)
            );
            // assert version requirement (if any) is fulfilled

            let package = Package::from_manifest_details(path.clone(), manifest.details);
            let package = self.packages.insert(package);

            // @Note we probably need to disallow referencing the same package through different
            // names from the same package to be able to generate a correct lock-file
            let resolved_transitive_dependencies =
                self.enqueue_dependencies(&path, &manifest.crates.dependencies, reporter)?;

            self.resolve_library_manifest(package, manifest.crates.library);
            self.add_resolved_dependencies(package, resolved_transitive_dependencies);

            // @Task handle unwrap: "error: crate does not contain a library"
            resolved_dependencies.insert(dependency_name.clone(), self[package].library.unwrap());
        }

        Ok(resolved_dependencies)
    }

    pub fn resolve_library_manifest(
        &mut self,
        package_index: PackageIndex,
        library: Option<LibraryManifest>,
    ) -> bool {
        let path = &self[package_index].path;
        let default_library_path = path.join(CrateType::Library.default_root_file_path());

        let path = match library {
            Some(library) => Some(library.path.unwrap_or(default_library_path)),
            None => default_library_path.exists().then(|| default_library_path),
        };

        let package_contains_library = path.is_some();

        if let Some(path) = path {
            let index = self.crates.insert_with(|index| {
                CrateScope::new(index, package_index, path, CrateType::Library)
            });
            self[package_index].library = Some(index);
        }

        package_contains_library
    }

    pub fn resolve_binary_manifest(
        &mut self,
        package_index: PackageIndex,
        binary: Option<BinaryManifest>,
    ) -> bool {
        let path = &self[package_index].path;
        let default_binary_path = path.join(CrateType::Binary.default_root_file_path());

        // @Beacon @Task also find other binaries in source/binaries/
        let paths = match binary {
            Some(binary) => vec![binary.path.unwrap_or(default_binary_path)],
            None => match default_binary_path.exists() {
                true => vec![default_binary_path],
                false => Vec::new(),
            },
        };

        let package_contains_binaries = !paths.is_empty();

        for path in paths {
            let index = self.crates.insert_with(|index| {
                CrateScope::new(index, package_index, path, CrateType::Binary)
            });
            self[package_index].binaries.push(index);
        }

        package_contains_binaries
    }

    pub fn resolve_library_and_binary_manifests(
        &mut self,
        package_index: PackageIndex,
        library: Option<LibraryManifest>,
        binary: Option<BinaryManifest>,
        reporter: &Reporter,
    ) -> Result {
        let package_contains_library = self.resolve_library_manifest(package_index, library);
        let package_contains_binaries = self.resolve_binary_manifest(package_index, binary);

        if !(package_contains_library || package_contains_binaries) {
            // @Task embellish
            Diagnostic::error()
                .message("package does neither contain a library nor any binaries")
                .report(reporter);
            return Err(());
        }

        Ok(())
    }

    pub fn add_resolved_dependencies(
        &mut self,
        package_index: PackageIndex,
        dependencies: HashMap<String, CrateIndex>,
    ) {
        self[package_index].dependencies = dependencies;
        self[package_index].is_fully_resolved = true;
    }

    pub fn into_unbuilt_and_built(self) -> (IndexMap<CrateIndex, CrateScope>, CrateStore) {
        (self.crates, CrateStore::with_packages(self.packages))
    }
}

impl Index<CrateIndex> for CrateBuildQueue {
    type Output = CrateScope;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.crates[index]
    }
}

impl IndexMut<CrateIndex> for CrateBuildQueue {
    fn index_mut(&mut self, index: CrateIndex) -> &mut Self::Output {
        &mut self.crates[index]
    }
}

impl Index<PackageIndex> for CrateBuildQueue {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

impl IndexMut<PackageIndex> for CrateBuildQueue {
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
    pub description: Option<String>,
    pub is_private: bool,
    pub library: Option<CrateIndex>,
    pub binaries: Vec<CrateIndex>,
    pub dependencies: HashMap<String, CrateIndex>,
    pub is_fully_resolved: bool,
}

impl Package {
    pub fn from_manifest_details(path: PathBuf, manifest: PackageManifestDetails) -> Self {
        Package {
            name: manifest.name,
            path,
            version: manifest.version,
            description: manifest.description,
            is_private: manifest.is_private,
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
            description: None,
            is_private: true,
            library: None,
            binaries: Vec::new(),
            dependencies: HashMap::default(),
            is_fully_resolved: false,
        }
    }
}

// @Temporary location
pub fn find_package(path: &Path) -> Result<&Path> {
    let manifest_path = path.join(PackageManifest::FILE_NAME);

    // @Task error handling
    if manifest_path.try_exists().unwrap() {
        Ok(path)
    } else {
        find_package(path.parent().ok_or(())?)
    }
}

pub mod manifest {
    use crate::{
        diagnostics::{Diagnostic, Reporter},
        error::Result,
        util::HashMap,
    };
    use serde::Deserialize;
    use std::path::{Path, PathBuf};

    #[derive(Deserialize)]
    pub struct PackageManifest {
        #[serde(flatten)]
        pub details: PackageManifestDetails,
        #[serde(flatten)]
        pub crates: PackageManifestCrates,
    }

    impl PackageManifest {
        pub const FILE_NAME: &'static str = "package.json5";

        // @Temporary signature: define errors
        // @Task handle errors
        // @Update @Beacon @Question should we really handle (i.e. using reporter) the errors
        // here? the current message does not make sense for implicitly opening `core`
        // and probably also not for trying to open dependencies...
        // @Update @Update @Task don't handle them here but at the caller's site!!
        pub fn from_package_path(package_path: &Path, reporter: &Reporter) -> Result<Self> {
            let manifest_path = package_path.join(Self::FILE_NAME);

            let manifest = match std::fs::read_to_string(&manifest_path) {
                Ok(manifest) => manifest,
                Err(error) => {
                    // @Task custom error messages dependening on io::ErrorKind
                    // @Temporary message
                    // @Update not compatible with find_package_path I guess...
                    Diagnostic::error()
                        .message("cannot find a valid package manifest")
                        .note(format!("path: {manifest_path:?}"))
                        .note(format!("reason: {error}"))
                        .report(reporter);
                    return Err(());
                }
            };

            // @Temporary error message
            json5::from_str(&manifest).map_err(|error| {
                Diagnostic::error()
                    .message("package manifest has an invalid format")
                    .note(format!("reason: {error}"))
                    .report(reporter)
            })
        }
    }

    #[derive(Deserialize)]
    pub struct PackageManifestDetails {
        // @Task newtype
        pub name: String,
        pub version: Option<Version>,
        pub description: Option<String>,
        #[serde(default, rename = "private")]
        pub is_private: bool,
    }

    #[derive(Deserialize)]
    pub struct PackageManifestCrates {
        pub library: Option<LibraryManifest>,
        // @Task Vec<_>
        pub binary: Option<BinaryManifest>,
        #[serde(default)]
        pub dependencies: HashMap<String, DependencyManifest>,
    }

    #[derive(Deserialize, Default)]
    pub struct LibraryManifest {
        // @Task pub name: Option<String>,
        pub path: Option<PathBuf>,
    }

    #[derive(Deserialize, Default)]
    pub struct BinaryManifest {
        // @Task pub name: Option<String>,
        pub path: Option<PathBuf>,
    }

    #[derive(Deserialize)]
    pub struct DependencyManifest {
        pub version: Option<VersionRequirement>,
        pub name: Option<String>,
        pub path: Option<String>,
        // pub url: Option<String>,
        // pub branch: Option<String>,
        // pub tag: Option<String>,
        // pub revision: Option<String>,
    }

    // @Temporary
    #[derive(Deserialize, Debug)]
    #[serde(transparent)]
    pub struct Version(pub String);

    // @Temporary
    #[derive(Deserialize)]
    #[serde(transparent)]
    pub struct VersionRequirement(pub String);
}
