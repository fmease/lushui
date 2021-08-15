use crate::{
    diagnostics::{Diagnostic, Reporter},
    entity::Entity,
    error::Result,
    resolver::{CrateScope, DeclarationIndex, Identifier},
    util::HashMap,
    FILE_EXTENSION,
};
use serde::Deserialize;
use std::{
    convert::TryInto,
    fmt,
    path::{Path, PathBuf},
};

/// A collection of [crates](Crate).
// conceptual @Bug: currently a PackageStore
#[derive(Default)]
pub struct CrateStore {
    crates: HashMap<CrateIndex, Package>,
}

impl CrateStore {
    pub fn add(&mut self, index: CrateIndex, crate_: Package) {
        self.crates.insert(index, crate_);
    }

    pub fn entity(&self, index: DeclarationIndex) -> &Entity {
        self.crates[&index.crate_index()]
            .scope
            .get(index.local_index())
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
            match crate_.scope.ffi.foreign_types.get(binder) {
                Some(Some(binder)) => return Some(binder),
                Some(None) => continue,
                None => unreachable!(),
            }
        }

        None
    }
}

impl std::ops::Index<CrateIndex> for CrateStore {
    type Output = Package;

    fn index(&self, index: CrateIndex) -> &Self::Output {
        &self.crates[&index]
    }
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct CrateIndex(pub u16);

impl fmt::Debug for CrateIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}c", self.0)
    }
}

impl indexed_vec::Idx for CrateIndex {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn index(self) -> usize {
        self.0 as _
    }
}

// @Temporary location
// @Temporary location
/// The path to the folder of libraries shipped with the compiler/interpreter.
// @Task make this configurable via CLI option & env var & config file
pub fn distributed_libraries_path() -> PathBuf {
    const DISTRIBUTED_LIBRARIES_FOLDER: &str = "libraries";

    Path::new(env!("CARGO_MANIFEST_DIR")).join(DISTRIBUTED_LIBRARIES_FOLDER)
}

pub const CORE_LIBRARY_NAME: &str = "core";

pub const DEFAULT_SOURCE_FOLDER_NAME: &str = "source";
pub const DEFAULT_LIBRARY_ROOT_FILE_STEM: &str = "library";
pub const DEFAULT_BINARY_ROOT_FILE_STEM: &str = "main";

pub fn default_library_root_file_path() -> PathBuf {
    Path::new(DEFAULT_SOURCE_FOLDER_NAME)
        .join(DEFAULT_LIBRARY_ROOT_FILE_STEM)
        .with_extension(FILE_EXTENSION)
}

pub fn default_binary_root_file_path() -> PathBuf {
    Path::new(DEFAULT_SOURCE_FOLDER_NAME)
        .join(DEFAULT_BINARY_ROOT_FILE_STEM)
        .with_extension(FILE_EXTENSION)
}

// @Beacon @Note really not so sure about the architecture: storing the index and the scope
pub struct Package {
    pub index: CrateIndex,
    pub name: String,
    pub path: PathBuf,
    // @Temporary type
    pub version: String,
    pub description: String,
    pub private: bool,
    pub library: Option<Library>,
    // @Task make this a binaries: Vec<Binary>
    pub binary: Option<Binary>,
    // @Temporary DependencyManifest -> Dependency
    pub dependencies: HashMap<String, DependencyManifest>,
    // @Temporary
    // pub resolved_dependencies: HashMap<String, CrateIndex>,
    pub metadata: PackageMetadata,
    pub scope: CrateScope,
    // @Temporary name, @Task better name
    // if this is the crate with no dependents, the final crate,
    // the crate the user actually wants to check/run
    pub is_main: bool,
}

impl fmt::Debug for Package {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Package")
            .field("index", &self.index)
            .field("name", &self.name)
            .field("path", &self.path)
            .field("version", &self.version)
            .field("description", &self.description)
            .field("private", &self.private)
            .field("library", &self.library)
            .field("binary", &self.binary)
            .field("dependencies", &self.dependencies)
            .field("is_main", &self.is_main)
            .finish()
    }
}

impl Package {
    pub fn from_manifest(
        is_main: bool,
        index: CrateIndex,
        path: PathBuf,
        resolved_dependencies: HashMap<String, CrateIndex>,
        manifest: PackageManifest,
    ) -> Self {
        let default_library_path = path.join(default_library_root_file_path());
        let default_binary_path = path.join(default_binary_root_file_path());

        let library = match manifest.library {
            Some(library) => Some(Library {
                path: library.path.unwrap_or(default_library_path),
            }),
            None => default_library_path.exists().then(|| Library {
                path: default_library_path,
            }),
        };
        let binary = match manifest.binary {
            Some(binary) => Some(Binary {
                path: binary.path.unwrap_or(default_binary_path),
            }),
            None => default_binary_path.exists().then(|| Binary {
                path: default_binary_path,
            }),
        };

        Self {
            index,
            name: manifest.name,
            path,
            version: manifest.version,
            description: manifest.description,
            private: manifest.private,
            library,
            binary,
            // @Temporary
            dependencies: manifest.dependencies,
            metadata: PackageMetadata {
                resolved_dependencies,
            },
            scope: CrateScope::new(index),
            is_main,
        }
    }
}

// @Temporary: not sure if we should use it
pub struct PackageMetadata {
    // pub index: CrateIndex,
    pub resolved_dependencies: HashMap<String, CrateIndex>,
}

#[derive(Debug)]
pub struct Binary {
    pub path: PathBuf,
}

#[derive(Debug)]
pub struct Library {
    pub path: PathBuf,
}

pub struct Dependency {
    // @Temporary type
    pub version: String,
    pub location: String,
}

// @Question rename to package manifest??
#[derive(Deserialize, Debug)]
pub struct PackageManifest {
    // @Task newtype
    pub name: String,
    // @Temporary type
    pub version: String,
    #[serde(default)]
    pub description: String,
    #[serde(default)]
    pub private: bool,

    pub library: Option<LibraryManifest>,
    // @Task Vec<_>
    pub binary: Option<BinaryManifest>,
    #[serde(default)]
    pub dependencies: HashMap<String, DependencyManifest>,
}

impl PackageManifest {
    pub const FILE_NAME: &'static str = "package.json5";

    // @Temporary signature: define errors
    // @Task handle errors
    // @Question handle search as well?? recursing upwards??
    // @Update @Beacon @Question should we really handle (i.e. using reporter) the errors
    // here? the current message does not make sense for implicitly opening `core`
    // and probably also not for trying to open dependencies...
    // @Update @Update @Task don't handle them here but at the caller's site!!
    pub fn open(path: &Path, reporter: &Reporter) -> Result<Self> {
        // @Beacon @Task search recursively upwards!
        let manifest = match std::fs::read_to_string(path.join(Self::FILE_NAME)) {
            Ok(manifest) => manifest,
            Err(error) => {
                // @Task custom error messages dependening on io::ErrorKind
                // @Temporary message
                Diagnostic::error()
                    .message("cannot find a valid package manifest")
                    .note(format!("path: {path:?}"))
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

#[derive(Deserialize, Debug, Default)]
pub struct LibraryManifest {
    pub path: Option<PathBuf>,
}

#[derive(Deserialize, Debug, Default)]
pub struct BinaryManifest {
    pub path: Option<PathBuf>,
}

#[derive(Deserialize, Debug)]
pub struct DependencyManifest {
    // @Temporary type
    pub version: Option<String>,
    pub name: Option<String>,
    pub path: Option<String>,
    // pub url: Option<String>,
    // pub branch: Option<String>,
    // pub tag: Option<String>,
    // pub revision: Option<String>,
}
