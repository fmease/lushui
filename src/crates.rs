use crate::{
    entity::Entity,
    error::Result,
    resolver::{CrateScope, DeclarationIndex, Identifier},
    util::HashMap,
};
use serde::Deserialize;
use std::{
    convert::TryInto,
    fmt,
    path::{Path, PathBuf},
};

/// A collection of [crates](Crate).
#[derive(Default)]
pub struct CrateStore {
    crates: HashMap<CrateIndex, Crate>,
}

impl CrateStore {
    pub fn add(&mut self, index: CrateIndex, crate_: Crate) {
        self.crates.insert(index, crate_);
    }

    // @Beacon @Temporary insecure, naive etc!!!
    pub fn by_name(&self, name: &str) -> Option<CrateIndex> {
        self.crates
            .iter()
            .find(|(_, crate_)| crate_.name == name)
            .map(|(&index, _)| index)
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
    type Output = Crate;

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

const DEFAULT_LIBRARY_ROOT_FILE_PATH: &str = "source/library.lushui";
const DEFAULT_BINARY_ROOT_FILE_PATH: &str = "source/main.lushui";

// @Beacon @Note really not so sure about the architecture: storing the index and the scope
pub struct Crate {
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
    pub dependencies: HashMap<String, Dependency>,
    pub scope: CrateScope,
    // @Temporary name, @Task better name
    // if this is the crate with no dependents, the final crate,
    // the crate the user actually wants to check/run
    pub is_main: bool,
}

impl Crate {
    pub fn from_manifest(
        is_main: bool,
        index: CrateIndex,
        path: PathBuf,
        manifest: Manifest,
    ) -> Self {
        let default_library_path = path.join(DEFAULT_LIBRARY_ROOT_FILE_PATH);
        let default_binary_path = path.join(DEFAULT_BINARY_ROOT_FILE_PATH);

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
        // @Beacon @Temporary
        let dependencies = HashMap::default();

        Self {
            index,
            name: manifest.name,
            path,
            version: manifest.version,
            description: manifest.description,
            private: manifest.private,
            library,
            binary,
            dependencies,
            scope: CrateScope::new(index),
            is_main,
        }
    }
}

pub struct Binary {
    pub path: PathBuf,
}

pub struct Library {
    pub path: PathBuf,
}

pub struct Dependency {
    // @Temporary type
    pub version: String,
    pub location: String,
}

#[derive(Deserialize)]
pub struct Manifest {
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

impl Manifest {
    pub const FILE_NAME: &'static str = "package.json5";

    // @Temporary signature: define errors
    // @Task handle errors
    // @Question handle search as well?? recursing upwards??
    pub fn open(path: &Path) -> Result<Self, !> {
        let manifest = std::fs::read_to_string(path.join(Self::FILE_NAME)).unwrap();
        Ok(json5::from_str(&manifest).unwrap())
    }
}

#[derive(Deserialize, Default)]
pub struct LibraryManifest {
    pub path: Option<PathBuf>,
}

#[derive(Deserialize, Default)]
pub struct BinaryManifest {
    pub path: Option<PathBuf>,
}

#[derive(Deserialize)]
pub struct DependencyManifest {
    // @Temporary type
    pub version: String,
    pub location: Option<String>,
}
