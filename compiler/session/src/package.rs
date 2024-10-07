use index_map::IndexMap;
use lexer::word::Word;
use std::{
    path::Path,
    sync::{LazyLock, Mutex},
};
use utility::{
    default,
    path::{CanonicalPath, CanonicalPathBuf},
    ComponentIndex, HashMap,
};

/// A collection of [components](crate::Component) and some metadata.
#[derive(Debug)]
pub struct Package {
    pub name: Word,
    pub path: ManifestPath,
    pub version: Version,
    pub description: String,
    pub components: HashMap<Word, PossiblyUnresolvedComponent>,
}

impl Package {
    pub fn folder(&self) -> &CanonicalPath {
        self.path.folder()
    }

    /// Test if this package is the standard library `core`.
    pub(crate) fn is_core(&self) -> bool {
        self.path == ManifestPath::core()
    }
}

pub const CORE_PACKAGE_NAME: &str = "core"; // @Task intern this on prefill

pub fn core_package_name() -> Word {
    Word::new_unchecked("core".into())
}

/// The path to the folder of packages shipped with the compiler.
pub fn distributed_packages_path() -> &'static CanonicalPath {
    static PATH: LazyLock<CanonicalPathBuf> = LazyLock::new(|| {
        CanonicalPathBuf::new(&Path::new(env!("CARGO_MANIFEST_DIR")).join("../../library")).unwrap()
    });

    &PATH
}

#[derive(Debug)]
pub struct Version(pub String);

#[derive(Debug)]
pub enum PossiblyUnresolvedComponent {
    Unresolved,
    Resolved(ComponentIndex),
}

// @Task make this type more ergonomic to use

#[derive(Clone, Copy, PartialEq, Eq, Hash, Debug)]
pub struct ManifestPath(u32);

impl ManifestPath {
    pub const FILE_NAME: &'static str = "package.recnot";

    pub fn to_path(self) -> &'static CanonicalPath {
        Interner::the().lock().unwrap().canonical_paths[self]
    }

    pub fn folder(&self) -> &CanonicalPath {
        self.to_path().parent().unwrap()
    }

    pub fn core() -> Self {
        static PATH: LazyLock<ManifestPath> = LazyLock::new(|| {
            let mut path = distributed_packages_path().to_path_buf();
            path.extend([CORE_PACKAGE_NAME, ManifestPath::FILE_NAME]);
            ManifestPath::from(CanonicalPathBuf::new_unchecked(path))
        });

        *PATH
    }
}

impl From<&CanonicalPath> for ManifestPath {
    fn from(path: &CanonicalPath) -> Self {
        Interner::the().lock().unwrap().intern_borrowed(path)
    }
}

impl From<CanonicalPathBuf> for ManifestPath {
    fn from(path: CanonicalPathBuf) -> Self {
        Interner::the().lock().unwrap().intern_owned(path)
    }
}

impl index_map::Index for ManifestPath {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn value(self) -> usize {
        self.0 as _
    }
}

// We roll our own non-generic interner because it's not worth pulling an extra dependency for this.
// Furthermore, I don't like the approach all(?) generic interning crates follow which is to utilize
// a `TypeId`-indexed map (I don't like `TypeId` as it's not 100% collision-resistant ATTOW).
#[derive(Default)]
struct Interner {
    manifest_paths: HashMap<&'static CanonicalPath, ManifestPath>,
    canonical_paths: IndexMap<ManifestPath, &'static CanonicalPath>,
}

impl Interner {
    fn the() -> &'static Mutex<Self> {
        static SELF: LazyLock<Mutex<Interner>> = LazyLock::new(|| default());

        &SELF
    }

    fn intern_borrowed(&mut self, path: &CanonicalPath) -> ManifestPath {
        if let Some(&path) = self.manifest_paths.get(path) {
            return path;
        }

        self.insert(Box::leak(Box::from(path)))
    }

    fn intern_owned(&mut self, path: CanonicalPathBuf) -> ManifestPath {
        if let Some(&path) = self.manifest_paths.get(&*path) {
            return path;
        }

        self.insert(CanonicalPathBuf::leak(path))
    }

    fn insert(&mut self, path: &'static CanonicalPath) -> ManifestPath {
        let manifest_path = self.canonical_paths.insert(path);
        self.manifest_paths.insert(path, manifest_path);
        manifest_path
    }
}
