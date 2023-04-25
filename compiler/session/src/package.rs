use lexer::word::Word;
use std::{path::Path, sync::LazyLock};
use utility::{
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
pub struct ManifestPath(pub internment::Intern<CanonicalPathBuf>);

impl ManifestPath {
    pub const FILE_NAME: &str = "package.recnot";

    pub fn folder(&self) -> &CanonicalPath {
        self.0.parent().unwrap()
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
        Self(internment::Intern::from_ref(path))
    }
}

impl From<CanonicalPathBuf> for ManifestPath {
    fn from(path: CanonicalPathBuf) -> Self {
        Self(path.into())
    }
}
