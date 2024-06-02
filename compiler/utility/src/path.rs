use std::{
    borrow::Borrow,
    ffi::OsStr,
    fmt, io,
    ops::Deref,
    path::{Path, PathBuf},
    ptr,
};

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CanonicalPath(Path);

impl CanonicalPath {
    pub fn new_unchecked<P>(path: &P) -> &Self
    where
        P: ?Sized + AsRef<Path>,
    {
        unsafe { &*(ptr::from_ref(path.as_ref()) as *const Self) }
    }

    pub fn as_path(&self) -> &Path {
        self
    }

    pub fn parent(&self) -> Option<&Self> {
        self.0.parent().map(Self::new_unchecked)
    }
}

impl Deref for CanonicalPath {
    type Target = Path;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl AsRef<Path> for CanonicalPath {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<OsStr> for CanonicalPath {
    fn as_ref(&self) -> &OsStr {
        self.as_os_str()
    }
}

impl ToOwned for CanonicalPath {
    type Owned = CanonicalPathBuf;

    fn to_owned(&self) -> Self::Owned {
        CanonicalPathBuf(self.0.to_owned())
    }
}

impl fmt::Debug for CanonicalPath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Clone)]
pub struct CanonicalPathBuf(PathBuf);

impl CanonicalPathBuf {
    pub fn new_unchecked(path: PathBuf) -> Self {
        Self(path)
    }

    pub fn new<P>(path: &P) -> io::Result<Self>
    where
        P: ?Sized + AsRef<Path>,
    {
        path.as_ref().canonicalize().map(Self)
    }

    pub fn into_inner(self) -> PathBuf {
        self.0
    }

    pub fn as_path(&self) -> &Path {
        self
    }
}

impl fmt::Debug for CanonicalPathBuf {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl Deref for CanonicalPathBuf {
    type Target = CanonicalPath;

    fn deref(&self) -> &Self::Target {
        CanonicalPath::new_unchecked(&self.0)
    }
}

impl AsRef<Path> for CanonicalPathBuf {
    fn as_ref(&self) -> &Path {
        self
    }
}

impl AsRef<OsStr> for CanonicalPathBuf {
    fn as_ref(&self) -> &OsStr {
        self.as_os_str()
    }
}

impl Borrow<CanonicalPath> for CanonicalPathBuf {
    fn borrow(&self) -> &CanonicalPath {
        self
    }
}

impl From<&CanonicalPath> for CanonicalPathBuf {
    fn from(path: &CanonicalPath) -> Self {
        path.to_owned()
    }
}

impl TryFrom<&Path> for CanonicalPathBuf {
    type Error = io::Error;

    fn try_from(path: &Path) -> Result<Self, Self::Error> {
        Self::new(path)
    }
}
