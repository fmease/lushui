// @Question can we move this into `package`?

use crate::{ComponentOutline, Session};
use derivation::{Elements, FromStr, Str};
use lexer::word::Word;
use span::Spanned;
use std::fmt;
use utility::{path::CanonicalPathBuf, CompIdx, HashMap};

// @Beacon @Beacon @Beacon @Task rename BuildUnit again to sth containing "Component"

pub struct BuildUnit {
    pub name: Word,
    pub index: CompIdx,
    // @Task make this a PathBuf (?)
    pub path: Spanned<CanonicalPathBuf>,
    pub ty: CompTy,
    pub dependencies: HashMap<Word, CompIdx>,
}

impl BuildUnit {
    // @Temporary
    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name,
            idx: self.index,
        }
    }

    /// Test if this component is the standard library `core`.
    // @Temporary
    pub fn is_core_lib(&self, sess: &Session<'_>) -> bool {
        sess.pkg_of(self.index).map_or(false, |pkg| {
            sess[pkg].is_core() && self.ty == CompTy::Library
        })
    }

    // @Beacon @Beacon @Beacon @Task store in a BuildUnit whether it is a root or not!
    // and then remove this method and the root_component in Session
    pub fn is_root(&self, sess: &Session<'_>) -> bool {
        self.index == sess.cx.root_comp.idx
    }
}

/// The type of a component.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Elements, FromStr, Str)]
#[format(dash_case)]
pub enum CompTy {
    BenchmarkSuite,
    Example,
    Executable,
    Library,
    TestSuite,
}

impl CompTy {
    pub const fn short_name(self) -> &'static str {
        match self {
            Self::BenchmarkSuite => "bench",
            Self::Example => "example",
            Self::Executable => "exe",
            Self::Library => "lib",
            Self::TestSuite => "test",
        }
    }
}

impl fmt::Display for CompTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
