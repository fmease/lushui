// @Question can we move this into `package`?

use crate::{ComponentOutline, Session};
use derivation::{Elements, FromStr, Str};
use span::Spanned;
use std::fmt;
use token::Word;
use utilities::{path::CanonicalPathBuf, ComponentIndex, HashMap};

// @Beacon @Beacon @Beacon @Task rename BuildUnit again to sth containing "Component"

pub struct BuildUnit {
    pub name: Word,
    pub index: ComponentIndex,
    // @Task make this a PathBuf (?)
    pub path: Spanned<CanonicalPathBuf>,
    pub type_: ComponentType,
    pub dependencies: HashMap<Word, ComponentIndex>,
}

impl BuildUnit {
    // @Temporary
    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name.clone(),
            index: self.index,
        }
    }

    /// Test if this component is the standard library `core`.
    // @Temporary
    pub fn is_core_library(&self, session: &Session<'_>) -> bool {
        session.package_of(self.index).map_or(false, |package| {
            session[package].is_core() && self.type_ == ComponentType::Library
        })
    }

    // @Beacon @Beacon @Beacon @Task store in a BuildUnit whether it is a root or not!
    // and then remove this method and the root_component in Session
    pub fn is_root(&self, session: &Session<'_>) -> bool {
        self.index == session.context.root_component.index
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, Elements, FromStr, Str)]
#[format(dash_case)]
pub enum ComponentType {
    BenchmarkSuite,
    Example,
    Executable,
    Library,
    TestSuite,
}

impl ComponentType {
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

impl fmt::Display for ComponentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
