use crate::{
    entity::Entity,
    hir::LocalDeclarationIndex,
    session::BuildSession,
    span::Spanned,
    syntax::Word,
    utility::{DisplayWith, HashMap},
};
use colored::Colorize;
use derivation::{Elements, FromStr, Str};
use index_map::IndexMap;
use std::{default::default, fmt, path::PathBuf};

pub type Components = IndexMap<ComponentIndex, Component>;

/// A sealed container of modules regarded as one unit embodying libraries and executables[^1].
///
/// [^1]: And integration and system tests, benchmarks and other things in the future.
pub struct Component {
    name: Word,
    index: ComponentIndex,
    // @Beacon @Task make this a Spanned<AbsolutePathBuf>,
    path: Spanned<PathBuf>,
    // @Task document this! @Note this is used by the lang-server which gets the document content by the client
    //       and which should not open the file at the given path to avoid TOC-TOU bugs / data races
    // @Beacon @Question should this be put on `Component` instead???
    pub(crate) content: Option<String>,
    // @Note I am not pumped about the current component type including such high-level types like "benchmark-suite"
    //       I feel like we are breaking layers of abstraction here, too. can we get rid of this field??
    type_: ComponentType,
    /// Resolved dependencies.
    pub(crate) dependencies: HashMap<Word, ComponentIndex>,
    /// All bindings inside of the component.
    // The first element has to be the root module.
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
}

impl Component {
    pub(crate) fn new(
        name: Word,
        index: ComponentIndex,
        path: Spanned<PathBuf>,
        content: Option<String>,
        type_: ComponentType,
        dependencies: HashMap<Word, ComponentIndex>,
    ) -> Self {
        Self {
            name,
            index,
            path,
            content,
            type_,
            dependencies,
            bindings: default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        use crate::{entity::EntityKind, resolver::Exposure};

        let name = Word::parse("test".into()).ok().unwrap();

        let mut component = Self::new(
            name.clone(),
            ComponentIndex(0),
            Spanned::new(default(), PathBuf::new()),
            None,
            ComponentType::Library,
            HashMap::default(),
        );
        component.bindings.insert(Entity {
            source: Spanned::new(default(), name).into(),
            parent: None,
            exposure: Exposure::Unrestricted,
            kind: EntityKind::module(),
            attributes: default(),
        });
        component
    }

    pub fn name(&self) -> &Word {
        &self.name
    }

    pub fn index(&self) -> ComponentIndex {
        self.index
    }

    pub fn path(&self) -> Spanned<&std::path::Path> {
        self.path.as_deref()
    }

    pub fn type_(&self) -> ComponentType {
        self.type_
    }

    pub fn is_library(&self) -> bool {
        self.type_() == ComponentType::Library
    }

    pub fn is_executable(&self) -> bool {
        self.type_() == ComponentType::Executable
    }

    /// Test if this component is the standard library `core`.
    pub fn is_core_library(&self, session: &BuildSession) -> bool {
        session.package_of(self.index).map_or(false, |package| {
            session[package].is_core() && self.is_library()
        })
    }

    pub fn is_goal(&self, session: &BuildSession) -> bool {
        self.index() == session.goal_component()
    }

    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name.clone(),
            index: self.index,
            type_: self.type_,
        }
    }
}

impl std::ops::Index<LocalDeclarationIndex> for Component {
    type Output = Entity;

    #[track_caller]
    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.bindings[index]
    }
}

impl std::ops::IndexMut<LocalDeclarationIndex> for Component {
    #[track_caller]
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.bindings[index]
    }
}

// @Note it would be better if we had `DebugWith`
impl DisplayWith for Component {
    type Context<'a> = &'a BuildSession;

    fn format(&self, session: &BuildSession, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "{} {} ({:?})", self.type_(), self.name(), self.index())?;

        writeln!(f, "  bindings:")?;

        for (index, entity) in &self.bindings {
            writeln!(
                f,
                "    {}: {}",
                format!("{index:?}").red(),
                entity.with((self, session))
            )?;
        }

        Ok(())
    }
}

// @Beacon @Task remove this type. it is only used by the documenter and that only because
// Component cannot really be used for some reason. investigate
#[derive(Clone)]
pub struct ComponentOutline {
    pub(crate) name: Word,
    pub(crate) index: ComponentIndex,
    pub(crate) type_: ComponentType,
}

#[derive(PartialEq, Eq, Clone, Copy, Hash)]
pub struct ComponentIndex(pub(crate) u16);

impl ComponentIndex {}

impl fmt::Debug for ComponentIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}c", self.0)
    }
}

impl index_map::Index for ComponentIndex {
    fn new(index: usize) -> Self {
        Self(index.try_into().unwrap())
    }

    fn value(self) -> usize {
        self.0 as _
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
