use crate::{
    entity::Entity,
    hir::{Identifier, LocalDeclarationIndex},
    package::{Package, PackageIndex},
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
    pub metadata: ComponentMetadata,
    /// Resolved dependencies.
    pub(crate) dependencies: HashMap<Word, ComponentIndex>,
    // @Task don't make this a field! decouple the concept of "library vs executable" from components
    //       instead, at the very least, make this a method that looks for a top-level `main`.
    //       by nature "after the fact", i.e. after name resolution happens.
    //       I feel like us registering the program entry during name resolution breaks layers of abstraction
    /// The `main` function (_program entry_) for executable components.
    pub entry: Option<Identifier>,
    /// All bindings inside of the component.
    // The first element has to be the root module.
    pub(crate) bindings: IndexMap<LocalDeclarationIndex, Entity>,
}

impl Component {
    pub(crate) fn new(
        metadata: ComponentMetadata,
        dependencies: HashMap<Word, ComponentIndex>,
    ) -> Self {
        Self {
            metadata,
            dependencies,
            entry: default(),
            bindings: default(),
        }
    }

    #[cfg(test)]
    pub(crate) fn test() -> Self {
        use crate::{entity::EntityKind, resolver::Exposure};

        let name = Word::parse("test".into()).ok().unwrap();

        let mut component = Self::new(
            ComponentMetadata::new(
                name.clone(),
                ComponentIndex(0),
                PackageIndex::new_unchecked(0),
                Spanned::new(default(), PathBuf::new()),
                None,
                ComponentType::Library,
            ),
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
        &self.metadata.name
    }

    pub(crate) fn index(&self) -> ComponentIndex {
        self.metadata.index
    }

    pub fn path(&self) -> Spanned<&std::path::Path> {
        self.metadata.path.as_deref()
    }

    pub fn type_(&self) -> ComponentType {
        self.metadata.type_
    }

    pub fn is_library(&self) -> bool {
        self.type_() == ComponentType::Library
    }

    pub fn is_executable(&self) -> bool {
        self.type_() == ComponentType::Executable
    }

    pub fn package<'s>(&self, session: &'s BuildSession) -> &'s Package {
        &session[self.metadata.package]
    }

    /// Test if this component is the standard library `core`.
    pub fn is_core_library(&self, session: &BuildSession) -> bool {
        self.package(session).is_core() && self.is_library()
    }

    pub fn is_goal(&self, session: &BuildSession) -> bool {
        self.index() == session.goal_component()
    }

    // @Task replace this with a method on BuildSession that looks through a `Package`s list of components!
    // @Note I want to get rid of the backreference `package` in `Component`s
    // @Update however, it's not as easy, since a `Component` needs to be fully built to be able to
    //         occur inside of a `Package` since a `ComponentIndex` has to be created
    pub fn in_goal_package(&self, session: &BuildSession) -> bool {
        self.metadata.package == session.goal_package()
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
        writeln!(
            f,
            "{} {} ({:?}) {:?}",
            self.type_(),
            self.name(),
            self.index(),
            self.metadata.package
        )?;

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

/// Metadata of a [`Component`].
#[derive(Clone)]
// @Note I don't like this name!
pub struct ComponentMetadata {
    pub(crate) name: Word,
    pub(crate) index: ComponentIndex,
    // @Beacon @Beacon @Task not all components need to have a corresp. package!!!!
    //                       make this field optional! even better if we could get rid of it entirely!
    //                       we are kind of breaking layers of abstraction here: components vs packages!
    //                       @Update
    //                       It's not as easy, since a `Package` needs a list of `ComponentIndex`es
    //                       but they aren't created until the components in question are fully built
    //                       however, in many cases, we want to get the package of the current component
    //                       (which ofc hasn't been built yet)
    //                       One solution (maybe): identifiy components in Packages via an
    //                       enum { Unbuilt { name: Word, "type", path: PathButh }, Built(ComponentIndex) }
    //                       ooorrr, store `ComponentMetadata` in variant Unbuilt and make
    //                       @Beacon ComponentMetadata.index an Option<ComponentIndex> / PossiblyUnresolved<ComponentIndex>
    pub(crate) package: PackageIndex,
    pub(crate) path: Spanned<PathBuf>,
    // @Task document this! @Note this is used by the lang-server which gets the document content by the client
    //       and which should not open the file at the given path to avoid TOC-TOU bugs / data races
    // @Beacon @Question should this be put on `Component` instead???
    pub(crate) content: Option<String>,
    // @Note I am not pumped about the current component type including such high-level types like "benchmark-suite"
    //       I feel like we are breaking layers of abstraction here, too. can we get rid of this field??
    pub(crate) type_: ComponentType,
}

impl ComponentMetadata {
    pub(crate) fn new(
        name: Word,
        index: ComponentIndex,
        package: PackageIndex,
        path: Spanned<PathBuf>,
        content: Option<String>,
        type_: ComponentType,
    ) -> Self {
        Self {
            name,
            index,
            package,
            path,
            content,
            type_,
        }
    }
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
