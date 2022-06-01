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
    /// The `main` function (_program entry_) for executable components.
    pub entry: Option<Identifier>,
    /// All bindings inside of the component.
    // The first element ha to be the root module.
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
pub struct ComponentMetadata {
    pub(crate) name: Word,
    pub(crate) index: ComponentIndex,
    pub(crate) package: PackageIndex,
    pub(crate) path: Spanned<PathBuf>,
    pub(crate) type_: ComponentType,
    /// Indicates if the name of the library or executable component coincides with
    /// the name of the executable[^1] or library component, respectively.
    ///
    /// [^1]: We haven't implemented multiple executable components per package yet.
    // @Beacon @Note this is no longer up to date with the concept of secondary libraries etc.
    pub is_ambiguously_named_within_package: bool,
}

impl ComponentMetadata {
    pub(crate) fn new(
        name: Word,
        index: ComponentIndex,
        package: PackageIndex,
        path: Spanned<PathBuf>,
        type_: ComponentType,
    ) -> Self {
        Self {
            name,
            index,
            package,
            path,
            type_,
            is_ambiguously_named_within_package: false,
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

impl fmt::Display for ComponentType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
