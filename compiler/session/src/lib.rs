#![feature(decl_macro, default_free_fn, once_cell)]

use derivation::{Elements, FromStr, Str};
use diagnostics::{error::Result, Reporter};
use hir::{
    special::{self, Bindings},
    DeclarationIndex, Entity, Expression, LocalDeclarationIndex,
};
use index_map::IndexMap;
use span::{SourceMap, Span, Spanned};
use std::{
    default::default,
    fmt,
    ops::Index,
    path::Path,
    sync::{Arc, LazyLock, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use token::Word;
use utilities::{
    path::{CanonicalPath, CanonicalPathBuf},
    ComponentIndex, HashMap,
};

pub mod interfaceable;

pub struct BuildSession {
    /// The components which have already been built in this session.
    components: HashMap<ComponentIndex, Component>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: HashMap<ManifestPath, Package>,
    /// The mapping from component to corresponding package.
    // @Task remove
    component_packages: HashMap<ComponentIndex, ManifestPath>,
    // @Task support multiple target components (depending on a user-supplied component filter)
    target_component: ComponentOutline,
    /// Intrinsic and known bindings.
    pub special: Bindings,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildSession {
    pub const OUTPUT_FOLDER_NAME: &'static str = "build";

    pub fn new(
        packages: HashMap<ManifestPath, Package>,
        component_packages: HashMap<ComponentIndex, ManifestPath>,
        target_component: ComponentOutline,
        map: &Arc<RwLock<SourceMap>>,
        reporter: Reporter,
    ) -> Self {
        Self {
            components: default(),
            packages,
            component_packages,
            target_component,
            special: default(),
            map: map.clone(),
            reporter,
        }
    }

    pub fn test() -> Self {
        use index_map::Index;

        let map: Arc<RwLock<SourceMap>> = default();

        Self {
            components: default(),
            packages: default(),
            component_packages: default(),
            target_component: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                index: ComponentIndex::new(0),
            },
            special: default(),
            map: map.clone(),
            reporter: Reporter::stderr().with_map(map),
        }
    }

    pub fn target_package(&self) -> Option<ManifestPath> {
        self.package_of(self.target_component.index)
    }

    pub fn target_component(&self) -> &ComponentOutline {
        &self.target_component
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<ManifestPath> {
        self.component_packages.get(&component).copied()
    }

    pub fn in_target_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component)
            .map_or(false, |package| self.target_package() == Some(package))
    }

    pub fn add(&mut self, component: Component) {
        self.components.insert(component.index(), component);
    }

    pub fn require_special(
        &self,
        special: impl Into<special::Binding>,
        user: Option<Span>,
    ) -> Result<Expression> {
        self.special
            .require(special, user)
            .map_err(|error| error.report(self.reporter()))
    }

    pub fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.map.read().unwrap()
    }

    pub fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.map.write().unwrap()
    }

    pub fn reporter(&self) -> &Reporter {
        &self.reporter
    }
}

impl Index<DeclarationIndex> for BuildSession {
    type Output = Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        &self[index.component()][index.local_unchecked()]
    }
}

impl Index<ComponentIndex> for BuildSession {
    type Output = Component;

    fn index(&self, index: ComponentIndex) -> &Self::Output {
        self.components.get(&index).unwrap_or_else(|| {
            panic!(
                "attempt to look up unbuilt or unknown component ‘{index:?}’ in the build session"
            )
        })
    }
}

impl Index<ManifestPath> for BuildSession {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.packages[&path]
    }
}

pub type Components = IndexMap<ComponentIndex, Component>;

/// A sealed container of modules regarded as one unit embodying libraries and executables[^1].
///
/// [^1]: And integration and system tests, benchmarks and other things in the future.
pub struct Component {
    name: Word,
    index: ComponentIndex,
    path: Spanned<CanonicalPathBuf>,
    // @Task document this! @Note this is used by the lang-server which gets the document content by the client
    //       and which should not open the file at the given path to avoid TOC-TOU bugs / data races
    pub content: Option<Arc<String>>,
    // @Note I am not pumped about the current component type including such high-level types like "benchmark-suite"
    //       I feel like we are breaking layers of abstraction here, too. can we get rid of this field??
    type_: ComponentType,
    /// Resolved dependencies.
    pub dependencies: HashMap<Word, ComponentIndex>,
    /// All bindings inside of the component.
    // The first element has to be the root module.
    pub bindings: IndexMap<LocalDeclarationIndex, Entity>,
}

impl Component {
    pub fn new(
        name: Word,
        index: ComponentIndex,
        path: Spanned<CanonicalPathBuf>,
        content: Option<Arc<String>>,
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

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use hir::Exposure;

        const NAME: &str = "test";
        const PATH: &str = "/test"; // @Task use a different path on Windows

        let name = Word::new_unchecked(NAME.into());
        let path = CanonicalPathBuf::new_unchecked(std::path::PathBuf::from(PATH));

        let mut component = Self::new(
            name.clone(),
            ComponentIndex(0),
            Spanned::new(default(), path),
            None,
            ComponentType::Library,
            HashMap::default(),
        );
        component.bindings.insert(Entity {
            source: Spanned::new(default(), name).into(),
            parent: None,
            exposure: Exposure::Unrestricted,
            kind: hir::EntityKind::module(),
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

    pub fn path(&self) -> Spanned<&CanonicalPath> {
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
            session.packages[&package].is_core() && self.is_library()
        })
    }

    pub fn is_target(&self, session: &BuildSession) -> bool {
        self.index() == session.target_component.index
    }

    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name.clone(),
            index: self.index,
        }
    }

    /// The root module / the component root.
    pub fn root(&self) -> DeclarationIndex {
        self.root_local().global(self)
    }

    /// The root module / the component root as a local index.
    #[allow(clippy::unused_self)] // leads to a more legible API
    pub fn root_local(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
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

pub trait IdentifierExt {
    fn local_declaration_index(&self, component: &Component) -> Option<LocalDeclarationIndex>;
}

impl IdentifierExt for hir::Identifier {
    fn local_declaration_index(&self, component: &Component) -> Option<LocalDeclarationIndex> {
        self.declaration_index()?.local(component)
    }
}

pub trait DeclarationIndexExt {
    #[allow(clippy::wrong_self_convention)] // false positive IMO, @Task report
    fn is_local(self, component: &Component) -> bool;

    fn local(self, component: &Component) -> Option<LocalDeclarationIndex>;
}

impl DeclarationIndexExt for DeclarationIndex {
    fn is_local(self, component: &Component) -> bool {
        self.component() == component.index()
    }

    fn local(self, component: &Component) -> Option<LocalDeclarationIndex> {
        self.is_local(component).then(|| self.local_unchecked())
    }
}

pub trait LocalDeclarationIndexExt {
    fn global(self, component: &Component) -> DeclarationIndex;
}

impl LocalDeclarationIndexExt for LocalDeclarationIndex {
    fn global(self, component: &Component) -> DeclarationIndex {
        DeclarationIndex::new(component.index(), self)
    }
}

// @Beacon @Task remove this type. it is only used by the documenter and that only because
// Component cannot really be used for some reason. investigate
#[derive(Clone)]
pub struct ComponentOutline {
    pub name: Word,
    pub index: ComponentIndex,
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

/// A collection of [components](Component) and some metadata.
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
    fn is_core(&self) -> bool {
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
    pub const FILE_NAME: &str = "package.metadata";

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
