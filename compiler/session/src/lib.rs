#![feature(decl_macro, default_free_fn, once_cell)]

use ast::Explicitness;
use derivation::{Elements, FromStr, Str};
use diagnostics::{error::Result, Reporter};
use hir::{
    interfaceable,
    special::{self, Bindings},
    DeclarationIndex, Entity, EntityKind, Expression, LocalDeclarationIndex,
};
use index_map::IndexMap;
use lexer::word::WordExt;
use span::Spanned;
use span::{SourceMap, Span};
use std::fmt;
use std::path::Path;
use std::sync::LazyLock;
use std::{
    default::default,
    ops::Index,
    path::PathBuf,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use token::Word;
use utilities::{condition, ComponentIndex, HashMap};

pub struct BuildSession {
    /// The components which have already been built in this session.
    components: HashMap<ComponentIndex, Component>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: IndexMap<PackageIndex, Package>,
    /// The mapping from component to corresponding package.
    component_packages: HashMap<ComponentIndex, PackageIndex>,
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
        packages: IndexMap<PackageIndex, Package>,
        component_packages: HashMap<ComponentIndex, PackageIndex>,
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

    pub fn target_package(&self) -> Option<PackageIndex> {
        self.package_of(self.target_component.index)
    }

    pub fn target_component(&self) -> &ComponentOutline {
        &self.target_component
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<PackageIndex> {
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

impl Index<PackageIndex> for BuildSession {
    type Output = Package;

    fn index(&self, index: PackageIndex) -> &Self::Output {
        &self.packages[index]
    }
}

pub trait InterfaceableBindingExt: Sized {
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self>;
    fn into_expression(self, session: &BuildSession) -> Result<Expression>;
}

impl InterfaceableBindingExt for interfaceable::Type {
    // @Task improve this code with the new enum logic
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use special::NumericType::*;
        use special::Type::*;

        macro is_special($binding:ident, $name:ident) {
            session.special.is(&$binding.0, $name)
        }

        Some(match &expression.bare {
            // @Note this lookup looks incredibly inefficient
            hir::BareExpression::Binding(binding) => condition! {
                is_special!(binding, Unit) => Self::Unit,
                is_special!(binding, Bool) => Self::Bool,
                is_special!(binding, Nat) => Self::Nat,
                is_special!(binding, Nat32) => Self::Nat32,
                is_special!(binding, Nat64) => Self::Nat64,
                is_special!(binding, Int) => Self::Int,
                is_special!(binding, Int32) => Self::Int32,
                is_special!(binding, Int64) => Self::Int64,
                is_special!(binding, Text) => Self::Text,
                else => return None,
            },
            hir::BareExpression::Application(application) => match &application.callee.bare {
                hir::BareExpression::Binding(binding) if is_special!(binding, Option) => {
                    Self::Option(Box::new(Self::from_expression(
                        &application.argument,
                        session,
                    )?))
                }
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, session: &BuildSession) -> Result<Expression> {
        use special::{NumericType::*, Type::*};

        macro special($name:ident) {
            session.require_special($name, None)
        }

        match self {
            Self::Unit => special!(Unit),
            Self::Bool => special!(Bool),
            Self::Nat => special!(Nat),
            Self::Nat32 => special!(Nat32),
            Self::Nat64 => special!(Nat64),
            Self::Int => special!(Int),
            Self::Int32 => special!(Int32),
            Self::Int64 => special!(Int64),
            Self::Text => special!(Text),
            Self::Option(type_) => Ok(application(
                special!(Option)?,
                type_.into_expression(session)?,
            )),
        }
    }
}

impl InterfaceableBindingExt for interfaceable::Value {
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use hir::BareExpression::*;
        use special::Constructor::*;

        macro is_special($binding:ident, $name:ident) {
            session.special.is(&$binding.0, $name)
        }

        Some(match &expression.bare {
            Text(text) => {
                use hir::Text::*;

                match &**text {
                    // @Note not great
                    Text(text) => Self::Text(text.clone()),
                }
            }
            Number(number) => {
                use hir::Number::*;

                match &**number {
                    Nat(nat) => Self::Nat(nat.clone()),
                    Nat32(nat) => Self::Nat32(*nat),
                    Nat64(nat) => Self::Nat64(*nat),
                    Int(int) => Self::Int(int.clone()),
                    Int32(int) => Self::Int32(*int),
                    Int64(int) => Self::Int64(*int),
                }
            }
            Binding(binding) => condition! {
                is_special!(binding, UnitUnit) => Self::Unit,
                is_special!(binding, BoolFalse) => Self::Bool(false),
                is_special!(binding, BoolTrue) => Self::Bool(true),
                else => return None,
            },

            Application(application0) => match &application0.callee.bare {
                Binding(binding) if is_special!(binding, OptionNone) => Self::Option {
                    value: None,
                    type_: interfaceable::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.bare {
                    Binding(binding) if is_special!(binding, OptionSome) => Self::Option {
                        value: Some(Box::new(Self::from_expression(
                            &application0.argument,
                            session,
                        )?)),
                        type_: interfaceable::Type::from_expression(
                            &application1.argument,
                            session,
                        )?,
                    },
                    _ => return None,
                },
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, session: &BuildSession) -> Result<Expression> {
        use hir::Number::*;
        use special::{Constructor::*, Type::*};

        Ok(match self {
            Self::Unit => session.require_special(Unit, None)?,
            Self::Bool(value) => {
                session.require_special(if value { BoolTrue } else { BoolFalse }, None)?
            }
            Self::Text(value) => Expression::bare(hir::Text::Text(value).into()),
            Self::Nat(value) => Expression::bare(Nat(value).into()),
            Self::Nat32(value) => Expression::bare(Nat32(value).into()),
            Self::Nat64(value) => Expression::bare(Nat64(value).into()),
            Self::Int(value) => Expression::bare(Int(value).into()),
            Self::Int32(value) => Expression::bare(Int32(value).into()),
            Self::Int64(value) => Expression::bare(Int64(value).into()),
            Self::Option { type_, value } => match value {
                Some(value) => application(
                    application(
                        session.require_special(OptionSome, None)?,
                        type_.into_expression(session)?,
                    ),
                    value.into_expression(session)?,
                ),
                None => application(
                    session.require_special(OptionNone, None)?,
                    type_.into_expression(session)?,
                ),
            },
            Self::IO { index, arguments } => Expression::new(
                default(),
                default(),
                hir::IO {
                    index,
                    arguments: arguments
                        .into_iter()
                        .map(|argument| argument.into_expression(session))
                        .collect::<Result<Vec<_>>>()?,
                }
                .into(),
            ),
        })
    }
}

#[allow(dead_code)] // @Temporary
fn application(callee: Expression, argument: Expression) -> Expression {
    Expression::new(
        default(),
        default(),
        hir::Application {
            callee,
            argument,
            explicitness: Explicitness::Explicit,
        }
        .into(),
    )
}

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
        path: Spanned<PathBuf>,
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

    pub fn test() -> Self {
        use hir::Exposure;

        let name: Word = Word::parse("test".into()).ok().unwrap();

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
    // @Beacon @Task make this an “AbsolutePathBuf”
    pub path: PathBuf,
    pub version: Version,
    pub description: String,
    pub components: HashMap<Word, PossiblyUnresolvedComponent>,
}

impl Package {
    /// Test if this package is the standard library `core`.
    fn is_core(&self) -> bool {
        self.path == core_package_path()
    }
}

/// The path to the folder of packages shipped with the compiler.
pub fn distributed_packages_path() -> &'static Path {
    // @Task make this configurable via CLI option & env var & config file
    static PATH: LazyLock<PathBuf> = LazyLock::new(|| {
        Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("../../library")
            .canonicalize()
            .unwrap()
    });

    &PATH
}

pub fn core_package_path() -> &'static Path {
    static PATH: LazyLock<PathBuf> =
        LazyLock::new(|| distributed_packages_path().join(CORE_PACKAGE_NAME));

    &PATH
}

pub const CORE_PACKAGE_NAME: &str = "core";

pub fn core_package_name() -> Word {
    Word::new_unchecked("core".into())
}

#[derive(Debug)]
pub struct Version(pub String);

#[derive(Debug)]
pub enum PossiblyUnresolvedComponent {
    Unresolved,
    Resolved(ComponentIndex),
}

#[derive(PartialEq, Eq, Clone, Copy, index_map::Index)]
pub struct PackageIndex(usize);

impl fmt::Debug for PackageIndex {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}p", self.0)
    }
}
