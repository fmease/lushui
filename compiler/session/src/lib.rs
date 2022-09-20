#![feature(decl_macro, default_free_fn, once_cell)]

use ast::Explicitness;
use derivation::{Elements, FromStr, Str};
use diagnostics::{Diagnostic, ErrorCode, Reporter};
use error::Result;
use hir::{
    interfaceable, intrinsic, known, DeclarationIndex, Entity, EntityKind, Expression, Identifier,
    LocalDeclarationIndex,
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
    /// The corresponding package of the components.
    component_packages: HashMap<ComponentIndex, PackageIndex>,
    goal_component: ComponentOutline,
    // @Task Identifier -> DeclarationIndex
    known_bindings: HashMap<known::Binding, Identifier>,
    // @Task make this a DoubleHashMap
    intrinsic_types: HashMap<intrinsic::Type, Identifier>,
    intrinsic_functions: HashMap<intrinsic::Function, intrinsic::FunctionValue>,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildSession {
    pub const OUTPUT_FOLDER_NAME: &'static str = "build";

    /// Create a new build session with all intrinsic functions defined.
    pub fn new(
        packages: IndexMap<PackageIndex, Package>,
        component_packages: HashMap<ComponentIndex, PackageIndex>,
        goal_component: ComponentOutline,
        map: &Arc<RwLock<SourceMap>>,
        reporter: Reporter,
    ) -> Self {
        Self {
            components: default(),
            packages,
            component_packages,
            goal_component,
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: intrinsic::functions(),
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
            goal_component: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                index: ComponentIndex::new(0),
            },
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: default(),
            map: map.clone(),
            reporter: Reporter::stderr().with_map(map),
        }
    }

    pub fn goal_package(&self) -> Option<PackageIndex> {
        self.package_of(self.goal_component.index)
    }

    pub fn goal_component(&self) -> &ComponentOutline {
        &self.goal_component
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<PackageIndex> {
        self.component_packages.get(&component).copied()
    }

    pub fn in_goal_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component)
            .map_or(false, |package| self.goal_package() == Some(package))
    }

    pub fn add(&mut self, component: Component) {
        self.components.insert(component.index(), component);
    }

    pub fn known_bindings(&self) -> impl Iterator<Item = (known::Binding, &Identifier)> {
        self.known_bindings
            .iter()
            .map(|(&known, identifier)| (known, identifier))
    }

    pub fn known_binding(&self, known: known::Binding) -> Option<&Identifier> {
        self.known_bindings.get(&known)
    }

    pub fn look_up_known_binding(&self, known: known::Binding) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| missing_known_binding_error(known).report(self.reporter()))?
            .into_expression())
    }

    pub fn look_up_known_type(
        &self,
        known: known::Binding,
        expression: Span,
    ) -> Result<Expression> {
        Ok(self
            .known_binding(known)
            .cloned()
            .ok_or_else(|| missing_known_type_error(known, expression).report(self.reporter()))?
            .into_expression())
    }

    pub fn define_known_binding(
        &mut self,
        binder: &Identifier,
        namespace: Option<&str>,
        attribute: Span,
    ) -> Result {
        let Some(binding) = known::Binding::parse(namespace, binder.as_str()) else {
            return Err(Diagnostic::error()
                .code(ErrorCode::E063)
                .message(format!(
                    "‘{}{binder}’ is not a known binding",
                    namespace.map(|namespace| format!(".{namespace}"))
                        .unwrap_or_default()
                ))
                .primary_span(binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        if let Some(previous) = self.known_bindings.get(&binding) {
            return Err(Diagnostic::error()
                .code(ErrorCode::E039)
                .message(format!(
                    "the known binding ‘{}’ is defined multiple times",
                    binding.path()
                ))
                .labeled_primary_span(binder, "redefinition")
                .labeled_secondary_span(previous, "previous definition")
                .report(self.reporter()));
        }

        self.known_bindings.insert(binding, binder.clone());

        Ok(())
    }

    pub fn intrinsic_types(&self) -> impl Iterator<Item = (intrinsic::Type, &Identifier)> {
        self.intrinsic_types
            .iter()
            .map(|(&intrinsic, identifier)| (intrinsic, identifier))
    }

    pub fn intrinsic_type(&self, intrinsic: intrinsic::Type) -> Option<&Identifier> {
        self.intrinsic_types.get(&intrinsic)
    }

    pub fn type_(&self) -> hir::Expression {
        // @Task don't unwrap!
        self.intrinsic_type(hir::intrinsic::Type::Type)
            .cloned()
            .unwrap()
            .into_expression()
    }

    // @Beacon @Task support paths!
    pub fn define_intrinsic_type(&mut self, binder: Identifier, attribute: Span) -> Result {
        let Ok(intrinsic) = binder.as_str().parse::<intrinsic::Type>() else {
            return Err(unrecognized_intrinsic_binding_error(binder.as_str(), intrinsic::Kind::Type)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        if let Some(previous) = self.intrinsic_type(intrinsic) {
            return Err(Diagnostic::error()
                .code(ErrorCode::E040)
                .message(format!(
                    "the intrinsic type ‘{intrinsic}’ is defined multiple times",
                ))
                .labeled_primary_span(&binder, "redefinition")
                .labeled_secondary_span(previous as &_, "previous definition")
                .report(self.reporter()));
        }

        self.intrinsic_types.insert(intrinsic, binder);

        Ok(())
    }

    pub fn define_intrinsic_function(
        &mut self,
        binder: Identifier,
        type_: Expression,
        attribute: Span,
    ) -> Result<EntityKind> {
        let Ok(intrinsic) = binder.as_str().parse() else {
            return Err(unrecognized_intrinsic_binding_error(binder.as_str(), intrinsic::Kind::Function)
                .primary_span(&binder)
                .secondary_span(attribute)
                .report(self.reporter()));
        };

        // @Task explain why we remove here
        // @Task explain why unwrap
        let function = self.intrinsic_functions.remove(&intrinsic).unwrap();

        Ok(EntityKind::IntrinsicFunction {
            type_,
            arity: function.arity,
            function: function.function,
        })
    }

    pub fn is_intrinsic_type(&self, intrinsic: intrinsic::Type, binder: &hir::Identifier) -> bool {
        self.intrinsic_type(intrinsic)
            .map_or(false, |intrinsic| intrinsic == binder)
    }

    pub fn look_up_intrinsic_type(
        &self,
        intrinsic: intrinsic::Type,
        expression: Option<Span>,
    ) -> Result<Expression> {
        if let Some(intrinsic) = self.intrinsic_type(intrinsic) {
            return Ok(intrinsic.clone().into_expression());
        }

        Err(
            missing_intrinsic_binding_error(intrinsic, intrinsic::Kind::Type)
                .with(|error| match expression {
                    Some(expression) => {
                        error.labeled_primary_span(expression, "the type of this expression")
                    }
                    None => error,
                })
                .report(self.reporter()),
        )
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
        use intrinsic::{NumericType::*, Type::*};
        use known::Binding::*;

        let known = |binding: &hir::Binding, known: known::Binding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.0 == known)
        };
        let intrinsic = |binding: &hir::Binding, intrinsic: intrinsic::Type| {
            session
                .intrinsic_type(intrinsic)
                .map_or(false, |intrinsic| &binding.0 == intrinsic)
        };

        Some(match &expression.bare {
            // @Note this lookup looks incredibly inefficient
            hir::BareExpression::Binding(binding) => condition! {
                known(binding, Unit) => Self::Unit,
                known(binding, Bool) => Self::Bool,
                intrinsic(binding, Nat.into()) => Self::Nat,
                intrinsic(binding, Nat32.into()) => Self::Nat32,
                intrinsic(binding, Nat64.into()) => Self::Nat64,
                intrinsic(binding, Int.into()) => Self::Int,
                intrinsic(binding, Int32.into()) => Self::Int32,
                intrinsic(binding, Int64.into()) => Self::Int64,
                intrinsic(binding, Text) => Self::Text,
                else => return None,
            },
            hir::BareExpression::Application(application) => match &application.callee.bare {
                hir::BareExpression::Binding(binding) if known(binding, Option) => Self::Option(
                    Box::new(Self::from_expression(&application.argument, session)?),
                ),
                _ => return None,
            },
            _ => return None,
        })
    }

    fn into_expression(self, session: &BuildSession) -> Result<Expression> {
        use intrinsic::{NumericType::*, Type::*};
        use known::Binding::*;

        let intrinsic = |binding| session.look_up_intrinsic_type(binding, None);
        let known = |binding| session.look_up_known_binding(binding);
        match self {
            Self::Unit => known(Unit),
            Self::Bool => known(Bool),
            Self::Nat => intrinsic(Nat.into()),
            Self::Nat32 => intrinsic(Nat32.into()),
            Self::Nat64 => intrinsic(Nat64.into()),
            Self::Int => intrinsic(Int.into()),
            Self::Int32 => intrinsic(Int32.into()),
            Self::Int64 => intrinsic(Int64.into()),
            Self::Text => intrinsic(Text),
            Self::Option(type_) => Ok(application(known(Option)?, type_.into_expression(session)?)),
        }
    }
}

impl InterfaceableBindingExt for interfaceable::Value {
    fn from_expression(expression: &Expression, session: &BuildSession) -> Option<Self> {
        use hir::BareExpression::*;
        use known::Binding::*;

        let known = |binding: &hir::Binding, known: known::Binding| {
            session
                .known_binding(known)
                .map_or(false, |known| &binding.0 == known)
        };

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
                known(binding, UnitUnit) => Self::Unit,
                known(binding, BoolFalse) => Self::Bool(false),
                known(binding, BoolTrue) => Self::Bool(true),
                else => return None,
            },

            Application(application0) => match &application0.callee.bare {
                Binding(binding) if known(binding, OptionNone) => Self::Option {
                    value: None,
                    type_: interfaceable::Type::from_expression(&application0.argument, session)?,
                },
                Application(application1) => match &application1.callee.bare {
                    Binding(binding) if known(binding, OptionSome) => Self::Option {
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
        use hir::{Number::*, Text::*};
        use known::Binding::*;

        Ok(match self {
            Self::Unit => session.look_up_known_binding(Unit)?,
            Self::Bool(value) => {
                session.look_up_known_binding(if value { BoolTrue } else { BoolFalse })?
            }
            Self::Text(value) => Expression::new(default(), default(), Text(value).into()),
            Self::Nat(value) => Expression::new(default(), default(), Nat(value).into()),
            Self::Nat32(value) => Expression::new(default(), default(), Nat32(value).into()),
            Self::Nat64(value) => Expression::new(default(), default(), Nat64(value).into()),
            Self::Int(value) => Expression::new(default(), default(), Int(value).into()),
            Self::Int32(value) => Expression::new(default(), default(), Int32(value).into()),
            Self::Int64(value) => Expression::new(default(), default(), Int64(value).into()),
            Self::Option { type_, value } => match value {
                Some(value) => application(
                    application(
                        session.look_up_known_binding(OptionSome)?,
                        type_.into_expression(session)?,
                    ),
                    value.into_expression(session)?,
                ),
                None => application(
                    session.look_up_known_binding(OptionNone)?,
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

fn missing_known_binding_error(binding: known::Binding) -> Diagnostic {
    Diagnostic::error().code(ErrorCode::E062).message(format!(
        "the known binding ‘{}’ is not defined",
        binding.path()
    ))
}

fn missing_known_type_error(binding: known::Binding, expression: Span) -> Diagnostic {
    missing_known_binding_error(binding)
        .labeled_primary_span(expression, "the type of this expression")
}

fn unrecognized_intrinsic_binding_error(name: &str, kind: intrinsic::Kind) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E061)
        .message(format!("‘{name}’ is not an intrinsic {kind}"))
}

fn missing_intrinsic_binding_error(
    intrinsic: intrinsic::Type,
    kind: intrinsic::Kind,
) -> Diagnostic {
    Diagnostic::error()
        .code(ErrorCode::E060)
        .message(format!("the intrinsic {kind} ‘{intrinsic}’ is not defined"))
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

    pub fn is_goal(&self, session: &BuildSession) -> bool {
        self.index() == session.goal_component.index
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
