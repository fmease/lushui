#![feature(decl_macro, default_free_fn, generic_associated_types, let_else, type_alias_impl_trait)]

use index_map::IndexMap;
use lushui_component_index::ComponentIndex;
use lushui_diagnostics::{Diagnostic, ErrorCode, Reporter};
use lushui_entity::{Entity, EntityKind};
use lushui_error::Result;
use lushui_hir::{self as hir, intrinsic, known, DeclarationIndex, Expression, Identifier};
use lushui_span::{SourceMap, Span};
use lushui_utilities::{HashMap, formatted};
use std::{
    default::default,
    ops::Index,
    path::PathBuf,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use lushui_hir::{LocalDeclarationIndex};
use colored::Colorize;
use derivation::{Elements, FromStr, Str};
use lushui_span::Spanned;
use lushui_token::Word;
use std::fmt;

const BUILD_FOLDER_NAME: &str = "build";

pub struct BuildSession {
    /// The components which have already been built in this session.
    components: HashMap<ComponentIndex, Component>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: IndexMap<PackageIndex, Package>,
    /// The corresponding package of the components.
    component_packages: HashMap<ComponentIndex, PackageIndex>,
    goal_component: ComponentIndex,
    // @Task Identifier -> DeclarationIndex
    known_bindings: HashMap<known::Binding, Identifier>,
    // @Task make this a DoubleHashMap
    intrinsic_types: HashMap<intrinsic::Type, Identifier>,
    intrinsic_functions: HashMap<intrinsic::Function, intrinsic::FunctionValue>,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl BuildSession {
    /// Create a new build session with all intrinsic functions defined.
    pub fn new(
        packages: IndexMap<PackageIndex, Package>,
        component_packages: HashMap<ComponentIndex, PackageIndex>,
        goal_component: ComponentIndex,
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
            goal_component: ComponentIndex::new(0),
            known_bindings: default(),
            intrinsic_types: default(),
            intrinsic_functions: default(),
            map: map.clone(),
            reporter: Reporter::stderr().with_map(map),
        }
    }

    pub fn goal_package(&self) -> Option<PackageIndex> {
        self.package_of(self.goal_component)
    }

    pub fn goal_component(&self) -> ComponentIndex {
        self.goal_component
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<PackageIndex> {
        self.component_packages.get(&component).copied()
    }

    pub fn in_goal_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component)
            .map_or(false, |package| self.goal_package() == Some(package))
    }

    /// The path to the folder containing the build artifacts.
    pub fn build_folder(&self) -> PathBuf {
        // match self.goal_package() {
        //     Some(package) => self[package].path.join(BUILD_FOLDER_NAME),
        //     // @Question how should the folder be called?
        //     None => todo!(),
        // }
        todo!() // @Beacon @Beacon @Beacon @Task
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

    // @Beacon @Beacon @Beacon @Temporary this is an ugly hack
    pub fn with<'a>(&'a self, component: &'a Component) -> DisplayContext<'a> {
        DisplayContext(component, self)
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
        // &self.packages[index]
        todo!() // @Beacon @Beacon @Beacon @Task
    }
}

// @Beacon @Beacon @Beacon @Temporary location, concrete impl etc
// @Note this is just uglyy!!! working around orphan rules etc!!
#[derive(Clone, Copy)]
pub struct DisplayContext<'a>(pub &'a Component, pub &'a BuildSession);

impl hir::DisplayContext for DisplayContext<'_> {
    fn path_to_string(self, binding: &hir::Identifier) -> String {
        todo!() // @Beacon @Beacon @Beacon @Task
    }
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
    // @Beacon @Question should this be put on `Component` instead???
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
        // use crate::{entity::EntityKind, resolver::Exposure};

        // let name = Word::parse("test".into()).ok().unwrap();

        // let mut component = Self::new(
        //     name.clone(),
        //     ComponentIndex(0),
        //     Spanned::new(default(), PathBuf::new()),
        //     None,
        //     ComponentType::Library,
        //     HashMap::default(),
        // );
        // component.bindings.insert(Entity {
        //     source: Spanned::new(default(), name).into(),
        //     parent: None,
        //     exposure: Exposure::Unrestricted,
        //     kind: EntityKind::module(),
        //     attributes: default(),
        // });
        // component

        todo!() // @Beacon @Beacon @Beacon @Task
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
        // session.package_of(self.index).map_or(false, |package| {
        //     session[package].is_core() && self.is_library()
        // })
        todo!() // @Beacon @Beacon @Beacon @Task
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

impl<C: hir::DisplayContext> hir::Display<C> for Component {
    type Output<'a> = impl fmt::Display where C: 'a;

    fn display(&self, context: C) -> Self::Output<'_> {
        formatted(move |f| {
            writeln!(f, "{} {} ({:?})", self.type_(), self.name(), self.index())?;

            writeln!(f, "  bindings:")?;

            for (index, entity) in &self.bindings {
                writeln!(
                    f,
                    "    {}: {}",
                    format!("{index:?}").red(),
                    entity.display(context)
                )?;
            }

            Ok(())
        })
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
    pub type_: ComponentType,
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
    pub components: HashMap<ComponentKey, PossiblyUnresolvedComponent>,
}

pub type ComponentKey = (Word, ComponentType);

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
