#![feature(decl_macro)]

use component::{Component, DeclarationIndexExt};
use diagnostics::{Diagnostic, Reporter, error::Result};
use hir::{
    DeclarationIndex, LocalDeclarationIndex,
    special::{self, Bindings},
};
use lexer::word::Word;
use package::{ManifestPath, Package};
use span::{SourceMap, Span};
use std::{
    ops::{Index, IndexMut},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use utility::{ComponentIndex, HashMap, default};

pub mod component;
pub mod interfaceable;
pub mod package;
pub mod unit;

pub const OUTPUT_FOLDER_NAME: &str = "build";

pub struct Session<'ctx> {
    component: Component,
    context: &'ctx mut Context,
}

impl<'ctx> Session<'ctx> {
    pub fn new(component: Component, context: &'ctx mut Context) -> Self {
        Self { component, context }
    }

    pub fn root_package(&self) -> Option<ManifestPath> {
        self.context.root_package()
    }

    pub fn root_component(&self) -> &ComponentOutline {
        self.context.root_component()
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<ManifestPath> {
        self.context.package_of(component)
    }

    pub fn package(&self) -> Option<ManifestPath> {
        self.context.package_of(self.component.index())
    }

    pub fn in_root_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component).is_some_and(|package| self.root_package() == Some(package))
    }

    pub fn define(&mut self, entity: hir::Entity) -> LocalDeclarationIndex {
        self.component.bindings.insert(entity)
    }

    pub fn parent_of(&self, index: DeclarationIndex) -> Option<DeclarationIndex> {
        Some(DeclarationIndex::new(index.component(), self[index].parent?))
    }

    // @Task make this an Index::index fn again
    pub fn look_up_component(&self, index: ComponentIndex) -> &Component {
        &self.context.components[&index]
    }

    pub fn component_of(&self, index: DeclarationIndex) -> &Component {
        if index.is_local(&self.component) {
            &self.component
        } else {
            self.look_up_component(index.component())
        }
    }

    pub fn define_special(
        &mut self,
        kind: special::Kind,
        binder: hir::Identifier,
        style: special::DefinitionStyle<'_, LocalDeclarationIndex>,
        attribute: Span,
    ) -> Result<special::Binding, Diagnostic> {
        use special::DefinitionStyle::*;

        self.context.specials.define(
            kind,
            binder,
            match style {
                Implicit { namespace } => Implicit {
                    namespace: namespace.map(|namespace| self.component[namespace].source.bare()),
                },
                Explicit { name } => Explicit { name },
            },
            attribute,
        )
    }

    pub fn require_special(
        &self,
        special: impl Into<special::Binding>,
        user: Option<Span>,
    ) -> Result<hir::Identifier> {
        let special = special.into();

        self.context
            .specials
            .get(special)
            .ok_or_else(|| error::missing_binding(special, user).report(self.reporter()))
    }

    pub fn component(&self) -> &Component {
        &self.component
    }

    pub fn specials(&self) -> &Bindings {
        &self.context.specials
    }

    pub fn context(&self) -> &Context {
        self.context
    }

    pub fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.context.map.read().unwrap()
    }

    pub fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.context.map.write().unwrap()
    }

    pub fn reporter(&self) -> &Reporter {
        &self.context.reporter
    }
}

impl Index<ManifestPath> for Session<'_> {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.context[path]
    }
}

impl Index<DeclarationIndex> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        match index.local(&self.component) {
            Some(index) => &self.component[index],
            None => &self.context[index],
        }
    }
}

impl Index<LocalDeclarationIndex> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.component[index]
    }
}

impl IndexMut<LocalDeclarationIndex> for Session<'_> {
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.component[index]
    }
}

pub struct Context {
    /// The components which have already been built in this session.
    components: HashMap<ComponentIndex, Component>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: HashMap<ManifestPath, Package>,
    /// The mapping from component to corresponding package.
    // @Task remove
    component_packages: HashMap<ComponentIndex, ManifestPath>,
    // @Task support multiple root components (depending on a user-supplied component filter)
    root_component: ComponentOutline,
    /// Intrinsic and known bindings.
    // @Temporary pub
    pub specials: Bindings,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl Context {
    pub fn new(
        packages: HashMap<ManifestPath, Package>,
        component_packages: HashMap<ComponentIndex, ManifestPath>,
        root_component: ComponentOutline,
        map: &Arc<RwLock<SourceMap>>,
        reporter: Reporter,
    ) -> Self {
        Self {
            components: default(),
            packages,
            component_packages,
            root_component,
            specials: default(),
            map: map.clone(),
            reporter,
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use utility::paint::ColorChoice;

        let map: Arc<RwLock<SourceMap>> = default();

        Self {
            components: default(),
            packages: default(),
            component_packages: default(),
            root_component: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                index: ComponentIndex::mock(),
            },
            specials: default(),
            map: map.clone(),
            reporter: Reporter::stderr(ColorChoice::Auto).with_map(map),
        }
    }

    // @Task find a better name!
    pub fn at<T>(
        &mut self,
        mut unit: unit::BuildUnit,
        handler: impl FnOnce(unit::BuildUnit, &mut Session<'_>) -> T,
    ) -> T {
        let component =
            Component::new(unit.name, unit.index, None, std::mem::take(&mut unit.dependencies));
        let mut session = Session::new(component, self);

        let result = handler(unit, &mut session);

        session.context.components.insert(session.component.index(), session.component);

        result
    }

    pub fn root_package(&self) -> Option<ManifestPath> {
        self.package_of(self.root_component.index)
    }

    pub fn root_component(&self) -> &ComponentOutline {
        &self.root_component
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<ManifestPath> {
        self.component_packages.get(&component).copied()
    }

    pub fn reporter(&self) -> &Reporter {
        &self.reporter
    }
}

impl Index<ManifestPath> for Context {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.packages[&path]
    }
}

impl Index<DeclarationIndex> for Context {
    type Output = hir::Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        &self.components[&index.component()][index.local_unchecked()]
    }
}

// @Beacon @Task remove this type. it is only used by the documenter and that only because
// Component cannot really be used for some reason. investigate
#[derive(Clone)]
pub struct ComponentOutline {
    pub name: Word,
    pub index: ComponentIndex,
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;
    use diagnostics::ErrorCode;

    pub(super) fn missing_binding(special: special::Binding, user: Option<Span>) -> Diagnostic {
        let kind = special.kind();

        Diagnostic::error()
            .code(match kind {
                special::Kind::Known => ErrorCode::E062,
                special::Kind::Intrinsic => ErrorCode::E060,
            })
            .message(format!("the {kind} binding ‘{special}’ is not defined"))
            .with(|it| match user {
                // @Task label
                Some(user) => it.span(user, "the type of this expression"),
                None => it,
            })
    }
}
