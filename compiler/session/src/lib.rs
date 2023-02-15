#![feature(decl_macro, default_free_fn, once_cell)]

use component::{Component, DeclarationIndexExt};
use diagnostics::{error::Result, Diagnostic, Reporter};
use hir::{
    special::{self, Bindings},
    DeclarationIndex, Expression, LocalDeclarationIndex,
};
use package::{ManifestPath, Package};
use span::{SourceMap, Span};
use std::{
    default::default,
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use token::Word;
use utilities::{ComponentIndex, HashMap};

pub mod component;
pub mod interfaceable;
pub mod package;
pub mod unit;

pub struct Session<'ctx> {
    component: Component,
    context: &'ctx mut Context,
}

// @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task
// improve naming scheme: either use `look_up{,_*}` everywhere or nowhere etc!
// @Task add docs

impl<'ctx> Session<'ctx> {
    // @Task to better location
    pub const OUTPUT_FOLDER_NAME: &'static str = "build";

    pub fn new(component: Component, context: &'ctx mut Context) -> Self {
        Self { component, context }
    }

    pub fn root_package(&self) -> Option<ManifestPath> {
        self.context.root_package()
    }

    pub fn root_component(&self) -> &ComponentOutline {
        self.context.root_component()
    }

    // @Task make this an Index::index fn again
    pub fn look_up_package(&self, path: ManifestPath) -> &Package {
        self.context.package(path)
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<ManifestPath> {
        self.context.package_of(component)
    }

    pub fn package(&self) -> Option<ManifestPath> {
        self.context.package_of(self.component.index())
    }

    pub fn in_root_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component)
            .map_or(false, |package| self.root_package() == Some(package))
    }

    pub fn define_unchecked(&mut self, entity: hir::Entity) -> LocalDeclarationIndex {
        self.component.bindings.insert(entity)
    }

    // @Task make this an Index::index fn again
    pub fn look_up(&self, index: DeclarationIndex) -> &hir::Entity {
        match index.local(&self.component) {
            Some(index) => &self.component[index],
            None => self.context.look_up(index),
        }
    }

    // @Task make this an Index::index fn again
    pub fn look_up_local(&self, index: LocalDeclarationIndex) -> &hir::Entity {
        &self.component[index]
    }

    // @Task make this an IndexMut::index_mut fn again
    pub fn look_up_local_mut(&mut self, index: LocalDeclarationIndex) -> &mut hir::Entity {
        &mut self.component[index]
    }

    pub fn parent_of(&self, index: DeclarationIndex) -> Option<DeclarationIndex> {
        Some(DeclarationIndex::new(
            index.component(),
            self.look_up(index).parent?,
        ))
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
                    namespace: namespace.map(|namespace| self.component[namespace].source.as_str()),
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
    ) -> Result<Expression> {
        self.context
            .specials
            .require(special, user)
            .map_err(|error| error.report(self.reporter()))
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
        use index_map::Index;

        let map: Arc<RwLock<SourceMap>> = default();

        Self {
            components: default(),
            packages: default(),
            component_packages: default(),
            root_component: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                index: ComponentIndex::new(0),
            },
            specials: default(),
            map: map.clone(),
            reporter: Reporter::stderr().with_map(map),
        }
    }

    // @Task find a better name!
    pub fn at<T>(
        &mut self,
        mut unit: unit::BuildUnit,
        handler: impl FnOnce(unit::BuildUnit, &mut Session<'_>) -> T,
    ) -> T {
        let component = Component::new(
            unit.name.clone(),
            unit.index,
            None,
            std::mem::take(&mut unit.dependencies),
        );
        let mut session = Session::new(component, self);

        let result = handler(unit, &mut session);

        session
            .context
            .components
            .insert(session.component.index(), session.component);

        result
    }

    pub fn root_package(&self) -> Option<ManifestPath> {
        self.package_of(self.root_component.index)
    }

    pub fn root_component(&self) -> &ComponentOutline {
        &self.root_component
    }

    pub fn package(&self, path: ManifestPath) -> &Package {
        &self.packages[&path]
    }

    pub fn package_of(&self, component: ComponentIndex) -> Option<ManifestPath> {
        self.component_packages.get(&component).copied()
    }

    pub fn look_up(&self, index: DeclarationIndex) -> &hir::Entity {
        &self.components[&index.component()][index.local_unchecked()]
    }

    pub fn reporter(&self) -> &Reporter {
        &self.reporter
    }
}

// @Beacon @Task remove this type. it is only used by the documenter and that only because
// Component cannot really be used for some reason. investigate
#[derive(Clone)]
pub struct ComponentOutline {
    pub name: Word,
    pub index: ComponentIndex,
}
