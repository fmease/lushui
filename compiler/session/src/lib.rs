#![feature(decl_macro, lazy_cell)]

use component::{Bindings, ComponentMetadata, DeclarationIndexExt};
use diagnostics::{error::Result, Diagnostic, Reporter};
use hir::{special, DeclarationIndex, LocalDeclarationIndex};
use index_map::IndexMap;
use lexer::word::Word;
use package::{ManifestPath, Package};
use span::{SourceMap, Span};
use std::{
    ops::{Index, IndexMut},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use unit::ComponentType;
use utility::{default, ComponentIndex, HashMap};

pub mod component;
pub mod interfaceable;
pub mod package;
pub mod unit;

pub const OUTPUT_FOLDER_NAME: &str = "build";

pub struct Session<'ctx> {
    // @Temporary pub
    pub component: ComponentIndex,
    // @Temporary pub
    pub context: &'ctx mut Context,
}

impl<'ctx> Session<'ctx> {
    #[cfg(feature = "test")]
    pub fn mock(context: &'ctx mut Context) -> Self {
        Self {
            component: ComponentIndex(0),
            context,
        }
    }

    pub fn new(component: ComponentIndex, context: &'ctx mut Context) -> Self {
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
        self.context.package_of(self.component)
    }

    pub fn in_root_package(&self, component: ComponentIndex) -> bool {
        self.package_of(component)
            .map_or(false, |package| self.root_package() == Some(package))
    }

    // FIXME: can this be simplified now?
    pub fn is_core_library(&self) -> bool {
        self.package_of(self.component).map_or(false, |package| {
            self[package].is_core()
                && self.context.components[self.component].type_ == ComponentType::Library
        })
    }

    // FIXME: can this be simplified now?
    // FIXME: bad name
    pub fn is_root_component(&self) -> bool {
        self.component == self.context.root_component.index
    }

    pub fn define(&mut self, entity: hir::Entity) -> LocalDeclarationIndex {
        self.context.bindings[self.component].insert(entity)
    }

    pub fn parent_of(&self, index: DeclarationIndex) -> Option<DeclarationIndex> {
        Some(DeclarationIndex::new(
            index.component(),
            self[index].parent?,
        ))
    }

    // FIXME: how is this used??? this is no longer correct in the local case (bindings is empty)
    pub fn component_of(&self, index: DeclarationIndex) -> &ComponentMetadata {
        if index.is_local(self.component) {
            &self.context.components[self.component]
        } else {
            &self[index.component()]
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
                    namespace: namespace.map(|namespace| {
                        self.context.bindings[self.component][namespace]
                            .source
                            .bare()
                    }),
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

    pub fn component(&self) -> &ComponentMetadata {
        &self.context.components[self.component]
    }

    pub fn specials(&self) -> &special::Bindings {
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

impl Index<ComponentIndex> for Session<'_> {
    type Output = ComponentMetadata;

    fn index(&self, index: ComponentIndex) -> &Self::Output {
        &self.context.components[index]
    }
}

impl Index<DeclarationIndex> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: DeclarationIndex) -> &Self::Output {
        // match index.local(self.component) {
        //     Some(index) => &self.bindings[index],
        //     None => &self.context[index],
        // }
        &self.context.bindings[index.component()][index.local_unchecked()]
    }
}

impl Index<LocalDeclarationIndex> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: LocalDeclarationIndex) -> &Self::Output {
        &self.context.bindings[self.component][index]
    }
}

impl IndexMut<LocalDeclarationIndex> for Session<'_> {
    fn index_mut(&mut self, index: LocalDeclarationIndex) -> &mut Self::Output {
        &mut self.context.bindings[self.component][index]
    }
}

// FIXME: Don't use any HashMaps here, they're unordered!
pub struct Context {
    components: IndexMap<ComponentIndex, ComponentMetadata>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    packages: HashMap<ManifestPath, Package>,
    /// The mapping from component to corresponding package.
    // @Task remove
    component_packages: HashMap<ComponentIndex, ManifestPath>,
    // @Task support multiple root components (depending on a user-supplied component filter)
    root_component: ComponentOutline,
    // @Temporary pub
    pub bindings: IndexMap<ComponentIndex, Bindings>,
    /// Intrinsic and known bindings.
    // @Temporary pub
    pub specials: special::Bindings,
    map: Arc<RwLock<SourceMap>>,
    reporter: Reporter,
}

impl Context {
    pub fn new(
        components: IndexMap<ComponentIndex, ComponentMetadata>,
        packages: HashMap<ManifestPath, Package>,
        component_packages: HashMap<ComponentIndex, ManifestPath>,
        root_component: ComponentOutline,
        map: &Arc<RwLock<SourceMap>>,
        reporter: Reporter,
    ) -> Self {
        let bindings = IndexMap::bare(vec![Bindings::default(); components.len()]);

        Self {
            components,
            packages,
            component_packages,
            bindings,
            root_component,
            specials: default(),
            map: map.clone(),
            reporter,
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use index_map::Index;
        use utility::paint::ColorChoice;

        let map: Arc<RwLock<SourceMap>> = default();

        // FIXME: empty units/components is incorrect technically speaking
        Self {
            components: default(),
            packages: default(),
            component_packages: default(),
            root_component: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                index: ComponentIndex::new(0),
            },
            bindings: default(),
            specials: default(),
            map: map.clone(),
            reporter: Reporter::stderr(ColorChoice::Auto).with_map(map),
        }
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

    pub fn components(&self) -> &IndexMap<ComponentIndex, ComponentMetadata> {
        &self.components
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
        &self.bindings[index.component()][index.local_unchecked()]
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
