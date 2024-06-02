use crate::{unit::ComponentType, ComponentOutline, Session};
use hir::{DeclarationIndex, Entity, LocalDeclarationIndex};
use index_map::IndexMap;
use lexer::word::Word;
use span::Spanned;
use utility::{path::CanonicalPathBuf, ComponentIndex, HashMap};

/// All bindings inside of the component.
///
/// The first item has to be the root module.
pub type Bindings = IndexMap<LocalDeclarationIndex, Entity>;

/// A sealed container of modules regarded as one unit.
// FIXME: make most fields private again + use accessors
pub struct ComponentMetadata {
    pub name: Word,
    pub index: ComponentIndex,
    // FIXME: path to what??
    pub path: Spanned<CanonicalPathBuf>,
    pub type_: ComponentType,
    dependencies: HashMap<Word, ComponentIndex>,
}

// @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task improve naming scheme around "root" (root component vs component root vs …)

impl ComponentMetadata {
    /// The root module / the component root as a local index.
    pub const ROOT: LocalDeclarationIndex = LocalDeclarationIndex::ROOT;

    pub fn new(
        name: Word,
        index: ComponentIndex,
        path: Spanned<CanonicalPathBuf>,
        type_: ComponentType,
        dependencies: HashMap<Word, ComponentIndex>,
    ) -> Self {
        Self {
            name,
            index,
            path,
            type_,
            dependencies,
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        // use hir::Exposure;
        use std::path::PathBuf;

        const NAME: &str = "test";
        let name = Word::new_unchecked(NAME.into());
        // FIXME: this gross!
        let path = Spanned::bare(CanonicalPathBuf::new_unchecked(PathBuf::new()));

        let component = Self::new(
            name,
            ComponentIndex(0),
            path,
            ComponentType::Library,
            HashMap::default(),
        );
        // FIXME:
        // component.bindings.insert(Entity {
        //     source: Spanned::bare(name).into(),
        //     parent: None,
        //     exposure: Exposure::Unrestricted,
        //     kind: hir::EntityKind::module(),
        //     attributes: default(),
        // });

        component
    }

    pub fn name(&self) -> Word {
        self.name
    }

    pub fn index(&self) -> ComponentIndex {
        self.index
    }

    /// The root module / the component root.
    pub fn root(&self) -> DeclarationIndex {
        Self::ROOT.global(self)
    }

    pub fn dependencies(&self) -> &HashMap<Word, ComponentIndex> {
        &self.dependencies
    }

    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name,
            index: self.index,
        }
    }
}

pub trait IdentifierExt<C> {
    fn local_declaration_index(&self, component: &C) -> Option<LocalDeclarationIndex>;
}

impl IdentifierExt<ComponentMetadata> for hir::Identifier {
    fn local_declaration_index(
        &self,
        component: &ComponentMetadata,
    ) -> Option<LocalDeclarationIndex> {
        self.declaration_index()?.local(component)
    }
}

impl IdentifierExt<Session<'_>> for hir::Identifier {
    fn local_declaration_index(&self, session: &Session<'_>) -> Option<LocalDeclarationIndex> {
        self.local_declaration_index(session.component())
    }
}

pub trait DeclarationIndexExt<C> {
    #[allow(clippy::wrong_self_convention)] // false positive IMO, @Task report
    fn is_local(self, context: C) -> bool;

    fn local(self, context: C) -> Option<LocalDeclarationIndex>;
}

impl DeclarationIndexExt<ComponentIndex> for DeclarationIndex {
    fn is_local(self, component: ComponentIndex) -> bool {
        self.component() == component
    }

    fn local(self, component: ComponentIndex) -> Option<LocalDeclarationIndex> {
        self.is_local(component).then(|| self.local_unchecked())
    }
}

impl DeclarationIndexExt<&ComponentMetadata> for DeclarationIndex {
    fn is_local(self, component: &ComponentMetadata) -> bool {
        self.is_local(component.index)
    }

    fn local(self, component: &ComponentMetadata) -> Option<LocalDeclarationIndex> {
        self.local(component.index)
    }
}

impl DeclarationIndexExt<&Session<'_>> for DeclarationIndex {
    fn is_local(self, session: &Session<'_>) -> bool {
        self.is_local(session.component())
    }

    fn local(self, session: &Session<'_>) -> Option<LocalDeclarationIndex> {
        self.local(session.component())
    }
}

pub trait LocalDeclarationIndexExt<C> {
    fn global(self, context: C) -> DeclarationIndex;
}

impl LocalDeclarationIndexExt<ComponentIndex> for LocalDeclarationIndex {
    fn global(self, component: ComponentIndex) -> DeclarationIndex {
        DeclarationIndex::new(component, self)
    }
}

impl LocalDeclarationIndexExt<&ComponentMetadata> for LocalDeclarationIndex {
    fn global(self, component: &ComponentMetadata) -> DeclarationIndex {
        self.global(component.index())
    }
}

impl LocalDeclarationIndexExt<&Session<'_>> for LocalDeclarationIndex {
    fn global(self, session: &Session<'_>) -> DeclarationIndex {
        self.global(session.component())
    }
}
