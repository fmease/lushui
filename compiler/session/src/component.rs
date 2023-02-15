use crate::{ComponentOutline, Session};
use hir::{DeclarationIndex, Entity, LocalDeclarationIndex};
use index_map::IndexMap;
use std::{default::default, sync::Arc};
use token::Word;
use utilities::{ComponentIndex, HashMap};

/// A sealed container of modules regarded as one unit.
pub struct Component {
    name: Word,
    index: ComponentIndex,
    // @Task document this! @Note this is used by the lang-server which gets the document content by the client
    //       and which should not open the file at the given path to avoid TOC-TOU bugs / data races
    pub content: Option<Arc<String>>,
    dependencies: HashMap<Word, ComponentIndex>,
    /// All bindings inside of the component.
    ///
    /// The first item has to be the root module.
    pub bindings: IndexMap<LocalDeclarationIndex, Entity>,
}

// @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task improve naming scheme around "root" (root component vs component root vs …)

impl Component {
    pub fn new(
        name: Word,
        index: ComponentIndex,
        content: Option<Arc<String>>,
        dependencies: HashMap<Word, ComponentIndex>,
    ) -> Self {
        Self {
            name,
            index,
            content,
            dependencies,
            bindings: default(),
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use hir::Exposure;
        use span::Spanned;

        const NAME: &str = "test";
        let name = Word::new_unchecked(NAME.into());

        let mut component = Self::new(name.clone(), ComponentIndex(0), None, HashMap::default());
        component.bindings.insert(Entity {
            source: Spanned::bare(name).into(),
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

    /// The root module / the component root.
    pub fn root(&self) -> DeclarationIndex {
        self.root_local().global(self)
    }

    /// The root module / the component root as a local index.
    #[allow(clippy::unused_self)] // leads to a more legible API
    pub fn root_local(&self) -> LocalDeclarationIndex {
        LocalDeclarationIndex::new(0)
    }

    pub fn dependencies(&self) -> &HashMap<Word, ComponentIndex> {
        &self.dependencies
    }

    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name.clone(),
            index: self.index,
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

pub trait IdentifierExt<C> {
    fn local_declaration_index(&self, component: &C) -> Option<LocalDeclarationIndex>;
}

impl IdentifierExt<Component> for hir::Identifier {
    fn local_declaration_index(&self, component: &Component) -> Option<LocalDeclarationIndex> {
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
    fn is_local(self, context: &C) -> bool;

    fn local(self, context: &C) -> Option<LocalDeclarationIndex>;
}

impl DeclarationIndexExt<Component> for DeclarationIndex {
    fn is_local(self, component: &Component) -> bool {
        self.component() == component.index()
    }

    fn local(self, component: &Component) -> Option<LocalDeclarationIndex> {
        self.is_local(component).then(|| self.local_unchecked())
    }
}

impl DeclarationIndexExt<Session<'_>> for DeclarationIndex {
    fn is_local(self, session: &Session<'_>) -> bool {
        self.is_local(session.component())
    }

    fn local(self, session: &Session<'_>) -> Option<LocalDeclarationIndex> {
        self.local(session.component())
    }
}

pub trait LocalDeclarationIndexExt<C> {
    fn global(self, context: &C) -> DeclarationIndex;
}

impl LocalDeclarationIndexExt<Component> for LocalDeclarationIndex {
    fn global(self, component: &Component) -> DeclarationIndex {
        DeclarationIndex::new(component.index(), self)
    }
}

impl LocalDeclarationIndexExt<Session<'_>> for LocalDeclarationIndex {
    fn global(self, session: &Session<'_>) -> DeclarationIndex {
        self.global(session.component())
    }
}
