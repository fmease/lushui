use crate::{ComponentOutline, Session};
use hir::{DeclIdx, Entity, LocalDeclIdx};
use index_map::IndexMap;
use lexer::word::Word;
use std::sync::Arc;
use utility::{default, CompIdx, HashMap};

/// A component: A sealed container of modules regarded as one unit.
pub struct Comp {
    name: Word,
    idx: CompIdx,
    // @Task document this! @Note this is used by the lang-server which gets the document content by the client
    //       and which should not open the file at the given path to avoid TOC-TOU bugs / data races
    pub content: Option<Arc<String>>,
    deps: HashMap<Word, CompIdx>,
    /// All bindings inside of the component.
    ///
    /// The first item has to be the root module.
    pub bindings: IndexMap<LocalDeclIdx, Entity>,
}

// @Beacon @Beacon @Beacon @Beacon @Beacon @Beacon @Task improve naming scheme around "root" (root component vs component root vs …)

impl Comp {
    pub fn new(
        name: Word,
        idx: CompIdx,
        content: Option<Arc<String>>,
        deps: HashMap<Word, CompIdx>,
    ) -> Self {
        Self {
            name,
            idx,
            content,
            deps,
            bindings: default(),
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use hir::Exposure;
        use span::Spanned;

        const NAME: &str = "test";
        let name = Word::new_unchecked(NAME.into());

        let mut comp = Self::new(name, CompIdx(0), None, HashMap::default());
        comp.bindings.insert(Entity {
            src: Spanned::bare(name).into(),
            parent: None,
            exp: Exposure::Unrestricted,
            kind: hir::EntityKind::module(),
            attrs: default(),
        });

        comp
    }

    pub fn name(&self) -> Word {
        self.name
    }

    pub fn idx(&self) -> CompIdx {
        self.idx
    }

    /// The root module / the component root.
    pub fn root(&self) -> DeclIdx {
        self.root_local().global(self)
    }

    /// The root module / the component root as a local index.
    #[allow(clippy::unused_self)] // leads to a more legible API
    pub fn root_local(&self) -> LocalDeclIdx {
        LocalDeclIdx::new(0)
    }

    pub fn deps(&self) -> &HashMap<Word, CompIdx> {
        &self.deps
    }

    pub fn outline(&self) -> ComponentOutline {
        ComponentOutline {
            name: self.name,
            idx: self.idx,
        }
    }
}

impl std::ops::Index<LocalDeclIdx> for Comp {
    type Output = Entity;

    #[track_caller]
    fn index(&self, idx: LocalDeclIdx) -> &Self::Output {
        &self.bindings[idx]
    }
}

impl std::ops::IndexMut<LocalDeclIdx> for Comp {
    #[track_caller]
    fn index_mut(&mut self, idx: LocalDeclIdx) -> &mut Self::Output {
        &mut self.bindings[idx]
    }
}

pub trait IdentExt<C> {
    fn local_decl_idx(&self, comp: &C) -> Option<LocalDeclIdx>;
}

impl IdentExt<Comp> for hir::Ident {
    fn local_decl_idx(&self, comp: &Comp) -> Option<LocalDeclIdx> {
        self.decl_idx()?.local(comp)
    }
}

impl IdentExt<Session<'_>> for hir::Ident {
    fn local_decl_idx(&self, sess: &Session<'_>) -> Option<LocalDeclIdx> {
        self.local_decl_idx(sess.comp())
    }
}

pub trait DeclIdxExt<Cx> {
    #[allow(clippy::wrong_self_convention)] // false positive IMO, @Task report
    fn is_local(self, cx: &Cx) -> bool;

    fn local(self, cx: &Cx) -> Option<LocalDeclIdx>;
}

impl DeclIdxExt<Comp> for DeclIdx {
    fn is_local(self, comp: &Comp) -> bool {
        self.comp() == comp.idx()
    }

    fn local(self, comp: &Comp) -> Option<LocalDeclIdx> {
        self.is_local(comp).then(|| self.local_unchecked())
    }
}

impl DeclIdxExt<Session<'_>> for DeclIdx {
    fn is_local(self, sess: &Session<'_>) -> bool {
        self.is_local(sess.comp())
    }

    fn local(self, sess: &Session<'_>) -> Option<LocalDeclIdx> {
        self.local(sess.comp())
    }
}

pub trait LocalDeclIdxExt<Cx> {
    fn global(self, cx: &Cx) -> DeclIdx;
}

impl LocalDeclIdxExt<Comp> for LocalDeclIdx {
    fn global(self, comp: &Comp) -> DeclIdx {
        DeclIdx::new(comp.idx(), self)
    }
}

impl LocalDeclIdxExt<Session<'_>> for LocalDeclIdx {
    fn global(self, sess: &Session<'_>) -> DeclIdx {
        self.global(sess.comp())
    }
}
