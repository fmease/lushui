#![feature(decl_macro)]

use component::{Comp, DeclIdxExt};
use diagnostics::{error::Result, Diag, Reporter};
use hir::{
    special::{self, Bindings},
    DeclIdx, LocalDeclIdx,
};
use lexer::word::Word;
use package::{ManifestPath, Package};
use span::{SourceMap, Span};
use std::{
    ops::{Index, IndexMut},
    sync::{Arc, RwLock, RwLockReadGuard, RwLockWriteGuard},
};
use utility::{default, CompIdx, HashMap};

pub mod component;
pub mod interfaceable;
pub mod package;
pub mod unit;

pub const OUTPUT_FOLDER_NAME: &str = "build";

pub struct Session<'cx> {
    comp: Comp,
    cx: &'cx mut Context,
}

impl<'cx> Session<'cx> {
    pub fn new(comp: Comp, cx: &'cx mut Context) -> Self {
        Self { comp, cx }
    }

    pub fn root_pkg(&self) -> Option<ManifestPath> {
        self.cx.root_pkg()
    }

    pub fn root_comp(&self) -> &ComponentOutline {
        self.cx.root_comp()
    }

    pub fn pkg_of(&self, comp: CompIdx) -> Option<ManifestPath> {
        self.cx.package_of(comp)
    }

    pub fn pkg(&self) -> Option<ManifestPath> {
        self.cx.package_of(self.comp.idx())
    }

    pub fn in_root_pkg(&self, comp: CompIdx) -> bool {
        self.pkg_of(comp)
            .map_or(false, |pkg| self.root_pkg() == Some(pkg))
    }

    pub fn define(&mut self, entity: hir::Entity) -> LocalDeclIdx {
        self.comp.bindings.insert(entity)
    }

    pub fn parent_of(&self, index: DeclIdx) -> Option<DeclIdx> {
        Some(DeclIdx::new(index.comp(), self[index].parent?))
    }

    // @Task make this an Index::index fn again
    pub fn look_up_comp(&self, index: CompIdx) -> &Comp {
        &self.cx.comps[&index]
    }

    pub fn comp_of(&self, index: DeclIdx) -> &Comp {
        if index.is_local(&self.comp) {
            &self.comp
        } else {
            self.look_up_comp(index.comp())
        }
    }

    pub fn define_special(
        &mut self,
        kind: special::Kind,
        binder: hir::Ident,
        style: special::DefinitionStyle<'_, LocalDeclIdx>,
        attr: Span,
    ) -> Result<special::Binding, Diag> {
        use special::DefinitionStyle::*;

        self.cx.specials.define(
            kind,
            binder,
            match style {
                Implicit { namespace } => Implicit {
                    namespace: namespace.map(|namespace| self.comp[namespace].src.bare()),
                },
                Explicit { name } => Explicit { name },
            },
            attr,
        )
    }

    pub fn require_special(
        &self,
        special: impl Into<special::Binding>,
        user: Option<Span>,
    ) -> Result<hir::Ident> {
        let special = special.into();

        self.cx
            .specials
            .get(special)
            .ok_or_else(|| error::missing_binding(special, user).report(self.rep()))
    }

    pub fn comp(&self) -> &Comp {
        &self.comp
    }

    pub fn specials(&self) -> &Bindings {
        &self.cx.specials
    }

    pub fn cx(&self) -> &Context {
        self.cx
    }

    pub fn shared_map(&self) -> RwLockReadGuard<'_, SourceMap> {
        self.cx.map.read().unwrap()
    }

    pub fn map(&self) -> RwLockWriteGuard<'_, SourceMap> {
        self.cx.map.write().unwrap()
    }

    pub fn rep(&self) -> &Reporter {
        &self.cx.rep
    }
}

impl Index<ManifestPath> for Session<'_> {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.cx[path]
    }
}

impl Index<DeclIdx> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: DeclIdx) -> &Self::Output {
        match index.local(&self.comp) {
            Some(index) => &self.comp[index],
            None => &self.cx[index],
        }
    }
}

impl Index<LocalDeclIdx> for Session<'_> {
    type Output = hir::Entity;

    fn index(&self, index: LocalDeclIdx) -> &Self::Output {
        &self.comp[index]
    }
}

impl IndexMut<LocalDeclIdx> for Session<'_> {
    fn index_mut(&mut self, index: LocalDeclIdx) -> &mut Self::Output {
        &mut self.comp[index]
    }
}

pub struct Context {
    /// The components which have already been built in this session.
    comps: HashMap<CompIdx, Comp>,
    /// The packages whose components have not necessarily been built yet in this session but are about to.
    pkgs: HashMap<ManifestPath, Package>,
    /// The mapping from component to corresponding package.
    // @Task remove
    comp_pkgs: HashMap<CompIdx, ManifestPath>,
    // @Task support multiple root components (depending on a user-supplied component filter)
    root_comp: ComponentOutline,
    /// Intrinsic and known bindings.
    // @Temporary pub
    pub specials: Bindings,
    map: Arc<RwLock<SourceMap>>,
    rep: Reporter,
}

impl Context {
    pub fn new(
        pkgs: HashMap<ManifestPath, Package>,
        comp_pkgs: HashMap<CompIdx, ManifestPath>,
        root_comp: ComponentOutline,
        map: &Arc<RwLock<SourceMap>>,
        rep: Reporter,
    ) -> Self {
        Self {
            comps: default(),
            pkgs,
            comp_pkgs,
            root_comp,
            specials: default(),
            map: map.clone(),
            rep,
        }
    }

    #[cfg(feature = "test")]
    pub fn mock() -> Self {
        use index_map::Index;
        use utility::paint::ColorChoice;

        let map: Arc<RwLock<SourceMap>> = default();

        Self {
            comps: default(),
            pkgs: default(),
            comp_pkgs: default(),
            root_comp: ComponentOutline {
                name: Word::new_unchecked("test".into()),
                idx: CompIdx::new(0),
            },
            specials: default(),
            map: map.clone(),
            rep: Reporter::stderr(ColorChoice::Auto).with_map(map),
        }
    }

    // @Task find a better name!
    pub fn at<T>(
        &mut self,
        mut unit: unit::BuildUnit,
        handler: impl FnOnce(unit::BuildUnit, &mut Session<'_>) -> T,
    ) -> T {
        let comp = Comp::new(
            unit.name,
            unit.index,
            None,
            std::mem::take(&mut unit.dependencies),
        );
        let mut sess = Session::new(comp, self);

        let res = handler(unit, &mut sess);

        sess.cx.comps.insert(sess.comp.idx(), sess.comp);

        res
    }

    pub fn root_pkg(&self) -> Option<ManifestPath> {
        self.package_of(self.root_comp.idx)
    }

    pub fn root_comp(&self) -> &ComponentOutline {
        &self.root_comp
    }

    pub fn package_of(&self, comp: CompIdx) -> Option<ManifestPath> {
        self.comp_pkgs.get(&comp).copied()
    }

    pub fn rep(&self) -> &Reporter {
        &self.rep
    }
}

impl Index<ManifestPath> for Context {
    type Output = Package;

    fn index(&self, path: ManifestPath) -> &Self::Output {
        &self.pkgs[&path]
    }
}

impl Index<DeclIdx> for Context {
    type Output = hir::Entity;

    fn index(&self, idx: DeclIdx) -> &Self::Output {
        &self.comps[&idx.comp()][idx.local_unchecked()]
    }
}

// @Beacon @Task remove this type. it is only used by the documenter and that only because
// Component cannot really be used for some reason. investigate
#[derive(Clone)]
pub struct ComponentOutline {
    pub name: Word,
    pub idx: CompIdx,
}

mod error {
    #[allow(clippy::wildcard_imports)] // private inline module
    use super::*;
    use diagnostics::ErrorCode;

    pub(super) fn missing_binding(special: special::Binding, user: Option<Span>) -> Diag {
        let kind = special.kind();

        Diag::error()
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
