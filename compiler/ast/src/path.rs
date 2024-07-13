use crate::Ident;
use span::{Span, Spanned, Spanning};
use std::fmt;
use utility::{smallvec, SmallVec};

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct Path {
    pub hanger: Option<Hanger>,
    /// Has to be non-empty if the `hanger` is `None`.
    // @Task make it impossible to construct with an empty vector
    pub segments: SmallVec<Ident, 1>,
}

impl Path {
    pub fn hung(hanger: Hanger, segments: SmallVec<Ident, 1>) -> Self {
        debug_assert_ne!(segments.len(), 0);

        Self {
            hanger: Some(hanger),
            segments,
        }
    }

    pub fn unhung(segments: SmallVec<Ident, 1>) -> Self {
        debug_assert_ne!(segments.len(), 0);

        Self {
            hanger: None,
            segments,
        }
    }

    pub fn join(mut self, other: Self) -> Result<Self, Hanger> {
        if let Some(hanger) = other.hanger
            && !matches!(hanger.bare, BareHanger::Self_)
        {
            return Err(hanger);
        }
        self.segments.extend(other.segments);
        Ok(self)
    }

    pub fn is_bare_hanger(&self, hanger: BareHanger) -> bool {
        self.hanger
            .map_or(false, |some_hanger| some_hanger.bare == hanger)
            && self.segments.is_empty()
    }

    /// The path head if it is an identifier.
    pub fn ident_head(&self) -> Option<Ident> {
        if self.hanger.is_some() {
            return None;
        }

        Some(self.segments[0])
    }
}

// @Question bad idea?
impl fmt::Debug for Path {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self, f)
    }
}

impl From<Ident> for Path {
    fn from(ident: Ident) -> Self {
        Self {
            hanger: None,
            segments: smallvec![ident],
        }
    }
}

impl From<Hanger> for Path {
    fn from(hanger: Hanger) -> Self {
        Self {
            hanger: Some(hanger),
            segments: SmallVec::new(),
        }
    }
}

impl Spanning for Path {
    fn span(&self) -> Span {
        // @Task improve
        if let Some(head) = &self.hanger {
            head.span().merge(&self.segments.last())
        } else {
            self.segments
                .first()
                .unwrap()
                .span()
                .merge(self.segments.last().unwrap())
        }
    }
}

pub type Hanger = Spanned<BareHanger>;

/// The non-identifier head of a path.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum BareHanger {
    Extern,
    Topmost,
    Super,
    Self_,
}

impl BareHanger {
    pub const fn name(self) -> &'static str {
        match self {
            Self::Extern => "extern",
            Self::Topmost => "topmost",
            Self::Super => "super",
            Self::Self_ => "self",
        }
    }
}
