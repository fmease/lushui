use crate::{App, Item, LocalBinder, NumLit, Path, RecLit, SeqLit, TextLit, Wildcard};

/// A pattern.
pub type Pat = Item<BarePat>;

/// A location-less pattern.
#[derive(Clone, PartialEq, Eq)]
pub enum BarePat {
    Wildcard(Box<Wildcard>),
    NumLit(Box<NumLit>),
    TextLit(Box<TextLit>),
    LetBinding(LocalBinder),
    Path(Box<Path>),
    App(Box<App<Pat>>),
    SeqLit(Box<SeqLit<Pat>>),
    RecLit(Box<RecLit<Pat>>),
}

impl From<Wildcard> for BarePat {
    fn from(wildcard: Wildcard) -> Self {
        Self::Wildcard(Box::new(wildcard))
    }
}

impl From<NumLit> for BarePat {
    fn from(num: NumLit) -> Self {
        Self::NumLit(Box::new(num))
    }
}

impl From<TextLit> for BarePat {
    fn from(text: TextLit) -> Self {
        Self::TextLit(Box::new(text))
    }
}

impl From<LocalBinder> for BarePat {
    fn from(binder: LocalBinder) -> Self {
        Self::LetBinding(binder)
    }
}

impl From<Path> for BarePat {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<App<Pat>> for BarePat {
    fn from(app: App<Pat>) -> Self {
        Self::App(Box::new(app))
    }
}

impl From<SeqLit<Pat>> for BarePat {
    fn from(seq: SeqLit<Pat>) -> Self {
        Self::SeqLit(Box::new(seq))
    }
}

impl From<RecLit<Pat>> for BarePat {
    fn from(rec: RecLit<Pat>) -> Self {
        Self::RecLit(Box::new(rec))
    }
}
