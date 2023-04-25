use crate::{
    Application, Item, LocalBinder, NumberLiteral, Path, RecordLiteral, SequenceLiteral,
    TextLiteral, Wildcard,
};

pub type Pattern = Item<BarePattern>;

/// A pattern without an enclosing [`span::Span`].
#[derive(Clone, PartialEq, Eq)]
pub enum BarePattern {
    Wildcard(Box<Wildcard>),
    NumberLiteral(Box<NumberLiteral>),
    TextLiteral(Box<TextLiteral>),
    LetBinding(LocalBinder),
    Path(Box<Path>),
    Application(Box<Application<Pattern>>),
    SequenceLiteral(Box<SequenceLiteral<Pattern>>),
    RecordLiteral(Box<RecordLiteral<Pattern>>),
}

impl From<Wildcard> for BarePattern {
    fn from(wildcard: Wildcard) -> Self {
        Self::Wildcard(Box::new(wildcard))
    }
}

impl From<NumberLiteral> for BarePattern {
    fn from(number: NumberLiteral) -> Self {
        Self::NumberLiteral(Box::new(number))
    }
}

impl From<TextLiteral> for BarePattern {
    fn from(text: TextLiteral) -> Self {
        Self::TextLiteral(Box::new(text))
    }
}

impl From<LocalBinder> for BarePattern {
    fn from(binder: LocalBinder) -> Self {
        Self::LetBinding(binder)
    }
}

impl From<Path> for BarePattern {
    fn from(path: Path) -> Self {
        Self::Path(Box::new(path))
    }
}

impl From<Application<Pattern>> for BarePattern {
    fn from(application: Application<Pattern>) -> Self {
        Self::Application(Box::new(application))
    }
}

impl From<SequenceLiteral<Pattern>> for BarePattern {
    fn from(sequence: SequenceLiteral<Pattern>) -> Self {
        Self::SequenceLiteral(Box::new(sequence))
    }
}

impl From<RecordLiteral<Pattern>> for BarePattern {
    fn from(record: RecordLiteral<Pattern>) -> Self {
        Self::RecordLiteral(Box::new(record))
    }
}
