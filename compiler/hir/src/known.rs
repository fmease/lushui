/// A known binding.
#[derive(PartialEq, Eq, Hash, Clone, Copy)]
pub enum Binding {
    /// The type `Unit`.
    Unit,
    /// The value `Unit.unit`.
    UnitUnit,
    /// The type `Bool`.
    Bool,
    /// The value `Bool.false`.
    BoolFalse,
    /// The value `Bool.true`.
    BoolTrue,
    /// The type `Option`.
    Option,
    /// The value `Option.none`.
    OptionNone,
    /// The value `Option.some`.
    OptionSome,
}

impl Binding {
    #[must_use]
    pub fn parse(namespace: Option<&str>, binder: &str) -> Option<Self> {
        Some(match (namespace, binder) {
            (None, "Unit") => Self::Unit,
            (Some("Unit"), "unit") => Self::UnitUnit,
            (None, "Bool") => Self::Bool,
            (Some("Bool"), "false") => Self::BoolFalse,
            (Some("Bool"), "true") => Self::BoolTrue,
            (None, "Option") => Self::Option,
            (Some("Option"), "none") => Self::OptionNone,
            (Some("Option"), "some") => Self::OptionSome,
            _ => return None,
        })
    }

    pub const fn path(self) -> &'static str {
        match self {
            Self::Unit => "Unit",
            Self::UnitUnit => "Unit.unit",
            Self::Bool => "Bool",
            Self::BoolFalse => "Bool.false",
            Self::BoolTrue => "Bool.true",
            Self::Option => "Option",
            Self::OptionNone => "Option.none",
            Self::OptionSome => "Option.some",
        }
    }
}
