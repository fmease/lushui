use derivation::Str;
use std::fmt;

use crate::utility::obtain;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Code {
    Error(ErrorCode),
    Lint(LintCode),
}

impl fmt::Display for Code {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Error(code) => write!(f, "{code}"),
            Self::Lint(code) => write!(f, "{code}"),
        }
    }
}

/// An error code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
#[forbid(missing_docs)]
pub enum ErrorCode {
    /// _Permanently unassigned_ (used in tests).
    E000,
    /// _Permanently unassigned_ (used in tests).
    E001,
    /// _Permanently unassigned_ (used in tests).
    E002,
    /// _Permanently unassigned_ (used in tests).
    E003,
    /// _Permanently unassigned_ (used in tests).
    E004,
    /// Unreadable number literal.
    E005,
    /// Duplicate or conflicting attributes.
    E006,
    /// Number literal does not fit type.
    E007,
    /// Number literal does not fit.
    E008,
    /// Re-export of private binding.
    E009,
    /// Unexpected token.
    E010,
    /// Undefined attribute.
    E011,
    /// Definitionless declaration.
    E012,
    /// Illegal attribute target.
    E013,
    /// Mutually exclusive attributes.
    E014,
    /// Missing mandatory type annotations.
    E015,
    /// Unable to load out-of-line module.
    E016,
    /// Attempt to access subbinding of non-namespace.
    E017,
    /// Undefined lint.
    E018,
    /// Attribute arguments arity mismatch.
    E019,
    /// Duplicate definitions.
    E020,
    /// Undefined binding.
    E021,
    /// Value used as a module.
    E022,
    /// Module used as a value.
    E023,
    /// Circular declaration.
    E024,
    /// Invalid unnamed path hanger.
    E025,
    /// `topmost` or `super` inside nested path.
    E026,
    /// Attribute argument type mismatch.
    E027,
    /// Unexpected named attribute argument.
    E028,
    /// Use of private binding.
    E029,
    /// Missing type annotation for lambda literal parameter or pattern.
    E030,
    /// Illegal function application.
    E031,
    /// Type mismatch.
    E032,
    /// Invalid constructor.
    E033,
    /// Invalid position of binder in pattern.
    E034,
    /// Type analysis.
    E035,
    /// Invalid word (package name, component name, metadata key, …).
    E036,
    /// Exposure reach not an ancestor of definition-site namespace.
    E037,
    /// Use of internal binding.
    E038,
    /// Redefinition of known binding.
    E039,
    /// Redefinition of intrinsic binding.
    E040,
    /// Module header is not the first declaration.
    E041,
    /// Intrinsic declaration with a body.
    E042,
    /// Attempt to construct a type with a literal that does not supported it.
    E043,
    /// Unbalanced bracket.
    E044,
    /// Trailing dash on identifier.
    E045,
    /// Invalid indentation.
    E046,
    /// Unterminated text literal.
    E047,
    /// Missing program entry.
    E050,
    /// Missing intrinsic binding.
    E060,
    /// Unrecognized intrinsic binding.
    E061,
    /// Missing known binding.
    E062,
    /// Unrecognized known binding.
    E063,
    /// Metadata: Type mismatch.
    E800,
    /// Metadata: Unknown entry.
    E801,
    /// Metadata: Missing entry.
    E802,
    /// Metadata: Duplicate entries.
    E803,
}

impl ErrorCode {
    pub(crate) const fn explanation(self) -> Option<&'static str> {
        #[allow(clippy::match_single_binding)]
        match self {
            // @Task
            _ => None,
        }
    }
}

impl TryFrom<Code> for ErrorCode {
    type Error = ();

    fn try_from(value: Code) -> Result<Self, Self::Error> {
        obtain!(value, Code::Error(code) => code).ok_or(())
    }
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(self, f)
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Str)]
#[format(dash_case)]
pub enum LintCode {
    /// _Permanently unassigned_ (used in tests).
    PermanentlyUnassignedOne,
    /// _Permanently unassigned_ (used in tests).
    PermanentlyUnassignedTwo,
    Deprecated,
}

impl fmt::Display for LintCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.name())
    }
}
