use diagnostics::{reporter::ErasedReportedError, Diag};
use lexer::word::Word;
use session::unit::CompTy;
use span::Spanned;

pub(crate) enum DependencyResolutionError {
    ErasedNonFatal(ErasedReportedError),
    ErasedFatal(ErasedReportedError),
    // @Note component exists, not fully resolved yet
    UnresolvedLocalComponent(Spanned<Word>),
    #[allow(dead_code)] // @Temporary
    Cycle(Spanned<Word>),
}

impl From<ErasedReportedError> for DependencyResolutionError {
    fn from(error: ErasedReportedError) -> Self {
        Self::ErasedNonFatal(error)
    }
}

pub(crate) fn undefined_component_error(name: Spanned<Word>, package: Word) -> Diag {
    // @Question should we special-case component name = package name?

    Diag::error()
        .message(format!(
            "the package ‘{package}’ does not contain a component called ‘{name}’"
        ))
        .unlabeled_span(name)
}

pub(crate) fn non_library_dependency_error(name: Spanned<Word>, ty: CompTy, package: Word) -> Diag {
    Diag::error()
        .message(format!(
            "the component ‘{name}’ in package ‘{package}’ is not a library",
        ))
        .unlabeled_span(name)
        .note(format!("one cannot depend on {ty} components"))
}
