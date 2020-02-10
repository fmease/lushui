use crate::span::Span;

pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    // code: Code
    // @Question multispan???
    pub span: Span,
    // @Note example: binding defined multiple time
    // primary span (this span): second binding (iilegal)
    // secondary span (span of subdiag): first binding (legal)
    pub sub: Vec<SubDiagnostic>,
}

pub struct SubDiagnostic {
    pub level: Level,
    pub message: String,
    pub span: Span,
}

pub enum Level {
    Bug,
    Fatal,
    Error,
    Warning,
    Note,
}
