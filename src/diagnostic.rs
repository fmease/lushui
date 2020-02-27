use crate::span::{SourceMap, Span};

// @Task document design questions properly (see stuff below)
pub struct Diagnostic {
    pub level: Level,
    pub message: String,
    // code: Code
    // @Question multispan??
    pub span: Option<Span>,
    // @Note example: binding defined multiple time
    // primary span (this span): second binding (iilegal)
    // secondary span (span of subdiag): first binding (legal)
    pub sub: Vec<SubDiagnostic>,
}

impl Diagnostic {
    pub fn bug(message: String, span: impl Into<Option<Span>>) -> Self {
        Self {
            level: Level::Bug,
            message,
            span: span.into(),
            sub: Vec::new(),
        }
    }

    pub fn fatal(message: String, span: impl Into<Option<Span>>) -> Self {
        Self {
            level: Level::Fatal,
            message,
            span: span.into(),
            sub: Vec::new(),
        }
    }

    pub fn error(message: String, span: impl Into<Option<Span>>) -> Self {
        Self {
            level: Level::Error,
            message,
            span: span.into(),
            sub: Vec::new(),
        }
    }

    pub fn warn(message: String, span: impl Into<Option<Span>>) -> Self {
        Self {
            level: Level::Warning,
            message,
            span: span.into(),
            sub: Vec::new(),
        }
    }

    pub fn emit(self, map: Option<&SourceMap>) {
        eprintln!("{}", self.display(map));
        eprintln!();
    }

    // @Beacon @Note to underline spans, we very likely need to
    // include a grapheme library so we can e.g. have two carets below a
    // Chinese character because they are so wide and just 1 below a
    // u with umlaut even if it consists of two code points (it's but 1
    // grapheme) (@Update you cannot get this right, not even rustc can :/)
    // @Beacon @Task make this more robust and able to handle multiline
    // spans (which we first need to implement in `crate::span`)
    fn display(&self, map: Option<&SourceMap>) -> String {
        let header = format!("{:#}: {}", self.level, self.message.bright_white().bold());
        if let Some(span) = self.span {
            let map = map.unwrap();
            let lines = map.resolve_span(span);
            let padding = lines.first.number.to_string().len();
            let highlight = lines.first.highlight;

            const SPACE: &str = " ";

            let preview = format!(
                "\n\
                {padding}{arrow} {file}:{line}:{column}\n\
                {padding} {bar}\n\
                {line} {bar} {snippet}{padding} {bar} {highlight_padding}{highlight}",
                arrow = "-->".bright_blue().bold(),
                file = lines.filename,
                line = lines.first.number,
                column = lines.first.highlight_column,
                snippet = lines.first.content,
                padding = SPACE.repeat(padding),
                highlight_padding = SPACE.repeat(*highlight.start()),
                highlight = "^"
                    .repeat(highlight.end() + 1 - highlight.start())
                    .color(self.level.color())
                    .bold(),
                bar = "|".bright_blue().bold()
            );

            header + &preview
        } else {
            header
        }
    }
}

pub struct SubDiagnostic {
    pub level: Level,
    pub message: String,
    pub span: Span,
}

// @Note rustc also has `Help`
#[derive(Clone, Copy)]
pub enum Level {
    Bug,
    Fatal,
    Error,
    Warning,
    Note,
}

use colored::{Color, Colorize};

impl Level {
    pub fn to_str(self) -> &'static str {
        match self {
            Self::Bug => "internal compiler error",
            Self::Fatal | Self::Error => "error",
            Self::Warning => "warning",
            Self::Note => "note",
        }
    }

    pub fn color(self) -> Color {
        match self {
            Self::Bug | Self::Fatal | Self::Error => Color::BrightRed,
            Self::Warning => Color::BrightYellow,
            Self::Note => Color::BrightBlue,
        }
    }
}

use std::fmt;

impl fmt::Display for Level {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if !f.alternate() {
            f.write_str(self.to_str())
        } else {
            write!(f, "{}", self.to_str().color(self.color()).bold())
        }
    }
}
