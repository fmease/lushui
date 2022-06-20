use super::{Code, Highlight, LintCode, Role, Severity, UnboxedUntaggedDiagnostic};
use crate::span::{SourceMap, Span};
use std::{collections::BTreeSet, default::default};
use tower_lsp::lsp_types::{
    self, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, Location,
    NumberOrString, Position, Range, Url,
};

const DIAGNOSTIC_SOURCE: &str = "source";

impl UnboxedUntaggedDiagnostic {
    pub(crate) fn into_lsp_diagnostic(self, map: &SourceMap) -> lsp_types::Diagnostic {
        let (range, related_information) = convert_highlights(self.highlights, map);

        let tags = self.code.and_then(|code| {
            (code == Code::Lint(LintCode::Deprecated)).then(|| vec![DiagnosticTag::DEPRECATED])
        });

        lsp_types::Diagnostic {
            range,
            severity: Some(self.severity.into()),
            code: self.code.map(Into::into),
            source: Some(DIAGNOSTIC_SOURCE.into()),
            // @Task explain the " "-hack
            message: self.message.unwrap_or_else(|| " ".into()).into(),
            related_information: Some(related_information),
            tags,
            ..default()
        }
    }
}

// @Beacon @Task document what happens in the case where we don't have any highlights
fn convert_highlights(
    highlights: BTreeSet<Highlight>,
    map: &SourceMap,
) -> (Range, Vec<DiagnosticRelatedInformation>) {
    let mut range = None;
    let mut related_information = Vec::new();

    // @Beacon @Task improve this conversion!!!
    for highlight in highlights {
        if highlight.role == Role::Primary && range.is_none() {
            // @Beacon @Bug we are ignoring the file assoc w/ the span!!!
            range = Some(highlight.span.into_location(map).range);
        } else {
            related_information.push(DiagnosticRelatedInformation {
                location: highlight.span.into_location(map),
                // @Task explain " "-hack
                message: highlight.label.unwrap_or_else(|| " ".into()).into(),
            });
        }
    }

    (range.unwrap_or_default(), related_information)
}

impl Span {
    fn into_location(self, map: &SourceMap) -> Location {
        let lines = map.lines_with_highlight(self);

        Location {
            // @Beacon @Task handle anonymous SourceFiles smh!!!
            uri: Url::from_file_path(lines.path.unwrap()).unwrap(),
            range: Range {
                start: Position {
                    line: lines.first.number - 1,
                    character: lines.first.highlight.start - 1,
                },
                end: match lines.last {
                    Some(line) => Position {
                        line: line.number - 1,
                        character: line.highlight.end - 1,
                    },
                    None => Position {
                        line: lines.first.number - 1,
                        character: lines.first.highlight.end - 1,
                    },
                },
            },
        }
    }
}

impl From<Severity> for DiagnosticSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Bug | Severity::Error => Self::ERROR,
            Severity::Warning => Self::WARNING,
            Severity::Debug => Self::INFORMATION,
        }
    }
}

impl From<Code> for NumberOrString {
    fn from(code: Code) -> Self {
        Self::String(code.to_string())
    }
}
