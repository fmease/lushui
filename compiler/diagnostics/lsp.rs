use super::{Code, Highlight, LintCode, Role, Severity, UnboxedUntaggedDiagnostic};
use crate::span::SourceMap;
use std::{collections::BTreeSet, default::default};
use tower_lsp::lsp_types::{
    self, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, MessageType,
    NumberOrString, OneOf, Range,
};

const DIAGNOSTIC_SOURCE: &str = "source";

pub(crate) type LspMessage = (MessageType, String);

impl UnboxedUntaggedDiagnostic {
    pub(crate) fn into_lsp_type(self, map: &SourceMap) -> OneOf<lsp_types::Diagnostic, LspMessage> {
        // @Task explain the " "-hack
        let message = self.message.unwrap_or_else(|| " ".into()).into();

        match convert_highlights(self.highlights, map) {
            Some((range, related_information)) => {
                let tags = self.code.and_then(|code| {
                    (code == Code::Lint(LintCode::Deprecated))
                        .then(|| vec![DiagnosticTag::DEPRECATED])
                });

                OneOf::Left(lsp_types::Diagnostic {
                    range,
                    severity: Some(self.severity.into()),
                    code: self.code.map(Into::into),
                    source: Some(DIAGNOSTIC_SOURCE.into()),
                    message,
                    related_information: Some(related_information),
                    tags,
                    ..default()
                })
            }
            None => OneOf::Right((self.severity.into(), message)),
        }
    }
}

fn convert_highlights(
    highlights: BTreeSet<Highlight>,
    map: &SourceMap,
) -> Option<(Range, Vec<DiagnosticRelatedInformation>)> {
    let mut range = None;
    let mut related_information = Vec::new();

    // @Beacon @Task improve this conversion!!!
    for highlight in highlights {
        if highlight.role == Role::Primary && range.is_none() {
            // @Beacon @Bug we are ignoring the file assoc w/ the span!!!
            range = Some(highlight.span.to_location(map).range);
        } else {
            related_information.push(DiagnosticRelatedInformation {
                location: highlight.span.to_location(map),
                // @Task explain " "-hack
                message: highlight.label.unwrap_or_else(|| " ".into()).into(),
            });
        }
    }

    Some((range?, related_information))
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

impl From<Severity> for MessageType {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Bug | Severity::Error => Self::ERROR,
            Severity::Warning => Self::WARNING,
            Severity::Debug => Self::INFO,
        }
    }
}

impl From<Code> for NumberOrString {
    fn from(code: Code) -> Self {
        Self::String(code.to_string())
    }
}
