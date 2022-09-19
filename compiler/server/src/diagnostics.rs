use crate::span::ToLocationExt;
use diagnostics::{Code, Highlight, LintCode, Role, Severity, UnboxedUntaggedDiagnostic};
use span::SourceMap;
use std::{collections::BTreeSet, default::default};
use tower_lsp::lsp_types as lsp;

const DIAGNOSTIC_SOURCE: &str = "source";

pub(crate) type LspMessage = (lsp::MessageType, String);

pub(crate) trait DiagnosticExt {
    fn into_lsp_type(self, map: &SourceMap) -> lsp::OneOf<lsp::Diagnostic, LspMessage>;
}

impl DiagnosticExt for UnboxedUntaggedDiagnostic {
    fn into_lsp_type(self, map: &SourceMap) -> lsp::OneOf<lsp::Diagnostic, LspMessage> {
        // @Task explain the " "-hack
        let message = self.message.unwrap_or_else(|| " ".into()).into();

        match convert_highlights(self.highlights, map) {
            Some((range, related_information)) => {
                let tags = self.code.and_then(|code| {
                    (code == Code::Lint(LintCode::Deprecated))
                        .then(|| vec![lsp::DiagnosticTag::DEPRECATED])
                });

                lsp::OneOf::Left(lsp::Diagnostic {
                    range,
                    severity: Some(IntoLspType::into(self.severity)),
                    code: self.code.map(IntoLspType::into),
                    source: Some(DIAGNOSTIC_SOURCE.into()),
                    message,
                    related_information: Some(related_information),
                    tags,
                    ..default()
                })
            }
            None => lsp::OneOf::Right((IntoLspType::into(self.severity), message)),
        }
    }
}

fn convert_highlights(
    highlights: BTreeSet<Highlight>,
    map: &SourceMap,
) -> Option<(lsp::Range, Vec<lsp::DiagnosticRelatedInformation>)> {
    let mut range = None;
    let mut related_information = Vec::new();

    // @Beacon @Task improve this conversion!!!
    for highlight in highlights {
        if highlight.role == Role::Primary && range.is_none() {
            // @Beacon @Bug we are ignoring the file assoc w/ the span!!!
            range = Some(highlight.span.to_location(map).range);
        } else {
            related_information.push(lsp::DiagnosticRelatedInformation {
                location: highlight.span.to_location(map),
                // @Task explain " "-hack
                message: highlight.label.unwrap_or_else(|| " ".into()).into(),
            });
        }
    }

    Some((range?, related_information))
}

trait IntoLspType<Output> {
    fn into(self) -> Output;
}

impl IntoLspType<lsp::DiagnosticSeverity> for Severity {
    fn into(self) -> lsp::DiagnosticSeverity {
        match self {
            Self::Bug | Self::Error => lsp::DiagnosticSeverity::ERROR,
            Self::Warning => lsp::DiagnosticSeverity::WARNING,
            Self::Debug => lsp::DiagnosticSeverity::INFORMATION,
        }
    }
}

impl IntoLspType<lsp::MessageType> for Severity {
    fn into(self) -> lsp::MessageType {
        match self {
            Self::Bug | Self::Error => lsp::MessageType::ERROR,
            Self::Warning => lsp::MessageType::WARNING,
            Self::Debug => lsp::MessageType::INFO,
        }
    }
}

impl IntoLspType<lsp::NumberOrString> for Code {
    fn into(self) -> lsp::NumberOrString {
        lsp::NumberOrString::String(self.to_string())
    }
}
