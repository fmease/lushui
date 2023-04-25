use crate::span::ToLocationExt;
use diagnostics::{Code, Highlight, LintCode, Role, Severity, UnboxedUntaggedDiagnostic};
use span::SourceMap;
use std::collections::BTreeSet;
use tower_lsp::lsp_types::{
    self, DiagnosticRelatedInformation, DiagnosticSeverity, DiagnosticTag, MessageType,
    NumberOrString, OneOf, Range,
};
use utility::default;

const SOURCE: &str = "lushui";

pub(crate) type LspMessage = (MessageType, String);

pub(crate) trait DiagnosticExt {
    fn into_lsp_type(self, map: &SourceMap) -> OneOf<lsp_types::Diagnostic, LspMessage>;
}

impl DiagnosticExt for UnboxedUntaggedDiagnostic {
    fn into_lsp_type(self, map: &SourceMap) -> OneOf<lsp_types::Diagnostic, LspMessage> {
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
                    severity: Some(IntoLspType::into(self.severity)),
                    code: self.code.map(IntoLspType::into),
                    source: Some(SOURCE.into()),
                    message,
                    related_information: Some(related_information),
                    tags,
                    ..default()
                })
            }
            None => OneOf::Right((IntoLspType::into(self.severity), message)),
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

trait IntoLspType<Output> {
    fn into(self) -> Output;
}

impl IntoLspType<DiagnosticSeverity> for Severity {
    fn into(self) -> DiagnosticSeverity {
        match self {
            Self::Bug | Self::Error => DiagnosticSeverity::ERROR,
            Self::Warning => DiagnosticSeverity::WARNING,
            Self::Debug => DiagnosticSeverity::INFORMATION,
        }
    }
}

impl IntoLspType<MessageType> for Severity {
    fn into(self) -> MessageType {
        match self {
            Self::Bug | Self::Error => MessageType::ERROR,
            Self::Warning => MessageType::WARNING,
            Self::Debug => MessageType::INFO,
        }
    }
}

impl IntoLspType<NumberOrString> for Code {
    fn into(self) -> NumberOrString {
        NumberOrString::String(self.to_string())
    }
}
