#[allow(clippy::wildcard_imports)] // private inline module
use super::*;
use lexer::token::{IndentationError, INDENTATION};

pub(super) fn invalid_token(error: lexer::Error) -> Diagnostic {
    use lexer::BareError::*;

    match error.bare {
        InvalidIndentation(difference, indentation_error) => Diagnostic::error()
            .code(ErrorCode::E046)
            .message(format!(
                "invalid indentation consisting of {} spaces",
                difference.0
            ))
            .unlabeled_span(error.span)
            .note(match indentation_error {
                IndentationError::Misaligned => {
                    format!("indentation needs to be a multiple of {}", INDENTATION.0)
                }
                IndentationError::TooDeep => format!(
                    "indentation is greater than {} and therefore too deep",
                    INDENTATION.0
                ),
            }),
        InvalidToken(token) => {
            let message = format!("found invalid character U+{:04X} ‘{token}’", token as u32);

            // @Task code
            Diagnostic::error()
                .message(message)
                .span(error.span, "unexpected token")
        }
        UnbalancedBracket(bracket) => Diagnostic::error()
            .code(ErrorCode::E044)
            .message(format!("unbalanced {} bracket", bracket.kind))
            .span(
                error.span,
                format!(
                    "has no matching {} {} bracket",
                    !bracket.orientation, bracket.kind
                ),
            ),
        // @Task improve message, mention closing it with quotes
        UnterminatedTextLiteral => Diagnostic::error()
            .code(ErrorCode::E047)
            .message("unterminated text literal")
            .unlabeled_span(error.span),
    }
}
