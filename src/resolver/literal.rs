// @Beacon @Beacon @Beacon @Note it's probably best to move the stuff here back to mod resolver and make it
// a method of Resolver since those param lists grew quite a bit!

use crate::{
    diagnostics::{Code, Diagnostic, Reporter},
    error::Result,
    hir::{DeclarationIndex, Number, Text},
    package::{session::IntrinsicType, BuildSession},
    span::Spanned,
};

const DEFAULT_NUMBER_LITERAL_TYPE: IntrinsicType = IntrinsicType::Nat;
const _DEFAULT_TEXT_LITERAL_TYPE: IntrinsicType = IntrinsicType::Text;

pub(super) fn resolve_number_literal(
    number: Spanned<&str>,
    type_: Option<DeclarationIndex>,
    session: &BuildSession,
    reporter: &Reporter,
) -> Result<Number> {
    let type_ = match type_ {
        Some(type_) => session
            .intrinsic_types()
            .find(|(_, identifier)| identifier.declaration_index().unwrap() == type_)
            .map(|(known, _)| known)
            .ok_or_else(|| {
                //@Temporary message
                Diagnostic::error()
                    .message("type does not have number literals")
                    .report(reporter)
            })?,
        None => DEFAULT_NUMBER_LITERAL_TYPE,
    };

    let result = match type_ {
        IntrinsicType::Nat => number.value.parse().map(Number::Nat).map_err(drop),
        IntrinsicType::Nat32 => number.value.parse().map(Number::Nat32).map_err(drop),
        IntrinsicType::Nat64 => number.value.parse().map(Number::Nat64).map_err(drop),
        IntrinsicType::Int => Ok(Number::Int(number.value.parse().unwrap())),
        IntrinsicType::Int32 => number.value.parse().map(Number::Int32).map_err(drop),
        IntrinsicType::Int64 => number.value.parse().map(Number::Int64).map_err(drop),
        _ => {
            //@Temporary message
            Diagnostic::error()
                .message("type does not have number literals")
                .report(reporter);
            return Err(());
        }
    };

    // @Beacon @Task adjust message texts to fit new context of namespaced literals
    result.map_err(|_| {
        Diagnostic::error()
            .code(Code::E007)
            .message(format!(
                "number literal `{number}` does not fit type `{type_}`",
            ))
            .primary_span(number)
            .note(format!(
                "values of this type must fit integer interval {}",
                interval(type_)
            ))
            .report(reporter);
    })
}

const fn interval(type_: IntrinsicType) -> &'static str {
    match type_ {
        // @Question use `∞`?
        IntrinsicType::Nat => "[0, infinity)",
        IntrinsicType::Nat32 => "[0, 2^32-1]",
        IntrinsicType::Nat64 => "[0, 2^64-1]",
        IntrinsicType::Int32 => "[-2^31, 2^31-1]",
        IntrinsicType::Int64 => "[-2^63, 2^63-1]",
        _ => unreachable!(),
    }
}

#[allow(clippy::unnecessary_wraps)] // @Temporary
pub(super) fn resolve_text_literal(
    text: Spanned<&str>,
    _namespace: Option<DeclarationIndex>,
    _session: &BuildSession,
    _reporter: &Reporter,
) -> Result<Text> {
    // @Beacon @Beacon @Beacon @Temporary
    Ok(Text::Text(text.value.to_owned()))
}

// @Task fn resolve_sequence_literal @Note also uses known bindings not intrinsic bindings (List, Vector, …)
