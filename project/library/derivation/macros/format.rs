use crate::utility::HelperAttribute;
use syn::{
    Error, Ident,
    parse::{Nothing, Parse, ParseStream},
};

pub(crate) struct FormatAttribute {
    pub(crate) letter_case: LetterCase,
}

impl HelperAttribute for FormatAttribute {
    const NAME: &'static str = "format";
}

impl Parse for FormatAttribute {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let letter_case = input.parse()?;
        let _: Nothing = input.parse()?;
        let _: Nothing = input.parse()?;

        Ok(Self { letter_case })
    }
}

#[derive(Clone, Copy)]
pub(crate) enum LetterCase {
    DashCase,
}

impl LetterCase {
    const fn name(self) -> &'static str {
        match self {
            Self::DashCase => "dash_case",
        }
    }
}

impl LetterCase {
    pub(crate) fn transform(self, ident: &Ident) -> String {
        match self {
            Self::DashCase => heck::AsKebabCase(ident.to_string()).to_string(),
        }
    }
}

impl Parse for LetterCase {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let ident: Ident = input.parse()?;

        if ident == Self::DashCase.name() {
            Ok(Self::DashCase)
        } else {
            Err(Error::new_spanned(&ident, "invalid letter case"))
        }
    }
}
