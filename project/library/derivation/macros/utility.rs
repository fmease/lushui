pub(crate) use proc_macro::TokenStream as TokenStream1;
pub(crate) use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{
    parse::{Error, Parse},
    AttrStyle, Attribute, Fields,
};

pub(crate) trait HelperAttribute: Parse {
    const NAME: &'static str;

    fn obtain<Owner: ToTokens>(owner: &Owner, attrs: &[Attribute]) -> syn::Result<Self> {
        Self::obtain_optional(attrs)?.ok_or_else(|| {
            Error::new_spanned(
                owner,
                format!("missing helper attribute `#[{}]`", Self::NAME),
            )
        })
    }

    fn obtain_optional(attrs: &[Attribute]) -> syn::Result<Option<Self>> {
        // @Task throw an error if there are several helper attributes
        attrs
            .iter()
            .find(|attr| attr.path().is_ident(Self::NAME) && matches!(attr.style, AttrStyle::Outer))
            .map(|attribute| match &attribute.meta {
                syn::Meta::List(list) => syn::parse2(list.tokens.clone()),
                syn::Meta::Path(_) | syn::Meta::NameValue(_) => todo!(), // @Task
            })
            .transpose()
    }
}

pub(crate) trait SerializeExt {
    fn serialize(self) -> TokenStream1;
}

impl SerializeExt for syn::Result<TokenStream2> {
    fn serialize(self) -> TokenStream1 {
        self.map_err(Error::into_compile_error)
            .unwrap_or_else(std::convert::identity)
            .into()
    }
}

pub(crate) fn ensure_variant_is_fieldless(fields: &Fields, macro_name: &str) -> syn::Result<()> {
    if let Fields::Named(_) | Fields::Unnamed(_) = fields {
        return Err(Error::new_spanned(
            fields,
            format!("variant may not have fields when deriving `derivation::{macro_name}`"),
        ));
    };

    Ok(())
}
