pub(crate) use proc_macro::TokenStream as TokenStream1;
pub(crate) use proc_macro2::TokenStream as TokenStream2;
use quote::ToTokens;
use syn::{
    parse::{Error, Parse},
    Attribute, Fields,
};

pub(crate) trait HelperAttribute: Parse {
    const NAME: &'static str;

    fn obtain<Owner: ToTokens>(owner: &Owner, attrs: &[Attribute]) -> Result<Self, Error> {
        // @Task throw an error if there are several helper attributes
        let attribute = attrs
            .iter()
            .find(|attr| attr.path.is_ident(Self::NAME))
            .ok_or_else(|| {
                let message = format!("missing helper attribute `#[{}]`", Self::NAME);
                Error::new_spanned(owner, message)
            })?;

        syn::parse(attribute.tokens.clone().into())
    }

    fn obtain_optional(attrs: &[Attribute]) -> Result<Option<Self>, Error> {
        // @Task throw an error if there are several helper attributes
        attrs
            .iter()
            .find(|attr| attr.path.is_ident(Self::NAME))
            .map(|attribute| syn::parse(attribute.tokens.clone().into()))
            .transpose()
    }
}

pub(crate) trait SerializeExt {
    fn serialize(self) -> TokenStream1;
}

impl SerializeExt for Result<TokenStream2, Error> {
    fn serialize(self) -> TokenStream1 {
        self.map_err(Error::into_compile_error)
            .unwrap_or_else(std::convert::identity)
            .into()
    }
}

pub(crate) fn ensure_variant_is_fieldless(fields: &Fields, macro_name: &str) -> Result<(), Error> {
    if let Fields::Named(_) | Fields::Unnamed(_) = fields {
        return Err(Error::new_spanned(
            fields,
            format!("variant may not have fields when deriving `derivation::{macro_name}`"),
        ));
    };

    Ok(())
}
