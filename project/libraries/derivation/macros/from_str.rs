use crate::{
    format::FormatAttribute,
    utility::{ensure_variant_is_fieldless, HelperAttribute, TokenStream1, TokenStream2},
};
use quote::quote;
use syn::Error;

pub(crate) fn derive(input: TokenStream1) -> Result<TokenStream2, Error> {
    let input: syn::ItemEnum = syn::parse(input)?;
    let type_ = input.ident;
    let FormatAttribute { letter_case } = HelperAttribute::obtain(&type_, &input.attrs)?;

    let mut mapping = Vec::with_capacity(input.variants.len() + 1);

    for variant in input.variants {
        ensure_variant_is_fieldless(&variant.fields, "FromStr")?;

        let name = &variant.ident;
        let representation = letter_case.transform(&name);

        mapping.push(quote! { #representation => Self::#name });
    }

    mapping.push(quote! { _ => return ::core::result::Result::Err(()) });

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        impl #impl_generics ::core::str::FromStr for #type_ #type_generics #where_clause {
            type Err = ();

            // @Task make `__source` hygienic smh
            fn from_str(__source: &::core::primitive::str) -> ::core::result::Result<Self, Self::Err> {
                ::core::result::Result::Ok(match __source {
                    #( #mapping ),*
                })
            }
        }
    })
}
