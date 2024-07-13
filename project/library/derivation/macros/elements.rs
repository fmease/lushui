use crate::utility::{TokenStream1, TokenStream2};
use quote::quote;
use syn::{Error, Fields};

pub(crate) fn derive(input: TokenStream1) -> Result<TokenStream2, Error> {
    let input: syn::ItemEnum = syn::parse(input)?;
    let ty = input.ident;
    let length = input.variants.len();

    let mut variants = Vec::with_capacity(length);

    for variant in input.variants {
        let fields = match &variant.fields {
            Fields::Named(fields) => {
                let fields = fields.named.iter().map(|field| &field.ident);
                quote! { { #( #fields: ::std::default::Default::default() ),* } }
            }
            Fields::Unnamed(fields) => {
                let fields = fields.unnamed.iter().map(|field| &field.ident);
                quote! { ( #( #fields ::std::default::Default::default() ),* ) }
            }
            Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        variants.push(quote! { Self::#name #fields });
    }

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        // @Task make `::derivation` hygienic
        impl #impl_generics ::derivation::Elements for #ty #type_generics #where_clause {
            type Iter = ::core::array::IntoIter<Self, #length>;

            fn elements() -> Self::Iter {
                [#( #variants ),*].into_iter()
            }
        }
    })
}
