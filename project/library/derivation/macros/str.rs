use crate::{
    format::FormatAttribute,
    utility::{HelperAttribute, TokenStream1, TokenStream2},
};
use proc_macro2::Span;
use quote::quote;
use syn::{
    parse::{Nothing, Parse, ParseStream},
    token::Paren,
    Error, Fields, Ident,
};

// @Task check if the type derives Copy and if so, pass self by value not by reference

pub(crate) fn derive(input: TokenStream1) -> Result<TokenStream2, Error> {
    let input: syn::ItemEnum = syn::parse(input)?;
    let type_ = input.ident;
    let visibility = input.vis;

    let FormatAttribute { letter_case } = HelperAttribute::obtain(&type_, &input.attrs)?;
    let StrAttribute { method } =
        HelperAttribute::obtain_optional(&input.attrs)?.unwrap_or_default();

    let mut mapping = Vec::with_capacity(input.variants.len());

    for variant in input.variants {
        let fields = match variant.fields {
            Fields::Named(_) => quote! { { .. } },
            Fields::Unnamed(_) => quote! { (..) },
            Fields::Unit => quote! {},
        };

        let name = &variant.ident;
        let representation = letter_case.transform(name);

        mapping.push(quote! { Self::#name #fields => #representation });
    }

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    let scrutinee = if mapping.is_empty() {
        // `Self` is an uninhabited type and to be able to generate an exhaustive
        // match expression, we need to dereference the receiver.
        quote! { *self }
    } else {
        quote! { self }
    };

    Ok(quote! {
        impl #impl_generics #type_ #type_generics #where_clause {
            #visibility const fn #method(&self) -> &'static ::core::primitive::str {
                match #scrutinee { #( #mapping ),* }
            }
        }
    })
}

pub(crate) struct StrAttribute {
    pub(crate) method: Ident,
}

impl Default for StrAttribute {
    fn default() -> Self {
        Self {
            method: Ident::new("name", Span::call_site()),
        }
    }
}

impl HelperAttribute for StrAttribute {
    const NAME: &'static str = "str";
}

impl Parse for StrAttribute {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let content;
        let _: Paren = syn::parenthesized!(content in input);
        let method = content.parse()?;
        let _: Nothing = content.parse()?;
        let _: Nothing = input.parse()?;

        Ok(Self { method })
    }
}
