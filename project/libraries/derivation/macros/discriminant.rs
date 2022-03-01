use crate::utilites::{HelperAttribute, TokenStream1, TokenStream2};
use quote::quote;
use syn::{
    parse::{Nothing, Parse, ParseStream},
    token::Paren,
    Error, Fields, Ident, Token,
};

// @Task allow configuring derives and visibility of the discriminant type
// @Task copy #[repr(_)] from input type to discriminant type

// @Note syntax of attrs:
// #[discriminant(MAPPING: $( #[…] )*) TYPE])]

pub(crate) fn derive(input: TokenStream1) -> Result<TokenStream2, Error> {
    let input: syn::ItemEnum = syn::parse(input)?;
    let visibility = input.vis;
    let type_ = input.ident;

    let DiscriminantAttribute {
        discriminant_type,
        discriminant_method,
    } = HelperAttribute::obtain(&type_, &input.attrs)?;

    let discriminants = input.variants.iter().map(|variant| &variant.ident);

    let mapping = input.variants.iter().map(|variant| {
        let fields = match variant.fields {
            Fields::Named(_) => quote! { { .. } },
            Fields::Unnamed(_) => quote! { (..) },
            Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        quote! { Self::#name #fields => self::#discriminant_type::#name }
    });

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        #[derive(Clone, Copy, PartialEq, Eq, Debug)] // @Task make this customizable
        // @Task add const params to the discr type
        #visibility enum #discriminant_type {
            #( #discriminants ),*
        }

        impl #impl_generics #type_ #type_generics #where_clause {
            #visibility const fn #discriminant_method(&self) -> self::#discriminant_type {
                match self {
                    #( #mapping ),*
                }
            }
        }
    })
}

struct DiscriminantAttribute {
    discriminant_method: Ident,
    discriminant_type: Ident,
}

impl HelperAttribute for DiscriminantAttribute {
    const NAME: &'static str = "discriminant";
}

impl Parse for DiscriminantAttribute {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let content;
        let _: Paren = syn::parenthesized!(content in input);
        let method = content.parse()?;
        let _: Token![:] = content.parse()?;
        let type_ = content.parse()?;
        let _: Nothing = content.parse()?;
        let _: Nothing = input.parse()?;

        Ok(Self {
            discriminant_method: method,
            discriminant_type: type_,
        })
    }
}
