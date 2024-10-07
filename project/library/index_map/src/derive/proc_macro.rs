#![feature(let_chains)]

use proc_macro::TokenStream as TokenStream1;
use proc_macro2::Span;
use quote::quote;
use syn::{parse::Error, spanned::Spanned};

// @Task make this more flexible: generics, non-tuple structs, enums w/ single variant etc
// @Task make this more robust: ::index_map might not refer to *our* index_map

#[proc_macro_derive(Index)]
pub fn derive_index(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as syn::ItemStruct);

    if !input.generics.params.is_empty() {
        return report(input.generics.params.span(), "parameters not supported");
    }

    let ident = input.ident;

    let fields = input.fields.span();

    let ty = if let syn::Fields::Unnamed(fields) = input.fields
        && let mut fields = fields.unnamed.into_iter()
        && let Some(field) = fields.next()
        && fields.next().is_none()
    {
        field.ty
    } else {
        return report(fields, "index types must have exactly one unnamed field");
    };

    let stream = quote! {
        impl ::index_map::Index for #ident {
            type Representation = #ty;

            fn new(index: Self::Representation, _: ::index_map::Guard) -> Self {
                self::#ident(index)
            }

            fn into_inner(self, _: ::index_map::Guard) -> Self::Representation {
                self.0
            }
        }
    };

    stream.into()
}

fn report(span: Span, message: impl std::fmt::Display) -> TokenStream1 {
    Error::new(span, message).into_compile_error().into()
}
