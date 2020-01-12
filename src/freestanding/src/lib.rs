extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse::Parse, Result};

#[proc_macro_attribute]
pub fn freestanding(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = syn::parse_macro_input!(item as syn::ItemEnum);

    // @Note not yet supported
    assert!(item.generics.where_clause.is_none());
    assert!(item.generics.params.is_empty());

    let mut shared_attrs = Vec::new();
    let mut attrs = Vec::new();
    let mut streamliner = None;

    for attr in &item.attrs {
        // @Task handle error
        let name = attr.path.get_ident().unwrap();

        match &*name.to_string() {
            "derive" => shared_attrs.push(attr),
            "doc" => attrs.push(attr),
            "common" => todo!(),
            "streamline" => {
                assert!(streamliner.is_none());

                let tokens = attr.tokens.clone().into();
                streamliner = Some(syn::parse_macro_input!(tokens as Streamliner));
            }
            _ => panic!(),
        }
    }

    let mut variants = Vec::with_capacity(item.variants.len());

    for variant in &item.variants {
        assert!(variant.discriminant.is_none());

        if variant.ident == item.ident {
            // @Task better error message
            panic!();
        }

        let mut attrs = Vec::new();

        for attr in &variant.attrs {
            // @Task handle error
            let name = attr.path.get_ident().unwrap();
            match &*name.to_string() {
                "doc" => attrs.push(attr),
                _ => panic!(),
            }
        }

        let fields: Vec<_> = match &variant.fields {
            syn::Fields::Named(fields) => fields
                .named
                .iter()
                .map(|field| syn::Field {
                    vis: item.vis.clone(),
                    ..field.clone()
                })
                .collect(),
            syn::Fields::Unnamed(_fields) => todo!(),
            syn::Fields::Unit => todo!(),
        };

        variants.push((variant.ident.clone(), attrs, fields));
    }

    let mut body = TokenStream2::new();

    for (name, ..) in &variants {
        let inner = match &streamliner {
            Some(Streamliner { path }) => quote! { #path<#name> },
            None => quote! { #name },
        };
        body.extend(quote! { #name(#inner), });
    }

    let vis = item.vis;
    let name = item.ident;

    let mut stream = quote! {
        #( #attrs )*
        #( #shared_attrs )*
        #vis enum #name { #body }
    };

    for (name, attrs, fields) in variants {
        stream.extend(quote! {
            #( #attrs )*
            #( #shared_attrs )*
            #vis struct #name { #( #fields, )* }
        });
    }

    stream.into()
}

struct Streamliner {
    path: syn::TypePath,
}

impl Parse for Streamliner {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let content;
        syn::parenthesized!(content in input);

        Ok(Streamliner {
            path: content.parse()?,
        })
    }
}
