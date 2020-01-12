extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{parse::Parse, Result};

#[proc_macro_attribute]
pub fn freestanding(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let item = syn::parse_macro_input!(item as syn::ItemEnum);

    assert!(
        item.generics.where_clause.is_none(),
        "where clauses not yet supported"
    );
    assert!(
        item.generics.params.is_empty(),
        "type parameters not yet supported"
    );

    let mut shared_attrs = Vec::new();
    let mut attrs = Vec::new();
    let mut streamliner = None;

    for attr in &item.attrs {
        // @Task handle error
        let name = attr.path.get_ident().unwrap();

        match &*name.to_string() {
            "derive" => shared_attrs.push(attr),
            "doc" => attrs.push(attr),
            "common" => panic!("common fields not yet supported"),
            "streamline" => {
                assert!(streamliner.is_none(), "more than one streamliner found");

                let tokens = attr.tokens.clone().into();
                streamliner = Some(syn::parse_macro_input!(tokens as Streamliner));
            }
            _ => panic!("unsupported attribute on enum"),
        }
    }

    let mut variants = Vec::with_capacity(item.variants.len());

    for variant in &item.variants {
        assert!(
            variant.discriminant.is_none(),
            "explicit discriminants not allowed"
        );

        if variant.ident == item.ident {
            // @Task better error message
            panic!("the name of the variant matches the name of the enum");
        }

        let mut attrs = Vec::new();

        for attr in &variant.attrs {
            // @Task handle error
            let name = attr
                .path
                .get_ident()
                .expect("unsupported attribute on variant");
            match &*name.to_string() {
                "doc" => attrs.push(attr),
                _ => panic!("unsupported attribute on variant"),
            }
        }

        let fields: Option<Vec<_>> = match &variant.fields {
            syn::Fields::Named(fields) => Some(
                fields
                    .named
                    .iter()
                    .map(|field| syn::Field {
                        vis: item.vis.clone(),
                        ..field.clone()
                    })
                    .collect(),
            ),
            syn::Fields::Unnamed(_fields) => panic!("unnamed fields not yet supported"),
            syn::Fields::Unit => None,
        };

        variants.push((variant.ident.clone(), attrs, fields));
    }

    let mut body = TokenStream2::new();

    for (name, _, fields) in &variants {
        body.extend(match fields {
            Some(_) => {
                let inner = match &streamliner {
                    Some(Streamliner { path }) => quote! { #path<#name> },
                    None => quote! { #name },
                };
                quote! { #name(#inner), }
            }
            None => quote! { #name, },
        })
    }

    let vis = item.vis;
    let name = item.ident;

    let mut stream = quote! {
        #( #attrs )*
        #( #shared_attrs )*
        #vis enum #name { #body }
    };

    for (name, attrs, fields) in variants {
        if let Some(fields) = fields {
            stream.extend(quote! {
                #( #attrs )*
                #( #shared_attrs )*
                #vis struct #name { #( #fields, )* }
            });
        }
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
