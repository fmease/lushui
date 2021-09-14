use proc_macro::TokenStream as TokenStream1;
use quote::quote;

// @Task support generics
// @Task allow configuring derives and visibility of the discriminant type
// @Task copy #[repr(_)] from input type to discriminant type

#[proc_macro_derive(Discriminant, attributes(discriminant))]
pub fn derive_discriminant(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as syn::ItemEnum);

    let visibility = input.vis;

    let type_name = input.ident;

    // @Task throw an error if there are several `#[discriminant]`s
    // @Task don't unwrap
    let discriminant_type_info = input
        .attrs
        .into_iter()
        .find(|attr| attr.path.is_ident("discriminant"))
        .unwrap()
        .tokens;

    let discriminant_type_name = syn::parse::<DiscriminantTypeInfo>(discriminant_type_info.into())
        .unwrap()
        .identifier;

    let discriminants = input.variants.iter().map(|variant| &variant.ident);

    let variant_discriminant_mappings = input.variants.iter().map(|variant| {
        let fields = match variant.fields {
            syn::Fields::Named(_) => quote! { { .. } },
            syn::Fields::Unnamed(_) => quote! { (..) },
            syn::Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        quote! { Self::#name #fields => Self::Discriminant::#name }
    });

    let stream = quote! {

        #[derive(Clone, Copy, PartialEq, Eq, Debug)] // @Task make this customizable
        #visibility enum #discriminant_type_name {
            #( #discriminants ),*
        }

        // @Bug not robust enough
        impl ::discriminant::Discriminant for #type_name {
            type Discriminant = self::#discriminant_type_name; // @Temporary

            fn discriminant(&self) -> Self::Discriminant {
                match self {
                    #( #variant_discriminant_mappings ),*
                }
            }
        }

    };

    stream.into()
}

struct DiscriminantTypeInfo {
    _parenthesis: syn::token::Paren,
    identifier: syn::Ident,
}

impl syn::parse::Parse for DiscriminantTypeInfo {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            _parenthesis: syn::parenthesized!(content in input),
            identifier: content.parse()?,
        })
    }
}
