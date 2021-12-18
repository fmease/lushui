use proc_macro::TokenStream as TokenStream1;
use quote::quote;
use syn::parse::Error;

// @Task support generics
// @Task allow configuring derives and visibility of the discriminant type
// @Task copy #[repr(_)] from input type to discriminant type

#[proc_macro_derive(Discriminant, attributes(discriminant))]
pub fn derive_discriminant(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as syn::ItemEnum);
    let visibility = input.vis;
    let type_ = input.ident;

    // @Task throw an error if there are several `#[discriminant]`s
    let discriminant_type_info = match input
        .attrs
        .into_iter()
        .find(|attr| attr.path.is_ident("discriminant"))
    {
        Some(attribute) => attribute.tokens,
        None => {
            return Error::new_spanned(
                &type_,
                "missing helper attribute `#[discriminant(…)]` specifying a type name",
            )
            .into_compile_error()
            .into();
        }
    };

    let (discriminant_type, discriminant_method) =
        match syn::parse::<DiscriminantTypeInfo>(discriminant_type_info.into()) {
            Ok(DiscriminantTypeInfo { type_, method, .. }) => (type_, method),
            // @Task use span of attribute
            Err(error) => return error.to_compile_error().into(),
        };

    let discriminants = input.variants.iter().map(|variant| &variant.ident);

    let mapping = input.variants.iter().map(|variant| {
        let fields = match variant.fields {
            syn::Fields::Named(_) => quote! { { .. } },
            syn::Fields::Unnamed(_) => quote! { (..) },
            syn::Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        quote! { Self::#name #fields => self::#discriminant_type::#name }
    });

    let stream = quote! {

        #[derive(Clone, Copy, PartialEq, Eq, Debug)] // @Task make this customizable
        #visibility enum #discriminant_type {
            #( #discriminants ),*
        }

        impl #type_ {
            #visibility const fn #discriminant_method(&self) -> self::#discriminant_type {
                match self {
                    #( #mapping ),*
                }
            }
        }

    };

    stream.into()
}

struct DiscriminantTypeInfo {
    _parenthesis: syn::token::Paren,
    type_: syn::Ident,
    _separator: syn::Token![::],
    method: syn::Ident,
}

impl syn::parse::Parse for DiscriminantTypeInfo {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let content;

        Ok(Self {
            _parenthesis: syn::parenthesized!(content in input),
            type_: content.parse()?,
            _separator: content.parse()?,
            method: content.parse()?,
        })
    }
}

// @Task use the Trait `Elements` for this
#[proc_macro_derive(Elements)]
pub fn derive_elements(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as syn::ItemEnum);

    let visibility = input.vis;
    let type_name = input.ident;

    let variants = input.variants.iter().map(|variant| {
        let fields = match &variant.fields {
            syn::Fields::Named(fields) => {
                let fields = fields.named.iter().map(|field| &field.ident);

                quote! { { #( #fields: ::std::default::Default::default() ),* } }
            }
            syn::Fields::Unnamed(fields) => {
                let fields = fields.unnamed.iter().map(|field| &field.ident);

                quote! { ( #( #fields ::std::default::Default::default() ),* ) }
            }
            syn::Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        quote! { Self::#name #fields }
    });

    let stream = quote! {
        impl #type_name {
            #visibility fn elements() -> impl ::std::iter::Iterator<Item = Self> {
                ::std::iter::IntoIterator::into_iter([#( #variants ),*])
            }
        }
    };

    stream.into()
}
