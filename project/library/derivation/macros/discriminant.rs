use crate::utility::{HelperAttribute, TokenStream1, TokenStream2};
use quote::quote;
use syn::{
    parse::{Nothing, Parse, ParseStream},
    Attribute, Error, Fields, Ident, Token,
};

// @Task allow configuring derives and visibility of the discriminant type
// @Task copy #[repr(_)] from input type to discriminant type

// @Note syntax of attrs:
// #[discriminant(MAPPING: $( #[…] )*) TYPE])]

pub(crate) fn derive(input: TokenStream1) -> Result<TokenStream2, Error> {
    let input: syn::ItemEnum = syn::parse(input)?;
    let visibility = input.vis;
    let ty = input.ident;

    let DiscriminantAttribute {
        ty: discriminant_ty,
        attrs: discriminant_attributes,
        method: discriminant_method,
    } = HelperAttribute::obtain(&ty, &input.attrs)?;

    let discriminants = input.variants.iter().map(|variant| &variant.ident);

    let mapping = input.variants.iter().map(|variant| {
        let fields = match variant.fields {
            Fields::Named(_) => quote! { { .. } },
            Fields::Unnamed(_) => quote! { (..) },
            Fields::Unit => quote! {},
        };
        let name = &variant.ident;

        quote! { Self::#name #fields => self::#discriminant_ty::#name }
    });

    let (impl_generics, type_generics, where_clause) = input.generics.split_for_impl();

    Ok(quote! {
        #[derive(Clone, Copy, PartialEq, Eq)]
        #( #discriminant_attributes )*
        // @Task add const params to the discr type
        #visibility enum #discriminant_ty {
            #( #discriminants ),*
        }

        impl #impl_generics #ty #type_generics #where_clause {
            #visibility const fn #discriminant_method(&self) -> self::#discriminant_ty {
                match self {
                    #( #mapping ),*
                }
            }
        }
    })
}

struct DiscriminantAttribute {
    method: Ident,
    attrs: Vec<Attribute>,
    ty: Ident,
}

impl HelperAttribute for DiscriminantAttribute {
    const NAME: &'static str = "discriminant";
}

impl Parse for DiscriminantAttribute {
    fn parse(input: ParseStream<'_>) -> syn::Result<Self> {
        let method = input.parse()?;
        let _: Token![:] = input.parse()?;
        let attrs = Attribute::parse_outer(input)?;
        let ty = input.parse()?;
        let _: Nothing = input.parse()?;
        let _: Nothing = input.parse()?;

        Ok(Self { method, attrs, ty })
    }
}
