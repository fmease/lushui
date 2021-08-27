use proc_macro::TokenStream as TokenStream1;
use quote::quote;
use syn::parse::Error;

// @Task make this more flexible: generics, non-tuple structs, enums w/ single variant etc
// @Task make this more robust: ::index_map might not refer to *our* index_map

#[proc_macro_derive(Index)]
pub fn derive_index(input: TokenStream1) -> TokenStream1 {
    let input = syn::parse_macro_input!(input as syn::ItemStruct);

    if !input.generics.params.is_empty() {
        return Error::new_spanned(&input.generics.params, "parameters not supported")
            .into_compile_error()
            .into();
    }

    let ident = input.ident;

    let stream = quote! {
        impl ::index_map::Index for #ident {
            fn new(index: ::core::primitive::usize) -> Self {
                self::#ident(index) // @Note not flexible
            }

            fn value(self) -> ::core::primitive::usize {
                self.0 // @Note note flexible
            }
        }
    };

    stream.into()
}
