mod derive_fn;
mod derive_type;

extern crate proc_macro;

use crate::derive_fn::derive_contract_function_set;
use crate::derive_type::{derive_type_enum, derive_type_struct};
use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, DeriveInput, Error, ImplItem, ImplItemFn, ItemImpl,
};

#[proc_macro_attribute]
pub fn contracttype(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    quote! {
        #[derive(soroban_builtin_sdk_macros::ContractType)]
        #input
    }
    .into()
}

#[doc(hidden)]
#[proc_macro_derive(ContractType)]
pub fn derive_contract_type(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let ident = &input.ident;
    let derived = match &input.data {
        syn::Data::Struct(s) => derive_type_struct(ident, s),
        syn::Data::Enum(e) => derive_type_enum(ident, e),
        syn::Data::Union(u) => Error::new(
            u.union_token.span(),
            "unions are unsupported as contract types",
        )
        .to_compile_error(),
    };
    quote! { #derived }.into()
}

fn get_methods(imp: &ItemImpl) -> impl Iterator<Item = &ImplItemFn> {
    imp.items.iter().filter_map(|i| match i {
        ImplItem::Fn(m) => Some(m),
        _ => None,
    })
}

#[proc_macro_attribute]
pub fn contractimpl(_metadata: TokenStream, input: TokenStream) -> TokenStream {
    let imp = parse_macro_input!(input as ItemImpl);
    let ty = &imp.self_ty;
    let all_methods: Vec<_> = get_methods(&imp).collect();

    let cfs = derive_contract_function_set(ty, all_methods.into_iter());
    quote! {
        #imp
        #cfs
    }
    .into()
}
