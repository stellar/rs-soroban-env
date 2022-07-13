extern crate proc_macro;

use proc_macro::TokenStream;
use quote::quote;
use syn::{
    parse_macro_input, LitInt,
};

use stellar_xdr::{ScEnvMetaEntry, WriteXdr};

#[proc_macro]
pub fn contract_env_meta(input: TokenStream) -> TokenStream {
    let version = parse_macro_input!(input as LitInt);
    let meta = ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(version.base10_parse().unwrap());
    let meta_xdr = meta.to_xdr().unwrap();
    let meta_xdr_lit = proc_macro2::Literal::byte_string(meta_xdr.as_slice());
    quote! { *#meta_xdr_lit } .into()
}
