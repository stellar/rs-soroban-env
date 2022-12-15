mod call_macro_with_all_host_functions;
mod path;

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, parse_macro_input, Ident, LitInt, LitStr, Token};

use stellar_xdr::{ScEnvMetaEntry, WriteXdr};

struct MetaInput {
    pub interface_version: u64,
}

impl Parse for MetaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(MetaInput {
            interface_version: {
                assert_eq!(input.parse::<Ident>()?, "interface_version");
                input.parse::<Token![:]>()?;
                let v = input.parse::<LitInt>()?.base10_parse()?;
                input.parse::<Token![,]>()?;
                v
            },
        })
    }
}

struct MetaConstsOutput {
    pub input: MetaInput,
}

impl MetaConstsOutput {
    pub fn to_meta_entries(&self) -> Vec<ScEnvMetaEntry> {
        vec![ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(
            self.input.interface_version,
        )]
    }
}

impl ToTokens for MetaConstsOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // Build params for expressing the interface version.
        let interface_version = self.input.interface_version;

        // Build params for expressing the meta xdr.
        let meta_xdr = self
            .to_meta_entries()
            .into_iter()
            .map(|entry| entry.to_xdr())
            .collect::<Result<Vec<Vec<u8>>, stellar_xdr::Error>>()
            .unwrap()
            .concat();
        let meta_xdr_len = meta_xdr.len();
        let meta_xdr_lit = proc_macro2::Literal::byte_string(meta_xdr.as_slice());

        // Output.
        tokens.extend(quote! {
            pub const INTERFACE_VERSION: u64 = #interface_version;
            pub const XDR: [u8; #meta_xdr_len] = *#meta_xdr_lit;
        });
    }
}

#[proc_macro]
pub fn generate_env_meta_consts(input: TokenStream) -> TokenStream {
    let meta_input = parse_macro_input!(input as MetaInput);
    let meta_consts_output = MetaConstsOutput { input: meta_input };
    quote! { #meta_consts_output }.into()
}

#[proc_macro]
pub fn generate_call_macro_with_all_host_functions(input: TokenStream) -> TokenStream {
    let file = parse_macro_input!(input as LitStr);
    match call_macro_with_all_host_functions::generate(file) {
        Ok(t) => t.into(),
        Err(e) => e.to_compile_error().into(),
    }
}
