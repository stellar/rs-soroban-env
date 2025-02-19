mod call_macro_with_all_host_functions;
mod path;
mod synth_dispatch_host_fn_tests;
mod synth_linear_memory_tests;
mod synth_wasm_expr_type;
use serde::{Deserialize, Serialize};

extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse::Parse, parse_macro_input, Ident, LitInt, LitStr, Token};

// Import the XDR definitions of a specific version -- curr or next -- of the xdr crate.
#[cfg(not(feature = "next"))]
use stellar_xdr::next as xdr;
#[cfg(feature = "next")]
use stellar_xdr::next as xdr;

use crate::xdr::{Limits, ScEnvMetaEntry, ScEnvMetaEntryInterfaceVersion, WriteXdr};

// We need the protocol version for some tests generated by this crate.
// Unfortunately it is not available at this layer and can't read from
// `meta.rs`, since this is at the lower layer (`meta.rs` is compile-time
// generated by routines here)
#[cfg(not(feature = "next"))]
pub(crate) const LEDGER_PROTOCOL_VERSION: u32 = 23;
#[cfg(feature = "next")]
pub(crate) const LEDGER_PROTOCOL_VERSION: u32 = 24;

struct MetaInput {
    pub interface_version: ScEnvMetaEntryInterfaceVersion,
}

impl Parse for MetaInput {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(MetaInput {
            interface_version: {
                assert_eq!(input.parse::<Ident>()?, "ledger_protocol_version");
                input.parse::<Token![:]>()?;
                let proto: u32 = input.parse::<LitInt>()?.base10_parse()?;
                input.parse::<Token![,]>()?;
                assert_eq!(input.parse::<Ident>()?, "pre_release_version");
                input.parse::<Token![:]>()?;
                let pre: u32 = input.parse::<LitInt>()?.base10_parse()?;
                input.parse::<Token![,]>()?;
                assert_eq!(proto, LEDGER_PROTOCOL_VERSION);
                ScEnvMetaEntryInterfaceVersion {
                    protocol: proto,
                    pre_release: pre,
                }
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
            self.input.interface_version.clone(),
        )]
    }
}

impl ToTokens for MetaConstsOutput {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        // Build params for expressing the interface version.
        let proto = &self.input.interface_version.protocol;
        let pre = &self.input.interface_version.pre_release;

        // Build params for expressing the meta xdr.
        let meta_xdr = self
            .to_meta_entries()
            .into_iter()
            // Limits::none here is okay since `MetaConstsOutput` is controled by us
            .map(|entry| entry.to_xdr(Limits::none()))
            .collect::<Result<Vec<Vec<u8>>, crate::xdr::Error>>()
            .unwrap()
            .concat();
        let meta_xdr_len = meta_xdr.len();
        let meta_xdr_lit = proc_macro2::Literal::byte_string(meta_xdr.as_slice());

        // Output.
        tokens.extend(quote! {
            pub const INTERFACE_VERSION: ScEnvMetaEntryInterfaceVersion = ScEnvMetaEntryInterfaceVersion{
                protocol: #proto,
                pre_release: #pre,
            };
            pub const XDR: [u8; #meta_xdr_len] = *#meta_xdr_lit;
        });
    }
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct Root {
    pub(crate) modules: Vec<Module>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct Module {
    pub(crate) name: String,
    pub(crate) export: String,
    pub(crate) functions: Vec<Function>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct Function {
    pub(crate) export: String,
    pub(crate) name: String,
    pub(crate) args: Vec<Arg>,
    pub(crate) r#return: String,
    pub(crate) docs: Option<String>,
    pub(crate) min_supported_protocol: Option<u32>,
    pub(crate) max_supported_protocol: Option<u32>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Arg {
    pub(crate) name: String,
    pub(crate) r#type: String,
}

fn load_env_file(file_lit: LitStr) -> Result<Root, syn::Error> {
    let file_str = file_lit.value();
    let file_path = path::abs_from_rel_to_manifest(&file_str);

    let file = std::fs::File::open(file_path).map_err(|e| {
        syn::Error::new(
            file_lit.span(),
            format!("error reading file '{file_str}': {e}"),
        )
    })?;

    serde_json::from_reader(file).map_err(|e| {
        syn::Error::new(
            file_lit.span(),
            format!("error parsing file '{file_str}': {e}"),
        )
    })
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

#[proc_macro]
pub fn generate_synth_wasm_expr_type(input: TokenStream) -> TokenStream {
    let file = parse_macro_input!(input as LitStr);
    match synth_wasm_expr_type::generate(file) {
        Ok(t) => t.into(),
        Err(e) => e.to_compile_error().into(),
    }
}

#[proc_macro]
pub fn generate_synth_dispatch_host_fn_tests(input: TokenStream) -> TokenStream {
    let file = parse_macro_input!(input as LitStr);
    let mut impls: TokenStream = TokenStream::new();
    let wasms: TokenStream =
        match synth_dispatch_host_fn_tests::generate_wasm_module_calling_host_functions(
            file.clone(),
        ) {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    let dispatch_wrong_types: TokenStream =
        match synth_dispatch_host_fn_tests::generate_hostfn_call_with_wrong_types(file.clone()) {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    let dispatch_invalid_obj_handles: TokenStream =
        match synth_dispatch_host_fn_tests::generate_hostfn_call_with_invalid_obj_handles(file) {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    impls.extend(wasms);
    impls.extend(dispatch_wrong_types);
    impls.extend(dispatch_invalid_obj_handles);
    impls
}

#[proc_macro]
pub fn generate_linear_memory_host_fn_tests(input: TokenStream) -> TokenStream {
    let file = parse_macro_input!(input as LitStr);
    let mut impls: TokenStream = TokenStream::new();
    let wasms: TokenStream =
        match synth_linear_memory_tests::generate_wasm_module_with_preloaded_linear_memory(file) {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    let testset1: TokenStream =
        match synth_linear_memory_tests::generate_tests_for_malformed_key_slices() {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    let testset2: TokenStream =
        match synth_linear_memory_tests::generate_tests_for_malformed_val_data() {
            Ok(t) => t.into(),
            Err(e) => e.to_compile_error().into(),
        };
    let testset3: TokenStream = match synth_linear_memory_tests::generate_tests_for_bytes() {
        Ok(t) => t.into(),
        Err(e) => e.to_compile_error().into(),
    };
    impls.extend(wasms);
    impls.extend(testset1);
    impls.extend(testset2);
    impls.extend(testset3);
    impls
}
