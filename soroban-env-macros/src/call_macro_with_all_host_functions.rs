use proc_macro2::{Literal, TokenStream};
use quote::{format_ident, quote};
use std::fs::File;
use syn::{Error, LitStr, Type};

use serde::{Deserialize, Serialize};

use crate::path;

pub fn generate(file_lit: LitStr) -> Result<TokenStream, Error> {
    let file_str = file_lit.value();
    let file_path = path::abs_from_rel_to_manifest(&file_str);
    let file = File::open(&file_path).map_err(|e| {
        Error::new(
            file_lit.span(),
            format!("error reading file '{}': {}", file_str, e),
        )
    })?;
    let modules: Vec<Module> = serde_json::from_reader(file).map_err(|e| {
        Error::new(
            file_lit.span(),
            format!("error parsing file '{}': {}", file_str, e),
        )
    })?;
    let modules = modules.iter().map(|m| {
        let name = format_ident!("{}", &m.name);
        let export = Literal::string(&m.export);
        let functions = m.functions.iter().map(|f| {
            let docs = f.docs.as_deref().unwrap_or_default();
            let export = &f.export;
            let name = format_ident!("{}", &f.name);
            let args = f.args.iter().map(|a| {
                let name = format_ident!("{}", &a.name);
                let r#type = format_ident!("{}", &a.r#type);
                let r#type = Type::Verbatim(quote! { #r#type });
                quote! { #name: #r#type }
            });
            let r#return = format_ident!("{}", &f.r#return);
            let r#return = Type::Verbatim(quote! { #r#return });
            quote! {
                #[doc = #docs]
                { #export, fn #name(#(#args),*) -> #r#return }
            }
        });
        quote! {
            mod #name #export {
                #(#functions)*
            }
        }
    });
    Ok(quote! {
        #[doc(hidden)]
        #[macro_export]
        macro_rules! _call_macro_with_all_host_functions {

            // The x-macro takes a single ident, the name of a macro to call ...
            {$macro_to_call_back:ident} => {

                // ... and just calls it back, passing a single large token-tree.
                $macro_to_call_back! {

                    // The token-tree we pass to the callback is a sequence of
                    // blocks that have the following structure:
                    //
                    //  mod $mod_id:ident $mod_str:literal {
                    //     ...
                    //     { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
                    //     ...
                    //  }
                    //
                    // Where the sub token-tree $args:tt is a normal parenthesized
                    // argument list of comma-separated arg:type pairs

                    #(#modules)*
                }
            };
        }
        pub use _call_macro_with_all_host_functions as call_macro_with_all_host_functions;
    })
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Module {
    pub name: String,
    pub export: String,
    pub functions: Vec<Function>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub export: String,
    pub name: String,
    pub args: Vec<Arg>,
    pub r#return: String,
    pub docs: Option<String>,
}

#[derive(Default, Debug, Clone, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Arg {
    pub name: String,
    pub r#type: String,
}
