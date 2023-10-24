use itertools::MultiUnzip;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, Error, FnArg, Type};

pub fn derive_contract_function_set<'a>(
    ty: &Type,
    methods: impl Iterator<Item = &'a syn::ImplItemFn>,
) -> TokenStream2 {
    let mut errors = Vec::<Error>::new();

    let (str_lits, func_calls): (Vec<_>, Vec<_>) = methods
        .enumerate()
        .map(|(i, m)| {
            let ident = &m.sig.ident;
            let name = ident.to_string();
            let str_lit = Literal::string(&name);
            let (arg_indices, args, arg_types): (Vec<_>, Vec<_>, Vec<_>) = m.sig.inputs.iter().skip(1).cloned().enumerate().map(|(i, a)| {
                let arg = format_ident!("arg{}", i);
                match a {
                    FnArg::Typed(t) => (i, arg, t.ty),
                    _ => {
                        errors.push(Error::new(a.span(), "invalid argument type"));
                        (i, arg, syn::parse_quote! { () })
                    }
                }
            }).multiunzip();
            let num_args = args.len();
            let func_call = quote! {
                #i => {
                    if args.len() == #num_args {
                        #(let #args: #arg_types = args.get(#arg_indices).cloned().ok_or(soroban_env_common::ConversionError)?.try_into_val(host)?;)*
                        Ok(Self::#ident(host, #(#args,)*)?.try_into_val(host)?)
                    } else {
                        Err(host.err(crate::xdr::ScErrorType::Context, crate::xdr::ScErrorCode::UnexpectedSize, "wrong number of args to func", &[func.into()]))
                    }
                }
            };
            (str_lit, func_call)
        })
        .multiunzip();

    if !errors.is_empty() {
        let compile_errors = errors.iter().map(Error::to_compile_error);
        quote! { #(#compile_errors)* }
    } else {
        quote! {
            impl crate::builtin_contracts::BuiltinContract for #ty {
                fn call(
                    &self,
                    func: &soroban_env_common::Symbol,
                    host: &crate::Host,
                    args: &[soroban_env_common::Val],
                ) -> Result<soroban_env_common::Val, crate::HostError> {
                    use soroban_env_common::EnvBase;
                    use super::*;
                    const FNS: &'static [&'static str] = &[#(&#str_lits),*];
                    match u32::from(host.symbol_index_in_strs(*func, FNS)?) as usize {
                        #(#func_calls)*
                        _ => Err(host.err(crate::xdr::ScErrorType::Context, crate::xdr::ScErrorCode::MissingValue, "function does not exist", &[func.into()]))
                    }
                }
            }
        }
    }
}
