use itertools::Itertools;
use proc_macro2::{Literal, TokenStream as TokenStream2};
use quote::{format_ident, quote};
use syn::{spanned::Spanned, DataEnum, DataStruct, Error, Fields, Ident};

pub fn derive_type_struct(ident: &Ident, data: &DataStruct) -> TokenStream2 {
    let len = Literal::usize_unsuffixed(data.fields.len());

    let (idents, str_lits, idx_lits): (Vec<_>, Vec<_>, Vec<_>) =
        if let Fields::Named(_) = &data.fields {
            data.fields
                .iter()
                .sorted_by_key(|field| field.ident.as_ref().unwrap().to_string())
                .enumerate()
                .map(|(i, f)| {
                    let ident = f.ident.as_ref().unwrap().clone();
                    let str_lit = Literal::string(&ident.to_string());
                    let idx_lit = Literal::usize_unsuffixed(i);
                    (ident, str_lit, idx_lit)
                })
                .multiunzip()
        } else {
            data.fields
                .iter()
                .enumerate()
                .map(|(i, _)| {
                    let ident = format_ident!("{}", i);
                    let str_lit = Literal::string(&ident.to_string());
                    let idx_lit = Literal::usize_unsuffixed(i);
                    (ident, str_lit, idx_lit)
                })
                .multiunzip()
        };

    quote! {

        impl soroban_env_common::Compare<#ident> for crate::Host {
            type Error = crate::HostError;
            fn compare(&self, a: &#ident, b: &#ident) -> Result<core::cmp::Ordering, crate::HostError> {
                #(match self.compare(&a.#idents, &b.#idents)? {
                    core::cmp::Ordering::Equal => (),
                    unequal => return Ok(unequal)
                })*
                Ok(core::cmp::Ordering::Equal)
            }
        }

        impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::MapObject> for #ident {
            type Error = crate::HostError;

            fn try_from_val(env: &crate::Host, val: &soroban_env_common::MapObject) -> Result<Self, Self::Error> {
                use soroban_env_common::EnvBase;
                const KEYS: [&'static str; #len] = [#(#str_lits),*];
                let mut vals: [soroban_env_common::Val; #len] = [soroban_env_common::Val::VOID.to_val(); #len];
                env.map_unpack_to_slice(*val, &KEYS, &mut vals)?;
                Ok(Self {
                    #(#idents: vals[#idx_lits].try_into_val(env)?,)*
                })
            }
        }

        impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::Val> for #ident {
            type Error = crate::HostError;

            fn try_from_val(env: &crate::Host, val: &soroban_env_common::Val) -> Result<Self, Self::Error> {
                let obj: soroban_env_common::MapObject = val.try_into()?;
                obj.try_into_val(env)
            }
        }

        impl soroban_env_common::TryFromVal<crate::Host, #ident> for soroban_env_common::Val {
            type Error = crate::HostError;

            fn try_from_val(env: &crate::Host, val: &#ident) -> Result<Self, Self::Error> {
                use soroban_env_common::EnvBase;
                const KEYS: [&'static str; #len] = [#(#str_lits),*];
                let vals: [soroban_env_common::Val; #len] = [
                    #(val.#idents.try_into_val(env)?),*
                ];
                Ok(env.map_new_from_slices(&KEYS, &vals)?.into())
            }
        }
    }
}

pub fn derive_type_enum(ident: &Ident, data: &DataEnum) -> TokenStream2 {
    let mut errors = Vec::<Error>::new();

    let (str_lits, froms, intos, syms, compares): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = data
        .variants
        .iter()
        .enumerate()
        .map(|(i, f)| {
            let case_ident = &f.ident;
            let case_name = case_ident.to_string();
            let idx_lit = Literal::usize_unsuffixed(i);
            let str_lit = Literal::string(&case_name);
            let case_sym = quote! { crate::Symbol::try_from_val(env, &#str_lit) };

            if f.fields.is_empty() {
                let from = quote! {
                    #idx_lit => Ok(Self::#case_ident)
                };
                let into = quote! {
                    #ident::#case_ident => {
                        Ok((#case_sym?,).try_into_val(env)?)
                    }
                };
                let sym = quote! {
                    #ident::#case_ident => #case_sym
                };
                let compare = quote! {
                    (#ident::#case_ident, #ident::#case_ident) => Ok(core::cmp::Ordering::Equal)
                };
                (str_lit, from, into, sym, compare)
            } else if f.fields.len() == 1 {
                let from = quote! {
                    #idx_lit => Ok(Self::#case_ident(vec.get(1)?))
                };
                let into = quote! {
                    #ident::#case_ident(x) => {
                        Ok((#case_sym?, crate::Val::try_from_val(env, x)?).try_into_val(env)?)
                    }
                };
                let sym = quote! {
                    #ident::#case_ident(_) => #case_sym
                };
                let compare = quote! {
                    (#ident::#case_ident(a), #ident::#case_ident(b)) => self.compare(a, b)
                };
                (str_lit, from, into, sym, compare)
            } else {
                errors.push(Error::new(
                    f.span(),
                    "tuple variants with more than 1 element not supported",
                ));
                let from = quote! {};
                let into = quote! {};
                let cmp = quote! {};
                let sym = quote! {};
                (str_lit, from, into, cmp, sym)
            }
        })
        .multiunzip();

    if !errors.is_empty() {
        let compile_errors = errors.iter().map(Error::to_compile_error);
        quote! { #(#compile_errors)* }
    } else {
        quote! {

            impl #ident {
                fn discriminant_sym(&self, env: &crate::Host) -> Result<crate::Symbol, crate::Error> {
                    use soroban_env_common::TryFromVal;
                    match self {
                        #(#syms,)*
                    }
                }
            }

            impl soroban_env_common::Compare<#ident> for crate::Host {
                type Error = crate::HostError;
                fn compare(&self, a: &#ident, b: &#ident) -> Result<core::cmp::Ordering, crate::HostError> {
                    match (a, b) {
                        #(#compares,)*
                        _ => self.compare(&a.discriminant_sym(self)?, &b.discriminant_sym(self)?)
                    }
                }
            }

            impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::VecObject> for #ident {
                type Error = crate::HostError;

                fn try_from_val(env: &crate::Host, val: &soroban_env_common::VecObject) -> Result<Self, Self::Error> {
                    use soroban_env_common::{EnvBase,TryFromVal};
                    const CASES: &'static [&'static str] = &[#(#str_lits),*];
                    let vec = crate::builtin_contracts::base_types::Vec::try_from_val(env, val)?;
                    let discriminant: soroban_env_common::Symbol = vec.get(0)?;
                    match u32::from(env.symbol_index_in_strs(discriminant, CASES)?) as usize {
                        #(#froms,)*
                        _ => Err(soroban_env_common::ConversionError.into())
                    }
                }
            }

            impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::Val> for #ident {
                type Error = crate::HostError;

                fn try_from_val(env: &crate::Host, val: &soroban_env_common::Val) -> Result<Self, Self::Error> {
                    let obj: soroban_env_common::VecObject = val.try_into()?;
                    obj.try_into_val(env)
                }
            }

            impl soroban_env_common::TryFromVal<crate::Host, #ident> for soroban_env_common::Val {
                type Error = crate::HostError;

                fn try_from_val(env: &crate::Host, val: &#ident) -> Result<soroban_env_common::Val, Self::Error> {
                    use soroban_env_common::TryFromVal;
                    match val {
                        #(#intos)*
                    }
                }
            }
        }
    }
}
