use itertools::MultiUnzip;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{spanned::Spanned, DataEnum, DataStruct, Error, Ident, Visibility};

pub fn derive_type_struct(ident: &Ident, data: &DataStruct) -> TokenStream2 {
    let (names, map_keys): (Vec<_>, Vec<_>) = data.fields
        .iter()
        .filter(|f| matches!(f.vis, Visibility::Public(_)))
        .enumerate()
        .map(|(i, f)| {
            let ident = f
                .ident
                .as_ref()
                .map_or_else(|| format_ident!("{}", i), Ident::clone);
            let name = ident.to_string();
            let map_key = quote! { // TODO: Handle field names longer than a symbol. Hash the name? Truncate the name?
                { const k: soroban_env_common::Symbol = soroban_env_common::Symbol::from_str(#name); k }
            };
            (ident, map_key)
        })
        .multiunzip();

    quote! {

        impl soroban_env_common::Compare<#ident> for crate::Host {
            type Error = crate::HostError;
            fn compare(&self, a: &#ident, b: &#ident) -> Result<core::cmp::Ordering, crate::HostError> {
                #(match self.compare(&a.#names, &b.#names)? {
                    core::cmp::Ordering::Equal => (),
                    unequal => return Ok(unequal)
                })*
                Ok(core::cmp::Ordering::Equal)
            }
        }

        impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::Object> for #ident {
            type Error = crate::HostError;

            fn try_from_val(env: &crate::Host, val: soroban_env_common::Object) -> Result<Self, Self::Error> {
                let map = Map::try_from_val(env, val)?;
                Ok(Self {
                    #(#names: map.get(#map_keys)?,)*
                })
            }
        }

        impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::RawVal> for #ident {
            type Error = crate::HostError;

            fn try_from_val(env: &crate::Host, val: soroban_env_common::RawVal) -> Result<Self, Self::Error> {
                <_ as soroban_env_common::TryFromVal<_, soroban_env_common::Object>>::try_from_val(env, val.try_into()?)
            }
        }

        impl TryIntoVal<crate::Host, soroban_env_common::RawVal> for #ident {
            type Error = crate::HostError;

            fn try_into_val(self, env: &crate::Host) -> Result<soroban_env_common::RawVal, Self::Error> {
                let mut map = Map::new(env)?;
                #(map.set(#map_keys, self.#names)?;)*
                map.try_into_val(env)
            }
        }

        impl TryIntoVal<crate::Host, #ident> for soroban_env_common::RawVal {
            type Error = crate::HostError;

            fn try_into_val(self, env: &crate::Host) -> Result<#ident, Self::Error> {
                <_ as soroban_env_common::TryFromVal<_, _>>::try_from_val(env, self)
            }
        }
    }
}

pub fn derive_type_enum(ident: &Ident, data: &DataEnum) -> TokenStream2 {
    let mut errors = Vec::<Error>::new();

    let (consts, froms, intos, syms, compares): (Vec<_>, Vec<_>, Vec<_>, Vec<_>, Vec<_>) = data.variants
        .iter()
        .map(|f| {
            let case_ident = &f.ident;
            let case_name = case_ident.to_string();
            let discriminant_ident = format_ident!("DISCRIMINANT_{}", case_name.to_uppercase());
            let discriminant_sym = quote! { crate::Symbol::from_str(#case_name) };
            let discriminant_const = quote! {
                const #discriminant_ident: u64 = #discriminant_sym.to_raw().get_payload()
            };

            if f.fields.is_empty() {
                let from = quote! {
                    #discriminant_ident => Ok(Self::#case_ident)
                };
                let into = quote! {
                    Self::#case_ident => {
                        vec.push({ const k: crate::Symbol = crate::Symbol::from_str(#case_name); k })?;
                    }
                };
                let sym = quote! {
                    #ident::#case_ident => #discriminant_sym
                };
                let compare = quote! {
                    (#ident::#case_ident, #ident::#case_ident) => Ok(core::cmp::Ordering::Equal)
                };
                (discriminant_const, from, into, sym, compare)
            } else if f.fields.len() == 1 {
                let from = quote! {
                    #discriminant_ident => Ok(Self::#case_ident(vec.get(1)?))
                };
                let into = quote! {
                    Self::#case_ident(x) => {
                        vec.push({ const k: crate::Symbol = crate::Symbol::from_str(#case_name); k })?;
                        vec.push(x)?;
                    }
                };
                let sym = quote! {
                    #ident::#case_ident(_) => #discriminant_sym
                };
                let compare = quote! {
                    (#ident::#case_ident(a), #ident::#case_ident(b)) => self.compare(a, b)
                };
                (discriminant_const, from, into, sym, compare)
            } else {
                errors.push(Error::new(f.span(), "tuple variants with more than 1 element not supported"));
                let from = quote! { };
                let into = quote! { };
                let cmp = quote! { };
                let sym = quote! { };
                (discriminant_const, from, into, cmp, sym)
            }
        })
        .multiunzip();

    if !errors.is_empty() {
        let compile_errors = errors.iter().map(Error::to_compile_error);
        quote! { #(#compile_errors)* }
    } else {
        quote! {

            impl #ident {
                fn discriminant_sym(&self) -> crate::Symbol {
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
                        _ => self.compare(&a.discriminant_sym(), &b.discriminant_sym())
                    }
                }
            }

            impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::Object> for #ident {
                type Error = crate::HostError;

                fn try_from_val(env: &crate::Host, val: soroban_env_common::Object) -> Result<Self, Self::Error> {
                    #(#consts;)*
                    let vec = crate::native_contract::base_types::Vec::try_from_val(env, val)?;
                    let discriminant: soroban_env_common::Symbol = vec.get(0)?;
                    match discriminant.to_raw().get_payload() {
                        #(#froms,)*
                        _ => Err(soroban_env_common::ConversionError.into())
                    }
                }
            }

            impl soroban_env_common::TryFromVal<crate::Host, soroban_env_common::RawVal> for #ident {
                type Error = crate::HostError;

                fn try_from_val(env: &crate::Host, val: soroban_env_common::RawVal) -> Result<Self, Self::Error> {
                    <_ as soroban_env_common::TryFromVal<_, soroban_env_common::Object>>::try_from_val(env, val.try_into()?)
                }
            }

            impl TryIntoVal<crate::Host, soroban_env_common::RawVal> for #ident {
                type Error = crate::HostError;

                fn try_into_val(self, env: &crate::Host) -> Result<soroban_env_common::RawVal, Self::Error> {
                    #(#consts;)*
                    let mut vec = crate::native_contract::base_types::Vec::new(env)?;
                    match self {
                        #(#intos)*
                    };
                    vec.try_into_val(env)
                }
            }

            impl TryIntoVal<crate::Host, #ident> for soroban_env_common::RawVal {
                type Error = crate::HostError;

                fn try_into_val(self, env: &crate::Host) -> Result<#ident, Self::Error> {
                    <_ as soroban_env_common::TryFromVal<_, _>>::try_from_val(env, self)
                }
            }
        }
    }
}
