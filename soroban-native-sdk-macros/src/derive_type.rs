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
        impl TryFrom<soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object>> for #ident {
            type Error = crate::HostError;

            fn try_from(value: soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object>) -> Result<Self, Self::Error> {
                let map = Map::try_from(value)?;
                Ok(Self {
                    #(#names: map.get(#map_keys)?,)*
                })
            }
        }

        impl TryFrom<soroban_env_common::EnvVal<crate::Host, soroban_env_common::RawVal>> for #ident {
            type Error = crate::HostError;

            fn try_from(value: soroban_env_common::EnvVal<crate::Host, soroban_env_common::RawVal>) -> Result<Self, Self::Error> {
                let env_obj: soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object> = value.try_into()?;
                env_obj.try_into()
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
    }
}

pub fn derive_type_enum(ident: &Ident, data: &DataEnum) -> TokenStream2 {
    let mut errors = Vec::<Error>::new();

    let (consts, froms, intos): (Vec<_>, Vec<_>, Vec<_>) = data.variants
        .iter()
        .map(|f| {
            let ident = &f.ident;
            let name = ident.to_string();
            let discriminant_ident = format_ident!("DISCRIMINANT_{}", name.to_uppercase());
            let discriminant_const = quote! {
                const #discriminant_ident: u64 = soroban_env_common::Symbol::from_str(#name).to_raw().get_payload()
            };

            if f.fields.is_empty() {
                let from = quote! {
                    #discriminant_ident => Ok(Self::#ident)
                };
                let into = quote! {
                    Self::#ident => {
                        vec.push({ const k: soroban_env_common::Symbol = soroban_env_common::Symbol::from_str(#name); k })?;
                    }
                };
                (discriminant_const, from, into)
            } else if f.fields.len() == 1 {
                let from = quote! {
                    #discriminant_ident => Ok(Self::#ident(vec.get(1)?))
                };
                let into = quote! {
                    Self::#ident(x) => {
                        vec.push({ const k: soroban_env_common::Symbol = soroban_env_common::Symbol::from_str(#name); k })?;
                        vec.push(x)?;
                    }
                };
                (discriminant_const, from, into)
            } else {
                errors.push(Error::new(f.span(), "tuple variants with more than 1 element not supported"));
                let from = quote! { };
                let into = quote! { };
                (discriminant_const, from, into)
            }
        })
        .multiunzip();

    if !errors.is_empty() {
        let compile_errors = errors.iter().map(Error::to_compile_error);
        quote! { #(#compile_errors)* }
    } else {
        quote! {
            impl TryFrom<soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object>> for #ident {
                type Error = crate::HostError;

                fn try_from(value: soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object>) -> Result<Self, Self::Error> {
                    #(#consts;)*
                    let vec = crate::native_contract::base_types::Vec::try_from(value)?;
                    let discriminant: soroban_env_common::Symbol = vec.get(0)?;
                    match discriminant.to_raw().get_payload() {
                        #(#froms,)*
                        _ => Err(soroban_env_common::ConversionError.into())
                    }
                }
            }

            impl TryFrom<soroban_env_common::EnvVal<crate::Host, soroban_env_common::RawVal>> for #ident {
                type Error = crate::HostError;

                fn try_from(value: soroban_env_common::EnvVal<crate::Host, soroban_env_common::RawVal>) -> Result<Self, Self::Error> {
                    let env_obj: soroban_env_common::EnvVal<crate::Host, soroban_env_common::Object> = value.try_into()?;
                    env_obj.try_into()
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
        }
    }
}
