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
                { const k: crate::Symbol = crate::Symbol::from_str(#name); k }
            };
            (ident, map_key)
        })
        .multiunzip();

    quote! {
        impl crate::Convert<crate::Object, #ident> for crate::Host {
            type Error = crate::HostError;
            fn convert_ref(&self, obj: &crate::Object) -> Result<#ident, crate::HostError> {
                let map: Map = self.convert_ref(obj)?;
                Ok(#ident {
                    #(#names: map.get(&#map_keys)?,)*
                })
            }
        }

        impl crate::Convert<crate::RawVal, #ident> for crate::Host {
            type Error = crate::HostError;
            fn convert_ref(&self, rv: &crate::RawVal) -> Result<#ident, crate::HostError> {
                let obj: crate::Object = rv.try_into()?;
                self.convert_ref(&obj)
            }
        }

        impl crate::Convert<#ident, crate::Object> for crate::Host {
            type Error = crate::HostError;
            fn convert_ref(&self, v: &#ident) -> Result<crate::Object, crate::HostError> {
                let mut map: Map = Map::new(self)?;
                #(map.set(&#map_keys, &v.#names)?;)*
                self.convert(map)
            }
        }

        impl crate::Convert<#ident, crate::RawVal> for crate::Host {
            type Error = crate::HostError;
            fn convert_ref(&self, v: &#ident) -> Result<crate::RawVal, crate::HostError> {
                let obj: crate::Object = self.convert_ref(v)?;
                Ok(obj.into())
            }
        }
    }
}

pub fn derive_type_enum(ty_ident: &Ident, data: &DataEnum) -> TokenStream2 {
    let mut errors = Vec::<Error>::new();

    let (consts, froms, intos): (Vec<_>, Vec<_>, Vec<_>) = data.variants
        .iter()
        .map(|f| {
            let case_ident = &f.ident;
            let case_name = case_ident.to_string();
            let discriminant_ident = format_ident!("DISCRIMINANT_{}", case_name.to_uppercase());
            let discriminant_const = quote! {
                const #discriminant_ident: u64 = crate::Symbol::from_str(#case_name).to_raw().get_payload()
            };

            if f.fields.is_empty() {
                let from = quote! {
                    #discriminant_ident => Ok(#ty_ident::#case_ident)
                };
                let into = quote! {
                    #ty_ident::#case_ident => {
                        vec.push(&{ const k: crate::Symbol = crate::Symbol::from_str(#case_name); k })?;
                    }
                };
                (discriminant_const, from, into)
            } else if f.fields.len() == 1 {
                let from = quote! {
                    #discriminant_ident => Ok(#ty_ident::#case_ident(vec.get(1)?))
                };
                let into = quote! {
                    #ty_ident::#case_ident(x) => {
                        vec.push(&{ const k: crate::Symbol = crate::Symbol::from_str(#case_name); k })?;
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

            impl crate::Convert<crate::Object, #ty_ident> for crate::Host {
                type Error = crate::HostError;
                fn convert_ref(&self, obj: &crate::Object) -> Result<#ty_ident, Self::Error> {
                    #(#consts;)*
                    let vec: crate::native_contract::base_types::Vec = self.convert_ref(obj)?;
                    let discriminant: crate::Symbol = vec.get(0)?;
                    match discriminant.to_raw().get_payload() {
                        #(#froms,)*
                        _ => Err(crate::ConversionError.into())
                    }
                }
            }

            impl crate::Convert<crate::RawVal, #ty_ident> for crate::Host {
                type Error = crate::HostError;
                fn convert_ref(&self, val: &crate::RawVal) -> Result<#ty_ident, Self::Error> {
                    let obj: crate::Object = val.try_into()?;
                    self.convert_ref(&obj)
                }
            }

            impl crate::Convert<#ty_ident, crate::Object> for crate::Host {
                type Error = crate::HostError;
                fn convert_ref(&self, obj: &#ty_ident) -> Result<crate::Object, Self::Error> {
                    #(#consts;)*
                    let mut vec = crate::native_contract::base_types::Vec::new(self)?;
                    match obj {
                        #(#intos)*
                    };
                    self.convert_ref(&vec)
                }
            }

            impl crate::Convert<#ty_ident, crate::RawVal> for crate::Host {
                type Error = crate::HostError;
                fn convert_ref(&self, v: &#ty_ident) -> Result<crate::RawVal, Self::Error> {
                    let obj: crate::Object = self.convert_ref(v)?;
                    Ok(obj.into())
                }
            }
        }
    }
}
