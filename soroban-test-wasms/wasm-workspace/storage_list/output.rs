warning: Patch `soroban-env-guest v21.0.0 (/Users/jay/Projects/rs-soroban-env/soroban-env-guest)` was not used in the crate graph.
Perhaps you misspelled the source URL being patched.
Possible URLs for `[patch.<URL>]`:
    crates-io
warning: Patch `soroban-env-host v21.0.0 (/Users/jay/Projects/rs-soroban-env/soroban-env-host)` was not used in the crate graph.
Perhaps you misspelled the source URL being patched.
Possible URLs for `[patch.<URL>]`:
    crates-io
warning: Patch `stellar-xdr v21.0.1 (/Users/jay/Projects/rs-stellar-xdr)` was not used in the crate graph.
Check that the patched package version and available features are compatible
with the dependency requirements. If the patch has a different version from
what is locked in the Cargo.lock file, run `cargo update` to use the new
version. This may also occur with an optional dependency that is not enabled.
    Checking storage_list v0.0.0 (/Users/jay/Projects/rs-soroban-env/soroban-test-wasms/wasm-workspace/storage_list)
Vec(ScSpecTypeVec { element_type: U32 })
Vec(ScSpecTypeVec { element_type: U32 })
Vec(ScSpecTypeVec { element_type: U32 })
    Finished dev [unoptimized + debuginfo] target(s) in 0.16s

#![feature(prelude_import)]
#![no_std]
#[prelude_import]
use core::prelude::rust_2021::*;
#[macro_use]
extern crate core;
extern crate compiler_builtins as _;
use soroban_sdk::{contract, contractimpl, contracttype, Env, Vec};
pub enum DataKey {
    List,
}
pub static __SPEC_XDR_TYPE_DATAKEY: [u8; 44usize] = DataKey::spec_xdr();
impl DataKey {
    pub const fn spec_xdr() -> [u8; 44usize] {
        *b"\x00\x00\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x07DataKey\x00\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x04List"
    }
}
impl soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val> for DataKey {
    type Error = soroban_sdk::ConversionError;
    #[inline(always)]
    fn try_from_val(
        env: &soroban_sdk::Env,
        val: &soroban_sdk::Val,
    ) -> Result<Self, soroban_sdk::ConversionError> {
        use soroban_sdk::{EnvBase, TryIntoVal, TryFromVal};
        const CASES: &'static [&'static str] = &["List"];
        let vec: soroban_sdk::Vec<soroban_sdk::Val> = val.try_into_val(env)?;
        let mut iter = vec.try_iter();
        let discriminant: soroban_sdk::Symbol = iter
            .next()
            .ok_or(soroban_sdk::ConversionError)??
            .try_into_val(env)
            .map_err(|_| soroban_sdk::ConversionError)?;
        Ok(
            match u32::from(
                env.symbol_index_in_strs(discriminant.to_symbol_val(), CASES)?,
            ) as usize
            {
                0 => {
                    if iter.len() > 0 {
                        return Err(soroban_sdk::ConversionError);
                    }
                    Self::List
                }
                _ => Err(soroban_sdk::ConversionError {})?,
            },
        )
    }
}
impl soroban_sdk::TryFromVal<soroban_sdk::Env, DataKey> for soroban_sdk::Val {
    type Error = soroban_sdk::ConversionError;
    #[inline(always)]
    fn try_from_val(
        env: &soroban_sdk::Env,
        val: &DataKey,
    ) -> Result<Self, soroban_sdk::ConversionError> {
        use soroban_sdk::{TryIntoVal, TryFromVal};
        match val {
            DataKey::List => {
                let tup: (soroban_sdk::Val,) = (
                    soroban_sdk::Symbol::try_from_val(env, &"List")?.to_val(),
                );
                tup.try_into_val(env).map_err(Into::into)
            }
        }
    }
}
#[automatically_derived]
impl ::core::clone::Clone for DataKey {
    #[inline]
    fn clone(&self) -> DataKey {
        DataKey::List
    }
}
#[automatically_derived]
impl ::core::fmt::Debug for DataKey {
    #[inline]
    fn fmt(&self, f: &mut ::core::fmt::Formatter) -> ::core::fmt::Result {
        ::core::fmt::Formatter::write_str(f, "List")
    }
}
pub struct Test;
///TestClient is a client for calling the contract defined in "Test".
pub struct TestClient<'a> {
    pub env: soroban_sdk::Env,
    pub address: soroban_sdk::Address,
    #[doc(hidden)]
    #[cfg(not(any(test, feature = "testutils")))]
    _phantom: core::marker::PhantomData<&'a ()>,
}
impl<'a> TestClient<'a> {
    pub fn new(env: &soroban_sdk::Env, address: &soroban_sdk::Address) -> Self {
        Self {
            env: env.clone(),
            address: address.clone(),
            #[cfg(not(any(test, feature = "testutils")))]
            _phantom: core::marker::PhantomData,
        }
    }
}
impl Test {
    pub fn set_list(env: Env, list: Vec<u32>) {
        env.storage().persistent().set(&DataKey::List, &list);
    }
    pub fn get_list(env: Env) -> Vec<u32> {
        env.storage().persistent().get(&DataKey::List).unwrap_or(Vec::new(&env))
    }
}
#[doc(hidden)]
#[allow(non_snake_case)]
pub static __SPEC_XDR_FN_SET_LIST: [u8; 48usize] = Test::spec_xdr_set_list();
impl Test {
    pub const fn spec_xdr_set_list() -> [u8; 48usize] {
        *b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08set_list\x00\x00\x00\x01\x00\x00\x00\x00\x00\x00\x00\x04list\x00\x00\x03\xea\x00\x00\x00\x04\x00\x00\x00\x00"
    }
}
#[doc(hidden)]
#[allow(non_snake_case)]
pub static __SPEC_XDR_FN_GET_LIST: [u8; 36usize] = Test::spec_xdr_get_list();
impl Test {
    pub const fn spec_xdr_get_list() -> [u8; 36usize] {
        *b"\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08get_list\x00\x00\x00\x00\x00\x00\x00\x01\x00\x00\x03\xea\x00\x00\x00\x04"
    }
}
impl<'a> TestClient<'a> {
    pub fn set_list(&self, list: &Vec<u32>) -> () {
        use core::ops::Not;
        use soroban_sdk::{IntoVal, FromVal};
        let res = self
            .env
            .invoke_contract(
                &self.address,
                &soroban_sdk::Symbol::new(&self.env, &"set_list"),
                ::soroban_sdk::Vec::from_array(&self.env, [list.into_val(&self.env)]),
            );
        res
    }
    pub fn try_set_list(
        &self,
        list: &Vec<u32>,
    ) -> Result<
        Result<
            (),
            <() as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error,
        >,
        Result<soroban_sdk::Error, soroban_sdk::InvokeError>,
    > {
        use soroban_sdk::{IntoVal, FromVal};
        let res = self
            .env
            .try_invoke_contract(
                &self.address,
                &soroban_sdk::Symbol::new(&self.env, &"set_list"),
                ::soroban_sdk::Vec::from_array(&self.env, [list.into_val(&self.env)]),
            );
        res
    }
    pub fn get_list(&self) -> Vec<u32> {
        use core::ops::Not;
        use soroban_sdk::{IntoVal, FromVal};
        let res = self
            .env
            .invoke_contract(
                &self.address,
                &soroban_sdk::Symbol::new(&self.env, &"get_list"),
                ::soroban_sdk::Vec::new(&self.env),
            );
        res
    }
    pub fn try_get_list(
        &self,
    ) -> Result<
        Result<
            Vec<u32>,
            <Vec<
                u32,
            > as soroban_sdk::TryFromVal<soroban_sdk::Env, soroban_sdk::Val>>::Error,
        >,
        Result<soroban_sdk::Error, soroban_sdk::InvokeError>,
    > {
        use soroban_sdk::{IntoVal, FromVal};
        let res = self
            .env
            .try_invoke_contract(
                &self.address,
                &soroban_sdk::Symbol::new(&self.env, &"get_list"),
                ::soroban_sdk::Vec::new(&self.env),
            );
        res
    }
}
#[doc(hidden)]
pub mod __set_list {
    use super::*;
    #[deprecated(note = "use `TestClient::new(&env, &contract_id).set_list` instead")]
    pub extern fn invoke_raw(
        env: soroban_sdk::Env,
        arg_0: soroban_sdk::Val,
    ) -> soroban_sdk::Val {
        <_ as soroban_sdk::IntoVal<
            soroban_sdk::Env,
            soroban_sdk::Val,
        >>::into_val(
            #[allow(deprecated)]
            &<super::Test>::set_list(
                env.clone(),
                <_ as soroban_sdk::unwrap::UnwrapOptimized>::unwrap_optimized(
                    <_ as soroban_sdk::TryFromValForContractFn<
                        soroban_sdk::Env,
                        soroban_sdk::Val,
                    >>::try_from_val_for_contract_fn(&env, &arg_0),
                ),
            ),
            &env,
        )
    }
    #[deprecated(note = "use `TestClient::new(&env, &contract_id).set_list` instead")]
    pub fn invoke_raw_slice(
        env: soroban_sdk::Env,
        args: &[soroban_sdk::Val],
    ) -> soroban_sdk::Val {
        #[allow(deprecated)] invoke_raw(env, args[0usize])
    }
    use super::*;
}
#[doc(hidden)]
pub mod __get_list {
    use super::*;
    #[deprecated(note = "use `TestClient::new(&env, &contract_id).get_list` instead")]
    pub extern fn invoke_raw(env: soroban_sdk::Env) -> soroban_sdk::Val {
        <_ as soroban_sdk::IntoVal<
            soroban_sdk::Env,
            soroban_sdk::Val,
        >>::into_val(#[allow(deprecated)] &<super::Test>::get_list(env.clone()), &env)
    }
    #[deprecated(note = "use `TestClient::new(&env, &contract_id).get_list` instead")]
    pub fn invoke_raw_slice(
        env: soroban_sdk::Env,
        args: &[soroban_sdk::Val],
    ) -> soroban_sdk::Val {
        #[allow(deprecated)] invoke_raw(env)
    }
    use super::*;
}
