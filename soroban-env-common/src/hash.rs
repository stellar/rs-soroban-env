//! These Hash impls exist strictly for use in soroban-env-host's tracing
//! subsystem. Since they do not "look through" to the environment they are
//! misleading and potentially dangerous to expose for general use.

use crate::{
    AddressObject, BytesObject, DurationObject, I128Object, I256Object, I64Object, MapObject,
    Object, StringObject, Symbol, SymbolObject, TimepointObject, U128Object, U256Object, U64Object,
    Val, VecObject,
};

impl core::hash::Hash for Val {
    fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
        self.get_payload().hash(state);
    }
}

macro_rules! impl_hash_for_wrapper {
    ($WRAPPER:ident) => {
        impl core::hash::Hash for $WRAPPER {
            fn hash<H: core::hash::Hasher>(&self, state: &mut H) {
                self.as_val().get_payload().hash(state);
            }
        }
    };
}

impl_hash_for_wrapper!(Object);
impl_hash_for_wrapper!(U64Object);
impl_hash_for_wrapper!(I64Object);
impl_hash_for_wrapper!(TimepointObject);
impl_hash_for_wrapper!(DurationObject);
impl_hash_for_wrapper!(U128Object);
impl_hash_for_wrapper!(I128Object);
impl_hash_for_wrapper!(U256Object);
impl_hash_for_wrapper!(I256Object);
impl_hash_for_wrapper!(BytesObject);
impl_hash_for_wrapper!(StringObject);
impl_hash_for_wrapper!(SymbolObject);
impl_hash_for_wrapper!(Symbol);
impl_hash_for_wrapper!(VecObject);
impl_hash_for_wrapper!(MapObject);
impl_hash_for_wrapper!(AddressObject);
