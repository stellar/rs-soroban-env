use soroban_synth_wasm::{Arity, LocalRef, ModEmitter, Operand};

use crate::{
    budget::AsBudget,
    host_object::{HostMap, HostVec},
    xdr::{
        Duration, Hash, ScAddress, ScBytes, ScErrorCode, ScErrorType, ScString, ScSymbol, TimePoint,
    },
    AddressObject, Bool, BytesObject, DurationObject, DurationSmall, DurationVal, Env, Error, Host,
    HostError, I128Object, I128Small, I128Val, I256Object, I256Small, I256Val, I32Val, I64Object,
    I64Small, MapObject, StorageType, StringObject, Symbol, SymbolObject, SymbolSmall,
    TimepointObject, TimepointSmall, TimepointVal, U128Object, U128Small, U128Val, U256Object,
    U256Small, U256Val, U32Val, U64Object, U64Small, U64Val, Val, VecObject, Void, I256, U256,
};

use soroban_env_macros::generate_synth_dispatch_host_fn_tests;

trait TestVal {
    fn test_val() -> Val;
}

trait TestValWithInitialValue {
    fn test_val_with_initial_value(i: i64) -> Val;
}

trait TestObject {
    fn test_object(host: &Host) -> Self;
}

trait TestObjectWithInitialLength {
    fn test_object_with_initial_length(host: &Host, len: u32) -> Self;
}

impl TestVal for Bool {
    fn test_val() -> Val {
        Bool::from(false).to_val()
    }
}
impl TestVal for Void {
    fn test_val() -> Val {
        Val::from_void().to_val()
    }
}
impl TestVal for Error {
    fn test_val() -> Val {
        Error::from_type_and_code(ScErrorType::Context, ScErrorCode::ExceededLimit).to_val()
    }
}
impl TestVal for U32Val {
    fn test_val() -> Val {
        Val::from_u32(123).to_val()
    }
}

impl TestValWithInitialValue for U32Val {
    fn test_val_with_initial_value(i: i64) -> Val {
        Val::from_u32(i as u32).to_val()
    }
}

impl TestVal for I32Val {
    fn test_val() -> Val {
        Val::from_i32(-123).to_val()
    }
}
impl TestVal for U64Small {
    fn test_val() -> Val {
        U64Small::from_u32(123).to_val()
    }
}
impl TestVal for I64Small {
    fn test_val() -> Val {
        I64Small::from_i32(-123).to_val()
    }
}
impl TestVal for TimepointSmall {
    fn test_val() -> Val {
        TimepointSmall::try_from(123u64).unwrap().to_val()
    }
}
impl TestVal for DurationSmall {
    fn test_val() -> Val {
        DurationSmall::try_from(123u64).unwrap().to_val()
    }
}
impl TestVal for U128Small {
    fn test_val() -> Val {
        U128Small::from_u32(123).to_val()
    }
}
impl TestVal for I128Small {
    fn test_val() -> Val {
        I128Small::from_i32(-123).to_val()
    }
}
impl TestVal for U256Small {
    fn test_val() -> Val {
        U256Small::from_u32(123).to_val()
    }
}
impl TestVal for I256Small {
    fn test_val() -> Val {
        I256Small::from_i32(-123).to_val()
    }
}
impl TestVal for SymbolSmall {
    fn test_val() -> Val {
        SymbolSmall::try_from_str("abc").unwrap().to_val()
    }
}
impl TestVal for U64Object {
    fn test_val() -> Val {
        unsafe { U64Object::from_handle(123).to_val() }
    }
}
impl TestVal for I64Object {
    fn test_val() -> Val {
        unsafe { I64Object::from_handle(123).to_val() }
    }
}
impl TestVal for TimepointObject {
    fn test_val() -> Val {
        unsafe { TimepointObject::from_handle(123).to_val() }
    }
}
impl TestVal for DurationObject {
    fn test_val() -> Val {
        unsafe { DurationObject::from_handle(123).to_val() }
    }
}
impl TestVal for U128Object {
    fn test_val() -> Val {
        unsafe { U128Object::from_handle(123).to_val() }
    }
}
impl TestVal for I128Object {
    fn test_val() -> Val {
        unsafe { I128Object::from_handle(123).to_val() }
    }
}
impl TestVal for U256Object {
    fn test_val() -> Val {
        unsafe { U256Object::from_handle(123).to_val() }
    }
}
impl TestVal for I256Object {
    fn test_val() -> Val {
        unsafe { I256Object::from_handle(123).to_val() }
    }
}
impl TestVal for BytesObject {
    fn test_val() -> Val {
        unsafe { BytesObject::from_handle(123).to_val() }
    }
}
impl TestVal for StringObject {
    fn test_val() -> Val {
        unsafe { StringObject::from_handle(123).to_val() }
    }
}
impl TestVal for SymbolObject {
    fn test_val() -> Val {
        unsafe { SymbolObject::from_handle(123).to_val() }
    }
}
impl TestVal for VecObject {
    fn test_val() -> Val {
        unsafe { VecObject::from_handle(123).to_val() }
    }
}
impl TestVal for MapObject {
    fn test_val() -> Val {
        unsafe { MapObject::from_handle(123).to_val() }
    }
}
impl TestVal for AddressObject {
    fn test_val() -> Val {
        unsafe { AddressObject::from_handle(123).to_val() }
    }
}

impl TestVal for Symbol {
    fn test_val() -> Val {
        SymbolSmall::test_val()
    }
}
impl TestVal for U64Val {
    fn test_val() -> Val {
        U64Small::test_val()
    }
}
impl TestVal for U128Val {
    fn test_val() -> Val {
        U128Small::test_val()
    }
}
impl TestVal for I128Val {
    fn test_val() -> Val {
        I128Small::test_val()
    }
}
impl TestVal for U256Val {
    fn test_val() -> Val {
        U256Small::test_val()
    }
}
impl TestVal for I256Val {
    fn test_val() -> Val {
        I256Small::test_val()
    }
}
impl TestVal for DurationVal {
    fn test_val() -> Val {
        DurationSmall::test_val()
    }
}
impl TestVal for TimepointVal {
    fn test_val() -> Val {
        TimepointSmall::test_val()
    }
}

impl TestVal for Val {
    fn test_val() -> Val {
        Void::test_val()
    }
}

impl TestVal for StorageType {
    fn test_val() -> Val {
        Val::from_u32(StorageType::Persistent as u32).to_val()
    }
}

impl TestObject for U64Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(u64::MAX).unwrap()
    }
}
impl TestObject for I64Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(i64::MAX).unwrap()
    }
}
impl TestObject for TimepointObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(TimePoint(u64::MAX)).unwrap()
    }
}
impl TestObject for DurationObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(Duration(u64::MAX)).unwrap()
    }
}
impl TestObject for U128Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(u128::MAX).unwrap()
    }
}
impl TestObject for I128Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(i128::MAX).unwrap()
    }
}
impl TestObject for U256Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(U256::MAX).unwrap()
    }
}
impl TestObject for I256Object {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(I256::MAX).unwrap()
    }
}
impl TestObject for BytesObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(ScBytes([0; 32].try_into().unwrap()))
            .unwrap()
    }
}

impl TestObjectWithInitialLength for BytesObject {
    fn test_object_with_initial_length(host: &Host, len: u32) -> Self {
        host.add_host_object(ScBytes(vec![0; len as usize].try_into().unwrap()))
            .unwrap()
    }
}

impl TestObject for StringObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(ScString::default()).unwrap()
    }
}
impl TestObject for SymbolObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(ScSymbol::default()).unwrap()
    }
}
impl TestObject for VecObject {
    fn test_object(host: &Host) -> Self {
        let v = HostVec::from_vec(vec![Val::from_void().to_val(); 1]).unwrap();
        host.add_host_object(v).unwrap()
    }
}
impl TestObject for MapObject {
    fn test_object(host: &Host) -> Self {
        let m = HostMap::from_map(
            vec![(Val::from_void().to_val(), Val::from_void().to_val())],
            host,
        )
        .unwrap();
        host.add_host_object(m).unwrap()
    }
}
impl TestObject for AddressObject {
    fn test_object(host: &Host) -> Self {
        host.add_host_object(ScAddress::Contract(Hash([0; 32])))
            .unwrap()
    }
}

generate_synth_dispatch_host_fn_tests!("../soroban-env-common/env.json");
