#![allow(dead_code)]
#![allow(unused_variables)]

use core::any::Any;

use super::{Env, RawVal};

// In guest code the environment is global/implicit, so is represented as a unit struct.
#[derive(Copy, Clone, Default)]
pub struct Guest;

impl Env for Guest {
    fn as_mut_any(&mut self) -> &mut dyn Any {
        self
    }

    fn check_same_env(&self, other: &Self) {
        // All guest environments are the same
        ()
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> i64 {
        todo!()
    }

    fn log_value(&mut self, v: RawVal) -> RawVal {
        unsafe { context::log_value(v) }
    }

    fn get_last_operation_result(&mut self) -> RawVal {
        unsafe { context::get_last_operation_result() }
    }

    fn obj_from_u64(&mut self, u: u64) -> RawVal {
        unsafe { u64::from_u64(u) }
    }

    fn obj_to_u64(&mut self, u: RawVal) -> u64 {
        unsafe { u64::to_u64(u) }
    }

    fn obj_from_i64(&mut self, i: i64) -> RawVal {
        unsafe { i64::from_i64(i) }
    }

    fn obj_to_i64(&mut self, i: RawVal) -> i64 {
        unsafe { i64::to_i64(i) }
    }

    fn map_new(&mut self) -> RawVal {
        unsafe { map::new() }
    }

    fn map_put(&mut self, m: RawVal, k: RawVal, v: RawVal) -> RawVal {
        unsafe { map::put(m, k, v) }
    }

    fn map_get(&mut self, m: RawVal, k: RawVal) -> RawVal {
        unsafe { map::get(m, k) }
    }

    fn map_del(&mut self, m: RawVal, k: RawVal) -> RawVal {
        unsafe { map::del(m, k) }
    }

    fn map_len(&mut self, m: RawVal) -> RawVal {
        unsafe { map::len(m) }
    }

    fn map_keys(&mut self, m: RawVal) -> RawVal {
        unsafe { map::keys(m) }
    }

    fn map_has(&mut self, m: RawVal, k: RawVal) -> RawVal {
        unsafe { map::has(m, k) }
    }

    fn vec_new(&mut self) -> RawVal {
        unsafe { vec::new() }
    }

    fn vec_put(&mut self, v: RawVal, i: RawVal, x: RawVal) -> RawVal {
        unsafe { vec::put(v, i, x) }
    }

    fn vec_get(&mut self, v: RawVal, i: RawVal) -> RawVal {
        unsafe { vec::get(v, i) }
    }

    fn vec_del(&mut self, v: RawVal, i: RawVal) -> RawVal {
        unsafe { vec::del(v, i) }
    }

    fn vec_len(&mut self, v: RawVal) -> RawVal {
        unsafe { vec::len(v) }
    }

    fn vec_push(&mut self, v: RawVal, x: RawVal) -> RawVal {
        unsafe { vec::push(v, x) }
    }

    fn vec_pop(&mut self, v: RawVal) -> RawVal {
        unsafe { vec::pop(v) }
    }

    fn vec_take(&mut self, v: RawVal, n: RawVal) -> RawVal {
        unsafe { vec::take(v, n) }
    }

    fn vec_drop(&mut self, v: RawVal, n: RawVal) -> RawVal {
        unsafe { vec::drop(v, n) }
    }

    fn vec_front(&mut self, v: RawVal) -> RawVal {
        unsafe { vec::front(v) }
    }

    fn vec_back(&mut self, v: RawVal) -> RawVal {
        unsafe { vec::back(v) }
    }

    fn vec_insert(&mut self, v: RawVal, i: RawVal, n: RawVal) -> RawVal {
        unsafe { vec::insert(v, i, n) }
    }

    fn vec_append(&mut self, v1: RawVal, v2: RawVal) -> RawVal {
        unsafe { vec::append(v1, v2) }
    }

    fn pay(&mut self, src: RawVal, dst: RawVal, asset: RawVal, amount: RawVal) -> RawVal {
        unsafe { ledger::pay(src, dst, asset, amount) }
    }

    fn account_balance(&mut self, acc: RawVal) -> RawVal {
        unsafe { ledger::account_balance(acc) }
    }

    fn account_trust_line(&mut self, acc: RawVal, asset: RawVal) -> RawVal {
        unsafe { ledger::account_trust_line(acc, asset) }
    }

    fn trust_line_balance(&mut self, tl: RawVal) -> RawVal {
        unsafe { ledger::trust_line_balance(tl) }
    }

    fn get_contract_data(&mut self, k: RawVal) -> RawVal {
        unsafe { ledger::get_contract_data(k) }
    }

    fn put_contract_data(&mut self, k: RawVal, v: RawVal) -> RawVal {
        unsafe { ledger::put_contract_data(k, v) }
    }

    fn has_contract_data(&mut self, k: RawVal) -> RawVal {
        unsafe { ledger::has_contract_data(k) }
    }
}

// Most host functions have strong contractual guarantees from the host: they
// will either return the correctly-typed objects here, or they will trap. So we
// do not need to check return types, and we can even unsafely downcast Vals to
// specific subtypes if we know them.
//
// (Recall that there must be no way for the guest to corrupt the host even if
// the guest does receive unexpected objects -- at worst the host can corrupt or
// confuse the guest this way, but the guest can never defend itself against a
// malicious host anyways.)
//
// We do this mostly to minimize codesize, and also reduce the chance of users
// missing an error in a contract binding to another language that doesn't have
// Result<> types. Every error that can trap this way typically has a way to
// avoid it by checking some value first (eg. check a map to see if it contains
// a value before getting it).
//
// The exceptions are ledger-interaction calls and especially cross-contract
// calls: these we project into a Result<> because they are fairly failure-prone
// and impossible to guard against failure of, typically. We assume users might
// wish to contain these and, in general, that users won't be doing a _ton_ of
// them so it's ok that they are a little more expensive code-size-wise.

// General context-access functions live in the wasm 'x' module.
mod context {
    use crate::RawVal;
    #[link(wasm_import_module = "x")]
    extern "C" {

        // link names are chosen to be (a) unrepresentable as rust identifiers so
        // they cannot collide with exported user functions and (b) very short, so
        // they do not take up a lot of space in import tables. They consist of a $
        // symbol followed by a single character category identifier and a single
        // character function identifier, each drawn from the same 64-character
        // repertoire as symbol: [_0-9A-Za-z], in that order. If we ever need more
        // than 64 functions within a category we can just overflow to 2-character
        // function identifiers; if we ever need more than 64 categories, we can
        // pick a different prefix-char for the category-overflow space; both of
        // these possibilities seem unlikely at present, but either way they're
        // fairly straightforward.
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn log_value(v: RawVal) -> RawVal;

        // Fetches an OpResult object for inspection, in the rare case the user
        // wants more detail than is conveyed in a simple Status.
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn get_last_operation_result() -> RawVal;
    }
}

// U64 functions live in the wasm 'u' module
mod u64 {
    use crate::RawVal;
    #[link(wasm_import_module = "u")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn from_u64(x: u64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn to_u64(x: RawVal) -> u64;
    }
}

// I64 functions live in the wasm 'i' module
mod i64 {
    use crate::RawVal;
    #[link(wasm_import_module = "i")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn from_i64(x: i64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn to_i64(x: RawVal) -> i64;
    }
}

// Map functions live in the wasm 'm' module
mod map {
    use crate::RawVal;
    #[link(wasm_import_module = "m")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn new() -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn put(m: RawVal, k: RawVal, v: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub(crate) fn get(m: RawVal, k: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub(crate) fn del(m: RawVal, k: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub(crate) fn len(m: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub(crate) fn keys(m: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub(crate) fn has(m: RawVal, k: RawVal) -> RawVal;
    }
}

// Vec functions live in the wasm 'v' module
mod vec {
    use crate::RawVal;
    #[link(wasm_import_module = "v")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn new() -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn put(v: RawVal, i: RawVal, x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub(crate) fn get(v: RawVal, i: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub(crate) fn del(v: RawVal, i: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub(crate) fn len(v: RawVal) -> RawVal;

        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub(crate) fn push(v: RawVal, x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub(crate) fn pop(v: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub(crate) fn take(v: RawVal, n: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub(crate) fn drop(v: RawVal, n: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub(crate) fn front(v: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$9")]
        pub(crate) fn back(v: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$A")]
        pub(crate) fn insert(v: RawVal, i: RawVal, x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$B")]
        pub(crate) fn append(v1: RawVal, v2: RawVal) -> RawVal;
    }
}

// Ledger functions live in the wasm 'l' module
mod ledger {
    use crate::RawVal;
    #[link(wasm_import_module = "l")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn get_current_ledger_num() -> RawVal;

        // NB: this returns a raw/unboxed u64, not a Val union.
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn get_current_ledger_close_time() -> u64;

        // NB: returns a Status; details can be fetched with
        // get_last_operation_result.
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub(crate) fn pay(src: RawVal, dst: RawVal, asset: RawVal, amount: RawVal) -> RawVal;

        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub(crate) fn put_contract_data(key: RawVal, val: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub(crate) fn has_contract_data(key: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub(crate) fn get_contract_data(key: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub(crate) fn del_contract_data(key: RawVal) -> RawVal;

        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub(crate) fn account_balance(acc: RawVal) -> RawVal;

        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub(crate) fn account_trust_line(acc: RawVal, asset: RawVal) -> RawVal;

        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub(crate) fn trust_line_balance(tl: RawVal) -> RawVal;
    }
}

// Cross-contract functions live in the wasm 'c' module
mod call {
    use crate::RawVal;
    #[link(wasm_import_module = "c")]
    extern "C" {
        // NB: returns callee-return-value-or-Status; details can be fetched with
        // get_last_operation_result.
        //
        // TODO: possibly revisit this since it adds ambiguity to whether callee
        // successfully returned a status, or call failed and failure _generated_ a
        // status. Possibly this distinction is too fussy to disambiguate.
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn call0(contract: RawVal, func: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn call1(contract: RawVal, func: RawVal, a: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub(crate) fn call2(contract: RawVal, func: RawVal, a: RawVal, b: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub(crate) fn call3(
            contract: RawVal,
            func: RawVal,
            a: RawVal,
            b: RawVal,
            c: RawVal,
        ) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub(crate) fn call4(
            contract: RawVal,
            func: RawVal,
            a: RawVal,
            b: RawVal,
            c: RawVal,
            d: RawVal,
        ) -> RawVal;
    }
}

// BigNum functions live in the wasm 'b' module
mod bignum {
    use crate::RawVal;
    #[link(wasm_import_module = "b")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub(crate) fn from_u64(x: u64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub(crate) fn add(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub(crate) fn sub(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub(crate) fn mul(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub(crate) fn div(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub(crate) fn rem(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub(crate) fn and(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub(crate) fn or(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub(crate) fn xor(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub(crate) fn shl(lhs: RawVal, rhs: u64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$9")]
        pub(crate) fn shr(lhs: RawVal, rhs: u64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$A")]
        pub(crate) fn cmp(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$B")]
        pub(crate) fn is_zero(x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$C")]
        pub(crate) fn neg(x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$D")]
        pub(crate) fn not(x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$E")]
        pub(crate) fn gcd(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$F")]
        pub(crate) fn lcm(lhs: RawVal, rhs: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$G")]
        pub(crate) fn pow(lhs: RawVal, rhs: u64) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$H")]
        pub(crate) fn pow_mod(p: RawVal, q: RawVal, m: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$I")]
        pub(crate) fn sqrt(x: RawVal) -> RawVal;
        #[cfg_attr(target_family = "wasm", link_name = "$J")]
        pub(crate) fn bits(x: RawVal) -> u64;
        #[cfg_attr(target_family = "wasm", link_name = "$K")]
        pub(crate) fn to_u64(x: RawVal) -> u64;
        #[cfg_attr(target_family = "wasm", link_name = "$L")]
        pub(crate) fn to_i64(x: RawVal) -> i64;
        #[cfg_attr(target_family = "wasm", link_name = "$M")]
        pub(crate) fn from_i64(x: i64) -> RawVal;
    }
}
