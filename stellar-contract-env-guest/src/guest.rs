#![allow(dead_code)]
#![allow(unused_variables)]
use super::{Env, Val};

// In guest code the environment is global/implicit, so is represented as a unit struct.
#[derive(Copy, Clone, Default)]
pub struct Guest;

impl Env for Guest {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self
    }

    fn check_same_env(&self, other: &Self) {
        // All guest environments are the same
        ()
    }

    fn obj_cmp(&self, a: Val, b: Val) -> i64 {
        todo!()
    }

    fn log_value(&mut self, v: Val) -> Val {
        unsafe { context::log_value(v) }
    }

    fn get_last_operation_result(&mut self) -> Val {
        unsafe { context::get_last_operation_result() }
    }

    fn obj_from_u64(&mut self, u: u64) -> Val {
        unsafe { u64::from_u64(u) }
    }

    fn obj_to_u64(&mut self, u: Val) -> u64 {
        unsafe { u64::to_u64(u) }
    }

    fn obj_from_i64(&mut self, i: i64) -> Val {
        unsafe { i64::from_i64(i) }
    }

    fn obj_to_i64(&mut self, i: Val) -> i64 {
        unsafe { i64::to_i64(i) }
    }

    fn map_new(&mut self) -> Val {
        unsafe { map::new() }
    }

    fn map_put(&mut self, m: Val, k: Val, v: Val) -> Val {
        unsafe { map::put(m, k, v) }
    }

    fn map_get(&mut self, m: Val, k: Val) -> Val {
        unsafe { map::get(m, k) }
    }

    fn map_del(&mut self, m: Val, k: Val) -> Val {
        unsafe { map::del(m, k) }
    }

    fn map_len(&mut self, m: Val) -> Val {
        unsafe { map::len(m) }
    }

    fn map_keys(&mut self, m: Val) -> Val {
        unsafe { map::keys(m) }
    }

    fn map_has(&mut self, m: Val, k: Val) -> Val {
        unsafe { map::has(m, k) }
    }

    fn vec_new(&mut self) -> Val {
        unsafe { vec::new() }
    }

    fn vec_put(&mut self, v: Val, i: Val, x: Val) -> Val {
        unsafe { vec::put(v, i, x) }
    }

    fn vec_get(&mut self, v: Val, i: Val) -> Val {
        unsafe { vec::get(v, i) }
    }

    fn vec_del(&mut self, v: Val, i: Val) -> Val {
        unsafe { vec::del(v, i) }
    }

    fn vec_len(&mut self, v: Val) -> Val {
        unsafe { vec::len(v) }
    }

    fn vec_push(&mut self, v: Val, x: Val) -> Val {
        unsafe { vec::push(v, x) }
    }

    fn vec_pop(&mut self, v: Val) -> Val {
        unsafe { vec::pop(v) }
    }

    fn vec_take(&mut self, v: Val, n: Val) -> Val {
        unsafe { vec::take(v, n) }
    }

    fn vec_drop(&mut self, v: Val, n: Val) -> Val {
        unsafe { vec::drop(v, n) }
    }

    fn vec_front(&mut self, v: Val) -> Val {
        unsafe { vec::front(v) }
    }

    fn vec_back(&mut self, v: Val) -> Val {
        unsafe { vec::back(v) }
    }

    fn vec_insert(&mut self, v: Val, i: Val, n: Val) -> Val {
        unsafe { vec::insert(v, i, n) }
    }

    fn vec_append(&mut self, v1: Val, v2: Val) -> Val {
        unsafe { vec::append(v1, v2) }
    }

    fn pay(&mut self, src: Val, dst: Val, asset: Val, amount: Val) -> Val {
        unsafe { ledger::pay(src, dst, asset, amount) }
    }

    fn account_balance(&mut self, acc: Val) -> Val {
        unsafe { ledger::account_balance(acc) }
    }

    fn account_trust_line(&mut self, acc: Val, asset: Val) -> Val {
        unsafe { ledger::account_trust_line(acc, asset) }
    }

    fn trust_line_balance(&mut self, tl: Val) -> Val {
        unsafe { ledger::trust_line_balance(tl) }
    }

    fn get_contract_data(&mut self, k: Val) -> Val {
        unsafe { ledger::get_contract_data(k) }
    }

    fn put_contract_data(&mut self, k: Val, v: Val) -> Val {
        unsafe { ledger::put_contract_data(k, v) }
    }

    fn has_contract_data(&mut self, k: Val) -> Val {
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
    use crate::Val;
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
        pub fn log_value(v: Val) -> Val;

        // Fetches an OpResult object for inspection, in the rare case the user
        // wants more detail than is conveyed in a simple Status.
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn get_last_operation_result() -> Val;
    }
}

// U64 functions live in the wasm 'u' module
mod u64 {
    use crate::Val;
    #[link(wasm_import_module = "u")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn from_u64(x: u64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn to_u64(x: Val) -> u64;
    }
}

// I64 functions live in the wasm 'i' module
mod i64 {
    use crate::Val;
    #[link(wasm_import_module = "i")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn from_i64(x: i64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn to_i64(x: Val) -> i64;
    }
}

// Map functions live in the wasm 'm' module
mod map {
    use crate::Val;
    #[link(wasm_import_module = "m")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn new() -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn put(m: Val, k: Val, v: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub fn get(m: Val, k: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub fn del(m: Val, k: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub fn len(m: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub fn keys(m: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub fn has(m: Val, k: Val) -> Val;
    }
}

// Vec functions live in the wasm 'v' module
mod vec {
    use crate::Val;
    #[link(wasm_import_module = "v")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn new() -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn put(v: Val, i: Val, x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub fn get(v: Val, i: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub fn del(v: Val, i: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub fn len(v: Val) -> Val;

        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub fn push(v: Val, x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub fn pop(v: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub fn take(v: Val, n: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub fn drop(v: Val, n: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub fn front(v: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$9")]
        pub fn back(v: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$A")]
        pub fn insert(v: Val, i: Val, x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$B")]
        pub fn append(v1: Val, v2: Val) -> Val;
    }
}

// Ledger functions live in the wasm 'l' module
mod ledger {
    use crate::Val;
    #[link(wasm_import_module = "l")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn get_current_ledger_num() -> Val;

        // NB: this returns a raw/unboxed u64, not a Val union.
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn get_current_ledger_close_time() -> u64;

        // NB: returns a Status; details can be fetched with
        // get_last_operation_result.
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub fn pay(src: Val, dst: Val, asset: Val, amount: Val) -> Val;

        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub fn put_contract_data(key: Val, val: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub fn has_contract_data(key: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub fn get_contract_data(key: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub fn del_contract_data(key: Val) -> Val;

        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub fn account_balance(acc: Val) -> Val;

        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub fn account_trust_line(acc: Val, asset: Val) -> Val;

        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub fn trust_line_balance(tl: Val) -> Val;
    }
}

// Cross-contract functions live in the wasm 'c' module
mod call {
    use crate::Val;
    #[link(wasm_import_module = "c")]
    extern "C" {
        // NB: returns callee-return-value-or-Status; details can be fetched with
        // get_last_operation_result.
        //
        // TODO: possibly revisit this since it adds ambiguity to whether callee
        // successfully returned a status, or call failed and failure _generated_ a
        // status. Possibly this distinction is too fussy to disambiguate.
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn call0(contract: Val, func: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn call1(contract: Val, func: Val, a: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub fn call2(contract: Val, func: Val, a: Val, b: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub fn call3(contract: Val, func: Val, a: Val, b: Val, c: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub fn call4(contract: Val, func: Val, a: Val, b: Val, c: Val, d: Val) -> Val;
    }
}

// BigNum functions live in the wasm 'b' module
mod bignum {
    use crate::Val;
    #[link(wasm_import_module = "b")]
    extern "C" {
        #[cfg_attr(target_family = "wasm", link_name = "$_")]
        pub fn from_u64(x: u64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$0")]
        pub fn add(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$1")]
        pub fn sub(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$2")]
        pub fn mul(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$3")]
        pub fn div(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$4")]
        pub fn rem(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$5")]
        pub fn and(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$6")]
        pub fn or(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$7")]
        pub fn xor(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$8")]
        pub fn shl(lhs: Val, rhs: u64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$9")]
        pub fn shr(lhs: Val, rhs: u64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$A")]
        pub fn cmp(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$B")]
        pub fn is_zero(x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$C")]
        pub fn neg(x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$D")]
        pub fn not(x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$E")]
        pub fn gcd(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$F")]
        pub fn lcm(lhs: Val, rhs: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$G")]
        pub fn pow(lhs: Val, rhs: u64) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$H")]
        pub fn pow_mod(p: Val, q: Val, m: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$I")]
        pub fn sqrt(x: Val) -> Val;
        #[cfg_attr(target_family = "wasm", link_name = "$J")]
        pub fn bits(x: Val) -> u64;
        #[cfg_attr(target_family = "wasm", link_name = "$K")]
        pub fn to_u64(x: Val) -> u64;
        #[cfg_attr(target_family = "wasm", link_name = "$L")]
        pub fn to_i64(x: Val) -> i64;
        #[cfg_attr(target_family = "wasm", link_name = "$M")]
        pub fn from_i64(x: i64) -> Val;
    }
}
