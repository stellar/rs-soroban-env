use super::Val;
use core::any;

// This trait needs to be implemented by any type that plays the role of the
// host for a contract. In a contract test or core setting this will be a full
// HostContext, whereas in a contract's wasm build it will be an empty
// type that calls through to global wasm imports provided at
// wasm-module-instantiation time.
pub trait Env: Sized + Clone {
    // Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    // Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);

    // Used to deep-compare objects, result is an -1, 0, 1
    // value for lt, eq, gt, respectively.
    fn obj_cmp(&self, a: Val, b: Val) -> i64;

    fn log_value(&mut self, v: Val) -> Val;
    fn get_last_operation_result(&mut self) -> Val;

    fn obj_from_u64(&mut self, u: u64) -> Val;
    fn obj_to_u64(&mut self, u: Val) -> u64;
    fn obj_from_i64(&mut self, i: i64) -> Val;
    fn obj_to_i64(&mut self, i: Val) -> i64;

    fn map_new(&mut self) -> Val;
    fn map_put(&mut self, m: Val, k: Val, v: Val) -> Val;
    fn map_get(&mut self, m: Val, k: Val) -> Val;
    fn map_del(&mut self, m: Val, k: Val) -> Val;
    fn map_len(&mut self, m: Val) -> Val;
    fn map_keys(&mut self, m: Val) -> Val;
    fn map_has(&mut self, m: Val, k: Val) -> Val;

    fn vec_new(&mut self) -> Val;
    fn vec_put(&mut self, v: Val, i: Val, x: Val) -> Val;
    fn vec_get(&mut self, v: Val, i: Val) -> Val;
    fn vec_del(&mut self, v: Val, i: Val) -> Val;
    fn vec_len(&mut self, v: Val) -> Val;
    fn vec_push(&mut self, v: Val, x: Val) -> Val;
    fn vec_pop(&mut self, v: Val) -> Val;
    fn vec_take(&mut self, v: Val, n: Val) -> Val;
    fn vec_drop(&mut self, v: Val, n: Val) -> Val;
    fn vec_front(&mut self, v: Val) -> Val;
    fn vec_back(&mut self, v: Val) -> Val;
    fn vec_insert(&mut self, v: Val, i: Val, n: Val) -> Val;
    fn vec_append(&mut self, v1: Val, v2: Val) -> Val;

    fn pay(&mut self, src: Val, dst: Val, asset: Val, amount: Val) -> Val;
    fn account_balance(&mut self, acc: Val) -> Val;
    fn account_trust_line(&mut self, acc: Val, asset: Val) -> Val;
    fn trust_line_balance(&mut self, tl: Val) -> Val;
    fn get_contract_data(&mut self, k: Val) -> Val;
    fn put_contract_data(&mut self, k: Val, v: Val) -> Val;
    fn has_contract_data(&mut self, k: Val) -> Val;
}
