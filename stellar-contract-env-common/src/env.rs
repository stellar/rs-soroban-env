use super::RawVal;
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
    fn obj_cmp(&self, a: RawVal, b: RawVal) -> i64;

    fn log_value(&mut self, v: RawVal) -> RawVal;
    fn get_last_operation_result(&mut self) -> RawVal;

    fn obj_from_u64(&mut self, u: u64) -> RawVal;
    fn obj_to_u64(&mut self, u: RawVal) -> u64;
    fn obj_from_i64(&mut self, i: i64) -> RawVal;
    fn obj_to_i64(&mut self, i: RawVal) -> i64;

    fn map_new(&mut self) -> RawVal;
    fn map_put(&mut self, m: RawVal, k: RawVal, v: RawVal) -> RawVal;
    fn map_get(&mut self, m: RawVal, k: RawVal) -> RawVal;
    fn map_del(&mut self, m: RawVal, k: RawVal) -> RawVal;
    fn map_len(&mut self, m: RawVal) -> RawVal;
    fn map_keys(&mut self, m: RawVal) -> RawVal;
    fn map_has(&mut self, m: RawVal, k: RawVal) -> RawVal;

    fn vec_new(&mut self) -> RawVal;
    fn vec_put(&mut self, v: RawVal, i: RawVal, x: RawVal) -> RawVal;
    fn vec_get(&mut self, v: RawVal, i: RawVal) -> RawVal;
    fn vec_del(&mut self, v: RawVal, i: RawVal) -> RawVal;
    fn vec_len(&mut self, v: RawVal) -> RawVal;
    fn vec_push(&mut self, v: RawVal, x: RawVal) -> RawVal;
    fn vec_pop(&mut self, v: RawVal) -> RawVal;
    fn vec_take(&mut self, v: RawVal, n: RawVal) -> RawVal;
    fn vec_drop(&mut self, v: RawVal, n: RawVal) -> RawVal;
    fn vec_front(&mut self, v: RawVal) -> RawVal;
    fn vec_back(&mut self, v: RawVal) -> RawVal;
    fn vec_insert(&mut self, v: RawVal, i: RawVal, n: RawVal) -> RawVal;
    fn vec_append(&mut self, v1: RawVal, v2: RawVal) -> RawVal;

    fn pay(&mut self, src: RawVal, dst: RawVal, asset: RawVal, amount: RawVal) -> RawVal;
    fn account_balance(&mut self, acc: RawVal) -> RawVal;
    fn account_trust_line(&mut self, acc: RawVal, asset: RawVal) -> RawVal;
    fn trust_line_balance(&mut self, tl: RawVal) -> RawVal;
    fn get_contract_data(&mut self, k: RawVal) -> RawVal;
    fn put_contract_data(&mut self, k: RawVal, v: RawVal) -> RawVal;
    fn has_contract_data(&mut self, k: RawVal) -> RawVal;
}
