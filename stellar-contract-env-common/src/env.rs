use super::RawVal;
use core::any;

pub trait EnvBase: Sized + Clone {
    // Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    // Used to check two environments are the same, trapping if not.
    fn check_same_env(&self, other: &Self);
}

// The set of host functions need to be statically reflected-on in
// a variety of contexts elsewhere in the guest and host, so we define
// them through an x-macro (a macro that calls a user-provided macro)
// and call the x-macro from all such contexts.

#[macro_export]
macro_rules! call_macro_with_all_host_functions {

    {$macro_to_call_back:ident} => {

        // We call the user-provided macro back with a sequence of token trees
        // that represent host function modules, each containing a set of token
        // trees that represent host functions. The format here is arbitrary, it
        // just has to convey all the relevant metadata we want to convey and be
        // sufficiently plainly structured that the callee macros can
        // pattern-match on it relatively easily.

        $macro_to_call_back! {

            mod context "x" {
                {"$_", fn log_value(&mut self)(v:RawVal) -> RawVal }
                {"$0", fn get_last_operation_result(&self)() -> RawVal }
                {"$1", fn obj_cmp(&self)(a:RawVal, b:RawVal) -> i64 }
            }

            mod u64 "u" {
                {"$_", fn obj_from_u64(&mut self)(v:u64) -> RawVal }
                {"$0", fn obj_to_u64(&self)(v:RawVal) -> u64 }
            }

            mod i64 "i" {
                {"$_", fn obj_from_i64(&mut self)(v:i64) -> RawVal }
                {"$0", fn obj_to_i64(&self)(v:RawVal) -> i64 }
            }

            mod map "m" {
                {"$_", fn map_new(&mut self)() -> RawVal }
                {"$0", fn map_put(&mut self)(m:RawVal, k:RawVal, v:RawVal) -> RawVal}
                {"$1", fn map_get(&mut self)(m:RawVal, k:RawVal) -> RawVal}
                {"$2", fn map_del(&mut self)(m:RawVal, k:RawVal) -> RawVal}
                {"$3", fn map_len(&mut self)(m:RawVal) -> RawVal}
                {"$4", fn map_keys(&mut self)(m:RawVal) -> RawVal}
                {"$5", fn map_has(&mut self)(m:RawVal,k:RawVal) -> RawVal}
            }

            mod vec "v" {
                {"$_", fn vec_new(&mut self)() -> RawVal}
                {"$0", fn vec_put(&mut self)(v:RawVal, i:RawVal, x:RawVal) -> RawVal}
                {"$1", fn vec_get(&mut self)(v:RawVal, i:RawVal) -> RawVal}
                {"$2", fn vec_del(&mut self)(v:RawVal, i:RawVal) -> RawVal}
                {"$3", fn vec_len(&mut self)(v:RawVal) -> RawVal}

                {"$4", fn vec_push(&mut self)(v:RawVal, x:RawVal) -> RawVal}
                {"$5", fn vec_pop(&mut self)(v:RawVal) -> RawVal}
                {"$6", fn vec_take(&mut self)(v:RawVal, n:RawVal) -> RawVal}
                {"$7", fn vec_drop(&mut self)(v:RawVal, n:RawVal) -> RawVal}
                {"$8", fn vec_front(&mut self)(v:RawVal) -> RawVal}
                {"$9", fn vec_back(&mut self)(v:RawVal) -> RawVal}
                {"$A", fn vec_insert(&mut self)(v:RawVal, i:RawVal, x:RawVal) -> RawVal}
                {"$B", fn vec_append(&mut self)(v1:RawVal, v2:RawVal) -> RawVal}
            }

            mod ledger "l" {
                {"$_", fn get_current_ledger_num(&self)() -> RawVal }
                {"$0", fn get_current_ledger_close_time(&self)() -> RawVal}

                {"$1", fn pay(&mut self)(src:RawVal, dst:RawVal, asset:RawVal, amt:RawVal) -> RawVal}

                {"$2", fn put_contract_data(&mut self)(k:RawVal, v:RawVal) -> RawVal}
                {"$3", fn has_contract_data(&mut self)(k:RawVal) -> RawVal}
                {"$4", fn get_contract_data(&mut self)(k:RawVal) -> RawVal}
                {"$5", fn del_contract_data(&mut self)(k:RawVal) -> RawVal}

                {"$6", fn account_balance(&mut self)(acct:RawVal) -> RawVal}
                {"$7", fn account_trust_line(&mut self)(acct:RawVal, asset:RawVal) -> RawVal}
                {"$8", fn trust_line_balance(&mut self)(tl:RawVal) -> RawVal}
            }

            mod call "c" {
                {"$_", fn call0(&mut self)(contract:RawVal,func:RawVal) -> RawVal}
                {"$0", fn call1(&mut self)(contract:RawVal,func:RawVal,a:RawVal) -> RawVal}
                {"$1", fn call2(&mut self)(contract:RawVal,func:RawVal,a:RawVal,b:RawVal) -> RawVal}
                {"$2", fn call3(&mut self)(contract:RawVal,func:RawVal,a:RawVal,b:RawVal,c:RawVal) -> RawVal}
                {"$3", fn call4(&mut self)(contract:RawVal,func:RawVal,a:RawVal,b:RawVal,c:RawVal,d:RawVal) -> RawVal}
            }

            mod bigint "b" {
                {"$_", fn bigint_from_u64(&mut self)(x:RawVal) -> RawVal}
                {"$0", fn bigint_add(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$1", fn bigint_sub(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$2", fn bigint_mul(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$3", fn bigint_div(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$4", fn bigint_rem(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$5", fn bigint_and(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$6", fn bigint_or(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$7", fn bigint_xor(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$8", fn bigint_shl(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$9", fn bigint_shr(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$A", fn bigint_cmp(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$B", fn bigint_is_zero(&mut self)(x:RawVal) -> RawVal}
                {"$C", fn bigint_neg(&mut self)(x:RawVal) -> RawVal}
                {"$D", fn bigint_not(&mut self)(x:RawVal) -> RawVal}
                {"$E", fn bigint_gcd(&mut self)(x:RawVal) -> RawVal}
                {"$F", fn bigint_lcm(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$G", fn bigint_pow(&mut self)(x:RawVal,y:RawVal) -> RawVal}
                {"$H", fn bigint_pow_mod(&mut self)(p:RawVal,q:RawVal,m:RawVal) -> RawVal}
                {"$I", fn bigint_sqrt(&mut self)(x:RawVal) -> RawVal}
                {"$J", fn bigint_bits(&mut self)(x:RawVal) -> RawVal}
                {"$K", fn bigint_to_u64(&mut self)(x:RawVal) -> u64}
                {"$L", fn bigint_to_i64(&mut self)(x:RawVal) -> i64}
                {"$M", fn bigint_from_i64(&mut self)(x:i64) -> RawVal}
            }
        }
    };
}

macro_rules! host_function {
    {fn $func_id:ident(&self)() -> $ret:ty} => {fn $func_id(&self) -> $ret;};
    {fn $func_id:ident(&self)($($arg:ident:$type:ty),*) -> $ret:ty} => {fn $func_id(&self, $($arg:$type),*) -> $ret;};
    {fn $func_id:ident(&mut self)() -> $ret:ty} => {fn $func_id(&mut self) -> $ret;};
    {fn $func_id:ident(&mut self)($($arg:ident:$type:ty),*) -> $ret:ty} => {fn $func_id(&mut self, $($arg:$type),*) -> $ret;};
}

macro_rules! generate_env_trait {
    {
        $(
            mod $mod_name:ident $mod_str:literal
            {
                $(
                    { $field_str:literal, fn $func_id:ident $selfspec:tt $args:tt -> $ret:ty }
                )*
            }
        )*
    }
    =>
    {
        // This trait needs to be implemented by any type that plays the role of the
        // host for a contract. In a contract test or core setting this will be a full
        // HostContext, whereas in a contract's wasm build it will be an empty
        // type that calls through to global wasm imports provided at
        // wasm-module-instantiation time.
        pub trait Env: EnvBase
        {
            $(
                $(
                    host_function!{fn $func_id $selfspec $args -> $ret}
                )*
            )*
        }
    };
}

call_macro_with_all_host_functions! { generate_env_trait }
