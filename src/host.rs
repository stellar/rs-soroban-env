use stellar_xdr::ScObjectType;

use crate::ValType;

use super::{Object, Val};
use core::any;

// HostConvertable is similar to ValType but also covers types with conversions
// that need a Host -- those that might require allocating an Object. ValType
// covers types that can always be directly converted to Val with no Host.
pub trait HostConvertable: Sized {
    fn into_val<H: Host + ?Sized>(self, host: &mut H) -> Val;
    fn try_from_val<H: Host + ?Sized>(v: Val, host: &mut H) -> Option<Self>;
}

impl<V: ValType> HostConvertable for V {
    fn into_val<H: Host + ?Sized>(self, _host: &mut H) -> Val {
        self.into()
    }

    fn try_from_val<H: Host + ?Sized>(v: Val, _host: &mut H) -> Option<Self> {
        if <V as ValType>::is_val_type(v) {
            Some(unsafe { <V as ValType>::unchecked_from_val(v) })
        } else {
            None
        }
    }
}

impl HostConvertable for i64 {
    fn into_val<H: Host + ?Sized>(self, host: &mut H) -> Val {
        if self >= 0 {
            unsafe { Val::unchecked_from_u63(self) }
        } else {
            host.obj_from_i64(self).into()
        }
    }

    fn try_from_val<H: Host + ?Sized>(v: Val, host: &mut H) -> Option<Self> {
        if v.is_u63() {
            Some(unsafe { v.unchecked_as_u63() })
        } else if Object::val_is_obj_type(v, ScObjectType::ScoI64) {
            Some(host.obj_to_i64(unsafe { Object::unchecked_from_val(v) }))
        } else {
            None
        }
    }
}

impl HostConvertable for u64 {
    fn into_val<H: Host + ?Sized>(self, host: &mut H) -> Val {
        if self <= (i64::MAX as u64) {
            unsafe { Val::unchecked_from_u63(self as i64) }
        } else {
            host.obj_from_u64(self).into()
        }
    }

    fn try_from_val<H: Host + ?Sized>(v: Val, host: &mut H) -> Option<Self> {
        if v.is_u63() {
            Some(unsafe { v.unchecked_as_u63() } as u64)
        } else if Object::val_is_obj_type(v, ScObjectType::ScoU64) {
            Some(host.obj_to_u64(unsafe { Object::unchecked_from_val(v) }))
        } else {
            None
        }
    }
}

// This trait needs to be implemented by any type that plays the role of the
// host for a contract. In a contract test or core setting this will be a full
// HostContext, whereas in a contract's wasm build it will be an empty
// type that calls through to global wasm imports provided at
// wasm-module-instantiation time.
pub trait Host {
    // Used for recovering the concrete type of the Host.
    fn as_mut_any(&mut self) -> &mut dyn any::Any;

    fn log_value(&mut self, v: Val) -> Val;
    fn get_last_operation_result(&mut self) -> Object;

    fn obj_from_u64(&mut self, u: u64) -> Object;
    fn obj_to_u64(&mut self, u: Object) -> u64;
    fn obj_from_i64(&mut self, i: i64) -> Object;
    fn obj_to_i64(&mut self, i: Object) -> i64;

    fn map_new(&mut self) -> Object;
    fn map_put(&mut self, m: Object, k: Val, v: Val) -> Object;
    fn map_get(&mut self, m: Object, k: Val) -> Val;
    fn map_del(&mut self, m: Object, k: Val) -> Object;
    fn map_len(&mut self, m: Object) -> Val;
    fn map_keys(&mut self, m: Object) -> Object;
    fn map_has(&mut self, m: Object, k: Val) -> Val;

    fn vec_new(&mut self) -> Object;
    fn vec_put(&mut self, v: Object, i: Val, x: Val) -> Object;
    fn vec_get(&mut self, v: Object, i: Val) -> Val;
    fn vec_del(&mut self, v: Object, i: Val) -> Object;
    fn vec_len(&mut self, v: Object) -> Val;
    fn vec_push(&mut self, v: Object, x: Val) -> Object;
    fn vec_pop(&mut self, v: Object) -> Object;
    fn vec_take(&mut self, v: Object, n: Val) -> Object;
    fn vec_drop(&mut self, v: Object, n: Val) -> Object;
    fn vec_front(&mut self, v: Object) -> Val;
    fn vec_back(&mut self, v: Object) -> Val;
    fn vec_insert(&mut self, v: Object, i: Val, n: Val) -> Object;
    fn vec_append(&mut self, v1: Object, v2: Object) -> Object;

    fn pay(&mut self, src: Object, dst: Object, asset: Object, amount: Val) -> Val;
    fn account_balance(&mut self, acc: Object) -> Val;
    fn account_trust_line(&mut self, acc: Object, asset: Object) -> Object;
    fn trust_line_balance(&mut self, tl: Object) -> Val;
    fn get_contract_data(&mut self, k: Val) -> Val;
    fn put_contract_data(&mut self, k: Val, v: Val) -> Val;
    fn has_contract_data(&mut self, k: Val) -> Val;
}
