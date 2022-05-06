use crate::{host::HostImpl, Env, Host, RawVal};
use core::fmt::Debug;
use std::rc::Weak;

// WeakHost is a newtype on Weak<HostImpl> so we can impl Env for it below. All
// it does is upgrade its weak reference and call through to the underlying
// method on Host, which holds a strong reference.
//
// It is not exported from the crate: we don't want users of Host to have to
// worry about weak references becoming invalid.
//
// It exists because Host _uses_ EnvVal<WeakHost> for its own internal
// representation of Vals -- HostVal -- when nesting them inside its own
// objects, and they call back through the WeakHost implementation of Env in
// order to do deep-equality and such.
//
// This is all a little convoluted and possibly too fixated on reusing code, but
// it means we can mostly use Val in 3 separate contexts (host-internal, wasm
// guest, and host-external-as-used-from-contract-unit-tests) only varying the
// type of the environment stored inside each (Guest, WeakHost, and Host
// respectively)

#[derive(Clone)]
pub(crate) struct WeakHost(pub(crate) Weak<HostImpl>);

impl Debug for WeakHost {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "WeakHost({:x})", self.0.as_ptr() as usize)
    }
}

impl WeakHost {
    fn get_host(&self) -> Host {
        Host(self.0.upgrade().expect("WeakHost upgrade"))
    }
}

impl Env for WeakHost {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self as &mut dyn std::any::Any
    }

    fn check_same_env(&self, other: &Self) {
        self.get_host().check_same_env(&other.get_host())
    }

    fn obj_cmp(&self, a: RawVal, b: RawVal) -> i64 {
        self.get_host().obj_cmp(a, b)
    }

    fn log_value(&mut self, v: RawVal) -> RawVal {
        self.get_host().log_value(v)
    }

    fn get_last_operation_result(&mut self) -> RawVal {
        self.get_host().get_last_operation_result()
    }

    fn obj_from_u64(&mut self, u: u64) -> RawVal {
        self.get_host().obj_from_u64(u)
    }

    fn obj_to_u64(&mut self, u: RawVal) -> u64 {
        self.get_host().obj_to_u64(u)
    }

    fn obj_from_i64(&mut self, i: i64) -> RawVal {
        self.get_host().obj_from_i64(i)
    }

    fn obj_to_i64(&mut self, i: RawVal) -> i64 {
        self.get_host().obj_to_i64(i)
    }

    fn map_new(&mut self) -> RawVal {
        self.get_host().map_new()
    }

    fn map_put(&mut self, m: RawVal, k: RawVal, v: RawVal) -> RawVal {
        self.get_host().map_put(m, k, v)
    }

    fn map_get(&mut self, m: RawVal, k: RawVal) -> RawVal {
        self.get_host().map_get(m, k)
    }

    fn map_del(&mut self, m: RawVal, k: RawVal) -> RawVal {
        self.get_host().map_del(m, k)
    }

    fn map_len(&mut self, m: RawVal) -> RawVal {
        self.get_host().map_len(m)
    }

    fn map_keys(&mut self, m: RawVal) -> RawVal {
        self.get_host().map_keys(m)
    }

    fn map_has(&mut self, m: RawVal, k: RawVal) -> RawVal {
        self.get_host().map_has(m, k)
    }

    fn vec_new(&mut self) -> RawVal {
        self.get_host().vec_new()
    }

    fn vec_put(&mut self, v: RawVal, i: RawVal, x: RawVal) -> RawVal {
        self.get_host().vec_put(v, i, x)
    }

    fn vec_get(&mut self, v: RawVal, i: RawVal) -> RawVal {
        self.get_host().vec_get(v, i)
    }

    fn vec_del(&mut self, v: RawVal, i: RawVal) -> RawVal {
        self.get_host().vec_del(v, i)
    }

    fn vec_len(&mut self, v: RawVal) -> RawVal {
        self.get_host().vec_len(v)
    }

    fn vec_push(&mut self, v: RawVal, x: RawVal) -> RawVal {
        self.get_host().vec_push(v, x)
    }

    fn vec_pop(&mut self, v: RawVal) -> RawVal {
        self.get_host().vec_pop(v)
    }

    fn vec_take(&mut self, v: RawVal, n: RawVal) -> RawVal {
        self.get_host().vec_take(v, n)
    }

    fn vec_drop(&mut self, v: RawVal, n: RawVal) -> RawVal {
        self.get_host().vec_drop(v, n)
    }

    fn vec_front(&mut self, v: RawVal) -> RawVal {
        self.get_host().vec_front(v)
    }

    fn vec_back(&mut self, v: RawVal) -> RawVal {
        self.get_host().vec_back(v)
    }

    fn vec_insert(&mut self, v: RawVal, i: RawVal, n: RawVal) -> RawVal {
        self.get_host().vec_insert(v, i, n)
    }

    fn vec_append(&mut self, v1: RawVal, v2: RawVal) -> RawVal {
        self.get_host().vec_append(v1, v2)
    }

    fn pay(&mut self, src: RawVal, dst: RawVal, asset: RawVal, amount: RawVal) -> RawVal {
        self.get_host().pay(src, dst, asset, amount)
    }

    fn account_balance(&mut self, acc: RawVal) -> RawVal {
        self.get_host().account_balance(acc)
    }

    fn account_trust_line(&mut self, acc: RawVal, asset: RawVal) -> RawVal {
        self.get_host().account_trust_line(acc, asset)
    }

    fn trust_line_balance(&mut self, tl: RawVal) -> RawVal {
        self.get_host().trust_line_balance(tl)
    }

    fn get_contract_data(&mut self, k: RawVal) -> RawVal {
        self.get_host().get_contract_data(k)
    }

    fn put_contract_data(&mut self, k: RawVal, v: RawVal) -> RawVal {
        self.get_host().put_contract_data(k, v)
    }

    fn has_contract_data(&mut self, k: RawVal) -> RawVal {
        self.get_host().has_contract_data(k)
    }
}
