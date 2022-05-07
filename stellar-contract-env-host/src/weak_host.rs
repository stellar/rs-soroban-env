use stellar_contract_env_common::call_macro_with_all_host_functions;

use crate::{host::HostImpl, Env, EnvBase, Host, RawVal};
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

impl EnvBase for WeakHost {
    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        self as &mut dyn std::any::Any
    }

    fn check_same_env(&self, other: &Self) {
        self.get_host().check_same_env(&other.get_host())
    }
}

macro_rules! host_function {
    {$mod_name:ident, fn $func_id:ident(&self)() -> $ret:ty} =>
    {fn $func_id(&self) -> $ret { self.get_host().$func_id() }};

    {$mod_name:ident, fn $func_id:ident(&self)($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {fn $func_id(&self, $($arg:$type),*) -> $ret { self.get_host().$func_id($($arg),*) }};

    {$mod_name:ident, fn $func_id:ident(&mut self)() -> $ret:ty} =>
    {fn $func_id(&mut self) -> $ret { self.get_host().$func_id() }};

    {$mod_name:ident, fn $func_id:ident(&mut self)($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {fn $func_id(&mut self, $($arg:$type),*) -> $ret { self.get_host().$func_id($($arg),*) }};
}

macro_rules! impl_env_for_weakhost {
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
        impl Env for WeakHost
        {
            $(
                $(
                    host_function!{$mod_name, fn $func_id $selfspec $args -> $ret}
                )*
            )*
        }
    };
}

call_macro_with_all_host_functions! { impl_env_for_weakhost }
