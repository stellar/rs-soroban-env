use soroban_env_common::call_macro_with_all_host_functions;

use crate::{host::HostImpl, Env, EnvBase, Host, Object, RawVal, Symbol};
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

    fn deep_clone(&self) -> Self {
        self.get_host().deep_clone().get_weak()
    }

    fn binary_copy_from_slice(&self, b: Object, b_pos: RawVal, mem: &[u8]) -> Object {
        self.get_host().binary_copy_from_slice(b, b_pos, mem)
    }

    fn binary_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) {
        self.get_host().binary_copy_to_slice(b, b_pos, mem)
    }

    fn binary_new_from_slice(&self, mem: &[u8]) -> Object {
        self.get_host().binary_new_from_slice(mem)
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) {
        self.get_host().log_static_fmt_val(fmt, v)
    }

    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) {
        self.get_host().log_static_fmt_static_str(fmt, s)
    }

    fn log_static_fmt_val_static_str(&self, fmt: &'static str, v: RawVal, s: &'static str) {
        self.get_host().log_static_fmt_val_static_str(fmt, v, s)
    }

    fn log_static_fmt_general(&self, fmt: &'static str, vals: &[RawVal], strs: &[&'static str]) {
        self.get_host().log_static_fmt_general(fmt, vals, strs)
    }
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: impl Env for WeakHost
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by impl_env_for_host below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method definition to be used in the
// Host implementation of the Env trait (calling through to the corresponding
// function on Host after upgrading the Weak ref to an Rc via get_host()).
macro_rules! weakhost_function_helper {
    {$mod_id:ident, fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $($arg:$type),*) -> $ret {
            self.get_host().$fn_id($($arg),*)
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of
// forwarding-method definitions, which it places in the body of the declaration
// of the implementation of Env for WeakHost.
macro_rules! impl_env_for_weakhost {
    {
        $(
            // This outer pattern matches a single 'mod' block of the token-tree
            // passed from the x-macro to this macro. It is embedded in a `$()*`
            // pattern-repetition matcher so that it will match all provided
            // 'mod' blocks provided.
            $(#[$mod_attr:meta])*
            mod $mod_id:ident $mod_str:literal
            {
                $(
                    // This inner pattern matches a single function description
                    // inside a 'mod' block in the token-tree passed from the
                    // x-macro to this macro. It is embedded in a `$()*`
                    // pattern-repetition matcher so that it will match all such
                    // descriptions.
                    $(#[$fn_attr:meta])*
                    { $fn_str:literal, fn $fn_id:ident $args:tt -> $ret:ty }
                )*
            }
        )*
    }

    => // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: the implementation of Env for
        // the WeakHost struct used by EnvVals in Host objects to call back into
        // the Host.
        impl Env for WeakHost
        {
            $(
                $(
                   // This invokes the weakhost_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the implementation of Env for WeakHost.
                     weakhost_function_helper!{$mod_id, fn $fn_id  $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing impl_env_for_weakhost as its callback macro.
call_macro_with_all_host_functions! { impl_env_for_weakhost }
