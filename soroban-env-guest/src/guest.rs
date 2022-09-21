#![allow(dead_code)]
#![allow(unused_variables)]

use soroban_env_common::call_macro_with_all_host_functions;

use super::{Env, EnvBase, Object, RawVal, Status, Symbol};
#[cfg(target_family = "wasm")]
use static_assertions as sa;

/// The [Guest] is the implementation of the [Env] interface seen and used by
/// contracts built into WASM for execution within a WASM VM. It is a 0-sized
/// "stub" type implementation of the [Env] interface that forwards each [Env]
/// method to an external function, imported from its runtime environment. This
/// implementation is automatically generated and has no interesting content.
#[derive(Copy, Clone, Default)]
pub struct Guest;

// The Guest struct is only meaningful when compiling for the WASM target. All
// these fns should not be called at all because the SDK's choice of Env should be
// Host for a non-WASM build.
#[cfg(not(target_family = "wasm"))]
impl EnvBase for Guest {
    fn as_mut_any(&mut self) -> &mut dyn core::any::Any {
        unimplemented!()
    }

    fn check_same_env(&self, other: &Self) {
        unimplemented!()
    }

    fn deep_clone(&self) -> Self {
        unimplemented!()
    }

    fn bytes_copy_from_slice(
        &self,
        b: Object,
        b_pos: RawVal,
        mem: &[u8],
    ) -> Result<Object, Status> {
        unimplemented!()
    }

    fn bytes_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) -> Result<(), Status> {
        unimplemented!()
    }

    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<Object, Status> {
        unimplemented!()
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), Status> {
        unimplemented!()
    }

    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), Status> {
        unimplemented!()
    }
}

#[cfg(target_family = "wasm")]
impl EnvBase for Guest {
    fn as_mut_any(&mut self) -> &mut dyn core::any::Any {
        return self;
    }

    fn check_same_env(&self, other: &Self) {
        ()
    }

    fn deep_clone(&self) -> Self {
        Self
    }

    fn bytes_copy_from_slice(
        &self,
        b: Object,
        b_pos: RawVal,
        mem: &[u8],
    ) -> Result<Object, Status> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: RawVal = RawVal::from_u32(mem.as_ptr() as u32);
        let len: RawVal = RawVal::from_u32(mem.len() as u32);
        // NB: any failure in the host function here will trap the guest,
        // not return, so we only have to code the happy path.
        Ok(self.bytes_copy_from_linear_memory(b, b_pos, lm_pos, len))
    }

    fn bytes_copy_to_slice(&self, b: Object, b_pos: RawVal, mem: &mut [u8]) -> Result<(), Status> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: RawVal = RawVal::from_u32(mem.as_ptr() as u32);
        let len: RawVal = RawVal::from_u32(mem.len() as u32);
        self.bytes_copy_to_linear_memory(b, b_pos, lm_pos, len);
        Ok(())
    }

    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<Object, Status> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: RawVal = RawVal::from_u32(mem.as_ptr() as u32);
        let len: RawVal = RawVal::from_u32(mem.len() as u32);
        Ok(self.bytes_new_from_linear_memory(lm_pos, len))
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), Status> {
        // TODO: It's possible we might want to do something in the wasm
        // case with static strings similar to the bytes functions above,
        // eg. decay the strings to u32 values and pass them to the host as linear
        // memory locations for capture into the debug-events buffer,
        // but for the time being we're going to _not_ do that because
        // we assume users building for wasm want their static strings
        // _removed_ from the bytes statically (it's also somewhat annoying
        // to implement the capture of static strings into the debug buffer,
        // it makes the debug buffer into non-Send+Sync and then we need
        // to remove it from the HostError, report separately from HostError's
        // Debug impl)
        Ok(())
    }

    fn log_static_fmt_static_str(&self, fmt: &'static str, s: &'static str) -> Result<(), Status> {
        // Intentionally a no-op in this cfg. See above.
        Ok(())
    }

    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), Status> {
        // Intentionally a no-op in this cfg. See above.
        Ok(())
    }

    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), Status> {
        // Intentionally a no-op in this cfg. See above.
        Ok(())
    }
}

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: impl Env for Guest
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by impl_env_for_guest below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method definition to be used in the
// Guest implementation of the Env trait (calling through to the corresponding
// unsafe extern function).
macro_rules! guest_function_helper {
    {$mod_id:ident, fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty}
    =>
    {
        fn $fn_id(&self, $($arg:$type),*) -> $ret {
            unsafe {
                $mod_id::$fn_id($($arg),*)
            }
        }
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a suite of
// forwarding-method definitions, which it places in the body of the declaration
// of the implementation of Env for Guest.
macro_rules! impl_env_for_guest {
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

    =>  // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a single item: the implementation of Env for
        // the Guest struct used by client contract code running in a WASM VM.
        impl Env for Guest
        {
            $(
                $(
                    // This invokes the guest_function_helper! macro above
                    // passing only the relevant parts of the declaration
                    // matched by the inner pattern above. It is embedded in two
                    // nested `$()*` pattern-repetition expanders that
                    // correspond to the pattern-repetition matchers in the
                    // match section, but we ignore the structure of the 'mod'
                    // block repetition-level from the outer pattern in the
                    // expansion, flattening all functions from all 'mod' blocks
                    // into the implementation of Env for Guest.
                    guest_function_helper!{$mod_id, fn $fn_id $args -> $ret}
                )*
            )*
        }
    };
}

// Here we invoke the x-macro passing generate_env_trait as its callback macro.
call_macro_with_all_host_functions! { impl_env_for_guest }

///////////////////////////////////////////////////////////////////////////////
/// X-macro use: extern mod blocks
///////////////////////////////////////////////////////////////////////////////

// This is a helper macro used only by impl_env_for_guest below. It consumes a
// token-tree of the form:
//
//  {fn $fn_id:ident $args:tt -> $ret:ty}
//
// and produces the the corresponding method definition to be used in the
// Guest implementation of the Env trait (calling through to the corresponding
// unsafe extern function).
macro_rules! extern_function_helper {
    {
        $fn_str:literal, $(#[$attr:meta])* fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty
    }
    =>
    {
        #[cfg_attr(target_family = "wasm", link_name = $fn_str)]
        $(#[$attr])*
        pub(crate) fn $fn_id($($arg:$type),*) -> $ret;
    };
}

// This is a callback macro that pattern-matches the token-tree passed by the
// x-macro (call_macro_with_all_host_functions) and produces a set of mod
// items containing extern "C" blocks, each containing extern function
// declarations.
macro_rules! generate_extern_modules {
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

    =>  // The part of the macro above this line is a matcher; below is its expansion.

    {
        // This macro expands to a set of mod items, each declaring all the extern fns
        // available for the `impl Env for Guest` methods above to call through to.
        $(
            // Unlike the other uses of the x-macro that "flatten" the
            // mod-and-fn structure of the matched token-tree, this callback
            // macro's expansion preserves the structure, creating a nested set
            // of mods and fns. There is therefore a mod declaration between the
            // outer and inner `$()*` pattern-repetition expanders.
            $(#[$mod_attr])*
            mod $mod_id {
                #[allow(unused_imports)]
                use crate::{RawVal,Object,Symbol,Status};
                #[link(wasm_import_module = $mod_str)]
                extern "C" {
                    $(
                        // This invokes the extern_function_helper! macro above
                        // passing only the relevant parts of the declaration
                        // matched by the inner pattern above. It is embedded in
                        // one `$()*` pattern-repetition expander so that it
                        // repeats only for the part of each mod that the
                        // corresponding pattern-repetition matcher.
                        extern_function_helper!{$fn_str, $(#[$fn_attr])* fn $fn_id $args -> $ret}
                    )*
                }
            }
        )*
    };
}

// Here we invoke the x-macro passing generate_extern_modules as its callback macro.
call_macro_with_all_host_functions! { generate_extern_modules }
