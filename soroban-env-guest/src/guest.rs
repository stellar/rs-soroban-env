use core::convert::Infallible;

use soroban_env_common::call_macro_with_all_host_functions;

use super::{
    AddressObject, Bool, BytesObject, DurationObject, Error, I128Object, I256Object, I256Val,
    I64Object, MapObject, StorageType, StringObject, SymbolObject, TimepointObject, U128Object,
    U256Object, U256Val, U32Val, U64Object, U64Val, Val, VecObject, Void,
};
use super::{Env, EnvBase, Symbol};
use static_assertions as sa;

// Just the smallest possible version of a runtime assertion-or-panic.
#[inline(always)]
fn require(b: bool) {
    if !b {
        core::arch::wasm32::unreachable()
    }
}

/// The [Guest] is the implementation of the [Env] interface seen and used by
/// contracts built into WASM for execution within a WASM VM. It is a 0-sized
/// "stub" type implementation of the [Env] interface that forwards each [Env]
/// method to an external function, imported from its runtime environment. This
/// implementation is automatically generated and has no interesting content.
#[derive(Copy, Clone, Default)]
pub struct Guest;

impl EnvBase for Guest {
    type Error = Infallible;

    fn error_from_error_val(&self, _e: crate::Error) -> Self::Error {
        core::arch::wasm32::unreachable()
    }

    #[cfg(feature = "testutils")]
    fn escalate_error_to_panic(&self, _e: Self::Error) -> ! {
        core::arch::wasm32::unreachable()
    }

    fn check_same_env(&self, _other: &Self) -> Result<(), Self::Error> {
        Ok(())
    }

    fn bytes_copy_from_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &[u8],
    ) -> Result<BytesObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        // NB: any failure in the host function here will trap the guest,
        // not return, so we only have to code the happy path.
        self.bytes_copy_from_linear_memory(b, b_pos, lm_pos, len)
    }

    fn bytes_copy_to_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.bytes_copy_to_linear_memory(b, b_pos, lm_pos, len)?;
        Ok(())
    }

    fn string_copy_to_slice(
        &self,
        b: StringObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.string_copy_to_linear_memory(b, b_pos, lm_pos, len)?;
        Ok(())
    }

    fn symbol_copy_to_slice(
        &self,
        b: SymbolObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.symbol_copy_to_linear_memory(b, b_pos, lm_pos, len)?;
        Ok(())
    }

    fn bytes_new_from_slice(&self, slice: &[u8]) -> Result<BytesObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.bytes_new_from_linear_memory(lm_pos, len)
    }

    fn string_new_from_slice(&self, slice: &[u8]) -> Result<StringObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.string_new_from_linear_memory(lm_pos, len)
    }

    fn symbol_new_from_slice(&self, slice: &[u8]) -> Result<SymbolObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let lm_pos: U32Val = Val::from_u32(slice.as_ptr() as u32);
        let len: U32Val = Val::from_u32(slice.len() as u32);
        self.symbol_new_from_linear_memory(lm_pos, len)
    }

    fn map_new_from_slices(&self, keys: &[&str], vals: &[Val]) -> Result<MapObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        require(keys.len() == vals.len());
        let keys_lm_pos: U32Val = Val::from_u32(keys.as_ptr() as u32);
        let vals_lm_pos: U32Val = Val::from_u32(vals.as_ptr() as u32);
        let len: U32Val = Val::from_u32(keys.len() as u32);
        self.map_new_from_linear_memory(keys_lm_pos, vals_lm_pos, len)
    }

    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [Val],
    ) -> Result<Void, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        require(keys.len() == vals.len());
        let keys_lm_pos: U32Val = Val::from_u32(keys.as_ptr() as u32);
        let vals_lm_pos: U32Val = Val::from_u32(vals.as_ptr() as u32);
        let len: U32Val = Val::from_u32(keys.len() as u32);
        self.map_unpack_to_linear_memory(map, keys_lm_pos, vals_lm_pos, len)
    }

    fn vec_new_from_slice(&self, vals: &[Val]) -> Result<VecObject, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let vals_lm_pos: U32Val = Val::from_u32(vals.as_ptr() as u32);
        let len: U32Val = Val::from_u32(vals.len() as u32);
        self.vec_new_from_linear_memory(vals_lm_pos, len)
    }

    fn vec_unpack_to_slice(&self, vec: VecObject, vals: &mut [Val]) -> Result<Void, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let vals_lm_pos: U32Val = Val::from_u32(vals.as_ptr() as u32);
        let len: U32Val = Val::from_u32(vals.len() as u32);
        self.vec_unpack_to_linear_memory(vec, vals_lm_pos, len)
    }

    fn symbol_index_in_strs(&self, key: Symbol, strs: &[&str]) -> Result<U32Val, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let strs_lm_pos: U32Val = Val::from_u32(strs.as_ptr() as u32);
        let len: U32Val = Val::from_u32(strs.len() as u32);
        self.symbol_index_in_linear_memory(key, strs_lm_pos, len)
    }

    fn log_from_slice(&self, msg: &str, vals: &[Val]) -> Result<Void, Self::Error> {
        sa::assert_eq_size!(u32, *const u8);
        sa::assert_eq_size!(u32, usize);
        let msg_lm_pos: U32Val = Val::from_u32(msg.as_ptr() as u32);
        let msg_lm_len: U32Val = Val::from_u32(msg.len() as u32);
        let vals_lm_pos: U32Val = Val::from_u32(vals.as_ptr() as u32);
        let vals_lm_len: U32Val = Val::from_u32(vals.len() as u32);
        self.log_from_linear_memory(msg_lm_pos, msg_lm_len, vals_lm_pos, vals_lm_len)
    }

    fn check_protocol_version_lower_bound(&self, _lower_bound: u32) -> Result<(), Self::Error> {
        core::arch::wasm32::unreachable()
    }

    fn check_protocol_version_upper_bound(&self, _upper_bound: u32) -> Result<(), Self::Error> {
        core::arch::wasm32::unreachable()
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
        fn $fn_id(&self, $($arg:$type),*) -> Result<$ret, Self::Error>{
            unsafe {
                Ok($mod_id::$fn_id($($arg),*))
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
                    { $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident $args:tt -> $ret:ty }
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
        $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?,
        $(#[$attr:meta])*
        fn $fn_id:ident($($arg:ident:$type:ty),*) -> $ret:ty
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
                    { $fn_str:literal, $($min_proto:literal)?, $($max_proto:literal)?, fn $fn_id:ident $args:tt -> $ret:ty }
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
                use crate::{Val,Object,Symbol,Error,MapObject,VecObject,BytesObject};
                #[allow(unused_imports)]
                use crate::{I128Object, I256Object, I256Val, I64Object, I64Val, U128Object, U256Object, U256Val, U32Val, U64Object, U64Val, StorageType, TimepointObject, DurationObject};
                #[allow(unused_imports)]
                use crate::{Void,AddressObject,SymbolObject,StringObject,Bool};
                #[link(wasm_import_module = $mod_str)]
                extern "C" {
                    $(
                        // This invokes the extern_function_helper! macro above
                        // passing only the relevant parts of the declaration
                        // matched by the inner pattern above. It is embedded in
                        // one `$()*` pattern-repetition expander so that it
                        // repeats only for the part of each mod that the
                        // corresponding pattern-repetition matcher.
                        extern_function_helper!{$fn_str, $($min_proto)?, $($max_proto)?, $(#[$fn_attr])* fn $fn_id $args -> $ret}
                    )*
                }
            }
        )*
    };
}

// Here we invoke the x-macro passing generate_extern_modules as its callback macro.
call_macro_with_all_host_functions! { generate_extern_modules }
