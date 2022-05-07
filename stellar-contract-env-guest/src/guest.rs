#![allow(dead_code)]
#![allow(unused_variables)]

use stellar_contract_env_common::call_macro_with_all_host_functions;

use super::{Env, EnvBase, RawVal};

// In guest code the environment is global/implicit, so is represented as a unit struct.
#[derive(Copy, Clone, Default)]
pub struct Guest;

impl EnvBase for Guest {
    fn as_mut_any(&mut self) -> &mut dyn core::any::Any {
        return self;
    }

    fn check_same_env(&self, other: &Self) {
        ()
    }
}

macro_rules! host_function {
    {$mod_name:ident, fn $func_id:ident(&self)() -> $ret:ty} =>
    {fn $func_id(&self) -> $ret { unsafe { $mod_name::$func_id() }}};

    {$mod_name:ident, fn $func_id:ident(&self)($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {fn $func_id(&self, $($arg:$type),*) -> $ret { unsafe { $mod_name::$func_id($($arg),*)}}};

    {$mod_name:ident, fn $func_id:ident(&mut self)() -> $ret:ty} =>
    {fn $func_id(&mut self) -> $ret { unsafe { $mod_name::$func_id() }}};

    {$mod_name:ident, fn $func_id:ident(&mut self)($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {fn $func_id(&mut self, $($arg:$type),*) -> $ret { unsafe { $mod_name::$func_id($($arg),*)}}};
}

macro_rules! impl_env_for_guest {
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
        impl Env for Guest
        {
            $(
                $(
                    host_function!{$mod_name, fn $func_id $selfspec $args -> $ret}
                )*
            )*
        }
    };
}

call_macro_with_all_host_functions! { impl_env_for_guest }

macro_rules! host_function {
    {$field_str:literal, fn $func_id:ident($($arg:ident:$type:ty),*) -> $ret:ty} =>
    {
        #[cfg_attr(target_family = "wasm", link_name = $field_str)]
        pub(crate) fn $func_id($($arg:$type),*) -> $ret;
    };
}

macro_rules! generate_extern_modules {
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
        $(
            mod $mod_name {
                #[allow(unused_imports)]
                use crate::{RawVal,RawObj};
                #[link(wasm_import_module = $mod_str)]
                extern "C" {
                    $(
                        host_function!{$field_str, fn $func_id $args -> $ret}
                    )*
                }
            }
        )*
    };
}

call_macro_with_all_host_functions! { generate_extern_modules }
