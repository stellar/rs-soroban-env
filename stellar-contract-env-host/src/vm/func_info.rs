use super::dispatch;
use crate::Host;
use stellar_contract_env_common::call_macro_with_all_host_functions;
use wasmi::{RuntimeArgs, RuntimeValue};

pub(crate) struct HostFuncInfo {
    pub(crate) mod_name: &'static str,
    pub(crate) field_name: &'static str,
    pub(crate) arity: usize,
    pub(crate) dispatch: fn(&mut Host, RuntimeArgs) -> Result<RuntimeValue, wasmi::Trap>,
}

macro_rules! arity_of {
    { () } => { 0 };
    { ($a0:ident:$t0:ty) } => { 1 };
    { ($a0:ident:$t0:ty, $a1:ident:$t1:ty) } => { 2 };
    { ($a0:ident:$t0:ty, $a1:ident:$t1:ty, $a2:ident:$t2:ty) } => { 3 };
    { ($a0:ident:$t0:ty, $a1:ident:$t1:ty, $a2:ident:$t2:ty, $a3:ident:$t3:ty) } => { 4 };
    { ($a0:ident:$t0:ty, $a1:ident:$t1:ty, $a2:ident:$t2:ty, $a3:ident:$t3:ty, $a4:ident:$t4:ty) } => { 5 };
    { ($a0:ident:$t0:ty, $a1:ident:$t1:ty, $a2:ident:$t2:ty, $a3:ident:$t3:ty, $a4:ident:$t4:ty, $a5:ident:$t5:ty) } => { 6 };
}

macro_rules! generate_host_function_infos {
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
        &[
           $(
                $(
                    HostFuncInfo {
                        mod_name: $mod_str,
                        field_name: $field_str,
                        arity: arity_of!{$args},
                        dispatch: dispatch::$func_id,
                    },
                )*
            )*
        ]
    };
}

pub(crate) static HOST_FUNCTIONS: &[HostFuncInfo] =
    call_macro_with_all_host_functions! { generate_host_function_infos };
