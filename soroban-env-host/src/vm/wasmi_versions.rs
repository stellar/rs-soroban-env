// We want to simultaneously switch all the types in the `vm` module and its
// support modules between wasmi 0.31 and 0.32. We do this by defining a trait
// that abstracts over the differences between the two versions, and then we
// define two structs that implement that trait for the two versions. We then
// use the trait as a type parameter in the structs and functions in the `vm`
// module, and define any helper functions that will vary by-version on the
// trait rather than as free functions.
//
// The other approach that might work here is macro-based (as we do elsewhere)
// but my hunch is that the ability to refer back through the trait to other
// types in the same version of wasmi will make it easier to parameterize this
// module than having to make the entire module a macro. We'll see.

use crate::{
    err,
    host::declared_size::DeclaredSizeForMetering,
    vm::{
        fuel_refillable::FuelRefillable, HostFuncInfo, McVer, ModuleCache, ParsedModule,
        RelativeObjectConversion, VersionedModuleCache, VersionedParsedModule, Vm, WasmiConfig,
    },
    xdr::{ScErrorCode, ScErrorType},
    Host, HostError, Symbol, WasmiMarshal031, WasmiMarshal036,
};
use std::{borrow::Cow, rc::Rc};

pub(crate) trait WasmiVersion: Sized {
    type Engine;
    type InstancePre;
    type Instance;
    type Module;
    type Linker;
    type Store: FuelRefillable;
    type Memory: Clone;
    type Value: DeclaredSizeForMetering + Clone;
    type Error: core::fmt::Debug + Into<crate::Error>;
    type Extern;
    type Func;

    type DispatchCaller<'a>;
    type DispatchFailure;

    const VALUE_ZERO: Self::Value;

    fn new_engine(config: &WasmiConfig) -> Self::Engine;
    fn new_linker(engine: &Self::Engine) -> Self::Linker;
    fn new_module(engine: &Self::Engine, wasm: &[u8]) -> Result<Self::Module, Self::Error>;
    fn get_module_engine(m: &Self::Module) -> &Self::Engine;
    fn module_custom_section(m: &Self::Module, name: impl AsRef<str>) -> Option<&[u8]>;
    fn module_func_imports(m: &Self::Module) -> impl Iterator<Item = (&str, &str)>;
    fn new_store(engine: &Self::Engine, host: Host) -> Self::Store;
    fn link_fn(linker: &mut Self::Linker, fi: &HostFuncInfo) -> Result<(), Self::Error>;
    fn upcast_versioned_parsed_module(vpm: &Rc<VersionedParsedModule<Self>>) -> ParsedModule;
    fn check_max_args(host: &Host, m: &Self::Module) -> Result<(), HostError>;
    fn linker_instantiate(
        linker: &Self::Linker,
        store: &mut Self::Store,
        module: &Self::Module,
    ) -> Result<Self::InstancePre, Self::Error>;
    fn ensure_no_start(
        instance: Self::InstancePre,
        store: &mut Self::Store,
    ) -> Result<Self::Instance, Self::Error>;
    fn get_export(
        instance: &Self::Instance,
        store: &Self::Store,
        name: &str,
    ) -> Option<Self::Extern>;
    fn export_to_memory(export: Self::Extern) -> Option<Self::Memory>;
    fn export_to_func(export: Self::Extern) -> Option<Self::Func>;
    fn call_func(
        func: &Self::Func,
        store: &mut Self::Store,
        inputs: &[Self::Value],
        outputs: &mut [Self::Value],
    ) -> Result<(), Self::Error>;
    fn handle_call_error(host: &Host, func_sym: &Symbol, e: Self::Error) -> HostError;
    fn downcast_module_cache<'mc>(
        host: &Host,
        mc: &'mc ModuleCache,
    ) -> Result<&'mc Rc<VersionedModuleCache<Self>>, HostError>;
    fn val_to_value(val: crate::Val) -> Self::Value;
    fn try_value_to_val(value: Self::Value) -> Option<crate::Val>;

    fn try_marshal_from_relative_value<T: RelativeObjectConversion>(
        value: Self::Value,
        host: &Host,
    ) -> Result<T, Self::DispatchFailure>;
    fn marshal_relative_from_self<T: RelativeObjectConversion>(
        val: T,
        host: &Host,
    ) -> Result<Self::Value, Self::DispatchFailure>;
}

pub(crate) struct Wasmi031;
pub(crate) struct Wasmi036;

fn check_max_args(host: &Host, results_len: usize, params_len: usize) -> Result<(), HostError> {
    if results_len > Vm::MAX_VM_ARGS {
        return Err(err!(
            host,
            (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
            "Too many return values in Wasm export",
            results_len
        ));
    }
    if params_len > Vm::MAX_VM_ARGS {
        return Err(err!(
            host,
            (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
            "Too many arguments in Wasm export",
            params_len
        ));
    }
    Ok(())
}

impl WasmiVersion for Wasmi031 {
    type Engine = wasmi_031::Engine;
    type InstancePre = wasmi_031::InstancePre;
    type Instance = wasmi_031::Instance;
    type Module = wasmi_031::Module;
    type Linker = wasmi_031::Linker<Host>;
    type Store = wasmi_031::Store<Host>;
    type Memory = wasmi_031::Memory;
    type Value = wasmi_031::Value;
    type Error = wasmi_031::Error;
    type Extern = wasmi_031::Extern;
    type Func = wasmi_031::Func;
    type DispatchCaller<'a> = wasmi_031::Caller<'a, Host>;
    type DispatchFailure = wasmi_031::core::Trap;

    const VALUE_ZERO: Self::Value = wasmi_031::Value::I64(0);

    fn new_engine(config: &WasmiConfig) -> Self::Engine {
        Self::Engine::new(&config.config_031)
    }
    fn new_linker(engine: &Self::Engine) -> Self::Linker {
        Self::Linker::new(engine)
    }

    fn new_module(engine: &Self::Engine, wasm: &[u8]) -> Result<Self::Module, Self::Error> {
        Self::Module::new(engine, wasm)
    }
    fn new_store(engine: &Self::Engine, host: Host) -> Self::Store {
        let mut store = Self::Store::new(engine, host);
        store.limiter(|h| h);
        store
    }
    fn link_fn(linker: &mut Self::Linker, fi: &HostFuncInfo) -> Result<(), Self::Error> {
        (fi.wrap_031)(linker)?;
        Ok(())
    }

    fn upcast_versioned_parsed_module(vpm: &Rc<VersionedParsedModule<Self>>) -> ParsedModule {
        vpm.clone().into()
    }

    fn check_max_args(host: &Host, m: &Self::Module) -> Result<(), HostError> {
        for e in m.exports() {
            match e.ty() {
                wasmi_031::ExternType::Func(f) => {
                    check_max_args(host, f.results().len(), f.params().len())?
                }
                _ => (),
            }
        }
        Ok(())
    }

    fn linker_instantiate(
        linker: &Self::Linker,
        store: &mut Self::Store,
        module: &Self::Module,
    ) -> Result<Self::InstancePre, Self::Error> {
        linker.instantiate(store, module)
    }

    fn ensure_no_start(
        instance: Self::InstancePre,
        store: &mut Self::Store,
    ) -> Result<Self::Instance, Self::Error> {
        instance
            .ensure_no_start(store)
            .map_err(|ie| Self::Error::Instantiation(ie))
    }

    fn get_export(
        instance: &Self::Instance,
        store: &Self::Store,
        name: &str,
    ) -> Option<Self::Extern> {
        instance.get_export(store, name)
    }

    fn export_to_memory(export: Self::Extern) -> Option<Self::Memory> {
        export.into_memory()
    }

    fn export_to_func(export: Self::Extern) -> Option<Self::Func> {
        export.into_func()
    }

    fn call_func(
        func: &Self::Func,
        store: &mut Self::Store,
        inputs: &[Self::Value],
        outputs: &mut [Self::Value],
    ) -> Result<(), Self::Error> {
        func.call(store, inputs, outputs)
    }

    fn handle_call_error(host: &Host, func_sym: &Symbol, e: Self::Error) -> HostError {
        match e {
            wasmi_031::Error::Trap(trap) => {
                if let Some(code) = trap.trap_code() {
                    let err = code.into();
                    let mut msg = Cow::Borrowed("VM call trapped");
                    host.with_debug_mode(|| {
                        msg = Cow::Owned(format!("VM call trapped: {:?}", &code));
                        Ok(())
                    });
                    return host.error(err, &msg, &[func_sym.to_val()]);
                }
                if let Some(he) = trap.downcast::<HostError>() {
                    host.log_diagnostics(
                        "VM call trapped with HostError",
                        &[func_sym.to_val(), he.error.to_val()],
                    );
                    return he;
                }
                return host.err(
                    ScErrorType::WasmVm,
                    ScErrorCode::InternalError,
                    "VM trapped but propagation failed",
                    &[],
                );
            }
            e => {
                let mut msg = Cow::Borrowed("VM call failed");
                host.with_debug_mode(|| {
                    msg = Cow::Owned(format!("VM call failed: {:?}", &e));
                    Ok(())
                });
                return host.error(e.into(), &msg, &[func_sym.to_val()]);
            }
        }
    }

    fn downcast_module_cache<'mc>(
        host: &Host,
        mc: &'mc ModuleCache,
    ) -> Result<&'mc Rc<VersionedModuleCache<Self>>, HostError> {
        match &mc.0 {
            McVer::Mc031(vmc) => Ok(vmc),
            _ => Err(err!(
                host,
                (ScErrorType::WasmVm, ScErrorCode::InternalError),
                "ModuleCache is not the expected version",
                []
            )),
        }
    }

    fn get_module_engine(m: &Self::Module) -> &Self::Engine {
        m.engine()
    }

    fn val_to_value(val: crate::Val) -> Self::Value {
        WasmiMarshal031::marshal_from_self(val)
    }

    fn try_value_to_val(value: Self::Value) -> Option<crate::Val> {
        <crate::Val as WasmiMarshal031>::try_marshal_from_value(value)
    }

    fn module_custom_section(m: &Self::Module, name: impl AsRef<str>) -> Option<&[u8]> {
        m.custom_sections().iter().find_map(|s| {
            if &*s.name == name.as_ref() {
                Some(&*s.data)
            } else {
                None
            }
        })
    }

    fn module_func_imports(m: &Self::Module) -> impl Iterator<Item = (&str, &str)> {
        m.imports().filter_map(|i| {
            if i.ty().func().is_some() {
                let mod_str = i.module();
                let fn_str = i.name();
                Some((mod_str, fn_str))
            } else {
                None
            }
        })
    }
    fn try_marshal_from_relative_value<T: RelativeObjectConversion>(
        value: Self::Value,
        host: &Host,
    ) -> Result<T, Self::DispatchFailure> {
        T::try_marshal_from_relative_value_031(value, host)
    }
    fn marshal_relative_from_self<T: RelativeObjectConversion>(
        val: T,
        host: &Host,
    ) -> Result<Self::Value, Self::DispatchFailure> {
        T::marshal_relative_from_self_031(val, host)
    }
}

impl WasmiVersion for Wasmi036 {
    type Engine = wasmi_036::Engine;
    type InstancePre = wasmi_036::InstancePre;
    type Instance = wasmi_036::Instance;
    type Module = wasmi_036::Module;
    type Linker = wasmi_036::Linker<Host>;
    type Store = wasmi_036::Store<Host>;
    type Memory = wasmi_036::Memory;
    type Value = wasmi_036::Val;
    type Error = wasmi_036::Error;
    type Extern = wasmi_036::Extern;
    type Func = wasmi_036::Func;

    type DispatchCaller<'a> = wasmi_036::Caller<'a, Host>;
    type DispatchFailure = wasmi_036::Error;

    const VALUE_ZERO: Self::Value = wasmi_036::Val::I64(0);

    fn new_engine(config: &WasmiConfig) -> Self::Engine {
        Self::Engine::new(&config.config_036)
    }
    fn new_linker(engine: &Self::Engine) -> Self::Linker {
        Self::Linker::new(engine)
    }
    fn new_module(engine: &Self::Engine, wasm: &[u8]) -> Result<Self::Module, Self::Error> {
        Self::Module::new(engine, wasm)
    }
    fn new_store(engine: &Self::Engine, host: Host) -> Self::Store {
        let mut store = Self::Store::new(engine, host);
        store.limiter(|h| h);
        store
    }

    fn link_fn(linker: &mut Self::Linker, fi: &HostFuncInfo) -> Result<(), Self::Error> {
        (fi.wrap_036)(linker)?;
        Ok(())
    }

    fn upcast_versioned_parsed_module(vpm: &Rc<VersionedParsedModule<Self>>) -> ParsedModule {
        vpm.clone().into()
    }
    fn check_max_args(host: &Host, m: &Self::Module) -> Result<(), HostError> {
        for e in m.exports() {
            match e.ty() {
                wasmi_036::ExternType::Func(f) => {
                    check_max_args(host, f.results().len(), f.params().len())?
                }
                _ => (),
            }
        }
        Ok(())
    }
    fn linker_instantiate(
        linker: &Self::Linker,
        store: &mut Self::Store,
        module: &Self::Module,
    ) -> Result<Self::InstancePre, Self::Error> {
        linker.instantiate(store, module)
    }

    fn ensure_no_start(
        instance: Self::InstancePre,
        store: &mut Self::Store,
    ) -> Result<Self::Instance, Self::Error> {
        instance.ensure_no_start(store).map_err(|ie| ie.into())
    }
    fn get_export(
        instance: &Self::Instance,
        store: &Self::Store,
        name: &str,
    ) -> Option<Self::Extern> {
        instance.get_export(store, name)
    }

    fn export_to_memory(export: Self::Extern) -> Option<Self::Memory> {
        export.into_memory()
    }

    fn export_to_func(export: Self::Extern) -> Option<Self::Func> {
        export.into_func()
    }

    fn call_func(
        func: &Self::Func,
        store: &mut Self::Store,
        inputs: &[Self::Value],
        outputs: &mut [Self::Value],
    ) -> Result<(), Self::Error> {
        func.call(store, inputs, outputs)
    }

    fn handle_call_error(host: &Host, func_sym: &Symbol, e: Self::Error) -> HostError {
        if let Some(code) = e.as_trap_code() {
            let err = code.into();
            let mut msg = Cow::Borrowed("VM call trapped");
            host.with_debug_mode(|| {
                msg = Cow::Owned(format!("VM call trapped: {:?}", &code));
                Ok(())
            });
            host.error(err, &msg, &[func_sym.to_val()])
        } else if let Some(_) = e.downcast_ref::<HostError>() {
            let Some(he) = e.downcast::<HostError>() else {
                return host.err(
                    ScErrorType::WasmVm,
                    ScErrorCode::InternalError,
                    "downcast failed during VM failure handling",
                    &[],
                );
            };
            host.log_diagnostics(
                "VM call trapped with HostError",
                &[func_sym.to_val(), he.error.to_val()],
            );
            he
        } else {
            let mut msg = Cow::Borrowed("VM call failed");
            host.with_debug_mode(|| {
                msg = Cow::Owned(format!("VM call failed: {:?}", &e));
                Ok(())
            });
            host.error(e.into(), &msg, &[func_sym.to_val()])
        }
    }

    fn downcast_module_cache<'mc>(
        host: &Host,
        mc: &'mc ModuleCache,
    ) -> Result<&'mc Rc<VersionedModuleCache<Self>>, HostError> {
        match &mc.0 {
            McVer::Mc036(vmc) => Ok(vmc),
            _ => Err(err!(
                host,
                (ScErrorType::WasmVm, ScErrorCode::InternalError),
                "ModuleCache is not the expected version",
                []
            )),
        }
    }

    fn get_module_engine(m: &Self::Module) -> &Self::Engine {
        m.engine()
    }

    fn val_to_value(val: crate::Val) -> Self::Value {
        WasmiMarshal036::marshal_from_self(val)
    }

    fn try_value_to_val(value: Self::Value) -> Option<crate::Val> {
        <crate::Val as WasmiMarshal036>::try_marshal_from_value(value)
    }

    fn module_custom_section(m: &Self::Module, name: impl AsRef<str>) -> Option<&[u8]> {
        m.exports();
        m.custom_sections().find_map(|s| {
            if s.name() == name.as_ref() {
                Some(s.data())
            } else {
                None
            }
        })
    }
    fn module_func_imports(m: &Self::Module) -> impl Iterator<Item = (&str, &str)> {
        m.imports().filter_map(|i| {
            if i.ty().func().is_some() {
                let mod_str = i.module();
                let fn_str = i.name();
                Some((mod_str, fn_str))
            } else {
                None
            }
        })
    }
    fn try_marshal_from_relative_value<T: RelativeObjectConversion>(
        value: Self::Value,
        host: &Host,
    ) -> Result<T, Self::DispatchFailure> {
        T::try_marshal_from_relative_value_036(value, host)
    }
    fn marshal_relative_from_self<T: RelativeObjectConversion>(
        val: T,
        host: &Host,
    ) -> Result<Self::Value, Self::DispatchFailure> {
        T::marshal_relative_from_self_036(val, host)
    }
}
