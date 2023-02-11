//! This module primarily provides the [Vm] type and the necessary name-lookup
//! and runtime-dispatch mechanisms needed to allow WASM modules to call into
//! the [Env](crate::Env) interface implemented by [Host].
//!
//! It also contains helper methods to look up and call into contract functions
//! in terms of [ScVal] and [RawVal] arguments.
//!
//! The implementation of WASM types and the WASM bytecode interpreter come from
//! the [wasmi](https://github.com/paritytech/wasmi) project.

mod dispatch;
mod func_info;

use crate::{
    budget::CostType,
    host::{Frame, HostImpl},
    HostError, VmCaller,
};
use std::{cell::RefCell, io::Cursor, ops::RangeInclusive, rc::Rc};

use super::{
    xdr::{Hash, ScVal, ScVec},
    Host, RawVal, Symbol,
};
use func_info::HOST_FUNCTIONS;
use soroban_env_common::{
    meta,
    xdr::{ReadXdr, ScEnvMetaEntry, ScHostFnErrorCode, ScVmErrorCode},
    ConversionError, SymbolStr, TryFromVal, TryIntoVal,
};

use wasmi::{
    core::Value, Caller, Engine, Instance, Linker, Memory, Module, StepMeter, Store,
    StoreContextMut,
};

impl wasmi::core::HostError for HostError {}

impl StepMeter for HostImpl {
    fn max_insn_step(&self) -> u64 {
        256
    }

    fn charge_cpu(&self, insns: u64) -> Result<(), wasmi::core::TrapCode> {
        // TODO reconcile TrapCode with HostError better.
        self.budget
            .clone()
            .charge(CostType::WasmInsnExec, insns)
            .map_err(|_| wasmi::core::TrapCode::CpuLimitExceeded)
    }

    fn charge_mem(&self, bytes: u64) -> Result<(), wasmi::core::TrapCode> {
        self.budget
            .clone()
            .charge(CostType::WasmMemAlloc, bytes)
            .map_err(|_| wasmi::core::TrapCode::MemLimitExceeded)
    }
}

/// A [Vm] is a thin wrapper around an instance of [wasmi::Module]. Multiple
/// [Vm]s may be held in a single [Host], and each contains a single WASM module
/// instantiation.
///
/// [Vm] rejects modules with either floating point or start functions.
///
/// [Vm] is configured to use its [Host] as a source of WASM imports.
/// Specifically [Host] implements [wasmi::ImportResolver] by resolving all and
/// only the functions declared in [Env](crate::Env) as imports, if requested by the
/// WASM module. Any other lookups on any tables other than import functions
/// will fail.
pub struct Vm {
    #[allow(dead_code)]
    pub(crate) contract_id: Hash,
    // TODO: consider moving store and possibly module to Host so they can be
    // recycled across calls. Or possibly beyond, to be recycled across txs.
    module: Module,
    store: RefCell<Store<Host>>,
    instance: Instance,
    memory: Option<Memory>,
}

/// Minimal description of a single function defined in a WASM module.
#[derive(Clone, Eq, PartialEq)]
pub struct VmFunction {
    pub name: String,
    pub param_count: usize,
    pub result_count: usize,
}

impl Vm {
    fn check_meta_section(host: &Host, m: &Module) -> Result<(), HostError> {
        // At present the supported interface-version range is always just a single
        // point, and it is hard-wired into the host as the current
        // `soroban_env_common` value [`meta::INTERFACE_VERSION`]. In the future when
        // we commit to API stability two things will change:
        //
        //   1. The value will stop being hard-wired; it will change based on the
        //      current ledger, as a config value that varies over time based on
        //      consensus.
        //
        //   2. It will (mostly) have a fixed lower bound and only ever have its upper
        //      bound expand, since that is what "API stability" means: old code still
        //      runs on new hosts. The "mostly" qualifier here covers the case where we
        //      have to reset the lower bound to expire old APIs (used by old
        //      contracts) if they prove to be a security risk; this will only happen
        //      in extreme cases, hopefully never.
        const SUPPORTED_INTERFACE_VERSION_RANGE: RangeInclusive<u64> =
            meta::INTERFACE_VERSION..=meta::INTERFACE_VERSION;

        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut cursor = Cursor::new(env_meta);
            for env_meta_entry in ScEnvMetaEntry::read_xdr_iter(&mut cursor) {
                match host.map_err(env_meta_entry)? {
                    ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) => {
                        if SUPPORTED_INTERFACE_VERSION_RANGE.contains(&v) {
                            return Ok(());
                        } else {
                            return Err(host.err_status_msg(
                                ScHostFnErrorCode::InputArgsInvalid,
                                "unexpected environment interface version",
                            ));
                        }
                    }
                    #[allow(unreachable_patterns)]
                    _ => (),
                }
            }
            Err(host.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "missing environment interface version",
            ))
        } else {
            Err(host.err_status_msg(
                ScHostFnErrorCode::InputArgsInvalid,
                "input contract missing metadata section",
            ))
        }
    }

    /// Constructs a new instance of a [Vm] within the provided [Host],
    /// establishing a new execution context for a contract identified by
    /// `contract_id` with WASM bytecode provided in `module_wasm_code`.
    ///
    /// This function performs several steps:
    ///
    ///   - Parses and performs WASM validation on the module.
    ///   - Checks that the module contains an [meta::INTERFACE_VERSION] that
    ///     matches the host.
    ///   - Checks that the module has no floating point code or `start`
    ///     function, or post-MVP wasm extensions.
    ///   - Instantiates the module, leaving it ready to accept function
    ///     invocations.
    ///   - Looks up and caches its linear memory export named `memory`
    ///     if it exists.
    ///
    /// This method is called automatically as part of [Host::invoke_function]
    /// and does not usually need to be called from outside the crate.
    pub fn new(
        host: &Host,
        contract_id: Hash,
        module_wasm_code: &[u8],
    ) -> Result<Rc<Self>, HostError> {
        host.charge_budget(CostType::VmInstantiation, module_wasm_code.len() as u64)?;

        let mut config = wasmi::Config::default();

        // Turn off all optional wasm features.
        config.wasm_multi_value(false);
        config.wasm_mutable_global(false);
        config.wasm_saturating_float_to_int(false);
        config.wasm_sign_extension(false);

        // This should always be true, and it enforces wasmi's notion of "deterministic only"
        // execution, which excludes all floating point ops. Double check to be sure.
        assert!(config.wasm_features().deterministic_only);

        let engine = Engine::new(&config);
        let module = host.map_err(Module::new(&engine, module_wasm_code))?;

        Self::check_meta_section(host, &module)?;

        let mut store = Store::new(&engine, host.clone());
        store.set_step_meter(host.0.clone());
        let mut linker = <Linker<Host>>::new();

        for hf in HOST_FUNCTIONS {
            let func = (hf.wrap)(&mut store);
            linker.define(hf.mod_str, hf.fn_str, func).map_err(|_| {
                host.err_status_msg(ScVmErrorCode::Instantiation, "error defining host function")
            })?;
        }

        let not_started_instance = host.map_err(linker.instantiate(&mut store, &module))?;

        let instance = not_started_instance
            .ensure_no_start(&mut store)
            .map_err(|_| {
                host.err_status_msg(
                    ScVmErrorCode::Instantiation,
                    "module contains disallowed start function",
                )
            })?;

        let memory = if let Some(ext) = instance.get_export(&mut store, "memory") {
            ext.into_memory()
        } else {
            None
        };

        let store = RefCell::new(store);
        Ok(Rc::new(Self {
            contract_id,
            module,
            store,
            instance,
            memory,
        }))
    }

    pub(crate) fn get_memory(&self, host: &Host) -> Result<Memory, HostError> {
        match self.memory {
            Some(mem) => Ok(mem),
            None => {
                Err(host.err_status_msg(ScVmErrorCode::Memory, "no linear memory named `memory`"))
            }
        }
    }

    pub(crate) fn invoke_function_raw(
        self: &Rc<Self>,
        host: &Host,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        host.charge_budget(CostType::InvokeVmFunction, 1)?;
        host.with_frame(
            Frame::ContractVM(self.clone(), *func, args.to_vec()),
            || {
                let wasm_args: Vec<Value> = args
                    .iter()
                    .map(|i| Value::I64(i.get_payload() as i64))
                    .collect();
                let mut wasm_ret: [Value; 1] = [Value::I64(0)];
                let func_ss: SymbolStr = func.try_into_val(host)?;
                let ext = match self
                    .instance
                    .get_export(&*self.store.borrow(), func_ss.as_ref())
                {
                    None => {
                        return Err(
                            host.err_status_msg(ScVmErrorCode::Unknown, "invoking unknown export")
                        )
                    }
                    Some(e) => e,
                };
                let func = match ext.into_func() {
                    None => {
                        return Err(
                            host.err_status_msg(ScVmErrorCode::Unknown, "export is not a function")
                        )
                    }
                    Some(e) => e,
                };
                host.map_err(func.call(
                    &mut *self.store.borrow_mut(),
                    wasm_args.as_slice(),
                    &mut wasm_ret,
                ))?;
                Ok(wasm_ret[0].try_into().ok_or(ConversionError)?)
            },
        )
    }

    /// Invokes a function in the VM's module, converting externally stable XDR
    /// [ScVal] arguments into [Host]-specific [RawVal]s and converting the
    /// [RawVal] returned from the invocation back to an [ScVal].
    ///
    /// This function, like [Vm::new], is called as part of
    /// [Host::invoke_function], and does not usually need to be called manually
    /// from outside the crate.
    //
    // NB: This function has to take self by [Rc] because it stores self in
    // a new Frame
    pub fn invoke_function(
        self: &Rc<Self>,
        host: &Host,
        func: &str,
        args: &ScVec,
    ) -> Result<ScVal, HostError> {
        let func_sym = Symbol::try_from_val(host, &func)?;
        let mut raw_args: Vec<RawVal> = Vec::new();
        for scv in args.0.iter() {
            raw_args.push(host.to_host_val(scv)?);
        }
        let raw_res = self.invoke_function_raw(host, &func_sym, raw_args.as_slice())?;
        Ok(host.from_host_val(raw_res)?)
    }

    /// Returns a list of functions in the WASM module loaded into the [Vm].
    pub fn functions(&self) -> Vec<VmFunction> {
        let mut res = Vec::new();
        for e in self.module.exports() {
            if let wasmi::ExportItemKind::Func(f) = e.kind() {
                res.push(VmFunction {
                    name: e.name().to_string(),
                    param_count: f.params().len(),
                    result_count: f.results().len(),
                })
            }
        }
        res
    }

    fn module_custom_section(m: &Module, name: impl AsRef<str>) -> Option<&[u8]> {
        m.custom_sections().iter().find_map(|s| {
            if &*s.name == name.as_ref() {
                Some(&*s.data)
            } else {
                None
            }
        })
    }

    /// Returns the raw bytes content of a named custom section from the WASM
    /// module loaded into the [Vm], or `None` if no such custom section exists.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        Self::module_custom_section(&self.module, name)
    }

    /// Utility function that synthesizes a `VmCaller<Host>` configured to point
    /// to this VM's `Store` and `Instance`, and calls the provided function
    /// back with it. Mainly used for testing.
    pub fn with_vmcaller<F, T>(&self, f: F) -> T
    where
        F: FnOnce(&mut VmCaller<Host>) -> T,
    {
        let store: &mut Store<Host> = &mut *self.store.borrow_mut();
        let mut ctx: StoreContextMut<Host> = store.into();
        let caller: Caller<Host> = Caller::new(&mut ctx, Some(self.instance));
        let mut vmcaller: VmCaller<Host> = VmCaller(Some(caller));
        f(&mut vmcaller)
    }
}
