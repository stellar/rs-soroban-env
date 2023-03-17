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

use crate::{err, host::Frame, xdr::ContractCostType, HostError};
use std::{cell::RefCell, io::Cursor, rc::Rc};

use super::{xdr::Hash, Host, RawVal, Symbol};
use func_info::HOST_FUNCTIONS;
use soroban_env_common::{
    meta::{self, get_ledger_protocol_version, get_pre_release_version},
    xdr::{ReadXdr, ScEnvMetaEntry, ScErrorCode, ScErrorType},
    ConversionError, SymbolStr, TryIntoVal, WasmiMarshal,
};

use wasmi::{Engine, Instance, Linker, Memory, Module, Store, Value};

#[cfg(any(test, feature = "testutils"))]
use soroban_env_common::{
    xdr::{ScVal, ScVec},
    TryFromVal, VmCaller,
};

impl wasmi::core::HostError for HostError {}

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
        // We check that the interface version number has the same pre-release number as
        // us as well as a protocol that's less than or equal to our protocol.

        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut cursor = Cursor::new(env_meta);
            if let Some(env_meta_entry) = ScEnvMetaEntry::read_xdr_iter(&mut cursor).next() {
                let ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) =
                    host.map_err(env_meta_entry)?;
                let got_pre = get_pre_release_version(v);
                let want_pre = get_pre_release_version(meta::INTERFACE_VERSION);
                let got_proto = get_ledger_protocol_version(v);
                let want_proto = get_ledger_protocol_version(meta::INTERFACE_VERSION);
                if got_pre != want_pre {
                    return Err(err!(
                        host,
                        (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                        "contract pre-release number does not match host",
                        got_pre,
                        want_pre
                    ));
                } else if got_proto > want_proto {
                    return Err(err!(
                        host,
                        (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                        "contract ledger protocol number exceeds host",
                        got_proto,
                        want_proto
                    ));
                } else {
                    return Ok(());
                }
            }
            Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "contract missing environment interface version",
                &[],
            ))
        } else {
            Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "contract missing metadata section",
                &[],
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
        // `VmInstantiation` is const cost in both cpu and mem. It has weak variance on
        host.charge_budget(
            ContractCostType::VmInstantiation,
            Some(module_wasm_code.len() as u64),
        )?;

        let mut config = wasmi::Config::default();

        // Turn off all optional wasm features.
        config.wasm_multi_value(false);
        config.wasm_mutable_global(false);
        config.wasm_saturating_float_to_int(false);
        config.wasm_sign_extension(false);
        config.floats(false);
        config.consume_fuel(true);

        let engine = Engine::new(&config);
        let module = host.map_err(Module::new(&engine, module_wasm_code))?;

        Self::check_meta_section(host, &module)?;

        let mut store = Store::new(&engine, host.clone());
        let mut linker = <Linker<Host>>::new(&engine);

        for hf in HOST_FUNCTIONS {
            let func = (hf.wrap)(&mut store);
            host.map_err(
                linker
                    .define(hf.mod_str, hf.fn_str, func)
                    .map_err(|le| wasmi::Error::Linker(le)),
            )?;
        }

        let not_started_instance = host.map_err(linker.instantiate(&mut store, &module))?;

        let instance = host.map_err(
            not_started_instance
                .ensure_no_start(&mut store)
                .map_err(|ie| wasmi::Error::Instantiation(ie)),
        )?;

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
            None => Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::MissingValue,
                "no linear memory named `memory`",
                &[],
            )),
        }
    }

    pub(crate) fn invoke_function_raw(
        self: &Rc<Self>,
        host: &Host,
        func_sym: &Symbol,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        host.charge_budget(ContractCostType::InvokeVmFunction, None)?;
        host.with_frame(
            Frame::ContractVM(self.clone(), *func_sym, args.to_vec()),
            || {
                let wasm_args: Vec<Value> = args
                    .iter()
                    .map(|i| Value::I64(i.get_payload() as i64))
                    .collect();
                let mut wasm_ret: [Value; 1] = [Value::I64(0)];
                let func_ss: SymbolStr = func_sym.try_into_val(host)?;
                let ext = match self
                    .instance
                    .get_export(&*self.store.borrow(), func_ss.as_ref())
                {
                    None => {
                        return Err(host.err(
                            ScErrorType::WasmVm,
                            ScErrorCode::MissingValue,
                            "invoking unknown export",
                            &[func_sym.to_raw()],
                        ))
                    }
                    Some(e) => e,
                };
                let func = match ext.into_func() {
                    None => {
                        return Err(host.err(
                            ScErrorType::WasmVm,
                            ScErrorCode::UnexpectedType,
                            "export is not a function",
                            &[func_sym.to_raw()],
                        ))
                    }
                    Some(e) => e,
                };

                let res = func.call(
                    &mut *self.store.borrow_mut(),
                    wasm_args.as_slice(),
                    &mut wasm_ret,
                );

                if let Err(e) = res {
                    // When a call fails with a wasmi::Error::Trap that carries a HostError
                    // we propagate that HostError as is, rather than producing something new.

                    match e {
                        wasmi::Error::Trap(trap) => {
                            if let Some(he) = trap.downcast::<HostError>() {
                                host.log_diagnostics(
                                    "VM call trapped with HostError",
                                    &[func_sym.to_raw(), he.error.to_raw()],
                                )?;
                                return Err(he);
                            }
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InternalError,
                                "VM trapped with HostError but propagation failed",
                                &[],
                            ));
                        }
                        e => {
                            return Err(if host.is_debug() {
                                // With diagnostics on: log as much detail as we can from wasmi.
                                let msg = format!("VM call failed: {:?}", &e);
                                host.error(e.into(), &msg, &[func_sym.to_raw()])
                            } else {
                                host.error(e.into(), "VM call failed", &[func_sym.to_raw()])
                            });
                        }
                    }
                }
                Ok(
                    <_ as WasmiMarshal>::try_marshal_from_value(wasm_ret[0].clone())
                        .ok_or(ConversionError)?,
                )
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
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn invoke_function(
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
        host.from_host_val(raw_res)
    }

    /// Returns a list of functions in the WASM module loaded into the [Vm].
    pub fn functions(&self) -> Vec<VmFunction> {
        let mut res = Vec::new();
        for e in self.module.exports() {
            if let wasmi::ExternType::Func(f) = e.ty() {
                res.push(VmFunction {
                    name: e.name().to_string(),
                    param_count: f.params().len(),
                    result_count: f.results().len(),
                })
            }
        }
        res
    }

    fn module_custom_section(_m: &Module, _name: impl AsRef<str>) -> Option<&[u8]> {
        todo!()
        /*
        m.custom_sections().iter().find_map(|s| {
            if &*s.name == name.as_ref() {
                Some(&*s.data)
            } else {
                None
            }
        })
        */
    }

    /// Returns the raw bytes content of a named custom section from the WASM
    /// module loaded into the [Vm], or `None` if no such custom section exists.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        Self::module_custom_section(&self.module, name)
    }

    // Utility function that synthesizes a `VmCaller<Host>` configured to point
    // to this VM's `Store` and `Instance`, and calls the provided function
    // back with it. Mainly used for testing.
    #[cfg(any(test, feature = "testutils"))]
    pub fn with_vmcaller<F, T>(&self, _f: F) -> T
    where
        F: FnOnce(&mut VmCaller<Host>) -> T,
    {
        // let store: &mut Store<Host> = &mut self.store.borrow_mut();
        // let mut ctx: StoreContextMut<Host> = store.into();
        // let caller: Caller<Host> = Caller::new(&mut ctx, Some(&self.instance));
        // let mut vmcaller: VmCaller<Host> = VmCaller(Some(caller));
        // f(&mut vmcaller)
        // TODO: they have made the Caller::new private. Need fork.
        todo!()
    }
}
