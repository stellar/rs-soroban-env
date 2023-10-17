//! This module primarily provides the [Vm] type and the necessary name-lookup
//! and runtime-dispatch mechanisms needed to allow WASM modules to call into
//! the [Env](crate::Env) interface implemented by [Host].
//!
//! It also contains helper methods to look up and call into contract functions
//! in terms of [ScVal] and [Val] arguments.
//!
//! The implementation of WASM types and the WASM bytecode interpreter come from
//! the [wasmi](https://github.com/paritytech/wasmi) project.

mod dispatch;
mod fuel_refillable;
mod func_info;

#[cfg(any(test, feature = "testutils"))]
pub(crate) use dispatch::dummy0;

use crate::{
    budget::AsBudget,
    err,
    host::{error::TryBorrowOrErr, metered_clone::MeteredContainer},
    xdr::ContractCostType,
    HostError,
};
use std::{cell::RefCell, io::Cursor, rc::Rc};

use super::{xdr::Hash, Host, Symbol, Val};
use fuel_refillable::FuelRefillable;
use func_info::HOST_FUNCTIONS;
use soroban_env_common::{
    meta::{self, get_ledger_protocol_version, get_pre_release_version},
    xdr::{
        DepthLimitedRead, ReadXdr, ScEnvMetaEntry, ScErrorCode, ScErrorType,
        DEFAULT_XDR_RW_DEPTH_LIMIT,
    },
    ConversionError, SymbolStr, TryIntoVal, WasmiMarshal,
};

use wasmi::{Engine, FuelConsumptionMode, Instance, Linker, Memory, Module, Store, Value};

#[cfg(any(test, feature = "testutils"))]
use crate::VmCaller;
#[cfg(any(test, feature = "testutils"))]
use wasmi::{Caller, StoreContextMut};
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
    fn check_contract_interface_version(
        host: &Host,
        interface_version: u64,
    ) -> Result<(), HostError> {
        let want_proto = {
            let ledger_proto = host.get_ledger_protocol_version()?;
            let env_proto = get_ledger_protocol_version(meta::INTERFACE_VERSION);
            if ledger_proto <= env_proto {
                // ledger proto should be before or equal to env proto
                ledger_proto
            } else {
                return Err(err!(
                    host,
                    (ScErrorType::Context, ScErrorCode::InternalError),
                    "ledger protocol number is ahead of supported env protocol number",
                    ledger_proto,
                    env_proto
                ));
            }
        };

        let got_pre = get_pre_release_version(interface_version);
        let got_proto = get_ledger_protocol_version(interface_version);

        if got_proto < want_proto {
            // Old protocols are finalized, we only support contracts
            // with similarly finalized (zero) prerelease numbers.
            if got_pre != 0 {
                return Err(err!(
                    host,
                    (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                    "contract pre-release number for old protocol is nonzero",
                    got_pre
                ));
            }
        } else if got_proto == want_proto {
            // Current protocol might have a nonzero prerelease number; we will
            // allow it only if it matches the current prerelease exactly.
            let want_pre = get_pre_release_version(meta::INTERFACE_VERSION);
            if want_pre != got_pre {
                return Err(err!(
                    host,
                    (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                    "contract pre-release number for current protocol does not match host",
                    got_pre,
                    want_pre
                ));
            }
        } else {
            // Future protocols we don't allow. It might be nice (in the sense
            // of "allowing uploads of a future-protocol contract that will go
            // live as soon as the network upgrades to it") but there's a risk
            // that the "future" protocol semantics baked in to a contract
            // differ from the final semantics chosen by the network, so to be
            // conservative we avoid even allowing this.
            return Err(err!(
                host,
                (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                "contract protocol number is newer than host",
                got_proto
            ));
        }
        Ok(())
    }

    fn check_meta_section(host: &Host, m: &Module) -> Result<(), HostError> {
        // We check that the interface version number has the same pre-release number as
        // us as well as a protocol that's less than or equal to our protocol.

        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut cursor =
                DepthLimitedRead::new(Cursor::new(env_meta), DEFAULT_XDR_RW_DEPTH_LIMIT);
            if let Some(env_meta_entry) = ScEnvMetaEntry::read_xdr_iter(&mut cursor).next() {
                let ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) =
                    host.map_err(env_meta_entry)?;
                Vm::check_contract_interface_version(host, v)
            } else {
                Err(host.err(
                    ScErrorType::WasmVm,
                    ScErrorCode::InvalidInput,
                    "contract missing environment interface version",
                    &[],
                ))
            }
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
        let _span = tracy_span!("Vm::new");

        host.charge_budget(
            ContractCostType::VmInstantiation,
            Some(module_wasm_code.len() as u64),
        )?;

        let mut config = wasmi::Config::default();
        let fuel_costs = host.as_budget().wasmi_fuel_costs()?;

        // Turn off all optional wasm features.
        config
            .wasm_multi_value(false)
            .wasm_mutable_global(true)
            .wasm_saturating_float_to_int(false)
            .wasm_sign_extension(true)
            .floats(false)
            .consume_fuel(true)
            .fuel_consumption_mode(FuelConsumptionMode::Eager)
            .set_fuel_costs(fuel_costs);

        let engine = Engine::new(&config);
        let module = {
            let _span0 = tracy_span!("parse module");
            host.map_err(Module::new(&engine, module_wasm_code))?
        };

        Self::check_meta_section(host, &module)?;

        let mut store = Store::new(&engine, host.clone());
        store.limiter(|host| host);

        let mut linker = <Linker<Host>>::new(&engine);

        {
            let _span0 = tracy_span!("define host functions");
            for hf in HOST_FUNCTIONS {
                let func = (hf.wrap)(&mut store);
                host.map_err(
                    linker
                        .define(hf.mod_str, hf.fn_str, func)
                        .map_err(|le| wasmi::Error::Linker(le)),
                )?;
            }
        }

        let not_started_instance = {
            let _span0 = tracy_span!("instantiate module");
            host.map_err(linker.instantiate(&mut store, &module))?
        };

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

        // Here we do _not_ supply the store with any fuel. Fuel is supplied
        // right before the VM is being run, i.e., before crossing the host->VM
        // boundary.
        #[cfg(any(test, feature = "testutils"))]
        host.call_any_lifecycle_hook(crate::host::HostLifecycleEvent::VmInstantiated)?;
        Ok(Rc::new(Self {
            contract_id,
            module,
            store: RefCell::new(store),
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

    // Wrapper for the [`Func`] call which is metered as a component.
    // Resolves the function entity, and takes care the conversion between and
    // tranfering of the host budget / VM fuel. This is where the host->VM->host
    // boundaries are crossed.
    pub(crate) fn metered_func_call(
        self: &Rc<Self>,
        host: &Host,
        func_sym: &Symbol,
        inputs: &[Value],
    ) -> Result<Val, HostError> {
        host.charge_budget(ContractCostType::InvokeVmFunction, None)?;

        // resolve the function entity to be called
        let func_ss: SymbolStr = func_sym.try_into_val(host)?;
        let ext = match self
            .instance
            .get_export(&*self.store.try_borrow_or_err()?, func_ss.as_ref())
        {
            None => {
                return Err(host.err(
                    ScErrorType::WasmVm,
                    ScErrorCode::MissingValue,
                    "invoking unknown export",
                    &[func_sym.to_val()],
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
                    &[func_sym.to_val()],
                ))
            }
            Some(e) => e,
        };

        // call the function
        let mut wasm_ret: [Value; 1] = [Value::I64(0)];
        self.store.try_borrow_mut_or_err()?.add_fuel_to_vm(host)?;
        // Metering: the `func.call` will trigger `wasmi::Call` (or `CallIndirect`) instruction,
        // which is technically covered by wasmi fuel metering. So we are double charging a bit
        // here (by a few 100s cpu insns). It is better to be safe.
        let res = func.call(
            &mut *self.store.try_borrow_mut_or_err()?,
            inputs,
            &mut wasm_ret,
        );
        // Due to the way wasmi's fuel metering works (it does `remaining.checked_sub(delta).ok_or(Trap)`),
        // there may be a small amount of fuel (less than delta -- the fuel cost of that failing
        // wasmi instruction) remaining when the `OutOfFuel` trap occurs. This is only observable
        // if the contract traps with `OutOfFuel`, which may appear confusing if they look closely
        // at the budget amount consumed. So it should be fine.
        self.store
            .try_borrow_mut_or_err()?
            .return_fuel_to_host(host)?;

        if let Err(e) = res {
            // When a call fails with a wasmi::Error::Trap that carries a HostError
            // we propagate that HostError as is, rather than producing something new.

            match e {
                wasmi::Error::Trap(trap) => {
                    if let Some(code) = trap.trap_code() {
                        let err = code.into();
                        return Err(if host.is_debug()? {
                            // With diagnostics on: log as much detail as we can from wasmi.
                            let msg = format!("VM call trapped: {:?}", &code);
                            host.error(err, &msg, &[func_sym.to_val(), err.to_val()])
                        } else {
                            err.into()
                        });
                    }
                    if let Some(he) = trap.downcast::<HostError>() {
                        host.log_diagnostics(
                            "VM call trapped with HostError",
                            &[func_sym.to_val(), he.error.to_val()],
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
                    return Err(if host.is_debug()? {
                        // With diagnostics on: log as much detail as we can from wasmi.
                        let msg = format!("VM call failed: {:?}", &e);
                        host.error(e.into(), &msg, &[func_sym.to_val()])
                    } else {
                        host.error(e.into(), "VM call failed", &[func_sym.to_val()])
                    });
                }
            }
        }
        host.relative_to_absolute(
            Val::try_marshal_from_value(wasm_ret[0].clone()).ok_or(ConversionError)?,
        )
    }

    pub(crate) fn invoke_function_raw(
        self: &Rc<Self>,
        host: &Host,
        func_sym: &Symbol,
        args: &[Val],
    ) -> Result<Val, HostError> {
        let _span = tracy_span!("Vm::invoke_function_raw");
        Vec::<Value>::charge_bulk_init_cpy(args.len() as u64, host.as_budget())?;
        let wasm_args: Vec<Value> = args
            .iter()
            .map(|i| host.absolute_to_relative(*i).map(|v| v.marshal_from_self()))
            .collect::<Result<Vec<Value>, HostError>>()?;
        self.metered_func_call(host, func_sym, wasm_args.as_slice())
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
    #[cfg(any(test, feature = "testutils"))]
    #[allow(unused)]
    pub(crate) fn with_vmcaller<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&mut VmCaller<Host>) -> Result<T, HostError>,
    {
        let store: &mut Store<Host> = &mut *self.store.try_borrow_mut_or_err()?;
        let mut ctx: StoreContextMut<Host> = store.into();
        let caller: Caller<Host> = Caller::new(&mut ctx, Some(&self.instance));
        let mut vmcaller: VmCaller<Host> = VmCaller(Some(caller));
        f(&mut vmcaller)
    }

    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn with_caller<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(Caller<Host>) -> Result<T, HostError>,
    {
        let store: &mut Store<Host> = &mut *self.store.try_borrow_mut_or_err()?;
        let mut ctx: StoreContextMut<Host> = store.into();
        let caller: Caller<Host> = Caller::new(&mut ctx, Some(&self.instance));
        f(caller)
    }
}
