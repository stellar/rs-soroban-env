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
mod module_cache;
mod parsed_module;

#[cfg(feature = "bench")]
pub(crate) use dispatch::dummy0;
#[cfg(test)]
pub(crate) use dispatch::protocol_gated_dummy;

use crate::{
    budget::{get_wasmi_config, AsBudget, Budget},
    host::{
        error::TryBorrowOrErr,
        metered_clone::MeteredContainer,
        metered_hash::{CountingHasher, MeteredHash},
    },
    xdr::{ContractCostType, Hash, ScErrorCode, ScErrorType},
    ConversionError, Host, HostError, Symbol, SymbolStr, TryIntoVal, Val, WasmiMarshal,
};
use std::{cell::RefCell, collections::BTreeSet, rc::Rc};

use fuel_refillable::FuelRefillable;
use func_info::HOST_FUNCTIONS;

pub use module_cache::ModuleCache;
pub use parsed_module::{ParsedModule, VersionedContractCodeCostInputs};

use wasmi::{Instance, Linker, Memory, Store, Value};

use crate::VmCaller;
use wasmi::{Caller, StoreContextMut};

impl wasmi::core::HostError for HostError {}

const MAX_VM_ARGS: usize = 32;
#[cfg(feature = "next")]
const WASM_STD_MEM_PAGE_SIZE_IN_BYTES: u32 = 0x10000;

struct VmInstantiationTimer {
    #[cfg(not(target_family = "wasm"))]
    host: Host,
    #[cfg(not(target_family = "wasm"))]
    start: std::time::Instant,
}
impl VmInstantiationTimer {
    fn new(_host: Host) -> Self {
        VmInstantiationTimer {
            #[cfg(not(target_family = "wasm"))]
            host: _host,
            #[cfg(not(target_family = "wasm"))]
            start: std::time::Instant::now(),
        }
    }
}
#[cfg(not(target_family = "wasm"))]
impl Drop for VmInstantiationTimer {
    fn drop(&mut self) {
        let _ = self.host.as_budget().track_time(
            ContractCostType::VmInstantiation,
            self.start.elapsed().as_nanos() as u64,
        );
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
    pub(crate) contract_id: Hash,
    #[allow(dead_code)]
    pub(crate) module: Rc<ParsedModule>,
    store: RefCell<Store<Host>>,
    instance: Instance,
    pub(crate) memory: Option<Memory>,
}

impl std::hash::Hash for Vm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.contract_id.hash(state);
    }
}

impl Vm {
    #[cfg(feature = "testutils")]
    pub fn get_all_host_functions() -> Vec<(&'static str, &'static str, u32)> {
        HOST_FUNCTIONS
            .iter()
            .map(|hf| (hf.mod_str, hf.fn_str, hf.arity))
            .collect()
    }

    /// Instantiates a VM given the arguments provided in [`Self::new`],
    /// or [`Self::new_from_module_cache`]
    fn instantiate(
        host: &Host,
        contract_id: Hash,
        parsed_module: Rc<ParsedModule>,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::instantiate");

        let engine = parsed_module.module.engine();
        let mut linker = <Linker<Host>>::new(engine);
        let mut store = Store::new(engine, host.clone());

        parsed_module.cost_inputs.charge_for_instantiation(host)?;

        store.limiter(|host| host);

        let module_imports: BTreeSet<(&str, &str)> = parsed_module
            .module
            .imports()
            .filter(|i| i.ty().func().is_some())
            .map(|i| {
                let mod_str = i.module();
                let fn_str = i.name();
                (mod_str, fn_str)
            })
            .collect();

        {
            // We perform link-time protocol version gating here.
            // Reasons for doing link-time instead of run-time check:
            // 1. VM instantiation is performed in both contract upload and
            //    execution, thus any errorous contract will be rejected at
            //    upload time.
            // 2. If a contract contains a call to an outdated host function,
            //    i.e. `contract_protocol > hf.max_supported_protocol`, failing
            //    early is preferred from resource usage perspective.
            // 3. If a contract contains a call to an non-existent host
            //    function, the current (correct) behavior is to return
            //    `Wasmi::LinkerError::MissingDefinition` error (which gets
            //    converted to a `(WasmVm, InvalidAction)`). If that host
            //    function is defined in a later protocol, and we replay that
            //    contract (in the earlier protocol where it belongs), not
            //    linking the function preserves the right behavior and error
            //    code.
            let _span0 = tracy_span!("define host functions");
            let ledger_proto = host.with_ledger_info(|li| Ok(li.protocol_version))?;
            for hf in HOST_FUNCTIONS {
                if let Some(min_proto) = hf.min_proto {
                    if parsed_module.proto_version < min_proto || ledger_proto < min_proto {
                        // We skip linking this hf instead of returning an error
                        // because we have to support old contracts during replay.
                        continue;
                    }
                }
                if let Some(max_proto) = hf.max_proto {
                    if parsed_module.proto_version > max_proto || ledger_proto > max_proto {
                        // We skip linking this hf instead of returning an error
                        // because we have to support old contracts during replay.
                        continue;
                    }
                }
                // We only link the functions that are actually used by the
                // contract. Linking is quite expensive.
                if !module_imports.contains(&(hf.mod_str, hf.fn_str)) {
                    continue;
                }
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
            host.map_err(linker.instantiate(&mut store, &parsed_module.module))?
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
        Ok(Rc::new(Self {
            contract_id,
            module: parsed_module,
            store: RefCell::new(store),
            instance,
            memory,
        }))
    }

    pub fn from_parsed_module(
        host: &Host,
        contract_id: Hash,
        parsed_module: Rc<ParsedModule>,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::from_parsed_module");
        VmInstantiationTimer::new(host.clone());
        Self::instantiate(host, contract_id, parsed_module)
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

    pub fn new(host: &Host, contract_id: Hash, wasm: &[u8]) -> Result<Rc<Self>, HostError> {
        let cost_inputs = VersionedContractCodeCostInputs::V0 {
            wasm_bytes: wasm.len(),
        };
        Self::new_with_cost_inputs(host, contract_id, wasm, cost_inputs)
    }

    pub fn new_with_cost_inputs(
        host: &Host,
        contract_id: Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::new");
        VmInstantiationTimer::new(host.clone());
        let config = get_wasmi_config(host.as_budget())?;
        let engine = wasmi::Engine::new(&config);
        let parsed_module = Rc::new(ParsedModule::new(host, &engine, wasm, cost_inputs)?);
        Self::instantiate(host, contract_id, parsed_module)
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

        if inputs.len() > MAX_VM_ARGS {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "Too many arguments in wasm invocation",
                &[func_sym.to_val()],
            ));
        }

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
            use std::borrow::Cow;

            // When a call fails with a wasmi::Error::Trap that carries a HostError
            // we propagate that HostError as is, rather than producing something new.

            match e {
                wasmi::Error::Trap(trap) => {
                    if let Some(code) = trap.trap_code() {
                        let err = code.into();
                        let mut msg = Cow::Borrowed("VM call trapped");
                        host.with_debug_mode(|| {
                            msg = Cow::Owned(format!("VM call trapped: {:?}", &code));
                            Ok(())
                        });
                        return Err(host.error(err, &msg, &[func_sym.to_val()]));
                    }
                    if let Some(he) = trap.downcast::<HostError>() {
                        host.log_diagnostics(
                            "VM call trapped with HostError",
                            &[func_sym.to_val(), he.error.to_val()],
                        );
                        return Err(he);
                    }
                    return Err(host.err(
                        ScErrorType::WasmVm,
                        ScErrorCode::InternalError,
                        "VM trapped but propagation failed",
                        &[],
                    ));
                }
                e => {
                    let mut msg = Cow::Borrowed("VM call failed");
                    host.with_debug_mode(|| {
                        msg = Cow::Owned(format!("VM call failed: {:?}", &e));
                        Ok(())
                    });
                    return Err(host.error(e.into(), &msg, &[func_sym.to_val()]));
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

    /// Returns the raw bytes content of a named custom section from the WASM
    /// module loaded into the [Vm], or `None` if no such custom section exists.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        self.module.custom_section(name)
    }

    /// Utility function that synthesizes a `VmCaller<Host>` configured to point
    /// to this VM's `Store` and `Instance`, and calls the provided function
    /// back with it. Mainly used for testing.
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

    #[cfg(feature = "bench")]
    pub(crate) fn with_caller<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(Caller<Host>) -> Result<T, HostError>,
    {
        let store: &mut Store<Host> = &mut *self.store.try_borrow_mut_or_err()?;
        let mut ctx: StoreContextMut<Host> = store.into();
        let caller: Caller<Host> = Caller::new(&mut ctx, Some(&self.instance));
        f(caller)
    }

    pub(crate) fn memory_hash_and_size(&self, budget: &Budget) -> Result<(u64, usize), HostError> {
        use std::hash::Hasher;
        if let Some(mem) = self.memory {
            self.with_vmcaller(|vmcaller| {
                let mut state = CountingHasher::default();
                let data = mem.data(vmcaller.try_ref()?);
                data.metered_hash(&mut state, budget)?;
                Ok((state.finish(), data.len()))
            })
        } else {
            Ok((0, 0))
        }
    }

    // This is pretty weak: we just observe the state that wasmi exposes through
    // wasm _exports_. There might be tables or globals a wasm doesn't export
    // but there's no obvious way to observe them.
    pub(crate) fn exports_hash_and_size(&self, budget: &Budget) -> Result<(u64, usize), HostError> {
        use std::hash::Hasher;
        use wasmi::{Extern, StoreContext};
        self.with_vmcaller(|vmcaller| {
            let ctx: StoreContext<'_, _> = vmcaller.try_ref()?.into();
            let mut size: usize = 0;
            let mut state = CountingHasher::default();
            for export in self.instance.exports(vmcaller.try_ref()?) {
                size = size.saturating_add(1);
                export.name().metered_hash(&mut state, budget)?;

                match export.into_extern() {
                    // Funcs are immutable, memory we hash separately above.
                    Extern::Func(_) | Extern::Memory(_) => (),

                    Extern::Table(t) => {
                        let sz = t.size(&ctx);
                        sz.metered_hash(&mut state, budget)?;
                        size = size.saturating_add(sz as usize);
                        for i in 0..sz {
                            if let Some(elem) = t.get(&ctx, i) {
                                // This is a slight fudge to avoid having to
                                // define a ton of additional MeteredHash impls
                                // for wasmi substructures, since there is a
                                // bounded size on the string representation of
                                // a value, we're comfortable going temporarily
                                // over budget here.
                                let s = format!("{:?}", elem);
                                budget.charge(ContractCostType::MemAlloc, Some(s.len() as u64))?;
                                s.metered_hash(&mut state, budget)?;
                            }
                        }
                    }
                    Extern::Global(g) => {
                        let s = format!("{:?}", g.get(&ctx));
                        budget.charge(ContractCostType::MemAlloc, Some(s.len() as u64))?;
                        s.metered_hash(&mut state, budget)?;
                    }
                }
            }
            Ok((state.finish(), size))
        })
    }
}
