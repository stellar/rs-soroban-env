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
pub(crate) use dispatch::dispatch_031::dummy0 as dummy0_031;
#[cfg(feature = "bench")]
pub(crate) use dispatch::dispatch_034::dummy0 as dummy0_034;

// FIXME: re-enable when fixing protocol_gate tests
// #[cfg(test)]
// pub(crate) use dispatch::dispatch_031::protocol_gated_dummy;

use crate::{
    budget::{get_wasmi_config, AsBudget, Budget, WasmiConfig},
    host::{
        error::TryBorrowOrErr,
        metered_clone::MeteredContainer,
        metered_hash::{CountingHasher, MeteredHash},
    },
    xdr::{ContractCostType, Hash, ScErrorCode, ScErrorType},
    ConversionError, Host, HostError, Symbol, SymbolStr, TryIntoVal, Val,
};
use std::{cell::RefCell, collections::BTreeSet, rc::Rc};

use fuel_refillable::FuelRefillable;
use func_info::{HostFuncInfo, HOST_FUNCTIONS};

pub use module_cache::ModuleCache;
use module_cache::{McVer, VersionedModuleCache};
pub use parsed_module::{ParsedModule, VersionedContractCodeCostInputs};

#[cfg(feature = "bench")]
pub(crate) use parsed_module::{PmVer, VersionedParsedModule};
#[cfg(not(feature = "bench"))]
use parsed_module::{PmVer, VersionedParsedModule};

#[cfg(feature = "bench")]
use crate::VmCaller;

impl wasmi_031::core::HostError for HostError {}
impl wasmi_034::core::HostError for HostError {}

mod wasmi_versions;
#[cfg(feature = "bench")]
pub(crate) use wasmi_versions::{Wasmi031, Wasmi034, WasmiVersion};
#[cfg(not(feature = "bench"))]
use wasmi_versions::{Wasmi031, Wasmi034, WasmiVersion};

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

/// A [Vm] is a thin wrapper around an Instance of a Wasm Module. Multiple
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

#[derive(Clone)]
pub struct Vm(pub(crate) VmVer);

#[derive(Clone)]
pub(crate) enum VmVer {
    Vm031(Rc<VersionedVm<Wasmi031>>),
    Vm034(Rc<VersionedVm<Wasmi034>>),
}

impl From<Rc<VersionedVm<Wasmi031>>> for Vm {
    fn from(vm: Rc<VersionedVm<Wasmi031>>) -> Self {
        Vm(VmVer::Vm031(vm))
    }
}

impl From<Rc<VersionedVm<Wasmi034>>> for Vm {
    fn from(vm: Rc<VersionedVm<Wasmi034>>) -> Self {
        Vm(VmVer::Vm034(vm))
    }
}

const WASMI_034_PROTOCOL_VERSION: u32 = 22;

pub(crate) struct VersionedVm<V: WasmiVersion> {
    pub(crate) contract_id: Hash,
    #[allow(dead_code)]
    module: Rc<VersionedParsedModule<V>>,
    store: RefCell<V::Store>,
    instance: V::Instance,
    memory: Option<V::Memory>,
}

impl std::hash::Hash for Vm {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.get_contract_id().hash(state);
    }
}

impl Host {
    pub(crate) fn make_linker<V: WasmiVersion>(
        engine: &V::Engine,
        symbols: &BTreeSet<(&str, &str)>,
    ) -> Result<V::Linker, HostError> {
        let mut linker = V::new_linker(&engine);
        for hf in HOST_FUNCTIONS {
            if symbols.contains(&(hf.mod_str, hf.fn_str)) {
                V::link_fn(&mut linker, hf)
                    .map_err(|e| <V::Error as Into<crate::Error>>::into(e))?
            }
        }
        Ok(linker)
    }
}

// In one very narrow context -- when recording, and with a module cache -- we
// defer the cost of parsing a module until we pop a control frame.
// Unfortunately we have to thread this information from the call site to here.
// See comment below where this type is used.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ModuleParseCostMode {
    Normal,
    #[cfg(any(test, feature = "recording_mode"))]
    PossiblyDeferredIfRecording,
}

impl Vm {
    pub const MAX_VM_ARGS: usize = 32;

    pub fn protocol_uses_legacy_stack_vm(protocol: u32) -> bool {
        protocol < WASMI_034_PROTOCOL_VERSION
    }

    pub fn from_parsed_module(
        host: &Host,
        contract_id: Hash,
        parsed_module: &ParsedModule,
    ) -> Result<Self, HostError> {
        match &parsed_module.0 {
            PmVer::Pm031(pm031) => {
                VersionedVm::<Wasmi031>::from_parsed_module(host, contract_id, pm031)
                    .map(Into::into)
            }
            PmVer::Pm034(pm034) => {
                VersionedVm::<Wasmi034>::from_parsed_module(host, contract_id, pm034)
                    .map(Into::into)
            }
        }
    }

    pub fn new(host: &Host, contract_id: Hash, wasm: &[u8]) -> Result<Self, HostError> {
        if Vm::protocol_uses_legacy_stack_vm(host.get_ledger_protocol_version()?) {
            VersionedVm::<Wasmi031>::new(host, contract_id, wasm).map(Into::into)
        } else {
            VersionedVm::<Wasmi034>::new(host, contract_id, wasm).map(Into::into)
        }
    }

    pub(crate) fn invoke_function_raw(
        &self,
        host: &Host,
        func_sym: &Symbol,
        args: &[Val],
    ) -> Result<Val, HostError> {
        match &self.0 {
            VmVer::Vm031(vm) => vm.invoke_function_raw(host, func_sym, args),
            VmVer::Vm034(vm) => vm.invoke_function_raw(host, func_sym, args),
        }
    }

    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        match &self.0 {
            VmVer::Vm031(vm) => vm.custom_section(name),
            VmVer::Vm034(vm) => vm.custom_section(name),
        }
    }

    pub(crate) fn get_contract_id(&self) -> &Hash {
        match &self.0 {
            VmVer::Vm031(vm) => &vm.contract_id,
            VmVer::Vm034(vm) => &vm.contract_id,
        }
    }
    pub(crate) fn new_with_cost_inputs(
        host: &Host,
        contract_id: Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
        cost_mode: ModuleParseCostMode,
    ) -> Result<Self, HostError> {
        if Vm::protocol_uses_legacy_stack_vm(host.get_ledger_protocol_version()?) {
            VersionedVm::<Wasmi031>::new_with_cost_inputs(
                host,
                contract_id,
                wasm,
                cost_inputs,
                cost_mode,
            )
            .map(Into::into)
        } else {
            VersionedVm::<Wasmi034>::new_with_cost_inputs(
                host,
                contract_id,
                wasm,
                cost_inputs,
                cost_mode,
            )
            .map(Into::into)
        }
    }

    pub(crate) fn get_module(&self) -> ParsedModule {
        match &self.0 {
            VmVer::Vm031(vm) => vm.module.clone().into(),
            VmVer::Vm034(vm) => vm.module.clone().into(),
        }
    }

    #[cfg(feature = "testutils")]
    pub fn get_all_host_functions() -> Vec<(&'static str, &'static str, u32)> {
        HOST_FUNCTIONS
            .iter()
            .map(|hf| (hf.mod_str, hf.fn_str, hf.arity))
            .collect()
    }

    #[cfg(feature = "testutils")]
    #[allow(clippy::type_complexity)]
    pub fn get_all_host_functions_with_supported_protocol_range(
    ) -> Vec<(&'static str, &'static str, u32, Option<u32>, Option<u32>)> {
        HOST_FUNCTIONS
            .iter()
            .map(|hf| (hf.mod_str, hf.fn_str, hf.arity, hf.min_proto, hf.max_proto))
            .collect()
    }
}

impl<V: WasmiVersion> VersionedVm<V> {
    /// Instantiates a VM given the arguments provided in [`Self::new`],
    /// or [`Self::new_from_module_cache`]
    fn instantiate(
        host: &Host,
        contract_id: Hash,
        parsed_module: &Rc<VersionedParsedModule<V>>,
        linker: &V::Linker,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::instantiate");

        let engine = V::get_module_engine(&parsed_module.module);
        let mut store = V::new_store(engine, host.clone());

        parsed_module.cost_inputs.charge_for_instantiation(host)?;

        {
            // We perform instantiation-time protocol version gating of
            // all module-imported symbols here.
            // Reasons for doing link-time instead of run-time check:
            // 1. VM instantiation is performed in both contract upload and
            //    execution, thus any errorous contract will be rejected at
            //    upload time.
            // 2. If a contract contains a call to an outdated host function,
            //    i.e. `contract_protocol > hf.max_supported_protocol`, failing
            //    early is preferred from resource usage perspective.
            // 3. If a contract contains a call to an non-existent host
            //    function, the current (correct) behavior is to return
            //    `Wasmi::errors::LinkerError::MissingDefinition` error (which gets
            //    converted to a `(WasmVm, InvalidAction)`). If that host
            //    function is defined in a later protocol, and we replay that
            //    contract (in the earlier protocol where it belongs), we need
            //    to return the same error.
            let _span0 = tracy_span!("define host functions");
            let ledger_proto = host.with_ledger_info(|li| Ok(li.protocol_version))?;
            parsed_module.with_import_symbols(host, |module_symbols| {
                for hf in HOST_FUNCTIONS {
                    if !module_symbols.contains(&(hf.mod_str, hf.fn_str)) {
                        continue;
                    }
                    if let Some(min_proto) = hf.min_proto {
                        if parsed_module.proto_version < min_proto || ledger_proto < min_proto {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidAction,
                                "contract calls a host function not yet supported by current protocol",
                                &[],
                            ));
                        }
                    }
                    if let Some(max_proto) = hf.max_proto {
                        if parsed_module.proto_version > max_proto || ledger_proto > max_proto {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidAction,
                                "contract calls a host function no longer supported in the current protocol",
                                &[],
                            ));
                        }
                    }
                }
                Ok(())
            })?;
        }

        let not_started_instance = {
            let _span0 = tracy_span!("instantiate module");
            host.map_err(V::linker_instantiate(
                linker,
                &mut store,
                &parsed_module.module,
            ))?
        };

        let instance = host.map_err(V::ensure_no_start(not_started_instance, &mut store))?;

        let memory = if let Some(ext) = V::get_export(&instance, &mut store, "memory") {
            V::export_to_memory(ext)
        } else {
            None
        };

        // Here we do _not_ supply the store with any fuel. Fuel is supplied
        // right before the VM is being run, i.e., before crossing the host->VM
        // boundary.
        Ok(Rc::new(Self {
            contract_id,
            module: parsed_module.clone(),
            store: RefCell::new(store),
            instance,
            memory,
        }))
    }

    fn from_parsed_module(
        host: &Host,
        contract_id: Hash,
        parsed_module: &Rc<VersionedParsedModule<V>>,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::from_parsed_module");
        VmInstantiationTimer::new(host.clone());
        if let Some(module_cache) = &*host.try_borrow_module_cache()? {
            let linker = &V::downcast_module_cache(host, module_cache)?.linker;
            Self::instantiate(host, contract_id, parsed_module, linker)
        } else {
            let linker = parsed_module.make_linker(host)?;
            Self::instantiate(host, contract_id, parsed_module, &linker)
        }
    }

    /// Constructs a new instance of a [Vm] within the provided [Host],
    /// establishing a new execution context for a contract identified by
    /// `contract_id` with Wasm bytecode provided in `module_wasm_code`.
    ///
    /// This function performs several steps:
    ///
    ///   - Parses and performs Wasm validation on the module.
    ///   - Checks that the module contains an [meta::INTERFACE_VERSION] that
    ///     matches the host.
    ///   - Checks that the module has no floating point code or `start`
    ///     function, or post-MVP wasm extensions.
    ///   - Instantiates the module, leaving it ready to accept function
    ///     invocations.
    ///   - Looks up and caches its linear memory export named `memory`
    ///     if it exists.
    ///
    /// With the introduction of the granular cost inputs this method
    /// should only be used for the one-off full parses of the new Wasms
    /// during the initial upload verification.

    fn new(host: &Host, contract_id: Hash, wasm: &[u8]) -> Result<Rc<Self>, HostError> {
        let cost_inputs = VersionedContractCodeCostInputs::V0 {
            wasm_bytes: wasm.len(),
        };
        Self::new_with_cost_inputs(
            host,
            contract_id,
            wasm,
            cost_inputs,
            ModuleParseCostMode::Normal,
        )
    }

    pub(crate) fn new_with_cost_inputs(
        host: &Host,
        contract_id: Hash,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
        cost_mode: ModuleParseCostMode,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::new");
        VmInstantiationTimer::new(host.clone());
        let parsed_module = Self::parse_module(host, wasm, cost_inputs, cost_mode)?;
        let linker = parsed_module.make_linker(host)?;
        Self::instantiate(host, contract_id, &parsed_module, &linker)
    }

    #[cfg(not(any(test, feature = "recording_mode")))]
    fn parse_module(
        host: &Host,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
        _cost_mode: ModuleParseCostMode,
    ) -> Result<Rc<VersionedParsedModule<V>>, HostError> {
        VersionedParsedModule::new_with_isolated_engine(host, wasm, cost_inputs)
    }

    /// This method exists to support [crate::storage::FootprintMode::Recording]
    /// when running in protocol versions that feature the [ModuleCache].
    ///
    /// There are two ways we can get to here:
    ///
    ///   1. When we're running in a protocol that doesn't support the
    ///   [ModuleCache] at all. In this case, we just parse the module and
    ///   charge for it as normal.
    ///
    ///   2. When we're in a protocol that _does_ support the [ModuleCache] but
    ///   are _also_ in [crate::storage::FootprintMode::Recording] mode and
    ///   _also_ being instantiated from [Host::call_contract_fn]. Then the
    ///   [ModuleCache] _did not get built_ during host setup (because we had
    ///   no footprint yet to buid the cache from), so our caller
    ///   [Host::call_contract_fn] sees no module cache, and so each call winds
    ///   up calling us here, reparsing each module as it's called, and then
    ///   throwing it away.
    ///
    /// When we are in case 2, we don't want to charge for all those reparses:
    /// we want to charge only for the post-parse instantiations _as if_ we had
    /// had the cache. The cache will actually be added in [Host::pop_context]
    /// _after_ a top-level recording-mode invocation completes, by reading the
    /// storage and parsing all the modules in it, in order to charge for
    /// parsing each used module _once_ and thereby produce a mostly-correct
    /// total cost.
    ///
    /// We still charge the reparses to the shadow budget, to avoid a DoS risk,
    /// and we still charge the instantiations to the real budget, to behave the
    /// same as if we had a cache.
    ///
    /// Finally, for those scratching their head about the overall structure:
    /// all of this happens as a result of the "module cache" not being
    /// especially cache-like (i.e. not being populated lazily, on-access). It's
    /// populated all at once, up front, because wasmi does not allow adding
    /// modules to an engine that's currently running.
    #[cfg(any(test, feature = "recording_mode"))]
    fn parse_module(
        host: &Host,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
        cost_mode: ModuleParseCostMode,
    ) -> Result<Rc<VersionedParsedModule<V>>, HostError> {
        if cost_mode == ModuleParseCostMode::PossiblyDeferredIfRecording
            && ModuleCache::should_use_for_protocol(host.get_ledger_protocol_version()?)
        {
            if host.in_storage_recording_mode()? {
                return host.budget_ref().with_observable_shadow_mode(|| {
                    VersionedParsedModule::<V>::new_with_isolated_engine(host, wasm, cost_inputs)
                });
            }
        }
        VersionedParsedModule::<V>::new_with_isolated_engine(host, wasm, cost_inputs)
    }

    pub(crate) fn get_memory(&self, host: &Host) -> Result<V::Memory, HostError> {
        match &self.memory {
            Some(mem) => Ok(mem.clone()),
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
        inputs: &[V::Value],
    ) -> Result<Val, HostError> {
        host.charge_budget(ContractCostType::InvokeVmFunction, None)?;

        // resolve the function entity to be called
        let func_ss: SymbolStr = func_sym.try_into_val(host)?;
        let ext = match V::get_export(
            &self.instance,
            &*self.store.try_borrow_or_err()?,
            func_ss.as_ref(),
        ) {
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
        let func = match V::export_to_func(ext) {
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

        if inputs.len() > Vm::MAX_VM_ARGS {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "Too many arguments in wasm invocation",
                &[func_sym.to_val()],
            ));
        }

        // call the function
        let mut wasm_ret: [V::Value; 1] = [V::VALUE_ZERO];
        self.store.try_borrow_mut_or_err()?.add_fuel_to_vm(host)?;
        // Metering: the `func.call` will trigger `wasmi::Call` (or `CallIndirect`) instruction,
        // which is technically covered by wasmi fuel metering. So we are double charging a bit
        // here (by a few 100s cpu insns). It is better to be safe.
        let res = V::call_func(
            &func,
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
            return Err(V::handle_call_error(host, func_sym, e));
        }
        host.relative_to_absolute(
            V::try_unmarshal_value_to_val(wasm_ret[0].clone()).ok_or(ConversionError)?,
        )
    }

    pub(crate) fn invoke_function_raw(
        self: &Rc<Self>,
        host: &Host,
        func_sym: &Symbol,
        args: &[Val],
    ) -> Result<Val, HostError> {
        let _span = tracy_span!("Vm::invoke_function_raw");
        Vec::<V::Value>::charge_bulk_init_cpy(args.len() as u64, host.as_budget())?;
        let wasm_args: Vec<V::Value> = args
            .iter()
            .map(|i| {
                host.absolute_to_relative(*i)
                    .map(|v| V::marshal_val_to_value(v))
            })
            .collect::<Result<Vec<V::Value>, HostError>>()?;
        self.metered_func_call(host, func_sym, wasm_args.as_slice())
    }

    /// Returns the raw bytes content of a named custom section from the WASM
    /// module loaded into the [Vm], or `None` if no such custom section exists.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        self.module.custom_section(name)
    }
}

impl VersionedVm<Wasmi031> {
    pub(crate) fn with_caller_031<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(wasmi_031::Caller<Host>) -> Result<T, HostError>,
    {
        let store: &mut wasmi_031::Store<Host> = &mut *self.store.try_borrow_mut_or_err()?;
        let mut ctx: wasmi_031::StoreContextMut<Host> = store.into();
        let caller: wasmi_031::Caller<Host> =
            wasmi_031::Caller::new(&mut ctx, Some(&self.instance));
        f(caller)
    }
}

impl VersionedVm<Wasmi034> {
    pub(crate) fn with_caller_034<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(wasmi_034::Caller<Host>) -> Result<T, HostError>,
    {
        let store: &mut wasmi_034::Store<Host> = &mut *self.store.try_borrow_mut_or_err()?;
        let mut ctx: wasmi_034::StoreContextMut<Host> = store.into();
        let caller: wasmi_034::Caller<Host> =
            wasmi_034::Caller::new(&mut ctx, Some(&self.instance));
        f(caller)
    }
}

impl Vm {
    /// Utility function that synthesizes a `VmCaller<Host>` configured to point
    /// to this VM's `Store` and `Instance`, and calls the provided function
    /// back with it. Mainly used for testing.
    #[cfg(feature = "bench")]
    #[allow(dead_code)]
    pub(crate) fn with_vmcaller<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&mut VmCaller<Host>) -> Result<T, HostError>,
    {
        match &self.0 {
            VmVer::Vm031(vm) => vm.with_caller_031(|caller| {
                let mut vmcaller: VmCaller<Host> = VmCaller::Vm031(caller);
                f(&mut vmcaller)
            }),
            VmVer::Vm034(vm) => vm.with_caller_034(|caller| {
                let mut vmcaller: VmCaller<Host> = VmCaller::Vm034(caller);
                f(&mut vmcaller)
            }),
        }
    }

    pub(crate) fn memory_hash_and_size(&self, budget: &Budget) -> Result<(u64, usize), HostError> {
        use std::hash::Hasher;
        let mut state = CountingHasher::default();
        let len = match &self.0 {
            VmVer::Vm031(vm) => {
                if let Some(mem) = vm.memory {
                    vm.with_caller_031(|caller| {
                        let data = mem.data(&caller);
                        data.metered_hash(&mut state, budget)?;
                        Ok(data.len())
                    })?
                } else {
                    return Ok((0, 0));
                }
            }
            VmVer::Vm034(vm) => {
                if let Some(mem) = vm.memory {
                    vm.with_caller_034(|caller| {
                        let data = mem.data(&caller);
                        data.metered_hash(&mut state, budget)?;
                        Ok(data.len())
                    })?
                } else {
                    return Ok((0, 0));
                }
            }
        };
        Ok((state.finish(), len))
    }

    // This is pretty weak: we just observe the state that wasmi exposes through
    // wasm _exports_. There might be tables or globals a wasm doesn't export
    // but there's no obvious way to observe them.
    pub(crate) fn exports_hash_and_size(&self, budget: &Budget) -> Result<(u64, usize), HostError> {
        use std::hash::Hasher;
        let mut state = CountingHasher::default();
        let mut size: usize = 0;

        // This is a slight fudge to avoid having to define a ton of additional
        // MeteredHash impls for wasmi substructures, since there is a bounded
        // size on the string representation of a value, we're comfortable going
        // temporarily over budget here.
        fn hash_impl_debug(
            d: &impl core::fmt::Debug,
            state: &mut CountingHasher,
            budget: &Budget,
        ) -> Result<(), HostError> {
            let s = format!("{:?}", d);
            budget.charge(ContractCostType::MemAlloc, Some(s.len() as u64))?;
            s.metered_hash(state, budget)
        }

        match &self.0 {
            VmVer::Vm031(vm) => {
                vm.with_caller_031(|caller| {
                    for export in vm.instance.exports(&caller) {
                        size = size.saturating_add(1);
                        export.name().metered_hash(&mut state, budget)?;
                        match export.into_extern() {
                            // Funcs are immutable, memory we hash separately above.
                            wasmi_031::Extern::Func(_) | wasmi_031::Extern::Memory(_) => (),
                            wasmi_031::Extern::Table(t) => {
                                let sz = t.size(&caller);
                                sz.metered_hash(&mut state, budget)?;
                                size = size.saturating_add(sz as usize);
                                for i in 0..sz {
                                    if let Some(elem) = t.get(&caller, i) {
                                        hash_impl_debug(&elem, &mut state, budget)?;
                                    }
                                }
                            }
                            wasmi_031::Extern::Global(g) => {
                                hash_impl_debug(&g.get(&caller), &mut state, budget)?;
                            }
                        }
                    }
                    Ok(())
                })?
            }
            VmVer::Vm034(vm) => vm.with_caller_034(|caller| {
                for export in vm.instance.exports(&caller) {
                    size = size.saturating_add(1);
                    export.name().metered_hash(&mut state, budget)?;
                    match export.into_extern() {
                        wasmi_034::Extern::Func(_) | wasmi_034::Extern::Memory(_) => (),
                        wasmi_034::Extern::Table(t) => {
                            let sz = t.size(&caller);
                            sz.metered_hash(&mut state, budget)?;
                            size = size.saturating_add(sz as usize);
                            for i in 0..sz {
                                if let Some(elem) = t.get(&caller, i) {
                                    hash_impl_debug(&elem, &mut state, budget)?;
                                }
                            }
                        }
                        wasmi_034::Extern::Global(g) => {
                            hash_impl_debug(&g.get(&caller), &mut state, budget)?;
                        }
                    }
                }
                Ok(())
            })?,
        }
        Ok((state.finish(), size))
    }
}
