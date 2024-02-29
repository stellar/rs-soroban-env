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

#[cfg(feature = "bench")]
pub(crate) use dispatch::dummy0;
#[cfg(test)]
pub(crate) use dispatch::protocol_gated_dummy;

use crate::{
    budget::{get_wasmi_config, AsBudget, Budget},
    err,
    host::{
        error::TryBorrowOrErr,
        metered_clone::MeteredContainer,
        metered_hash::{CountingHasher, MeteredHash},
    },
    meta::{self, get_ledger_protocol_version},
    xdr::{
        ContractCodeCostInputs, ContractCostType, Hash, Limited, ReadXdr, ScEnvMetaEntry,
        ScErrorCode, ScErrorType,
    },
    ConversionError, Host, HostError, Symbol, SymbolStr, TryIntoVal, Val, WasmiMarshal,
    DEFAULT_XDR_RW_LIMITS,
};
use std::{cell::RefCell, collections::BTreeSet, io::Cursor, rc::Rc, time::Instant};

use fuel_refillable::FuelRefillable;
use func_info::HOST_FUNCTIONS;

use wasmi::{Engine, Instance, Linker, Memory, Module, Store, Value};

use crate::VmCaller;
use wasmi::{Caller, StoreContextMut};
impl wasmi::core::HostError for HostError {}

const MAX_VM_ARGS: usize = 32;

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
    // TODO: consider moving store and possibly module to Host so they can be
    // recycled across calls. Or possibly beyond, to be recycled across txs.
    // https://github.com/stellar/rs-soroban-env/issues/827
    module: Module,
    cost_inputs: ContractCodeCostInputs,
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

        // Not used when "next" is enabled
        #[cfg(not(feature = "next"))]
        let got_pre = meta::get_pre_release_version(interface_version);

        let got_proto = get_ledger_protocol_version(interface_version);

        if got_proto < want_proto {
            // Old protocols are finalized, we only support contracts
            // with similarly finalized (zero) prerelease numbers.
            //
            // Note that we only enable this check if the "next" feature isn't enabled
            // because a "next" stellar-core can still run a "curr" test using non-finalized
            // test wasms. The "next" feature isn't safe for production and is meant to
            // simulate the protocol version after the one currently supported in
            // stellar-core, so bypassing this check for "next" is safe.
            #[cfg(not(feature = "next"))]
            if got_pre != 0 {
                return Err(err!(
                    host,
                    (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                    "contract pre-release number for old protocol is nonzero",
                    got_pre
                ));
            }
        } else if got_proto == want_proto {
            // Relax this check as well for the "next" feature to allow for flexibility while testing.
            // stellar-core can pass in an older protocol version, in which case the pre-release version
            // will not match up with the "next" feature (The "next" pre-release version is always 1).
            #[cfg(not(feature = "next"))]
            {
                // Current protocol might have a nonzero prerelease number; we will
                // allow it only if it matches the current prerelease exactly.
                let want_pre = meta::get_pre_release_version(meta::INTERFACE_VERSION);
                if want_pre != got_pre {
                    return Err(err!(
                        host,
                        (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                        "contract pre-release number for current protocol does not match host",
                        got_pre,
                        want_pre
                    ));
                }
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

    fn check_meta_section(host: &Host, m: &Module) -> Result<u64, HostError> {
        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut limits = DEFAULT_XDR_RW_LIMITS;
            limits.len = env_meta.len();
            let mut cursor = Limited::new(Cursor::new(env_meta), limits);
            if let Some(env_meta_entry) = ScEnvMetaEntry::read_xdr_iter(&mut cursor).next() {
                let ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) =
                    host.map_err(env_meta_entry)?;
                Vm::check_contract_interface_version(host, v)?;
                Ok(v)
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

    fn check_max_args(host: &Host, m: &Module) -> Result<(), HostError> {
        for e in m.exports() {
            match e.ty() {
                wasmi::ExternType::Func(f) => {
                    if f.params().len() > MAX_VM_ARGS || f.results().len() > MAX_VM_ARGS {
                        return Err(host.err(
                            ScErrorType::WasmVm,
                            ScErrorCode::InvalidInput,
                            "Too many arguments or results in wasm export",
                            &[],
                        ));
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    #[cfg(feature = "testutils")]
    pub fn get_all_host_functions() -> Vec<(&'static str, &'static str, u32)> {
        HOST_FUNCTIONS
            .iter()
            .map(|hf| (hf.mod_str, hf.fn_str, hf.arity))
            .collect()
    }

    /// Instantiates a VM given the arguments provided in [`Self::new`]
    fn instantiate(
        host: &Host,
        contract_id: Hash,
        module_wasm_code: &[u8],
        code_cost_inputs: Option<ContractCodeCostInputs>,
    ) -> Result<Rc<Self>, HostError> {
        match &code_cost_inputs {
            // When we have a set of cost inputs, we charge each of them. This
            // is the "cheap" path where we're going to charge a model cost that
            // is much closer to the true cost of instantiating the contract,
            // modeled as a variety of different linear terms.
            Some(inputs) => {
                host.charge_budget(
                    ContractCostType::VmInstantiateInstructions,
                    Some(inputs.n_instructions as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateFunctions,
                    Some(inputs.n_functions as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateGlobals,
                    Some(inputs.n_globals as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateTableEntries,
                    Some(inputs.n_table_entries as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateTypes,
                    Some(inputs.n_types as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateDataSegments,
                    Some(inputs.n_data_segments as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateElemSegments,
                    Some(inputs.n_elem_segments as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateImports,
                    Some(inputs.n_imports as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateExports,
                    Some(inputs.n_exports as u64),
                )?;
                host.charge_budget(
                    ContractCostType::VmInstantiateMemoryPages,
                    Some(inputs.n_memory_pages as u64),
                )?;
            }
            None => {
                // When we don't have a set of cost inputs, either because the
                // contract predates storing cost inputs or because we're doing
                // an upload-time analysis of an unknown contract, we charge
                // based on the byte size and assume the worst-case cost for
                // each byte. This is the "expensive" path, but we only have to
                // charge it during upload, after which we'll store the cost
                // inputs for use in future instantiations.
                host.charge_budget(
                    ContractCostType::VmInstantiation,
                    Some(module_wasm_code.len() as u64),
                )?;
            }
        }

        let config = get_wasmi_config(host)?;
        let engine = Engine::new(&config);
        let module = {
            let _span0 = tracy_span!("parse module");
            host.map_err(Module::new(&engine, module_wasm_code))?
        };

        Self::check_max_args(host, &module)?;
        let interface_version = Self::check_meta_section(host, &module)?;
        let contract_proto = get_ledger_protocol_version(interface_version);

        let cost_inputs = match code_cost_inputs {
            Some(inputs) => inputs,
            None => Self::extract_contract_cost_inputs(host.clone(), module_wasm_code)?,
        };

        let mut store = Store::new(&engine, host.clone());
        store.limiter(|host| host);

        let module_imports: BTreeSet<(&str, &str)> = module
            .imports()
            .filter(|i| i.ty().func().is_some())
            .map(|i| {
                let mod_str = i.module();
                let fn_str = i.name();
                (mod_str, fn_str)
            })
            .collect();

        let mut linker = <Linker<Host>>::new(&engine);

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
                    if contract_proto < min_proto || ledger_proto < min_proto {
                        // We skip linking this hf instead of returning an error
                        // because we have to support old contracts during replay.
                        continue;
                    }
                }
                if let Some(max_proto) = hf.max_proto {
                    if contract_proto > max_proto || ledger_proto > max_proto {
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
        Ok(Rc::new(Self {
            contract_id,
            module,
            cost_inputs,
            store: RefCell::new(store),
            instance,
            memory,
        }))
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
        code_cost_inputs: Option<ContractCodeCostInputs>,
    ) -> Result<Rc<Self>, HostError> {
        let _span = tracy_span!("Vm::new");

        if cfg!(not(target_family = "wasm")) {
            let now = Instant::now();
            let vm = Self::instantiate(host, contract_id, module_wasm_code, code_cost_inputs)?;

            host.as_budget().track_time(
                ContractCostType::VmInstantiation,
                now.elapsed().as_nanos() as u64,
            )?;

            Ok(vm)
        } else {
            Self::instantiate(host, contract_id, module_wasm_code, code_cost_inputs)
        }
    }

    fn check_const_expr_simple(host: &Host, expr: &wasmparser::ConstExpr) -> Result<(), HostError> {
        use wasmparser::Operator::*;
        let mut op = expr.get_operators_reader();
        while !op.eof() {
            match host.map_err(op.read())? {
                I32Const { .. } | I64Const { .. } | RefFunc { .. } | RefNull { .. } | End => (),
                _ => {
                    return Err(host.err(
                        ScErrorType::WasmVm,
                        ScErrorCode::InvalidInput,
                        "unsupported complex wasm constant expression",
                        &[],
                    ))
                }
            }
        }
        Ok(())
    }

    fn extract_contract_cost_inputs(
        host: Host,
        wasm: &[u8],
    ) -> Result<ContractCodeCostInputs, HostError> {
        use wasmparser::{ElementItems, ElementKind, Parser, Payload::*, TableInit};

        if !Parser::is_core_wasm(wasm) {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "unsupported non-core wasm module",
                &[],
            ));
        }

        let mut costs = ContractCodeCostInputs {
            n_instructions: 0,
            n_functions: 0,
            n_globals: 0,
            n_table_entries: 0,
            n_types: 0,
            n_data_segments: 0,
            n_elem_segments: 0,
            n_imports: 0,
            n_exports: 0,
            n_memory_pages: 0,
        };

        let parser = Parser::new(0);
        let mut elements: u32 = 0;
        let mut data: u32 = 0;
        for section in parser.parse_all(wasm) {
            let section = host.map_err(section)?;
            match section {
                // Ignored sections.
                Version { .. }
                | DataCountSection { .. }
                | CustomSection(_)
                | CodeSectionStart { .. }
                | End(_) => (),

                // Component-model stuff or other unsupported sections. Error out.
                StartSection { .. }
                | ModuleSection { .. }
                | InstanceSection(_)
                | CoreTypeSection(_)
                | ComponentSection { .. }
                | ComponentInstanceSection(_)
                | ComponentAliasSection(_)
                | ComponentTypeSection(_)
                | ComponentCanonicalSection(_)
                | ComponentStartSection { .. }
                | ComponentImportSection(_)
                | ComponentExportSection(_)
                | TagSection(_)
                | UnknownSection { .. } => {
                    return Err(host.err(
                        ScErrorType::WasmVm,
                        ScErrorCode::InvalidInput,
                        "unsupported wasm section",
                        &[],
                    ))
                }

                MemorySection(s) => {
                    for mem in s {
                        let mem = host.map_err(mem)?;
                        if mem.memory64 {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "unsupported 64-bit memory",
                                &[],
                            ));
                        }
                        if mem.shared {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "unsupported shared memory",
                                &[],
                            ));
                        }
                        if mem.initial > 0xffff {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "unsupported memory size",
                                &[],
                            ));
                        }
                        costs.n_memory_pages =
                            costs.n_memory_pages.saturating_add(mem.initial as u32);
                    }
                }

                TypeSection(s) => costs.n_types = costs.n_types.saturating_add(s.count()),
                ImportSection(s) => costs.n_imports = costs.n_imports.saturating_add(s.count()),
                FunctionSection(s) => {
                    costs.n_functions = costs.n_functions.saturating_add(s.count())
                }
                TableSection(s) => {
                    for table in s {
                        let table = host.map_err(table)?;
                        costs.n_table_entries =
                            costs.n_table_entries.saturating_add(table.ty.initial);
                        match table.init {
                            TableInit::RefNull => (),
                            TableInit::Expr(ref expr) => {
                                Self::check_const_expr_simple(&host, &expr)?;
                            }
                        }
                    }
                }
                GlobalSection(s) => {
                    costs.n_globals = costs.n_globals.saturating_add(s.count());
                    for global in s {
                        let global = host.map_err(global)?;
                        Self::check_const_expr_simple(&host, &global.init_expr)?;
                    }
                }
                ExportSection(s) => costs.n_exports = costs.n_exports.saturating_add(s.count()),
                ElementSection(s) => {
                    costs.n_elem_segments = costs.n_elem_segments.saturating_add(1);
                    elements = elements.saturating_add(s.count());
                    for elem in s {
                        let elem = host.map_err(elem)?;
                        match elem.kind {
                            ElementKind::Declared | ElementKind::Passive => (),
                            ElementKind::Active { offset_expr, .. } => {
                                Self::check_const_expr_simple(&host, &offset_expr)?
                            }
                        }
                        match elem.items {
                            ElementItems::Functions(_) => (),
                            ElementItems::Expressions(_, exprs) => {
                                for expr in exprs {
                                    let expr = host.map_err(expr)?;
                                    Self::check_const_expr_simple(&host, &expr)?;
                                }
                            }
                        }
                    }
                }
                DataSection(s) => {
                    costs.n_data_segments = costs.n_data_segments.saturating_add(1);
                    for d in s {
                        let d = host.map_err(d)?;
                        if d.data.len() > u32::MAX as usize {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "data segment too large",
                                &[],
                            ));
                        }
                        data = data.saturating_add(d.data.len() as u32);
                        match d.kind {
                            wasmparser::DataKind::Active { offset_expr, .. } => {
                                Self::check_const_expr_simple(&host, &offset_expr)?
                            }
                            wasmparser::DataKind::Passive => (),
                        }
                    }
                }
                CodeSectionEntry(s) => {
                    let ops = host.map_err(s.get_operators_reader())?;
                    for _op in ops {
                        costs.n_instructions = costs.n_instructions.saturating_add(1);
                    }
                }
            }
        }
        if elements > costs.n_table_entries {
            dbg!(elements);
            dbg!(costs.n_table_entries);
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "too many elements in wasm elem section(s)",
                &[],
            ));
        }
        Ok(costs)
    }

    pub fn get_contract_code_cost_inputs(&self) -> ContractCodeCostInputs {
        self.cost_inputs.clone()
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
