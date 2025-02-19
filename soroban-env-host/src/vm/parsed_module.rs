use crate::{
    budget::{AsBudget, Budget},
    err,
    host::metered_clone::MeteredContainer,
    meta,
    xdr::{
        ContractCodeEntry, ContractCodeEntryExt, ContractCostType, Limited, ReadXdr,
        ScEnvMetaEntry, ScEnvMetaEntryInterfaceVersion, ScErrorCode, ScErrorType,
    },
    ErrorHandler, Host, HostError, Val, DEFAULT_XDR_RW_LIMITS,
};

use super::{Vm, HOST_FUNCTIONS};
use std::{collections::BTreeSet, io::Cursor, sync::Arc};

#[derive(Debug, Clone)]
pub enum VersionedContractCodeCostInputs {
    V0 { wasm_bytes: usize },
    V1(crate::xdr::ContractCodeCostInputs),
}

impl VersionedContractCodeCostInputs {
    pub fn is_v0(&self) -> bool {
        match self {
            Self::V0 { .. } => true,
            Self::V1(_) => false,
        }
    }

    pub fn charge_for_parsing(&self, budget: &impl AsBudget) -> Result<(), HostError> {
        let budget = budget.as_budget();
        match self {
            Self::V0 { wasm_bytes } => {
                budget.charge(ContractCostType::VmInstantiation, Some(*wasm_bytes as u64))?;
            }
            Self::V1(inputs) => {
                budget.charge(
                    ContractCostType::ParseWasmInstructions,
                    Some(inputs.n_instructions as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmFunctions,
                    Some(inputs.n_functions as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmGlobals,
                    Some(inputs.n_globals as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmTableEntries,
                    Some(inputs.n_table_entries as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmTypes,
                    Some(inputs.n_types as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmDataSegments,
                    Some(inputs.n_data_segments as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmElemSegments,
                    Some(inputs.n_elem_segments as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmImports,
                    Some(inputs.n_imports as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmExports,
                    Some(inputs.n_exports as u64),
                )?;
                budget.charge(
                    ContractCostType::ParseWasmDataSegmentBytes,
                    Some(inputs.n_data_segment_bytes as u64),
                )?;
            }
        }
        Ok(())
    }

    pub fn charge_for_instantiation(&self, _host: &Host) -> Result<(), HostError> {
        match self {
            Self::V0 { wasm_bytes } => {
                // Before soroban supported cached instantiation, the full cost
                // of parsing-and-instantiation was charged to the
                // VmInstantiation cost type and we already charged it by the
                // time we got here, in `charge_for_parsing` above. At-and-after
                // the protocol that enabled cached instantiation, the
                // VmInstantiation cost type was repurposed to only cover the
                // cost of parsing, so we have to charge the "second half" cost
                // of instantiation separately here.
                _host.charge_budget(
                    ContractCostType::VmCachedInstantiation,
                    Some(*wasm_bytes as u64),
                )?;
            }
            Self::V1(inputs) => {
                _host.charge_budget(ContractCostType::InstantiateWasmInstructions, None)?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmFunctions,
                    Some(inputs.n_functions as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmGlobals,
                    Some(inputs.n_globals as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmTableEntries,
                    Some(inputs.n_table_entries as u64),
                )?;
                _host.charge_budget(ContractCostType::InstantiateWasmTypes, None)?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmDataSegments,
                    Some(inputs.n_data_segments as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmElemSegments,
                    Some(inputs.n_elem_segments as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmImports,
                    Some(inputs.n_imports as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmExports,
                    Some(inputs.n_exports as u64),
                )?;
                _host.charge_budget(
                    ContractCostType::InstantiateWasmDataSegmentBytes,
                    Some(inputs.n_data_segment_bytes as u64),
                )?;
            }
        }
        Ok(())
    }
}

// A `CompilationContext` abstracts over the necessary budgeting and
// error-reporting dimensions of both the `Host` (when building a
// contract for throwaway use in an isolated context like contract-upload)
// and other contexts that might want to compile code (like embedders that
// precompile contracts).
pub trait CompilationContext: AsBudget + ErrorHandler {}
impl CompilationContext for Host {}

/// A [ParsedModule] contains the parsed [wasmi::Module] for a given Wasm blob,
/// as well as a protocol number and set of [ContractCodeCostInputs] extracted
/// from the module when it was parsed.
pub struct ParsedModule {
    pub wasmi_module: wasmi::Module,
    pub proto_version: u32,
    pub cost_inputs: VersionedContractCodeCostInputs,
}

pub fn wasm_module_memory_cost(
    budget: &Budget,
    contract_code_entry: &ContractCodeEntry,
) -> Result<u64, HostError> {
    match &contract_code_entry.ext {
        ContractCodeEntryExt::V0 => budget.get_memory_cost(
            ContractCostType::VmInstantiation,
            Some(contract_code_entry.code.len() as u64),
        ),
        ContractCodeEntryExt::V1(contract_code_entry_v1) => {
            let cost_inputs = &contract_code_entry_v1.cost_inputs;
            let mut res = 0_u64;
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmInstructions,
                Some(cost_inputs.n_instructions as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmFunctions,
                Some(cost_inputs.n_functions as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmGlobals,
                Some(cost_inputs.n_globals as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmTableEntries,
                Some(cost_inputs.n_table_entries as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmTypes,
                Some(cost_inputs.n_types as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmDataSegments,
                Some(cost_inputs.n_data_segments as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmElemSegments,
                Some(cost_inputs.n_elem_segments as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmImports,
                Some(cost_inputs.n_imports as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmExports,
                Some(cost_inputs.n_exports as u64),
            )?);
            res = res.saturating_add(budget.get_memory_cost(
                ContractCostType::ParseWasmDataSegmentBytes,
                Some(cost_inputs.n_data_segment_bytes as u64),
            )?);
            Ok(res)
        }
    }
}

impl ParsedModule {
    pub fn new<Ctx: CompilationContext>(
        context: &Ctx,
        curr_ledger_protocol: u32,
        wasmi_engine: &wasmi::Engine,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<Arc<Self>, HostError> {
        cost_inputs.charge_for_parsing(context.as_budget())?;
        let (wasmi_module, proto_version) =
            Self::parse_wasm(context, curr_ledger_protocol, wasmi_engine, wasm)?;
        Ok(Arc::new(Self {
            wasmi_module,
            proto_version,
            cost_inputs,
        }))
    }

    pub fn with_import_symbols<T>(
        &self,
        host: &Host,
        callback: impl FnOnce(&BTreeSet<(&str, &str)>) -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        // Cap symbols we're willing to import at 10 characters for each of
        // module and function name. in practice they are all 1-2 chars, but
        // we'll leave some future-proofing room here. The important point
        // is to not be introducing a DoS vector.
        const SYM_LEN_LIMIT: usize = 10;
        let symbols: BTreeSet<(&str, &str)> = self
            .wasmi_module
            .imports()
            .filter_map(|i| {
                if i.ty().func().is_some() {
                    let mod_str = i.module();
                    let fn_str = i.name();
                    if mod_str.len() < SYM_LEN_LIMIT && fn_str.len() < SYM_LEN_LIMIT {
                        return Some((mod_str, fn_str));
                    }
                }
                None
            })
            .collect();

        // We approximate the cost of `BTreeSet` with the cost of initializng a
        // `Vec` with the same elements, and we are doing it after the set has
        // been created. The element count has been limited/charged during the
        // parsing phase, so there is no DOS factor. We don't charge for
        // insertion/lookups, since they should be cheap and number of
        // operations on the set is limited.
        Vec::<(&str, &str)>::charge_bulk_init_cpy(symbols.len() as u64, host)?;
        callback(&symbols)
    }

    pub fn make_wasmi_linker(&self, host: &Host) -> Result<wasmi::Linker<Host>, HostError> {
        self.with_import_symbols(host, |symbols| {
            Host::make_minimal_wasmi_linker_for_symbols(host, self.wasmi_module.engine(), symbols)
        })
    }

    pub fn new_with_isolated_engine(
        host: &Host,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<Arc<Self>, HostError> {
        use crate::budget::AsBudget;
        let wasmi_config = crate::vm::get_wasmi_config(host.as_budget())?;
        let wasmi_engine = wasmi::Engine::new(&wasmi_config);

        Self::new(
            host,
            host.get_ledger_protocol_version()?,
            &wasmi_engine,
            wasm,
            cost_inputs,
        )
    }

    /// Parse the Wasm blob into a [Module] and its protocol number, checking its interface version
    fn parse_wasm<Ctx: CompilationContext>(
        context: &Ctx,
        curr_ledger_protocol: u32,
        wasmi_engine: &wasmi::Engine,
        wasm: &[u8],
    ) -> Result<(wasmi::Module, u32), HostError> {
        let module = {
            let _span = tracy_span!("wasmi::Module::new");
            context.map_err(wasmi::Module::new(&wasmi_engine, wasm))?
        };
        Self::check_max_args(context, &module)?;
        let interface_version = Self::check_meta_section(context, curr_ledger_protocol, &module)?;
        let contract_proto = interface_version.protocol;

        Ok((module, contract_proto))
    }

    fn check_contract_interface_version<Ctx: CompilationContext>(
        context: &Ctx,
        curr_ledger_protocol: u32,
        interface_version: &ScEnvMetaEntryInterfaceVersion,
    ) -> Result<(), HostError> {
        let want_proto = {
            let env_proto = meta::INTERFACE_VERSION.protocol;
            if curr_ledger_protocol <= env_proto {
                // ledger proto should be before or equal to env proto
                curr_ledger_protocol
            } else {
                return Err(context.error(
                    (ScErrorType::Context, ScErrorCode::InternalError).into(),
                    "ledger protocol number is ahead of supported env protocol number",
                    &[
                        Val::from_u32(curr_ledger_protocol).to_val(),
                        Val::from_u32(env_proto).to_val(),
                    ],
                ));
            }
        };

        // Not used when "next" is enabled
        #[cfg(not(feature = "next"))]
        let got_pre = interface_version.pre_release;

        let got_proto = interface_version.protocol;

        if got_proto < want_proto {
            // Old protocols are finalized, we only support contracts
            // with similarly finalized (zero) prerelease numbers.
            //
            // Note that we only enable this check if the "next" feature isn't enabled
            // because a "next" stellar-core can still run a "curr" test using non-finalized
            // test Wasms. The "next" feature isn't safe for production and is meant to
            // simulate the protocol version after the one currently supported in
            // stellar-core, so bypassing this check for "next" is safe.
            #[cfg(not(feature = "next"))]
            if got_pre != 0 {
                return Err(context.error(
                    (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                    "contract pre-release number for old protocol is nonzero",
                    &[Val::from_u32(got_pre).to_val()],
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
                let want_pre = meta::INTERFACE_VERSION.pre_release;
                if want_pre != got_pre {
                    return Err(context.error(
                        (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                        "contract pre-release number for current protocol does not match host",
                        &[
                            Val::from_u32(got_pre).to_val(),
                            Val::from_u32(want_pre).to_val(),
                        ],
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
            return Err(context.error(
                (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                "contract protocol number is newer than host",
                &[Val::from_u32(got_proto).to_val()],
            ));
        }
        Ok(())
    }

    pub(crate) fn check_contract_imports_match_host_protocol(
        &self,
        host: &Host,
    ) -> Result<(), HostError> {
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
        let _span = tracy_span!("ParsedModule::check_contract_imports_match_host_protocol");
        let ledger_proto = host.with_ledger_info(|li| Ok(li.protocol_version))?;
        self.with_import_symbols(host, |module_symbols| {
                for hf in HOST_FUNCTIONS {
                    if !module_symbols.contains(&(hf.mod_str, hf.fn_str)) {
                        continue;
                    }
                    if let Some(min_proto) = hf.min_proto {
                        if self.proto_version < min_proto || ledger_proto < min_proto {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidAction,
                                "contract calls a host function not yet supported by current protocol",
                                &[],
                            ));
                        }
                    }
                    if let Some(max_proto) = hf.max_proto {
                        if self.proto_version > max_proto || ledger_proto > max_proto {
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
        Ok(())
    }

    fn module_custom_section(m: &wasmi::Module, name: impl AsRef<str>) -> Option<&[u8]> {
        m.custom_sections().iter().find_map(|s| {
            if &*s.name == name.as_ref() {
                Some(&*s.data)
            } else {
                None
            }
        })
    }

    /// Returns the raw bytes content of a named custom section from the Wasm
    /// module loaded into the [Vm], or `None` if no such custom section exists.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        Self::module_custom_section(&self.wasmi_module, name)
    }

    fn check_meta_section<Ctx: CompilationContext>(
        context: &Ctx,
        curr_ledger_protocol: u32,
        m: &wasmi::Module,
    ) -> Result<ScEnvMetaEntryInterfaceVersion, HostError> {
        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut limits = DEFAULT_XDR_RW_LIMITS;
            limits.len = env_meta.len();
            let mut cursor = Limited::new(Cursor::new(env_meta), limits);
            if let Some(env_meta_entry) = ScEnvMetaEntry::read_xdr_iter(&mut cursor).next() {
                let ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) =
                    context.map_err(env_meta_entry)?;
                Self::check_contract_interface_version(context, curr_ledger_protocol, &v)?;
                Ok(v)
            } else {
                Err(context.error(
                    (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                    "contract missing environment interface version",
                    &[],
                ))
            }
        } else {
            Err(context.error(
                (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                "contract missing metadata section",
                &[],
            ))
        }
    }

    fn check_max_args<E: ErrorHandler>(handler: &E, m: &wasmi::Module) -> Result<(), HostError> {
        for e in m.exports() {
            match e.ty() {
                wasmi::ExternType::Func(f) => {
                    if f.results().len() > Vm::MAX_VM_ARGS {
                        return Err(handler.error(
                            (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                            "Too many return values in Wasm export",
                            &[Val::from_u32(f.results().len() as u32).to_val()],
                        ));
                    }
                    if f.params().len() > Vm::MAX_VM_ARGS {
                        return Err(handler.error(
                            (ScErrorType::WasmVm, ScErrorCode::InvalidInput).into(),
                            "Too many arguments Wasm export",
                            &[Val::from_u32(f.params().len() as u32).to_val()],
                        ));
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }

    // Do a second, manual parse of the Wasm blob to extract cost parameters we're
    // interested in.
    pub fn extract_refined_contract_cost_inputs(
        host: &Host,
        wasm: &[u8],
    ) -> Result<crate::xdr::ContractCodeCostInputs, HostError> {
        use wasmparser::{ElementItems, ElementKind, Parser, Payload::*, TableInit};

        if !Parser::is_core_wasm(wasm) {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "unsupported non-core wasm module",
                &[],
            ));
        }

        let mut costs = crate::xdr::ContractCodeCostInputs {
            ext: crate::xdr::ExtensionPoint::V0,
            n_instructions: 0,
            n_functions: 0,
            n_globals: 0,
            n_table_entries: 0,
            n_types: 0,
            n_data_segments: 0,
            n_elem_segments: 0,
            n_imports: 0,
            n_exports: 0,
            n_data_segment_bytes: 0,
        };

        let parser = Parser::new(0);
        let mut elements: u32 = 0;
        let mut available_memory: u32 = 0;
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
                        "unsupported wasm section type",
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
                        if mem
                            .initial
                            .saturating_mul(crate::vm::WASM_STD_MEM_PAGE_SIZE_IN_BYTES as u64)
                            > u32::MAX as u64
                        {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "unsupported memory size",
                                &[],
                            ));
                        }
                        available_memory = available_memory.saturating_add(
                            (mem.initial as u32)
                                .saturating_mul(crate::vm::WASM_STD_MEM_PAGE_SIZE_IN_BYTES),
                        );
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
                    costs.n_elem_segments = costs.n_elem_segments.saturating_add(s.count());
                    for elem in s {
                        let elem = host.map_err(elem)?;
                        match elem.kind {
                            ElementKind::Declared | ElementKind::Passive => (),
                            ElementKind::Active { offset_expr, .. } => {
                                Self::check_const_expr_simple(&host, &offset_expr)?
                            }
                        }
                        match elem.items {
                            ElementItems::Functions(fs) => {
                                elements = elements.saturating_add(fs.count());
                            }
                            ElementItems::Expressions(_, exprs) => {
                                elements = elements.saturating_add(exprs.count());
                                for expr in exprs {
                                    let expr = host.map_err(expr)?;
                                    Self::check_const_expr_simple(&host, &expr)?;
                                }
                            }
                        }
                    }
                }
                DataSection(s) => {
                    costs.n_data_segments = costs.n_data_segments.saturating_add(s.count());
                    for d in s {
                        let d = host.map_err(d)?;
                        if d.data.len() > u32::MAX as usize {
                            return Err(host.err(
                                ScErrorType::WasmVm,
                                ScErrorCode::InvalidInput,
                                "data segment exceeds u32::MAX",
                                &[],
                            ));
                        }
                        costs.n_data_segment_bytes = costs
                            .n_data_segment_bytes
                            .saturating_add(d.data.len() as u32);
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
        if costs.n_data_segment_bytes > available_memory {
            return Err(err!(
                host,
                (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                "data segment(s) content exceeds memory size",
                costs.n_data_segment_bytes,
                available_memory
            ));
        }
        if elements > costs.n_table_entries {
            return Err(err!(
                host,
                (ScErrorType::WasmVm, ScErrorCode::InvalidInput),
                "elem segments(s) content exceeds table size",
                elements,
                costs.n_table_entries
            ));
        }
        Ok(costs)
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
                        "unsupported complex Wasm constant expression",
                        &[],
                    ))
                }
            }
        }
        Ok(())
    }
}
