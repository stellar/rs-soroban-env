use crate::{
    err,
    meta::{self, get_ledger_protocol_version},
    xdr::{ContractCostType, Limited, ReadXdr, ScEnvMetaEntry, ScErrorCode, ScErrorType},
    Host, HostError, DEFAULT_XDR_RW_LIMITS,
};

use wasmi::{Engine, Module};

use super::MAX_VM_ARGS;
use std::io::Cursor;

#[derive(Debug, Clone)]
pub enum VersionedContractCodeCostInputs {
    V0 {
        wasm_bytes: usize,
    },
    #[cfg(feature = "next")]
    V1(crate::xdr::ContractCodeCostInputs),
}

impl VersionedContractCodeCostInputs {
    pub fn is_v0(&self) -> bool {
        match self {
            Self::V0 { .. } => true,
            #[cfg(feature = "next")]
            Self::V1(_) => false,
        }
    }
    pub fn charge_for_parsing(&self, host: &Host) -> Result<(), HostError> {
        match self {
            Self::V0 { wasm_bytes } => {
                host.charge_budget(ContractCostType::VmInstantiation, Some(*wasm_bytes as u64))?;
            }
            #[cfg(feature = "next")]
            Self::V1(inputs) => {
                host.charge_budget(
                    ContractCostType::ParseWasmInstructions,
                    Some(inputs.n_instructions as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmFunctions,
                    Some(inputs.n_functions as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmGlobals,
                    Some(inputs.n_globals as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmTableEntries,
                    Some(inputs.n_table_entries as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmTypes,
                    Some(inputs.n_types as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmDataSegments,
                    Some(inputs.n_data_segments as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmElemSegments,
                    Some(inputs.n_elem_segments as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmImports,
                    Some(inputs.n_imports as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmExports,
                    Some(inputs.n_exports as u64),
                )?;
                host.charge_budget(
                    ContractCostType::ParseWasmMemoryPages,
                    Some(inputs.n_memory_pages as u64),
                )?;
            }
        }
        Ok(())
    }
    pub fn charge_for_instantiation(&self, _host: &Host) -> Result<(), HostError> {
        match self {
            Self::V0 { .. } => {
                // No-op, already charged when parsing
            }
            #[cfg(feature = "next")]
            Self::V1(inputs) => {
                _host.charge_budget(
                    ContractCostType::InstantiateWasmInstructions,
                    Some(inputs.n_instructions as u64),
                )?;
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
                _host.charge_budget(
                    ContractCostType::InstantiateWasmTypes,
                    Some(inputs.n_types as u64),
                )?;
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
                    ContractCostType::InstantiateWasmMemoryPages,
                    Some(inputs.n_memory_pages as u64),
                )?;
            }
        }
        Ok(())
    }
}

/// A [ParsedModule] contains the parsed [wasmi::Module] for a given wasm blob,
/// as well as a protocol number and set of [ContractCodeCostInputs] extracted
/// from the module when it was parsed.
pub struct ParsedModule {
    pub module: Module,
    pub proto_version: u32,
    pub cost_inputs: VersionedContractCodeCostInputs,
}

impl ParsedModule {
    pub fn new(
        host: &Host,
        engine: &Engine,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<Self, HostError> {
        cost_inputs.charge_for_parsing(host)?;
        let (module, proto_version) = Self::parse_wasm(host, engine, wasm)?;
        Ok(Self {
            module,
            proto_version,
            cost_inputs,
        })
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn new_with_isolated_engine(
        host: &Host,
        wasm: &[u8],
        cost_inputs: VersionedContractCodeCostInputs,
    ) -> Result<Self, HostError> {
        use crate::budget::AsBudget;
        let config = crate::vm::get_wasmi_config(host.as_budget())?;
        let engine = Engine::new(&config);
        cost_inputs.charge_for_parsing(host)?;
        let (module, proto_version) = Self::parse_wasm(host, &engine, wasm)?;
        Ok(Self {
            module,
            proto_version,
            cost_inputs,
        })
    }

    /// Parse the wasm blob into a [Module] and its protocol number, checking check its interface version
    fn parse_wasm(host: &Host, engine: &Engine, wasm: &[u8]) -> Result<(Module, u32), HostError> {
        let module = {
            let _span0 = tracy_span!("parse module");
            host.map_err(Module::new(&engine, wasm))?
        };

        Self::check_max_args(host, &module)?;
        let interface_version = Self::check_meta_section(host, &module)?;
        let contract_proto = get_ledger_protocol_version(interface_version);

        Ok((module, contract_proto))
    }

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
    #[allow(dead_code)]
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        Self::module_custom_section(&self.module, name)
    }

    fn check_meta_section(host: &Host, m: &Module) -> Result<u64, HostError> {
        if let Some(env_meta) = Self::module_custom_section(m, meta::ENV_META_V0_SECTION_NAME) {
            let mut limits = DEFAULT_XDR_RW_LIMITS;
            limits.len = env_meta.len();
            let mut cursor = Limited::new(Cursor::new(env_meta), limits);
            if let Some(env_meta_entry) = ScEnvMetaEntry::read_xdr_iter(&mut cursor).next() {
                let ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) =
                    host.map_err(env_meta_entry)?;
                Self::check_contract_interface_version(host, v)?;
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

    // Do a second, manual parse of the wasm blob to extract cost parameters we're
    // interested in.
    #[cfg(feature = "next")]
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
        if data > costs.n_memory_pages.saturating_mul(0x10000) {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "data segments exceed memory size",
                &[],
            ));
        }
        if elements > costs.n_table_entries {
            return Err(host.err(
                ScErrorType::WasmVm,
                ScErrorCode::InvalidInput,
                "too many elements in wasm elem section(s)",
                &[],
            ));
        }
        Ok(costs)
    }

    #[cfg(feature = "next")]
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
}
