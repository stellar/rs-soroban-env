mod dispatch;
mod func_info;

use crate::{budget::CostType, HostError};
use std::{io::Cursor, rc::Rc};

use super::{
    xdr::{Hash, ScVal, ScVec},
    Host, RawVal,
};
use func_info::HOST_FUNCTIONS;
use parity_wasm::elements::{self, Internal, Type};
use soroban_env_common::{
    meta,
    xdr::{ReadXdr, ScEnvMetaEntry, ScHostFnErrorCode, ScVmErrorCode},
};
use wasmi::{
    Externals, FuncInstance, ImportResolver, Module, ModuleInstance, ModuleRef, RuntimeArgs,
    RuntimeValue, ValueType,
};

impl wasmi::HostError for HostError {}

impl Externals for Host {
    fn invoke_index(
        &mut self,
        index: usize,
        args: RuntimeArgs,
    ) -> Result<Option<wasmi::RuntimeValue>, wasmi::Trap> {
        if index > HOST_FUNCTIONS.len() {
            return Err(wasmi::TrapCode::TableAccessOutOfBounds.into());
        }
        let hf = &HOST_FUNCTIONS[index];
        if hf.arity != args.len() {
            return Err(wasmi::TrapCode::UnexpectedSignature.into());
        }

        let rv = (hf.dispatch)(self, args)?;
        Ok(Some(rv))
    }

    fn max_insn_step(&self) -> u64 {
        256
    }

    fn charge_cpu(&mut self, insns: u64) -> Result<(), wasmi::TrapCode> {
        // TODO reconcile TrapCode with HostError better.
        self.charge_budget(CostType::WasmInsnExec, insns)
            .map_err(|_| wasmi::TrapCode::CpuLimitExceeded)
    }

    fn charge_mem(&mut self, bytes: u64) -> Result<(), wasmi::TrapCode> {
        self.charge_budget(CostType::WasmInsnExec, bytes)
            .map_err(|_| wasmi::TrapCode::MemLimitExceeded)
    }
}

impl ImportResolver for Host {
    fn resolve_func(
        &self,
        module_name: &str,
        field_name: &str,
        signature: &wasmi::Signature,
    ) -> Result<wasmi::FuncRef, wasmi::Error> {
        for (i, hf) in HOST_FUNCTIONS.iter().enumerate() {
            if module_name == hf.mod_str && field_name == hf.fn_str {
                if signature.params().len() != hf.arity
                    || !signature.params().iter().all(|p| *p == ValueType::I64)
                    || signature.return_type() != Some(ValueType::I64)
                {
                    return Err(wasmi::Error::Function(format!(
                        "Bad imported function signature on {}.{}",
                        module_name, field_name
                    )));
                }
                return Ok(FuncInstance::alloc_host(signature.clone(), i));
            }
        }
        Err(wasmi::Error::Function(format!(
            "No such function: {}.{}",
            module_name, field_name
        )))
    }

    fn resolve_global(
        &self,
        module_name: &str,
        field_name: &str,
        _descriptor: &wasmi::GlobalDescriptor,
    ) -> Result<wasmi::GlobalRef, wasmi::Error> {
        Err(wasmi::Error::Global(format!(
            "No such global: {}.{}",
            module_name, field_name
        )))
    }

    fn resolve_memory(
        &self,
        module_name: &str,
        field_name: &str,
        _descriptor: &wasmi::MemoryDescriptor,
    ) -> Result<wasmi::MemoryRef, wasmi::Error> {
        Err(wasmi::Error::Memory(format!(
            "No such memory: {}.{}",
            module_name, field_name
        )))
    }

    fn resolve_table(
        &self,
        module_name: &str,
        field_name: &str,
        _descriptor: &wasmi::TableDescriptor,
    ) -> Result<wasmi::TableRef, wasmi::Error> {
        Err(wasmi::Error::Table(format!(
            "No such table: {}.{}",
            module_name, field_name
        )))
    }
}

// A VM is held in a Host and contains a single WASM module instantiation.
//
// It denies modules with either floating point or start functions.
//
// In order to call construct or it, one must provide a reference to the Host,
// which will be used for looking up function references and invoking them.
// The code to do the lookups and invocation is above.
//
// Any lookups on any tables other than import functions will fail, and only
// those import functions listed above will succeed.
pub struct Vm {
    #[allow(dead_code)]
    pub(crate) contract_id: Hash,
    elements_module: elements::Module,
    instance: ModuleRef, // this is a cloneable Rc<ModuleInstance>
}

#[derive(Clone, Eq, PartialEq)]
pub struct VmFunction {
    pub name: String,
    pub param_count: usize,
    pub result_count: usize,
}

impl Vm {
    fn check_meta_section(host: &Host, m: &elements::Module) -> Result<(), HostError> {
        if let Some(env_meta) = Self::module_custom_section(m, "contractenvmetav0") {
            let mut cursor = Cursor::new(env_meta);
            for env_meta_entry in ScEnvMetaEntry::read_xdr_iter(&mut cursor) {
                match host.map_err(env_meta_entry)? {
                    ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(v) => {
                        if v == meta::INTERFACE_VERSION {
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
                "input contract missing `contractenvmetav0` section",
            ))
        }
    }

    pub fn new(
        host: &Host,
        contract_id: Hash,
        module_wasm_code: &[u8],
    ) -> Result<Rc<Self>, HostError> {
        let elements_module: elements::Module =
            host.map_err(elements::deserialize_buffer(module_wasm_code))?;

        Self::check_meta_section(host, &elements_module)?;

        let module: Module =
            host.map_err(Module::from_parity_wasm_module(elements_module.clone()))?;
        host.map_err(module.deny_floating_point())?;

        let not_started_instance = host.map_err(ModuleInstance::new(&module, host))?;
        if not_started_instance.has_start() {
            return Err(host.err_status_msg(
                ScVmErrorCode::Instantiation,
                "module contains disallowed start function",
            ));
        }
        let instance = not_started_instance.assert_no_start();
        Ok(Rc::new(Self {
            contract_id,
            elements_module,
            instance,
        }))
    }

    pub(crate) fn with_memory_access<F, U>(&self, host: &Host, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&wasmi::MemoryRef) -> Result<U, HostError>,
    {
        match self.instance.export_by_name("memory") {
            Some(ev) => match ev.as_memory() {
                Some(mem) => f(mem),
                None => Err(host.err_status_msg(
                    ScVmErrorCode::Memory,
                    "export name `memory` is not of memory type",
                )),
            },
            None => Err(host.err_status_msg(ScVmErrorCode::Memory, "`memory` export not found")),
        }
    }

    pub(crate) fn invoke_function_raw(
        self: &Rc<Self>,
        host: &Host,
        func: &str,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        let mut frame_guard = host.push_vm_frame(self.clone());
        let wasm_args: Vec<_> = args
            .iter()
            .map(|i| RuntimeValue::I64(i.get_payload() as i64))
            .collect();

        // Wasmi wants a mut& to an E:Externals value, which is reasonable but
        // irrelevant (and awkward) in our case since our Host (which implements
        // Externals) is a Rc<RefCell<...>> with interior mutability. So rather
        // than propagate the irrelevant-and-awkward mut& requirement to our
        // callers, we just clone Host (which is, again, just a refcounted
        // pointer) into a mutable local variable here, and pass a &mut to that
        // local.
        let mut host = host.clone();

        let wasm_res = self
            .instance
            .invoke_export(func, wasm_args.as_slice(), &mut host);
        let wasm_ret = host.map_err(wasm_res)?;
        if let Some(RuntimeValue::I64(ret)) = wasm_ret {
            frame_guard.commit();
            Ok(RawVal::from_payload(ret as u64))
        } else {
            Err(host.err_status_msg(
                ScVmErrorCode::TrapUnexpectedSignature,
                "contract function did not return an i64",
            ))
        }
    }

    /// Invoke a function in the VM's module, converting externally stable
    /// XDR ScVal arguments into Host-specific RawVals and converting the
    /// RawVal result back to an ScVal.
    ///
    /// This function has to take self by [`Rc`] because it stores self in
    /// a new [`Frame`]
    pub fn invoke_function(
        self: &Rc<Self>,
        host: &Host,
        func: &str,
        args: &ScVec,
    ) -> Result<ScVal, HostError> {
        let mut raw_args: Vec<RawVal> = Vec::new();
        for scv in args.0.iter() {
            raw_args.push(host.to_host_val(scv)?.val);
        }
        let raw_res = self.invoke_function_raw(host, func, raw_args.as_slice())?;
        Ok(host.from_host_val(raw_res)?)
    }

    /// Functions returns a list of functions in the WASM loaded into the Vm.
    pub fn functions(&self) -> Vec<VmFunction> {
        if let Some(export_section) = self.elements_module.export_section() {
            let fn_import_count = self
                .elements_module
                .import_count(elements::ImportCountType::Function);
            // A function in the export section points to a function in the
            // function section, which references a type in the type section.
            // The type contains the list of parameters.
            export_section
                .entries()
                .iter()
                .filter_map(|entry| match entry.internal() {
                    Internal::Function(idx) => {
                        let fn_type = self.elements_module.function_section().and_then(|fs| {
                            // Function index space includes imported
                            // functions. Imported functions are always
                            // indexed prior to other functions and so
                            // the index points into the space of the
                            // imported functions and the module's
                            // functions. To get the index of the
                            // function in the function section, the
                            // index is reduced by the import count.
                            let fs_idx = (*idx as usize) - fn_import_count;
                            fs.entries().get(fs_idx).and_then(|f| {
                                self.elements_module.type_section().and_then(|ts| {
                                    ts.types().get(f.type_ref() as usize).and_then(|t| match t {
                                        Type::Function(ft) => Some(ft),
                                    })
                                })
                            })
                        });
                        Some(VmFunction {
                            name: entry.field().to_string(),
                            param_count: fn_type.map_or(0, |fn_type| fn_type.params().len()),
                            result_count: fn_type.map_or(0, |fn_type| fn_type.results().len()),
                        })
                    }
                    _ => None,
                })
                .collect()
        } else {
            vec![]
        }
    }

    fn module_custom_section(m: &elements::Module, name: impl AsRef<str>) -> Option<&[u8]> {
        m.custom_sections().find_map(|s| {
            if s.name() == name.as_ref() {
                Some(s.payload())
            } else {
                None
            }
        })
    }

    // Custom section returns the bytes within the named custom section of the
    // loaded WASM file.
    pub fn custom_section(&self, name: impl AsRef<str>) -> Option<&[u8]> {
        Self::module_custom_section(&self.elements_module, name)
    }
}
