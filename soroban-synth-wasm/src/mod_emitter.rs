use crate::FuncEmitter;
use soroban_env_common::xdr::{Limits, ScEnvMetaEntry, WriteXdr};
use std::str::FromStr;
use std::{borrow::Cow, collections::BTreeMap, env};
#[cfg(feature = "adversarial")]
use wasm_encoder::StartSection;
use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, DataCountSection, DataSection, ElementSection, Elements,
    EntityType, ExportKind, ExportSection, Function, FunctionSection, GlobalSection, GlobalType,
    ImportSection, MemorySection, MemoryType, Module, RefType, TableSection, TableType,
    TypeSection, ValType,
};

/// Wrapper for a u32 that defines the arity of a function -- that is, the number of
/// inputs the function takes. In this crate function types are simplified to all
/// take only some number (the arity) of I64 values and return a single I64, so a
/// function type can be defined strictly by its arity.
#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct Arity(pub u32);

/// Wrapper for a u32 that references a function type in the `type` section of the
/// module being emitted. There will usually be only one such type for any given
/// arity, though there may be none: they are emitted into the `type` section on
/// demand, as references to them are required.
#[derive(Hash, PartialEq, Eq, Copy, Clone, PartialOrd, Ord)]
pub struct TypeRef(pub u32);

/// Wrapper for a u32 that references a function in both the `function` and
/// `code` sections of the module being emitted (the two sections have parallel
/// entries: entries in the `function` section declare functions and entries in
/// the `code` section define their bodies).
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub struct FuncRef(pub u32);

/// An index into the globals for the current module.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub struct GlobalRef(pub u32);

/// Utility type for emitting a contract WASM module, with several simplifying
/// assumptions specific to soroban contracts. For example: all function types
/// are defined using a single [`Arity`] number, assuming that all functions
/// take some number of I64 values and return a single I64.
///
/// It also manages a dictionary of types and imports, allowing access to them
/// by arity and name. These facilities are mostly used by the host function
/// helper methods on [`FuncEmitter`].
pub struct ModEmitter {
    module: Module,

    types: TypeSection,
    imports: ImportSection,
    funcs: FunctionSection,
    tables: TableSection,
    memories: MemorySection,
    globals: GlobalSection,
    exports: ExportSection,
    #[cfg(feature = "adversarial")]
    start: Option<StartSection>,
    elements: ElementSection,
    codes: CodeSection,
    data: DataSection,
    // The data count section is used to simplify single-pass validation. It is optional.
    data_count: Option<DataCountSection>,

    // key is (args arity, return arity)
    type_refs: BTreeMap<(Arity, Arity), TypeRef>,
    // import functions cannot have return arity != 1
    import_refs: BTreeMap<(String, String, Arity), FuncRef>,
}

impl Default for ModEmitter {
    /// For backward compatibility sake with earlier versions of this code, this
    /// is hard-wired to emit a module with a handful of miscellaneous content
    /// and, crucially, metadata identifying itself as a protocol-20 module
    /// rather than one from some newer release. If you want an actually-empty
    /// `ModEmitter` you should call [`ModEmitter::new`].
    fn default() -> Self {
        let mut me = Self::new();
        me.add_protocol_version_meta(20);
        me.table(RefType::FUNCREF, 128, None);
        me.memory(1, None, false, false);
        me.global(ValType::I64, true, &ConstExpr::i64_const(42));
        me.export("memory", wasm_encoder::ExportKind::Memory, 0);
        me
    }
}

impl ModEmitter {
    /// Creates the same "miscellaneous content" `ModEmitter` as
    /// [`ModEmitter::default`], but calls
    /// [`Self::add_test_protocol_version_meta`] to allow variability in the
    /// protocol version rather than hard wiring to version 20.
    pub fn default_with_test_protocol() -> Self {
        let mut me = Self::new();
        me.add_test_protocol_version_meta();
        me.table(RefType::FUNCREF, 128, None);
        me.memory(1, None, false, false);
        me.global(ValType::I64, true, &ConstExpr::i64_const(42));
        me.export("memory", wasm_encoder::ExportKind::Memory, 0);
        me
    }

    /// Creates the same `TEST_PROTOCOL`-qualified "miscellaneous content"
    /// `ModEmitter` as [`ModEmitter::default_with_test_protocol`] except with a
    /// caller-specified number of linear memory pages and function table
    /// entries.
    pub fn from_configs(mem_pages: u32, elem_count: u32) -> Self {
        let mut me = Self::new();
        me.add_test_protocol_version_meta();
        me.table(RefType::FUNCREF, elem_count, None);
        me.memory(mem_pages as u64, None, false, false);
        me.global(ValType::I64, true, &ConstExpr::i64_const(42));
        me.export("memory", wasm_encoder::ExportKind::Memory, 0);
        me
    }

    /// Add a metadata section marking the module as belonging to the specified
    /// protocol version.
    pub fn add_protocol_version_meta(&mut self, protocol_version: u32) -> &mut Self {
        let meta = ScEnvMetaEntry::ScEnvMetaKindInterfaceVersion(
            soroban_env_common::meta::make_interface_version(protocol_version, 0),
        );
        self.custom_section(
            soroban_env_common::meta::ENV_META_V0_SECTION_NAME,
            &meta.to_xdr(Limits::none()).unwrap(),
        )
    }

    /// Calls [`Self::add_protocol_version_meta`] with a number specified by
    /// either the environment variable `TEST_PROTOCOL`, or the value of
    /// [`soroban_env_common::meta::INTERFACE_VERSION`] if the environment
    /// variable is not set.
    pub fn add_test_protocol_version_meta(&mut self) -> &mut Self {
        let protocol_version = env::var("TEST_PROTOCOL")
            .map(|v| u32::from_str(&v).unwrap())
            .unwrap_or(soroban_env_common::meta::get_ledger_protocol_version(
                soroban_env_common::meta::INTERFACE_VERSION,
            ));
        self.add_protocol_version_meta(protocol_version)
    }

    /// Creates an empty `ModEmitter`, which does not even have a
    /// protocol metadata section.
    pub fn new() -> Self {
        let module = Module::new();
        let types = TypeSection::new();
        let imports = ImportSection::new();
        let funcs = FunctionSection::new();
        let tables = TableSection::new();
        let memories = MemorySection::new();
        let globals = GlobalSection::new();
        let exports = ExportSection::new();
        let elements = ElementSection::new();
        let codes = CodeSection::new();
        let data = DataSection::new();
        let type_refs = BTreeMap::new();
        let import_refs = BTreeMap::new();
        Self {
            module,
            types,
            imports,
            funcs,
            tables,
            memories,
            globals,
            exports,
            #[cfg(feature = "adversarial")]
            start: None,
            elements,
            codes,
            data,
            data_count: None,
            type_refs,
            import_refs,
        }
    }

    pub fn custom_section(&mut self, name: &str, data: &[u8]) -> &mut Self {
        self.module.section(&CustomSection {
            name: Cow::Borrowed(name),
            data: Cow::Borrowed(data),
        });
        self
    }

    pub fn table(
        &mut self,
        element_type: RefType,
        minimum: u32,
        maximum: Option<u32>,
    ) -> &mut Self {
        self.tables.table(TableType {
            element_type,
            minimum,
            maximum,
        });
        self
    }

    pub fn memory(
        &mut self,
        minimum: u64,
        maximum: Option<u64>,
        memory64: bool,
        shared: bool,
    ) -> &mut Self {
        self.memories.memory(MemoryType {
            minimum,
            maximum,
            memory64,
            shared,
        });
        self
    }

    pub fn global(&mut self, val_type: ValType, mutable: bool, init_expr: &ConstExpr) -> &mut Self {
        self.globals
            .global(GlobalType { val_type, mutable }, init_expr);
        self
    }

    pub fn export(&mut self, name: &str, kind: ExportKind, index: u32) -> &mut Self {
        self.exports.export(name, kind, index);
        self
    }

    #[cfg(feature = "adversarial")]
    pub fn start(&mut self, fid: FuncRef) -> &mut Self {
        self.start = Some(StartSection {
            function_index: fid.0,
        });
        self
    }

    pub fn data_count(&mut self, count: u32) -> &mut Self {
        self.data_count = Some(DataCountSection { count });
        self
    }

    /// Create a new [`FuncEmitter`] with the given [`Arity`] and locals count.
    /// Transfers ownership of `self` to the [`FuncEmitter`], which can be
    /// recovered by calling [`FuncEmitter::finish`].
    pub fn func(self, arity: Arity, n_locals: u32) -> FuncEmitter {
        FuncEmitter::new(self, arity, Arity(1), n_locals)
    }

    #[cfg(feature = "adversarial")]
    pub fn func_with_arity_and_ret(self, arity: Arity, ret: Arity, n_locals: u32) -> FuncEmitter {
        FuncEmitter::new(self, arity, ret, n_locals)
    }

    /// Return the unique [`TypeRef`] for a function with a given args [`Arity`]
    /// and return [`Arity`], creating such a type in the `type` section of the
    /// module if such a type does not already exist.
    #[allow(clippy::map_entry)]
    pub fn get_fn_type(&mut self, arity: Arity, ret: Arity) -> TypeRef {
        let key = (arity, ret);
        if self.type_refs.contains_key(&key) {
            self.type_refs[&key]
        } else {
            let params: Vec<_> = std::iter::repeat(ValType::I64)
                .take(key.0 .0 as usize)
                .collect();
            let rets: Vec<_> = std::iter::repeat(ValType::I64)
                .take(key.1 .0 as usize)
                .collect();
            let ty_id = TypeRef(self.types.len());
            self.types.function(params, rets);
            self.type_refs.insert(key, ty_id);
            ty_id
        }
    }

    #[cfg(feature = "adversarial")]
    pub fn add_raw_fn_type(&mut self, params: &[ValType], results: &[ValType]) {
        self.types
            .function(params.iter().cloned(), results.iter().cloned());
    }

    #[cfg(feature = "adversarial")]
    pub fn add_fn_type_no_check(&mut self, arity: Arity, ret: Arity) -> TypeRef {
        let params: Vec<_> = std::iter::repeat(ValType::I64)
            .take(arity.0 as usize)
            .collect();
        let rets: Vec<_> = std::iter::repeat(ValType::I64)
            .take(ret.0 as usize)
            .collect();
        let ty_id = TypeRef(self.types.len());
        self.types.function(params, rets);
        ty_id
    }

    /// Return the unique [`FuncRef`] for a function import with a given module
    /// name, function name, and arity, creating such an import in the `import`
    /// section of the module if it does not already exist.
    #[allow(clippy::map_entry)]
    pub fn import_func(&mut self, module: &str, fname: &str, arity: Arity) -> FuncRef {
        assert!(
            self.funcs.is_empty(),
            "must import all functions before defining any exports"
        );
        let key = (module.to_owned(), fname.to_owned(), arity);
        if self.import_refs.contains_key(&key) {
            self.import_refs[&key]
        } else {
            let import_id = FuncRef(self.imports.len());
            // import func must have return arity == 1
            let ty_id = self.get_fn_type(arity, Arity(1));
            self.imports
                .import(module, fname, EntityType::Function(ty_id.0));
            self.import_refs.insert(key, import_id);
            import_id
        }
    }

    #[cfg(feature = "adversarial")]
    pub fn import_func_no_check(&mut self, module: &str, fname: &str, arity: Arity) -> FuncRef {
        let import_id = FuncRef(self.imports.len());
        // import func must have return arity == 1
        let ty_id = self.get_fn_type(arity, Arity(1));
        self.imports
            .import(module, fname, EntityType::Function(ty_id.0));
        import_id
    }

    /// Define a function in the module with a given arity, adding its code to
    /// the `code` section of the module and declaring it in the `function`
    /// section of the module, and returning a new [`FuncRef`] denoting it.
    pub fn define_func(&mut self, arity: Arity, ret: Arity, func: &Function) -> FuncRef {
        let ty = self.get_fn_type(arity, ret);
        assert!(self.funcs.len() == self.codes.len());
        let fid = self.imports.len() + self.funcs.len();
        self.funcs.function(ty.0);
        self.codes.function(func);
        FuncRef(fid)
    }

    /// Export a given [`FuncRef`] under a given name, adding it to the
    /// `export` section of the module.
    pub fn export_func(&mut self, fid: FuncRef, name: &str) {
        self.exports
            .export(name, wasm_encoder::ExportKind::Func, fid.0);
    }

    pub fn define_elem_funcs(&mut self, funcs: &[FuncRef]) {
        let table_index = 0;
        let offset = ConstExpr::i32_const(0);
        let ids: Vec<u32> = funcs.iter().map(|r| r.0).collect();
        let functions = Elements::Functions(ids.as_slice());
        self.elements.active(Some(table_index), &offset, functions);
    }

    pub fn define_global_i64(&mut self, val: i64, mutable: bool, export: Option<&str>) {
        let idx = self.globals.len();
        self.global(ValType::I64, mutable, &ConstExpr::i64_const(val));
        if let Some(name) = export {
            self.exports.export(name, ExportKind::Global, idx);
        }
    }

    pub fn define_data_segment(&mut self, mem_offset: u32, data: Vec<u8>) {
        self.data
            .active(0, &ConstExpr::i32_const(mem_offset as i32), data);
    }

    #[cfg(feature = "adversarial")]
    pub fn define_active_elements(
        &mut self,
        table_index: Option<u32>,
        offset: &ConstExpr,
        elements: Elements<'_>,
    ) {
        self.elements.active(table_index, offset, elements);
    }

    #[cfg(feature = "adversarial")]
    pub fn define_global(&mut self, val_type: ValType, mutable: bool, init_expr: &ConstExpr) {
        self.global(val_type, mutable, init_expr);
    }

    #[cfg(feature = "adversarial")]
    pub fn define_active_data(&mut self, memory_index: u32, offset: &ConstExpr, data: Vec<u8>) {
        self.data.active(memory_index, offset, data);
    }

    /// Finish emitting code, consuming the `self`, serializing a WASM binary
    /// blob, validating and returning it. Panics the resulting blob fails
    /// validation.
    pub fn finish(mut self) -> Vec<u8> {
        // NB: these sections must be emitted in this order, by spec.
        if !self.types.is_empty() {
            self.module.section(&self.types);
        }
        if !self.imports.is_empty() {
            self.module.section(&self.imports);
        }
        if !self.funcs.is_empty() {
            self.module.section(&self.funcs);
        }
        if !self.tables.is_empty() {
            self.module.section(&self.tables);
        }
        if !self.memories.is_empty() {
            self.module.section(&self.memories);
        }
        if !self.globals.is_empty() {
            self.module.section(&self.globals);
        }
        if !self.exports.is_empty() {
            self.module.section(&self.exports);
        }
        if !self.elements.is_empty() {
            self.module.section(&self.elements);
        }
        if let Some(data_count) = self.data_count {
            self.module.section(&data_count);
        }
        if !self.codes.is_empty() {
            self.module.section(&self.codes);
        }
        if !self.data.is_empty() {
            self.module.section(&self.data);
        }
        let bytes = self.module.finish();
        match wasmparser::validate(bytes.as_slice()) {
            Ok(_) => bytes,
            Err(ty) => panic!("invalid WASM module: {:?}", ty.message()),
        }
    }

    #[cfg(feature = "adversarial")]
    pub fn finish_no_validate(mut self) -> Vec<u8> {
        // NB: these sections must be emitted in this order, by spec.
        if !self.types.is_empty() {
            self.module.section(&self.types);
        }
        if !self.imports.is_empty() {
            self.module.section(&self.imports);
        }
        if !self.funcs.is_empty() {
            self.module.section(&self.funcs);
        }
        if !self.tables.is_empty() {
            self.module.section(&self.tables);
        }
        if !self.memories.is_empty() {
            self.module.section(&self.memories);
        }
        if !self.globals.is_empty() {
            self.module.section(&self.globals);
        }
        if !self.exports.is_empty() {
            self.module.section(&self.exports);
        }
        if let Some(start) = self.start {
            self.module.section(&start);
        }
        if !self.elements.is_empty() {
            self.module.section(&self.elements);
        }
        if let Some(data_count) = self.data_count {
            self.module.section(&data_count);
        }
        if !self.codes.is_empty() {
            self.module.section(&self.codes);
        }
        if !self.data.is_empty() {
            self.module.section(&self.data);
        }

        self.module.finish()
    }
}
