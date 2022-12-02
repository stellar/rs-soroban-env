use crate::FuncEmitter;
use std::collections::HashMap;
use wasm_encoder::{
    CodeSection, ConstExpr, CustomSection, ElementSection, Elements, EntityType, ExportSection,
    Function, FunctionSection, GlobalSection, GlobalType, ImportSection, MemorySection, MemoryType,
    Module, TableSection, TableType, TypeSection, ValType,
};

/// Wrapper for a u32 that defines the arity of a function -- that is, the number of
/// inputs the function takes. In this crate function types are simplified to all
/// take only some number (the arity) of I64 values and return a single I64, so a
/// function type can be defined strictly by its arity.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub struct Arity(pub u32);

/// Wrapper for a u32 that references a function type in the `type` section of the
/// module being emitted. There will usually be only one such type for any given
/// arity, though there may be none: they are emitted into the `type` section on
/// demand, as references to them are required.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
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
    elements: ElementSection,
    codes: CodeSection,

    type_refs: HashMap<Arity, TypeRef>,
    import_refs: HashMap<(String, String, Arity), FuncRef>,
}

impl ModEmitter {
    pub fn new() -> Self {
        let mut module = Module::new();

        let metasection = CustomSection {
            name: soroban_env_common::meta::ENV_META_V0_SECTION_NAME,
            data: &soroban_env_common::meta::XDR,
        };
        module.section(&metasection);

        let types = TypeSection::new();
        let imports = ImportSection::new();
        let funcs = FunctionSection::new();
        let mut tables = TableSection::new();
        tables.table(TableType {
            element_type: ValType::FuncRef,
            minimum: 128,
            maximum: None,
        });
        let mut memories = MemorySection::new();
        memories.memory(MemoryType {
            minimum: 1,
            maximum: None,
            memory64: false,
            shared: false,
        });
        let mut globals = GlobalSection::new();
        globals.global(
            GlobalType {
                val_type: ValType::I64,
                mutable: true,
            },
            &ConstExpr::i64_const(42),
        );
        let exports = ExportSection::new();
        let elements = ElementSection::new();
        let codes = CodeSection::new();
        let typerefs = HashMap::new();
        let importrefs = HashMap::new();
        Self {
            module,
            types,
            imports,
            funcs,
            tables,
            memories,
            globals,
            exports,
            elements,
            codes,
            type_refs: typerefs,
            import_refs: importrefs,
        }
    }

    /// Create a new [`FuncEmitter`] with the given [`Arity`] and locals count.
    /// Transfers ownership of `self` to the [`FuncEmitter`], which can be
    /// recovered by calling [`FuncEmitter::finish`].
    pub fn func(self, arity: Arity, n_locals: u32) -> FuncEmitter {
        FuncEmitter::new(self, arity, n_locals)
    }

    /// Return the unique [`TypeRef`] for a function with a given [`Arity`],
    /// creating such a type in the `type` section of the module if such a type
    /// does not already exist.
    pub fn get_fn_type(&mut self, arity: Arity) -> TypeRef {
        if self.type_refs.contains_key(&arity) {
            self.type_refs[&arity]
        } else {
            let params: Vec<_> = std::iter::repeat(ValType::I64)
                .take(arity.0 as usize)
                .collect();
            let ty_id = TypeRef(self.types.len());
            self.types.function(params, vec![ValType::I64]);
            self.type_refs.insert(arity, ty_id);
            ty_id
        }
    }

    /// Return the unique [`FuncRef`] for a function import with a given module
    /// name, function name, and arity, creating such an import in the `import`
    /// section of the module if it does not already exist.
    pub fn import_func(&mut self, module: &str, fname: &str, arity: Arity) -> FuncRef {
        if self.funcs.len() != 0 {
            panic!("must import all functions before defining any exports");
        }
        let key = (module.to_owned(), fname.to_owned(), arity);
        if self.import_refs.contains_key(&key) {
            self.import_refs[&key]
        } else {
            let import_id = FuncRef(self.imports.len());
            let ty_id = self.get_fn_type(arity);
            self.imports
                .import(module, fname, EntityType::Function(ty_id.0));
            self.import_refs.insert(key, import_id);
            import_id
        }
    }

    /// Define a function in the module with a given arity, adding its code to
    /// the `code` section of the module and declaring it in the `function`
    /// section of the module, and returning a new [`FuncRef`] denoting it.
    pub fn define_func(&mut self, arity: Arity, func: &Function) -> FuncRef {
        let ty = self.get_fn_type(arity);
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

    pub fn define_elems(&mut self, funcs: &[FuncRef]) {
        let table_index = 0;
        let offset = ConstExpr::i32_const(0);
        let element_type = ValType::FuncRef;
        let ids: Vec<u32> = funcs.iter().map(|r| r.0).collect();
        let functions = Elements::Functions(ids.as_slice());
        self.elements
            .active(Some(table_index), &offset, element_type, functions);
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
        if !self.codes.is_empty() {
            self.module.section(&self.codes);
        }
        let bytes = self.module.finish();
        match wasmparser::validate(bytes.as_slice()) {
            Ok(_) => bytes,
            Err(ty) => panic!("invalid WASM module: {:?}", ty.message()),
        }
    }
}
