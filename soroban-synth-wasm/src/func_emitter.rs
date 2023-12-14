use crate::{Arity, FuncRef, GlobalRef, ModEmitter, TypeRef};
use soroban_env_common::{
    xdr::ScError, Bool, Error, I32Val, StorageType, Symbol, Tag, U32Val, Val, Void,
};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

/// An index into the _locals_ for the current function, which may refer
/// either to a function argument or a local variable.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub struct LocalRef(pub u32);

/// An abstraction over inputs to WASM operations: locals, constants, and/or the
/// implicit value already on the top of the WASM stack machine. Primarily used
/// as an [`Into`] target type for the [`FuncEmitter::push`] method, allowing
/// various types such as [`Val`] or [`Symbol`] to be passed to host function
/// call emitters, which in turn pass them to [`FuncEmitter::push`].
///
/// The [`Operand::StackTop`] case can only be provided as the first argument to
/// any sequence of push calls. See [`FuncEmitter::push_full`] for details.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub enum Operand {
    Local(LocalRef),
    Const64(i64),
    Const32(i32),
    StackTop,
}

impl From<LocalRef> for Operand {
    fn from(lr: LocalRef) -> Self {
        Operand::Local(lr)
    }
}

impl From<i64> for Operand {
    fn from(r: i64) -> Self {
        Operand::Const64(r)
    }
}

impl From<u64> for Operand {
    fn from(r: u64) -> Self {
        Operand::Const64(r as i64)
    }
}

impl From<StorageType> for Operand {
    fn from(r: StorageType) -> Self {
        Operand::Const64(r as i64)
    }
}

impl From<Val> for Operand {
    fn from(r: Val) -> Self {
        Operand::Const64(r.get_payload() as i64)
    }
}

impl From<Bool> for Operand {
    fn from(s: Bool) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<Void> for Operand {
    fn from(s: Void) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<U32Val> for Operand {
    fn from(s: U32Val) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<I32Val> for Operand {
    fn from(s: I32Val) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<Symbol> for Operand {
    fn from(s: Symbol) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<Error> for Operand {
    fn from(s: Error) -> Self {
        Operand::from(Val::from(s))
    }
}

impl From<ScError> for Operand {
    fn from(s: ScError) -> Self {
        Operand::from(Val::from(s))
    }
}

/// Emits a single function into a parent [`ModEmitter`]. Mostly this is a type
/// to hang utility functions off of that generate complex instruction
/// sequences, specifically those that emit calls to imports of all the host
/// functions defined in [`soroban_env_common::Env`].
///
/// Elements of the member vectors [`FuncEmitter::args`] and
/// [`FuncEmitter::locals`] should be used to access correctly-indexed
/// [`LocalRef`]s pointing to function arguments and locals, respectively.
pub struct FuncEmitter {
    pub(crate) mod_emit: ModEmitter,
    pub(crate) arity: Arity,
    pub(crate) ret: Arity,
    pub(crate) func: Function,

    /// A vector of [`LocalRef`]s that cover the arguments to the function. If
    /// the [`FuncEmitter`] is constructed with `Arity(k)` then the `args`
    /// vector be `k` elements long, and the `n`th element of it will be the
    /// `n`th argument passed to the function (and will happen to have index
    /// `n`, but the point here is to let client code ignore that fact).
    ///
    /// We also store an optional `&'static str` associated with each arg,
    /// indicating that it's _allocated_ to a specific type of value (with
    /// meaning of the strings decided by the caller). This is a convention
    /// maintained by [`FuncEmitter::alloc_arg`].
    pub args: Vec<(LocalRef, Option<&'static str>)>,

    /// A vector of [`LocalRef`]s that cover the locals declared inside the
    /// function, with local indexes starting _after_ the argument indexes. If
    /// the [`FuncEmitter`] is constructed with `n_locals=k` then the `locals`
    /// vector will be `k` elements long, and the `n`th element of it will be
    /// the `n`th local variable, regardless of the argument count (it will
    /// happen to have index `n+a` where `a` is the arity of the function, but
    /// the point here is to let client code ignore that fact).
    ///
    /// We also store an optional `&'static str` associated with each local,
    /// indicating that it's _allocated_ to a specific type of value (with
    /// meaning of the strings decided by the caller). This is a convention
    /// maintained by [`FuncEmitter::alloc_local`].
    pub locals: Vec<(LocalRef, Option<&'static str>)>,
}

impl FuncEmitter {
    /// Construct a new `FuncEmitter` with `arity` arguments and `n_locals`
    /// local variables. This will establish two member vectors
    /// [`FuncEmitter::args`] and [`FuncEmitter::locals`] of [`LocalRef`]s, each
    /// indexing into the arguments and locals. Access these member vectors
    /// directly to get the corresponding [`LocalRef`]s.
    pub fn new(mod_emit: ModEmitter, arity: Arity, ret: Arity, n_locals: u32) -> Self {
        let func = Function::new([(n_locals, ValType::I64)]);
        let args = (0..arity.0).map(|n| (LocalRef(n), None)).collect();
        let locals = (arity.0..arity.0 + n_locals)
            .map(|n| (LocalRef(n), None))
            .collect();
        Self {
            mod_emit,
            arity,
            ret,
            func,
            args,
            locals,
        }
    }

    pub fn alloc_arg(&mut self, ty: &'static str) -> LocalRef {
        for (lref, ltag) in self.args.iter_mut() {
            if ltag.is_none() {
                *ltag = Some(ty);
                return *lref;
            }
        }
        panic!(
            "alloc_arg exhausted {} args without finding room for {:?}",
            self.args.len(),
            ty
        )
    }

    pub fn alloc_local(&mut self, ty: &'static str) -> LocalRef {
        for (lref, ltag) in self.locals.iter_mut() {
            if ltag.is_none() {
                *ltag = Some(ty);
                return *lref;
            }
        }
        panic!(
            "alloc_local exhausted {} locals without finding room for {:?}",
            self.locals.len(),
            ty
        )
    }

    pub fn alloc_and_store_local(&mut self, ty: &'static str) -> LocalRef {
        let lr = self.alloc_local(ty);
        self.local_set(lr);
        lr
    }

    // A utility function for selecting a typed local based on an index into the
    // subsequence of locals (or args) with a given tag. If no appropriate local
    // can be found, we return None.
    pub fn maybe_choose_local(&mut self, ty: &'static str, index: u8) -> Option<LocalRef> {
        let index = (index as usize) % (self.args.len() + self.locals.len());
        if let Some((local, _)) = self
            .args
            .iter()
            .chain(self.locals.iter())
            .filter(|(_, tyopt)| *tyopt == Some(ty))
            .cycle()
            .nth(index)
        {
            Some(*local)
        } else {
            None
        }
    }

    /// Emit an instruction into the function.
    pub fn insn(&mut self, i: &Instruction) -> &mut Self {
        self.func.instruction(i);
        self
    }

    /// Push an [`Operand`] (or something that can be converted into one) onto
    /// the stack. Does not allow pushing [`Operand::StackTop`], see
    /// [`FuncEmitter::push_full`] for details.
    pub fn push<OP: Into<Operand>>(&mut self, op: OP) -> &mut Self {
        let mut is_first_arg = false;
        self.push_full(op, &mut is_first_arg)
    }

    /// Push an [`Operand`] (or something that can be converted into one) onto
    /// the stack, passing an additional mutable `bool` flag indicating whether
    /// this call is logically "first" in a sequence of pushes. This flag is
    /// reset to `false` during each call, but its _input_ state is used to
    /// control whether [`Operand::StackTop`] is allowed: it is only allowed
    /// when `*is_first_arg==true` initially, and passing [`Operand::StackTop`]
    /// when `*is_first_arg==false` will panic. This mechanism is only used by
    /// the wrapper methods that emit [`soroban_env_common::Env`] host function
    /// calls, and allows them to support callers passing [`Operand::StackTop`]
    /// as their first argument, but only their first argument.
    pub fn push_full<OP: Into<Operand>>(&mut self, op: OP, is_first_arg: &mut bool) -> &mut Self {
        let first = *is_first_arg;
        *is_first_arg = false;
        let insn = match op.into() {
            Operand::StackTop => {
                assert!(first, "can only use Operand::StackTop as first arg");
                return self;
            }
            Operand::Local(loc) => Instruction::LocalGet(loc.0),
            Operand::Const64(con) => Instruction::I64Const(con),
            Operand::Const32(con) => Instruction::I32Const(con),
        };
        self.insn(&insn)
    }

    /// Emit an [`Instruction::If`], call `t(self)`, then emit
    /// an [`Instruction::End`].
    pub fn if_then<THEN>(&mut self, t: THEN) -> &mut Self
    where
        THEN: FnOnce(&mut Self) -> &mut Self,
    {
        let s = self.insn(&Instruction::If(BlockType::Empty));
        let s = t(s);
        s.insn(&Instruction::End)
    }

    /// Emit an if-then that traps.
    pub fn if_then_trap(&mut self) -> &mut Self {
        self.if_then(|fe| fe.trap())
    }

    /// Emit code to check that the top of stack value has a given [`Tag`],
    /// trapping if not. Consumes the top of stack value, so you might need to
    /// call [`FuncEmitter::dup_via`] first.
    pub fn assert_val_tag(&mut self, tag: Tag) -> &mut Self {
        self.i64_const(Tag::val_mask())
            .i64_and()
            .i64_const(tag.val_const())
            .i64_ne()
            .if_then_trap()
    }

    /// Emit an [`Instruction::LocalTee`] followed by an
    /// [`Instruction::LocalGet`], effectively duplicating the top-of-stack
    /// element.
    pub fn dup_via(&mut self, tmp: LocalRef) -> &mut Self {
        // A quirk of the WASM "stack machine" is that it lacks most normal
        // stack-machine operators like dup or pick, uses local variables for
        // them instead. Usually this is actually better, but 'dup' is quite
        // common and useful, has to be done by bouncing off a local.
        self.local_tee(tmp).local_get(tmp)
    }
    /// Emit an [`Instruction::BrTable`]
    pub fn br_table(&mut self, ls: &[u32], l: u32) -> &mut Self {
        self.insn(&Instruction::BrTable(std::borrow::Cow::Borrowed(ls), l))
    }
    /// Emit an [`Instruction::MemoryGrow`]
    pub fn memory_grow(&mut self) -> &mut Self {
        self.insn(&Instruction::MemoryGrow(0))
    }
    /// Emit an [`Instruction::MemorySize`]
    pub fn memory_size(&mut self) -> &mut Self {
        self.insn(&Instruction::MemorySize(0))
    }
    /// Emit an [`Instruction::MemoryFill`]
    pub fn memory_fill(&mut self) -> &mut Self {
        self.insn(&Instruction::MemoryFill(0))
    }
    /// Emit an [`Instruction::Block`]
    pub fn block(&mut self) -> &mut Self {
        self.insn(&Instruction::Block(BlockType::Empty))
    }
    /// Emit an [`Instruction::Br`]
    pub fn br(&mut self, loc: u32) -> &mut Self {
        self.insn(&Instruction::Br(loc))
    }
    /// Emit an [`Instruction::Call`]
    pub fn call_func(&mut self, fun: FuncRef) -> &mut Self {
        self.insn(&Instruction::Call(fun.0))
    }
    /// Emit an [`Instruction::CallIndirect`]
    pub fn call_func_indirect(&mut self, ty: TypeRef) -> &mut Self {
        self.insn(&Instruction::CallIndirect { ty: ty.0, table: 0 })
    }

    /// Emit an [`Instruction::End`] and finish emitting code for this function,
    /// defining it in its enclosing [`ModEmitter`] and returning that
    /// [`ModEmitter`] as well as a [`FuncRef`] referring to the newly-defined
    /// function.
    pub fn finish(mut self) -> (ModEmitter, FuncRef) {
        self.insn(&Instruction::End);
        let fid = self.mod_emit.define_func(self.arity, self.ret, &self.func);
        (self.mod_emit, fid)
    }

    /// Call [`FuncEmitter::finish`] and then export the newly-defined function
    /// under the provided `name`, returning only the [`ModEmitter`].
    pub fn finish_and_export(self, name: &str) -> ModEmitter {
        let (mut me, fid) = self.finish();
        me.export_func(fid, name);
        me
    }
}

macro_rules! trivial_control_insn {
    ( $(($func_name: ident, $insn: ident)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self) -> &mut Self {
                self.insn(&Instruction::$insn)
            }
        )*}
    };
}
trivial_control_insn!(
    (drop, Drop),
    (select, Select),
    (end, End),
    (ret, Return),
    (trap, Unreachable)
);

macro_rules! variable_insn {
    ( $(($func_name: ident, $insn: ident, $ref: ty)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self, r: $ref) -> &mut Self {
                self.insn(&Instruction::$insn(r.0))
            }
        )*
        }
    }
}
variable_insn!(
    (local_get, LocalGet, LocalRef),
    (local_set, LocalSet, LocalRef),
    (local_tee, LocalTee, LocalRef),
    (global_get, GlobalGet, GlobalRef),
    (global_set, GlobalSet, GlobalRef)
);

macro_rules! consts {
    ( $(($func_name: ident, $insn: ident, $ty: ty)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self, i: $ty,
            ) -> &mut Self {
                self.insn(&Instruction::$insn(i))
            }
        )*
        }
    }
}
consts!((i64_const, I64Const, i64), (i32_const, I32Const, i32));
#[cfg(feature = "adversarial")]
consts!((f64_const, F64Const, f64));

macro_rules! store_load {
    ( $(($func_name: ident, $insn: ident)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self, offset: u64,
                align: u32
            ) -> &mut Self {
                self.insn(&Instruction::$insn(MemArg {
                    offset,
                    align,
                    memory_index: 0,
                }))
            }
        )*
        }
    }
}
store_load!(
    (i64_store, I64Store),
    (i64_load, I64Load),
    (i64_load8_s, I64Load8S),
    (i64_load16_s, I64Load16S),
    (i64_load32_s, I64Load32S),
    (i64_store8, I64Store8),
    (i64_store16, I64Store16),
    (i64_store32, I64Store32),
    (f64_load, F64Load)
);
#[cfg(feature = "adversarial")]
store_load!(
    (f64_store, F64Store),
    (v128_load, V128Load),
    (v128_store, V128Store)
);

macro_rules! numeric_insn {
    ( $(($func_name: ident, $insn: ident)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self) -> &mut Self {
                self.insn(&Instruction::$insn)
            }
        )*
        }
    }
}
numeric_insn!(
    (i32_wrap_i64, I32WrapI64),
    (i64_eqz, I64Eqz),
    (i64_eq, I64Eq),
    (i64_ne, I64Ne),
    (i64_lt_s, I64LtS),
    (i64_lt_u, I64LtU),
    (i64_gt_s, I64GtS),
    (i64_gt_u, I64GtU),
    (i64_le_s, I64LeS),
    (i64_le_u, I64LeU),
    (i64_ge_s, I64GeS),
    (i64_ge_u, I64GeU),
    (i64_clz, I64Clz),
    (i64_ctz, I64Ctz),
    (i64_popcnt, I64Popcnt),
    (i64_add, I64Add),
    (i64_sub, I64Sub),
    (i64_mul, I64Mul),
    (i64_div_s, I64DivS),
    (i64_div_u, I64DivU),
    (i64_rem_s, I64RemS),
    (i64_rem_u, I64RemU),
    (i64_and, I64And),
    (i64_or, I64Or),
    (i64_xor, I64Xor),
    (i64_shl, I64Shl),
    (i64_shr_s, I64ShrS),
    (i64_shr_u, I64ShrU),
    (i64_rotl, I64Rotl),
    (i64_rotr, I64Rotr),
    // post-MVP instructions
    (i64_extend32s, I64Extend32S)
);
#[cfg(feature = "adversarial")]
numeric_insn!(
    (f64_add, F64Add),
    (f64_sub, F64Sub),
    (f64_mul, F64Mul),
    (f64_div, F64Div),
    (i32x4_add, I32x4Add)
);
