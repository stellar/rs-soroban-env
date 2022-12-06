use crate::{Arity, FuncRef, GlobalRef, ModEmitter, TypeRef};
use soroban_env_common::{xdr::ScStatus, RawVal, Status, Symbol, Tag};
use wasm_encoder::{BlockType, Function, Instruction, MemArg, ValType};

/// An index into the _locals_ for the current function, which may refer
/// either to a function argument or a local variable.
#[derive(Hash, PartialEq, Eq, Copy, Clone)]
pub struct LocalRef(pub u32);

/// An abstraction over inputs to WASM operations: locals, constants, and/or the
/// implicit value already on the top of the WASM stack machine. Primarily used
/// as an [`Into`] target type for the [`FuncEmitter::push`] method, allowing
/// various types such as [`RawVal`] or [`Symbol`] to be passed to host function
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

impl From<RawVal> for Operand {
    fn from(r: RawVal) -> Self {
        Operand::Const64(r.get_payload() as i64)
    }
}

impl From<Symbol> for Operand {
    fn from(s: Symbol) -> Self {
        let r: RawVal = s.into();
        r.into()
    }
}

impl From<Status> for Operand {
    fn from(s: Status) -> Self {
        let r: RawVal = s.into();
        r.into()
    }
}

impl From<ScStatus> for Operand {
    fn from(s: ScStatus) -> Self {
        let r: RawVal = s.into();
        r.into()
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
    pub(crate) func: Function,

    /// A vector of [`LocalRef`]s that cover the arguments to the function. If
    /// the [`FuncEmitter`] is constructed with `Arity(k)` then the `args`
    /// vector be `k` elements long, and the `n`th element of it will be the
    /// `n`th argument passed to the function (and will happen to have index
    /// `n`, but the point here is to let client code ignore that fact).
    pub args: Vec<LocalRef>,

    /// A vector of [`LocalRef`]s that cover the locals declared inside the
    /// function, with local indexes starting _after_ the argument indexes.
    /// If the [`FuncEmitter`] is constructed with `n_locals=k` then the
    /// `locals` vector will be `k` elements long, and the `n`th element of
    /// it will be the `n`th local variable, regardless of the argument count
    /// (it will happen to have index `n+a` where `a` is the arity of the
    /// function, but the point here is to let client code ignore that fact).
    pub locals: Vec<LocalRef>,
}

impl FuncEmitter {
    /// Construct a new `FuncEmitter` with `arity` arguments and `n_locals`
    /// local variables. This will establish two member vectors
    /// [`FuncEmitter::args`] and [`FuncEmitter::locals`] of [`LocalRef`]s, each
    /// indexing into the arguments and locals. Access these member vectors
    /// directly to get the corresponding [`LocalRef`]s.
    pub fn new(mod_emit: ModEmitter, arity: Arity, n_locals: u32) -> Self {
        let func = Function::new([(n_locals, ValType::I64)]);
        let args = (0..arity.0).map(|n| LocalRef(n)).collect();
        let locals = (arity.0..arity.0 + n_locals).map(|n| LocalRef(n)).collect();
        Self {
            mod_emit,
            arity,
            func,
            args,
            locals,
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
                if !first {
                    panic!("can only use Operand::StackTop as first arg");
                }
                return self;
            }
            Operand::Local(loc) => Instruction::LocalGet(loc.0),
            Operand::Const64(con) => Instruction::I64Const(con),
            Operand::Const32(con) => Instruction::I32Const(con),
        };
        self.insn(&insn)
    }

    /// Emit an [`Instruction::I64Const`]
    pub fn i64_const(&mut self, i: i64) -> &mut Self {
        self.insn(&Instruction::I64Const(i))
    }
    /// Emit an [`Instruction::I32Const`]
    pub fn i32_const(&mut self, i: i32) -> &mut Self {
        self.insn(&Instruction::I32Const(i))
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
        self.i64_const(Tag::rawval_mask())
            .i64_and()
            .i64_const(tag.rawval_const())
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
        let fid = self.mod_emit.define_func(self.arity, &self.func);
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

macro_rules! i64_mem_insn {
    ( $(($func_name: ident, $insn: ident)),* )
    =>
    {
        impl FuncEmitter {
        $(
            pub fn $func_name(&mut self, memory_index: u32) -> &mut Self {
                self.insn(&Instruction::$insn(MemArg {
                    offset: 0,
                    align: 0,
                    memory_index,
                }))
            }
        )*
        }
    }
}
i64_mem_insn!(
    (i64_store, I64Store),
    (i64_load, I64Load),
    (i64_load8_s, I64Load8S),
    (i64_load16_s, I64Load16S),
    (i64_load32_s, I64Load32S),
    (i64_store8, I64Store8),
    (i64_store16, I64Store16),
    (i64_store32, I64Store32)
);

macro_rules! i64_numeric_insn {
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
i64_numeric_insn!(
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
    (i64_rotr, I64Rotr)
);
