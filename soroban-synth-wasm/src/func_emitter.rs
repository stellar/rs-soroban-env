use crate::{Arity, FuncRef, ModEmitter};
use soroban_env_common::{xdr::ScStatus, RawVal, Status, Symbol, Tag};
use wasm_encoder::{BlockType, Function, Instruction, ValType};

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
    pub fn const64(&mut self, i: i64) -> &mut Self {
        self.insn(&Instruction::I64Const(i))
    }
    /// Emit an [`Instruction::I32Const`]
    pub fn const32(&mut self, i: i32) -> &mut Self {
        self.insn(&Instruction::I32Const(i))
    }
    /// Emit an [`Instruction::I32Const`] representing a boolean
    pub fn const_bool(&mut self, b: bool) -> &mut Self {
        self.insn(&Instruction::I32Const(if b { 1 } else { 0 }))
    }

    /// Emit an [`Instruction::I64Mul`]
    pub fn mul64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Mul)
    }
    /// Emit an [`Instruction::I64Add`]
    pub fn add64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Add)
    }
    /// Emit an [`Instruction::I64And`]
    pub fn and64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64And)
    }
    /// Emit an [`Instruction::I32And`]
    pub fn and32(&mut self) -> &mut Self {
        self.insn(&Instruction::I32And)
    }
    /// Emit an [`Instruction::I64Or`]
    pub fn or64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Or)
    }
    /// Emit an [`Instruction::I32Or`]
    pub fn or32(&mut self) -> &mut Self {
        self.insn(&Instruction::I32Or)
    }
    /// Emit an [`Instruction::I64Shl`]
    pub fn shl64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Shl)
    }
    /// Emit an [`Instruction::I64Sub`]
    pub fn sub64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Sub)
    }
    /// Emit an [`Instruction::I64Eq`]
    pub fn eq64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Eq)
    }
    /// Emit an [`Instruction::I64Eqz`]
    pub fn eqz64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Eqz)
    }
    /// Emit an [`Instruction::I64Ne`]
    pub fn ne64(&mut self) -> &mut Self {
        self.insn(&Instruction::I64Ne)
    }

    /// Emit an [`Instruction::Unreachable`]
    pub fn trap(&mut self) -> &mut Self {
        self.insn(&Instruction::Unreachable)
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
        self.const64(Tag::rawval_mask())
            .and64()
            .const64(tag.rawval_const())
            .ne64()
            .if_then_trap()
    }

    /// Emit an [`Instruction::LocalGet`]
    pub fn get(&mut self, loc: LocalRef) -> &mut Self {
        self.insn(&Instruction::LocalGet(loc.0))
    }
    /// Emit an [`Instruction::LocalSet`]
    pub fn set(&mut self, loc: LocalRef) -> &mut Self {
        self.insn(&Instruction::LocalSet(loc.0))
    }
    /// Emit an [`Instruction::LocalTee`]
    pub fn tee(&mut self, loc: LocalRef) -> &mut Self {
        self.insn(&Instruction::LocalTee(loc.0))
    }
    /// Emit an [`Instruction::LocalTee`] followed by an
    /// [`Instruction::LocalGet`], effectively duplicating the top-of-stack
    /// element.
    pub fn dup_via(&mut self, tmp: LocalRef) -> &mut Self {
        // A quirk of the WASM "stack machine" is that it lacks most normal
        // stack-machine operators like dup or pick, uses local variables for
        // them instead. Usually this is actually better, but 'dup' is quite
        // common and useful, has to be done by bouncing off a local.
        self.tee(tmp).get(tmp)
    }
    /// Emit an [`Instruction::Drop`]
    pub fn drop(&mut self) -> &mut Self {
        self.insn(&Instruction::Drop)
    }
    /// Emit an [`Instruction::MemoryGrow`]
    pub fn mem_grow(&mut self, loc: LocalRef) -> &mut Self {
        self.insn(&Instruction::MemoryGrow(loc.0))
    }
    /// Emit an [`Instruction::MemorySize`]
    pub fn mem_size(&mut self, loc: LocalRef) -> &mut Self {
        self.insn(&Instruction::MemorySize(loc.0))
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
