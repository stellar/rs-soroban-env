//! This module contains "expressions": a recursive datatype that describes
//! well-formed (and well-typed) program fragments we can translate to WASM
//! using the [`FuncEmitter`] interface.
//!
//! Each expresison type in this module implements the [`Emit`] trait that
//! generates some WASM code (depending on the expression type and variant) and
//! returns a weakly-typed [`Operand`], which can in turn be used as an input to
//! code emitting any enclosing expression. The higher-level [`Val`] type that
//! each such [`Operand`] actually encodes depends on the expression type. For
//! example the [`Operand`]s returned from emitting any [`MapExpr`] will encode
//! an object handle with tag [`Tag::MapObject`].
//!
//! Despite the fact that WASM is loosely-speaking "a stack machine", for a
//! couple different reasons when generating code here we have each expression
//! leave _nothing_ on the WASM stack, instead storing its result to a local and
//! returning a [`LocalRef`] to it. This both fits other parts of the code
//! generation interface better and allows us to recycle locals by type between
//! expressions without having to model or shuffle the stack.
//!
//! The expression types exist partly to facilitate higher-level code generation
//! but mostly so that the fuzzer can derive and generate [`Arbitrary`]
//! instances of them, as concrete datatypes fed to a fuzz target.

#![allow(non_camel_case_types)]
#![allow(dead_code)]
#![allow(unused_imports)]
use crate::{Arity, FuncEmitter, ModEmitter, Operand};
use soroban_env_macros::generate_synth_wasm_expr_type;

pub trait Emit {
    /// Drive a [`FuncEmitter`] to generate WASM code that evaluates `self`,
    /// then return an [`Operand`] that refers to the result of that evaluation,
    /// usually a [`LocalRef`]
    fn emit(&self, func: &mut FuncEmitter) -> Operand;

    /// Return the number of local variables required to emit this expression. We
    /// need this because our WASM emitter needs a local-variable count up front
    /// before it emits. Typically this will be the sum of all of the
    /// num_locals() results from all of self's subexpressions plus one.
    fn num_locals(&self) -> u32;

    fn as_single_function_wasm_module(&self, name: &str, argtypes: &[&'static str]) -> Vec<u8> {
        let n_locals = self.num_locals();
        let mut fe =
            ModEmitter::default_with_test_protocol().func(Arity(argtypes.len() as u32), n_locals);
        for arg in argtypes.iter() {
            fe.alloc_arg(arg);
        }
        let result = self.emit(&mut fe);
        fe.push(result);
        fe.finish_and_export(name).finish()
    }
}

// This is a little awkward, but ExprVal is such a branchy type that the default
// "20 levels deep" recursion that Arbitrary allows in its depth limiter never
// even manages to finish calculating `size_hint` recursively while starting up.
// We avoid this by wrapping ExprVal in our own Arbitrary impl that is .. a lot
// more aggressive.
#[derive(Clone, Debug)]
pub struct Expr(pub ExprVal);
impl arbitrary::Arbitrary<'_> for Expr {
    fn size_hint(_depth: usize) -> (usize, Option<usize>) {
        (0, None)
    }

    fn arbitrary(u: &mut arbitrary::Unstructured<'_>) -> arbitrary::Result<Self> {
        Ok(Expr(ExprVal::arbitrary(u)?))
    }
}

generate_synth_wasm_expr_type!("../soroban-env-common/env.json");
