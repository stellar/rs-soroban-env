use crate::{Arity, GlobalRef, ModEmitter, Operand};
use expect_test::expect;
use soroban_env_common::{RawVal, Symbol, Tag};
use wasmprinter::print_bytes;

#[test]
fn test_synth_wasm() {
    let mut fe = ModEmitter::new().func(Arity(0), 2);
    let s = fe.locals[0];
    let tmp = fe.locals[1];

    fe.push(Symbol::from_str("somekey"));
    fe.local_set(s);

    fe.map_new();

    fe.dup_via(tmp);
    fe.assert_val_tag(Tag::Object);

    // StackTop here is just a convenient way to pass the `m:Object` parameter. Instead of passing
    // it in explicitly, we just drop it on the stack and tell the instruction to pick up
    // "whatever is on top of the stack"
    fe.map_put(Operand::StackTop, s, RawVal::from_u32(123));
    fe.map_get(Operand::StackTop, s);

    let bytes = fe.finish_and_export("test_map").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");

    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (type (;1;) (func (param i64 i64 i64) (result i64)))
          (type (;2;) (func (param i64 i64) (result i64)))
          (import "m" "_" (func (;0;) (type 0)))
          (import "m" "0" (func (;1;) (type 1)))
          (import "m" "1" (func (;2;) (type 2)))
          (func (;3;) (type 0) (result i64)
            (local i64 i64)
            i64.const 62479605476329
            local.set 0
            call 0
            local.tee 1
            local.get 1
            i64.const 15
            i64.and
            i64.const 7
            i64.ne
            if  ;; label = @1
              unreachable
            end
            local.get 0
            i64.const 1969
            call 1
            local.get 0
            call 2
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test_map" (func 3))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn call_local() {
    // a local wasm function -- the callee
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Symbol::from_str("pass"));
    let (m0, f0) = fe.finish();
    // the caller
    fe = m0.func(Arity(0), 0);
    fe.call_func(f0);
    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (func (;0;) (type 0) (result i64)
            i64.const 224846729
          )
          (func (;1;) (type 0) (result i64)
            call 0
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 1))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn call_import() {
    let mut me = ModEmitter::new();
    // import the function -- the callee
    let f0 = me.import_func("t", "_", Arity(0));
    // the caller
    let mut fe = me.func(Arity(0), 0);
    fe.call_func(f0);
    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (import "t" "_" (func (;0;) (type 0)))
          (func (;1;) (type 0) (result i64)
            call 0
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 1))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn call_indirect() {
    let mut me = ModEmitter::new();
    // an imported function
    let f0 = me.import_func("t", "_", Arity(0));
    // a local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::from_str("pass"));
    let (me, f1) = fe.finish();
    // another local wasm function
    let mut fe = me.func(Arity(0), 0);
    fe.push(Symbol::from_str("fail"));
    let (mut me, f2) = fe.finish();
    // store in table
    me.define_elems(&[f0, f1, f2]);
    let ty = me.get_fn_type(Arity(0));
    // the caller
    fe = me.func(Arity(1), 0);
    fe.push(Operand::Const32(1));
    fe.call_func_indirect(ty);

    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (type (;1;) (func (param i64) (result i64)))
          (import "t" "_" (func (;0;) (type 0)))
          (func (;1;) (type 0) (result i64)
            i64.const 224846729
          )
          (func (;2;) (type 0) (result i64)
            i64.const 182893337
          )
          (func (;3;) (type 1) (param i64) (result i64)
            i32.const 1
            call_indirect (type 0)
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 3))
          (elem (;0;) (i32.const 0) func 0 1 2)
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn get_global() {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.global_get(GlobalRef(0));

    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (func (;0;) (type 0) (result i64)
            global.get 0
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 0))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn store_i64() {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.push(Operand::Const32(0));
    fe.push(Operand::Const64(5));
    fe.i64_store(0);
    fe.push(Symbol::from_str("pass"));

    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (func (;0;) (type 0) (result i64)
            i32.const 0
            i64.const 5
            i64.store align=1
            i64.const 224846729
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 0))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn br() {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.block(); // 3
    fe.block(); // 2
    fe.block(); // 1
    fe.block(); // 0
    fe.br(0);
    fe.end(); // 0
    fe.br(0);
    fe.end(); // 1
    fe.br(0);
    fe.end(); // 2
    fe.br(0);
    fe.end(); // 3
    fe.push(Symbol::from_str("pass"));
    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (func (;0;) (type 0) (result i64)
            block  ;; label = @1
              block  ;; label = @2
                block  ;; label = @3
                  block  ;; label = @4
                    br 0 (;@4;)
                  end
                  br 0 (;@3;)
                end
                br 0 (;@2;)
              end
              br 0 (;@1;)
            end
            i64.const 224846729
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 0))
        )"#]];
    expected.assert_eq(&printed);
}

#[test]
fn br_table() {
    let mut fe = ModEmitter::new().func(Arity(0), 0);
    fe.block(); // 3
    fe.block(); // 2
    fe.block(); // 1
    fe.block(); // 0
    fe.i32_const(10); // selector
    fe.br_table(&[0, 1, 2, 3], 3);
    fe.end(); // 0
    fe.push(Symbol::from_str("a"));
    fe.ret();
    fe.end(); // 1
    fe.push(Symbol::from_str("b"));
    fe.ret();
    fe.end(); // 2
    fe.push(Symbol::from_str("c"));
    fe.ret();
    fe.end(); // 3
    fe.push(Symbol::from_str("d"));
    fe.ret();
    let bytes = fe.finish_and_export("test").finish();
    let printed = print_bytes(bytes).expect("wasmprinter");
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect![[r#"
        (module
          (type (;0;) (func (result i64)))
          (func (;0;) (type 0) (result i64)
            block  ;; label = @1
              block  ;; label = @2
                block  ;; label = @3
                  block  ;; label = @4
                    i32.const 10
                    br_table 0 (;@4;) 1 (;@3;) 2 (;@2;) 3 (;@1;) 3 (;@1;)
                  end
                  i64.const 617
                  return
                end
                i64.const 633
                return
              end
              i64.const 649
              return
            end
            i64.const 665
            return
          )
          (table (;0;) 128 funcref)
          (memory (;0;) 1)
          (global (;0;) (mut i64) i64.const 42)
          (export "test" (func 0))
        )"#]];
    expected.assert_eq(&printed);
}
