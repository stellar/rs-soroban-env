use crate::{Arity, ModEmitter, Operand};
use expect_test::expect;
use soroban_env_common::{RawVal, Symbol, Tag};
use wasmprinter::print_bytes;

#[test]
fn test_synth_wasm() {
    let mut fe = ModEmitter::new().func(Arity(0), 2);
    let s = fe.locals[0];
    let tmp = fe.locals[1];

    fe.push(Symbol::from_str("somekey"));
    fe.set(s);

    fe.map_new();

    fe.dup_via(tmp);
    fe.assert_val_tag(Tag::Object);

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
          (memory (;0;) 1)
          (export "test_map" (func 3))
        )"#]];
    expected.assert_eq(&printed);
}
