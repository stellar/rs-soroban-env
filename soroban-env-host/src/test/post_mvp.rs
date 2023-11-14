use crate::{Env, EnvBase, Host, HostError, Symbol, Tag, TryFromVal};
use soroban_synth_wasm::{Arity, ModEmitter};

// Emit a wasm module that uses post-MVP WASM features. Specifically
// mutable-globals and sign-ext.
fn post_mvp_wasm_module() -> Vec<u8> {
    let mut me = ModEmitter::default();

    // Emit an exported mutable global
    me.define_global_i64(-100, true, Some("global"));

    let mut fe = me.func(Arity(0), 0);
    fe.i64_const(0x0000_0000_ffff_abcd_u64 as i64);

    // Emit an op from the new sign-ext repertoire
    fe.i64_extend32s();

    // Turn this into a I64Small
    fe.i64_const(8);
    fe.i64_shl();
    fe.i64_const(Tag::I64Small as i64);
    fe.i64_or();

    fe.finish_and_export("test").finish()
}

#[test]
fn test_enabled_post_mvp_features() -> Result<(), HostError> {
    let wasm = post_mvp_wasm_module();
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let addr = host.register_test_contract_wasm(wasm.as_slice());
    let res = host.call(
        addr,
        Symbol::try_from_small_str("test")?,
        host.vec_new_from_slice(&[])?,
    )?;
    let res_i64 = i64::try_from_val(&*host, &res)?;
    assert_eq!(res_i64, 0xffff_ffff_ffff_abcd_u64 as i64);
    Ok(())
}
