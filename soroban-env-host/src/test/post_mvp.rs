use crate::{testutils::wasm, Env, EnvBase, Host, HostError, Symbol, TryFromVal};

#[test]
fn test_enabled_post_mvp_features() -> Result<(), HostError> {
    let wasm = wasm::post_mvp_wasm_module();
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
