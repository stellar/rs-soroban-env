use crate::{
    meta::{get_ledger_protocol_version, INTERFACE_VERSION},
    testutils::{generate_account_id, generate_bytes_array, wasm as wasm_util},
    xdr::{ScErrorCode, ScErrorType},
    AddressObject, Env, Host, HostError, LedgerInfo, Symbol, Val, WasmiMarshal,
};

#[test]
fn ledger_protocol_greater_than_env_protocol_should_fail() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let env_proto = get_ledger_protocol_version(INTERFACE_VERSION);

    // This test only makes sense if TEST_PROTOCOL is equal to the env version
    if env_proto != host.get_ledger_protocol_version()? {
        return Ok(());
    }

    let host = observe_host!(host);
    host.with_mut_ledger_info(|li| li.protocol_version = env_proto + 1)?;
    let wasm = wasm_util::wasm_module_calling_protocol_gated_host_fn(env_proto);
    let id = host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        generate_account_id(&host),
        generate_bytes_array(&host),
    );
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::Context, ScErrorCode::InternalError)
    ));
    Ok(())
}

#[test]
fn wasm_protocol_greater_than_ledger_protocol_should_fail() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let wasm =
        wasm_util::wasm_module_calling_protocol_gated_host_fn(host.get_ledger_protocol_version()?);
    host.with_mut_ledger_info(|li| li.protocol_version -= 1)?;
    let id = host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        generate_account_id(&host),
        generate_bytes_array(&host),
    );
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::WasmVm, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

fn configure_ledger_and_wasm_for_protocol_test(
    host: &Host,
    ledger_proto: u32,
    wasm_proto: u32,
) -> Result<AddressObject, HostError> {
    host.enable_debug()?;
    let mut li = LedgerInfo::default();
    li.protocol_version = ledger_proto;
    host.set_ledger_info(li)?;
    let wasm = wasm_util::wasm_module_calling_protocol_gated_host_fn(wasm_proto);
    host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        generate_account_id(&host),
        generate_bytes_array(&host),
    )
}

#[test]
fn test_host_protocol_gating_for_wasm() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    // 1. The contract calls a host function that is undefined for its protocol
    //    (18), but defined in a future protocol (19). The contract is
    //    **current** to the ledger protocol version. In this case a
    //    `Wasmi::LinkerError::MissingDefinition` i.e. `(WasmVm, InvalidAction)`
    //    should be returned.
    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //           ^ ledger
    //           ^ wasm
    //
    let id = configure_ledger_and_wasm_for_protocol_test(&host, 18, 18);
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));

    // 2. Same as 1, except the contract's version is **previous** to the
    //    ledger protocol version. This should behave exactly the same as 1.
    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                            ^ ledger
    //           ^ wasm
    //
    let id = configure_ledger_and_wasm_for_protocol_test(&host, 19, 18);
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));

    // 3. The contract is **current** to the ledger protocol version, both
    //    matches the hostfn supported version (19). In this case the call
    //    should be successful.
    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                            ^ ledger
    //                            ^ wasm
    //
    let id = configure_ledger_and_wasm_for_protocol_test(&host, 19, 19);
    assert!(id.is_ok());
    let res = host.call(
        id?,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(res.is_ok());

    // 4. The ledger protocol version has moved past the host function supported
    //    protocol version. In this case, a
    //    `Wasmi::LinkerError::MissingDefinition` i.e. `(WasmVm, InvalidAction)`
    //    should be returned.
    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                                            ^ ledger
    //                            ^ wasm
    //
    let id = configure_ledger_and_wasm_for_protocol_test(&host, 20, 19);
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));

    // 5. The contract is built at a more advanced protocol (20), and it
    //    includes call to a retired host function (valid at protocol 19). In
    //    this case, a `Wasmi::LinkerError::MissingDefinition` i.e. `(WasmVm,
    //    InvalidAction)` should be returned.
    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                                            ^ ledger
    //                                            ^ wasm
    //
    let id = configure_ledger_and_wasm_for_protocol_test(&host, 20, 20);
    assert!(HostError::result_matches_err(
        id,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));

    Ok(())
}

#[test]
fn test_native_mode_calling_protocol_gated_host_fn() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.enable_debug()?;
    let mut li = LedgerInfo::default();

    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //           ^ ledger
    //
    li.protocol_version = 18;
    host.set_ledger_info(li.clone())?;
    let res = <Host as Env>::protocol_gated_dummy(&host);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Context, ScErrorCode::IndexBounds)
    ));

    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                            ^ ledger
    //
    li.protocol_version = 19;
    host.set_ledger_info(li.clone())?;
    let res = <Host as Env>::protocol_gated_dummy(&host);
    assert!(res.is_ok());

    //
    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                                            ^ ledger
    //
    li.protocol_version = 20;
    host.set_ledger_info(li)?;
    let res = <Host as Env>::protocol_gated_dummy(&host);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Context, ScErrorCode::IndexBounds)
    ));

    Ok(())
}

fn configure_protocol_test_for_runtime_guardrail(
    host: &Host,
    ledger_proto: u32,
) -> Result<wasmi::Value, wasmi::Error> {
    host.enable_debug().unwrap();
    let mut li = LedgerInfo::default();
    li.protocol_version = ledger_proto;
    host.set_ledger_info(li).unwrap();
    let wasm = wasm_util::wasm_module_calling_protocol_gated_host_fn(ledger_proto);
    register_and_invoke_custom_vm_no_linker_check(host, wasm.as_slice())
}

fn register_and_invoke_custom_vm_no_linker_check(
    host: &Host,
    wasm_code: &[u8],
) -> Result<wasmi::Value, wasmi::Error> {
    use crate::vm::protocol_gated_dummy;
    use wasmi::{Engine, Func, Linker, Module, Store, Value};
    let mut config = wasmi::Config::default();
    config.consume_fuel(true);
    let engine = Engine::new(&config);
    let module = Module::new(&engine, wasm_code).unwrap();
    let mut store = Store::new(&engine, host.clone());
    store.add_fuel(10000000).unwrap();
    store.limiter(|host| host);
    let mut linker = <Linker<Host>>::new(&engine);

    let wrap = |store| Func::wrap(store, protocol_gated_dummy);
    let func = (wrap)(&mut store);
    linker.define("t", "0", func).unwrap();
    let not_started_instance = linker.instantiate(&mut store, &module).unwrap();
    let instance = not_started_instance.ensure_no_start(&mut store).unwrap();

    let ext = instance.get_export(&mut store, "test").unwrap();
    let func = ext.into_func().unwrap();
    let mut wasm_ret: [Value; 1] = [Value::I64(0)];
    let res = func
        .call(&mut store, &[], &mut wasm_ret)
        .map(|_| wasm_ret[0].clone());
    res
}

fn fish_host_error_from_wasm_trap(
    host: &Host,
    res: Result<wasmi::Value, wasmi::Error>,
) -> Result<Val, HostError> {
    res.map(|r| {
        host.relative_to_absolute(Val::try_marshal_from_value(r).unwrap())
            .unwrap()
    })
    .map_err(|e| match e {
        wasmi::Error::Trap(t) => t.downcast().unwrap(),
        _ => panic!(),
    })
}

// Tests the additional runtime protocol guardrail during host function
// dispatch. The runtime protocol gating are put in place as an additional
// safeguard, so these tests create a VM with link-time checks intentionally
// turned off.
#[test]
fn test_additional_protocol_guardrail_during_invocation() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //           ^ ledger
    let res = configure_protocol_test_for_runtime_guardrail(&host, 18);
    HostError::result_matches_err(
        fish_host_error_from_wasm_trap(&host, res),
        (ScErrorType::Context, ScErrorCode::IndexBounds),
    );

    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                            ^ ledger
    let res = configure_protocol_test_for_runtime_guardrail(&host, 19);
    assert!(res.is_ok());

    // ----------|----------------|----------------|-------------------->
    //           18               19               20(curr-env)
    //                            ^ hostfn
    //                                            ^ ledger
    let res = configure_protocol_test_for_runtime_guardrail(&host, 20);
    HostError::result_matches_err(
        fish_host_error_from_wasm_trap(&host, res),
        (ScErrorType::Context, ScErrorCode::IndexBounds),
    );
    Ok(())
}
