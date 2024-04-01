use crate::{
    budget::AsBudget,
    events::{
        Events, InternalContractEvent, InternalDiagnosticArg, InternalDiagnosticEvent,
        InternalEvent,
    },
    testutils::AsScVal,
    xdr::{
        ContractCostType, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0,
        ExtensionPoint, Hash, ScAddress, ScErrorCode, ScErrorType, ScMap, ScMapEntry, ScVal,
    },
    ContractFunctionSet, Env, Error, Host, HostError, Symbol, SymbolSmall, Val, VecObject,
};
use expect_test::expect;
use more_asserts::assert_le;
use soroban_env_common::EnvBase;
use std::rc::Rc;

pub struct ContractWithSingleEvent;

impl ContractFunctionSet for ContractWithSingleEvent {
    fn call(&self, _func: &Symbol, host: &Host, _args: &[Val]) -> Option<Val> {
        // Add a contract event
        let mut data = host.map_new().unwrap();
        data = host.map_put(data, 1_u32.into(), 2_u32.into()).unwrap();
        let mut topics = host.vec_new().unwrap();
        topics = host.vec_push_back(topics, 0u32.into()).unwrap();
        topics = host.vec_push_back(topics, 1u32.into()).unwrap();
        Some(host.contract_event(topics, data.to_val()).unwrap().into())
    }
}

#[test]
fn contract_event() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let dummy_id = [0; 32];
    let dummy_address = ScAddress::Contract(Hash(dummy_id));
    let id = host.add_host_object(dummy_address)?;
    let test_contract = Rc::new(ContractWithSingleEvent {});
    let sym = Symbol::try_from_small_str("add").unwrap();
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    host.register_test_contract(id, test_contract)?;
    assert_eq!(
        host.call(id, sym, args)?.get_payload(),
        Val::from_void().to_val().get_payload()
    );

    let event_ref = ContractEvent {
        ext: ExtensionPoint::V0,
        contract_id: Some(Hash(dummy_id)),
        type_: ContractEventType::Contract,
        body: ContractEventBody::V0(ContractEventV0 {
            topics: host.map_err(vec![ScVal::U32(0), ScVal::U32(1)].try_into())?,
            data: ScVal::Map(Some(host.map_err(ScMap::try_from(vec![ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            }]))?)),
        }),
    };

    // Fish out the last contract event and check that it is
    // the correct type.
    let events = host.get_events()?.0;
    match events.last() {
        Some(he) if he.event.type_ == ContractEventType::Contract => {
            assert_eq!(he.event, event_ref)
        }
        _ => panic!("missing contract event"),
    }
    Ok(())
}

pub struct ContractWithMultipleEvents;

impl ContractFunctionSet for ContractWithMultipleEvents {
    fn call(&self, _func: &Symbol, host: &Host, _args: &[Val]) -> Option<Val> {
        let topics = host.test_vec_obj(&[0, 1]).unwrap();
        let data = Val::from(0u32);
        host.record_contract_event(ContractEventType::Contract, topics, data)
            .unwrap();
        host.log_diagnostics("debug event 0", &[]);
        host.record_contract_event(ContractEventType::System, topics, data)
            .unwrap();
        Some(().into())
    }
}

#[test]
fn test_event_rollback() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let dummy_address = ScAddress::Contract(Hash([0; 32]));
    let id = host.add_host_object(dummy_address)?;
    let test_contract = Rc::new(ContractWithMultipleEvents {});
    let sym = Symbol::try_from_small_str("add").unwrap();
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    host.register_test_contract(id, test_contract)?;
    assert_eq!(
        host.call(id, sym, args)?.get_payload(),
        Val::from_void().to_val().get_payload()
    );
    host.try_borrow_events_mut()?.rollback(1)?;
    // run `UPDATE_EXPECT=true cargo test` to update this.
    let expected = expect!["[HostEvent { event: ContractEvent { ext: V0, contract_id: Some(Hash(0000000000000000000000000000000000000000000000000000000000000000)), type_: Contract, body: V0(ContractEventV0 { topics: VecM([I32(0), I32(1)]), data: U32(0) }) }, failed_call: false }, HostEvent { event: ContractEvent { ext: V0, contract_id: Some(Hash(0000000000000000000000000000000000000000000000000000000000000000)), type_: System, body: V0(ContractEventV0 { topics: VecM([I32(0), I32(1)]), data: U32(0) }) }, failed_call: true }]"];
    let actual = format!("{:?}", host.try_borrow_events()?.externalize(&host)?.0);
    expected.assert_eq(&actual);
    Ok(())
}

#[test]
fn test_internal_contract_events_metering_not_free() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_prng());
    let dummy_id = [0; 32];
    let ce = InternalContractEvent {
        type_: ContractEventType::Contract,
        contract_id: Some(host.test_bin_obj(&dummy_id)?),
        topics: host.test_vec_obj(&[0, 1, 2, 3])?,
        data: Val::from_void().to_val(),
    };

    let _ = host
        .clone()
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::MemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::MemCpy, 10, 0, 1, 0);

    let _ = host.with_events_mut(|events| Ok(events.record(InternalEvent::Contract(ce), &host)))?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 30);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 3);

    let _ = host.try_borrow_events()?.externalize(&host)?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 100);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 10);
    Ok(())
}

#[test]
fn test_internal_diagnostic_event_metering_free() -> Result<(), HostError> {
    let dummy_id = [0; 32];
    let contract_id = Some(Hash(dummy_id));
    let topics = vec![
        InternalDiagnosticArg::HostVal(SymbolSmall::try_from_str("error")?.to_val()),
        InternalDiagnosticArg::HostVal(Val::from_i32(0).to_val()),
    ];
    let args = vec![InternalDiagnosticArg::XdrVal(1_i32.as_scval())];
    let de = Rc::new(InternalDiagnosticEvent {
        contract_id,
        topics,
        args,
    });

    let host = observe_host!(Host::test_host_with_prng());
    let _ = host
        .clone()
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::MemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::MemCpy, 10, 0, 1, 0);

    // `DEBUG` mode is required for the host to collect diagnostic events
    host.enable_debug()?;

    let _ =
        host.with_events_mut(|events| Ok(events.record(InternalEvent::Diagnostic(de), &host)))?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 0);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 0);

    let _ = host.try_borrow_events()?.externalize(&host)?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 0);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 0);
    Ok(())
}

fn log_some_diagnostics(host: Host) -> Result<Events, HostError> {
    let args: Vec<_> = (0..1000).map(|u| Val::from_u32(u).to_val()).collect();
    let contract_id = Hash([0; 32]);
    host.log_diagnostics("logging some diagnostics", args.as_slice());
    host.error(
        Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError),
        "something internal went wrong",
        args.as_slice(),
    );
    host.fn_call_diagnostics(&contract_id, &Symbol::try_from_small_str("fn_call")?, &args);
    host.fn_return_diagnostics(
        &contract_id,
        &Symbol::try_from_small_str("fn_return")?,
        &Symbol::try_from_small_str("pass")?.into(),
    );
    let (_, evts) = host.try_finish()?;
    Ok(evts)
}

#[test]
fn test_diagnostic_events_do_not_affect_metering_with_debug_off() -> Result<(), HostError> {
    // DEBUG mode OFF

    // NB: We don't observe here since the test is sensitive to shadow budget.
    let host = Host::test_host_with_prng();
    let budget = host.as_budget().clone();
    budget.reset_default()?;
    let evts = log_some_diagnostics(host)?;
    assert_eq!(budget.get_cpu_insns_consumed()?, 0);
    assert_eq!(budget.get_mem_bytes_consumed()?, 0);
    assert_eq!(budget.get_shadow_cpu_insns_consumed()?, 0);
    assert_eq!(budget.get_shadow_mem_bytes_consumed()?, 0);
    assert_eq!(evts.0.len(), 0);
    Ok(())
}

#[test]
fn test_diagnostic_events_do_not_affect_metering_with_debug_on_and_sufficient_budget(
) -> Result<(), HostError> {
    // DEBUG mode ON, budget sufficient

    // NB: We don't observe here since the test is sensitive to shadow budget.
    let host = Host::test_host_with_prng();
    host.enable_debug()?;
    let budget = host.as_budget().clone();
    budget.reset_default()?;
    let evts = log_some_diagnostics(host)?;
    assert_eq!(budget.get_cpu_insns_consumed()?, 0);
    assert_eq!(budget.get_mem_bytes_consumed()?, 0);
    assert_ne!(budget.get_shadow_cpu_insns_consumed()?, 0);
    assert_ne!(budget.get_shadow_mem_bytes_consumed()?, 0);
    assert_eq!(evts.0.len(), 4);
    Ok(())
}

#[test]
fn test_diagnostic_events_do_not_affect_metering_with_debug_on_and_insufficient_budget(
) -> Result<(), HostError> {
    // DEBUG mode ON, budget insufficient

    // NB: We don't observe here since the test is sensitive to shadow budget.
    let host = Host::test_host_with_prng();
    host.enable_debug()?;
    let budget = host.as_budget().clone();
    budget.reset_default()?;
    host.set_shadow_budget_limits(100000, 100000)?;
    let evts = log_some_diagnostics(host)?;
    assert_eq!(budget.get_cpu_insns_consumed()?, 0);
    assert_eq!(budget.get_mem_bytes_consumed()?, 0);
    assert_ne!(budget.get_shadow_cpu_insns_consumed()?, 0);
    assert_ne!(budget.get_shadow_mem_bytes_consumed()?, 0);
    assert_eq!(evts.0.len(), 0);
    Ok(())
}

#[test]
#[cfg(all(not(feature = "next"), feature = "testutils"))]
// This is a regression test: we accidentally wired up the tracing
// infrastructure in such a way that when it did a try_borrow on host fields it
// wanted to observe, it called the helpers that emit "internal error"
// diagnostic events for any failed borrows. We actually don't want that to
// happen, we want failed borrows from the tracing subsystem to be silent.
fn test_observation_does_not_emit_diagnostic_events_from_failed_borrows() -> Result<(), HostError> {
    let host = Host::test_host_with_prng();
    let obs_host = observe_host!(host.clone());
    host.enable_debug()?;
    let storage = host.try_borrow_storage_mut()?;
    host.obj_from_i64(1)?;
    drop(storage);
    drop(obs_host);
    let (_, evts) = host.try_finish()?;
    dbg!(&evts);
    assert_eq!(evts.0.len(), 0);
    Ok(())
}

#[test]
fn nonexistent_topic_obj_handle() -> Result<(), HostError> {
    let host = Host::test_host_with_prng();
    let data = Val::from_void().to_val();
    let topics = unsafe { VecObject::from_handle(123) }; // non-existent handle
    let res = host.contract_event(topics, data);
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Value, ScErrorCode::InvalidInput)
    ));
    Ok(())
}

#[test]
fn too_many_event_topics() -> Result<(), HostError> {
    let topics: Vec<_> = (0..0x00FFFFFF).map(|u| Val::from_u32(u).to_val()).collect();
    // We don't observe this test: it makes way too big a trace.
    let host = Host::test_host_with_prng();
    let budget = host.as_budget().clone();
    budget.reset_unlimited()?;
    let topics = host.vec_new_from_slice(topics.as_slice())?;
    // if the topics manages to pass through the host-val machineary and VecObject is
    // created, recording the contract event a cheap operation.
    budget.reset_default()?;
    for i in 0..100 {
        host.contract_event(topics.clone(), Val::from_u32(i).to_val())?;
    }
    assert_le!(budget.get_cpu_insns_consumed()?, 200_000);
    assert_le!(budget.get_mem_bytes_consumed()?, 10000);

    // once trying to externalize, the limit will be exceeded
    let res = host.try_finish();
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn too_big_event_topic() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_prng());
    let budget = host.as_budget().clone();
    budget.reset_unlimited()?;
    let bytes = host.bytes_new_from_slice(&[0; 0x0FFFFFFF])?;
    let topics = host.vec_new_from_slice(&[bytes.to_val()])?;
    budget.reset_default()?;
    host.contract_event(topics, Val::from_u32(0).to_val())?;
    assert_le!(budget.get_cpu_insns_consumed()?, 5000);
    assert_le!(budget.get_mem_bytes_consumed()?, 500);

    let host = (*host).clone();
    let res = host.try_finish();
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}
#[test]
fn too_big_event_data() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_prng());
    let budget = host.as_budget().clone();
    budget.reset_unlimited()?;
    let bytes = host.bytes_new_from_slice(&[0; 0x0FFFFFFF])?;
    let topics = host.vec_new_from_slice(&[Val::from_u32(0).to_val()])?;
    budget.reset_default()?;
    host.contract_event(topics, bytes.to_val())?;
    assert_le!(budget.get_cpu_insns_consumed()?, 5000);
    assert_le!(budget.get_mem_bytes_consumed()?, 500);

    let host = (*host).clone();
    let res = host.try_finish();
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn too_many_events_in_loop() -> Result<(), HostError> {
    // We don't observe this test: it makes way too big a trace.
    let host = Host::test_host_with_prng();
    let budget = host.as_budget().clone();
    budget.reset_unlimited()?;
    let topics = host.vec_new_from_slice(&[Val::from_u32(0).to_val()])?;
    budget.reset_default()?;
    let mut i = 0;
    loop {
        if let Err(e) = host.contract_event(topics, Val::from_u32(i).to_val()) {
            assert_eq!(
                e.error,
                Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit)
            );
            break;
        }
        i += 1;
    }
    Ok(())
}
