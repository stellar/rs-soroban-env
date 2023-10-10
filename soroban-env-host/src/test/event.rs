use crate::{
    budget::AsBudget,
    events::{
        InternalContractEvent, InternalDiagnosticArg, InternalDiagnosticEvent, InternalEvent,
    },
    test::util::AsScVal,
    xdr::{
        ContractCostType, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0,
        ExtensionPoint, Hash, ScAddress, ScMap, ScMapEntry, ScVal,
    },
    ContractFunctionSet, Env, Host, HostError, Symbol, SymbolSmall, Val,
};
use expect_test::expect;
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
    let host = Host::test_host_with_recording_footprint();
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
        host.log_diagnostics("debug event 0", &[]).unwrap();
        host.record_contract_event(ContractEventType::System, topics, data)
            .unwrap();
        Some(().into())
    }
}

#[test]
fn test_event_rollback() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
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
    let host = Host::test_host();
    let dummy_id = [0; 32];
    let ce = InternalContractEvent {
        type_: ContractEventType::Contract,
        contract_id: Some(host.test_bin_obj(&dummy_id)?),
        topics: host.test_vec_obj(&[0, 1, 2, 3])?,
        data: Val::from_void().to_val(),
    };

    let host = host
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::MemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::MemCpy, 10, 0, 1, 0);

    let _ = host.with_events_mut(|events| {
        Ok(events.record(InternalEvent::Contract(ce), host.as_budget()))
    })?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 30);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 3);

    let _ = host.try_borrow_events()?.externalize(&host)?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 100);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 10);
    Ok(())
}

#[test]
fn test_internal_diagnostic_event_metering_free() -> Result<(), HostError> {
    let host = Host::test_host();
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

    let host = host
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::MemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::MemCpy, 10, 0, 1, 0);

    let _ = host.with_events_mut(|events| {
        Ok(events.record(InternalEvent::Diagnostic(de), host.as_budget()))
    })?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 0);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 0);

    let _ = host.try_borrow_events()?.externalize(&host)?;
    assert_eq!(host.as_budget().get_cpu_insns_consumed()?, 0);
    assert_eq!(host.as_budget().get_mem_bytes_consumed()?, 0);
    Ok(())
}
