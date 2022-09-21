use crate::{
    events::HostEvent,
    xdr::{
        ContractEvent, ContractEventBody, ContractEventType, ContractEventV0, ExtensionPoint, Hash,
        ScMap, ScMapEntry, ScObject::Map, ScVal,
    },
    ContractFunctionSet, Env, EnvBase, Host, HostError, RawVal, Symbol,
};
use std::rc::Rc;

pub struct EmptyContractWithEvents;

impl ContractFunctionSet for EmptyContractWithEvents {
    fn call(&self, _func: &Symbol, host: &Host, _args: &[RawVal]) -> Option<RawVal> {
        // Add a contract event
        let mut data = host.map_new();
        data = host.map_put(data, 1_u32.into(), 2_u32.into());
        let mut topics = host.vec_new(().into());
        topics = host.vec_push_back(topics, 0u32.into());
        topics = host.vec_push_back(topics, 1u32.into());
        Some(host.contract_event(topics, data.to_raw()))
    }
}

#[test]
fn contract_event() -> Result<(), HostError> {
    let host = Host::default();
    let dummy_id = [0; 32];
    let id = host.bytes_new_from_slice(&dummy_id)?;
    let test_contract = Rc::new(EmptyContractWithEvents {});
    let sym = Symbol::from_str("add");
    let args = host.test_vec_obj::<i32>(&[1, 2])?;
    host.register_test_contract(id, test_contract)?;
    assert_eq!(
        host.call(id, sym.into(), args.into()).get_payload(),
        RawVal::from_void().get_payload()
    );

    let event_ref = ContractEvent {
        ext: ExtensionPoint::V0,
        contract_id: Some(Hash(dummy_id)),
        type_: ContractEventType::Contract,
        body: ContractEventBody::V0(ContractEventV0 {
            topics: host.map_err(vec![ScVal::U32(0), ScVal::U32(1)].try_into())?,
            data: ScVal::Object(Some(Map(host.map_err(ScMap::try_from(vec![
                ScMapEntry {
                    key: ScVal::U32(1),
                    val: ScVal::U32(2),
                },
            ]))?))),
        }),
    };

    // Fish out the last contract event and check that it is
    // correct, and formats as expected.
    let events = host.get_events()?;
    match events.0.last() {
        Some(HostEvent::Contract(ce)) => {
            assert_eq!(*ce, event_ref)
        }
        _ => {
            panic!("missing contract event")
        }
    };
    Ok(())
}
