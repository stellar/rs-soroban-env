use crate::{
    budget::{Budget, CostType},
    xdr::{ScMap, ScMapEntry, ScObject, ScVal},
    CheckedEnv, Host, HostError, Symbol,
};
use soroban_test_wasms::VEC;

#[test]
fn xdr_object_conversion() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget()
        .enable_model(CostType::ValXdrConv);
    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(4),
            },
        ]
        .try_into(),
    )?;
    let scobj = ScObject::Map(scmap);
    host.to_host_obj(&scobj)?;

    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValXdrConv), 5);
        assert_eq!(budget.get_cpu_insns_count(), 50);
        assert_eq!(budget.get_mem_bytes_count(), 5);
    });
    Ok(())
}

#[test]
fn vm_hostfn_invocation() -> Result<(), HostError> {
    let dummy_id = [0; 32];
    let budget = Budget::default();
    let storage =
        Host::test_storage_with_contracts(vec![dummy_id.into()], vec![VEC], budget.clone());
    let host = Host::with_storage_and_budget(storage, budget)
        .test_budget()
        .enable_model(CostType::HostFunction);

    let obj = host.test_bin_obj(&dummy_id)?;
    // `vec_err` is a test contract function which calls `vec_new` (1 call)
    // and `vec_put` (1 call) so total input of 2 to the budget from `CostType::HostFunction`.
    let sym = Symbol::from_str("vec_err");
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::HostFunction), 2);
        assert_eq!(budget.get_cpu_insns_count(), 20);
        assert_eq!(budget.get_mem_bytes_count(), 2);
    });

    Ok(())
}

#[test]
fn metered_xdr() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget()
        .enable_model(CostType::ValSer)
        .enable_model(CostType::ValDeser);
    let scmap: ScMap = host.map_err(
        vec![
            ScMapEntry {
                key: ScVal::U32(1),
                val: ScVal::U32(2),
            },
            ScMapEntry {
                key: ScVal::U32(2),
                val: ScVal::U32(4),
            },
        ]
        .try_into(),
    )?;
    let mut w = Vec::<u8>::new();
    host.metered_write_xdr(&scmap, &mut w)?;
    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValSer), w.len() as u64);
    });

    host.metered_from_xdr::<ScMap>(w.as_slice())?;
    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValDeser), w.len() as u64);
    });
    Ok(())
}
