use crate::{
    budget::CostType,
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
    let storage = Host::test_storage_with_contracts(vec![dummy_id.into()], vec![VEC]);
    let host = Host::with_storage(storage)
        .test_budget()
        .enable_model(CostType::HostFunction);

    let obj = host.test_bin_obj(&dummy_id)?;
    // `vec_err` is a test contract function which calls `vec_new` (1 param)
    // and `vec_put` (3 params) so total 4 inputs to the budget from `CostType::HostFunction`.
    let sym = Symbol::from_str("vec_err");
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    host.get_budget(|budget| {
        assert_eq!(budget.get_input(CostType::HostFunction), 4);
        assert_eq!(budget.get_cpu_insns_count(), 40);
        assert_eq!(budget.get_mem_bytes_count(), 4);
    });

    Ok(())
}
