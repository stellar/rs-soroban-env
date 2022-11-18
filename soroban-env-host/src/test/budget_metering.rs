use crate::{
    budget::{Budget, CostType},
    xdr::{ScMap, ScMapEntry, ScObject, ScVal, ScVmErrorCode},
    CheckedEnv, Host, HostError, RawVal, Symbol,
};
use soroban_test_wasms::VEC;

#[test]
fn xdr_object_conversion() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100_000, 100_000)
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

    host.with_budget(|budget| {
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
        .test_budget(100_000, 100_000)
        .enable_model(CostType::InvokeVmFunction)
        .enable_model(CostType::InvokeHostFunction);

    let obj = host.test_bin_obj(&dummy_id)?;
    // `vec_err` is a test contract function which calls `vec_new` (1 call)
    // and `vec_put` (1 call) so total input of 2 to the budget from `CostType::InvokeHostFunction`.
    let sym = Symbol::from_str("vec_err");
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(obj.to_object(), sym.into(), args.clone().into())?;
    host.with_budget(|budget| {
        assert_eq!(budget.get_input(CostType::InvokeVmFunction), 1);
        assert_eq!(budget.get_input(CostType::InvokeHostFunction), 2);
        assert_eq!(budget.get_cpu_insns_count(), 30);
        assert_eq!(budget.get_mem_bytes_count(), 3);
    });

    Ok(())
}

#[test]
fn metered_xdr() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100_000, 100_000)
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
    host.with_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValSer), w.len() as u64);
    });

    host.metered_from_xdr::<ScMap>(w.as_slice())?;
    host.with_budget(|budget| {
        assert_eq!(budget.get_input(CostType::ValDeser), w.len() as u64);
    });
    Ok(())
}

#[test]
fn metered_xdr_out_of_budget() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(10, 10)
        .enable_model(CostType::ValSer);
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
    let res = host.metered_write_xdr(&scmap, &mut w);
    let code = ScVmErrorCode::TrapCpuLimitExceeded;
    assert!(HostError::result_matches_err_status(res, code));
    Ok(())
}

#[test]
fn map_insert_key_vec_obj() -> Result<(), HostError> {
    let mut host = Host::test_host().test_budget(1000, 1000);
    let mut m = host.map_new()?;
    let k0 = host.test_vec_obj(&[2, 3])?;
    let v0: RawVal = 6_u32.into();
    let k1 = host.test_vec_obj(&[5, 6, 7])?;
    let v1: RawVal = 8_u32.into();
    m = host.map_put(m, k0.into(), v0)?;

    // now we enable various cost models
    host = host
        .enable_model(CostType::VisitObject)
        .enable_model(CostType::ImVecCmp)
        .enable_model(CostType::ImMapMutEntry);
    host.map_put(m, k1.into(), v1)?;

    host.with_budget(|budget| {
        // 4 = 1 visit map + 1 visit k1 + (obj_comp which needs to) 1 visit both k0 and k1
        assert_eq!(budget.get_input(CostType::VisitObject), 4);
        assert_eq!(budget.get_input(CostType::ImVecCmp), 2); // the shorter vec len
        assert_eq!(budget.get_input(CostType::ImMapMutEntry), 1); // len of the map before put
    });

    Ok(())
}
