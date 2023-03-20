use crate::{
    budget::{AsBudget, CostType},
    host::metered_clone::MeteredClone,
    xdr::{ScMap, ScMapEntry, ScVal, ScVmErrorCode},
    Env, Host, HostError, RawVal, Symbol,
};
use expect_test::{self, expect};
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
    host.to_host_val(&ScVal::Map(Some(scmap)))?;

    host.with_budget(|budget| {
        // NB: It might seem like this should be 5 rather han 6
        // but due to the fact that one can convert an "object" or
        // a "value" on separate paths that both need metering,
        // we wind up double-counting the conversion of "objects".
        // Possibly this should be improved in the future.
        assert_eq!(budget.get_input(CostType::ValXdrConv), 6);
        assert_eq!(budget.get_cpu_insns_count(), 60);
        assert_eq!(budget.get_mem_bytes_count(), 6);
    });
    Ok(())
}

#[test]
fn vm_hostfn_invocation() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id_obj = host.register_test_contract_wasm(VEC)?;
    let host = host
        .test_budget(100_000, 100_000)
        .enable_model(CostType::InvokeVmFunction)
        .enable_model(CostType::InvokeHostFunction);

    // `vec_err` is a test contract function which calls `vec_new` (1 call)
    // and `vec_put` (1 call) so total input of 2 to the budget from `CostType::InvokeHostFunction`.
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(id_obj, sym, args.into())?;
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
        .enable_model(CostType::MapEntry);
    host.map_put(m, k1.into(), v1)?;

    host.with_budget(|budget| {
        // 4 = 1 visit map + 1 visit k1 + (obj_comp which needs to) 1 visit both k0 and k1
        assert_eq!(budget.get_input(CostType::VisitObject), 4);
        // upper bound of number of map-accesses, counting both binary-search and point-access.
        assert_eq!(budget.get_input(CostType::MapEntry), 5);
    });

    Ok(())
}

#[test]
fn test_recursive_type_clone() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100_000, 100_000)
        .enable_model(CostType::HostMemAlloc)
        .enable_model(CostType::HostMemCpy);
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
    let v: Vec<Box<ScMap>> = vec![
        Box::new(scmap.clone()),
        Box::new(scmap.clone()),
        Box::new(scmap),
    ];

    v.metered_clone(host.as_budget())?;

    //*********************************************************************************************************************************************/
    /* Type(size, count) | Vec(24,1) ---> Box(8,3) ----> ScMap(24,3) --> Vec(24,3) ----> ScMapEntry(80,6) --> ScVal(40, 12) --> U32(4, 12)        */
    /* MemAlloc          |            8x3      +    24x3              +             80x6                                                    = 576 */
    /* MemCpy            |  24    +   8x3      +    24x3              +             80x6                                                    = 600 */
    //*********************************************************************************************************************************************/
    expect!["576"].assert_eq(
        host.as_budget()
            .get_input(CostType::HostMemAlloc)
            .to_string()
            .as_str(),
    );
    // 600 = 576 + 24 is correct because we need to copy all the memory allocated, as well as the
    // memory layout of the top level type (Vec).
    expect!["600"].assert_eq(
        host.as_budget()
            .get_input(CostType::HostMemCpy)
            .to_string()
            .as_str(),
    );
    Ok(())
}
