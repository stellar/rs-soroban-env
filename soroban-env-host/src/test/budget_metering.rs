use crate::{
    budget::AsBudget,
    host::metered_clone::MeteredClone,
    xdr::{ContractCostType, ScMap, ScMapEntry, ScVal},
    Env, Host, HostError, Symbol, Val,
};
use expect_test::{self, expect};
use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_test_wasms::VEC;

#[test]
fn xdr_object_conversion() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100_000, 100_000)
        .enable_model(ContractCostType::ValXdrConv, 10, 0, 1, 0);
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
        assert_eq!(budget.get_tracker(ContractCostType::ValXdrConv).0, 6);
        assert_eq!(budget.get_cpu_insns_consumed(), 60);
        assert_eq!(budget.get_mem_bytes_consumed(), 6);
        Ok(())
    })?;
    Ok(())
}

#[test]
fn vm_hostfn_invocation() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id_obj = host.register_test_contract_wasm(VEC);
    let host = host
        .test_budget(100_000, 100_000)
        .enable_model(ContractCostType::InvokeVmFunction, 10, 0, 1, 0)
        .enable_model(ContractCostType::InvokeHostFunction, 10, 0, 1, 0);

    // `vec_err` is a test contract function which calls `vec_new` (1 call)
    // and `vec_put` (1 call) so total input of 2 to the budget from `CostType::InvokeHostFunction`.
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(id_obj, sym, args)?;
    host.with_budget(|budget| {
        assert_eq!(budget.get_tracker(ContractCostType::InvokeVmFunction).0, 1);
        assert_eq!(
            budget.get_tracker(ContractCostType::InvokeHostFunction).0,
            2
        );
        assert_eq!(budget.get_cpu_insns_consumed(), 30);
        assert_eq!(budget.get_mem_bytes_consumed(), 3);
        Ok(())
    })?;

    Ok(())
}

#[test]
fn metered_xdr() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100_000, 100_000)
        .enable_model(ContractCostType::ValSer, 0, 10, 0, 1)
        .enable_model(ContractCostType::ValDeser, 0, 10, 0, 1);
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
        assert_eq!(
            budget.get_tracker(ContractCostType::ValSer).1,
            Some(w.len() as u64)
        );
        Ok(())
    })?;

    host.metered_from_xdr::<ScMap>(w.as_slice())?;
    host.with_budget(|budget| {
        assert_eq!(
            budget.get_tracker(ContractCostType::ValDeser).1,
            Some(w.len() as u64)
        );
        Ok(())
    })?;
    Ok(())
}

#[test]
fn metered_xdr_out_of_budget() -> Result<(), HostError> {
    let host =
        Host::test_host()
            .test_budget(10, 10)
            .enable_model(ContractCostType::ValSer, 0, 10, 0, 1);
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
    let code = (ScErrorType::Budget, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn map_insert_key_vec_obj() -> Result<(), HostError> {
    let mut host = Host::test_host().test_budget(1000, 1000);
    let mut m = host.map_new()?;
    let k0 = host.test_vec_obj(&[2, 3])?;
    let v0: Val = 6_u32.into();
    let k1 = host.test_vec_obj(&[5, 6, 7])?;
    let v1: Val = 8_u32.into();
    m = host.map_put(m, k0.into(), v0)?;

    // now we enable various cost models
    host = host
        .enable_model(ContractCostType::VisitObject, 10, 0, 1, 0)
        .enable_model(ContractCostType::MapEntry, 10, 0, 1, 0);
    host.map_put(m, k1.into(), v1)?;

    host.with_budget(|budget| {
        // 4 = 1 visit map + 1 visit k1 + (obj_comp which needs to) 1 visit both k0 and k1
        assert_eq!(budget.get_tracker(ContractCostType::VisitObject).0, 4);
        // upper bound of number of map-accesses, counting both binary-search and point-access.
        assert_eq!(budget.get_tracker(ContractCostType::MapEntry).0, 5);
        Ok(())
    })?;

    Ok(())
}

#[test]
fn test_recursive_type_clone() -> Result<(), HostError> {
    let host = Host::test_host()
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::HostMemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::HostMemCpy, 10, 0, 1, 0);
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
            .get_tracker(ContractCostType::HostMemAlloc)
            .1
            .unwrap()
            .to_string()
            .as_str(),
    );
    // 600 = 576 + 24 is correct because we need to copy all the memory allocated, as well as the
    // memory layout of the top level type (Vec).
    expect!["600"].assert_eq(
        host.as_budget()
            .get_tracker(ContractCostType::HostMemCpy)
            .1
            .unwrap()
            .to_string()
            .as_str(),
    );
    Ok(())
}

// This test is a sanity check to make sure we didn't accidentally change the cost schedule.
// If the cost schedule have changed, need to update this test by running
// `UPDATE_EXPECT=true cargo test`
#[test]
fn total_amount_charged_from_random_inputs() -> Result<(), HostError> {
    let host = Host::default();

    let tracker: Vec<(u64, Option<u64>)> = vec![
        (246, None),
        (184, None),
        (1, Some(152)),
        (1, Some(65)),
        (1, Some(74)),
        (176, None),
        (97, None),
        (148, None),
        (1, Some(49)),
        (1, Some(103)),
        (1, Some(193)),
        (226, None),
        (250, None),
        (186, None),
        (152, None),
        (1, Some(227)),
        (1, Some(69)),
        (1, Some(160)),
        (1, Some(147)),
        (47, None),
        (263, None),
        (1, Some(1)),
        (1, None),
        (1, None),
        (1, None),
    ];

    for ty in ContractCostType::variants() {
        host.with_budget(|b| b.batched_charge(ty, tracker[ty as usize].0, tracker[ty as usize].1))?;
    }
    let actual = format!("{:?}", host.as_budget());
    expect![[r#"
        =====================================================================================================================================================================
        Cpu limit: 40000000; used: 10344986
        Mem limit: 52428800; used: 219205
        =====================================================================================================================================================================
        CostType                 iterations     input          cpu_insns      mem_bytes      const_term_cpu      lin_term_cpu        const_term_mem      lin_term_mem        
        WasmInsnExec             246            None           1722           0              7                   0                   0                   0                   
        WasmMemAlloc             184            None           0              184            0                   0                   1                   0                   
        HostMemAlloc             1              Some(152)      2350           160            2350                0                   8                   1                   
        HostMemCpy               1              Some(65)       23             0              23                  0                   0                   0                   
        HostMemCmp               1              Some(74)       117            0              43                  1                   0                   0                   
        InvokeHostFunction       176            None           163328         0              928                 0                   0                   0                   
        VisitObject              97             None           1843           0              19                  0                   0                   0                   
        ValXdrConv               148            None           19832          0              134                 0                   0                   0                   
        ValSer                   1              Some(49)       636            156            587                 1                   9                   3                   
        ValDeser                 1              Some(103)      870            107            870                 0                   4                   1                   
        ComputeSha256Hash        1              Some(193)      8094           40             1725                33                  40                  0                   
        ComputeEd25519PubKey     226            None           5774526        0              25551               0                   0                   0                   
        MapEntry                 250            None           13250          0              53                  0                   0                   0                   
        VecEntry                 186            None           930            0              5                   0                   0                   0                   
        GuardFrame               152            None           615600         71744          4050                0                   472                 0                   
        VerifyEd25519Sig         1              Some(227)      374401         0              369634              21                  0                   0                   
        VmMemRead                1              Some(69)       0              0              0                   0                   0                   0                   
        VmMemWrite               1              Some(160)      124            0              124                 0                   0                   0                   
        VmInstantiation          1              Some(147)      671595         123751         600447              484                 117871              40                  
        InvokeVmFunction         47             None           278522         22842          5926                0                   486                 0                   
        ChargeBudget             288            None           37440          0              130                 0                   0                   0                   
        ComputeKeccak256Hash     1              Some(1)        3368           40             3322                46                  40                  0                   
        ComputeEcdsaSecp256k1Key 1              None           56525          0              56525               0                   0                   0                   
        ComputeEcdsaSecp256k1Sig 1              None           250            0              250                 0                   0                   0                   
        RecoverEcdsaSecp256k1Key 1              None           2319640        181            2319640             0                   181                 0                   
        =====================================================================================================================================================================

    "#]]
    .assert_eq(&actual);
    Ok(())
}
