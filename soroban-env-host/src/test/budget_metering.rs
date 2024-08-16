use crate::{
    budget::{AsBudget, Budget},
    host::metered_clone::{MeteredClone, MeteredIterator},
    host::metered_xdr::metered_write_xdr,
    xdr::{ContractCostType, ScMap, ScMapEntry, ScVal},
    Env, Host, HostError, Symbol, Val,
};
use expect_test::{self, expect};
use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_test_wasms::VEC;

#[test]
fn xdr_object_conversion() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host_with_prng());
    let _ = host.clone().test_budget(100_000, 100_000).enable_model(
        ContractCostType::MemCpy,
        1,
        0,
        1,
        0,
    );
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
        // 3 iterations:
        // - 1 for the vec cpy
        // - 1 for the bulk bytes cpy
        // - 1 for Vec -> MeteredOrdMap element scan
        assert_eq!(budget.get_tracker(ContractCostType::MemCpy)?.iterations, 3);
        // 120 bytes in total:
        // - 72 bytes copied for the ScVal->Val conversion: 24 (Vec bytes) + 2 (map entries) x (8 (padding bytes) + 8 (key bytes) + 8 (val bytes))
        // - 48 bytes for element scan: 2 elements * 24 bytes per element (8 padding bytes + 8 key bytes + 8 val bytes)
        assert_eq!(
            budget.get_tracker(ContractCostType::MemCpy)?.inputs,
            Some(120)
        );
        Ok(())
    })?;
    Ok(())
}

#[test]
fn vm_hostfn_invocation() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id_obj = host.register_test_contract_wasm(VEC);
    // this contract requests initial pages = 16 worth of linear memory, not sure why
    let _ = host
        .clone()
        .test_budget(100_000, 1_048_576)
        .enable_model(ContractCostType::InvokeVmFunction, 10, 0, 1, 0)
        .enable_model(ContractCostType::DispatchHostFunction, 10, 0, 1, 0);

    // `vec_err` is a test contract function which calls `vec_new` (1 call)
    // and `vec_put` (1 call) so total input of 2 to the budget from `CostType::DispatchHostFunction`.
    let sym = Symbol::try_from_small_str("vec_err").unwrap();
    let args = host.test_vec_obj::<u32>(&[1])?;

    // try_call
    host.try_call(id_obj, sym, args)?;
    host.with_budget(|budget| {
        assert_eq!(
            budget
                .get_tracker(ContractCostType::InvokeVmFunction)?
                .iterations,
            1
        );
        assert_eq!(
            budget
                .get_tracker(ContractCostType::DispatchHostFunction)?
                .iterations,
            2
        );
        assert_eq!(budget.get_cpu_insns_consumed()?, 30);
        assert_eq!(budget.get_mem_bytes_consumed()?, 3);
        Ok(())
    })?;

    Ok(())
}

#[test]
fn test_vm_fuel_metering() -> Result<(), HostError> {
    use crate::testutils::wasm::wasm_module_with_4n_insns;
    let host = Host::test_host_with_recording_footprint();
    let id_obj = host.register_test_contract_wasm(&wasm_module_with_4n_insns(1000));
    let sym = Symbol::try_from_small_str("test").unwrap();
    let args = host.test_vec_obj::<u32>(&[4375])?;
    let budget_err = (ScErrorType::Budget, ScErrorCode::ExceededLimit);

    // successful call with sufficient budget
    let host = host
        .test_budget(100_000, 1_048_576)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    host.call(id_obj, sym, args)?;
    let (cpu_count, cpu_consumed, mem_consumed, wasm_mem_alloc) = host.with_budget(|budget| {
        Ok((
            budget
                .get_tracker(ContractCostType::WasmInsnExec)?
                .iterations,
            budget.get_cpu_insns_consumed()?,
            budget.get_mem_bytes_consumed()?,
            budget.get_wasm_mem_alloc()?,
        ))
    })?;
    assert_eq!(
        (cpu_count, cpu_consumed, wasm_mem_alloc, mem_consumed),
        (1999, 11994, 65536, 73718)
    );

    // giving it the exact required amount will succeed
    let (cpu_required, mem_required) = (cpu_consumed, mem_consumed);
    let host = host
        .test_budget(cpu_required, mem_required)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    host.clear_module_cache()?;
    host.call(id_obj, sym, args)?;
    host.with_budget(|budget| {
        assert_eq!(budget.get_cpu_insns_consumed()?, cpu_required);
        assert_eq!(budget.get_wasm_mem_alloc()?, wasm_mem_alloc);
        Ok(())
    })?;

    // give it one less cpu results in failure with no cpu consumption but full mem consumption
    let (cpu_required, mem_required) = (cpu_consumed - 1, mem_consumed);
    let host = host
        .test_budget(cpu_required, mem_required)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    host.clear_module_cache()?;
    let res = host.try_call(id_obj, sym, args);
    assert!(HostError::result_matches_err(res, budget_err));
    host.with_budget(|budget| {
        assert_eq!(budget.get_cpu_insns_consumed()?, 0);
        assert_eq!(budget.get_wasm_mem_alloc()?, wasm_mem_alloc);
        Ok(())
    })?;

    // give it less than 1 page of memory in failure with no cpu consumption or mem consumption
    let (cpu_required, mem_required) = (cpu_consumed, 65535);
    let host = host
        .test_budget(cpu_required, mem_required)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    host.clear_module_cache()?;
    let res = host.try_call(id_obj, sym, args);
    assert!(HostError::result_matches_err(res, budget_err));
    host.with_budget(|budget| {
        assert_eq!(budget.get_cpu_insns_consumed()?, 0);
        assert_eq!(budget.get_wasm_mem_alloc()?, 0);
        Ok(())
    })?;

    Ok(())
}

#[test]
fn metered_xdr() -> Result<(), HostError> {
    let host = Host::test_host_with_prng()
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
    metered_write_xdr(host.budget_ref(), &scmap, &mut w)?;
    host.with_budget(|budget| {
        assert_eq!(
            budget.get_tracker(ContractCostType::ValSer)?.inputs,
            Some(w.len() as u64)
        );
        Ok(())
    })?;

    host.metered_from_xdr::<ScMap>(w.as_slice())?;
    host.with_budget(|budget| {
        assert_eq!(
            budget.get_tracker(ContractCostType::ValDeser)?.inputs,
            Some(w.len() as u64)
        );
        Ok(())
    })?;
    Ok(())
}

#[test]
fn metered_xdr_out_of_budget() -> Result<(), HostError> {
    let host = Host::test_host_with_prng()
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
    let res = metered_write_xdr(host.budget_ref(), &scmap, &mut w);
    let code = (ScErrorType::Budget, ScErrorCode::ExceededLimit);
    assert!(HostError::result_matches_err(res, code));
    Ok(())
}

#[test]
fn map_insert_key_vec_obj() -> Result<(), HostError> {
    let mut host = Host::test_host_with_prng().test_budget(1000, 1000);
    let mut m = host.map_new()?;
    let k0 = host.test_vec_obj(&[2, 3])?;
    let v0: Val = 6_u32.into();
    let k1 = host.test_vec_obj(&[5, 6, 7])?;
    let v1: Val = 8_u32.into();
    m = host.map_put(m, k0.into(), v0)?;

    // now we enable various cost models
    host = host.enable_model(ContractCostType::VisitObject, 10, 0, 1, 0);
    host.map_put(m, k1.into(), v1)?;

    host.with_budget(|budget| {
        // 17 visit-objs =
        //    1 to ensure value integrity of key for first map-put
        //  + 1 to get map to do first map-put
        //  + 1 to ensure value integrity of key for second map-put
        //  + 1 to get map to do second map-put
        //  + 2 to check integrity of k0 and k1 for obj_cmp during lookup
        //  + 2 for actually doing comparison in obj_cmp
        //  + 4 more to do same 2+2 visits when validating order of new map
        //  + 5 lookups on objects returned from 5 host fn calls to check their integrity
        // = 17
        assert_eq!(
            budget
                .get_tracker(ContractCostType::VisitObject)?
                .iterations,
            17
        );
        // upper bound of number of map-accesses, counting both binary-search, point-access and validate-scan.
        Ok(())
    })?;

    Ok(())
}

#[test]
fn test_recursive_type_clone() -> Result<(), HostError> {
    let host = Host::test_host_with_prng()
        .test_budget(100000, 100000)
        .enable_model(ContractCostType::MemAlloc, 10, 0, 1, 0)
        .enable_model(ContractCostType::MemCpy, 10, 0, 1, 0);
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
    /* Type(size, count) | Vec(24,1) ---> Box(8,3) ----> ScMap(24,3) --> Vec(24,3) ----> ScMapEntry(128,6) --> ScVal(64, 12) --> U32(4, 12)        */
    /* MemAlloc          |            8x3      +    24x3              +             128x6                                                    = 864 */
    /* MemCpy            |  24    +   8x3      +    24x3              +             128x6                                                    = 888 */
    //*********************************************************************************************************************************************/
    expect!["864"].assert_eq(
        host.as_budget()
            .get_tracker(ContractCostType::MemAlloc)?
            .inputs
            .unwrap()
            .to_string()
            .as_str(),
    );
    // 600 = 576 + 24 is correct because we need to copy all the memory allocated, as well as the
    // memory layout of the top level type (Vec).
    expect!["888"].assert_eq(
        host.as_budget()
            .get_tracker(ContractCostType::MemCpy)?
            .inputs
            .unwrap()
            .to_string()
            .as_str(),
    );
    Ok(())
}

#[test]
fn test_metered_collection() -> Result<(), HostError> {
    let budget = Budget::default();
    let v: Vec<i32> = vec![1, 2, -3, 4, -6, -11];
    let res = v
        .iter()
        .filter(|i| i.abs() > 3)
        .map(|i| Ok(i.abs() as u64))
        .metered_collect::<Result<Vec<u64>, HostError>>(&budget)??;
    assert_eq!(res, vec![4, 6, 11]);
    Ok(())
}

// This test is a sanity check to make sure we didn't accidentally change the cost schedule.
// If the cost schedule have changed, need to update this test by running
// `UPDATE_EXPECT=true cargo test`
#[test]
#[allow(unused_variables)]
fn total_amount_charged_from_random_inputs() -> Result<(), HostError> {
    let host = Host::default();
    let proto = Host::current_test_protocol();

    let mut tracker: Vec<(u64, Option<u64>)> = vec![
        (246, None),
        (1, Some(152)),
        (1, Some(65)),
        (1, Some(74)),
        (176, None),
        (97, None),
        (1, Some(49)),
        (1, Some(103)),
        (1, Some(193)),
        (226, None),
        (1, Some(227)),
        (1, Some(147)),
        (1, Some(147)),
        (47, None),
        (1, Some(1)),
        (1, None),
        (1, None),
        (1, None),
        (1, None),
        (1, None),
        (1, None),
        (1, None),
        (1, Some(1)),
    ];

    tracker.extend_from_slice(&[
        (1, Some(1)), /* ParseWasmInstructions*/
        (1, Some(1)), /* ParseWasmFunctions*/
        (1, Some(1)), /* ParseWasmGlobals*/
        (1, Some(1)), /* ParseWasmTableEntries*/
        (1, Some(1)), /* ParseWasmTypes*/
        (1, Some(1)), /* ParseWasmDataSegments*/
        (1, Some(1)), /* ParseWasmElemSegments*/
        (1, Some(1)), /* ParseWasmImports*/
        (1, Some(1)), /* ParseWasmExports*/
        (1, Some(1)), /* ParseWasmDataSegmentBytes*/
        (1, None),    /* InstantiateWasmInstructions*/
        (1, Some(1)), /* InstantiateWasmFunctions*/
        (1, Some(1)), /* InstantiateWasmGlobals*/
        (1, Some(1)), /* InstantiateWasmTableEntries*/
        (1, None),    /* InstantiateWasmTypes*/
        (1, Some(1)), /* InstantiateWasmDataSegments*/
        (1, Some(1)), /* InstantiateWasmElemSegments*/
        (1, Some(1)), /* InstantiateWasmImports*/
        (1, Some(1)), /* InstantiateWasmExports*/
        (1, Some(1)), /* InstantiateWasmDataSegmentBytes*/
        (1, None),    /* Sec1DecodePointUncompressed*/
        (1, None),    /* VerifyEcdsaSecp256r1Sig        */
    ]);

    for (ty, &(iterations, input)) in tracker.iter().enumerate() {
        host.with_budget(|b| b.bulk_charge(ContractCostType::VARIANTS[ty], iterations, input))?;
    }

    for (ty, &(iterations, input)) in tracker.iter().enumerate() {
        host.as_budget().with_shadow_mode(|| {
            host.as_budget()
                .bulk_charge(ContractCostType::VARIANTS[ty], iterations, input)
        })
    }

    let actual = format!("{:?}", host.as_budget());
    let expected = expect![[r#"
    ===============================================================================================================================================================================
    Cpu limit: 100000000; used: 15313119
    Mem limit: 41943040; used: 298417
    ===============================================================================================================================================================================
    CostType                           iterations     input          cpu_insns      mem_bytes      const_term_cpu      lin_term_cpu        const_term_mem      lin_term_mem        
    WasmInsnExec                       246            None           984            0              4                   0                   0                   0                   
    MemAlloc                           1              Some(152)      453            168            434                 16                  16                  128                 
    MemCpy                             1              Some(65)       50             0              42                  16                  0                   0                   
    MemCmp                             1              Some(74)       53             0              44                  16                  0                   0                   
    DispatchHostFunction               176            None           54560          0              310                 0                   0                   0                   
    VisitObject                        97             None           5917           0              61                  0                   0                   0                   
    ValSer                             1              Some(49)       241            389            230                 29                  242                 384                 
    ValDeser                           1              Some(103)      62271          309            59052               4001                0                   384                 
    ComputeSha256Hash                  1              Some(193)      14310          0              3738                7012                0                   0                   
    ComputeEd25519PubKey               226            None           9097178        0              40253               0                   0                   0                   
    VerifyEd25519Sig                   1              Some(227)      384738         0              377524              4068                0                   0                   
    VmInstantiation                    1              Some(147)      97310          135880         31271               57504               130065              5064                
    VmCachedInstantiation              1              Some(147)      41608          71169          40828               680                 69472               1478                
    InvokeVmFunction                   47             None           101003         705            2149                0                   15                  0                   
    ComputeKeccak256Hash               1              Some(1)        3812           0              3766                5969                0                   0                   
    DecodeEcdsaCurve256Sig             1              None           710            0              710                 0                   0                   0                   
    RecoverEcdsaSecp256k1Key           1              None           2315295        181            2315295             0                   181                 0                   
    Int256AddSub                       1              None           4404           99             4404                0                   99                  0                   
    Int256Mul                          1              None           4947           99             4947                0                   99                  0                   
    Int256Div                          1              None           4911           99             4911                0                   99                  0                   
    Int256Pow                          1              None           4286           99             4286                0                   99                  0                   
    Int256Shift                        1              None           913            99             913                 0                   99                  0                   
    ChaCha20DrawBytes                  1              Some(1)        1061           0              1058                501                 0                   0                   
    ParseWasmInstructions              1              Some(1)        37421          13981          37421               32                  13980               215                 
    ParseWasmFunctions                 1              Some(1)        657            180            0                   84156               0                   23056               
    ParseWasmGlobals                   1              Some(1)        1276           93             0                   163415              0                   11924               
    ParseWasmTableEntries              1              Some(1)        231            47             0                   29644               0                   6121                
    ParseWasmTypes                     1              Some(1)        6977           387            0                   893113              0                   49554               
    ParseWasmDataSegments              1              Some(1)        1444           43             0                   184921              0                   5525                
    ParseWasmElemSegments              1              Some(1)        2440           367            0                   312369              0                   47034               
    ParseWasmImports                   1              Some(1)        4134           795            0                   529255              0                   101762              
    ParseWasmExports                   1              Some(1)        2825           277            0                   361665              0                   35491               
    ParseWasmDataSegmentBytes          1              Some(1)        0              1              0                   14                  0                   129                 
    InstantiateWasmInstructions        1              None           43208          70792          43208               0                   70792               0                   
    InstantiateWasmFunctions           1              Some(1)        62             138            0                   8050                0                   17749               
    InstantiateWasmGlobals             1              Some(1)        83             53             0                   10647               0                   6833                
    InstantiateWasmTableEntries        1              Some(1)        15             8              0                   1933                0                   1025                
    InstantiateWasmTypes               1              None           0              0              0                   0                   0                   0                   
    InstantiateWasmDataSegments        1              Some(1)        134            1012           0                   17164               0                   129632              
    InstantiateWasmElemSegments        1              Some(1)        267            106            0                   34261               0                   13665               
    InstantiateWasmImports             1              Some(1)        5829           770            0                   746142              0                   98578               
    InstantiateWasmExports             1              Some(1)        2313           71             0                   296177              0                   9176                
    InstantiateWasmDataSegmentBytes    1              Some(1)        0              0              0                   14                  0                   126                 
    Sec1DecodePointUncompressed        1              None           1882           0              1882                0                   0                   0                   
    VerifyEcdsaSecp256r1Sig            1              None           3000906        0              3000906             0                   0                   0                   
    ===============================================================================================================================================================================
    Internal details (diagnostics info, does not affect fees) 
    Total # times meter was called: 45
    Shadow cpu limit: 100000000; used: 15313119
    Shadow mem limit: 41943040; used: 298417
    ===============================================================================================================================================================================

    "#]];
    expected.assert_eq(&actual);

    assert_eq!(
        host.as_budget().get_cpu_insns_consumed()?,
        host.as_budget().get_shadow_cpu_insns_consumed()?
    );
    assert_eq!(
        host.as_budget().get_mem_bytes_consumed()?,
        host.as_budget().get_shadow_mem_bytes_consumed()?
    );

    Ok(())
}
