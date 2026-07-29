use crate::{
    budget::{AsBudget, Budget},
    host::{
        metered_clone::{MeteredClone, MeteredIterator},
        metered_xdr::metered_write_xdr,
    },
    xdr::{ContractCostType, ScMap, ScMapEntry, ScVal},
    Env, ErrorHandler, Host, HostError, Symbol, Val,
};
use expect_test::{self, expect};
use soroban_env_common::xdr::{ScErrorCode, ScErrorType};
use soroban_test_wasms::VEC;

/// One WASM linear-memory page, in bytes.
const WASM_PAGE: u64 = 0x10000;

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
    // This contract reserves 16 pages (1 MiB) of initial linear memory: that
    // is the shadow stack the Rust/LLVM toolchain reserves by default
    // (`-z stack-size=1048576` with `--stack-first`, the rustc default since
    // 1.67), placed at the bottom of linear memory.
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
    let (cpu_count, cpu_consumed, mem_net, mem_peak, wasm_mem_alloc) =
        host.with_budget(|budget| {
            Ok((
                budget
                    .get_tracker(ContractCostType::WasmInsnExec)?
                    .iterations,
                budget.get_cpu_insns_consumed()?,
                budget.get_mem_bytes_net()?,
                budget.get_mem_bytes_consumed()?,
                budget.get_wasm_mem_alloc()?,
            ))
        })?;
    // `mem_net` is the running net after the call VM's linear memory (65536,
    // one page) is refunded at teardown; `mem_peak` (the reported consumption)
    // is the gross peak that still includes it.
    assert_eq!(
        (cpu_count, cpu_consumed, wasm_mem_alloc, mem_net, mem_peak),
        (4005, 24030, 65536, 8326, 73862)
    );
    // A single VM whose whole linear memory is live at the peak: peak equals
    // the net plus that one refunded page.
    assert_eq!(mem_peak, mem_net + wasm_mem_alloc);

    // Giving it the exact required amount will succeed. The mem limit must
    // cover the *peak* live memory (`mem_peak`), i.e. the value the reported
    // consumption now surfaces directly.
    let (cpu_required, mem_required) = (cpu_consumed, mem_peak);
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
    // (mem limit is the gross peak so mem is not the limiting factor here).
    let (cpu_required, mem_required) = (cpu_consumed - 1, mem_peak);
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
    expect!["1248"].assert_eq(
        host.as_budget()
            .get_tracker(ContractCostType::MemAlloc)?
            .inputs
            .unwrap()
            .to_string()
            .as_str(),
    );
    // 600 = 576 + 24 is correct because we need to copy all the memory allocated, as well as the
    // memory layout of the top level type (Vec).
    expect!["1272"].assert_eq(
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
        .map(|i| Ok(i.unsigned_abs() as u64))
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

    tracker.extend_from_slice(&[
        (1, None),    /* Bls12381EncodeFp */
        (1, None),    /* Bls12381DecodeFp */
        (1, None),    /* Bls12381G1CheckPointOnCurve */
        (1, None),    /* Bls12381G1CheckPointInSubgroup */
        (1, None),    /* Bls12381G2CheckPointOnCurve */
        (1, None),    /* Bls12381G2CheckPointInSubgroup */
        (1, None),    /* Bls12381G1ProjectiveToAffine */
        (1, None),    /* Bls12381G2ProjectiveToAffine */
        (1, None),    /* Bls12381G1Add */
        (1, None),    /* Bls12381G1Mul */
        (1, Some(1)), /* Bls12381G1Msm */
        (1, None),    /* Bls12381MapFpToG1 */
        (1, Some(1)), /* Bls12381HashToG1 */
        (1, None),    /* Bls12381G2Add */
        (1, None),    /* Bls12381G2Mul */
        (1, Some(1)), /* Bls12381G2Msm */
        (1, None),    /* Bls12381MapFp2ToG2 */
        (1, Some(1)), /* Bls12381HashToG2 */
        (1, Some(1)), /* Bls12381Pairing */
        (1, None),    /* Bls12381FrFromU256 */
        (1, None),    /* Bls12381FrToU256 */
        (1, None),    /* Bls12381FrAddSub */
        (1, None),    /* Bls12381FrMul */
        (1, Some(1)), /* Bls12381FrPow */
        (1, None),    /* Bls12381FrInv */
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
        Cpu limit: 100000000; used: 68471938
        Mem limit: 41943040; used(net): 727679; peak: 727679
        ===============================================================================================================================================================================
        CostType                           iterations     input          cpu_insns      mem_bytes      const_term_cpu      lin_term_cpu        const_term_mem      lin_term_mem        
        WasmInsnExec                       246            None           984            0              4                   0                   0                   0                   
        MemAlloc                           1              Some(152)      453            168            434                 16                  16                  128                 
        MemCpy                             1              Some(65)       50             0              42                  16                  0                   0                   
        MemCmp                             1              Some(74)       53             0              44                  16                  0                   0                   
        DispatchHostFunction               176            None           51920          0              295                 0                   0                   0                   
        VisitObject                        97             None           5820           0              60                  0                   0                   0                   
        ValSer                             1              Some(49)       230            389            221                 26                  242                 384                 
        ValDeser                           1              Some(103)      3846           309            331                 4369                0                   384                 
        ComputeSha256Hash                  1              Some(193)      14210          0              3636                7013                0                   0                   
        ComputeEd25519PubKey               226            None           9097856        0              40256               0                   0                   0                   
        VerifyEd25519Sig                   1              Some(227)      384749         0              377551              4059                0                   0                   
        VmInstantiation                    1              Some(147)      469979         138403         417482              45712               132773              4903                
        VmCachedInstantiation              1              Some(147)      41870          70869          41142               634                 69472               1217                
        InvokeVmFunction                   47             None           91415          658            1945                0                   14                  0                   
        ComputeKeccak256Hash               1              Some(1)        6527           0              6481                5943                0                   0                   
        DecodeEcdsaCurve256Sig             1              None           711            0              711                 0                   0                   0                   
        RecoverEcdsaSecp256k1Key           1              None           2314804        181            2314804             0                   181                 0                   
        Int256AddSub                       1              None           4176           99             4176                0                   99                  0                   
        Int256Mul                          1              None           4716           99             4716                0                   99                  0                   
        Int256Div                          1              None           4680           99             4680                0                   99                  0                   
        Int256Pow                          1              None           4256           99             4256                0                   99                  0                   
        Int256Shift                        1              None           884            99             884                 0                   99                  0                   
        ChaCha20DrawBytes                  1              Some(1)        1062           0              1059                502                 0                   0                   
        ParseWasmInstructions              1              Some(1)        73275          17614          73077               25410               17564               6457                
        ParseWasmFunctions                 1              Some(1)        4224           370            0                   540752              0                   47464               
        ParseWasmGlobals                   1              Some(1)        1377           104            0                   176363              0                   13420               
        ParseWasmTableEntries              1              Some(1)        234            49             0                   29989               0                   6285                
        ParseWasmTypes                     1              Some(1)        8292           505            0                   1061449             0                   64670               
        ParseWasmDataSegments              1              Some(1)        1854           227            0                   237336              0                   29074               
        ParseWasmElemSegments              1              Some(1)        2566           375            0                   328476              0                   48095               
        ParseWasmImports                   1              Some(1)        5483           806            0                   701845              0                   103229              
        ParseWasmExports                   1              Some(1)        3354           284            0                   429383              0                   36394               
        ParseWasmDataSegmentBytes          1              Some(1)        0              2              0                   28                  0                   257                 
        InstantiateWasmInstructions        1              None           43030          70704          43030               0                   70704               0                   
        InstantiateWasmFunctions           1              Some(1)        59             114            0                   7556                0                   14613               
        InstantiateWasmGlobals             1              Some(1)        83             53             0                   10711               0                   6833                
        InstantiateWasmTableEntries        1              Some(1)        25             8              0                   3300                0                   1025                
        InstantiateWasmTypes               1              None           0              0              0                   0                   0                   0                   
        InstantiateWasmDataSegments        1              Some(1)        179            1012           0                   23038               0                   129632              
        InstantiateWasmElemSegments        1              Some(1)        331            106            0                   42488               0                   13665               
        InstantiateWasmImports             1              Some(1)        6476           762            0                   828974              0                   97637               
        InstantiateWasmExports             1              Some(1)        2321           71             0                   297100              0                   9176                
        InstantiateWasmDataSegmentBytes    1              Some(1)        0              0              0                   14                  0                   126                 
        Sec1DecodePointUncompressed        1              None           1882           0              1882                0                   0                   0                   
        VerifyEcdsaSecp256r1Sig            1              None           3000906        0              3000906             0                   0                   0                   
        Bls12381EncodeFp                   1              None           661            0              661                 0                   0                   0                   
        Bls12381DecodeFp                   1              None           985            0              985                 0                   0                   0                   
        Bls12381G1CheckPointOnCurve        1              None           1934           0              1934                0                   0                   0                   
        Bls12381G1CheckPointInSubgroup     1              None           730510         0              730510              0                   0                   0                   
        Bls12381G2CheckPointOnCurve        1              None           5921           0              5921                0                   0                   0                   
        Bls12381G2CheckPointInSubgroup     1              None           1057822        0              1057822             0                   0                   0                   
        Bls12381G1ProjectiveToAffine       1              None           92642          0              92642               0                   0                   0                   
        Bls12381G2ProjectiveToAffine       1              None           100742         0              100742              0                   0                   0                   
        Bls12381G1Add                      1              None           7689           0              7689                0                   0                   0                   
        Bls12381G1Mul                      1              None           2458985        0              2458985             0                   0                   0                   
        Bls12381G1Msm                      1              Some(1)        3083017        111576         2347584             94135478            109494              266603              
        Bls12381MapFpToG1                  1              None           1020885        2776           1020885             0                   2776                0                   
        Bls12381HashToG1                   1              Some(1)        2638504        5896           2638451             6803                5896                0                   
        Bls12381G2Add                      1              None           25207          0              25207               0                   0                   0                   
        Bls12381G2Mul                      1              None           7873219        0              7873219             0                   0                   0                   
        Bls12381G2Msm                      1              Some(1)        9996543        221736         7663880             298580871           219654              266603              
        Bls12381MapFp2ToG2                 1              None           1856539        1672           1856539             0                   1672                0                   
        Bls12381HashToG2                   1              Some(1)        6315508        3960           6315452             7232                3960                0                   
        Bls12381Pairing                    1              Some(1)        15503174       75176          10558948            632860943           2204                9340474             
        Bls12381FrFromU256                 1              None           1994           0              1994                0                   0                   0                   
        Bls12381FrToU256                   1              None           1155           248            1155                0                   248                 0                   
        Bls12381FrAddSub                   1              None           74             0              74                  0                   0                   0                   
        Bls12381FrMul                      1              None           332            0              332                 0                   0                   0                   
        Bls12381FrPow                      1              Some(1)        1273           1              691                 74558               0                   128                 
        Bls12381FrInv                      1              None           35421          0              35421               0                   0                   0                   
        Bn254EncodeFp                      0              None           0              0              344                 0                   0                   0                   
        Bn254DecodeFp                      0              None           0              0              476                 0                   0                   0                   
        Bn254G1CheckPointOnCurve           0              None           0              0              904                 0                   0                   0                   
        Bn254G2CheckPointOnCurve           0              None           0              0              2811                0                   0                   0                   
        Bn254G2CheckPointInSubgroup        0              None           0              0              1706052             0                   0                   0                   
        Bn254G1ProjectiveToAffine          0              None           0              0              61                  0                   0                   0                   
        Bn254G1Add                         0              None           0              0              3623                0                   0                   0                   
        Bn254G1Mul                         0              None           0              0              1150435             0                   0                   0                   
        Bn254Pairing                       0              Some(0)        0              0              5263916             392472814           1821                6232546             
        Bn254FrFromU256                    0              None           0              0              2052                0                   0                   0                   
        Bn254FrToU256                      0              None           0              0              1133                0                   312                 0                   
        Bn254FrAddSub                      0              None           0              0              74                  0                   0                   0                   
        Bn254FrMul                         0              None           0              0              332                 0                   0                   0                   
        Bn254FrPow                         0              Some(0)        0              0              755                 68930               0                   0                   
        Bn254FrInv                         0              None           0              0              33151               0                   0                   0                   
        Bn254G1Msm                         0              Some(0)        0              0              1185193             41568084            73061               229779              
        ===============================================================================================================================================================================
        Internal details (diagnostics info, does not affect fees) 
        Total # times meter was called: 70
        Shadow cpu limit: 100000000; used: 68471938
        Shadow mem limit: 41943040; used: 727679
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

#[test]
fn budget_refund_mem_primitives() -> Result<(), HostError> {
    let host = Host::test_host();
    let budget = host.budget_cloned();
    budget.reset_default()?;
    // Charge some memory, then refund it.
    budget.charge(ContractCostType::MemAlloc, Some(4096))?;
    let after_charge = budget.get_mem_bytes_net()?;
    assert!(after_charge >= 4096);
    budget.refund_mem(4096)?;
    // Refund lowers the running net...
    assert_eq!(budget.get_mem_bytes_net()?, after_charge - 4096);
    // ...but never the peak: `get_mem_bytes_consumed` (the reported peak) stays
    // at the high-water mark reached before the refund.
    assert_eq!(budget.get_mem_bytes_consumed()?, after_charge);
    // Refund is saturating: refunding more than consumed floors the net at 0
    // (and still does not touch the peak).
    budget.refund_mem(u64::MAX)?;
    assert_eq!(budget.get_mem_bytes_net()?, 0);
    assert_eq!(budget.get_mem_bytes_consumed()?, after_charge);
    Ok(())
}

/// A VM's linear memory is charged during the call but refunded at teardown,
/// so it is not part of the net memory consumed after the call returns.
#[test]
fn vm_linear_memory_refunded_on_teardown() -> Result<(), HostError> {
    use crate::testutils::wasm::wasm_module_with_4n_insns;
    let host = Host::test_host_with_recording_footprint();
    let id = host.register_test_contract_wasm(&wasm_module_with_4n_insns(50));
    let host = host
        .test_budget(10_000_000, 10_485_760)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let sym = Symbol::try_from_small_str("test").unwrap();
    host.call(id, sym, host.test_vec_obj::<u32>(&[10])?)?;
    let (net, peak, vm_page) = host.with_budget(|b| {
        Ok((
            b.get_mem_bytes_net()?,
            b.get_mem_bytes_consumed()?,
            b.get_wasm_mem_alloc()?,
        ))
    })?;
    // The synthetic module declares `(memory 1)`, so its VM reserves exactly
    // one 64KiB page. With the `MemAlloc` model set to 1 mem_byte per byte,
    // the gross linear memory charged is exactly that page.
    assert_eq!(vm_page, WASM_PAGE, "vm_page = {vm_page}");
    // That whole page is refunded at teardown, so the net consumption retains
    // none of it: net is strictly the (much smaller) persistent host
    // allocations.
    assert!(
        net < WASM_PAGE,
        "net {net} should exclude the refunded VM page ({WASM_PAGE})"
    );
    // But the reported (peak) consumption still includes it. With a single VM
    // whose whole linear memory is live at the peak, the peak is exactly the
    // net plus that one refunded page:
    //   peak == net + wasm_mem_alloc.
    assert_eq!(
        peak,
        net + vm_page,
        "peak {peak} should equal net {net} + refunded VM memory {vm_page}"
    );
    Ok(())
}

/// Calling the same contract sequentially does not accumulate VM linear memory
/// in the budget: each call's VM memory is refunded before the next runs, so
/// the second call adds its own host allocations but not another VM page.
#[test]
fn sequential_calls_do_not_accumulate_vm_linear_memory() -> Result<(), HostError> {
    use crate::testutils::wasm::wasm_module_with_4n_insns;
    let host = Host::test_host_with_recording_footprint();
    let id = host.register_test_contract_wasm(&wasm_module_with_4n_insns(50));
    let host = host
        .test_budget(100_000_000, 104_857_600)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let sym = Symbol::try_from_small_str("test").unwrap();

    host.call(id, sym, host.test_vec_obj::<u32>(&[10])?)?;
    let after_one = host.as_budget().get_mem_bytes_net()?;
    assert!(
        after_one < WASM_PAGE,
        "first call added {after_one}, which must exclude the refunded VM page ({WASM_PAGE})"
    );
    host.call(id, sym, host.test_vec_obj::<u32>(&[10])?)?;
    let after_two = host.as_budget().get_mem_bytes_net()?;
    let delta = after_two - after_one;
    assert!(
        delta < WASM_PAGE,
        "second call added {delta}, which must exclude the refunded VM page ({WASM_PAGE})"
    );
    // Two sequential calls instantiate two one-page VMs
    let gross = host.as_budget().get_wasm_mem_alloc()?;
    assert_eq!(gross, 2 * WASM_PAGE, "gross = {gross}");
    Ok(())
}

/// A cross-contract call keeps the caller's VM alive while the callee's VM is
/// instantiated, so at the deepest point both VMs' linear memory is live and
/// charged simultaneously (the peak). Both are torn down by the time the
/// top-level call returns, so the combined VM memory is fully refunded and
/// excluded from the net consumption.
#[test]
fn cross_contract_call_refunds_combined_vm_linear_memory() -> Result<(), HostError> {
    use soroban_env_common::TryIntoVal;
    use soroban_test_wasms::{ADD_I32, INVOKE_CONTRACT};
    let host = Host::test_host_with_recording_footprint();
    // `add_with` on INVOKE_CONTRACT calls `add` on the target (here ADD_I32),
    // so a single top-level call nests two VMs: INVOKE_CONTRACT -> ADD_I32.
    let add_id = host.register_test_contract_wasm(ADD_I32);
    let invoke_id = host.register_test_contract_wasm(INVOKE_CONTRACT);
    let host = host
        .test_budget(100_000_000, 104_857_600)
        .enable_model(ContractCostType::WasmInsnExec, 6, 0, 0, 0)
        // 1 mem_byte charged per byte of linear memory, so the gross tracker
        // equals the raw page bytes and the assertions below are exact.
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let (a, b) = (3i32, 4i32);
    let add_with = Symbol::try_from_small_str("add_with").unwrap();

    host.call(invoke_id, add_with, test_vec![&host, a, b, add_id].into())?;
    let gross = host.as_budget().get_wasm_mem_alloc()?;
    let net = host.as_budget().get_mem_bytes_net()?;

    // Peak: both VMs' linear memory is charged at once. Each contract declares
    // its initial linear memory statically (verified against the compiled
    // wasm memory sections; neither performs a runtime `memory.grow`):
    //   - ADD_I32:         `(memory 16)` -- exactly the toolchain-default
    //                      16-page (1 MiB)
    //   - INVOKE_CONTRACT: `(memory 17)` -- the same 16-page shadow stack plus
    //                      one page for its static data segment (SDK error
    //                      strings from the `try_invoke_contract` path)
    // So the combined peak is 16 + 17 = 33 pages.
    assert_eq!(gross, 33 * WASM_PAGE, "gross = {gross}");
    // Refund: both VMs are torn down by the time the call returns. The 33-page
    // combined VM memory was refunded, leaving only small persistent host
    // allocations.
    assert!(
        net < WASM_PAGE,
        "net {net} should exclude the {gross} of refunded VM linear memory"
    );
    Ok(())
}

/// Creating a contract whose executable is an `ExternalRef` probes the
/// referenced wasm's protocol version by instantiating a throwaway VM
/// (`get_contract_protocol_version`); that probe VM's linear memory must be
/// refunded just like on the `Wasm`-executable path.
#[test]
fn external_ref_probe_vm_linear_memory_refunded() -> Result<(), HostError> {
    use crate::testutils::generate_account_id;
    use crate::xdr::ScAddress;
    use soroban_env_common::{EnvBase, StorageType};
    use soroban_test_wasms::{ADD_I32, SUM_I32};

    let host = Host::test_host_with_recording_footprint();
    host.switch_to_recording_auth(true)?;

    // An owner contract holding an executable ref to ADD_I32's wasm.
    let owner = host.register_test_contract_wasm(SUM_I32);
    let wasm_hash_obj = host.upload_contract_wasm(ADD_I32.to_vec())?;
    let owner_id = host.contract_id_from_address(owner)?;
    let tag_val = host
        .create_executable_tag(host.string_new_from_slice(b"exec tag")?)?
        .to_val();
    host.with_test_contract_frame(
        owner_id,
        Symbol::try_from_small_str("set_ref").unwrap(),
        || {
            host.put_contract_data(tag_val, wasm_hash_obj.to_val(), StorageType::Persistent)
                .map(Into::into)
        },
    )?;
    let deployer = host.add_host_object(ScAddress::Account(generate_account_id(&host)))?;
    let tag_obj = host.create_executable_tag(host.string_new_from_slice(b"exec tag")?)?;
    let salt = host.bytes_new_from_slice(&[1u8; 32])?;

    // Meter only the creation, with 1 mem_byte charged per byte of linear
    // memory so the assertions are exact.
    let host = host.test_budget(100_000_000, 104_857_600).enable_model(
        ContractCostType::MemAlloc,
        0,
        0,
        0,
        1,
    );
    let g0 = host.as_budget().get_wasm_mem_alloc()?;
    let n0 = host.as_budget().get_mem_bytes_net()?;
    host.create_external_ref_contract(deployer, owner, tag_obj, salt, host.vec_new()?)?;
    let gross = host.as_budget().get_wasm_mem_alloc()? - g0;
    let net = host.as_budget().get_mem_bytes_net()? - n0;

    // The only VM in the creation flow is the protocol-version probe of the
    // referenced wasm: ADD_I32's toolchain-default 16-page (1 MiB) stack.
    assert_eq!(gross, 16 * WASM_PAGE, "gross = {gross}");
    // That probe VM's memory is refunded, so net retains none of it.
    assert!(
        net < WASM_PAGE,
        "net {net} should exclude the refunded probe VM memory ({gross})"
    );
    Ok(())
}
