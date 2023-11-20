use expect_test::expect;
use soroban_test_wasms::HOSTILE;

use crate::{
    budget::{AsBudget, Budget},
    host_object::HostVec,
    storage::Storage,
    test::wasm_util,
    xdr::{AccountId, ContractCostType, PublicKey, ScErrorCode, ScErrorType, Uint256},
    DiagnosticLevel, Env, EnvBase, Error, Host, HostError, Symbol, SymbolSmall, Tag, Val,
    VecObject,
};

#[test]
fn hostile_iloop_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("iloop")?,
        host.add_host_object(HostVec::new())?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_badack_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("badack")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

#[test]
fn hostile_ssmash_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("ssmash")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_oob1_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("oob1")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_oob2_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("oob2")?,
        host.add_host_object(HostVec::new())?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn hostile_objs_traps() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    host.with_budget(|b| b.reset_default())?;
    host.with_budget(|b| b.reset_unlimited_cpu())?;

    // This one should just run out of memory
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("objs")?,
        host.add_host_object(HostVec::new())?,
    );

    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    Ok(())
}

fn assert_err_value_invalid_input(res: Result<Val, HostError>) {
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::Value, ScErrorCode::InvalidInput),
    ));
}

#[test]
fn hostile_forged_objects_trap() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);

    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;
    host.with_budget(|b| b.reset_default())?;
    host.with_budget(|b| b.reset_unlimited_cpu())?;

    fn forged_val_to_forge_call_args(host: &Host, val: Val) -> Result<VecObject, HostError> {
        let payload = val.get_payload();
        let lo = payload as u32;
        let hi = (payload >> 32) as u32;
        host.vec_new_from_slice(&[lo.into(), hi.into()])
    }

    // Here we're passing a vector of two numbers that, when reassembled into a
    // payload and cast to an object, denote an absolute object reference. These
    // should fail because relative-to-absolute conversion will reject it.
    let absolute_vec = host.vec_new_from_slice(&[1u32.into(), 2u32.into()])?;
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgeref")?,
        forged_val_to_forge_call_args(&host, absolute_vec.to_val())?,
    );
    assert_err_value_invalid_input(res);

    // Here we just pick a big handle number -- but with a zero bit set, so it's
    // a relative handle -- to poke around "random object space" to see if we
    // can get an object. This will fail because it doesn't denote anything in
    // the relative object table (it's past the end).
    let big_vec_ref = unsafe { VecObject::from_handle(0xffff0) };
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgeref")?,
        forged_val_to_forge_call_args(&host, big_vec_ref.to_val())?,
    );
    assert_err_value_invalid_input(res);

    // Here we call a function that tries to forge the type of an object
    // reference and call a method on it. This fails in the relative-to-absolute
    // conversion path, where we check identity of types.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety1")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert_err_value_invalid_input(res);

    // Here we call a function that tries to forge the type of an object
    // reference and just pass it as an _argument_ to another function. This
    // fails in the same place as the previous test.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety2")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert_err_value_invalid_input(res);

    // Here we call a function that passes an argument to a host function
    // with a bad val tag. This fails during argument unmarshalling in
    // dispatch functions.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("badtag")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert_err_value_invalid_input(res);

    Ok(())
}

const BAD_VALS: &[u64] = &[
    // These are Vals with bad tags.
    0x0000_0000_0000_00_10_u64,
    0x0000_0000_0000_00_4e_u64,
    0x0000_0000_0000_00_7f_u64,
    0x0000_0000_0000_00_ff_u64,
    // These are False with nonzero major and minor-bits.
    0x1111_0000_0000_00_00_u64,
    0x0000_0000_0000_11_00_u64,
    // These are True with nonzero major and minor-bits.
    0x1111_0000_0000_00_01_u64,
    0x0000_0000_0000_11_01_u64,
    // These are Void with nonzero major and minor-bits.
    0x1111_0000_0000_00_02_u64,
    0x0000_0000_0000_11_02_u64,
    // These are Error with invalid types.
    0x0000_0000_0000_0a_03_u64,
    0x0000_0000_0000_ff_03_u64,
    0x0000_0000_f000_00_03_u64,
    // These are Error with invalid values.
    0x0000_000a_0000_01_03_u64,
    0x0000_00ff_0000_01_03_u64,
    0xf000_0000_0000_01_03_u64,
    // This is a U32Val with nonzero minor-bits.
    0x0000_0000_0000_11_04_u64,
    // This is a I32Val with nonzero minor-bits.
    0x0000_0000_0000_11_05_u64,
    // These are SymbolSmalls with the two most significant bits set.
    0x8000_0000_0000_00_0e_u64,
    0x4000_0000_0000_00_0e_u64,
    0xc000_0000_0000_00_0e_u64,
    0xc000_0000_0000_ab_0e_u64,
    0xc000_0000_00ab_cd_0e_u64,
    0xffff_ffff_ffff_ff_0e_u64,
    // This is a BytesObject with nonzero minor-bits.
    // It should be rejected _before_ its invalid
    // object-index or object-type is discovered.
    0x0000_0000_0000_11_48_u64,
];

#[test]
fn guest_val_integrity_errors() {
    fn check_badval(host: &Host, contract: crate::AddressObject, badu64: u64) {
        // To smuggle a badval into the callee at all we need to split it in
        // two u32 vals. If we try to pass it _as_ a Val, the code that builds
        // the argument vector will reject it.
        let lo = crate::U32Val::from(badu64 as u32).to_val();
        let hi = crate::U32Val::from((badu64 >> 32) as u32).to_val();
        let badval = Val::from_payload(badu64);
        let res = if let Ok(u) = crate::U32Val::try_from(badval) {
            // create a valid local vector with a single boolean element and then
            // index into it using the bad value
            let vin = vec![Val::TRUE.to_val(); u32::from(u) as usize + 1];
            let vec = host.vec_new_from_slice(&vin).unwrap();
            host.call(
                contract,
                Symbol::try_from_small_str("idxbad").unwrap(),
                host.vec_new_from_slice(&[vec.to_val(), lo, hi]).unwrap(),
            )
        } else {
            // pass the bad value as a polymorphic Val argument to vec_push
            let vec = host.vec_new_from_slice(&[]).unwrap();
            host.call(
                contract,
                Symbol::try_from_small_str("pushbad").unwrap(),
                host.vec_new_from_slice(&[vec.to_val(), lo, hi]).unwrap(),
            )
        };
        assert_err_value_invalid_input(res);
    }

    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(HOSTILE);
    for i in BAD_VALS {
        check_badval(&host, contract_id_obj, *i);
    }
}

#[test]
fn local_val_integrity_errors() {
    fn check_badval(host: &Host, badval: u64) {
        let badval = Val::from_payload(badval);
        let res = if let Ok(u) = crate::U32Val::try_from(badval) {
            // create a valid local vector with a single boolean element and then
            // index into it using the bad value
            let vin = vec![Val::TRUE.to_val(); u32::from(u) as usize + 1];
            let vec = host.vec_new_from_slice(&vin).unwrap();
            host.vec_get(vec, u)
        } else {
            // pass the bad value as a polymorphic Val argument to vec_new_from_slice
            host.vec_new_from_slice(&[badval]).map(|x| x.to_val())
        };
        assert_err_value_invalid_input(res);
    }

    let host = Host::default();
    for i in BAD_VALS {
        check_badval(&host, *i);
    }
}

#[test]
fn excessive_memory_growth() -> Result<(), HostError> {
    // `memory_grow(32)`, wasmi will desire 33 pages of memory, that includes the
    // initial page.
    let wasm = wasm_util::wasm_module_with_mem_grow(32);
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
    let host = host
        .test_budget(0, 0)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    host.set_diagnostic_level(crate::DiagnosticLevel::Debug)?;

    // This one should just run out of memory.
    {
        let mem_budget = 32 * 0x10_000;
        host.as_budget().reset_limits(40_000, mem_budget)?;
        let res = host.call(
            contract_id_obj,
            Symbol::try_from_small_str("test")?,
            host.add_host_object(HostVec::new())?,
        );
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Budget, ScErrorCode::ExceededLimit)
        ));
        // only the intial page has been allocated
        assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 0x10_000);
    }

    // giving it 33 pages plus some small overhead should be okay
    {
        let mem_budget = 33 * 0x10_000 + 2000;
        // Note on requiring non-zero cpu limit: even though no cpu instruction
        // is consumed, wasmi does require checking internally there is enough fuel
        // to finish the task. memory_grow is a special instruction that requires extra
        // fuel (64 bytes per fuel), so we would roughly require `requested memory size / 64`
        // cpu instructions to satisfy wasmi.
        host.as_budget().reset_limits(40_000, mem_budget)?;
        let res = host.call(
            contract_id_obj,
            Symbol::try_from_small_str("test")?,
            host.add_host_object(HostVec::new())?,
        );
        assert!(res.is_ok());
        // initial 1 page + 32 extra pages has been allocated
        assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 33 * 0x10_000);
    }

    Ok(())
}

fn instantiate_with_mem_and_table_sizes(
    host: &Host,
    mem_pages: u32,
    elem_count: u32,
) -> Result<crate::AddressObject, HostError> {
    let wasm = wasm_util::wasm_module_with_user_specified_initial_size(mem_pages, elem_count);
    host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        AccountId(PublicKey::PublicKeyTypeEd25519(Uint256([0; 32]))),
        [0; 32],
    )
}

#[test]
fn moderate_sized_initial_memory_request_ok() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_mem_and_table_sizes(&host, 10, 0);
    assert!(res.is_ok());
    assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 0x10_000 * 10);
    Ok(())
}

#[test]
fn initial_memory_request_over_limit_fails() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_mem_and_table_sizes(&host, 1000, 0);
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit),
    ));
    // no wasm memory is allocated at all
    assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 0);
    Ok(())
}

#[test]
fn moderate_sized_initial_table_request_ok() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_mem_and_table_sizes(&host, 0, 500);
    assert!(res.is_ok());
    Ok(())
}

#[test]
fn initial_table_request_over_limit_fails() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_mem_and_table_sizes(&host, 0, 2000);
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit),
    ));
    Ok(())
}

fn instantiate_with_data_segment(
    host: &Host,
    mem_pages: u32,
    mem_offset: u32,
    len: u32,
) -> Result<crate::AddressObject, HostError> {
    let wasm = wasm_util::wasm_module_with_large_data_segment(mem_pages, mem_offset, len);
    host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        AccountId(PublicKey::PublicKeyTypeEd25519(Uint256([0; 32]))),
        [0; 32],
    )
}

#[test]
fn data_segment_smaller_than_a_page_fits_in_one_page_memory() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.as_budget().reset_unlimited_cpu()?;
    let res = instantiate_with_data_segment(&host, 1, 0, 5000);
    assert!(res.is_ok());
    assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 0x10_000);
    Ok(())
}

#[test]
fn data_segment_larger_than_a_page_does_not_fit_in_one_page_memory() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.as_budget().reset_unlimited_cpu()?;
    let res = instantiate_with_data_segment(&host, 1, 0, 100_000);
    assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 0x10_000);
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::WasmVm, ScErrorCode::IndexBounds),
    ));
    Ok(())
}

#[test]
fn data_segment_larger_than_a_page_fits_in_two_page_memory() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    host.as_budget().reset_unlimited_cpu()?;
    let res = instantiate_with_data_segment(&host, 2, 0, 100_000);
    assert!(res.is_ok());
    assert_eq!(host.as_budget().get_wasm_mem_alloc()?, 2 * 0x10_000);
    Ok(())
}

fn instantiate_with_page_and_segment_count(
    host: &Host,
    num_pages: u32,
    num_sgmts: u32,
    seg_size: u32,
) -> Result<crate::AddressObject, HostError> {
    let wasm = wasm_util::wasm_module_with_multiple_data_sections(num_pages, num_sgmts, seg_size);
    host.register_test_contract_wasm_from_source_account(
        wasm.as_slice(),
        AccountId(PublicKey::PublicKeyTypeEd25519(Uint256([0; 32]))),
        [0; 32],
    )
}

#[test]
fn many_small_segments_ok() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_page_and_segment_count(&host, 1, 10000, 1);
    assert!(res.is_ok());
    Ok(())
}

#[test]
fn few_large_segments_ok() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_page_and_segment_count(&host, 1, 10, 10000);
    assert!(res.is_ok());
    Ok(())
}

#[test]
fn many_large_segments_exceeds_budget() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_page_and_segment_count(&host, 20, 10000, 10000);
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit),
    ));
    Ok(())
}

#[test]
fn way_too_many_segments_exceeds_budget() -> Result<(), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let res = instantiate_with_page_and_segment_count(&host, 1, 10_000_000, 1);
    assert!(HostError::result_matches_err(
        res,
        Error::from_type_and_code(ScErrorType::Budget, ScErrorCode::ExceededLimit),
    ));
    Ok(())
}

// Regression test for infinte loop / recursion
// while externalizing diagnostics for objects
// with invalid references.
#[test]
fn broken_object() {
    fn val_from_body_and_tag(body: u64, tag: Tag) -> Val {
        unsafe {
            // Safety: Val is a repr(transparent) u64
            const TAG_BITS: usize = 8;
            std::mem::transmute((body << TAG_BITS) | (tag as u64))
        }
    }

    let budget = Budget::default();
    let storage = Storage::default();
    let host = Host::with_storage_and_budget(storage, budget);

    // Diagnostics must be on
    host.set_diagnostic_level(DiagnosticLevel::Debug).unwrap();

    // Bogus u256 object
    let bad_val = val_from_body_and_tag(u64::MAX, Tag::U256Object);

    // This iloops externalizing diagnostics for the error it is generating.
    let _args = host.vec_new_from_slice(&[bad_val]);
}

#[test]
fn excessive_logging() -> Result<(), HostError> {
    let wasm = wasm_util::wasm_module_with_linear_memory_logging();
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    let expected_budget = expect![[r#"
        =======================================================
        Cpu limit: 2000000; used: 1077542
        Mem limit: 500000; used: 203445
        =======================================================
        CostType                 cpu_insns      mem_bytes      
        WasmInsnExec             294            0              
        MemAlloc                 19915          67248          
        MemCpy                   11658          0              
        MemCmp                   3786           0              
        DispatchHostFunction     263            0              
        VisitObject              432            0              
        ValSer                   0              0              
        ValDeser                 0              0              
        ComputeSha256Hash        2924           0              
        ComputeEd25519PubKey     0              0              
        VerifyEd25519Sig         0              0              
        VmInstantiation          1037145        136183         
        VmCachedInstantiation    0              0              
        InvokeVmFunction         1125           14             
        ComputeKeccak256Hash     0              0              
        ComputeEcdsaSecp256k1Sig 0              0              
        RecoverEcdsaSecp256k1Key 0              0              
        Int256AddSub             0              0              
        Int256Mul                0              0              
        Int256Div                0              0              
        Int256Pow                0              0              
        Int256Shift              0              0              
        ChaCha20DrawBytes        0              0              
        =======================================================

    "#]];

    // moderate logging
    {
        host.budget_ref().reset_limits(2_000_000, 500_000)?;
        let res = host.call(
            contract_id_obj,
            Symbol::try_from_small_str("test")?,
            host.test_vec_obj(&[0_u32, 10_u32, 0_u32, 10_u32])?,
        )?;
        assert_eq!(SymbolSmall::try_from(res)?.to_string(), "pass");
        // three debug events: fn_call, log, fn_return
        assert_eq!(host.get_events()?.0.len(), 3);
        assert!(
            !host.as_budget().shadow_mem_limit_exceeded()?
                && !host.as_budget().shadow_cpu_limit_exceeded()?
        );
        let actual = format!("{}", host.as_budget());
        expected_budget.assert_eq(&actual);
    }

    // excessive logging
    {
        host.budget_ref().reset_limits(2_000_000, 500_000)?;
        let res = host.call(
            contract_id_obj,
            Symbol::try_from_small_str("test")?,
            // log the entire page of linear memory
            host.test_vec_obj(&[0_u32, 65536_u32, 0_u32, 8192_u32])?,
        )?;
        // logging failure occurs in debug mode will not result in invocation failure
        assert_eq!(SymbolSmall::try_from(res)?.to_string(), "pass");
        // no event will be externalized, since the `events.externalize()` happens after
        // the internal limit has been exhausted
        assert_eq!(host.get_events()?.0.len(), 0);
        // the internal limit has been exceeded
        assert!(host.as_budget().shadow_mem_limit_exceeded()?);
        let actual = format!("{}", host.as_budget());
        // the actual production budget numbers should stay the same
        expected_budget.assert_eq(&actual);
    }

    // increasing the shadow budget should make everything happy again
    {
        host.budget_ref().reset_limits(2_000_000, 500_000)?;
        host.set_shadow_budget_limits(2_000_000, 1_000_000)?;
        let res = host.call(
            contract_id_obj,
            Symbol::try_from_small_str("test")?,
            // log the entire page of linear memory
            host.test_vec_obj(&[0_u32, 65536_u32, 0_u32, 8192_u32])?,
        )?;
        // logging failure occurs in debug mode will not result in invocation failure
        assert_eq!(SymbolSmall::try_from(res)?.to_string(), "pass");
        assert!(
            !host.as_budget().shadow_mem_limit_exceeded()?
                && !host.as_budget().shadow_cpu_limit_exceeded()?
        );
        let actual = format!("{}", host.as_budget());
        // the actual production budget numbers should stay the same
        expected_budget.assert_eq(&actual);
    }

    Ok(())
}

#[test]
fn test_unreachable_contract_should_fail() -> Result<(), HostError> {
    let wasm = wasm_util::wasm_module_with_unreachable();
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    host.budget_ref().reset_limits(2_000_000, 500_000)?;
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    Ok(())
}

#[test]
fn test_indirect_call_via_table_access() -> Result<(), HostError> {
    // this module contains a table with 128 FuncRef elements, 3 of which are
    // occuplied with 1 host function 2 contract functions
    let wasm = wasm_util::wasm_module_with_indirect_call();
    let host = Host::test_host_with_recording_footprint();
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());
    host.budget_ref().reset_unlimited()?;

    let call_fn = |raw_bits: u64| -> Result<Val, HostError> {
        // construct HostVec directly to avoid the check_val_integrety
        let hv = HostVec::from_vec(vec![Val::from_payload(raw_bits)])?;
        let args = host.add_host_object(hv)?;
        host.call(contract_id_obj, Symbol::try_from_small_str("test")?, args)
    };

    // three functions matches the correct type, they should succeed
    for i in 0..=2 {
        let res = call_fn(i);
        assert!(res.is_ok());
    }
    // between 3 and 128 -- the elements count, the FuncRef will be missing
    // but they are valid table bounds.
    // Need to skip the Tag::Object ranges to avoid the internall error converting
    // object handle.
    for i in 3..=Tag::ObjectCodeLowerBound as u64 {
        let res = call_fn(i);
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::WasmVm, ScErrorCode::MissingValue)
        ));
    }
    // after 127 the index is out of table elements range
    for i in 128..200 {
        let res = call_fn(i);
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::WasmVm, ScErrorCode::IndexBounds)
        ));
    }
    Ok(())
}

#[test]
fn test_div_by_zero() -> Result<(), HostError> {
    let wasm = wasm_util::wasm_module_with_div_by_zero();
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    host.budget_ref().reset_limits(2_000_000, 500_000)?;
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::ArithDomain)
    ));
    Ok(())
}

#[test]
fn test_integer_overflow() -> Result<(), HostError> {
    let wasm = wasm_util::wasm_module_with_integer_overflow();
    let host = Host::test_host_with_recording_footprint();
    host.enable_debug()?;
    let contract_id_obj = host.register_test_contract_wasm(wasm.as_slice());

    host.budget_ref().reset_limits(2_000_000, 500_000)?;
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("test")?,
        host.test_vec_obj::<u32>(&[])?,
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::WasmVm, ScErrorCode::ArithDomain)
    ));
    Ok(())
}
