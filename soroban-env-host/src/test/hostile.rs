use expect_test::expect;
use soroban_env_common::{
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    Env, EnvBase, Symbol, SymbolSmall, Tag, Val, VecObject,
};
use soroban_test_wasms::HOSTILE;

use crate::{
    budget::{AsBudget, Budget},
    host_object::HostVec,
    storage::Storage,
    test::wasm_util,
    DiagnosticLevel, Host, HostError,
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
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Context, ScErrorCode::InvalidInput)
    ));

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
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Context, ScErrorCode::InvalidInput)
    ));

    // Here we call a function that tries to forge the type of an object
    // reference and call a method on it. This fails in the relative-to-absolute
    // conversion path, where we check identity of types.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety1")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Object, ScErrorCode::UnexpectedType)
    ));

    // Here we call a function that tries to forge the type of an object
    // reference and just pass it as an _argument_ to another function. This
    // fails in the same place as the previous test.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("forgety2")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Object, ScErrorCode::UnexpectedType)
    ));

    // Here we call a function that passes an argument to a host function
    // with a bad val tag. This should fail in check_val_integrity.
    let res = host.call(
        contract_id_obj,
        Symbol::try_from_small_str("badtag")?,
        host.vec_new_from_slice(&[absolute_vec.to_val()])?,
    );
    assert!(HostError::result_matches_err(
        res.clone(),
        (ScErrorType::Value, ScErrorCode::InvalidInput)
    ));

    Ok(())
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
        Cpu limit: 2000000; used: 1075678
        Mem limit: 500000; used: 203437
        =======================================================
        CostType                 cpu_insns      mem_bytes      
        WasmInsnExec             294            0              
        MemAlloc                 18774          67200          
        MemCpy                   11151          0              
        MemCmp                   3786           0              
        DispatchHostFunction     263            0              
        VisitObject              216            0              
        ValSer                   0              0              
        ValDeser                 0              0              
        ComputeSha256Hash        2924           40             
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
