use std::cmp::Ordering;
use std::io::Write;
use std::rc::Rc;

use crate::events::diagnostic::DiagnosticLevel;
use crate::{xdr, ContractFunctionSet, Host, StorageType, Val};
use soroban_env_common::Compare;
use soroban_env_common::{Env, Symbol, TryFromVal, TryIntoVal};
use soroban_test_wasms::CONTRACT_STORAGE;

fn storage_fn_name(host: &Host, fn_name: &str, storage: &str) -> Symbol {
    Symbol::try_from_val(host, &format!("{}_{}", fn_name, storage).as_str()).unwrap()
}

// This is testing that an error (triggered in Storage::extend_ttl) generates
// a backtrace, and that backtrace is carried all the way to the contract call
// site. This involves handing the backtrace off from the original error to
// a secondary error event.
#[test]
fn test_backtrace_wasm() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.set_diagnostic_level(DiagnosticLevel::Debug).unwrap();
    let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);

    // Create some storage
    let key = Symbol::try_from_small_str("key").unwrap();
    let r = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "temporary"),
        test_vec![&*host, key, 1_u64].into(),
    );

    assert!(r.is_ok());

    // Advance the ledger but leave the expired storage in the host.
    // This will trigger an error about "accessing no-longer-live entry".
    let max_live_until_ledger: u32 = host.max_live_until_ledger().unwrap().into();
    let ledger_seq: u32 = host.get_ledger_sequence().unwrap().into();
    let max_extend = max_live_until_ledger - ledger_seq;
    let threshold: u32 = 1;
    let _ = host.with_mut_ledger_info(|ledger| {
        ledger.sequence_number += 1000000000;
        ledger.timestamp += 10000000;
    });

    let r = host.try_call(
        contract_id,
        storage_fn_name(&host, "extend", "temporary"),
        test_vec![&*host, key, threshold, max_extend].into(),
    );

    // The error should contain Storage::extend_ttl in the backtrace
    let err = r.unwrap_err();
    let mut buf = Vec::<u8>::new();
    let _ = writeln!(&mut buf, "{:?}", err);
    let buf = String::from_utf8_lossy(&buf);
    let expected_frame = buf.contains("Storage::extend_ttl");
    assert!(expected_frame);
}

struct Contract;

use soroban_env_common::EnvBase;

impl ContractFunctionSet for Contract {
    fn call(&self, func: &Symbol, host: &Host, args: &[Val]) -> Option<Val> {
        let put_temporary = Symbol::try_from_val(host, &"put_temporary").unwrap();
        let extend_temporary = Symbol::try_from_val(host, &"extend_temporary").unwrap();

        if host.compare(func, &put_temporary).unwrap() == Ordering::Equal {
            host.put_contract_data(args[0], args[1], StorageType::Temporary)
                .unwrap();
            Some(().into())
        } else if host.compare(func, &extend_temporary).unwrap() == Ordering::Equal {
            let r = host.extend_contract_data_ttl(
                args[0],
                StorageType::Temporary,
                args[1].try_into().unwrap(),
                args[2].try_into().unwrap(),
            );
            if let Err(e) = r {
                // This is what the SDK does when it generates a HostError while
                // calling a contract, and this code path needs to preserve
                // the backtrace across the panic.
                host.escalate_error_to_panic(e);
            }
            Some(().into())
        } else {
            panic!()
        }
    }
}

#[test]
fn test_backtrace_native() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    host.set_diagnostic_level(DiagnosticLevel::Debug).unwrap();
    let contract_id = host
        .add_host_object(xdr::ScAddress::Contract(xdr::Hash([0; 32])))
        .unwrap();
    host.register_test_contract(contract_id, Rc::new(Contract))
        .unwrap();

    // Create some storage
    let key = Symbol::try_from_small_str("key").unwrap();
    let r = host.try_call(
        contract_id,
        storage_fn_name(&host, "put", "temporary"),
        test_vec![&*host, key, 1_u64].into(),
    );

    assert!(r.is_ok());

    // Advance the ledger but leave the expired storage in the host.
    // This will trigger an error about "accessing no-longer-live entry".
    let max_live_until_ledger: u32 = host.max_live_until_ledger().unwrap().into();
    let ledger_seq: u32 = host.get_ledger_sequence().unwrap().into();
    let max_extend = max_live_until_ledger - ledger_seq;
    let threshold: u32 = 1;
    let _ = host.with_mut_ledger_info(|ledger| {
        ledger.sequence_number += 1000000000;
        ledger.timestamp += 10000000;
    });

    let r = host.try_call(
        contract_id,
        storage_fn_name(&host, "extend", "temporary"),
        test_vec![&*host, key, threshold, max_extend].into(),
    );

    // The error should contain Storage::extend_ttl in the backtrace
    let err = r.unwrap_err();
    let mut buf = Vec::<u8>::new();
    let _ = writeln!(&mut buf, "{:?}", err);
    let buf = String::from_utf8_lossy(&buf);
    let expected_frame = buf.contains("Storage::extend_ttl");
    assert!(expected_frame);
}
