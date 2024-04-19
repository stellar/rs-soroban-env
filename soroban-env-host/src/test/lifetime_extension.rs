// Note: ignoring error handling safety in these tests.
use crate::xdr::{ContractExecutable, Hash};
use soroban_env_common::{AddressObject, Env};
use soroban_test_wasms::CONTRACT_STORAGE;

use crate::Host;

struct InstanceCodeTest {
    host: Host,
    contract_id: AddressObject,
    contract: Hash,
    code: Hash,
}

impl InstanceCodeTest {
    // We can potentially add some customizability for the ledger here.
    fn setup() -> Self {
        let host = Host::test_host_with_recording_footprint();
        let contract_id = host.register_test_contract_wasm(CONTRACT_STORAGE);
        let hash = host.contract_id_from_address(contract_id).unwrap();

        let code = if let ContractExecutable::Wasm(hash) = host
            .retrieve_contract_instance_from_storage(
                &host.contract_instance_ledger_key(&hash).unwrap(),
            )
            .unwrap()
            .executable
        {
            hash
        } else {
            panic!("Expected Wasm executable")
        };

        host.set_ledger_info(crate::LedgerInfo {
            protocol_version: Host::current_test_protocol(),
            sequence_number: 4090,
            max_entry_ttl: 10000,
            ..Default::default()
        })
        .unwrap();

        Self {
            host,
            contract_id,
            contract: hash,
            code,
        }
    }
}

mod separate_instance_code_extension {
    use crate::budget::AsBudget;

    use super::*;

    const PROTOCOL_SUPPORT_FOR_SEPARATE_EXTENSIONS: u32 = 21;

    #[test]
    fn extend_only_instance() {
        let InstanceCodeTest {
            host,
            contract_id,
            contract,
            ..
        } = InstanceCodeTest::setup();

        if host.get_ledger_protocol_version().unwrap() < PROTOCOL_SUPPORT_FOR_SEPARATE_EXTENSIONS {
            return;
        }

        assert!(host
            .extend_contract_instance_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        let entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_instance_ledger_key(&contract).unwrap(),
                host.as_budget(),
            )
            .unwrap();

        assert_eq!(entry_with_live_until.1, Some(9090));
    }

    #[test]
    fn extend_only_code() {
        let InstanceCodeTest {
            host,
            contract_id,
            code,
            ..
        } = InstanceCodeTest::setup();

        if host.get_ledger_protocol_version().unwrap() < PROTOCOL_SUPPORT_FOR_SEPARATE_EXTENSIONS {
            return;
        }

        assert!(host
            .extend_contract_code_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        let entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_code_ledger_key(&code).unwrap(),
                host.as_budget(),
            )
            .unwrap();

        assert_eq!(entry_with_live_until.1, Some(9090));
    }

    #[test]
    fn extend_code_and_instance() {
        let InstanceCodeTest {
            host,
            contract_id,
            code,
            contract,
        } = InstanceCodeTest::setup();

        assert!(host
            .extend_contract_instance_and_code_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        let code_entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_code_ledger_key(&code).unwrap(),
                host.as_budget(),
            )
            .unwrap();

        assert_eq!(code_entry_with_live_until.1, Some(9090));

        let instance_entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_instance_ledger_key(&contract).unwrap(),
                host.as_budget(),
            )
            .unwrap();

        assert_eq!(instance_entry_with_live_until.1, Some(9090));
    }
}
