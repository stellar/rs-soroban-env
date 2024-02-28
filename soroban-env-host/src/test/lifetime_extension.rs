// Note: ignoring error handling safety in these tests.
use soroban_env_common::{AddressObject, Env, TryFromVal};
use soroban_test_wasms::CONTRACT_STORAGE;
use stellar_xdr::next::{ContractExecutable, Hash, ScAddress, ScVal};

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
        let hash = if let ScVal::Address(addr) =
            ScVal::try_from_val(&host, contract_id.as_val()).unwrap()
        {
            let ScAddress::Contract(hash) = addr else {
                panic!("Expected contract")
            };
            hash
        } else {
            panic!("Expected address type")
        };

        let code = if let ContractExecutable::Wasm(hash) = host
            .retrieve_contract_instance_from_storage(
                &host.contract_instance_ledger_key(&hash).unwrap(),
            )
            .unwrap()
            .executable
        {
            hash
        } else {
            panic!("Expected WASM executable")
        };

        host.set_ledger_info(crate::LedgerInfo {
            protocol_version: 21,
            sequence_number: 4090,
            timestamp: Default::default(),
            network_id: Default::default(),
            base_reserve: Default::default(),
            min_temp_entry_ttl: Default::default(),
            min_persistent_entry_ttl: Default::default(),
            max_entry_ttl: 10000,
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

#[cfg(feature = "next")]
mod separate_instance_code_extension {
    use super::*;

    #[test]
    fn extend_only_instance() {
        let InstanceCodeTest {
            host,
            contract_id,
            contract,
            ..
        } = InstanceCodeTest::setup();

        assert!(host
            .extend_contract_instance_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        assert_eq!(
            host.retrieve_entry_with_lifetime(
                host.contract_instance_ledger_key(&contract).unwrap()
            )
            .unwrap()
            .unwrap()
            .1,
            Some(9090)
        );
    }

    #[test]
    fn extend_only_code() {
        let InstanceCodeTest {
            host,
            contract_id,
            code,
            ..
        } = InstanceCodeTest::setup();

        assert!(host
            .extend_contract_code_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        assert_eq!(
            host.retrieve_entry_with_lifetime(host.contract_code_ledger_key(&code).unwrap())
                .unwrap()
                .unwrap()
                .1,
            Some(9090)
        );
    }
}
