// Note: ignoring error handling safety in these tests.
use crate::xdr::{ContractExecutable, ContractId, Hash};
use soroban_env_common::{AddressObject, Env};
use soroban_test_wasms::CONTRACT_STORAGE;

use crate::Host;

struct InstanceCodeTest {
    host: Host,
    contract_id: AddressObject,
    contract: ContractId,
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
        let entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_instance_ledger_key(&contract).unwrap(),
                &host,
                None,
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

        assert!(host
            .extend_contract_code_ttl(contract_id, 5.into(), 5000.into())
            .is_ok());
        let entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(&host.contract_code_ledger_key(&code).unwrap(), &host, None)
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
            .get_with_live_until_ledger(&host.contract_code_ledger_key(&code).unwrap(), &host, None)
            .unwrap();

        assert_eq!(code_entry_with_live_until.1, Some(9090));

        let instance_entry_with_live_until = host
            .try_borrow_storage_mut()
            .unwrap()
            .get_with_live_until_ledger(
                &host.contract_instance_ledger_key(&contract).unwrap(),
                &host,
                None,
            )
            .unwrap();

        assert_eq!(instance_entry_with_live_until.1, Some(9090));
    }
}

// TTL extension tests for the contracts with executable reference (CAP-0085).
// The 'code' extension scope for such contracts covers both the executable
// reference entry in the owner contract and the referenced Wasm code entry.
#[cfg(feature = "next")]
mod cap_85_external_ref_extension {
    use super::*;
    use crate::{
        testutils::generate_account_id,
        xdr::{ContractExecutableExternalRef, LedgerKey, ScAddress, ScString},
        Symbol,
    };
    use soroban_env_common::{ContractTtlExtension, EnvBase, StorageType};
    use soroban_test_wasms::{ADD_I32, SUM_I32};
    use std::rc::Rc;

    struct ExternalRefTest {
        host: Host,
        contract: AddressObject,
        instance_key: Rc<LedgerKey>,
        ref_key: Rc<LedgerKey>,
        code_key: Rc<LedgerKey>,
    }

    impl ExternalRefTest {
        fn setup() -> Self {
            let host = Host::test_host_with_recording_footprint();
            host.switch_to_recording_auth(true).unwrap();

            let owner = host.register_test_contract_wasm(SUM_I32);
            let owner_id = host.contract_id_from_address(owner).unwrap();
            let wasm_hash_obj = host.upload_contract_wasm(ADD_I32.to_vec()).unwrap();
            let tag_str = b"ttl tag";

            // Store the executable reference entry in the owner's storage.
            let tag_val = host
                .create_executable_tag(host.string_new_from_slice(tag_str).unwrap())
                .unwrap()
                .to_val();
            host.with_test_contract_frame(
                owner_id,
                Symbol::try_from_small_str("set_ref").unwrap(),
                || {
                    host.put_contract_data(tag_val, wasm_hash_obj.to_val(), StorageType::Persistent)
                        .map(Into::into)
                },
            )
            .unwrap();

            // Create a contract that uses the executable reference.
            let deployer = host
                .add_host_object(ScAddress::Account(generate_account_id(&host)))
                .unwrap();
            let tag_obj = host
                .create_executable_tag(host.string_new_from_slice(tag_str).unwrap())
                .unwrap();
            let salt = host.bytes_new_from_slice(&[3u8; 32]).unwrap();
            let contract = host
                .create_external_ref_contract(
                    deployer,
                    owner,
                    tag_obj,
                    salt,
                    host.vec_new().unwrap(),
                )
                .unwrap();
            let contract_id = host.contract_id_from_address(contract).unwrap();

            let instance_key = host.contract_instance_ledger_key(&contract_id).unwrap();
            let ref_key = host
                .executable_ref_ledger_key(&ContractExecutableExternalRef {
                    executable_owner: host.scaddress_from_address(owner).unwrap(),
                    tag: ScString(tag_str.as_slice().try_into().unwrap()),
                })
                .unwrap();
            let code_key = host
                .contract_code_ledger_key(
                    &host
                        .hash_from_bytesobj_input("wasm_hash", wasm_hash_obj)
                        .unwrap(),
                )
                .unwrap();

            host.set_ledger_info(crate::LedgerInfo {
                protocol_version: Host::current_test_protocol(),
                sequence_number: 4090,
                max_entry_ttl: 10000,
                ..Default::default()
            })
            .unwrap();

            Self {
                host,
                contract,
                instance_key,
                ref_key,
                code_key,
            }
        }

        // Returns the live-until ledgers of the (contract instance, executable
        // reference entry, Wasm code entry).
        fn live_untils(&self) -> (u32, u32, u32) {
            let live_until = |key: &Rc<LedgerKey>| {
                self.host
                    .try_borrow_storage_mut()
                    .unwrap()
                    .get_with_live_until_ledger(key, &self.host, None)
                    .unwrap()
                    .1
                    .unwrap()
            };
            (
                live_until(&self.instance_key),
                live_until(&self.ref_key),
                live_until(&self.code_key),
            )
        }
    }

    #[test]
    fn test_extend_only_code_extends_reference_and_code_entries() {
        let t = ExternalRefTest::setup();
        assert_eq!(t.live_untils(), (4095, 4095, 4095));
        assert!(t
            .host
            .extend_contract_code_ttl(t.contract, 5000.into(), 5000.into())
            .is_ok());
        assert_eq!(t.live_untils(), (4095, 9090, 9090));
    }

    #[test]
    fn test_extend_code_and_instance_extends_all_entries() {
        let t = ExternalRefTest::setup();
        assert!(t
            .host
            .extend_contract_instance_and_code_ttl(t.contract, 5000.into(), 5000.into())
            .is_ok());
        assert_eq!(t.live_untils(), (9090, 9090, 9090));
    }

    #[test]
    fn test_extend_current_contract_instance_and_code_extends_all_entries() {
        let t = ExternalRefTest::setup();
        let contract_id = t.host.contract_id_from_address(t.contract).unwrap();
        t.host
            .with_test_contract_frame(
                contract_id,
                Symbol::try_from_small_str("extend").unwrap(),
                || {
                    t.host
                        .extend_current_contract_instance_and_code_ttl(5000.into(), 5000.into())
                        .map(Into::into)
                },
            )
            .unwrap();
        assert_eq!(t.live_untils(), (9090, 9090, 9090));
    }

    #[test]
    fn test_extend_v2_code_scope_covers_reference_and_code_entries() {
        let t = ExternalRefTest::setup();
        // Extends the entries with the provided scope and returns the change
        // of live-until ledger for (instance, reference, code) entries. The
        // expected change is always equal to `max_extension` (500) for the
        // entries in scope, as the requested extensions are much larger.
        let extend = |scope: ContractTtlExtension, extend_to: u32| {
            let before = t.live_untils();
            t.host
                .extend_contract_instance_and_code_ttl_v2(
                    t.contract,
                    scope,
                    extend_to.into(),
                    1.into(),
                    500.into(),
                )
                .unwrap();
            let after = t.live_untils();
            (after.0 - before.0, after.1 - before.1, after.2 - before.2)
        };
        assert_eq!(
            extend(ContractTtlExtension::InstanceAndCode, 5000),
            (500, 500, 500)
        );
        assert_eq!(extend(ContractTtlExtension::Instance, 6000), (500, 0, 0));
        assert_eq!(extend(ContractTtlExtension::Code, 7000), (0, 500, 500));
    }
}
