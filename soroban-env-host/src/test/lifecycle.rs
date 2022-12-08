use crate::{
    budget::{AsBudget, Budget},
    host::metered_map::MeteredOrdMap,
    storage::{AccessType, Footprint, Storage},
    xdr::{
        self, ContractId, CreateContractArgs, Hash, HashIdPreimage, HashIdPreimageContractId,
        HashIdPreimageSourceAccountContractId, HostFunction, InstallContractCodeArgs,
        LedgerEntryData, ScContractCode, ScObject, ScVal, ScVec, Uint256,
    },
    CheckedEnv, Host, LedgerInfo, Symbol,
};
use soroban_env_common::{RawVal, TryIntoVal};
use soroban_test_wasms::CREATE_CONTRACT;

use sha2::{Digest, Sha256};

use super::util::{generate_account_id, generate_bytes_array};

fn get_contract_wasm_ref(host: &Host, contract_id: Hash) -> Hash {
    let storage_key = host.contract_source_ledger_key(contract_id);
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key, host.as_budget()).unwrap());

        match s.get(&storage_key, host.as_budget()).unwrap().data {
            LedgerEntryData::ContractData(cde) => match cde.val {
                ScVal::Object(Some(ScObject::ContractCode(ScContractCode::WasmRef(h)))) => Ok(h),
                _ => panic!("expected ScContractCode"),
            },
            _ => panic!("expected contract data"),
        }
    })
    .unwrap()
}

fn get_contract_wasm(host: &Host, wasm_hash: Hash) -> Vec<u8> {
    let storage_key = host.contract_code_ledger_key(wasm_hash);
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key, host.as_budget()).unwrap());

        match s.get(&storage_key, host.as_budget()).unwrap().data {
            LedgerEntryData::ContractCode(code_entry) => Ok(code_entry.code.to_vec()),
            _ => panic!("expected contract WASM code"),
        }
    })
    .unwrap()
}

fn get_bytes_from_sc_val(val: ScVal) -> Vec<u8> {
    match val {
        ScVal::Object(Some(scobj)) => match scobj {
            ScObject::Bytes(bytes) => bytes.to_vec(),
            _ => panic!("Wrong type"),
        },
        _ => panic!("Wrong type"),
    }
}

fn test_host() -> Host {
    let network_passphrase = generate_bytes_array().to_vec();
    let budget = Budget::default();
    let storage = Storage::with_enforcing_footprint_and_map(
        Footprint::default(),
        MeteredOrdMap::new(&budget).unwrap(),
    );
    let host = Host::with_storage_and_budget(storage, budget);
    host.set_ledger_info(LedgerInfo {
        network_passphrase,
        ..Default::default()
    });

    host
}

fn test_create_contract_from_source_account(host: &Host, code: &[u8]) -> Hash {
    let source_account = generate_account_id();
    let salt = generate_bytes_array();
    host.set_source_account(source_account.clone());
    // Make contractID so we can include it in the footprint
    let id_pre_image =
        HashIdPreimage::ContractIdFromSourceAccount(HashIdPreimageSourceAccountContractId {
            source_account: source_account,
            salt: Uint256(salt.to_vec().try_into().unwrap()),
            network_id: host
                .hash_from_obj_input("network_id", host.get_ledger_network_id().unwrap())
                .unwrap(),
        });

    let contract_id = sha256_hash_id_preimage(id_pre_image);

    let install_args = xdr::InstallContractCodeArgs {
        code: code.to_vec().try_into().unwrap(),
    };

    let wasm_hash = sha256_hash_id_preimage(install_args.clone());

    host.with_mut_storage(|s: &mut Storage| {
        s.footprint
            .record_access(
                &host.contract_source_ledger_key(contract_id.clone()),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        s.footprint
            .record_access(
                &host.contract_code_ledger_key(wasm_hash.clone()),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        Ok(())
    })
    .unwrap();

    // Create contract
    let wasm_id: RawVal = host
        .invoke_function(HostFunction::InstallContractCode(install_args.clone()))
        .unwrap()
        .try_into_val(host)
        .unwrap();
    let wasm_id = host
        .hash_from_obj_input("wasm_hash", wasm_id.try_into().unwrap())
        .unwrap();
    let created_id_sc_val = host
        .invoke_function(HostFunction::CreateContract(CreateContractArgs {
            contract_id: ContractId::SourceAccount(Uint256(salt.to_vec().try_into().unwrap())),
            source: ScContractCode::WasmRef(wasm_id),
        }))
        .unwrap();

    assert_eq!(
        contract_id.as_slice(),
        get_bytes_from_sc_val(created_id_sc_val).as_slice()
    );
    assert_eq!(
        wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, contract_id.clone()).as_slice()
    );
    assert_eq!(code, get_contract_wasm(&host, wasm_hash));

    contract_id
}

// VM tests
#[test]
fn create_contract_using_parent_id_test() {
    let host = test_host();
    let parent_contract_id = test_create_contract_from_source_account(&host, CREATE_CONTRACT);
    let salt = generate_bytes_array();
    let child_pre_image = HashIdPreimage::ContractIdFromContract(HashIdPreimageContractId {
        contract_id: parent_contract_id.clone(),
        salt: Uint256(salt.clone()),
        network_id: host
            .hash_from_obj_input("network_id", host.get_ledger_network_id().unwrap())
            .unwrap(),
    });

    let child_id = sha256_hash_id_preimage(child_pre_image);
    let child_wasm: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";
    let install_args = xdr::InstallContractCodeArgs {
        code: child_wasm.to_vec().try_into().unwrap(),
    };

    // Install the code for the child contract.
    let wasm_hash = sha256_hash_id_preimage(install_args.clone());
    // Add the contract code and code reference access to the footprint.
    host.with_mut_storage(|s: &mut Storage| {
        s.footprint
            .record_access(
                &host.contract_source_ledger_key(child_id.clone()),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        s.footprint
            .record_access(
                &host.contract_code_ledger_key(wasm_hash.clone()),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        Ok(())
    })
    .unwrap();

    // Prepare arguments for the factory contract call.
    let args_scvec: ScVec = vec![
        ScVal::Object(Some(ScObject::Bytes(wasm_hash.0.try_into().unwrap()))),
        ScVal::Object(Some(ScObject::Bytes(salt.try_into().unwrap()))),
    ]
    .try_into()
    .unwrap();
    let args = host.to_host_obj(&ScObject::Vec(args_scvec)).unwrap();
    // Can't create the contract yet, as the code hasn't been installed yet.
    assert!(host
        .call(
            host.test_bin_obj(&parent_contract_id.0)
                .unwrap()
                .try_into()
                .unwrap(),
            Symbol::from_str("create").into(),
            args.clone().into(),
        )
        .is_err());

    // Install the code of the child contract.
    let wasm_hash_sc_val = host
        .invoke_function(HostFunction::InstallContractCode(InstallContractCodeArgs {
            code: child_wasm.try_into().unwrap(),
        }))
        .unwrap();
    assert_eq!(
        wasm_hash.as_slice(),
        get_bytes_from_sc_val(wasm_hash_sc_val).as_slice()
    );
    assert_eq!(child_wasm, get_contract_wasm(&host, wasm_hash.clone()));

    // Now successfully create the child contract itself.
    host.call(
        host.test_bin_obj(&parent_contract_id.0)
            .unwrap()
            .try_into()
            .unwrap(),
        Symbol::from_str("create").into(),
        args.into(),
    )
    .unwrap();

    assert_eq!(
        wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, child_id).as_slice()
    );
}

#[test]
fn create_contract_from_source_account() {
    let code: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";
    test_create_contract_from_source_account(&test_host(), code);
}

pub(crate) fn sha256_hash_id_preimage<T: xdr::WriteXdr>(pre_image: T) -> xdr::Hash {
    let mut buf = Vec::new();
    pre_image
        .write_xdr(&mut buf)
        .expect("preimage write failed");

    xdr::Hash(Sha256::digest(buf).try_into().expect("invalid hash"))
}
