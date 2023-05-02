use crate::events::{Event, HostEvent};
use crate::native_contract::testutils::HostVec;
use crate::{
    budget::{AsBudget, Budget},
    host_vec,
    storage::{AccessType, Footprint, Storage, StorageMap},
    xdr::{
        self, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0, ContractId,
        CreateContractArgs, ExtensionPoint, Hash, HashIdPreimage, HashIdPreimageContractId,
        HashIdPreimageSourceAccountContractId, HostFunction, LedgerEntryData, ScContractExecutable,
        ScSymbol, ScVal, ScVec, Uint256, UploadContractWasmArgs,
    },
    Env, Host, LedgerInfo, Symbol,
};
use sha2::{Digest, Sha256};
use soroban_env_common::xdr::HostFunctionArgs;
use soroban_env_common::{xdr::ScBytes, RawVal, TryIntoVal, VecObject};
use soroban_test_wasms::{ADD_I32, CREATE_CONTRACT, UPDATEABLE_CONTRACT};

use super::util::{generate_account_id, generate_bytes_array};

fn get_contract_wasm_ref(host: &Host, contract_id: Hash) -> Hash {
    let storage_key = host.contract_executable_ledger_key(&contract_id).unwrap();
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key, host.as_budget()).unwrap());

        match &s.get(&storage_key, host.as_budget()).unwrap().data {
            LedgerEntryData::ContractData(cde) => match &cde.val {
                ScVal::ContractExecutable(ScContractExecutable::WasmRef(h)) => Ok(h.clone()),
                _ => panic!("expected ScContractExecutable"),
            },
            _ => panic!("expected contract data"),
        }
    })
    .unwrap()
}

fn get_contract_wasm(host: &Host, wasm_hash: Hash) -> Vec<u8> {
    let storage_key = host.wasm_ledger_key(&wasm_hash).unwrap();
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key, host.as_budget()).unwrap());

        match &s.get(&storage_key, host.as_budget()).unwrap().data {
            LedgerEntryData::ContractCode(code_entry) => Ok(code_entry.code.to_vec()),
            _ => panic!("expected contract WASM code"),
        }
    })
    .unwrap()
}

fn get_bytes_from_sc_val(val: &ScVal) -> Vec<u8> {
    let ScVal::Bytes(bytes) = val else {
        panic!("Wrong type")
    };
    bytes.to_vec()
}

fn test_host() -> Host {
    let budget = Budget::default();
    let storage =
        Storage::with_enforcing_footprint_and_map(Footprint::default(), StorageMap::new().unwrap());
    let host = Host::with_storage_and_budget(storage, budget.clone());
    host.set_ledger_info(LedgerInfo {
        network_id: generate_bytes_array(),
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
                .hash_from_bytesobj_input("network_id", host.get_ledger_network_id().unwrap())
                .unwrap(),
        });

    let contract_id = sha256_hash_id_preimage(id_pre_image);

    let upload_args = xdr::UploadContractWasmArgs {
        code: code.to_vec().try_into().unwrap(),
    };

    let wasm_hash = sha256_hash_id_preimage(upload_args.clone());

    host.with_mut_storage(|s: &mut Storage| {
        s.footprint
            .record_access(
                &host.contract_executable_ledger_key(&contract_id).unwrap(),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        s.footprint
            .record_access(
                &host.wasm_ledger_key(&wasm_hash).unwrap(),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        Ok(())
    })
    .unwrap();

    // Upload & create contract
    let res = host
        .invoke_functions(vec![
            HostFunction {
                args: HostFunctionArgs::UploadContractWasm(upload_args.clone()),
                auth: Default::default(),
            },
            HostFunction {
                args: HostFunctionArgs::CreateContract(CreateContractArgs {
                    contract_id: ContractId::SourceAccount(Uint256(
                        salt.to_vec().try_into().unwrap(),
                    )),
                    executable: ScContractExecutable::WasmRef(wasm_hash.clone()),
                }),
                auth: Default::default(),
            },
        ])
        .unwrap();
    assert_eq!(res.len(), 2);
    assert_eq!(
        wasm_hash.as_slice(),
        get_bytes_from_sc_val(&res[0]).as_slice()
    );
    assert_eq!(
        wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, contract_id.clone()).as_slice()
    );
    assert_eq!(
        contract_id.as_slice(),
        get_bytes_from_sc_val(&res[1]).as_slice()
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
            .hash_from_bytesobj_input("network_id", host.get_ledger_network_id().unwrap())
            .unwrap(),
    });

    let child_id = sha256_hash_id_preimage(child_pre_image);
    let child_wasm: &[u8] = b"70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4";
    let upload_args = xdr::UploadContractWasmArgs {
        code: child_wasm.to_vec().try_into().unwrap(),
    };

    // Install the code for the child contract.
    let wasm_hash = sha256_hash_id_preimage(upload_args.clone());
    // Add the contract code and code reference access to the footprint.
    host.with_mut_storage(|s: &mut Storage| {
        s.footprint
            .record_access(
                &host.contract_executable_ledger_key(&child_id).unwrap(),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        s.footprint
            .record_access(
                &host.wasm_ledger_key(&wasm_hash).unwrap(),
                AccessType::ReadWrite,
                host.as_budget(),
            )
            .unwrap();
        Ok(())
    })
    .unwrap();

    // Prepare arguments for the factory contract call.
    let args_scvec: ScVec = vec![
        ScVal::Bytes(ScBytes(wasm_hash.0.try_into().unwrap())),
        ScVal::Bytes(ScBytes(salt.try_into().unwrap())),
    ]
    .try_into()
    .unwrap();
    let args: VecObject = host
        .to_host_val(&ScVal::Vec(Some(args_scvec)))
        .unwrap()
        .try_into()
        .unwrap();
    // Can't create the contract yet, as the code hasn't been installed yet.
    assert!(host
        .call(
            host.test_bin_obj(&parent_contract_id.0)
                .unwrap()
                .try_into()
                .unwrap(),
            Symbol::try_from_small_str("create").unwrap().into(),
            args.clone().into(),
        )
        .is_err());

    // Install the code of the child contract.
    let wasm_hash_sc_val = host
        .invoke_functions(vec![HostFunction {
            args: HostFunctionArgs::UploadContractWasm(UploadContractWasmArgs {
                code: child_wasm.try_into().unwrap(),
            }),
            auth: Default::default(),
        }])
        .unwrap()[0]
        .clone();
    assert_eq!(
        wasm_hash.as_slice(),
        get_bytes_from_sc_val(&wasm_hash_sc_val).as_slice()
    );
    assert_eq!(child_wasm, get_contract_wasm(&host, wasm_hash.clone()));

    // Now successfully create the child contract itself.
    host.call(
        host.test_bin_obj(&parent_contract_id.0)
            .unwrap()
            .try_into()
            .unwrap(),
        Symbol::try_from_small_str("create").unwrap().into(),
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

    xdr::Hash(Sha256::digest(&buf).try_into().expect("invalid hash"))
}

#[test]
fn test_contract_wasm_update() {
    let host = Host::test_host_with_recording_footprint();

    let old_upload_args = xdr::UploadContractWasmArgs {
        code: UPDATEABLE_CONTRACT.to_vec().try_into().unwrap(),
    };
    let old_wasm_hash_obj: RawVal = host
        .invoke_functions(vec![HostFunction {
            args: HostFunctionArgs::UploadContractWasm(old_upload_args.clone()),
            auth: Default::default(),
        }])
        .unwrap()[0]
        .try_into_val(&host)
        .unwrap();

    let contract_id_obj = host
        .register_test_contract_wasm(UPDATEABLE_CONTRACT)
        .unwrap();
    let contract_id = host
        .hash_from_bytesobj_input("contract_id", contract_id_obj)
        .unwrap();
    let updated_wasm = ADD_I32;
    let upload_args = xdr::UploadContractWasmArgs {
        code: updated_wasm.to_vec().try_into().unwrap(),
    };

    let updated_wasm_hash_obj: RawVal = host
        .invoke_functions(vec![HostFunction {
            args: HostFunctionArgs::UploadContractWasm(upload_args.clone()),
            auth: Default::default(),
        }])
        .unwrap()[0]
        .try_into_val(&host)
        .unwrap();
    let res: i32 = host
        .call(
            contract_id_obj,
            Symbol::try_from_small_str("update").unwrap(),
            host_vec![&host, &updated_wasm_hash_obj].into(),
        )
        .unwrap()
        .try_into_val(&host)
        .unwrap();
    // Make sure execution continued after the update and we've got the function
    // return value.
    assert_eq!(res, 123);

    // Verify the contract update event.
    let events = host.get_events().unwrap().0;
    let old_wasm_hash = host
        .hash_from_bytesobj_input("old_wasm", old_wasm_hash_obj.try_into_val(&host).unwrap())
        .unwrap();
    let updated_wasm_hash = host
        .hash_from_bytesobj_input(
            "new_wasm",
            updated_wasm_hash_obj.try_into_val(&host).unwrap(),
        )
        .unwrap();
    match events.last() {
        Some(HostEvent {
            event: Event::Contract(ce),
            failed_call: _,
        }) => {
            assert_eq!(
                ce,
                &ContractEvent {
                    ext: ExtensionPoint::V0,
                    contract_id: Some(contract_id),
                    type_: ContractEventType::System,
                    body: ContractEventBody::V0(ContractEventV0 {
                        topics: vec![
                            ScVal::Symbol(ScSymbol("executable_update".try_into().unwrap())),
                            ScVal::Vec(Some(ScVec(
                                vec![
                                    ScVal::Symbol(ScSymbol("WasmRef".try_into().unwrap())),
                                    ScVal::Bytes(ScBytes(old_wasm_hash.0.try_into().unwrap()))
                                ]
                                .try_into()
                                .unwrap()
                            ))),
                            ScVal::Vec(Some(ScVec(
                                vec![
                                    ScVal::Symbol(ScSymbol("WasmRef".try_into().unwrap())),
                                    ScVal::Bytes(ScBytes(updated_wasm_hash.0.try_into().unwrap()))
                                ]
                                .try_into()
                                .unwrap()
                            )))
                        ]
                        .try_into()
                        .unwrap(),
                        data: ScVal::Vec(Some(ScVec(vec![].try_into().unwrap()))),
                    }),
                }
            );
        }
        _ => panic!("unexpected event"),
    }

    // Now the contract implementation has the `add` function for adding two
    // numbers.
    let updated_res: i32 = host
        .call(
            contract_id_obj,
            Symbol::try_from_small_str("add").unwrap(),
            host_vec![&host, 10_i32, 20_i32].into(),
        )
        .unwrap()
        .try_into_val(&host)
        .unwrap();
    assert_eq!(updated_res, 30);
}
