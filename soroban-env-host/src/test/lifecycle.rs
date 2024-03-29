use crate::auth::RecordedAuthPayload;
use crate::{
    budget::{AsBudget, Budget},
    storage::{AccessType, Footprint, Storage, StorageMap},
    xdr::{
        self, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0,
        ContractExecutable, CreateContractArgs, ExtensionPoint, Hash, HashIdPreimage,
        HashIdPreimageContractId, LedgerEntryData, ScSymbol, ScVal, ScVec, Uint256,
    },
    Env, Host, LedgerInfo, Symbol, DEFAULT_XDR_RW_LIMITS,
};
use sha2::{Digest, Sha256};
use soroban_env_common::xdr::{
    ContractIdPreimage, ContractIdPreimageFromAddress, HostFunction, Limited, ScAddress,
    ScErrorCode, ScErrorType, SorobanAuthorizationEntry, SorobanAuthorizedFunction,
    SorobanAuthorizedInvocation, SorobanCredentials, VecM,
};
use soroban_env_common::{xdr::ScBytes, EnvBase, TryIntoVal, Val};
use soroban_env_common::{StorageType, VecObject};
use soroban_test_wasms::{ADD_I32, CREATE_CONTRACT, UPDATEABLE_CONTRACT};

use crate::testutils::{generate_account_id, generate_bytes_array};

fn get_contract_wasm_ref(host: &Host, contract_id: Hash) -> Hash {
    let storage_key = host.contract_instance_ledger_key(&contract_id).unwrap();
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has(&storage_key, host.as_budget()).unwrap());

        match &s.get(&storage_key, host.as_budget()).unwrap().data {
            LedgerEntryData::ContractData(e) => match &e.val {
                ScVal::ContractInstance(i) => match &i.executable {
                    ContractExecutable::Wasm(h) => Ok(h.clone()),
                    _ => panic!("expected Wasm executable"),
                },
                _ => panic!("expected ContractInstance"),
            },
            _ => panic!("expected contract data"),
        }
    })
    .unwrap()
}

fn get_contract_wasm(host: &Host, wasm_hash: Hash) -> Vec<u8> {
    let storage_key = host.contract_code_ledger_key(&wasm_hash).unwrap();
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
        Storage::with_enforcing_footprint_and_map(Footprint::default(), StorageMap::new());
    let host = Host::with_storage_and_budget(storage, budget);
    host.set_base_prng_seed(*Host::TEST_PRNG_SEED).unwrap();
    host.set_ledger_info(LedgerInfo {
        protocol_version: crate::meta::get_ledger_protocol_version(crate::meta::INTERFACE_VERSION),
        network_id: generate_bytes_array(&host),
        ..Default::default()
    })
    .unwrap();

    host
}

fn test_create_contract_from_source_account(host: &Host, wasm: &[u8]) -> Hash {
    let source_account = generate_account_id(host);
    let salt = generate_bytes_array(host);
    host.set_source_account(source_account.clone()).unwrap();
    let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
        address: ScAddress::Account(source_account.clone()),
        salt: Uint256(salt.to_vec().try_into().unwrap()),
    });
    // Make contractID so we can include it in the footprint
    let full_id_preimage = HashIdPreimage::ContractId(HashIdPreimageContractId {
        network_id: host
            .hash_from_bytesobj_input("network_id", host.get_ledger_network_id().unwrap())
            .unwrap(),
        contract_id_preimage: contract_id_preimage.clone(),
    });

    let contract_id = sha256_hash_id_preimage(full_id_preimage);

    let wasm_hash = Hash(Sha256::digest(wasm).try_into().unwrap());

    host.setup_storage_entry(
        host.contract_instance_ledger_key(&contract_id).unwrap(),
        None,
        AccessType::ReadWrite,
    )
    .unwrap();
    host.setup_storage_entry(
        host.contract_code_ledger_key(&wasm_hash).unwrap(),
        None,
        AccessType::ReadWrite,
    )
    .unwrap();

    // Create contract
    let created_wasm_hash: Val = host
        .invoke_function(HostFunction::UploadContractWasm(wasm.try_into().unwrap()))
        .unwrap()
        .try_into_val(host)
        .unwrap();
    let created_wasm_hash = host
        .hash_from_bytesobj_input("wasm_hash", created_wasm_hash.try_into().unwrap())
        .unwrap();
    let create_contract_args = CreateContractArgs {
        contract_id_preimage,
        executable: ContractExecutable::Wasm(created_wasm_hash.clone()),
    };
    host.set_authorization_entries(vec![SorobanAuthorizationEntry {
        credentials: SorobanCredentials::SourceAccount,
        root_invocation: SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::CreateContractHostFn(create_contract_args.clone()),
            sub_invocations: Default::default(),
        },
    }])
    .unwrap();
    let created_id_sc_val = host
        .invoke_function(HostFunction::CreateContract(create_contract_args))
        .unwrap();

    assert_eq!(
        ScVal::Address(ScAddress::Contract(contract_id.clone())),
        created_id_sc_val
    );
    assert_eq!(
        created_wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, contract_id.clone()).as_slice()
    );
    assert_eq!(wasm, get_contract_wasm(&host, wasm_hash));

    contract_id
}

// VM tests
#[test]
fn create_contract_using_parent_id_test() {
    let host = test_host();
    let parent_contract_id = test_create_contract_from_source_account(&host, CREATE_CONTRACT);
    let parent_contract_address = host
        .add_host_object(ScAddress::Contract(parent_contract_id.clone()))
        .unwrap();
    let salt = generate_bytes_array(&host);
    let child_pre_image = HashIdPreimage::ContractId(HashIdPreimageContractId {
        network_id: host
            .hash_from_bytesobj_input("network_id", host.get_ledger_network_id().unwrap())
            .unwrap(),
        contract_id_preimage: ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: ScAddress::Contract(parent_contract_id.clone()),
            salt: Uint256(salt),
        }),
    });

    let child_id = sha256_hash_id_preimage(child_pre_image);
    let child_wasm: &[u8] = &[];

    // Install the code for the child contract.
    let wasm_hash = xdr::Hash(Sha256::digest(&child_wasm).try_into().unwrap());
    // Add the contract code and code reference access to the footprint.
    host.setup_storage_entry(
        host.contract_instance_ledger_key(&child_id).unwrap(),
        None,
        AccessType::ReadWrite,
    )
    .unwrap();
    host.setup_storage_entry(
        host.contract_code_ledger_key(&wasm_hash).unwrap(),
        None,
        AccessType::ReadWrite,
    )
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
    // Can't create the contract yet, as the code hasn't been uploaded yet.
    assert!(host
        .call(
            parent_contract_address,
            Symbol::try_from_small_str("create").unwrap(),
            args,
        )
        .is_err());

    // Install the code of the child contract.
    let wasm_hash_sc_val = host
        .invoke_function(HostFunction::UploadContractWasm(
            child_wasm.try_into().unwrap(),
        ))
        .unwrap()
        .clone();
    assert_eq!(
        wasm_hash.as_slice(),
        get_bytes_from_sc_val(&wasm_hash_sc_val).as_slice()
    );
    assert_eq!(child_wasm, get_contract_wasm(&host, wasm_hash.clone()));

    // Now successfully create the child contract itself.
    host.call(
        parent_contract_address,
        Symbol::try_from_small_str("create").unwrap(),
        args,
    )
    .unwrap();

    assert_eq!(
        wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, child_id).as_slice()
    );
}

#[test]
fn create_contract_from_source_account() {
    test_create_contract_from_source_account(&test_host(), &[]);
}

pub(crate) fn sha256_hash_id_preimage<T: xdr::WriteXdr>(pre_image: T) -> xdr::Hash {
    let mut buf = Limited::new(Vec::new(), DEFAULT_XDR_RW_LIMITS);
    pre_image
        .write_xdr(&mut buf)
        .expect("preimage write failed");

    xdr::Hash(
        Sha256::digest(&buf.inner.as_slice())
            .try_into()
            .expect("invalid hash"),
    )
}

#[test]
fn test_contract_wasm_update() {
    let host = observe_host!(Host::test_host_with_recording_footprint());

    let old_wasm_hash_obj: Val = host
        .invoke_function(HostFunction::UploadContractWasm(
            UPDATEABLE_CONTRACT.to_vec().try_into().unwrap(),
        ))
        .unwrap()
        .try_into_val(&*host)
        .unwrap();

    let contract_addr_obj = host.register_test_contract_wasm(UPDATEABLE_CONTRACT);

    // Try updating with non-existing hash first.
    let non_existent_hash = [0u8; 32];
    let non_existent_contract_wasm = host.bytes_new_from_slice(&non_existent_hash).unwrap();
    let non_existent_wasm_res = host.call(
        contract_addr_obj,
        Symbol::try_from_small_str("update").unwrap(),
        test_vec![&*host, &non_existent_contract_wasm, &false].into(),
    );
    assert!(non_existent_wasm_res.is_err());
    let non_existent_wasm_err = non_existent_wasm_res.err().unwrap().error;
    assert!(non_existent_wasm_err.is_type(ScErrorType::Storage));
    assert!(non_existent_wasm_err.is_code(ScErrorCode::MissingValue));
    assert!(host.get_events().unwrap().0.is_empty());

    let updated_wasm = ADD_I32;
    let updated_wasm_hash_obj: Val = host
        .invoke_function(HostFunction::UploadContractWasm(
            updated_wasm.to_vec().try_into().unwrap(),
        ))
        .unwrap()
        .try_into_val(&*host)
        .unwrap();

    // Now do a successful update, but fail the contract after that.
    let failed_call_res = host.call(
        contract_addr_obj,
        Symbol::try_from_small_str("update").unwrap(),
        test_vec![&*host, &updated_wasm_hash_obj, &true].into(),
    );
    assert!(failed_call_res.is_err());
    let failed_call_err = failed_call_res.err().unwrap().error;
    assert!(failed_call_err.is_type(ScErrorType::WasmVm));
    assert!(failed_call_err.is_code(ScErrorCode::InvalidAction));
    // The update now has happened, but then got rolled back. Make sure
    // that it got converted to a failed system event.
    let failed_call_events = host.get_events().unwrap().0;
    assert_eq!(failed_call_events.len(), 1);
    match failed_call_events.last() {
        Some(he) => {
            assert!(he.failed_call);
            assert_eq!(he.event.type_, ContractEventType::System);
        }
        _ => {
            panic!("unexpected event");
        }
    }

    let res: i32 = host
        .call(
            contract_addr_obj,
            Symbol::try_from_small_str("update").unwrap(),
            test_vec![&*host, &updated_wasm_hash_obj, &false].into(),
        )
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    // Make sure execution continued after the update and we've got the function
    // return value.
    assert_eq!(res, 123);
    // Verify that instance storage has been updated as well.
    host.with_test_contract_frame(
        host.contract_id_from_address(contract_addr_obj).unwrap(),
        Symbol::try_from_small_str("").unwrap(),
        || {
            let stored_value: i32 = host
                .get_contract_data(
                    Symbol::try_from_small_str("foo").unwrap().into(),
                    StorageType::Instance,
                )
                .unwrap()
                .try_into_val(&*host)
                .unwrap();
            assert_eq!(stored_value, 111);
            Ok(Val::VOID.into())
        },
    )
    .unwrap();

    // Verify the contract update event.
    let events = host.get_events().unwrap().0;
    let old_wasm_hash = host
        .hash_from_bytesobj_input("old_wasm", old_wasm_hash_obj.try_into_val(&*host).unwrap())
        .unwrap();
    let updated_wasm_hash = host
        .hash_from_bytesobj_input(
            "new_wasm",
            updated_wasm_hash_obj.try_into_val(&*host).unwrap(),
        )
        .unwrap();
    assert_eq!(events.len(), 2);
    match events.last() {
        Some(he) => {
            assert!(!he.failed_call);
            assert_eq!(
                he.event,
                ContractEvent {
                    ext: ExtensionPoint::V0,
                    contract_id: Some(host.contract_id_from_address(contract_addr_obj).unwrap()),
                    type_: ContractEventType::System,
                    body: ContractEventBody::V0(ContractEventV0 {
                        topics: vec![
                            ScVal::Symbol(ScSymbol("executable_update".try_into().unwrap())),
                            ScVal::Vec(Some(ScVec(
                                vec![
                                    ScVal::Symbol(ScSymbol("Wasm".try_into().unwrap())),
                                    ScVal::Bytes(ScBytes(old_wasm_hash.0.try_into().unwrap())),
                                ]
                                .try_into()
                                .unwrap()
                            ))),
                            ScVal::Vec(Some(ScVec(
                                vec![
                                    ScVal::Symbol(ScSymbol("Wasm".try_into().unwrap())),
                                    ScVal::Bytes(ScBytes(updated_wasm_hash.0.try_into().unwrap())),
                                ]
                                .try_into()
                                .unwrap()
                            ))),
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
            contract_addr_obj,
            Symbol::try_from_small_str("add").unwrap(),
            test_vec![&*host, 10_i32, 20_i32].into(),
        )
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    assert_eq!(updated_res, 30);
}

#[test]
fn test_contract_wasm_update_with_try_call() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let contract_addr_obj = host.register_test_contract_wasm(UPDATEABLE_CONTRACT);
    let updated_contract_addr_obj = host.register_test_contract_wasm(UPDATEABLE_CONTRACT);
    let updated_wasm = ADD_I32;
    let updated_wasm_hash_obj: Val = host
        .invoke_function(HostFunction::UploadContractWasm(
            updated_wasm.to_vec().try_into().unwrap(),
        ))
        .unwrap()
        .try_into_val(&*host)
        .unwrap();

    // Run `update` that fails via external contract that does `try_call`.
    // The overall call succeeds, but the internal contract stays unchanged.
    let failed_call_res: Option<i32> = host
        .call(
            contract_addr_obj,
            Symbol::try_from_small_str("try_upd").unwrap(),
            test_vec![
                &*host,
                &updated_contract_addr_obj,
                &updated_wasm_hash_obj,
                &true
            ]
            .into(),
        )
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    assert_eq!(failed_call_res, None);

    // Make sure failure event is recorded.
    let failed_call_events = host.get_events().unwrap().0;
    assert_eq!(failed_call_events.len(), 1);
    match failed_call_events.last() {
        Some(he) => {
            assert!(he.failed_call);
            assert_eq!(he.event.type_, ContractEventType::System);
        }
        _ => {
            panic!("unexpected event");
        }
    }

    let res: Option<i32> = host
        .call(
            contract_addr_obj,
            Symbol::try_from_small_str("try_upd").unwrap(),
            test_vec![
                &*host,
                &updated_contract_addr_obj,
                &updated_wasm_hash_obj,
                &false
            ]
            .into(),
        )
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    assert_eq!(res, Some(123));
    let success_call_events = host.get_events().unwrap().0;
    assert_eq!(success_call_events.len(), 2);
    // Make sure event is recorded.
    match success_call_events.last() {
        Some(he) => {
            assert!(!he.failed_call);
            assert_eq!(he.event.type_, ContractEventType::System);
        }
        _ => {
            panic!("unexpected event");
        }
    }

    // Make sure internal contract has been updated.
    let updated_res: i32 = host
        .call(
            updated_contract_addr_obj,
            Symbol::try_from_small_str("add").unwrap(),
            test_vec![&*host, 10_i32, 20_i32].into(),
        )
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    assert_eq!(updated_res, 30);
}

#[test]
fn test_create_contract_from_source_account_recording_auth() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let source_account = generate_account_id(&host);
    let salt = generate_bytes_array(&host);
    host.set_source_account(source_account.clone()).unwrap();
    host.switch_to_recording_auth(true).unwrap();
    let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
        address: ScAddress::Account(source_account.clone()),
        salt: Uint256(salt.to_vec().try_into().unwrap()),
    });

    let created_wasm_hash: Val = host
        .invoke_function(HostFunction::UploadContractWasm(
            CREATE_CONTRACT.try_into().unwrap(),
        ))
        .unwrap()
        .try_into_val(&*host)
        .unwrap();
    let created_wasm_hash = host
        .hash_from_bytesobj_input("wasm_hash", created_wasm_hash.try_into().unwrap())
        .unwrap();
    let create_contract_args = CreateContractArgs {
        contract_id_preimage,
        executable: ContractExecutable::Wasm(created_wasm_hash.clone()),
    };
    let _ = host
        .invoke_function(HostFunction::CreateContract(create_contract_args.clone()))
        .unwrap();

    assert_eq!(
        host.get_recorded_auth_payloads().unwrap(),
        vec![RecordedAuthPayload {
            address: None,
            nonce: None,
            invocation: SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::CreateContractHostFn(create_contract_args),
                sub_invocations: VecM::default(),
            },
        }]
    );
}

#[test]
fn test_invalid_contract() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let bytes = [0u8; 32];

    let err = host
        .invoke_function(HostFunction::UploadContractWasm(bytes.try_into().unwrap()))
        .err()
        .unwrap();

    assert!(err.error.is_type(ScErrorType::WasmVm));
    assert!(err.error.is_code(ScErrorCode::InvalidAction));
}

#[test]
fn test_large_contract() {
    let host = observe_host!(Host::test_host_with_recording_footprint());
    let bytes = vec![0u8; u32::MAX.try_into().unwrap()];

    let err = host
        .invoke_function(HostFunction::UploadContractWasm(bytes.try_into().unwrap()))
        .err()
        .unwrap();

    assert!(err.error.is_type(ScErrorType::Budget));
    assert!(err.error.is_code(ScErrorCode::ExceededLimit));
}

#[cfg(feature = "next")]
#[allow(dead_code)]
mod cap_54_55_56 {

    use super::*;
    use crate::{
        storage::{FootprintMap, StorageMap},
        test::observe::ObservedHost,
        testutils::wasm::wasm_module_with_a_bit_of_everything,
        vm::ModuleCache,
        xdr::{
            ContractCostType::{self, *},
            LedgerEntry, LedgerKey,
        },
        AddressObject, HostError,
    };
    use std::rc::Rc;

    const V_NEW: u32 = ModuleCache::MIN_LEDGER_VERSION;
    const V_OLD: u32 = V_NEW - 1;
    const NEW_COST_TYPES: &'static [ContractCostType] = &[
        ParseWasmInstructions,
        ParseWasmFunctions,
        ParseWasmGlobals,
        ParseWasmTableEntries,
        ParseWasmTypes,
        ParseWasmDataSegments,
        ParseWasmElemSegments,
        ParseWasmImports,
        ParseWasmExports,
        ParseWasmDataSegmentBytes,
        InstantiateWasmInstructions,
        InstantiateWasmFunctions,
        InstantiateWasmGlobals,
        InstantiateWasmTableEntries,
        InstantiateWasmTypes,
        InstantiateWasmDataSegments,
        InstantiateWasmElemSegments,
        InstantiateWasmImports,
        InstantiateWasmExports,
        InstantiateWasmDataSegmentBytes,
    ];

    fn new_host_with_protocol_and_uploaded_contract(
        hostname: &'static str,
        proto: u32,
    ) -> Result<(ObservedHost, AddressObject), HostError> {
        let host = Host::test_host_with_recording_footprint();
        let host = ObservedHost::new(hostname, host);
        host.with_mut_ledger_info(|ledger_info| ledger_info.protocol_version = proto)?;
        let contract_addr_obj =
            host.register_test_contract_wasm(&wasm_module_with_a_bit_of_everything(proto));
        Ok((host, contract_addr_obj))
    }

    struct ContractAndWasmEntries {
        contract_key: Rc<LedgerKey>,
        contract_entry: Rc<LedgerEntry>,
        wasm_key: Rc<LedgerKey>,
        wasm_entry: Rc<LedgerEntry>,
    }

    impl ContractAndWasmEntries {
        fn from_contract_addr(
            host: &Host,
            contract_addr_obj: AddressObject,
        ) -> Result<Self, HostError> {
            let contract_id = host.contract_id_from_address(contract_addr_obj)?;
            Self::from_contract_id(host, contract_id)
        }
        fn reload(self, host: &Host) -> Result<Self, HostError> {
            host.with_mut_storage(|storage| {
                let budget = host.budget_cloned();
                let contract_entry = storage.get(&self.contract_key, &budget)?;
                let wasm_entry = storage.get(&self.wasm_key, &budget)?;
                Ok(ContractAndWasmEntries {
                    contract_key: self.contract_key,
                    contract_entry,
                    wasm_key: self.wasm_key,
                    wasm_entry,
                })
            })
        }
        fn from_contract_id(host: &Host, contract_id: Hash) -> Result<Self, HostError> {
            let contract_key = host.contract_instance_ledger_key(&contract_id)?;
            let wasm_hash = get_contract_wasm_ref(host, contract_id);
            let wasm_key = host.contract_code_ledger_key(&wasm_hash)?;

            host.with_mut_storage(|storage| {
                let budget = host.budget_cloned();
                let contract_entry = storage.get(&contract_key, &budget)?;
                let wasm_entry = storage.get(&wasm_key, &budget)?;
                Ok(ContractAndWasmEntries {
                    contract_key,
                    contract_entry,
                    wasm_key,
                    wasm_entry,
                })
            })
        }
        fn read_only_footprint(&self, budget: &Budget) -> Footprint {
            Footprint(
                FootprintMap::new()
                    .insert(self.contract_key.clone(), AccessType::ReadOnly, budget)
                    .unwrap()
                    .insert(self.wasm_key.clone(), AccessType::ReadOnly, budget)
                    .unwrap(),
            )
        }
        fn wasm_writing_footprint(&self, budget: &Budget) -> Footprint {
            Footprint(
                FootprintMap::new()
                    .insert(self.contract_key.clone(), AccessType::ReadOnly, budget)
                    .unwrap()
                    .insert(self.wasm_key.clone(), AccessType::ReadWrite, budget)
                    .unwrap(),
            )
        }
        fn storage_map(&self, budget: &Budget) -> StorageMap {
            StorageMap::new()
                .insert(
                    self.contract_key.clone(),
                    Some((self.contract_entry.clone(), Some(99999))),
                    budget,
                )
                .unwrap()
                .insert(
                    self.wasm_key.clone(),
                    Some((self.wasm_entry.clone(), Some(99999))),
                    budget,
                )
                .unwrap()
        }
        fn read_only_storage(&self, budget: &Budget) -> Storage {
            Storage::with_enforcing_footprint_and_map(
                self.read_only_footprint(budget),
                self.storage_map(budget),
            )
        }
        fn wasm_writing_storage(&self, budget: &Budget) -> Storage {
            Storage::with_enforcing_footprint_and_map(
                self.wasm_writing_footprint(budget),
                self.storage_map(budget),
            )
        }
    }

    fn upload_and_get_contract_and_wasm_entries(
        upload_hostname: &'static str,
        upload_proto: u32,
    ) -> Result<ContractAndWasmEntries, HostError> {
        let (host, contract) =
            new_host_with_protocol_and_uploaded_contract(upload_hostname, upload_proto)?;
        ContractAndWasmEntries::from_contract_addr(&host, contract)
    }

    fn upload_and_call(
        upload_hostname: &'static str,
        upload_proto: u32,
        call_hostname: &'static str,
        call_proto: u32,
    ) -> Result<(Budget, Storage), HostError> {
        // Phase 1: upload contract, tear down host, "close the ledger" and possibly change protocol.
        let (host, contract) =
            new_host_with_protocol_and_uploaded_contract(upload_hostname, upload_proto)?;
        let contract_id = host.contract_id_from_address(contract)?;
        let realhost = host.clone();
        drop(host);
        let (storage, _events) = realhost.try_finish()?;

        // Phase 2: build new host with previous ledger output as storage, call contract. Possibly on new protocol.
        let host = Host::with_storage_and_budget(storage, Budget::default());
        host.enable_debug()?;
        let host = ObservedHost::new(call_hostname, host);
        host.set_ledger_info(LedgerInfo {
            protocol_version: call_proto,
            ..Default::default()
        })?;
        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;
        let _ = host.call(
            contract,
            Symbol::try_from_small_str("test").unwrap(),
            host.vec_new()?,
        )?;
        let realhost = host.clone();
        drop(host);
        let budget = realhost.budget_cloned();
        let (storage, _events) = realhost.try_finish()?;
        Ok((budget, storage))
    }

    fn code_entry_has_cost_inputs(entry: &Rc<LedgerEntry>) -> bool {
        match &entry.data {
            LedgerEntryData::ContractCode(cce) => match &cce.ext {
                crate::xdr::ContractCodeEntryExt::V1(_v1) => return true,
                _ => (),
            },
            _ => panic!("expected LedgerEntryData::ContractCode"),
        }
        false
    }

    // Test that running on protocol vOld only charges the VmInstantiation cost
    // type.
    #[test]
    fn test_v_old_only_charges_vm_instantiation() -> Result<(), HostError> {
        let (budget, _storage) = upload_and_call(
            "test_v_old_only_charges_vminstantiation_upload",
            V_OLD,
            "test_v_old_only_charges_vm_instantiation_call",
            V_OLD,
        )?;
        assert_ne!(budget.get_tracker(VmInstantiation)?.cpu, 0);
        assert_eq!(budget.get_tracker(VmCachedInstantiation)?.cpu, 0);
        for ct in NEW_COST_TYPES {
            assert_eq!(budget.get_tracker(*ct)?.cpu, 0);
        }
        Ok(())
    }

    // Test that running on protocol vNew on a ContractCode LE that does not have
    // ContractCodeCostInputs charges the VmInstantiation and VmCachedInstantiation
    // cost types.
    #[test]
    fn test_v_new_no_contract_code_cost_inputs() -> Result<(), HostError> {
        let (budget, _storage) = upload_and_call(
            "test_v_new_no_contract_code_cost_inputs_upload",
            V_OLD,
            "test_v_new_no_contract_code_cost_inputs_call",
            V_NEW,
        )?;
        assert_ne!(budget.get_tracker(VmInstantiation)?.cpu, 0);
        assert_ne!(budget.get_tracker(VmCachedInstantiation)?.cpu, 0);
        for ct in NEW_COST_TYPES {
            assert_eq!(budget.get_tracker(*ct)?.cpu, 0);
        }
        Ok(())
    }

    // Test that running on protocol vNew does add ContractCodeCostInputs to a
    // newly uploaded ContractCode LE.
    #[test]
    fn test_v_new_gets_contract_code_cost_inputs() -> Result<(), HostError> {
        let entries = upload_and_get_contract_and_wasm_entries(
            "test_v_new_gets_contract_code_cost_inputs_upload",
            V_NEW,
        )?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));
        Ok(())
    }

    // Test that running on protocol vNew on a ContractCode LE that does have
    // ContractCodeCostInputs charges the new cost model types nonzero costs
    // (both parsing and instantiation).
    #[test]
    fn test_v_new_with_contract_code_cost_inputs_causes_nonzero_costs() -> Result<(), HostError> {
        let (budget, _storage) = upload_and_call(
            "test_v_new_with_contract_code_cost_inputs_causes_nonzero_costs_upload",
            V_NEW,
            "test_v_new_with_contract_code_cost_inputs_causes_nonzero_costs_call",
            V_NEW,
        )?;
        assert_eq!(budget.get_tracker(VmInstantiation)?.cpu, 0);
        assert_eq!(budget.get_tracker(VmCachedInstantiation)?.cpu, 0);
        for ct in NEW_COST_TYPES {
            if *ct == InstantiateWasmTypes {
                // This is a zero-cost type in the current calibration of the
                // new model -- and exceptional case in this test -- though we
                // keep it in case it becomes nonzero at some point (it's
                // credible that it would).
                continue;
            }
            assert_ne!(budget.get_tracker(*ct)?.cpu, 0);
        }
        Ok(())
    }

    // Test that running on protocol vOld does not add ContractCodeCostInputs to a
    // newly uploaded ContractCode LE.
    #[test]
    fn test_v_old_no_contract_code_cost_inputs() -> Result<(), HostError> {
        let entries = upload_and_get_contract_and_wasm_entries(
            "test_v_old_no_contract_code_cost_inputs_upload",
            V_OLD,
        )?;
        assert!(!code_entry_has_cost_inputs(&entries.wasm_entry));
        Ok(())
    }

    // Test that running on protocol vOld does not rewrite a ContractCode LE when it
    // already exists.
    #[test]
    fn test_v_old_no_rewrite() -> Result<(), HostError> {
        let entries =
            upload_and_get_contract_and_wasm_entries("test_v_old_no_rewrite_upload", V_OLD)?;
        // make a new storage map for a new run
        let budget = Budget::default();
        let storage = entries.read_only_storage(&budget);
        let host = Host::with_storage_and_budget(storage, budget);
        let host = ObservedHost::new("test_v_old_no_rewrite_call", host);
        host.set_ledger_info(LedgerInfo {
            protocol_version: V_OLD,
            ..Default::default()
        })?;
        host.upload_contract_wasm(wasm_module_with_a_bit_of_everything(V_OLD))?;
        Ok(())
    }

    // Test that running on protocol vNew does rewrite a ContractCode LE when it
    // already exists but doesn't yet have ContractCodeCostInputs.
    #[test]
    fn test_v_new_rewrite() -> Result<(), HostError> {
        let entries = upload_and_get_contract_and_wasm_entries("test_v_new_rewrite_upload", V_OLD)?;
        assert!(!code_entry_has_cost_inputs(&entries.wasm_entry));

        // make a new storage map for a new upload but with read-only footprint -- this should fail
        let budget = Budget::default();
        let storage = entries.read_only_storage(&budget);
        let host = Host::with_storage_and_budget(storage, budget);
        let host = ObservedHost::new("test_v_new_rewrite_call_fail", host);
        host.set_ledger_info(LedgerInfo {
            protocol_version: V_NEW,
            ..Default::default()
        })?;
        let wasm_blob = match &entries.wasm_entry.data {
            LedgerEntryData::ContractCode(cce) => cce.code.to_vec(),
            _ => panic!("expected ContractCode"),
        };
        assert!(host.upload_contract_wasm(wasm_blob.clone()).is_err());
        let entries = entries.reload(&host)?;
        assert!(!code_entry_has_cost_inputs(&entries.wasm_entry));

        // make a new storage map for a new upload but with read-write footprint -- this should pass
        let budget = Budget::default();
        let storage = entries.wasm_writing_storage(&budget);
        let host = Host::with_storage_and_budget(storage, budget);
        let host = ObservedHost::new("test_v_new_rewrite_call_succeed", host);
        host.set_ledger_info(LedgerInfo {
            protocol_version: V_NEW,
            ..Default::default()
        })?;
        host.upload_contract_wasm(wasm_blob)?;
        let entries = entries.reload(&host)?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        Ok(())
    }

    // Test that running on protocol vNew does not rewrite a ContractCode LE when it
    // already exists and already has ContractCodeCostInputs.
    #[test]
    fn test_v_new_no_rewrite() -> Result<(), HostError> {
        let entries =
            upload_and_get_contract_and_wasm_entries("test_v_new_no_rewrite_upload", V_NEW)?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        // make a new storage map for a new upload but with read-only footprint -- this should pass
        let budget = Budget::default();
        let storage = entries.read_only_storage(&budget);
        let host = Host::with_storage_and_budget(storage, budget);
        let host = ObservedHost::new("test_v_new_no_rewrite_call_pass", host);
        host.set_ledger_info(LedgerInfo {
            protocol_version: V_NEW,
            ..Default::default()
        })?;
        let wasm_blob = match &entries.wasm_entry.data {
            LedgerEntryData::ContractCode(cce) => cce.code.to_vec(),
            _ => panic!("expected ContractCode"),
        };
        host.upload_contract_wasm(wasm_blob.clone())?;
        let entries = entries.reload(&host)?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        Ok(())
    }
}
