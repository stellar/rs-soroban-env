use crate::auth::RecordedAuthPayload;
use crate::HostError;
use crate::{
    budget::{AsBudget, Budget},
    storage::{AccessType, Footprint, Storage, StorageMap},
    xdr::{
        self, AccountId, ContractEvent, ContractEventBody, ContractEventType, ContractEventV0,
        ContractExecutable, ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgs,
        CreateContractArgsV2, ExtensionPoint, Hash, HashIdPreimage, HashIdPreimageContractId,
        HostFunction, LedgerEntryData, Limited, ScAddress, ScBytes, ScErrorCode, ScErrorType,
        ScSymbol, ScVal, ScVec, SorobanAuthorizationEntry, SorobanAuthorizedFunction,
        SorobanAuthorizedInvocation, SorobanCredentials, Uint256, VecM,
    },
    Env, Host, LedgerInfo, Symbol, DEFAULT_XDR_RW_LIMITS,
};
use pretty_assertions::assert_eq;
use sha2::{Digest, Sha256};
use soroban_env_common::{EnvBase, StorageType, TryIntoVal, Val, VecObject};
use soroban_test_wasms::{ADD_I32, CREATE_CONTRACT, UPDATEABLE_CONTRACT};

use crate::testutils::{generate_account_id, generate_bytes_array};

fn get_contract_wasm_ref(host: &Host, contract_id: Hash) -> Hash {
    let storage_key = host.contract_instance_ledger_key(&contract_id).unwrap();
    host.with_mut_storage(|s: &mut Storage| {
        assert!(s.has_with_host(&storage_key, &host, None).unwrap());

        match &s.get_with_host(&storage_key, host, None).unwrap().data {
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
        assert!(s.has_with_host(&storage_key, &host, None).unwrap());

        match &s.get_with_host(&storage_key, host, None).unwrap().data {
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
        protocol_version: Host::current_test_protocol(),
        network_id: generate_bytes_array(&host),
        ..Default::default()
    })
    .unwrap();
    host.enable_debug().unwrap();

    host
}

#[allow(dead_code)]
enum CreateContractArgsVersion {
    V1,
    V2,
}

#[derive(Default)]
struct CreateContractTestParams {
    sub_invocation_auth: Vec<SorobanAuthorizedInvocation>,
    use_create_contract_args_v1: bool,
    use_auth_invocation_v1: bool,
    skip_auth: bool,
}

impl CreateContractTestParams {
    fn create_host_fn(
        &self,
        contract_id_preimage: &ContractIdPreimage,
        uploaded_wasm_hash: &Hash,
        constructor_args: &Vec<ScVal>,
    ) -> HostFunction {
        if self.use_create_contract_args_v1 {
            HostFunction::CreateContract(CreateContractArgs {
                contract_id_preimage: contract_id_preimage.clone(),
                executable: ContractExecutable::Wasm(uploaded_wasm_hash.clone()),
            })
        } else {
            HostFunction::CreateContractV2(CreateContractArgsV2 {
                contract_id_preimage: contract_id_preimage.clone(),
                executable: ContractExecutable::Wasm(uploaded_wasm_hash.clone()),
                constructor_args: constructor_args.clone().try_into().unwrap(),
            })
        }
    }

    fn create_auth_invocation(
        &self,
        contract_id_preimage: &ContractIdPreimage,
        uploaded_wasm_hash: &Hash,
        constructor_args: &Vec<ScVal>,
    ) -> SorobanAuthorizedInvocation {
        if self.use_auth_invocation_v1 {
            SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::CreateContractHostFn(CreateContractArgs {
                    contract_id_preimage: contract_id_preimage.clone(),
                    executable: ContractExecutable::Wasm(uploaded_wasm_hash.clone()),
                }),
                sub_invocations: Default::default(),
            }
        } else {
            SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::CreateContractV2HostFn(CreateContractArgsV2 {
                    contract_id_preimage: contract_id_preimage.clone(),
                    executable: ContractExecutable::Wasm(uploaded_wasm_hash.clone()),
                    constructor_args: constructor_args.clone().try_into().unwrap(),
                }),
                sub_invocations: self.sub_invocation_auth.clone().try_into().unwrap(),
            }
        }
    }
}

fn get_contract_id_from_address(
    host: &Host,
    address: ScAddress,
    salt: [u8; 32],
) -> (Hash, ContractIdPreimage) {
    let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
        address,
        salt: Uint256(salt.to_vec().try_into().unwrap()),
    });
    // Make ContractID so we can include it in the footprint
    let full_id_preimage = HashIdPreimage::ContractId(HashIdPreimageContractId {
        network_id: host
            .hash_from_bytesobj_input("network_id", host.get_ledger_network_id().unwrap())
            .unwrap(),
        contract_id_preimage: contract_id_preimage.clone(),
    });

    let contract_id = sha256_hash_id_preimage(full_id_preimage);
    (contract_id, contract_id_preimage)
}

fn get_contract_id(host: &Host, account: AccountId, salt: [u8; 32]) -> (Hash, ContractIdPreimage) {
    get_contract_id_from_address(host, ScAddress::Account(account), salt)
}

// Create a contract using a v1 or v2 host function.
// The error is returned only for the host function invocation, so it's safe
// to write tests that want the host function to fail. All the setup calls
// panic on error instead.
fn create_contract_with_constructor(
    host: &Host,
    source_account: AccountId,
    salt: [u8; 32],
    wasm: &[u8],
    constructor_args: &Vec<ScVal>,
    params: CreateContractTestParams,
) -> Result<Hash, HostError> {
    host.set_source_account(source_account.clone()).unwrap();
    let (contract_id, contract_id_preimage) = get_contract_id(&host, source_account, salt);

    let wasm_hash = Hash(Sha256::digest(wasm).try_into().unwrap());

    // Prepare RW footprint.
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

    // Upload Wasm
    let uploaded_wasm_hash: Val = host
        .invoke_function(HostFunction::UploadContractWasm(wasm.try_into().unwrap()))
        .unwrap()
        .try_into_val(host)
        .unwrap();
    let uploaded_wasm_hash = host
        .hash_from_bytesobj_input("wasm_hash", uploaded_wasm_hash.try_into().unwrap())
        .unwrap();

    // Create contract
    // Check that the test is not misconfigured - we can't pass constructor args
    // into V1 XDR.
    assert!(
        constructor_args.is_empty()
            || (!params.use_create_contract_args_v1 && !params.use_create_contract_args_v1)
    );
    let host_fn =
        params.create_host_fn(&contract_id_preimage, &uploaded_wasm_hash, constructor_args);
    let auth_invocation =
        params.create_auth_invocation(&contract_id_preimage, &uploaded_wasm_hash, constructor_args);

    if !params.skip_auth {
        host.set_authorization_entries(vec![SorobanAuthorizationEntry {
            credentials: SorobanCredentials::SourceAccount,
            root_invocation: auth_invocation,
        }])
        .unwrap();
    }
    let created_id_sc_val = host.invoke_function(host_fn)?;

    assert_eq!(
        ScVal::Address(ScAddress::Contract(contract_id.clone())),
        created_id_sc_val
    );
    assert_eq!(
        uploaded_wasm_hash.as_slice(),
        get_contract_wasm_ref(&host, contract_id.clone()).as_slice()
    );
    assert_eq!(wasm, get_contract_wasm(&host, wasm_hash));

    Ok(contract_id)
}

fn create_contract_from_source_account(host: &Host, wasm: &[u8]) -> Hash {
    create_contract_with_constructor(
        host,
        generate_account_id(host),
        generate_bytes_array(host),
        wasm,
        &vec![],
        Default::default(),
    )
    .unwrap()
}

// VM tests
#[test]
fn create_contract_using_parent_id_test() {
    let host = test_host();
    let parent_contract_id = create_contract_from_source_account(&host, CREATE_CONTRACT);
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
    let child_wasm = ADD_I32;

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
fn test_create_contract_from_source_account() {
    create_contract_from_source_account(&test_host(), &ADD_I32);
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
    let _ = host
        .invoke_function(HostFunction::CreateContract(CreateContractArgs {
            contract_id_preimage: contract_id_preimage.clone(),
            executable: ContractExecutable::Wasm(created_wasm_hash.clone()),
        }))
        .unwrap();

    assert_eq!(
        host.get_recorded_auth_payloads().unwrap(),
        vec![RecordedAuthPayload {
            address: None,
            nonce: None,
            invocation: SorobanAuthorizedInvocation {
                function: SorobanAuthorizedFunction::CreateContractV2HostFn(CreateContractArgsV2 {
                    contract_id_preimage,
                    executable: ContractExecutable::Wasm(created_wasm_hash.clone()),
                    constructor_args: Default::default(),
                }),
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

mod cap_54_55_56 {
    use super::*;
    use more_asserts::assert_gt;
    use pretty_assertions::{assert_eq, assert_ne};
    use soroban_test_wasms::UPLOAD_CONTRACT;

    use crate::{
        crypto::sha256_hash_from_bytes,
        storage::{FootprintMap, StorageMap},
        test::observe::ObservedHost,
        testutils::wasm::wasm_module_with_a_bit_of_everything,
        xdr::{
            ContractCostType::{self, *},
            LedgerEntry, LedgerKey,
        },
        AddressObject, HostError, SymbolSmall,
    };
    use std::rc::Rc;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TestContractCostModelMode {
        OldContractWithNoCostInputs,
        NewContractWithCostInputs,
    }
    use TestContractCostModelMode::*;

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

    fn is_instantiation_cost(ct: ContractCostType) -> bool {
        match ct {
            InstantiateWasmInstructions
            | InstantiateWasmFunctions
            | InstantiateWasmGlobals
            | InstantiateWasmTableEntries
            | InstantiateWasmTypes
            | InstantiateWasmDataSegments
            | InstantiateWasmElemSegments
            | InstantiateWasmImports
            | InstantiateWasmExports
            | InstantiateWasmDataSegmentBytes => true,
            _ => false,
        }
    }

    fn new_host_with_uploaded_contract(
        hostname: &'static str,
    ) -> Result<(ObservedHost, AddressObject), HostError> {
        let host = Host::test_host_with_recording_footprint();
        host.enable_debug()?;
        let host = ObservedHost::new(hostname, host);
        let contract_addr_obj = host.register_test_contract_wasm(
            &wasm_module_with_a_bit_of_everything(host.get_ledger_protocol_version()?),
        );
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
                let contract_entry = storage.get_with_host(&self.contract_key, host, None)?;
                let wasm_entry = storage.get_with_host(&self.wasm_key, host, None)?;
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
                let contract_entry = storage.get_with_host(&contract_key, host, None)?;
                let wasm_entry = storage.get_with_host(&wasm_key, host, None)?;
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
        contract_cost_model_mode: TestContractCostModelMode,
    ) -> Result<ContractAndWasmEntries, HostError> {
        let (host, contract) = new_host_with_uploaded_contract(upload_hostname)?;
        if contract_cost_model_mode == OldContractWithNoCostInputs {
            clobber_refined_cost_model(&host, host.contract_id_from_address(contract)?.clone())?;
        }
        ContractAndWasmEntries::from_contract_addr(&host, contract)
    }

    fn observed_test_host_with_storage_and_budget(
        hostname: &'static str,
        storage: Storage,
        budget: Budget,
    ) -> Result<ObservedHost, HostError> {
        let host = Host::with_storage_and_budget(storage, budget);
        host.enable_debug()?;
        host.set_test_ledger_info_with_current_test_protocol();
        let host = ObservedHost::new(hostname, host);
        Ok(host)
    }

    fn upload_and_make_host_for_next_ledger(
        upload_hostname: &'static str,
        second_hostname: &'static str,
        contract_cost_model_mode: TestContractCostModelMode,
    ) -> Result<(ObservedHost, Hash), HostError> {
        // Phase 1: upload contract, tear down host, "close the ledger" and possibly change protocol.
        let (host, contract) = new_host_with_uploaded_contract(upload_hostname)?;
        let contract_id = host.contract_id_from_address(contract)?;
        if contract_cost_model_mode == OldContractWithNoCostInputs {
            clobber_refined_cost_model(&host, contract_id.clone())?;
        }
        let realhost = host.clone();
        drop(host);
        let (storage, _events) = realhost.try_finish()?;
        let storage = Storage::with_enforcing_footprint_and_map(storage.footprint, storage.map);

        // Phase 2: build new host with previous ledger output as storage. Possibly on new protocol.
        let host = observed_test_host_with_storage_and_budget(
            second_hostname,
            storage,
            Budget::default(),
        )?;
        Ok((host, contract_id))
    }

    fn clobber_refined_cost_model(host: &Host, contract_id: Hash) -> Result<(), HostError> {
        let contract_key = host.contract_instance_ledger_key(&contract_id)?;
        let ContractExecutable::Wasm(wasm_hash) = host
            .retrieve_contract_instance_from_storage(&contract_key)?
            .executable
        else {
            panic!("expected Wasm executable");
        };
        let code_key = Rc::new(LedgerKey::ContractCode(xdr::LedgerKeyContractCode {
            hash: wasm_hash,
        }));
        let mut storage = host.try_borrow_storage_mut()?;
        let (entry, live_until_ledger) =
            storage.get_with_live_until_ledger(&code_key, host, None)?;
        let LedgerEntryData::ContractCode(code) = &entry.data else {
            panic!("expected ContractCode");
        };
        let new_entry = Rc::new(LedgerEntry {
            data: LedgerEntryData::ContractCode(xdr::ContractCodeEntry {
                ext: xdr::ContractCodeEntryExt::V0,
                ..code.clone()
            }),
            ..(*entry).clone()
        });
        storage.put(&code_key, &new_entry, live_until_ledger, host.as_budget())?;
        Ok(())
    }

    fn upload_and_call(
        upload_hostname: &'static str,
        call_hostname: &'static str,
        contract_cost_model_mode: TestContractCostModelMode,
    ) -> Result<(Budget, Storage), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            upload_hostname,
            call_hostname,
            contract_cost_model_mode,
        )?;
        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;
        let _ = host.call(
            contract,
            Symbol::try_from_small_str("test")?,
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

    // region: CAP-0054 refined cost model

    // Test that running on protocol vNew on a ContractCode LE that does not have
    // ContractCodeCostInputs charges the VmInstantiation and VmCachedInstantiation
    // cost types.
    #[test]
    fn test_v_new_no_contract_code_cost_inputs() -> Result<(), HostError> {
        let (budget, _storage) = upload_and_call(
            "test_v_new_no_contract_code_cost_inputs_upload",
            "test_v_new_no_contract_code_cost_inputs_call",
            OldContractWithNoCostInputs,
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
            NewContractWithCostInputs,
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
            "test_v_new_with_contract_code_cost_inputs_causes_nonzero_costs_call",
            NewContractWithCostInputs,
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

    // Test that running on protocol vNew does rewrite a ContractCode LE when it
    // already exists but doesn't yet have ContractCodeCostInputs.
    #[test]
    fn test_v_new_rewrite() -> Result<(), HostError> {
        let entries = upload_and_get_contract_and_wasm_entries(
            "test_v_new_rewrite_upload",
            OldContractWithNoCostInputs,
        )?;
        assert!(!code_entry_has_cost_inputs(&entries.wasm_entry));

        // make a new storage map for a new upload but with read-only footprint -- this should fail
        let budget = Budget::default();
        let storage = entries.read_only_storage(&budget);
        let host = observed_test_host_with_storage_and_budget(
            "test_v_new_rewrite_call_fail",
            storage,
            budget,
        )?;
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
        let host = observed_test_host_with_storage_and_budget(
            "test_v_new_rewrite_call_succeed",
            storage,
            budget,
        )?;
        host.upload_contract_wasm(wasm_blob)?;
        let entries = entries.reload(&host)?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        Ok(())
    }

    // Test that running on protocol vNew does not rewrite a ContractCode LE when it
    // already exists and already has ContractCodeCostInputs.
    #[test]
    fn test_v_new_no_rewrite() -> Result<(), HostError> {
        let entries = upload_and_get_contract_and_wasm_entries(
            "test_v_new_no_rewrite_upload",
            NewContractWithCostInputs,
        )?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        // make a new storage map for a new upload but with read-only footprint -- this should pass
        let budget = Budget::default();
        let storage = entries.read_only_storage(&budget);
        let host = observed_test_host_with_storage_and_budget(
            "test_v_new_no_rewrite_call_pass",
            storage,
            budget,
        )?;
        let wasm_blob = match &entries.wasm_entry.data {
            LedgerEntryData::ContractCode(cce) => cce.code.to_vec(),
            _ => panic!("expected ContractCode"),
        };
        host.upload_contract_wasm(wasm_blob.clone())?;
        let entries = entries.reload(&host)?;
        assert!(code_entry_has_cost_inputs(&entries.wasm_entry));

        Ok(())
    }
    // endregion: CAP-0054 refined cost model

    // region: CAP-0056 ModuleCache related tests

    // Test that running on protocol vNew does add ModuleCache entries.
    #[test]
    fn test_v_new_module_cache() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_module_cache_upload",
            "test_v_new_module_cache_check",
            OldContractWithNoCostInputs,
        )?;
        // force a module-cache build (this normally happens on first VM call)
        host.build_module_cache_if_needed()?;
        let wasm = get_contract_wasm_ref(&host, contract_id);
        let module_cache = host.try_borrow_module_cache()?;
        if let Some(module_cache) = &*module_cache {
            assert!(module_cache.get_module(&host, &wasm).is_ok());
        } else {
            panic!("expected module cache");
        }
        Ok(())
    }

    // Test that, when running on protocol vNew instantiating a contract without
    // ContractCodeCostInputs, repeated invocations of the same contract
    // increase the VmCachedInstantiation costs but do not increase the
    // VmInstantiation costs.
    #[test]
    fn test_v_new_no_contract_code_cost_inputs_cached_instantiation() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_no_contract_code_cost_inputs_cached_instantiation_upload",
            "test_v_new_no_contract_code_cost_inputs_cached_instantiation_call",
            OldContractWithNoCostInputs,
        )?;

        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;
        let test_symbol = Symbol::try_from_small_str("test")?;
        let args = host.vec_new()?;
        let _ = host.call(contract, test_symbol, args)?;

        let budget = host.budget_cloned();

        // Double check we're not charging the new cost types
        for ct in NEW_COST_TYPES {
            assert_eq!(budget.get_tracker(*ct)?.cpu, 0);
        }

        // Check that we're charging both the old cost types
        let first_call_vm_instantiation = budget.get_tracker(VmInstantiation)?.cpu;
        let first_call_vm_cached_instantiation = budget.get_tracker(VmCachedInstantiation)?.cpu;

        assert_ne!(first_call_vm_instantiation, 0);
        assert_ne!(first_call_vm_cached_instantiation, 0);

        // Do a second call and check that it only increased the cached cost type.
        let _ = host.call(contract, test_symbol, args)?;

        assert_eq!(
            budget.get_tracker(VmInstantiation)?.cpu,
            first_call_vm_instantiation
        );
        assert_gt!(
            budget.get_tracker(VmCachedInstantiation)?.cpu,
            first_call_vm_cached_instantiation
        );

        Ok(())
    }

    // Test that, when running on protocol vNew instantiating a contract with
    // ContractCodeCostInputs, repeated invocations of the same contract
    // increase the new refined cost model InstantiateWasm* cost types but
    // do not increase the ParseWasm* cost types.

    #[test]
    fn test_v_new_with_contract_code_cost_inputs_cached_instantiation() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_with_contract_code_cost_inputs_cached_instantiation_upload",
            "test_v_new_with_contract_code_cost_inputs_cached_instantiation_call",
            NewContractWithCostInputs,
        )?;

        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;
        let test_symbol = Symbol::try_from_small_str("test")?;
        let args = host.vec_new()?;
        let _ = host.call(contract, test_symbol, args)?;

        let budget = host.budget_cloned();

        // Check that we're not charging the old cost types
        assert_eq!(budget.get_tracker(VmInstantiation)?.cpu, 0);
        assert_eq!(budget.get_tracker(VmCachedInstantiation)?.cpu, 0);

        let mut first_costs = Vec::new();

        for ct in NEW_COST_TYPES {
            let cost = budget.get_tracker(*ct)?.cpu;
            first_costs.push(cost);
            if *ct == InstantiateWasmTypes {
                // This is a zero-cost type in the current calibration of the
                // new model -- and exceptional case in this test -- though we
                // keep it in case it becomes nonzero at some point (it's
                // credible that it would).
                continue;
            }
            assert_ne!(cost, 0);
        }

        // Do a second call and check that it only increased the cached cost type.
        let _ = host.call(contract, test_symbol, args)?;

        for (ct, first_cost) in NEW_COST_TYPES.iter().zip(first_costs.iter()) {
            if *ct == InstantiateWasmTypes {
                continue;
            }
            let cost = budget.get_tracker(*ct)?.cpu;
            if is_instantiation_cost(*ct) {
                assert_gt!(cost, *first_cost);
            } else {
                assert_eq!(cost, *first_cost);
            }
        }

        Ok(())
    }

    // Test that, when running on protocol vNew and calling a wasm that is not
    // in the footprint or storage, we just get a storage error, not any kind of
    // internal error. This is a legitimate (if erroneous) way to invoke the host
    // from outside and we should fail gracefully.
    #[test]
    fn test_v_new_call_nonexistent_wasm() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_call_nonexistent_wasm_upload",
            "test_v_new_call_nonexistent_wasm_call",
            NewContractWithCostInputs,
        )?;
        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;

        // Remove the wasm from the storage and footprint.
        let wasm_to_delete =
            wasm_module_with_a_bit_of_everything(host.get_ledger_protocol_version()?);
        let wasm_hash = Hash(
            sha256_hash_from_bytes(&wasm_to_delete, host.as_budget())?
                .try_into()
                .unwrap(),
        );
        let wasm_key = host.contract_code_ledger_key(&wasm_hash)?;
        host.with_mut_storage(|storage| {
            let budget = host.budget_cloned();
            storage.footprint.0 = storage
                .footprint
                .0
                .remove::<Rc<LedgerKey>>(&wasm_key, &budget)?
                .unwrap()
                .0;
            storage.map = storage
                .map
                .remove::<Rc<LedgerKey>>(&wasm_key, &budget)?
                .unwrap()
                .0;
            Ok(())
        })?;

        // Cache is built here, wasm is not present by time cache is built so it fails.
        let test_symbol = Symbol::try_from_small_str("test")?;
        let args = host.vec_new()?;
        let res = host.call(contract, test_symbol, args);
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Storage, ScErrorCode::ExceededLimit)
        ));
        Ok(())
    }

    // Test that, when running on protocol vNew and calling a wasm that is in
    // the footprint but not in storage, we get a storage error as well (though
    // a different one: ScErrorCode::MissingValue). This is a minor variant of
    // the `test_v_new_call_nonexistent_wasm` test above.
    #[test]
    fn test_v_new_call_wasm_in_footprint_but_not_storage() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_call_wasm_in_footprint_but_not_storage_upload",
            "test_v_new_call_wasm_in_footprint_but_not_storage_call",
            NewContractWithCostInputs,
        )?;
        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;

        // Remove the wasm from storage by setting its value to `None`.
        let wasm_to_delete =
            wasm_module_with_a_bit_of_everything(host.get_ledger_protocol_version()?);
        let wasm_hash = Hash(
            sha256_hash_from_bytes(&wasm_to_delete, host.as_budget())?
                .try_into()
                .unwrap(),
        );
        let wasm_key = host.contract_code_ledger_key(&wasm_hash)?;
        host.with_mut_storage(|storage| {
            let budget = host.budget_cloned();
            storage.map = storage.map.insert(wasm_key, None, &budget)?;
            Ok(())
        })?;

        // Cache is built here, wasm is not present by time cache is built so it fails.
        let test_symbol = Symbol::try_from_small_str("test")?;
        let args = host.vec_new()?;
        let res = host.call(contract, test_symbol, args);
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Storage, ScErrorCode::MissingValue)
        ));
        Ok(())
    }

    // Test that, when running on protocol vNew and calling a wasm that
    // initially _is_ in the footprint or storage, but is _deleted_ during
    // execution, we then get a storage error. This is a minor variant of the
    // `test_v_new_call_nonexistent_wasm` test above, and currently represents a
    // scenario that can't happen, but it might in the future and it's nice to
    // keep the cache as close to "reflecting storage" as possible.
    #[test]
    fn test_v_new_call_runtime_deleted_wasm() -> Result<(), HostError> {
        let (host, contract_id) = upload_and_make_host_for_next_ledger(
            "test_v_new_call_runtime_deleted_wasm_upload",
            "test_v_new_call_runtime_deleted_wasm_call",
            NewContractWithCostInputs,
        )?;
        let contract = host.add_host_object(crate::xdr::ScAddress::Contract(contract_id))?;

        // Cache is built here, wasm is present, so call succeeds.
        let test_symbol = Symbol::try_from_small_str("test")?;
        let args = host.vec_new()?;
        let _ = host.call(contract, test_symbol, args)?;

        // Remove the wasm from the storage and footprint.
        let wasm_to_delete =
            wasm_module_with_a_bit_of_everything(host.get_ledger_protocol_version()?);
        let wasm_hash = Hash(
            sha256_hash_from_bytes(&wasm_to_delete, host.as_budget())?
                .try_into()
                .unwrap(),
        );
        let wasm_key = host.contract_code_ledger_key(&wasm_hash)?;
        host.with_mut_storage(|storage| {
            let budget = host.budget_cloned();
            storage.footprint.0 = storage
                .footprint
                .0
                .remove::<Rc<LedgerKey>>(&wasm_key, &budget)?
                .unwrap()
                .0;
            storage.map = storage
                .map
                .remove::<Rc<LedgerKey>>(&wasm_key, &budget)?
                .unwrap()
                .0;
            Ok(())
        })?;

        // Cache contains the wasm but storage doesn't, so we should fail.
        let res = host.call(contract, test_symbol, args);
        assert!(HostError::result_matches_err(
            res,
            (ScErrorType::Storage, ScErrorCode::ExceededLimit)
        ));
        Ok(())
    }

    // Test that, when running on protocol vNew and uploading a contract
    // mid-transaction using a host function, one can call the contract even
    // though it won't reside in the module cache because the cache is frozen on
    // first access.
    #[test]
    fn test_v_new_update_contract_with_module_cache() -> Result<(), HostError> {
        let host = Host::test_host_with_recording_footprint();
        host.enable_debug()?;
        let host = ObservedHost::new("test_v_new_update_contract_with_module_cache", host);
        let upload_contract_addr_obj = host.register_test_contract_wasm(UPLOAD_CONTRACT);
        let updateable_contract_addr_obj = host.register_test_contract_wasm(UPDATEABLE_CONTRACT);

        // Prep the footprint and storage map for accepting an upload, the way
        // we would if we'd had a transaction declare a write-only footprint
        // entry for the wasm key.
        let wasm_to_upload =
            wasm_module_with_a_bit_of_everything(host.get_ledger_protocol_version()?);
        let wasm_hash = Hash(
            sha256_hash_from_bytes(&wasm_to_upload, host.as_budget())?
                .try_into()
                .unwrap(),
        );
        let wasm_code_key = host.contract_code_ledger_key(&wasm_hash)?;
        host.with_mut_storage(|storage| {
            storage.footprint.record_access(
                &wasm_code_key,
                AccessType::ReadWrite,
                host.as_budget(),
            )?;
            storage.map = storage.map.insert(wasm_code_key, None, host.as_budget())?;
            Ok(())
        })?;

        host.switch_to_enforcing_storage()?;

        let wasm_bytes = host.bytes_new_from_slice(&wasm_to_upload)?;
        let upload_args = host.vec_new_from_slice(&[wasm_bytes.to_val()])?;

        // Module cache will be built _before_ this call is dispatched, and call
        // will attempt to add a wasm to storage that is not in the cache.
        let wasm_hash_val = host.call(
            upload_contract_addr_obj,
            Symbol::try_from_small_str("upload")?,
            upload_args,
        )?;

        // Now we update the updatable contract to point to it and
        // invoke it, which should actually work, just not go through
        // the module cache.
        let update_args = host
            .vec_new_from_slice(&[wasm_hash_val, /*fail:*/ Val::from_bool(false).to_val()])?;
        let _ = host.call(
            updateable_contract_addr_obj,
            Symbol::try_from_small_str("update")?,
            update_args,
        )?;

        // Check that we have charged nonzero new-style wasm parsing costs.
        let budget = host.budget_cloned();
        let pre_parse_cost = budget.get_tracker(ParseWasmInstructions)?.cpu;
        assert_ne!(pre_parse_cost, 0);

        // Check that the module cache did not get populated with the new wasm.
        if let Some(module_cache) = &*host.try_borrow_module_cache()? {
            assert!(module_cache.get_module(&host, &wasm_hash)?.is_none());
        } else {
            panic!("expected module cache");
        }

        let updated_args = host.vec_new()?;
        let res = host.call(
            updateable_contract_addr_obj,
            Symbol::try_from_small_str("test")?,
            updated_args,
        )?;
        assert_eq!(SymbolSmall::try_from(res)?.to_string(), "pass");

        // Check that the call to the updated wasm did more parsing, i.e. did
        // not have a cache hit.
        let post_parse_cost = budget.get_tracker(ParseWasmInstructions)?.cpu;
        assert_ne!(post_parse_cost, pre_parse_cost);

        Ok(())
    }

    // endregion: CAP-0056 ModuleCache related tests
}

mod cap_58_constructor {
    use super::*;
    use crate::xdr::InvokeContractArgs;

    use soroban_test_wasms::{
        ADD_I32_P22, AUTH_TEST_CONTRACT, CONSTRUCTOR_TEST_CONTRACT_P21,
        CONSTRUCTOR_TEST_CONTRACT_P22, DEPLOYER_WITH_CONSTRUCTOR,
        NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21, NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P22,
    };

    const DEPLOY_FUNCTIONS_WITH_NO_ARGUMENT_CONSTRUCTOR: [&str; 2] =
        ["deploy_no_constructor", "deploy_no_constructor_args"];

    fn test_host() -> Host {
        let host = Host::test_host_with_recording_footprint();
        host.enable_debug().unwrap();
        host
    }

    fn create_contract_with_instantiated_deployer(
        host: &Host,
        wasm: &[u8],
        deployer_contract_address: ScAddress,
        deploy_function: &str,
        deploy_function_args: Vec<ScVal>,
    ) -> Result<ScAddress, HostError> {
        let wasm_hash = host.upload_contract_wasm(wasm.to_vec()).unwrap();
        let wasm_hash_val: ScVal = host.from_host_val(wasm_hash.into()).unwrap();
        let mut args = deploy_function_args;
        args.insert(0, wasm_hash_val);
        let host_fn = HostFunction::InvokeContract(InvokeContractArgs {
            contract_address: deployer_contract_address,
            function_name: deploy_function.try_into().unwrap(),
            args: args.try_into().unwrap(),
        });
        let output_address = host.invoke_function(host_fn)?;
        match output_address {
            ScVal::Address(addr) => Ok(addr),
            _ => unreachable!(),
        }
    }

    fn create_contract_with_deployer(
        host: &Host,
        wasm: &[u8],
        deploy_function: &str,
        deploy_function_args: Vec<ScVal>,
    ) -> Result<ScAddress, HostError> {
        let deployer_contract = host.register_test_contract_wasm(DEPLOYER_WITH_CONSTRUCTOR);
        let deployer_contract_address = host.scaddress_from_address(deployer_contract).unwrap();
        create_contract_with_instantiated_deployer(
            host,
            wasm,
            deployer_contract_address,
            deploy_function,
            deploy_function_args,
        )
    }

    fn create_contract_error_test(
        host: &Host,
        wasm: &[u8],
        constructor_args: Vec<ScVal>,
        params: CreateContractTestParams,
        error_type: ScErrorType,
        error_code: ScErrorCode,
    ) {
        let err = create_contract_with_constructor(
            &host,
            generate_account_id(&host),
            generate_bytes_array(&host),
            wasm,
            &constructor_args,
            params,
        )
        .err()
        .unwrap();
        dbg!(&err);
        assert!(err.error.is_type(error_type));
        assert!(err.error.is_code(error_code));
    }

    fn create_contract_with_deployer_error_test(
        host: &Host,
        wasm: &[u8],
        deploy_function: &str,
        deploy_function_args: Vec<ScVal>,
        error_type: ScErrorType,
        error_code: ScErrorCode,
    ) {
        let err = create_contract_with_deployer(&host, wasm, deploy_function, deploy_function_args)
            .err()
            .unwrap();
        assert!(err.error.is_type(error_type));
        assert!(err.error.is_code(error_code));
    }

    mod old_contracts {
        use soroban_test_wasms::CONSTRUCTOR_WITH_RETURN_VALUE_P21;

        use super::*;
        #[test]
        fn test_constructor_function_is_ignored() {
            for wasm in [
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
                CONSTRUCTOR_TEST_CONTRACT_P21,
            ] {
                for use_create_contract_args_v1 in [false, true] {
                    for use_auth_invocation_v1 in [false, true] {
                        let host = test_host();
                        let contract_id = create_contract_with_constructor(
                            &host,
                            generate_account_id(&host),
                            generate_bytes_array(&host),
                            wasm,
                            &vec![],
                            CreateContractTestParams {
                                use_create_contract_args_v1,
                                use_auth_invocation_v1,
                                ..Default::default()
                            },
                        )
                        .unwrap();
                        let err = host
                            .call(
                                host.add_host_object(ScAddress::Contract(contract_id))
                                    .unwrap(),
                                Symbol::try_from_small_str("get_data").unwrap(),
                                test_vec![&host, Symbol::try_from_small_str("key").unwrap()]
                                    .as_object(),
                            )
                            .err()
                            .unwrap();
                        // SDK produces panic when non-existent value is accessed.
                        assert!(err.error.is_type(ScErrorType::WasmVm));
                        assert!(err.error.is_code(ScErrorCode::InvalidAction));
                    }
                }
            }
        }

        #[test]
        fn test_constructor_with_return_value_is_ignored() {
            for use_create_contract_args_v1 in [false, true] {
                for use_auth_invocation_v1 in [false, true] {
                    let host = test_host();
                    let res = create_contract_with_constructor(
                        &host,
                        generate_account_id(&host),
                        generate_bytes_array(&host),
                        CONSTRUCTOR_WITH_RETURN_VALUE_P21,
                        &vec![],
                        CreateContractTestParams {
                            use_create_contract_args_v1,
                            use_auth_invocation_v1,
                            ..Default::default()
                        },
                    );
                    assert!(res.is_ok());
                }
            }
        }

        #[test]
        fn test_constructor_function_is_ignored_with_deployer() {
            for wasm in [
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
                CONSTRUCTOR_TEST_CONTRACT_P21,
            ] {
                for deploy_fn in DEPLOY_FUNCTIONS_WITH_NO_ARGUMENT_CONSTRUCTOR {
                    let host = test_host();
                    let contract_address =
                        create_contract_with_deployer(&host, wasm, deploy_fn, vec![]).unwrap();
                    let err = host
                        .call(
                            host.add_host_object(contract_address).unwrap(),
                            Symbol::try_from_small_str("get_data").unwrap(),
                            test_vec![&host, Symbol::try_from_small_str("key").unwrap()]
                                .as_object(),
                        )
                        .err()
                        .unwrap();
                    // SDK produces panic when non-existent value is accessed.
                    assert!(err.error.is_type(ScErrorType::WasmVm));
                    assert!(err.error.is_code(ScErrorCode::InvalidAction));
                }
            }
        }

        #[test]
        fn test_passing_constructor_arguments_not_allowed() {
            let host = test_host();
            // Make sure auth is not an issue.
            host.switch_to_recording_auth(false).unwrap();
            create_contract_error_test(
                &host,
                CONSTRUCTOR_TEST_CONTRACT_P21,
                vec![
                    ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                    ScVal::Symbol("key".try_into().unwrap()),
                    ScVal::U32(100),
                    ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                ],
                Default::default(),
                // Old contracts are always treated as if they don't have an explicit
                // constructor defined, so we don't get to VM and exit early.
                ScErrorType::Context,
                ScErrorCode::InvalidAction,
            );
        }

        #[test]
        fn test_passing_constructor_arguments_not_allowed_with_deployer() {
            let host = test_host();
            // Make sure auth is not an issue.
            host.switch_to_recording_auth(false).unwrap();
            create_contract_with_deployer_error_test(
                &host,
                CONSTRUCTOR_TEST_CONTRACT_P21,
                "deploy_with_external_auth",
                vec![
                    ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                    ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                ],
                // We should have host fn failed, which always gets mapped to
                // the context/invalid action error.
                ScErrorType::Context,
                ScErrorCode::InvalidAction,
            );
        }
    }

    mod new_contracts {
        use soroban_test_wasms::{CONSTRUCTOR_WITH_RESULT, CONSTRUCTOR_WITH_RETURN_VALUE_P22};

        use super::*;

        mod constructor_supported {
            use crate::e2e_testutils::get_wasm_hash;

            use super::*;
            use pretty_assertions::assert_eq;

            #[test]
            fn test_default_constructor() {
                for use_create_contract_args_v1 in [false, true] {
                    for use_auth_invocation_v1 in [false, true] {
                        let host = test_host();
                        let res = create_contract_with_constructor(
                            &host,
                            generate_account_id(&host),
                            generate_bytes_array(&host),
                            ADD_I32_P22,
                            &vec![],
                            CreateContractTestParams {
                                use_create_contract_args_v1,
                                use_auth_invocation_v1,
                                ..Default::default()
                            },
                        );
                        assert!(res.is_ok());
                    }
                }
            }

            #[test]
            fn test_default_constructor_with_deployer() {
                for deploy_fn in DEPLOY_FUNCTIONS_WITH_NO_ARGUMENT_CONSTRUCTOR {
                    let host = test_host();
                    let res = create_contract_with_deployer(&host, ADD_I32_P22, deploy_fn, vec![]);
                    assert!(res.is_ok());
                }
            }

            #[test]
            fn test_constructor_without_arguments() {
                for use_create_contract_args_v1 in [false, true] {
                    for use_auth_invocation_v1 in [false, true] {
                        let host = test_host();
                        let contract_id = create_contract_with_constructor(
                            &host,
                            generate_account_id(&host),
                            generate_bytes_array(&host),
                            NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P22,
                            &vec![],
                            CreateContractTestParams {
                                use_create_contract_args_v1,
                                use_auth_invocation_v1,
                                ..Default::default()
                            },
                        )
                        .unwrap();
                        let res: u32 = host
                            .call(
                                host.add_host_object(ScAddress::Contract(contract_id))
                                    .unwrap(),
                                Symbol::try_from_small_str("get_data").unwrap(),
                                test_vec![&host, Symbol::try_from_small_str("key").unwrap()]
                                    .as_object(),
                            )
                            .unwrap()
                            .try_into_val(&host)
                            .unwrap();
                        assert_eq!(res, 6);
                    }
                }
            }

            #[test]
            fn test_constructor_without_arguments_with_deployer() {
                for deploy_fn in DEPLOY_FUNCTIONS_WITH_NO_ARGUMENT_CONSTRUCTOR {
                    let host = test_host();
                    let contract_address = create_contract_with_deployer(
                        &host,
                        NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P22,
                        deploy_fn,
                        vec![],
                    )
                    .unwrap();
                    let res: u32 = host
                        .call(
                            host.add_host_object(contract_address).unwrap(),
                            Symbol::try_from_small_str("get_data").unwrap(),
                            test_vec![&host, Symbol::try_from_small_str("key").unwrap()]
                                .as_object(),
                        )
                        .unwrap()
                        .try_into_val(&host)
                        .unwrap();
                    assert_eq!(res, 6);
                }
            }

            #[test]
            fn test_constructor_with_arguments_and_auth() {
                let host = test_host();
                let auth_contract = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);
                let auth_contract_val: ScVal = auth_contract.as_val().try_into_val(&host).unwrap();
                let auth_contract_addr = host.scaddress_from_address(auth_contract).unwrap();

                let source_account = generate_account_id(&host);
                let salt = generate_bytes_array(&host);
                let (new_contract_id, _) =
                    get_contract_id(&host, source_account.clone(), salt.clone());
                let source_account_val = ScVal::Address(ScAddress::Account(source_account.clone()));
                let args_vec = vec![
                    source_account_val.clone(),
                    ScVal::Symbol("key".try_into().unwrap()),
                    ScVal::U32(100),
                    auth_contract_val,
                ];
                let params = CreateContractTestParams {
                    sub_invocation_auth: vec![SorobanAuthorizedInvocation {
                        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                            contract_address: ScAddress::Contract(new_contract_id),
                            function_name: "__constructor".try_into().unwrap(),
                            args: args_vec.clone().try_into().unwrap(),
                        }),
                        sub_invocations: vec![SorobanAuthorizedInvocation {
                            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                                contract_address: auth_contract_addr,
                                function_name: "do_auth".try_into().unwrap(),
                                args: vec![source_account_val, ScVal::U32(100)]
                                    .try_into()
                                    .unwrap(),
                            }),
                            sub_invocations: Default::default(),
                        }]
                        .try_into()
                        .unwrap(),
                    }],
                    ..Default::default()
                };

                let contract_id = create_contract_with_constructor(
                    &host,
                    source_account,
                    salt,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    &args_vec,
                    params,
                )
                .unwrap();
                let res: u32 = host
                    .call(
                        host.add_host_object(ScAddress::Contract(contract_id))
                            .unwrap(),
                        Symbol::try_from_small_str("get_data").unwrap(),
                        test_vec![&host, Symbol::try_from_small_str("key").unwrap()].as_object(),
                    )
                    .unwrap()
                    .try_into_val(&host)
                    .unwrap();
                assert_eq!(res, 303);
            }

            #[test]
            fn test_constructor_with_arguments_and_auth_with_deployer() {
                let host = test_host();
                let auth_contract = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);
                let auth_contract_val: ScVal = auth_contract.as_val().try_into_val(&host).unwrap();
                let auth_contract_addr = host.scaddress_from_address(auth_contract).unwrap();

                let source_account = generate_account_id(&host);
                host.set_source_account(source_account.clone()).unwrap();

                let salt = [4u8; 32];
                let (_, contract_id_preimage) =
                    get_contract_id(&host, source_account.clone(), salt.clone());

                let wasm_hash = get_wasm_hash(CONSTRUCTOR_TEST_CONTRACT_P22);
                let deployer_contract = host.register_test_contract_wasm(DEPLOYER_WITH_CONSTRUCTOR);
                let deployer_contract_address =
                    host.scaddress_from_address(deployer_contract).unwrap();

                let constructor_args = vec![
                    ScVal::Address(deployer_contract_address.clone()),
                    ScVal::Symbol("key".try_into().unwrap()),
                    ScVal::U32(100),
                    auth_contract_val,
                ];

                let auth_invocation = SorobanAuthorizedInvocation {
                    function: SorobanAuthorizedFunction::CreateContractV2HostFn(
                        CreateContractArgsV2 {
                            contract_id_preimage,
                            executable: ContractExecutable::Wasm(wasm_hash.try_into().unwrap()),
                            constructor_args: constructor_args.clone().try_into().unwrap(),
                        },
                    ),
                    sub_invocations: Default::default(),
                };

                host.set_authorization_entries(vec![SorobanAuthorizationEntry {
                    credentials: SorobanCredentials::SourceAccount,
                    root_invocation: auth_invocation,
                }])
                .unwrap();
                let contract_address = create_contract_with_instantiated_deployer(
                    &host,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    deployer_contract_address,
                    "deploy_with_external_auth",
                    vec![
                        ScVal::Address(ScAddress::Account(source_account.clone())),
                        ScVal::Address(auth_contract_addr),
                    ],
                )
                .unwrap();
                let res: u32 = host
                    .call(
                        host.add_host_object(contract_address).unwrap(),
                        Symbol::try_from_small_str("get_data").unwrap(),
                        test_vec![&host, Symbol::try_from_small_str("key").unwrap()].as_object(),
                    )
                    .unwrap()
                    .try_into_val(&host)
                    .unwrap();
                assert_eq!(res, 303);
            }

            #[test]
            fn test_constructor_with_arguments_and_invoker_auth_with_deployer() {
                let host = test_host();
                let auth_contract = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);
                let auth_contract_addr = host.scaddress_from_address(auth_contract).unwrap();

                let contract_address = create_contract_with_deployer(
                    &host,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    "deploy_with_contract_auth",
                    vec![ScVal::Address(auth_contract_addr)],
                )
                .unwrap();
                let res: u32 = host
                    .call(
                        host.add_host_object(contract_address).unwrap(),
                        Symbol::try_from_small_str("get_data").unwrap(),
                        test_vec![&host, Symbol::try_from_small_str("key").unwrap()].as_object(),
                    )
                    .unwrap()
                    .try_into_val(&host)
                    .unwrap();
                assert_eq!(res, 303);
            }

            #[test]
            fn test_constructor_with_arguments_and_recording_auth() {
                let host = test_host();
                host.switch_to_recording_auth(false).unwrap();

                let auth_contract = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);
                let auth_contract_val: ScVal = auth_contract.as_val().try_into_val(&host).unwrap();
                let auth_contract_addr = host.scaddress_from_address(auth_contract).unwrap();

                let source_account = generate_account_id(&host);
                let salt = generate_bytes_array(&host);
                let (new_contract_id, contract_id_preimage) =
                    get_contract_id(&host, source_account.clone(), salt.clone());
                let authorizer_address = ScAddress::Contract(Hash(generate_bytes_array(&host)));
                let authorizer_val = ScVal::Address(authorizer_address.clone());
                let constructor_args = vec![
                    authorizer_val.clone(),
                    ScVal::Symbol("key".try_into().unwrap()),
                    ScVal::U32(100),
                    auth_contract_val,
                ];
                let params = CreateContractTestParams {
                    skip_auth: true,
                    ..Default::default()
                };

                let contract_id = create_contract_with_constructor(
                    &host,
                    source_account.clone(),
                    salt,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    &constructor_args,
                    params,
                )
                .unwrap();

                let recorded_auth = host.get_authenticated_authorizations().unwrap();

                let expected_auth_creator = SorobanAuthorizedInvocation {
                    function: SorobanAuthorizedFunction::CreateContractV2HostFn(
                        CreateContractArgsV2 {
                            contract_id_preimage,
                            executable: ContractExecutable::Wasm(Hash(
                                Sha256::digest(CONSTRUCTOR_TEST_CONTRACT_P22)
                                    .try_into()
                                    .unwrap(),
                            )),
                            constructor_args: constructor_args.clone().try_into().unwrap(),
                        },
                    ),
                    sub_invocations: Default::default(),
                };

                let expected_auth_authorizer = SorobanAuthorizedInvocation {
                    function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                        contract_address: ScAddress::Contract(new_contract_id),
                        function_name: "__constructor".try_into().unwrap(),
                        args: constructor_args.clone().try_into().unwrap(),
                    }),
                    sub_invocations: vec![SorobanAuthorizedInvocation {
                        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                            contract_address: auth_contract_addr,
                            function_name: "do_auth".try_into().unwrap(),
                            args: vec![authorizer_val, ScVal::U32(100)].try_into().unwrap(),
                        }),
                        sub_invocations: Default::default(),
                    }]
                    .try_into()
                    .unwrap(),
                };

                assert_eq!(
                    recorded_auth,
                    vec![
                        (ScAddress::Account(source_account), expected_auth_creator),
                        (authorizer_address, expected_auth_authorizer)
                    ]
                );

                let res: u32 = host
                    .call(
                        host.add_host_object(ScAddress::Contract(contract_id))
                            .unwrap(),
                        Symbol::try_from_small_str("get_data").unwrap(),
                        test_vec![&host, Symbol::try_from_small_str("key").unwrap()].as_object(),
                    )
                    .unwrap()
                    .try_into_val(&host)
                    .unwrap();
                assert_eq!(res, 303);
            }
        }

        #[test]
        fn test_legacy_create_contract_host_fn_not_supported_for_constructor_with_arguments() {
            let host = test_host();
            host.switch_to_recording_auth(false).unwrap();

            let err = create_contract_with_constructor(
                &host,
                generate_account_id(&host),
                generate_bytes_array(&host),
                CONSTRUCTOR_TEST_CONTRACT_P22,
                &vec![],
                CreateContractTestParams {
                    use_create_contract_args_v1: true,
                    ..Default::default()
                },
            )
            .err()
            .unwrap();
            assert!(err.error.is_type(ScErrorType::Context));
            assert!(err.error.is_code(ScErrorCode::InvalidAction));
        }

        mod bad_argument_count {
            use super::*;

            #[test]
            fn test_default_constructor_with_extra_args() {
                create_contract_error_test(
                    &test_host(),
                    ADD_I32_P22,
                    vec![ScVal::U32(100)],
                    Default::default(),
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                );
            }

            #[test]
            fn test_no_argument_constructor_with_extra_args() {
                create_contract_error_test(
                    &test_host(),
                    NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P22,
                    vec![ScVal::U32(100)],
                    Default::default(),
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                );
            }

            #[test]
            fn test_constructor_with_arguments_and_missing_args() {
                let host = test_host();
                host.switch_to_recording_auth(false).unwrap();
                create_contract_error_test(
                    &host,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    vec![ScVal::U32(100)],
                    Default::default(),
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                );
            }

            #[test]
            fn test_constructor_with_arguments_and_extra_args() {
                let host = test_host();
                host.switch_to_recording_auth(false).unwrap();
                create_contract_error_test(
                    &host,
                    CONSTRUCTOR_TEST_CONTRACT_P22,
                    vec![
                        ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                        ScVal::Symbol("key".try_into().unwrap()),
                        ScVal::U32(100),
                        ScVal::Address(ScAddress::Account(generate_account_id(&host))),
                        ScVal::U32(100),
                    ],
                    Default::default(),
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                );
            }
        }

        #[test]
        fn test_constructor_errors_prevent_contract_creation() {
            for input_value in [0, 1, 2] {
                let host = test_host();
                create_contract_error_test(
                    &host,
                    CONSTRUCTOR_WITH_RESULT,
                    vec![ScVal::U32(input_value)],
                    Default::default(),
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                );
            }
        }

        #[test]
        fn test_constructor_with_return_value_is_not_supported() {
            for use_create_contract_args_v1 in [false, true] {
                for use_auth_invocation_v1 in [false, true] {
                    let host = test_host();
                    create_contract_error_test(
                        &host,
                        CONSTRUCTOR_WITH_RETURN_VALUE_P22,
                        vec![],
                        CreateContractTestParams {
                            use_create_contract_args_v1,
                            use_auth_invocation_v1,
                            ..Default::default()
                        },
                        ScErrorType::Value,
                        ScErrorCode::UnexpectedType,
                    );
                }
            }
        }
    }

    mod auth_context {
        use super::*;
        use crate::builtin_contracts::account_contract::{
            AuthorizationContext, ContractAuthorizationContext, CreateContractHostFnContext,
            CreateContractWithConstructorHostFnContext,
        };
        use crate::builtin_contracts::base_types::{BytesN, Vec as HostVec};
        use crate::builtin_contracts::common_types::ContractExecutable;
        use crate::xdr::SorobanAddressCredentials;
        use pretty_assertions::assert_eq;
        use soroban_test_wasms::CUSTOM_ACCOUNT_CONTEXT_TEST_CONTRACT;

        fn upload_wasm(host: &Host, wasm: &[u8]) -> (Hash, ContractExecutable) {
            let uploaded_wasm_hash: Val = host
                .invoke_function(HostFunction::UploadContractWasm(wasm.try_into().unwrap()))
                .unwrap()
                .try_into_val(host)
                .unwrap();
            let uploaded_wasm_hash = host
                .hash_from_bytesobj_input("wasm_hash", uploaded_wasm_hash.try_into().unwrap())
                .unwrap();
            let executable = ContractExecutable::Wasm(
                BytesN::from_slice(host, uploaded_wasm_hash.as_slice()).unwrap(),
            );
            (uploaded_wasm_hash, executable)
        }

        fn create_contract_with_custom_account_auth_context(
            host: &Host,
            account_address: &ScAddress,
            wasm_hash: Hash,
            salt: [u8; 32],
            constructor_args: &Vec<ScVal>,
            params: CreateContractTestParams,
            expected_context: HostVec,
        ) -> Result<Hash, HostError> {
            let (contract_id, contract_id_preimage) =
                get_contract_id_from_address(host, account_address.clone(), salt);
            // Check that the test is not misconfigured - we can't pass constructor args
            // into V1 XDR.
            assert!(constructor_args.is_empty() || !params.use_create_contract_args_v1);
            let host_fn =
                params.create_host_fn(&contract_id_preimage, &wasm_hash, &constructor_args);
            let auth_invocation =
                params.create_auth_invocation(&contract_id_preimage, &wasm_hash, &constructor_args);
            let credentials = SorobanCredentials::Address(SorobanAddressCredentials {
                address: account_address.clone(),
                nonce: 123,
                signature_expiration_ledger: 1000,
                signature: host.from_host_val(expected_context.into()).unwrap(),
            });
            if !params.skip_auth {
                host.set_authorization_entries(vec![SorobanAuthorizationEntry {
                    credentials,
                    root_invocation: auth_invocation,
                }])
                .unwrap();
            }
            let created_id_sc_val = host.invoke_function(host_fn)?;

            assert_eq!(
                ScVal::Address(ScAddress::Contract(contract_id.clone())),
                created_id_sc_val
            );
            assert_eq!(
                wasm_hash.as_slice(),
                get_contract_wasm_ref(&host, contract_id.clone()).as_slice()
            );

            Ok(contract_id)
        }

        #[test]
        fn test_no_argument_constructor_uses_v1_context() {
            for wasm in [
                ADD_I32,
                ADD_I32_P22,
                CONSTRUCTOR_TEST_CONTRACT_P21,
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
            ] {
                for use_create_contract_args_v1 in [false, true] {
                    for use_auth_invocation_v1 in [false, true] {
                        let host = test_host();
                        let account_contract =
                            host.register_test_contract_wasm(CUSTOM_ACCOUNT_CONTEXT_TEST_CONTRACT);
                        let account_address =
                            host.scaddress_from_address(account_contract).unwrap();

                        let (wasm_hash, wasm_exec) = upload_wasm(&host, wasm);
                        let salt = generate_bytes_array(&host);

                        let res = create_contract_with_custom_account_auth_context(
                            &host,
                            &account_address,
                            wasm_hash,
                            salt.clone(),
                            &vec![],
                            CreateContractTestParams {
                                use_create_contract_args_v1,
                                use_auth_invocation_v1,
                                ..Default::default()
                            },
                            test_vec![
                                &host,
                                AuthorizationContext::CreateContractHostFn(
                                    CreateContractHostFnContext {
                                        executable: wasm_exec,
                                        salt: BytesN::from_slice(&host, salt.as_slice()).unwrap()
                                    }
                                )
                            ],
                        );
                        assert!(res.is_ok());
                    }
                }
            }
        }

        #[test]
        fn test_no_argument_constructor_does_not_use_v2_context() {
            // Note: this test is kind of redundant logic-wise (if v1 context
            // is used, then v2 isn't), but it's important to make sure that the
            // context check actually happens.
            for wasm in [
                ADD_I32,
                ADD_I32_P22,
                CONSTRUCTOR_TEST_CONTRACT_P21,
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
                NO_ARGUMENT_CONSTRUCTOR_TEST_CONTRACT_P21,
            ] {
                for use_create_contract_args_v1 in [false, true] {
                    for use_auth_invocation_v1 in [false, true] {
                        let host = test_host();
                        let account_contract =
                            host.register_test_contract_wasm(CUSTOM_ACCOUNT_CONTEXT_TEST_CONTRACT);
                        let account_address =
                            host.scaddress_from_address(account_contract).unwrap();
                        let (wasm_hash, wasm_exec) = upload_wasm(&host, wasm);
                        let salt = generate_bytes_array(&host);

                        let err = create_contract_with_custom_account_auth_context(
                            &host,
                            &account_address,
                            wasm_hash,
                            salt.clone(),
                            &vec![],
                            CreateContractTestParams {
                                use_create_contract_args_v1,
                                use_auth_invocation_v1,
                                ..Default::default()
                            },
                            test_vec![
                                &host,
                                test_vec![
                                    &host,
                                    AuthorizationContext::CreateContractWithCtorHostFn(
                                        CreateContractWithConstructorHostFnContext {
                                            executable: wasm_exec,
                                            salt: BytesN::from_slice(&host, salt.as_slice())
                                                .unwrap(),
                                            constructor_args: test_vec![&host],
                                        }
                                    )
                                ],
                            ],
                        )
                        .err()
                        .unwrap();
                        assert!(err.error.is_type(ScErrorType::Auth));
                        assert!(err.error.is_code(ScErrorCode::InvalidAction));
                    }
                }
            }
        }

        #[test]
        fn test_constructor_with_args_uses_v2_context() {
            let host = test_host();
            let auth_contract = host.register_test_contract_wasm(AUTH_TEST_CONTRACT);

            let account_contract =
                host.register_test_contract_wasm(CUSTOM_ACCOUNT_CONTEXT_TEST_CONTRACT);
            let account_address = host.scaddress_from_address(account_contract).unwrap();

            let (wasm_hash, wasm_exec) = upload_wasm(&host, CONSTRUCTOR_TEST_CONTRACT_P22);
            let salt = generate_bytes_array(&host);

            let key_symbol = ScVal::Symbol("key".try_into().unwrap());
            let input_val = 100_u32;
            let (created_contract_id, _) =
                get_contract_id_from_address(&host, account_address.clone(), salt);
            let created_contract_address = ScAddress::Contract(created_contract_id);
            let constructor_args_vec = test_vec![
                &host,
                account_contract,
                host.to_host_val(&key_symbol).unwrap(),
                input_val,
                auth_contract
            ];
            let expected_context = test_vec![
                &host,
                AuthorizationContext::CreateContractWithCtorHostFn(
                    CreateContractWithConstructorHostFnContext {
                        executable: wasm_exec,
                        salt: BytesN::from_slice(&host, salt.as_slice()).unwrap(),
                        constructor_args: constructor_args_vec.clone(),
                    }
                ),
                AuthorizationContext::Contract(ContractAuthorizationContext {
                    contract: created_contract_address.try_into_val(&host).unwrap(),
                    fn_name: "__constructor".try_into_val(&host).unwrap(),
                    args: constructor_args_vec.clone(),
                }),
                AuthorizationContext::Contract(ContractAuthorizationContext {
                    contract: auth_contract.try_into_val(&host).unwrap(),
                    fn_name: "do_auth".try_into_val(&host).unwrap(),
                    args: test_vec![&host, account_contract, input_val],
                }),
            ];
            let account_address_sc_val = ScVal::Address(account_address.clone());
            let auth_contract_val: ScVal = auth_contract.as_val().try_into_val(&host).unwrap();
            let constructor_args_sc_vals = vec![
                account_address_sc_val.clone(),
                key_symbol.clone(),
                ScVal::U32(input_val),
                auth_contract_val,
            ];
            let params = CreateContractTestParams {
                sub_invocation_auth: vec![SorobanAuthorizedInvocation {
                    function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                        contract_address: created_contract_address,
                        function_name: "__constructor".try_into().unwrap(),
                        args: constructor_args_sc_vals.clone().try_into().unwrap(),
                    }),
                    sub_invocations: vec![SorobanAuthorizedInvocation {
                        function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                            contract_address: host.scaddress_from_address(auth_contract).unwrap(),
                            function_name: "do_auth".try_into().unwrap(),
                            args: vec![account_address_sc_val, ScVal::U32(input_val)]
                                .try_into()
                                .unwrap(),
                        }),
                        sub_invocations: Default::default(),
                    }]
                    .try_into()
                    .unwrap(),
                }],
                ..Default::default()
            };
            let res = create_contract_with_custom_account_auth_context(
                &host,
                &account_address,
                wasm_hash,
                salt.clone(),
                &constructor_args_sc_vals,
                params,
                expected_context,
            );
            assert!(res.is_ok());
        }
    }
}
