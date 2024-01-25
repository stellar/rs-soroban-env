use crate::builtin_contracts::testutils::TestSigner;
use crate::e2e_invoke::{
    invoke_host_function_in_recording_mode, ledger_entry_to_ledger_key, LedgerEntryLiveUntilChange,
};
use crate::testutils::MockSnapshotSource;
use crate::{
    budget::Budget,
    e2e_invoke::{invoke_host_function, LedgerEntryChange},
    xdr::{
        AccountEntry, AccountEntryExt, AccountId, ContractCodeEntry, ContractDataDurability,
        ContractDataEntry, ContractEvent, ContractExecutable, ContractIdPreimage,
        ContractIdPreimageFromAddress, CreateContractArgs, DiagnosticEvent, ExtensionPoint,
        HashIdPreimage, HashIdPreimageContractId, HashIdPreimageSorobanAuthorization, HostFunction,
        InvokeContractArgs, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerFootprint,
        LedgerKey, LedgerKeyContractCode, LedgerKeyContractData, Limits, PublicKey, ReadXdr,
        ScAddress, ScBytes, ScContractInstance, ScErrorCode, ScErrorType, ScMapEntry, ScVal,
        SequenceNumber, SorobanAuthorizationEntry, SorobanAuthorizedFunction,
        SorobanAuthorizedInvocation, SorobanCredentials, SorobanResources, Thresholds, TtlEntry,
        Uint256, WriteXdr,
    },
    Host, HostError, LedgerInfo,
};
use ed25519_dalek::SigningKey;
use pretty_assertions::assert_eq;
use rand::rngs::StdRng;
use rand::SeedableRng;
use sha2::{Digest, Sha256};
use soroban_test_wasms::{ADD_F32, ADD_I32, AUTH_TEST_CONTRACT, CONTRACT_STORAGE};
use std::rc::Rc;

const DEFAULT_LEDGER_SEQ: u32 = 1_000_000;
const DEFAULT_NETWORK_ID: [u8; 32] = [5; 32];

// It's tricky to get exactly the same instruction consumption
// in the recording storage/auth mode vs the enforcing mode. For
// example, frame snapshots in enforcing mode contain all the auths and
// storage entries, while in recording mode these snapshots will be
// smaller as storage/auth are populated eagerly.
// We don't anticipate this divergence to be too high though, which
// is why this coefficient should be low.
const RECORDING_MODE_INSTRUCTIONS_COEFFICIENT: f64 = 1.01;

fn default_ledger_info() -> LedgerInfo {
    LedgerInfo {
        protocol_version: 20,
        sequence_number: DEFAULT_LEDGER_SEQ,
        timestamp: 12345678,
        network_id: DEFAULT_NETWORK_ID,
        base_reserve: 5_000_000,
        min_temp_entry_ttl: 16,
        min_persistent_entry_ttl: 100_000,
        max_entry_ttl: 10_000_000,
    }
}

fn upload_wasm_host_fn(wasm: &[u8]) -> HostFunction {
    HostFunction::UploadContractWasm(wasm.try_into().unwrap())
}

fn get_wasm_hash(wasm: &[u8]) -> [u8; 32] {
    Sha256::digest(wasm).into()
}

fn get_wasm_key(wasm: &[u8]) -> LedgerKey {
    LedgerKey::ContractCode(LedgerKeyContractCode {
        hash: get_wasm_hash(wasm).try_into().unwrap(),
    })
}

fn get_contract_id_preimage(account_id: &AccountId, salt: &[u8; 32]) -> ContractIdPreimage {
    ContractIdPreimage::Address(ContractIdPreimageFromAddress {
        address: ScAddress::Account(account_id.clone()),
        salt: Uint256(salt.clone().try_into().unwrap()),
    })
}

fn get_contract_id_hash(id_preimage: &ContractIdPreimage) -> [u8; 32] {
    let preimage = HashIdPreimage::ContractId(HashIdPreimageContractId {
        network_id: DEFAULT_NETWORK_ID.clone().try_into().unwrap(),
        contract_id_preimage: id_preimage.clone(),
    });
    Sha256::digest(&preimage.to_xdr(Limits::none()).unwrap()).into()
}

fn ledger_entry(le_data: LedgerEntryData) -> LedgerEntry {
    LedgerEntry {
        last_modified_ledger_seq: 0,
        data: le_data,
        ext: LedgerEntryExt::V0,
    }
}

fn account_entry(account_id: &AccountId) -> LedgerEntry {
    ledger_entry(LedgerEntryData::Account(AccountEntry {
        account_id: account_id.clone(),
        balance: 10_000_000,
        seq_num: SequenceNumber(0),
        num_sub_entries: 0,
        inflation_dest: None,
        flags: 0,
        home_domain: Default::default(),
        thresholds: Thresholds([1, 0, 0, 0]),
        signers: Default::default(),
        ext: AccountEntryExt::V0,
    }))
}

fn wasm_entry(wasm: &[u8]) -> LedgerEntry {
    ledger_entry(LedgerEntryData::ContractCode(ContractCodeEntry {
        ext: ExtensionPoint::V0,
        hash: get_wasm_hash(wasm).try_into().unwrap(),
        code: wasm.try_into().unwrap(),
    }))
}

fn wasm_entry_size(wasm: &[u8]) -> u32 {
    wasm_entry(wasm).to_xdr(Limits::none()).unwrap().len() as u32
}

fn get_account_id(pub_key: [u8; 32]) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(pub_key.try_into().unwrap()))
}

fn prng_seed() -> [u8; 32] {
    [0; 32]
}

fn bytes_sc_val(bytes: &[u8]) -> ScVal {
    ScVal::Bytes(ScBytes(bytes.try_into().unwrap()))
}

fn resources(
    instructions: u32,
    ro_footprint: Vec<LedgerKey>,
    rw_footprint: Vec<LedgerKey>,
) -> SorobanResources {
    let footprint = LedgerFootprint {
        read_only: ro_footprint.try_into().unwrap(),
        read_write: rw_footprint.try_into().unwrap(),
    };
    SorobanResources {
        footprint,
        instructions,
        read_bytes: 0,
        write_bytes: 0,
    }
}

fn compute_key_hash(key: &LedgerKey) -> Vec<u8> {
    let key_xdr = key.to_xdr(Limits::none()).unwrap();
    let hash: [u8; 32] = Sha256::digest(&key_xdr).into();
    hash.to_vec()
}

fn ttl_entry(key: &LedgerKey, ttl: u32) -> TtlEntry {
    TtlEntry {
        key_hash: compute_key_hash(key).try_into().unwrap(),
        live_until_ledger_seq: ttl,
    }
}

fn symbol_sc_val(s: &str) -> ScVal {
    ScVal::Symbol(s.try_into().unwrap())
}

fn u64_sc_val(v: u64) -> ScVal {
    ScVal::U64(v)
}

fn u32_sc_val(v: u32) -> ScVal {
    ScVal::U32(v)
}

fn create_contract_auth(
    contract_id_preimage: &ContractIdPreimage,
    wasm: &[u8],
) -> SorobanAuthorizationEntry {
    SorobanAuthorizationEntry {
        credentials: SorobanCredentials::SourceAccount,
        root_invocation: SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::CreateContractHostFn(CreateContractArgs {
                contract_id_preimage: contract_id_preimage.clone(),
                executable: ContractExecutable::Wasm(get_wasm_hash(wasm).try_into().unwrap()),
            }),
            sub_invocations: Default::default(),
        },
    }
}

struct AuthContractInvocationNode {
    address: ScAddress,
    children: Vec<AuthContractInvocationNode>,
}

fn sc_struct_field(key: &str, val: ScVal) -> ScMapEntry {
    ScMapEntry {
        key: ScVal::Symbol(key.try_into().unwrap()),
        val,
    }
}

impl AuthContractInvocationNode {
    fn into_scval(self, address_count: usize) -> ScVal {
        let children: Vec<ScVal> = self
            .children
            .into_iter()
            .map(|c| c.into_scval(address_count))
            .collect();
        let fields = vec![
            sc_struct_field("children", ScVal::Vec(Some(children.try_into().unwrap()))),
            sc_struct_field("contract", ScVal::Address(self.address)),
            sc_struct_field(
                "need_auth",
                ScVal::Vec(Some(
                    vec![ScVal::Bool(true); address_count].try_into().unwrap(),
                )),
            ),
            sc_struct_field("try_call", ScVal::Bool(false)),
        ];
        ScVal::Map(Some(fields.try_into().unwrap()))
    }
}

fn auth_contract_invocation(
    addresses: Vec<ScAddress>,
    tree: AuthContractInvocationNode,
) -> HostFunction {
    let address_count = addresses.len();
    let address_vals: Vec<ScVal> = addresses.into_iter().map(|a| ScVal::Address(a)).collect();
    let addresses_val = ScVal::Vec(Some(address_vals.try_into().unwrap()));
    HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: tree.address.clone(),
        function_name: "tree_fn".try_into().unwrap(),
        args: vec![addresses_val, tree.into_scval(address_count)]
            .try_into()
            .unwrap(),
    })
}

fn sign_auth_entry(
    ledger_info: &LedgerInfo,
    signers: &Vec<TestSigner>,
    auth_entry: SorobanAuthorizationEntry,
) -> SorobanAuthorizationEntry {
    let mut out = auth_entry;

    match &mut out.credentials {
        SorobanCredentials::SourceAccount => {}
        SorobanCredentials::Address(creds) => {
            let dummy_host = Host::test_host();
            let signature_payload_preimage =
                HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                    network_id: ledger_info.network_id.try_into().unwrap(),
                    invocation: out.root_invocation.clone(),
                    nonce: creds.nonce,
                    signature_expiration_ledger: ledger_info.sequence_number
                        + ledger_info.min_temp_entry_ttl
                        - 1,
                });
            let signature_payload: [u8; 32] =
                Sha256::digest(&signature_payload_preimage.to_xdr(Limits::none()).unwrap()).into();
            let signer = signers
                .iter()
                .find(|s| s.sc_address() == creds.address)
                .unwrap();
            creds.signature = signer.sign(&dummy_host, &signature_payload);
            creds.signature_expiration_ledger =
                ledger_info.sequence_number + ledger_info.min_temp_entry_ttl - 1;
        }
    }
    out
}

impl PartialEq<Self> for HostError {
    fn eq(&self, other: &Self) -> bool {
        self.error == other.error
    }
}

impl Eq for HostError {}

#[derive(Debug, Eq, PartialEq, Clone)]
struct LedgerEntryChangeHelper {
    read_only: bool,
    key: LedgerKey,
    old_entry_size_bytes: u32,
    new_value: Option<LedgerEntry>,
    ttl_change: Option<LedgerEntryLiveUntilChange>,
}

impl From<LedgerEntryChange> for LedgerEntryChangeHelper {
    fn from(c: LedgerEntryChange) -> Self {
        Self {
            read_only: c.read_only,
            key: LedgerKey::from_xdr(c.encoded_key, Limits::none()).unwrap(),
            old_entry_size_bytes: c.old_entry_size_bytes,
            new_value: c
                .encoded_new_value
                .map(|v| LedgerEntry::from_xdr(v, Limits::none()).unwrap()),
            ttl_change: c.ttl_change,
        }
    }
}

impl LedgerEntryChangeHelper {
    fn no_op_change(entry: &LedgerEntry, live_until_ledger: u32) -> Self {
        let ledger_key = ledger_entry_to_ledger_key(entry, &Budget::default()).unwrap();
        let durability = match &ledger_key {
            LedgerKey::ContractData(cd) => Some(cd.durability),
            LedgerKey::ContractCode(_) => Some(ContractDataDurability::Persistent),
            _ => None,
        };
        Self {
            read_only: true,
            key: ledger_key.clone(),
            old_entry_size_bytes: entry.to_xdr(Limits::none()).unwrap().len() as u32,
            new_value: None,
            ttl_change: if let Some(durability) = durability {
                Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&ledger_key),
                    durability,
                    old_live_until_ledger: live_until_ledger,
                    new_live_until_ledger: live_until_ledger,
                })
            } else {
                None
            },
        }
    }
}

struct InvokeHostFunctionHelperResult {
    invoke_result: Result<ScVal, HostError>,
    ledger_changes: Vec<LedgerEntryChangeHelper>,
    contract_events: Vec<ContractEvent>,
    diagnostic_events: Vec<DiagnosticEvent>,
    budget: Budget,
}

struct InvokeHostFunctionRecordingHelperResult {
    invoke_result: Result<ScVal, HostError>,
    resources: SorobanResources,
    auth: Vec<SorobanAuthorizationEntry>,
    ledger_changes: Vec<LedgerEntryChangeHelper>,
    contract_events: Vec<ContractEvent>,
    diagnostic_events: Vec<DiagnosticEvent>,
}

fn invoke_host_function_helper(
    enable_diagnostics: bool,
    host_fn: &HostFunction,
    resources: &SorobanResources,
    source_account: &AccountId,
    auth_entries: Vec<SorobanAuthorizationEntry>,
    ledger_info: &LedgerInfo,
    ledger_entries_with_ttl: Vec<(LedgerEntry, Option<u32>)>,
    prng_seed: &[u8; 32],
) -> Result<InvokeHostFunctionHelperResult, HostError> {
    let limits = Limits::none();
    let encoded_host_fn = host_fn.to_xdr(limits.clone()).unwrap();
    let encoded_resources = resources.to_xdr(limits.clone()).unwrap();
    let encoded_source_account = source_account.to_xdr(limits.clone()).unwrap();
    let encoded_auth_entries: Vec<Vec<u8>> = auth_entries
        .iter()
        .map(|e| e.to_xdr(limits.clone()).unwrap())
        .collect();
    let encoded_ledger_entries: Vec<Vec<u8>> = ledger_entries_with_ttl
        .iter()
        .map(|e| e.0.to_xdr(limits.clone()).unwrap())
        .collect();
    let encoded_ttl_entries: Vec<Vec<u8>> = ledger_entries_with_ttl
        .iter()
        .map(|e| {
            let (le, ttl) = e;
            let key = match &le.data {
                LedgerEntryData::ContractData(cd) => {
                    LedgerKey::ContractData(LedgerKeyContractData {
                        contract: cd.contract.clone(),
                        key: cd.key.clone(),
                        durability: cd.durability.clone(),
                    })
                }
                LedgerEntryData::ContractCode(code) => {
                    LedgerKey::ContractCode(LedgerKeyContractCode {
                        hash: code.hash.clone(),
                    })
                }
                _ => {
                    return vec![];
                }
            };
            ttl_entry(&key, ttl.unwrap())
                .to_xdr(limits.clone())
                .unwrap()
        })
        .collect();
    let budget = Budget::default();
    budget
        .reset_cpu_limit(resources.instructions as u64)
        .unwrap();
    let mut diagnostic_events = Vec::<DiagnosticEvent>::new();
    let res = invoke_host_function(
        &budget,
        enable_diagnostics,
        encoded_host_fn,
        encoded_resources,
        encoded_source_account,
        encoded_auth_entries.into_iter(),
        ledger_info.clone(),
        encoded_ledger_entries.into_iter(),
        encoded_ttl_entries.into_iter(),
        prng_seed.to_vec(),
        &mut diagnostic_events,
    )?;
    Ok(InvokeHostFunctionHelperResult {
        invoke_result: res
            .encoded_invoke_result
            .map(|v| ScVal::from_xdr(v, limits.clone()).unwrap()),
        ledger_changes: res.ledger_changes.into_iter().map(|c| c.into()).collect(),
        contract_events: res
            .encoded_contract_events
            .iter()
            .map(|v| ContractEvent::from_xdr(v, limits.clone()).unwrap())
            .collect(),
        diagnostic_events,
        budget,
    })
}

fn invoke_host_function_recording_helper(
    enable_diagnostics: bool,
    host_fn: &HostFunction,
    source_account: &AccountId,
    auth_entries: Option<Vec<SorobanAuthorizationEntry>>,
    ledger_info: &LedgerInfo,
    ledger_entries_with_ttl: Vec<(LedgerEntry, Option<u32>)>,
    prng_seed: &[u8; 32],
    max_instructions_override: Option<u64>,
) -> Result<InvokeHostFunctionRecordingHelperResult, HostError> {
    let budget = Budget::default();
    if let Some(max_insns) = max_instructions_override {
        budget.reset_cpu_limit(max_insns).unwrap();
    }
    let snapshot = Rc::new(MockSnapshotSource::from_entries(ledger_entries_with_ttl));
    let mut diagnostic_events = vec![];
    let res = invoke_host_function_in_recording_mode(
        &budget,
        enable_diagnostics,
        host_fn,
        source_account,
        auth_entries,
        ledger_info.clone(),
        snapshot,
        prng_seed.clone(),
        &mut diagnostic_events,
    )?;
    Ok(InvokeHostFunctionRecordingHelperResult {
        invoke_result: res.invoke_result,
        resources: res.resources,
        auth: res.auth,
        ledger_changes: res.ledger_changes.into_iter().map(|c| c.into()).collect(),
        contract_events: res.contract_events,
        diagnostic_events,
    })
}

fn invoke_host_function_using_simulation_with_signers(
    enable_diagnostics: bool,
    host_fn: &HostFunction,
    source_account: &AccountId,
    ledger_info: &LedgerInfo,
    ledger_entries_with_ttl: Vec<(LedgerEntry, Option<u32>)>,
    prng_seed: &[u8; 32],
    signers: &Vec<TestSigner>,
) -> Result<InvokeHostFunctionHelperResult, HostError> {
    let mut recording_result = invoke_host_function_recording_helper(
        enable_diagnostics,
        host_fn,
        source_account,
        None,
        ledger_info,
        ledger_entries_with_ttl.clone(),
        prng_seed,
        None,
    )
    .unwrap();
    let signed_auth: Vec<_> = recording_result
        .auth
        .into_iter()
        .map(|a| sign_auth_entry(ledger_info, signers, a))
        .collect();

    let recording_result_with_enforcing_auth = invoke_host_function_recording_helper(
        enable_diagnostics,
        host_fn,
        source_account,
        Some(signed_auth.clone()),
        ledger_info,
        ledger_entries_with_ttl.clone(),
        prng_seed,
        None,
    )
    .unwrap();
    assert_eq!(
        recording_result.invoke_result,
        recording_result_with_enforcing_auth.invoke_result
    );
    assert_eq!(
        recording_result.resources.footprint,
        recording_result_with_enforcing_auth.resources.footprint
    );
    assert_eq!(
        recording_result.resources.read_bytes,
        recording_result_with_enforcing_auth.resources.read_bytes
    );
    assert_eq!(
        recording_result.resources.write_bytes,
        recording_result_with_enforcing_auth.resources.write_bytes
    );

    assert_eq!(
        recording_result.ledger_changes,
        recording_result_with_enforcing_auth.ledger_changes
    );
    assert_eq!(
        recording_result.contract_events,
        recording_result_with_enforcing_auth.contract_events
    );
    assert_eq!(
        recording_result.diagnostic_events,
        recording_result_with_enforcing_auth.diagnostic_events
    );

    // Instructions are expected to be slightly different between recording and
    // enforcing modes, so just make sure that the estimation is within the small
    // coefficient.
    recording_result.resources.instructions = (recording_result.resources.instructions as f64
        * RECORDING_MODE_INSTRUCTIONS_COEFFICIENT)
        as u32;
    assert!(
        recording_result.resources.instructions
            >= recording_result_with_enforcing_auth.resources.instructions
    );
    let enforcing_result = invoke_host_function_helper(
        enable_diagnostics,
        host_fn,
        &recording_result.resources,
        source_account,
        signed_auth,
        ledger_info,
        ledger_entries_with_ttl,
        prng_seed,
    )?;

    assert_eq!(
        recording_result.invoke_result,
        enforcing_result.invoke_result
    );
    assert_eq!(
        recording_result.ledger_changes,
        enforcing_result.ledger_changes
    );
    assert_eq!(
        recording_result.contract_events,
        enforcing_result.contract_events
    );
    assert_eq!(
        recording_result.diagnostic_events,
        enforcing_result.diagnostic_events
    );
    Ok(enforcing_result)
}

fn invoke_host_function_using_simulation(
    enable_diagnostics: bool,
    host_fn: &HostFunction,
    source_account: &AccountId,
    ledger_info: &LedgerInfo,
    ledger_entries_with_ttl: Vec<(LedgerEntry, Option<u32>)>,
    prng_seed: &[u8; 32],
) -> Result<InvokeHostFunctionHelperResult, HostError> {
    invoke_host_function_using_simulation_with_signers(
        enable_diagnostics,
        host_fn,
        source_account,
        ledger_info,
        ledger_entries_with_ttl,
        prng_seed,
        &vec![],
    )
}

struct CreateContractData {
    deployer: AccountId,
    wasm_key: LedgerKey,
    wasm_entry: LedgerEntry,
    contract_key: LedgerKey,
    contract_entry: LedgerEntry,
    contract_address: ScAddress,
    auth_entry: SorobanAuthorizationEntry,
    host_fn: HostFunction,
}

impl CreateContractData {
    fn new(salt: [u8; 32], wasm: &[u8]) -> Self {
        let deployer = get_account_id([123; 32]);
        let contract_id_preimage = get_contract_id_preimage(&deployer, &salt);

        let host_fn = HostFunction::CreateContract(CreateContractArgs {
            contract_id_preimage: contract_id_preimage.clone(),
            executable: ContractExecutable::Wasm(get_wasm_hash(wasm).try_into().unwrap()),
        });
        let contract_address = ScAddress::Contract(
            get_contract_id_hash(&contract_id_preimage)
                .try_into()
                .unwrap(),
        );
        let contract_key = LedgerKey::ContractData(LedgerKeyContractData {
            contract: contract_address.clone(),
            key: ScVal::LedgerKeyContractInstance,
            durability: ContractDataDurability::Persistent,
        });
        let auth_entry = create_contract_auth(&contract_id_preimage, wasm);

        let contract_entry = ledger_entry(LedgerEntryData::ContractData(ContractDataEntry {
            ext: ExtensionPoint::V0,
            contract: contract_address.clone(),
            key: ScVal::LedgerKeyContractInstance,
            durability: ContractDataDurability::Persistent,
            val: ScVal::ContractInstance(ScContractInstance {
                executable: ContractExecutable::Wasm(get_wasm_hash(wasm).try_into().unwrap()),
                storage: None,
            }),
        }));

        Self {
            deployer,
            wasm_key: get_wasm_key(wasm),
            wasm_entry: wasm_entry(wasm),
            contract_key,
            contract_entry,
            contract_address,
            auth_entry,
            host_fn,
        }
    }
}

fn invoke_contract_host_fn(contract: &ScAddress, fn_name: &str, args: Vec<ScVal>) -> HostFunction {
    HostFunction::InvokeContract(InvokeContractArgs {
        contract_address: contract.clone(),
        function_name: fn_name.try_into().unwrap(),
        args: args.try_into().unwrap(),
    })
}

fn contract_data_key(
    contract_address: &ScAddress,
    key: &ScVal,
    durability: ContractDataDurability,
) -> LedgerKey {
    LedgerKey::ContractData(LedgerKeyContractData {
        contract: contract_address.clone(),
        key: key.clone(),
        durability,
    })
}

fn contract_data_entry(
    contract_address: &ScAddress,
    key: &ScVal,
    value: &ScVal,
    durability: ContractDataDurability,
) -> LedgerEntry {
    ledger_entry(LedgerEntryData::ContractData(ContractDataEntry {
        ext: ExtensionPoint::V0,
        contract: contract_address.clone(),
        key: key.clone(),
        durability,
        val: value.clone(),
    }))
}

#[test]
fn test_run_out_of_budget_before_calling_host() {
    let res = invoke_host_function_helper(
        true,
        &upload_wasm_host_fn(ADD_I32),
        &resources(1000, vec![], vec![]),
        &get_account_id([0; 32]),
        vec![],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
}

#[test]
fn test_run_out_of_budget_before_calling_host_in_recording_mode() {
    let res = invoke_host_function_recording_helper(
        true,
        &upload_wasm_host_fn(ADD_I32),
        &get_account_id([0; 32]),
        None,
        &default_ledger_info(),
        vec![],
        &prng_seed(),
        Some(1000),
    );
    assert!(HostError::result_matches_err(
        res,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
}

#[test]
fn test_wasm_upload_success() {
    let ledger_key = get_wasm_key(ADD_I32);
    let ledger_info = default_ledger_info();

    let res = invoke_host_function_helper(
        false,
        &upload_wasm_host_fn(ADD_I32),
        &resources(10_000_000, vec![], vec![ledger_key.clone()]),
        &get_account_id([123; 32]),
        vec![],
        &ledger_info,
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(res.contract_events.is_empty());
    assert_eq!(
        res.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );
    assert_eq!(
        res.ledger_changes,
        vec![LedgerEntryChangeHelper {
            read_only: false,
            key: ledger_key.clone(),
            old_entry_size_bytes: 0,
            new_value: Some(wasm_entry(ADD_I32)),
            ttl_change: Some(LedgerEntryLiveUntilChange {
                key_hash: compute_key_hash(&ledger_key),
                durability: ContractDataDurability::Persistent,
                old_live_until_ledger: 0,
                new_live_until_ledger: ledger_info.sequence_number
                    + ledger_info.min_persistent_entry_ttl
                    - 1,
            }),
        }]
    );
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_wasm_upload_success_in_recording_mode() {
    let ledger_key = get_wasm_key(ADD_I32);
    let ledger_info = default_ledger_info();

    let res = invoke_host_function_recording_helper(
        false,
        &upload_wasm_host_fn(ADD_I32),
        &get_account_id([123; 32]),
        None,
        &ledger_info,
        vec![],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert!(res.contract_events.is_empty());
    assert_eq!(
        res.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );
    assert_eq!(
        res.ledger_changes,
        vec![LedgerEntryChangeHelper {
            read_only: false,
            key: ledger_key.clone(),
            old_entry_size_bytes: 0,
            new_value: Some(wasm_entry(ADD_I32)),
            ttl_change: Some(LedgerEntryLiveUntilChange {
                key_hash: compute_key_hash(&ledger_key),
                durability: ContractDataDurability::Persistent,
                old_live_until_ledger: 0,
                new_live_until_ledger: ledger_info.sequence_number
                    + ledger_info.min_persistent_entry_ttl
                    - 1,
            }),
        }]
    );
    assert!(res.auth.is_empty());
    assert_eq!(
        res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: Default::default(),
                read_write: vec![ledger_key.clone()].try_into().unwrap()
            },
            instructions: 1060474,
            read_bytes: 0,
            write_bytes: 636,
        }
    );
}

#[test]
fn test_wasm_upload_failure_in_recording_mode() {
    let ledger_info = default_ledger_info();

    let res = invoke_host_function_recording_helper(
        true,
        &upload_wasm_host_fn(&[0_u8; 1000]),
        &get_account_id([123; 32]),
        None,
        &ledger_info,
        vec![],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.auth.is_empty());
    assert_eq!(
        res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: Default::default(),
                read_write: Default::default(),
            },
            instructions: 1093647,
            read_bytes: 0,
            write_bytes: 0,
        }
    );
}

#[test]
fn test_wasm_upload_success_using_simulation() {
    let res = invoke_host_function_using_simulation(
        true,
        &upload_wasm_host_fn(ADD_I32),
        &get_account_id([123; 32]),
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    );
    assert!(res.is_ok());
}

#[test]
fn test_wasm_upload_failure_using_simulation() {
    let res = invoke_host_function_using_simulation(
        false,
        &upload_wasm_host_fn(&[0_u8; 1000]),
        &get_account_id([123; 32]),
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::WasmVm, ScErrorCode::InvalidAction)
    ));
}

#[test]
fn test_wasm_upload_budget_exceeded() {
    let ledger_key = get_wasm_key(CONTRACT_STORAGE);
    let ledger_info = default_ledger_info();

    let res = invoke_host_function_helper(
        true,
        &upload_wasm_host_fn(CONTRACT_STORAGE),
        &resources(1_000_000, vec![], vec![ledger_key.clone()]),
        &get_account_id([123; 32]),
        vec![],
        &ledger_info,
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Budget, ScErrorCode::ExceededLimit)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.contract_events.is_empty());
    assert_eq!(res.budget.get_cpu_insns_remaining().unwrap(), 0);
}

#[test]
fn test_wasm_upload_with_incorrect_footprint_fails() {
    // RO footprint instead of RW
    let res = invoke_host_function_helper(
        false,
        &upload_wasm_host_fn(ADD_I32),
        &resources(10_000_000, vec![get_wasm_key(ADD_I32)], vec![]),
        &get_account_id([111; 32]),
        vec![],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));

    assert!(res.ledger_changes.is_empty());
    // Diagnostics is disabled for this case
    assert!(res.diagnostic_events.is_empty());
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_wasm_upload_without_footprint_fails() {
    let res = invoke_host_function_helper(
        true,
        &upload_wasm_host_fn(ADD_I32),
        &resources(10_000_000, vec![], vec![]),
        &get_account_id([123; 32]),
        vec![],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_wasm_reupload_is_no_op() {
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_helper(
        false,
        &upload_wasm_host_fn(ADD_I32),
        &resources(10_000_000, vec![], vec![get_wasm_key(ADD_I32)]),
        &get_account_id([123; 32]),
        vec![],
        &ledger_info,
        vec![(wasm_entry(ADD_I32), Some(ledger_info.sequence_number))],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );
    assert_eq!(
        res.ledger_changes,
        vec![LedgerEntryChangeHelper {
            read_only: false,
            key: get_wasm_key(ADD_I32),
            old_entry_size_bytes: wasm_entry_size(ADD_I32),
            new_value: Some(wasm_entry(ADD_I32)),
            ttl_change: Some(LedgerEntryLiveUntilChange {
                key_hash: compute_key_hash(&get_wasm_key(ADD_I32)),
                durability: ContractDataDurability::Persistent,
                old_live_until_ledger: ledger_info.sequence_number,
                new_live_until_ledger: ledger_info.sequence_number,
            })
        }]
    );
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_wasm_upload_success_with_extra_footprint_entries() {
    let ledger_info = default_ledger_info();

    let res = invoke_host_function_helper(
        false,
        &upload_wasm_host_fn(ADD_I32),
        &resources(
            10_000_000,
            vec![get_wasm_key(CONTRACT_STORAGE)],
            vec![get_wasm_key(ADD_I32), get_wasm_key(ADD_F32)],
        ),
        &get_account_id([123; 32]),
        vec![],
        &ledger_info,
        vec![(
            wasm_entry(ADD_F32),
            Some(ledger_info.sequence_number + 1000),
        )],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        bytes_sc_val(&get_wasm_hash(ADD_I32))
    );
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: get_wasm_key(ADD_I32),
                old_entry_size_bytes: 0,
                new_value: Some(wasm_entry(ADD_I32)),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&get_wasm_key(ADD_I32)),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_persistent_entry_ttl
                        - 1,
                }),
            },
            LedgerEntryChangeHelper {
                read_only: false,
                key: get_wasm_key(ADD_F32),
                old_entry_size_bytes: wasm_entry_size(ADD_F32),
                new_value: Some(wasm_entry(ADD_F32)),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&get_wasm_key(ADD_F32)),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: ledger_info.sequence_number + 1000,
                    new_live_until_ledger: ledger_info.sequence_number + 1000,
                }),
            },
            LedgerEntryChangeHelper {
                read_only: true,
                key: get_wasm_key(CONTRACT_STORAGE),
                old_entry_size_bytes: 0,
                new_value: None,
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&get_wasm_key(CONTRACT_STORAGE)),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: 0,
                }),
            },
        ]
    );
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_create_contract_success() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(
            10_000_000,
            vec![cd.wasm_key.clone()],
            vec![cd.contract_key.clone()],
        ),
        &cd.deployer,
        vec![cd.auth_entry],
        &ledger_info,
        vec![(
            cd.wasm_entry.clone(),
            Some(ledger_info.sequence_number + 100),
        )],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(cd.contract_address.clone())
    );
    assert!(res.contract_events.is_empty());
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: cd.contract_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(cd.contract_entry),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&cd.contract_key),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_persistent_entry_ttl
                        - 1,
                }),
            },
            LedgerEntryChangeHelper::no_op_change(
                &cd.wasm_entry,
                ledger_info.sequence_number + 100
            ),
        ]
    );
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_create_contract_success_in_recording_mode() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_recording_helper(
        true,
        &cd.host_fn,
        &cd.deployer,
        None,
        &ledger_info,
        vec![(
            cd.wasm_entry.clone(),
            Some(ledger_info.sequence_number + 100),
        )],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(cd.contract_address.clone())
    );
    assert!(res.contract_events.is_empty());
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: cd.contract_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(cd.contract_entry),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&cd.contract_key),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_persistent_entry_ttl
                        - 1,
                }),
            },
            LedgerEntryChangeHelper::no_op_change(
                &cd.wasm_entry,
                ledger_info.sequence_number + 100
            ),
        ]
    );
    assert_eq!(res.auth, vec![cd.auth_entry]);
    assert_eq!(
        res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: vec![cd.wasm_key].try_into().unwrap(),
                read_write: vec![cd.contract_key].try_into().unwrap()
            },
            instructions: 449456,
            read_bytes: 636,
            write_bytes: 104,
        }
    );
}

#[test]
fn test_create_contract_success_in_recording_mode_with_enforced_auth() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_recording_helper(
        true,
        &cd.host_fn,
        &cd.deployer,
        Some(vec![cd.auth_entry.clone()]),
        &ledger_info,
        vec![(
            cd.wasm_entry.clone(),
            Some(ledger_info.sequence_number + 100),
        )],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(cd.contract_address.clone())
    );
    assert!(res.contract_events.is_empty());
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: cd.contract_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(cd.contract_entry),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&cd.contract_key),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_persistent_entry_ttl
                        - 1,
                }),
            },
            LedgerEntryChangeHelper::no_op_change(
                &cd.wasm_entry,
                ledger_info.sequence_number + 100
            ),
        ]
    );
    assert_eq!(res.auth, vec![cd.auth_entry]);
    assert_eq!(
        res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: vec![cd.wasm_key].try_into().unwrap(),
                read_write: vec![cd.contract_key].try_into().unwrap()
            },
            instructions: 450897,
            read_bytes: 636,
            write_bytes: 104,
        }
    );
}

#[test]
fn test_create_contract_success_using_simulation() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_using_simulation(
        true,
        &cd.host_fn,
        &cd.deployer,
        &ledger_info,
        vec![(
            cd.wasm_entry.clone(),
            Some(ledger_info.sequence_number + 100),
        )],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(cd.contract_address.clone())
    );
}

#[test]
fn test_create_contract_success_with_extra_footprint_entries() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let cd2 = CreateContractData::new([222; 32], ADD_F32);
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(
            10_000_000,
            vec![cd.wasm_key.clone(), cd2.wasm_key.clone()],
            vec![cd.contract_key.clone(), cd2.contract_key.clone()],
        ),
        &cd.deployer,
        vec![cd.auth_entry],
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd2.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 200),
            ),
        ],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(
        res.invoke_result.unwrap(),
        ScVal::Address(cd.contract_address.clone())
    );
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: cd.contract_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(cd.contract_entry),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&cd.contract_key),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_persistent_entry_ttl
                        - 1,
                }),
            },
            LedgerEntryChangeHelper {
                read_only: false,
                key: cd2.contract_key.clone(),
                old_entry_size_bytes: 0,
                new_value: None,
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&cd2.contract_key),
                    durability: ContractDataDurability::Persistent,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: 0,
                }),
            },
            LedgerEntryChangeHelper::no_op_change(
                &cd.wasm_entry,
                ledger_info.sequence_number + 100
            ),
            LedgerEntryChangeHelper::no_op_change(
                &cd2.wasm_entry,
                ledger_info.sequence_number + 200
            ),
        ]
    );
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_create_contract_without_footprint_fails() {
    let cd = CreateContractData::new([111; 32], ADD_I32);

    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(10_000_000, vec![], vec![]),
        &cd.deployer,
        vec![cd.auth_entry],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_create_contract_without_auth_fails() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    // No auth
    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(10_000_000, vec![cd.wasm_key], vec![cd.contract_key]),
        &cd.deployer,
        vec![],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_create_contract_without_wasm_entry_fails() {
    let cd = CreateContractData::new([111; 32], ADD_I32);

    // No Wasm entry
    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(10_000_000, vec![cd.wasm_key], vec![cd.contract_key]),
        &cd.deployer,
        vec![cd.auth_entry],
        &default_ledger_info(),
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Storage, ScErrorCode::MissingValue)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
}

#[test]
fn test_create_contract_with_incorrect_auth_fails() {
    let cd = CreateContractData::new([111; 32], ADD_I32);
    let res = invoke_host_function_helper(
        true,
        &cd.host_fn,
        &resources(10_000_000, vec![cd.wasm_key], vec![cd.contract_key]),
        &cd.deployer,
        // Auth entry is for the different salt
        vec![create_contract_auth(
            &get_contract_id_preimage(&cd.deployer, &[1; 32]),
            ADD_I32,
        )],
        &default_ledger_info(),
        vec![(
            wasm_entry(ADD_I32),
            Some(default_ledger_info().sequence_number + 100),
        )],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Auth, ScErrorCode::InvalidAction)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
}

#[test]
fn test_invoke_contract_with_storage_ops_success() {
    let cd = CreateContractData::new([111; 32], CONTRACT_STORAGE);
    let ledger_info = default_ledger_info();
    let key = symbol_sc_val("key");
    let val = u64_sc_val(u64::MAX);
    let host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "put_temporary",
        vec![key.clone(), val.clone()],
    );
    let data_key = contract_data_key(
        &cd.contract_address,
        &key,
        ContractDataDurability::Temporary,
    );
    let res = invoke_host_function_helper(
        true,
        &host_fn,
        &resources(
            10_000_000,
            vec![cd.contract_key.clone(), cd.wasm_key.clone()],
            vec![data_key.clone()],
        ),
        &cd.deployer,
        vec![],
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
        ],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(res.invoke_result.unwrap(), ScVal::Void);
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);

    let contract_entry_change = LedgerEntryChangeHelper::no_op_change(
        &cd.contract_entry,
        ledger_info.sequence_number + 1000,
    );
    let wasm_entry_change =
        LedgerEntryChangeHelper::no_op_change(&cd.wasm_entry, ledger_info.sequence_number + 100);
    let new_entry = contract_data_entry(
        &cd.contract_address,
        &key,
        &val,
        ContractDataDurability::Temporary,
    );
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: data_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(new_entry.clone()),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&data_key),
                    durability: ContractDataDurability::Temporary,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_temp_entry_ttl
                        - 1,
                }),
            },
            contract_entry_change.clone(),
            wasm_entry_change.clone()
        ]
    );

    let extend_host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "extend_temporary",
        vec![key.clone(), u32_sc_val(501), u32_sc_val(5000)],
    );
    let extend_res = invoke_host_function_helper(
        true,
        &extend_host_fn,
        &resources(
            10_000_000,
            vec![
                cd.contract_key.clone(),
                cd.wasm_key.clone(),
                data_key.clone(),
            ],
            vec![],
        ),
        &cd.deployer,
        vec![],
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (new_entry.clone(), Some(ledger_info.sequence_number + 500)),
        ],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(extend_res.invoke_result.unwrap(), ScVal::Void);
    assert!(extend_res.contract_events.is_empty());
    assert_eq!(
        extend_res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: true,
                key: data_key.clone(),
                old_entry_size_bytes: new_entry.to_xdr(Limits::none()).unwrap().len() as u32,
                new_value: None,
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&data_key),
                    durability: ContractDataDurability::Temporary,
                    old_live_until_ledger: ledger_info.sequence_number + 500,
                    new_live_until_ledger: ledger_info.sequence_number + 5000,
                }),
            },
            contract_entry_change.clone(),
            wasm_entry_change.clone()
        ]
    );
    assert!(extend_res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(extend_res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_invoke_contract_with_storage_ops_success_in_recording_mode() {
    let cd = CreateContractData::new([111; 32], CONTRACT_STORAGE);
    let ledger_info = default_ledger_info();
    let key = symbol_sc_val("key");
    let val = u64_sc_val(u64::MAX);
    let host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "put_temporary",
        vec![key.clone(), val.clone()],
    );
    let data_key = contract_data_key(
        &cd.contract_address,
        &key,
        ContractDataDurability::Temporary,
    );
    let res = invoke_host_function_recording_helper(
        true,
        &host_fn,
        &cd.deployer,
        None,
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
        ],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert_eq!(res.invoke_result.unwrap(), ScVal::Void);
    assert!(res.contract_events.is_empty());
    let contract_entry_change = LedgerEntryChangeHelper::no_op_change(
        &cd.contract_entry,
        ledger_info.sequence_number + 1000,
    );
    let wasm_entry_change =
        LedgerEntryChangeHelper::no_op_change(&cd.wasm_entry, ledger_info.sequence_number + 100);
    let new_entry = contract_data_entry(
        &cd.contract_address,
        &key,
        &val,
        ContractDataDurability::Temporary,
    );
    assert_eq!(
        res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: false,
                key: data_key.clone(),
                old_entry_size_bytes: 0,
                new_value: Some(new_entry.clone()),
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&data_key),
                    durability: ContractDataDurability::Temporary,
                    old_live_until_ledger: 0,
                    new_live_until_ledger: ledger_info.sequence_number
                        + ledger_info.min_temp_entry_ttl
                        - 1,
                }),
            },
            contract_entry_change.clone(),
            wasm_entry_change.clone()
        ]
    );
    assert_eq!(
        res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: vec![cd.contract_key.clone(), cd.wasm_key.clone()]
                    .try_into()
                    .unwrap(),
                read_write: vec![data_key.clone()].try_into().unwrap(),
            },
            instructions: 2221742,
            read_bytes: 3084,
            write_bytes: 80,
        }
    );

    let extend_host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "extend_temporary",
        vec![key.clone(), u32_sc_val(501), u32_sc_val(5000)],
    );
    let extend_res = invoke_host_function_recording_helper(
        true,
        &extend_host_fn,
        &cd.deployer,
        None,
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (new_entry.clone(), Some(ledger_info.sequence_number + 500)),
        ],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert_eq!(extend_res.invoke_result.unwrap(), ScVal::Void);
    assert!(extend_res.contract_events.is_empty());
    assert_eq!(
        extend_res.ledger_changes,
        vec![
            LedgerEntryChangeHelper {
                read_only: true,
                key: data_key.clone(),
                old_entry_size_bytes: new_entry.to_xdr(Limits::none()).unwrap().len() as u32,
                new_value: None,
                ttl_change: Some(LedgerEntryLiveUntilChange {
                    key_hash: compute_key_hash(&data_key),
                    durability: ContractDataDurability::Temporary,
                    old_live_until_ledger: ledger_info.sequence_number + 500,
                    new_live_until_ledger: ledger_info.sequence_number + 5000,
                }),
            },
            contract_entry_change.clone(),
            wasm_entry_change.clone()
        ]
    );
    assert_eq!(
        extend_res.resources,
        SorobanResources {
            footprint: LedgerFootprint {
                read_only: vec![
                    data_key.clone(),
                    cd.contract_key.clone(),
                    cd.wasm_key.clone(),
                ]
                .try_into()
                .unwrap(),
                read_write: Default::default(),
            },
            instructions: 2333780,
            read_bytes: 3164,
            write_bytes: 0,
        }
    );
}

#[test]
fn test_invoke_contract_with_storage_ops_success_using_simulation() {
    let cd = CreateContractData::new([111; 32], CONTRACT_STORAGE);
    let ledger_info = default_ledger_info();
    let key = symbol_sc_val("key");
    let val = u64_sc_val(u64::MAX);
    let host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "put_temporary",
        vec![key.clone(), val.clone()],
    );
    let res = invoke_host_function_using_simulation(
        true,
        &host_fn,
        &cd.deployer,
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
        ],
        &prng_seed(),
    )
    .unwrap();
    assert_eq!(res.invoke_result.unwrap(), ScVal::Void);
    let extend_host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "extend_temporary",
        vec![key.clone(), u32_sc_val(501), u32_sc_val(5000)],
    );
    let new_entry = contract_data_entry(
        &cd.contract_address,
        &key,
        &val,
        ContractDataDurability::Temporary,
    );
    let extend_res = invoke_host_function_recording_helper(
        true,
        &extend_host_fn,
        &cd.deployer,
        None,
        &ledger_info,
        vec![
            (
                cd.wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                cd.contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (new_entry, Some(ledger_info.sequence_number + 500)),
        ],
        &prng_seed(),
        None,
    )
    .unwrap();
    assert_eq!(extend_res.invoke_result.unwrap(), ScVal::Void);
}

#[test]
fn test_invoke_contract_without_footprint_fails() {
    let cd = CreateContractData::new([111; 32], CONTRACT_STORAGE);
    let ledger_info = default_ledger_info();
    let host_fn = invoke_contract_host_fn(
        &cd.contract_address,
        "put_temporary",
        vec![symbol_sc_val("key"), u64_sc_val(u64::MAX)],
    );
    let res = invoke_host_function_helper(
        true,
        &host_fn,
        &resources(10_000_000, vec![], vec![]),
        &cd.deployer,
        vec![],
        &ledger_info,
        vec![],
        &prng_seed(),
    )
    .unwrap();
    assert!(HostError::result_matches_err(
        res.invoke_result,
        (ScErrorType::Storage, ScErrorCode::ExceededLimit)
    ));
    assert!(res.ledger_changes.is_empty());
    assert!(res.diagnostic_events.len() >= 1);
    assert!(res.contract_events.is_empty());
    assert!(res.budget.get_cpu_insns_consumed().unwrap() > 0);
    assert!(res.budget.get_mem_bytes_consumed().unwrap() > 0);
}

#[test]
fn test_classic_account_auth_using_simulation() {
    let mut prng = StdRng::from_seed([123; 32]);
    let keys = vec![
        SigningKey::generate(&mut prng),
        SigningKey::generate(&mut prng),
    ];
    let signers = vec![TestSigner::account(&keys[0]), TestSigner::account(&keys[1])];

    let contracts = vec![
        CreateContractData::new([1; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([2; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([3; 32], AUTH_TEST_CONTRACT),
        CreateContractData::new([4; 32], AUTH_TEST_CONTRACT),
    ];

    let tree = AuthContractInvocationNode {
        address: contracts[0].contract_address.clone(),
        children: vec![
            AuthContractInvocationNode {
                address: contracts[1].contract_address.clone(),
                children: vec![AuthContractInvocationNode {
                    address: contracts[2].contract_address.clone(),
                    children: vec![AuthContractInvocationNode {
                        address: contracts[3].contract_address.clone(),
                        children: vec![],
                    }],
                }],
            },
            AuthContractInvocationNode {
                address: contracts[2].contract_address.clone(),
                children: vec![
                    AuthContractInvocationNode {
                        address: contracts[1].contract_address.clone(),
                        children: vec![],
                    },
                    AuthContractInvocationNode {
                        address: contracts[3].contract_address.clone(),
                        children: vec![],
                    },
                ],
            },
        ],
    };
    let source_account = get_account_id([123; 32]);
    let host_fn = auth_contract_invocation(
        vec![
            ScAddress::Account(source_account.clone()),
            signers[0].sc_address(),
            signers[1].sc_address(),
        ],
        tree,
    );
    let ledger_info = default_ledger_info();
    let res = invoke_host_function_using_simulation_with_signers(
        true,
        &host_fn,
        &source_account,
        &ledger_info,
        vec![
            (
                contracts[0].wasm_entry.clone(),
                Some(ledger_info.sequence_number + 100),
            ),
            (
                contracts[0].contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (
                contracts[1].contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (
                contracts[2].contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (
                contracts[3].contract_entry.clone(),
                Some(ledger_info.sequence_number + 1000),
            ),
            (account_entry(&signers[0].account_id()), None),
            (account_entry(&signers[1].account_id()), None),
        ],
        &prng_seed(),
        &signers,
    )
    .unwrap();
    assert!(res.invoke_result.is_ok());
}
