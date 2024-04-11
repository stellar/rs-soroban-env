use crate::vm::ModuleCache;
use crate::vm::ParsedModule;
use crate::xdr::{
    AccountEntry, AccountEntryExt, AccountId, ContractCodeEntry, ContractDataDurability,
    ContractDataEntry, ContractExecutable, ContractIdPreimage, ContractIdPreimageFromAddress,
    CreateContractArgs, ExtensionPoint, HashIdPreimage, HashIdPreimageContractId, HostFunction,
    InvokeContractArgs, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyContractCode, LedgerKeyContractData, Limits, PublicKey, ScAddress, ScBytes,
    ScContractInstance, ScMapEntry, ScSymbol, ScVal, SequenceNumber, SorobanAuthorizationEntry,
    SorobanAuthorizedFunction, SorobanAuthorizedInvocation, SorobanCredentials, Thresholds,
    Uint256, WriteXdr,
};
use crate::xdr::{ContractCodeEntryExt, ContractCodeEntryV1};
use crate::{Host, LedgerInfo};
use sha2::{Digest, Sha256};

pub const DEFAULT_LEDGER_SEQ: u32 = 1_000_000;
pub const DEFAULT_NETWORK_ID: [u8; 32] = [5; 32];

pub fn ledger_entry(le_data: LedgerEntryData) -> LedgerEntry {
    LedgerEntry {
        last_modified_ledger_seq: 0,
        data: le_data,
        ext: LedgerEntryExt::V0,
    }
}

pub fn account_entry(account_id: &AccountId) -> LedgerEntry {
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

pub fn get_wasm_hash(wasm: &[u8]) -> [u8; 32] {
    Sha256::digest(wasm).into()
}

pub fn bytes_sc_val(bytes: &[u8]) -> ScVal {
    ScVal::Bytes(ScBytes(bytes.try_into().unwrap()))
}

/// Helper for creating a non-validated Wasm entry without refined
/// cost inputs.
/// Useful for tests that don't care about the Wasm entry contents.
pub fn wasm_entry_non_validated(wasm: &[u8]) -> LedgerEntry {
    let ext = ContractCodeEntryExt::V0;

    ledger_entry(LedgerEntryData::ContractCode(ContractCodeEntry {
        ext,
        hash: get_wasm_hash(wasm).try_into().unwrap(),
        code: wasm.try_into().unwrap(),
    }))
}

/// Creates a ContractCode ledger entry containing the provided Wasm blob.
/// The entry has the most 'up-to-date' contents possible (such as refined costs
/// in protocol 21).
pub fn wasm_entry(wasm: &[u8]) -> LedgerEntry {
    wasm_entry_with_refined_contract_cost_inputs(
        wasm,
        e2e_test_protocol_version() >= ModuleCache::MIN_LEDGER_VERSION,
    )
}

pub(crate) fn wasm_entry_with_refined_contract_cost_inputs(
    wasm: &[u8],
    add_refined_cost_inputs: bool,
) -> LedgerEntry {
    let ext = if !add_refined_cost_inputs {
        ContractCodeEntryExt::V0
    } else {
        let dummy_host = crate::Host::test_host();
        dummy_host.set_ledger_info(default_ledger_info()).unwrap();
        ContractCodeEntryExt::V1(ContractCodeEntryV1 {
            cost_inputs: ParsedModule::extract_refined_contract_cost_inputs(&dummy_host, wasm)
                .unwrap(),
            ext: ExtensionPoint::V0,
        })
    };
    ledger_entry(LedgerEntryData::ContractCode(ContractCodeEntry {
        ext,
        hash: get_wasm_hash(wasm).try_into().unwrap(),
        code: wasm.try_into().unwrap(),
    }))
}

pub fn e2e_test_protocol_version() -> u32 {
    Host::current_test_protocol()
}

pub fn default_ledger_info() -> LedgerInfo {
    LedgerInfo {
        protocol_version: e2e_test_protocol_version(),
        sequence_number: DEFAULT_LEDGER_SEQ,
        timestamp: 12345678,
        network_id: DEFAULT_NETWORK_ID,
        base_reserve: 5_000_000,
        min_temp_entry_ttl: 16,
        min_persistent_entry_ttl: 100_000,
        max_entry_ttl: 10_000_000,
    }
}

pub struct CreateContractData {
    pub deployer: AccountId,
    pub wasm_key: LedgerKey,
    pub wasm_entry: LedgerEntry,
    pub contract_key: LedgerKey,
    pub contract_entry: LedgerEntry,
    pub contract_address: ScAddress,
    pub auth_entry: SorobanAuthorizationEntry,
    pub host_fn: HostFunction,
}

impl CreateContractData {
    pub fn new(salt: [u8; 32], wasm: &[u8]) -> Self {
        Self::new_with_refined_contract_cost_inputs(
            salt,
            wasm,
            e2e_test_protocol_version() >= ModuleCache::MIN_LEDGER_VERSION,
        )
    }
    pub fn new_with_refined_contract_cost_inputs(
        salt: [u8; 32],
        wasm: &[u8],
        refined_cost_inputs: bool,
    ) -> Self {
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

        let wasm_entry = wasm_entry_with_refined_contract_cost_inputs(wasm, refined_cost_inputs);

        Self {
            deployer,
            wasm_key: get_wasm_key(wasm),
            wasm_entry,
            contract_key,
            contract_entry,
            contract_address,
            auth_entry,
            host_fn,
        }
    }
}

pub fn upload_wasm_host_fn(wasm: &[u8]) -> HostFunction {
    HostFunction::UploadContractWasm(wasm.try_into().unwrap())
}

pub fn get_wasm_key(wasm: &[u8]) -> LedgerKey {
    LedgerKey::ContractCode(LedgerKeyContractCode {
        hash: get_wasm_hash(wasm).try_into().unwrap(),
    })
}
pub fn get_account_id(pub_key: [u8; 32]) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(pub_key.try_into().unwrap()))
}

pub fn get_contract_id_preimage(account_id: &AccountId, salt: &[u8; 32]) -> ContractIdPreimage {
    ContractIdPreimage::Address(ContractIdPreimageFromAddress {
        address: ScAddress::Account(account_id.clone()),
        salt: Uint256(*salt),
    })
}

pub fn get_contract_id_hash(id_preimage: &ContractIdPreimage) -> [u8; 32] {
    let preimage = HashIdPreimage::ContractId(HashIdPreimageContractId {
        network_id: DEFAULT_NETWORK_ID.try_into().unwrap(),
        contract_id_preimage: id_preimage.clone(),
    });
    Sha256::digest(&preimage.to_xdr(Limits::none()).unwrap()).into()
}

pub fn create_contract_auth(
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

#[derive(Clone)]
pub struct AuthContractInvocationNode {
    pub address: ScAddress,
    pub children: Vec<AuthContractInvocationNode>,
}

pub fn sc_struct_field(key: &str, val: ScVal) -> ScMapEntry {
    ScMapEntry {
        key: ScVal::Symbol(key.try_into().unwrap()),
        val,
    }
}

impl AuthContractInvocationNode {
    pub fn into_scval(self, address_count: usize) -> ScVal {
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

    pub fn into_authorized_invocation(self) -> SorobanAuthorizedInvocation {
        let children: Vec<SorobanAuthorizedInvocation> = self
            .children
            .into_iter()
            .map(|c| c.into_authorized_invocation())
            .collect();
        SorobanAuthorizedInvocation {
            function: SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                contract_address: self.address,
                function_name: ScSymbol("tree_fn".try_into().unwrap()),
                args: Default::default(),
            }),
            sub_invocations: children.try_into().unwrap(),
        }
    }
}

pub fn auth_contract_invocation(
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
