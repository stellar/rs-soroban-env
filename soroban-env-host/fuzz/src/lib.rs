mod debug_log;
pub use debug_log::*;

use sha2::Sha256;
use soroban_env_host::{
    budget::Budget,
    meta,
    storage::{AccessType, Footprint, FootprintMap, FootprintMode, Storage, StorageMap},
    xdr::{
        AccountId, ContractCodeEntry, ContractDataDurability, ContractDataEntry,
        ContractExecutable, ExtensionPoint, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt,
        LedgerKey, LedgerKeyContractCode, LedgerKeyContractData, ScAddress, ScContractInstance,
        ScVal, ScVec, Uint256,
    },
    Host, LedgerInfo,
};
use std::rc::Rc;

pub trait AsScVal {
    fn as_scval(&self) -> ScVal;
}

impl AsScVal for u32 {
    fn as_scval(&self) -> ScVal {
        ScVal::U32(*self)
    }
}

impl AsScVal for i32 {
    fn as_scval(&self) -> ScVal {
        ScVal::I32(*self)
    }
}

impl AsScVal for ScVec {
    fn as_scval(&self) -> ScVal {
        ScVal::Vec(Some(self.clone()))
    }
}

pub fn test_scvec<T: AsScVal>(vals: &[T]) -> Vec<ScVal> {
    vals.iter().map(|x| x.as_scval()).collect()
}

pub const TEST_PRNG_SEED: &'static [u8; 32] = b"12345678901234567890123456789012";

pub const TEST_CONTRACT_ID: &'static [u8; 32] = &[0; 32];

pub const TEST_SOURCE_ACCOUNT: &'static [u8; 32] = &[0; 32];

pub const TEST_FN_NAME: &'static str = "test";

pub fn storage_with_test_contract(contract_code: Vec<u8>, budget: &Budget) -> Storage {
    let contract_code_hash: [u8; 32] = <Sha256 as sha2::Digest>::digest(contract_code.as_slice())
        .try_into()
        .unwrap();
    let contract_code_lk = LedgerKey::ContractCode(LedgerKeyContractCode {
        hash: Hash(contract_code_hash),
    });

    let contract_code_le = LedgerEntry {
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractCode(ContractCodeEntry {
            hash: Hash(contract_code_hash),
            ext: ExtensionPoint::V0,
            code: contract_code.try_into().unwrap(),
        }),
        ext: LedgerEntryExt::V0,
    };

    let contract_id_hash = Hash(TEST_CONTRACT_ID.clone());
    let contract_instance_lk = LedgerKey::ContractData(LedgerKeyContractData {
        key: ScVal::LedgerKeyContractInstance,
        durability: ContractDataDurability::Persistent,
        contract: ScAddress::Contract(contract_id_hash.clone()),
    });

    let contract_instance_le = LedgerEntry {
        // This is modified to the appropriate value on the core side during
        // commiting the ledger transaction.
        last_modified_ledger_seq: 0,
        data: LedgerEntryData::ContractData(ContractDataEntry {
            contract: ScAddress::Contract(contract_id_hash.clone()),
            key: ScVal::LedgerKeyContractInstance,
            val: ScVal::ContractInstance(ScContractInstance {
                executable: ContractExecutable::Wasm(Hash(contract_code_hash)),
                storage: None,
            }),
            durability: ContractDataDurability::Persistent,
            ext: ExtensionPoint::V0,
        }),
        ext: LedgerEntryExt::V0,
    };

    let fpm = FootprintMap::from_map(
        vec![
            (Rc::new(contract_instance_lk.clone()), AccessType::ReadOnly),
            (Rc::new(contract_code_lk.clone()), AccessType::ReadOnly),
        ],
        &budget,
    )
    .unwrap();
    let footprint = Footprint(fpm);
    let map = StorageMap::from_map(
        vec![
            (
                Rc::new(contract_instance_lk),
                Some((Rc::new(contract_instance_le), Some(u32::MAX))),
            ),
            (
                Rc::new(contract_code_lk),
                Some((Rc::new(contract_code_le), Some(u32::MAX))),
            ),
        ],
        &budget,
    )
    .unwrap();

    Storage {
        footprint,
        mode: FootprintMode::Enforcing,
        map,
    }
}

pub fn test_host_with_storage_and_budget(storage: Storage, budget: Budget) -> Host {
    let host = Host::with_storage_and_budget(storage, budget);
    host.set_source_account(AccountId(
        soroban_env_host::xdr::PublicKey::PublicKeyTypeEd25519(Uint256(
            TEST_SOURCE_ACCOUNT.clone(),
        )),
    ))
    .unwrap();
    host.set_base_prng_seed(*TEST_PRNG_SEED).unwrap();
    host.set_ledger_info(LedgerInfo {
        protocol_version: meta::get_ledger_protocol_version(meta::INTERFACE_VERSION),
        sequence_number: 0,
        timestamp: 0,
        network_id: [0; 32],
        base_reserve: 0,
        min_persistent_entry_ttl: 4096,
        min_temp_entry_ttl: 16,
        max_entry_ttl: 6_312_000,
    })
    .unwrap();
    host
}
