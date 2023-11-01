use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use rand::RngCore;
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ContractCostType, LedgerEntry, LedgerEntryData, LedgerKey,
        PublicKey, ScAddress, ScErrorCode, ScErrorType, ScVal, ScVec, Uint256,
    },
    AddressObject, BytesObject, Env, EnvBase, Symbol, Val, VecObject,
};

use crate::{
    budget::{AsBudget, Budget},
    host::HostLifecycleEvent,
    storage::{SnapshotSource, Storage},
    xdr, Error, Host, HostError, LedgerInfo,
};


// Test utilities for the host, used in various tests in sub-modules.
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

pub fn generate_account_id(host: &Host) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
        generate_bytes_array(host),
    )))
}

pub fn generate_bytes_array(host: &Host) -> [u8; 32] {
    let mut bytes: [u8; 32] = Default::default();
    host.with_test_prng(|chacha| Ok(chacha.fill_bytes(&mut bytes)))
        .unwrap();
    bytes
}



pub struct MockSnapshotSource(BTreeMap<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>);

impl MockSnapshotSource {
    pub fn new() -> Self {
        Self(BTreeMap::<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>::new())
    }
}
impl SnapshotSource for MockSnapshotSource {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError> {
        if let Some(val) = self.0.get(key) {
            Ok((Rc::clone(&val.0), val.1))
        } else {
            Err(Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::MissingValue).into())
        }
    }

    fn has(&self, key: &Rc<LedgerKey>) -> Result<bool, HostError> {
        Ok(self.0.contains_key(key))
    }
}

impl Host {
    pub const TEST_PRNG_SEED: &[u8; 32] = b"12345678901234567890123456789012";

    pub fn test_host() -> Self {
        let host = Host::default();
        host.set_base_prng_seed(*Host::TEST_PRNG_SEED).unwrap();
        host
    }

    pub fn test_host_with_recording_footprint() -> Self {
        let snapshot_source = Rc::<MockSnapshotSource>::new(MockSnapshotSource::new());
        let storage = Storage::with_recording_footprint(snapshot_source);
        let host = Host::with_storage_and_budget(storage, Budget::default());
        host.set_base_prng_seed(*Host::TEST_PRNG_SEED).unwrap();
        host.set_ledger_info(LedgerInfo {
            protocol_version: crate::meta::get_ledger_protocol_version(
                crate::meta::INTERFACE_VERSION,
            ),
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

    pub fn test_account_ledger_key_entry_pair(
        account_id: AccountId,
    ) -> (Rc<LedgerKey>, Rc<LedgerEntry>) {
        let lk = Rc::new(LedgerKey::Account(xdr::LedgerKeyAccount {
            account_id: account_id.clone(),
        }));
        let account_entry = AccountEntry {
            account_id,
            balance: 100,
            seq_num: xdr::SequenceNumber(0),
            num_sub_entries: 0,
            inflation_dest: None,
            flags: 0,
            home_domain: Default::default(),
            thresholds: xdr::Thresholds([0; 4]),
            signers: Default::default(),
            ext: xdr::AccountEntryExt::V0,
        };
        let le = Rc::new(LedgerEntry {
            last_modified_ledger_seq: 0,
            data: LedgerEntryData::Account(account_entry),
            ext: xdr::LedgerEntryExt::V0,
        });
        (lk, le)
    }

    pub fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<VecObject, HostError> {
        let v = self.test_scvec(vals)?;
        Ok(self.to_host_val(&ScVal::Vec(Some(v)))?.try_into()?)
    }

    pub fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<Val, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Vec(Some(v)))
    }

    pub fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScVal, HostError> {
        Ok(ScVal::Bytes(self.map_err(vals.to_vec().try_into())?))
    }

    pub fn test_bin_obj(&self, vals: &[u8]) -> Result<BytesObject, HostError> {
        let scval: ScVal = self.test_bin_scobj(vals)?;
        let val: Val = self.to_host_val(&scval)?;
        Ok(val.try_into()?)
    }

    // Registers a contract with provided Wasm code and returns the registered
    // contract's address.
    // The contract address deterministically depends on the input account and
    // salt, so this can be used with enforcing ledger footprint (but the
    // footprint still has to be specified outside of this).
    pub fn register_test_contract_wasm_from_source_account(
        &self,
        contract_wasm: &[u8],
        account: AccountId,
        salt: [u8; 32],
    ) -> AddressObject {
        let _span = tracy_span!("register_test_contract_wasm_from_source_account");
        // Use source account-based auth in order to avoid using nonces which
        // won't work well with enforcing ledger footprint.
        let prev_source_account = self.source_account_id().unwrap();
        // Use recording auth to skip specifying the auth payload.
        let prev_auth_manager = self.snapshot_auth_manager().unwrap();
        self.switch_to_recording_auth(true).unwrap();

        let wasm_hash = self
            .upload_wasm(self.bytes_new_from_slice(contract_wasm).unwrap())
            .unwrap();
        self.set_source_account(account.clone()).unwrap();
        let contract_address = self
            .create_contract(
                self.add_host_object(ScAddress::Account(account.clone()))
                    .unwrap(),
                wasm_hash,
                self.bytes_new_from_slice(&salt).unwrap(),
            )
            .unwrap();
        if let Some(prev_account) = prev_source_account {
            self.set_source_account(prev_account).unwrap();
        }
        self.set_auth_manager(prev_auth_manager).unwrap();
        contract_address
    }

    // Registers a contract with provided Wasm code and returns the registered
    // contract's address.
    // The contract address will be generated randomly, so this won't work with
    // enforcing ledger footprint.
    pub fn register_test_contract_wasm(&self, contract_wasm: &[u8]) -> AddressObject {
        self.register_test_contract_wasm_from_source_account(
            contract_wasm,
            generate_account_id(self),
            generate_bytes_array(self),
        )
    }
}
