use std::rc::Rc;

use rand::{thread_rng, RngCore};
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ContractCostType, ContractExecutable, ContractIdPreimage,
        ContractIdPreimageFromAddress, CreateContractArgs, HostFunction, LedgerEntry,
        LedgerEntryData, LedgerKey, PublicKey, ScAddress, ScVal, ScVec, Uint256,
    },
    AddressObject, BytesObject, TryIntoVal, Val, VecObject,
};

use crate::{
    budget::Budget,
    storage::{test_storage::MockSnapshotSource, Storage},
    xdr, Host, HostError,
};

// Test utilities for the host, used in various tests in sub-modules.
pub(crate) trait AsScVal {
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

pub(crate) fn generate_account_id() -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
        generate_bytes_array(),
    )))
}

pub(crate) fn generate_bytes_array() -> [u8; 32] {
    let mut bytes: [u8; 32] = Default::default();
    thread_rng().fill_bytes(&mut bytes);
    bytes
}

#[allow(dead_code)]
impl Host {
    pub(crate) fn test_host() -> Self {
        Host::default()
    }

    pub(crate) fn test_host_with_recording_footprint() -> Self {
        let snapshot_source = Rc::<MockSnapshotSource>::new(MockSnapshotSource::new());
        let storage = Storage::with_recording_footprint(snapshot_source);
        let host = Host::with_storage_and_budget(storage, Budget::default());
        host.set_ledger_info(Default::default());
        host
    }

    pub(crate) fn test_budget(self, cpu: u64, mem: u64) -> Self {
        self.with_budget(|budget| {
            budget.reset_limits(cpu, mem); // something big but finite that we may exceed
            budget.reset_models();
            Ok(())
        })
        .unwrap();
        self
    }

    pub(crate) fn enable_model(
        self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: u64,
        const_mem: u64,
        lin_mem: u64,
    ) -> Self {
        self.with_budget(|budget| {
            budget
                .0
                .borrow_mut()
                .cpu_insns
                .get_cost_model_mut(ty)
                .const_term = const_cpu as i64;
            budget
                .0
                .borrow_mut()
                .cpu_insns
                .get_cost_model_mut(ty)
                .linear_term = lin_cpu as i64;

            budget
                .0
                .borrow_mut()
                .mem_bytes
                .get_cost_model_mut(ty)
                .const_term = const_mem as i64;
            budget
                .0
                .borrow_mut()
                .mem_bytes
                .get_cost_model_mut(ty)
                .linear_term = lin_mem as i64;
            Ok(())
        })
        .unwrap();
        self
    }

    pub(crate) fn test_account_ledger_key_entry_pair(
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
        let le = Host::ledger_entry_from_data(LedgerEntryData::Account(account_entry));
        (lk, le)
    }

    pub(crate) fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub(crate) fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<VecObject, HostError> {
        let v = self.test_scvec(vals)?;
        Ok(self.to_host_val(&ScVal::Vec(Some(v)))?.try_into()?)
    }

    pub(crate) fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<Val, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Vec(Some(v)))
    }

    pub(crate) fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScVal, HostError> {
        Ok(ScVal::Bytes(self.map_err(vals.to_vec().try_into())?))
    }

    pub(crate) fn test_bin_obj(&self, vals: &[u8]) -> Result<BytesObject, HostError> {
        let scval: ScVal = self.test_bin_scobj(vals)?;
        let rawval: Val = self.to_host_val(&scval)?;
        Ok(rawval.try_into()?)
    }

    // Registers a contract with provided WASM source and returns the registered
    // contract's address.
    // This relies on the host to have no footprint enforcement.
    pub(crate) fn register_test_contract_wasm(&self, contract_wasm: &[u8]) -> AddressObject {
        let prev_auth_manager = self.snapshot_auth_manager();
        self.switch_to_recording_auth();

        let wasm_id: Val = self
            .invoke_function(HostFunction::UploadContractWasm(
                contract_wasm.to_vec().try_into().unwrap(),
            ))
            .unwrap()
            .try_into_val(self)
            .unwrap();

        let wasm_id = self
            .hash_from_bytesobj_input("wasm_hash", wasm_id.try_into().unwrap())
            .unwrap();
        let address_obj: Val = self
            .invoke_function(HostFunction::CreateContract(CreateContractArgs {
                contract_id_preimage: ContractIdPreimage::Address(ContractIdPreimageFromAddress {
                    address: ScAddress::Contract(xdr::Hash(generate_bytes_array())),
                    salt: Uint256(generate_bytes_array()),
                }),
                executable: ContractExecutable::Wasm(wasm_id),
            }))
            .unwrap()
            .try_into_val(self)
            .unwrap();
        self.set_auth_manager(prev_auth_manager);
        address_obj.try_into().unwrap()
    }
}
