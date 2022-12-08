use std::rc::Rc;

use rand::{thread_rng, RngCore};
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ContractId, CreateContractArgs, HostFunction,
        InstallContractCodeArgs, LedgerEntry, LedgerEntryData, LedgerKey, PublicKey,
        ScContractCode, ScObject, ScVal, ScVec, Uint256,
    },
    Object, RawVal, TryIntoVal,
};

use crate::{
    budget::{Budget, CostType},
    storage::{test_storage::MockSnapshotSource, Storage},
    xdr, Host, HostError, LedgerInfo,
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
        });
        self
    }

    pub(crate) fn enable_model(self, ty: CostType) -> Self {
        self.with_budget(|budget| {
            budget
                .0
                .borrow_mut()
                .cpu_insns
                .get_cost_model_mut(ty)
                .lin_param = 10;
            budget
                .0
                .borrow_mut()
                .mem_bytes
                .get_cost_model_mut(ty)
                .lin_param = 1;
        });
        self
    }

    pub(crate) fn test_account_ledger_key_entry_pair(
        account_id: AccountId,
    ) -> (LedgerKey, LedgerEntry) {
        let lk = LedgerKey::Account(xdr::LedgerKeyAccount {
            account_id: account_id.clone(),
        });
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
        (
            lk,
            Host::ledger_entry_from_data(LedgerEntryData::Account(account_entry)),
        )
    }

    pub(crate) fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub(crate) fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<Object, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_obj(&ScObject::Vec(v))
    }

    pub(crate) fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<RawVal, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Object(Some(ScObject::Vec(v))))
    }

    pub(crate) fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScObject, HostError> {
        Ok(ScObject::Bytes(self.map_err(vals.try_into())?))
    }

    pub(crate) fn test_bin_obj(&self, vals: &[u8]) -> Result<Object, HostError> {
        self.to_host_obj(&self.test_bin_scobj(vals)?)
    }

    pub(crate) fn raw_val_vec_to_sc_vec(&self, v: Vec<RawVal>) -> ScVec {
        let mut res = Vec::<ScVal>::new();
        for val in v {
            res.push(val.try_into_val(self).unwrap());
        }
        res.try_into().unwrap()
    }

    // Registers a contract with provided WASM source and returns the registered
    // contract ID.
    // This relies on the host to have no footprint enforcement.
    pub(crate) fn register_test_contract_wasm(
        &self,
        contract_wasm: &[u8],
    ) -> Result<Object, HostError> {
        self.set_source_account(generate_account_id());
        let wasm_id: RawVal = self
            .invoke_function(HostFunction::InstallContractCode(InstallContractCodeArgs {
                code: contract_wasm
                    .to_vec()
                    .try_into()
                    .map_err(|_| self.err_general("too large wasm"))?,
            }))?
            .try_into_val(self)?;
        let wasm_id = self.hash_from_obj_input("wasm_hash", wasm_id.try_into()?)?;
        let id_obj: RawVal = self
            .invoke_function(HostFunction::CreateContract(CreateContractArgs {
                contract_id: ContractId::SourceAccount(Uint256(generate_bytes_array())),
                source: ScContractCode::WasmRef(wasm_id),
            }))?
            .try_into_val(self)?;
        self.remove_source_account();
        Ok(id_obj.try_into()?)
    }
}

impl Default for LedgerInfo {
    fn default() -> Self {
        Self {
            protocol_version: Default::default(),
            sequence_number: Default::default(),
            timestamp: Default::default(),
            network_passphrase: vec![0; 32],
            base_reserve: Default::default(),
        }
    }
}
