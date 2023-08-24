use std::rc::Rc;

use rand::{thread_rng, RngCore};
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ContractCostType, LedgerEntry, LedgerEntryData, LedgerKey,
        PublicKey, ScAddress, ScVal, ScVec, Uint256,
    },
    AddressObject, BytesObject, Env, EnvBase, Symbol, Val, VecObject,
};
use soroban_synth_wasm::{Arity, ModEmitter, Operand};

use crate::{
    budget::{AsBudget, Budget},
    storage::{test_storage::MockSnapshotSource, Storage},
    xdr, Host, HostError, LedgerInfo,
};

use soroban_bench_utils::HostTracker;

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

pub(crate) fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(1), 0);
    let arg = fe.args[0];
    fe.push(Operand::Const64(1));
    for i in 0..n {
        fe.push(arg);
        fe.push(Operand::Const64(i as i64));
        fe.i64_mul();
        fe.i64_add();
    }
    fe.drop();
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
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
        host.set_ledger_info(LedgerInfo {
            protocol_version: 20,
            sequence_number: 0,
            timestamp: 0,
            network_id: [0; 32],
            base_reserve: 0,
            min_persistent_entry_expiration: 4096,
            min_temp_entry_expiration: 16,
            max_entry_expiration: 6_312_000,
        })
        .unwrap();
        host
    }

    pub(crate) fn test_budget(self, cpu: u64, mem: u64) -> Self {
        self.with_budget(|budget| {
            budget.reset_limits(cpu, mem)?; // something big but finite that we may exceed
            budget.reset_models()?;
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
            budget.override_model_with_unscaled_params(ty, const_cpu, lin_cpu, const_mem, lin_mem)
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
        let le = Rc::new(LedgerEntry {
            last_modified_ledger_seq: 0,
            data: LedgerEntryData::Account(account_entry),
            ext: xdr::LedgerEntryExt::V0,
        });
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

    // Registers a contract with provided Wasm code and returns the registered
    // contract's address.
    // The contract address deterministically depends on the input account and
    // salt, so this can be used with enforcing ledger footprint (but the
    // footprint still has to be specified outside of this).
    pub(crate) fn register_test_contract_wasm_from_source_account(
        &self,
        contract_wasm: &[u8],
        account: AccountId,
        salt: [u8; 32],
    ) -> AddressObject {
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
    pub(crate) fn register_test_contract_wasm(&self, contract_wasm: &[u8]) -> AddressObject {
        self.register_test_contract_wasm_from_source_account(
            contract_wasm,
            generate_account_id(),
            generate_bytes_array(),
        )
    }

    pub(crate) fn measured_call(
        &self,
        contract: AddressObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<Val, HostError> {
        let budget = self.as_budget();
        budget.reset_unlimited()?;
        let ht = HostTracker::start(None);

        let val = self.call(contract, func, args);
        let cpu_consumed = budget
            .get_cpu_insns_consumed()
            .expect("unable to retrieve cpu consumed");
        let mem_consumed = budget
            .get_mem_bytes_consumed()
            .expect("unable to retrieve mem consumed");

        let (cpu_insns, mem_bytes, _) = ht.stop();
        println!(
                "\nmodel cpu consumed: {}, actual cpu insns {} \nmodel mem consumed: {}, actual mem bytes {}",
                cpu_consumed, cpu_insns, mem_consumed, mem_bytes
            );

        val
    }
}
