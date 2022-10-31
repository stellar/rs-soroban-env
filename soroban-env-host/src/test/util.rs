use crate::{
    budget::{Budget, CostType},
    host::metered_map::MeteredOrdMap,
    host_object::{HostObj, HostVal},
    im_rc::OrdMap,
    storage::{AccessType, Footprint, Storage},
    xdr,
    xdr::{
        AccountEntry, AccountId, ContractDataEntry, Hash, LedgerEntry, LedgerEntryData,
        LedgerEntryExt, LedgerKey, LedgerKeyContractData, ScContractCode, ScObject, ScStatic,
        ScVal, ScVec,
    },
    Host, HostError,
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

#[allow(dead_code)]
impl Host {
    pub(crate) fn test_host() -> Self {
        Host::default()
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

    pub(crate) fn test_storage_with_contracts(
        ids: Vec<Hash>,
        codes: Vec<&'static [u8]>,
        budget: Budget,
    ) -> Storage {
        let it = ids.iter().zip(codes.iter());
        let mut footprint = Footprint::default();
        let mut map = OrdMap::default();
        for (id, contract) in it {
            let key = ScVal::Static(ScStatic::LedgerKeyContractCode);
            let storage_key = LedgerKey::ContractData(LedgerKeyContractData {
                contract_id: id.clone(),
                key: key.clone(),
            });
            // We unwrap here rather than host.map_err because the host doesn't exist yet.
            let val = ScVal::Object(Some(ScObject::ContractCode(ScContractCode::Wasm(
                (*contract).try_into().unwrap(),
            ))));
            let le = LedgerEntry {
                last_modified_ledger_seq: 0,
                data: LedgerEntryData::ContractData(ContractDataEntry {
                    contract_id: id.clone(),
                    key,
                    val,
                }),
                ext: LedgerEntryExt::V0,
            };
            map.insert(Box::new(storage_key.clone()), Some(Box::new(le)));
            footprint
                .record_access(&storage_key, AccessType::ReadOnly)
                .unwrap();
        }
        Storage::with_enforcing_footprint_and_map(footprint, MeteredOrdMap { budget, map })
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
        let le = LedgerEntry {
            last_modified_ledger_seq: 0,
            data: LedgerEntryData::Account(account_entry),
            ext: LedgerEntryExt::V0,
        };
        (lk, le)
    }

    pub(crate) fn test_scvec<T: AsScVal>(&self, vals: &[T]) -> Result<ScVec, HostError> {
        let v: Vec<ScVal> = vals.iter().map(|x| x.as_scval()).collect();
        self.map_err(v.try_into())
    }

    pub(crate) fn test_vec_obj<T: AsScVal>(&self, vals: &[T]) -> Result<HostObj, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_obj(&ScObject::Vec(v))
    }

    pub(crate) fn test_vec_val<T: AsScVal>(&self, vals: &[T]) -> Result<HostVal, HostError> {
        let v = self.test_scvec(vals)?;
        self.to_host_val(&ScVal::Object(Some(ScObject::Vec(v))))
    }

    pub(crate) fn test_bin_scobj(&self, vals: &[u8]) -> Result<ScObject, HostError> {
        Ok(ScObject::Bytes(self.map_err(vals.try_into())?))
    }

    pub(crate) fn test_bin_obj(&self, vals: &[u8]) -> Result<HostObj, HostError> {
        self.to_host_obj(&self.test_bin_scobj(vals)?)
    }
}
