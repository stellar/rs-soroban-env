use crate::{
    budget::CostType,
    host_object::{HostObj, HostVal},
    im_rc::OrdMap,
    storage::{AccessType, Footprint, Storage},
    xdr::{
        ContractDataEntry, Hash, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
        LedgerKeyContractData, ScContractCode, ScObject, ScStatic, ScVal, ScVec,
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

    pub(crate) fn test_budget(self) -> Self {
        self.get_budget_mut(|budget| {
            budget.reset_limits(100_000, 100_000); // something big but finite that we may exceed
            budget.reset_models();
        });
        self
    }

    pub(crate) fn enable_model(self, ty: CostType) -> Self {
        self.get_budget_mut(|budget| {
            budget.cpu_insns.get_cost_model_mut(ty).lin_param = 10;
            budget.mem_bytes.get_cost_model_mut(ty).lin_param = 1;
        });
        self
    }

    pub(crate) fn test_storage_with_contracts(
        ids: Vec<Hash>,
        codes: Vec<&'static [u8]>,
    ) -> Storage {
        let it = ids.iter().zip(codes.iter());
        let mut footprint = Footprint::default();
        let mut map = OrdMap::default();
        for (_, (id, contract)) in it.enumerate() {
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
            map.insert(storage_key.clone(), Some(le));
            footprint.record_access(&storage_key, AccessType::ReadOnly);
        }
        Storage::with_enforcing_footprint_and_map(footprint, map)
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
