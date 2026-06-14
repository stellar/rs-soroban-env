use crate::{
    storage::InstanceStorageMap,
    xdr::{LedgerEntryData, ScErrorCode, ScErrorType},
    AddressObject, Host, HostError, StorageType, Val,
};

impl Host {
    fn contract_data_entry_value(&self, entry: &LedgerEntryData) -> Result<Val, HostError> {
        match entry {
            LedgerEntryData::ContractData(e) => self.to_valid_host_val(&e.val),
            _ => Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "expected contract data ledger entry",
                &[],
            )),
        }
    }

    pub(crate) fn try_get_contract_data_value(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<Option<Val>, HostError> {
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                let key = self.storage_key_from_val(k, t.try_into()?)?;
                let entry = self
                    .try_borrow_storage_mut()?
                    .try_get(&key, self, Some(k))?;
                entry
                    .map(|entry| self.contract_data_entry_value(&entry.data))
                    .transpose()
            }
            StorageType::Instance => {
                self.with_instance_storage(|s| s.map.get(&k, self).map(|v| v.copied()))
            }
        }
    }

    pub(crate) fn try_get_external_contract_data_value(
        &self,
        contract: AddressObject,
        k: Val,
        t: StorageType,
    ) -> Result<Option<Val>, HostError> {
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                let key = self.storage_key_from_address_val(contract, k, t.try_into()?)?;
                let entry = self
                    .try_borrow_storage_mut()?
                    .try_get(&key, self, Some(k))?;
                entry
                    .map(|entry| self.contract_data_entry_value(&entry.data))
                    .transpose()
            }
            StorageType::Instance => self.try_get_external_instance_data_value(contract, k),
        }
    }

    fn try_get_external_instance_data_value(
        &self,
        contract: AddressObject,
        k: Val,
    ) -> Result<Option<Val>, HostError> {
        let contract_id = self.contract_id_from_address(contract)?;
        let instance_key = self.contract_instance_ledger_key(&contract_id)?;
        let entry = self
            .try_borrow_storage_mut()?
            .try_get(&instance_key, self, None)?;
        let Some(entry) = entry else {
            return Ok(None);
        };
        let instance = self.extract_contract_instance_from_ledger_entry(&entry)?;
        let instance_storage = InstanceStorageMap::from_instance_xdr(&instance, self)?;
        instance_storage
            .map
            .get(&k, self)
            .map(|value| value.copied())
    }
}
