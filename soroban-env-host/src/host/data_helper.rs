use soroban_env_common::{
    xdr::{Asset, HashIdPreimageSourceAccountContractId},
    CheckedEnv, InvokerType,
};

use crate::xdr::{
    AccountEntry, AccountId, ContractDataEntry, Hash, HashIdPreimage, HashIdPreimageContractId,
    HashIdPreimageEd25519ContractId, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyAccount, LedgerKeyContractData, LedgerKeyTrustLine, ScContractCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScObject, ScStatic, ScVal, Uint256,
};
use crate::{Host, HostError, Object};

use super::metered_clone::MeteredClone;

impl Host {
    // Notes on metering: free
    pub fn contract_code_ledger_key(&self, contract_id: Hash) -> LedgerKey {
        LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::Static(ScStatic::LedgerKeyContractCode),
        })
    }

    // Notes on metering: retrieving from storage covered. Rest are free.
    pub fn retrieve_contract_code_from_storage(
        &self,
        key: &LedgerKey,
    ) -> Result<ScContractCode, HostError> {
        let scval = match self.0.storage.borrow_mut().get(key)?.data {
            LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
            _ => Err(self.err_status(ScHostStorageErrorCode::ExpectContractData)),
        }?;
        match scval {
            ScVal::Object(Some(ScObject::ContractCode(code))) => Ok(code),
            _ => {
                return Err(self.err_status_msg(
                    ScHostValErrorCode::UnexpectedValType,
                    "ledger entry for contract code does not contain contract code",
                ))
            }
        }
    }

    // Notes on metering: `from_host_obj` and `put` to storage covered, rest are free.
    pub fn store_contract_code(
        &self,
        contract: ScContractCode,
        contract_id: Hash,
        key: &LedgerKey,
    ) -> Result<(), HostError> {
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key: ScVal::Static(ScStatic::LedgerKeyContractCode),
            val: ScVal::Object(Some(ScObject::ContractCode(contract))),
        });
        let val = LedgerEntry {
            last_modified_ledger_seq: 0,
            data,
            ext: LedgerEntryExt::V0,
        };
        self.0.storage.borrow_mut().put(&key, &val)?;
        Ok(())
    }

    // notes on metering: covers the key and salt. Rest are free.
    pub fn id_preimage_from_ed25519(
        &self,
        key: Uint256,
        salt: Uint256,
    ) -> Result<Vec<u8>, HostError> {
        let pre_image = HashIdPreimage::ContractIdFromEd25519(HashIdPreimageEd25519ContractId {
            ed25519: key,
            salt,
        });
        let mut buf = Vec::new();
        self.metered_write_xdr(&pre_image, &mut buf)?;
        Ok(buf)
    }

    // metering: covered by components
    pub fn id_preimage_from_contract(
        &self,
        contract_id: Hash,
        salt: Uint256,
    ) -> Result<Vec<u8>, HostError> {
        let pre_image =
            HashIdPreimage::ContractIdFromContract(HashIdPreimageContractId { contract_id, salt });
        let mut buf = Vec::new();
        self.metered_write_xdr(&pre_image, &mut buf)?;
        Ok(buf)
    }

    // metering: covered by components
    pub fn id_preimage_from_asset(&self, asset: Asset) -> Result<Vec<u8>, HostError> {
        let pre_image = HashIdPreimage::ContractIdFromAsset(asset);
        let mut buf = Vec::new();
        self.metered_write_xdr(&pre_image, &mut buf)?;
        Ok(buf)
    }

    // metering: covered by components
    pub fn id_preimage_from_source_account(&self, salt: Uint256) -> Result<Vec<u8>, HostError> {
        if self.get_invoker_type()? != InvokerType::Account as u64 {
            return Err(self.err_general("invoker is not an account"));
        }

        let source_account = self.source_account()?;
        let pre_image =
            HashIdPreimage::ContractIdFromSourceAccount(HashIdPreimageSourceAccountContractId {
                source_account,
                salt,
            });
        let mut buf = Vec::new();
        self.metered_write_xdr(&pre_image, &mut buf)?;
        Ok(buf)
    }

    // notes on metering: `get` from storage and `to_u256` covered. Rest are free.
    pub fn load_account(&self, a: Object) -> Result<AccountEntry, HostError> {
        let acc = LedgerKey::Account(LedgerKeyAccount {
            account_id: self
                .visit_obj(a, |id: &AccountId| Ok(id.metered_clone(&self.0.budget)?))?,
        });
        self.with_mut_storage(|storage| match storage.get(&acc)?.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(self.err_general("not account")),
        })
    }

    // notes on metering: covered by `has` and `to_u256`.
    pub fn has_account(&self, a: Object) -> Result<bool, HostError> {
        let acc = LedgerKey::Account(LedgerKeyAccount {
            account_id: self
                .visit_obj(a, |id: &AccountId| Ok(id.metered_clone(&self.0.budget)?))?,
        });
        self.with_mut_storage(|storage| storage.has(&acc))
    }

    pub fn to_trustline_key(
        &self,
        account: Object,
        asset_code: Object,
        issuer: Object,
    ) -> Result<LedgerKey, HostError> {
        use crate::xdr::{AlphaNum12, AlphaNum4, AssetCode12, AssetCode4, TrustLineAsset};
        let asset = self.visit_obj(asset_code, |b: &Vec<u8>| {
            if b.len() > 0 && b.len() <= 4 {
                Ok(TrustLineAsset::CreditAlphanum4(AlphaNum4 {
                    asset_code: AssetCode4(
                        b.as_slice()
                            .try_into()
                            .map_err(|_| self.err_general("invalid AssetCode4"))?,
                    ),
                    issuer: self.to_account_id(issuer)?,
                }))
            } else if b.len() > 0 && b.len() <= 12 {
                Ok(TrustLineAsset::CreditAlphanum12(AlphaNum12 {
                    asset_code: AssetCode12(
                        b.as_slice()
                            .try_into()
                            .map_err(|_| self.err_general("invalid AssetCode12"))?,
                    ),
                    issuer: self.to_account_id(issuer)?,
                }))
            } else {
                Err(self.err_general("invalid asset code"))
            }
        })?;
        Ok(LedgerKey::Trustline(LedgerKeyTrustLine {
            account_id: self.to_account_id(account)?,
            asset,
        }))
    }
}
