use crate::xdr::{
    AccountEntry, AccountId, ContractDataEntry, Hash, HashIdPreimage, HashIdPreimageContractId,
    HashIdPreimageEd25519ContractId, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyAccount, LedgerKeyContractData, LedgerKeyTrustLine, PublicKey, ScContractCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScObject, ScStatic, ScVal, Uint256, WriteXdr,
};
use crate::{Host, HostError, Object};

impl Host {
    pub fn contract_code_ledger_key(&self, contract_id: Hash) -> LedgerKey {
        LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::Static(ScStatic::LedgerKeyContractCode),
        })
    }

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

    pub fn id_preimage_from_ed25519(
        &self,
        key: Uint256,
        salt: Uint256,
    ) -> Result<Vec<u8>, HostError> {
        //Create contract and contractID
        let pre_image = HashIdPreimage::ContractIdFromEd25519(HashIdPreimageEd25519ContractId {
            ed25519: key,
            salt,
        });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| self.err_general("invalid hash"))?;
        Ok(buf)
    }

    pub fn id_preimage_from_contract(
        &self,
        contract_id: Hash,
        salt: Uint256,
    ) -> Result<Vec<u8>, HostError> {
        let pre_image =
            HashIdPreimage::ContractIdFromContract(HashIdPreimageContractId { contract_id, salt });
        let mut buf = Vec::new();
        pre_image
            .write_xdr(&mut buf)
            .map_err(|_| self.err_general("invalid hash"))?;
        Ok(buf)
    }

    pub fn load_account(&self, account_id: Object) -> Result<AccountEntry, HostError> {
        let lk = LedgerKey::Account(LedgerKeyAccount {
            account_id: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(account_id)?)),
        });
        self.visit_storage(|storage| match storage.get(&lk)?.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(self.err_general("not account")),
        })
    }

    pub fn to_trustline_key(
        &self,
        account_id: Object,
        asset_code: Object,
        issuer: Object,
    ) -> Result<LedgerKey, HostError> {
        use crate::xdr::{AlphaNum12, AlphaNum4, AssetCode12, AssetCode4, TrustLineAsset};
        let asset = self.visit_obj(asset_code, |b: &Vec<u8>| {
            if b.len() > 0 && b.len() <= 4 {
                Ok(TrustLineAsset::CreditAlphanum4(AlphaNum4 {
                    asset_code: AssetCode4(b.as_slice().try_into().unwrap()),
                    issuer: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(issuer)?)),
                }))
            } else if b.len() > 0 && b.len() <= 12 {
                Ok(TrustLineAsset::CreditAlphanum12(AlphaNum12 {
                    asset_code: AssetCode12(b.as_slice().try_into().unwrap()),
                    issuer: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(issuer)?)),
                }))
            } else {
                Err(self.err_general("invalid asset code"))
            }
        })?;
        Ok(LedgerKey::Trustline(LedgerKeyTrustLine {
            account_id: AccountId(PublicKey::PublicKeyTypeEd25519(self.to_u256(account_id)?)),
            asset,
        }))
    }
}
