use std::cmp::min;

use soroban_env_common::{
    xdr::{
        Asset, HashIdPreimageSourceAccountContractId, PublicKey, Signer, SignerKey,
        ThresholdIndexes, TrustLineAsset,
    },
    CheckedEnv, InvokerType,
};

use crate::xdr::{
    AccountEntry, AccountId, ContractDataEntry, Hash, HashIdPreimage, HashIdPreimageContractId,
    HashIdPreimageEd25519ContractId, LedgerEntry, LedgerEntryData, LedgerEntryExt, LedgerKey,
    LedgerKeyAccount, LedgerKeyContractData, LedgerKeyTrustLine, ScContractCode,
    ScHostStorageErrorCode, ScHostValErrorCode, ScObject, ScStatic, ScVal, Uint256,
};
use crate::{Host, HostError};

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

    // notes on metering: `get` from storage is covered. Rest are free.
    pub fn load_account(&self, account_id: AccountId) -> Result<AccountEntry, HostError> {
        let acc = self.to_account_key(account_id);
        self.with_mut_storage(|storage| match storage.get(&acc)?.data {
            LedgerEntryData::Account(ae) => Ok(ae),
            _ => Err(self.err_general("not account")),
        })
    }

    // notes on metering: covered by `has`.
    pub fn has_account(&self, account_id: AccountId) -> Result<bool, HostError> {
        let acc = self.to_account_key(account_id);
        self.with_mut_storage(|storage| storage.has(&acc))
    }

    pub(crate) fn to_account_key(&self, account_id: AccountId) -> LedgerKey {
        LedgerKey::Account(LedgerKeyAccount { account_id })
    }

    pub(crate) fn create_asset_4(&self, asset_code: [u8; 4], issuer: AccountId) -> TrustLineAsset {
        use crate::xdr::{AlphaNum4, AssetCode4};
        TrustLineAsset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(asset_code),
            issuer,
        })
    }

    pub(crate) fn create_asset_12(
        &self,
        asset_code: [u8; 12],
        issuer: AccountId,
    ) -> TrustLineAsset {
        use crate::xdr::{AlphaNum12, AssetCode12};
        TrustLineAsset::CreditAlphanum12(AlphaNum12 {
            asset_code: AssetCode12(asset_code),
            issuer,
        })
    }

    pub(crate) fn to_trustline_key(
        &self,
        account_id: AccountId,
        asset: TrustLineAsset,
    ) -> LedgerKey {
        LedgerKey::Trustline(LedgerKeyTrustLine { account_id, asset })
    }

    pub(crate) fn get_signer_weight_from_account(
        &self,
        target_signer: Uint256,
        account: &AccountEntry,
    ) -> Result<u8, HostError> {
        if account.account_id
            == AccountId(PublicKey::PublicKeyTypeEd25519(
                target_signer.metered_clone(&self.0.budget)?,
            ))
        {
            // Target signer is the master key, so return the master weight
            let threshold = account.thresholds.0[ThresholdIndexes::MasterWeight as usize];
            Ok(threshold)
        } else {
            // Target signer is not the master key, so search the account signers
            let signers: &Vec<Signer> = account.signers.as_ref();
            for signer in signers {
                if let SignerKey::Ed25519(ref this_signer) = signer.key {
                    if &target_signer == this_signer {
                        // Clamp the weight at 255. Stellar protocol before v10
                        // allowed weights to exceed 255, but the max threshold
                        // is 255, hence there is no point in having a larger
                        // weight.
                        let weight = min(signer.weight, u8::MAX as u32);
                        // We've found the target signer in the account signers, so return the weight
                        return Ok(weight
                            .try_into()
                            .map_err(|_| self.err_general("signer weight overflow"))?);
                    }
                }
            }
            // We didn't find the target signer, return 0 weight to indicate that.
            Ok(0u8)
        }
    }
}
