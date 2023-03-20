use core::cmp::min;
use std::rc::Rc;

use soroban_env_common::Env;

use crate::budget::AsBudget;
use crate::xdr::{
    AccountEntry, AccountId, Asset, ContractCodeEntry, ContractDataEntry, Hash, HashIdPreimage,
    HashIdPreimageContractId, HashIdPreimageCreateContractArgs, HashIdPreimageEd25519ContractId,
    HashIdPreimageFromAsset, HashIdPreimageSourceAccountContractId, LedgerEntry, LedgerEntryData,
    LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyContractCode, LedgerKeyContractData,
    LedgerKeyTrustLine, PublicKey, ScContractExecutable, ScHostStorageErrorCode,
    ScHostValErrorCode, ScVal, Signer, SignerKey, ThresholdIndexes, TrustLineAsset, Uint256,
};
use crate::{Host, HostError};

use super::invoker_type::InvokerType;
use super::metered_clone::MeteredClone;

impl Host {
    // Notes on metering: free
    pub fn contract_source_ledger_key(
        &self,
        contract_id: &Hash,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let contract_id = contract_id.metered_clone(self.as_budget())?;
        Ok(Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::LedgerKeyContractExecutable,
        })))
    }

    // Notes on metering: retrieving from storage covered. Rest are free.
    pub(crate) fn retrieve_contract_source_from_storage(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<ScContractExecutable, HostError> {
        let entry = self.0.storage.borrow_mut().get(key, self.as_budget())?;
        match &entry.data {
            LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => match val {
                ScVal::ContractExecutable(code) => Ok(code.clone()),
                _ => Err(self.err_status_msg(
                    ScHostValErrorCode::UnexpectedValType,
                    "ledger entry for contract code does not contain contract executable",
                )),
            },
            _ => Err(self.err_status(ScHostStorageErrorCode::ExpectContractData)),
        }
    }

    pub(crate) fn contract_code_ledger_key(
        &self,
        wasm_hash: &Hash,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let wasm_hash = wasm_hash.metered_clone(self.as_budget())?;
        Ok(Rc::new(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: wasm_hash,
        })))
    }

    pub(crate) fn retrieve_contract_code_from_storage(
        &self,
        wasm_hash: &Hash,
    ) -> Result<ContractCodeEntry, HostError> {
        let key = self.contract_code_ledger_key(wasm_hash)?;
        match &self
            .0
            .storage
            .borrow_mut()
            .get(&key, self.as_budget())?
            .data
        {
            LedgerEntryData::ContractCode(e) => Ok(e.clone()),
            _ => Err(self.err_status(ScHostStorageErrorCode::AccessToUnknownEntry)),
        }
    }

    // Notes on metering: `from_host_obj` and `put` to storage covered, rest are free.
    pub(crate) fn store_contract_source(
        &self,
        contract_source: ScContractExecutable,
        contract_id: Hash,
        key: &Rc<LedgerKey>,
    ) -> Result<(), HostError> {
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key: ScVal::LedgerKeyContractExecutable,
            val: ScVal::ContractExecutable(contract_source),
        });
        self.0.storage.borrow_mut().put(
            key,
            &Host::ledger_entry_from_data(data),
            self.as_budget(),
        )?;
        Ok(())
    }

    // notes on metering: covers the key and salt. Rest are free.
    pub fn id_preimage_from_ed25519(
        &self,
        key: Uint256,
        salt: Uint256,
    ) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::ContractIdFromEd25519(
            HashIdPreimageEd25519ContractId {
                network_id: self
                    .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
                ed25519: key,
                salt,
            },
        ))
    }

    // metering: covered by components
    pub fn id_preimage_from_contract(
        &self,
        contract_id: Hash,
        salt: Uint256,
    ) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::ContractIdFromContract(
            HashIdPreimageContractId {
                network_id: self
                    .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
                contract_id,
                salt,
            },
        ))
    }

    // metering: covered by components
    pub fn id_preimage_from_asset(&self, asset: Asset) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::ContractIdFromAsset(
            HashIdPreimageFromAsset {
                network_id: self
                    .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
                asset,
            },
        ))
    }

    // metering: covered by components
    pub fn id_preimage_from_source_account(
        &self,
        salt: Uint256,
    ) -> Result<HashIdPreimage, HostError> {
        if self.get_invoker_type()? != InvokerType::Account as u64 {
            return Err(self.err_general("invoker is not an account"));
        }

        let source_account = self
            .source_account()
            .ok_or_else(|| self.err_general("unexpected missing invoker in id preimage"))?;
        Ok(HashIdPreimage::ContractIdFromSourceAccount(
            HashIdPreimageSourceAccountContractId {
                network_id: self
                    .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
                source_account,
                salt,
            },
        ))
    }

    // metering: covered by components
    pub fn create_contract_args_hash_preimage(
        &self,
        source: ScContractExecutable,
        salt: Uint256,
    ) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::CreateContractArgs(
            HashIdPreimageCreateContractArgs {
                network_id: self
                    .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
                source,
                salt,
            },
        ))
    }

    // notes on metering: `get` from storage is covered. Rest are free.
    pub fn load_account(&self, account_id: AccountId) -> Result<AccountEntry, HostError> {
        let acc = self.to_account_key(account_id);
        self.with_mut_storage(|storage| match &storage.get(&acc, self.as_budget())?.data {
            LedgerEntryData::Account(ae) => Ok(ae.clone()), // TODO: clone needs to be metered
            _ => Err(self.err_general("not account")),
        })
    }

    pub(crate) fn to_account_key(&self, account_id: AccountId) -> Rc<LedgerKey> {
        Rc::new(LedgerKey::Account(LedgerKeyAccount { account_id }))
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
    ) -> Rc<LedgerKey> {
        Rc::new(LedgerKey::Trustline(LedgerKeyTrustLine {
            account_id,
            asset,
        }))
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
                        return weight
                            .try_into()
                            .map_err(|_| self.err_general("signer weight overflow"));
                    }
                }
            }
            // We didn't find the target signer, return 0 weight to indicate that.
            Ok(0u8)
        }
    }

    pub(crate) fn ledger_entry_from_data(data: LedgerEntryData) -> Rc<LedgerEntry> {
        Rc::new(LedgerEntry {
            // This is modified to the appropriate value on the core side during
            // commiting the ledger transaction.
            last_modified_ledger_seq: 0,
            data,
            ext: LedgerEntryExt::V0,
        })
    }
}
