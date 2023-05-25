use core::cmp::min;
use std::rc::Rc;

use soroban_env_common::xdr::{
    BytesM, ContractCodeEntryBody, ContractDataEntryBody, ContractDataEntryData, ContractDataType,
    ContractIdPreimage, ContractLedgerEntryType, HashIdPreimageContractId, ScAddress, ScErrorCode,
    ScErrorType,
};
use soroban_env_common::{AddressObject, Env, U32Val};

use crate::budget::AsBudget;
use crate::xdr::{
    AccountEntry, AccountId, ContractDataEntry, Hash, HashIdPreimage, LedgerEntry, LedgerEntryData,
    LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyContractCode, LedgerKeyContractData,
    LedgerKeyTrustLine, PublicKey, ScContractExecutable, ScVal, Signer, SignerKey,
    ThresholdIndexes, TrustLineAsset, Uint256,
};
use crate::{err, Host, HostError};

use super::metered_clone::MeteredClone;

impl Host {
    // Notes on metering: free
    pub fn contract_executable_ledger_key(
        &self,
        contract_id: &Hash,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let contract_id = contract_id.metered_clone(self.as_budget())?;
        Ok(Rc::new(LedgerKey::ContractData(LedgerKeyContractData {
            contract_id,
            key: ScVal::LedgerKeyContractExecutable,
            type_: ContractDataType::Recreatable,
            le_type: ContractLedgerEntryType::DataEntry,
        })))
    }

    // Notes on metering: retrieving from storage covered. Rest are free.
    pub(crate) fn retrieve_contract_executable_from_storage(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<ScContractExecutable, HostError> {
        let entry = self.0.storage.borrow_mut().get(key, self.as_budget())?;
        match &entry.data {
            LedgerEntryData::ContractData(ContractDataEntry { body, .. }) => match body {
                ContractDataEntryBody::DataEntry(data) => match &data.val {
                    ScVal::ContractExecutable(code) => Ok(code.clone()),
                    other => Err(err!(
                        self,
                        (ScErrorType::Storage, ScErrorCode::UnexpectedType),
                        "ledger entry for contract code does not contain contract executable",
                        *other
                    )),
                },
                _ => Err(err!(
                    self,
                    (ScErrorType::Storage, ScErrorCode::UnexpectedType),
                    "expected DataEntry",
                )),
            },
            _ => Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::UnexpectedType,
                "expected ContractData ledger entry",
                &[],
            )),
        }
    }

    pub(crate) fn wasm_ledger_key(&self, wasm_hash: &Hash) -> Result<Rc<LedgerKey>, HostError> {
        let wasm_hash = wasm_hash.metered_clone(self.as_budget())?;
        Ok(Rc::new(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: wasm_hash,
            le_type: ContractLedgerEntryType::DataEntry,
        })))
    }

    pub(crate) fn retrieve_wasm_from_storage(&self, wasm_hash: &Hash) -> Result<BytesM, HostError> {
        let key = self.wasm_ledger_key(wasm_hash)?;
        match &self
            .0
            .storage
            .borrow_mut()
            .get(&key, self.as_budget())?
            .data
        {
            LedgerEntryData::ContractCode(e) => match &e.body {
                ContractCodeEntryBody::DataEntry(code) => Ok(code.clone()),
                _ => Err(err!(
                    self,
                    (ScErrorType::Storage, ScErrorCode::UnexpectedType),
                    "expected DataEntry",
                )),
            },
            _ => Err(err!(
                self,
                (ScErrorType::Storage, ScErrorCode::UnexpectedType),
                "expected ContractCode ledger entry",
                *wasm_hash
            )),
        }
    }

    pub(crate) fn contract_code_exists(&self, wasm_hash: &Hash) -> Result<bool, HostError> {
        let key = self.wasm_ledger_key(wasm_hash)?;
        self.0.storage.borrow_mut().has(&key, self.as_budget())
    }

    // Notes on metering: `from_host_obj` and `put` to storage covered, rest are free.
    pub(crate) fn store_contract_executable(
        &self,
        executable: ScContractExecutable,
        contract_id: Hash,
        key: &Rc<LedgerKey>,
    ) -> Result<(), HostError> {
        let body = ContractDataEntryBody::DataEntry(ContractDataEntryData {
            val: ScVal::ContractExecutable(executable),
            flags: 0,
        });
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id,
            key: ScVal::LedgerKeyContractExecutable,
            body,
            type_: ContractDataType::Recreatable,
            expiration_ledger_seq: self.with_ledger_info(|li| Ok(li.sequence_number))? + 4096, //TODO: use constant? Ideally we'd use the settings
        });
        self.0.storage.borrow_mut().put(
            key,
            &Host::ledger_entry_from_data(data),
            self.as_budget(),
        )?;
        Ok(())
    }

    // metering: covered by components
    pub fn get_full_contract_id_preimage(
        &self,
        init_preimage: ContractIdPreimage,
    ) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::ContractId(HashIdPreimageContractId {
            network_id: self
                .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
            contract_id_preimage: init_preimage,
        }))
    }

    // notes on metering: `get` from storage is covered. Rest are free.
    pub fn load_account(&self, account_id: AccountId) -> Result<AccountEntry, HostError> {
        let acc = self.to_account_key(account_id);
        self.with_mut_storage(|storage| match &storage.get(&acc, self.as_budget())?.data {
            LedgerEntryData::Account(ae) => ae.metered_clone(self.as_budget()),
            e => Err(err!(
                self,
                (ScErrorType::Storage, ScErrorCode::UnexpectedType),
                "ledger entry is not account",
                e.name()
            )),
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
                        return weight.try_into().map_err(|_| {
                            self.err(
                                ScErrorType::Auth,
                                ScErrorCode::ArithDomain,
                                "signer weight does not fit in u8",
                                &[U32Val::from(weight).to_raw()],
                            )
                        });
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

    pub(crate) fn contract_id_from_scaddress(&self, address: ScAddress) -> Result<Hash, HostError> {
        match address {
            ScAddress::Account(_) => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::InvalidInput,
                "not a contract address",
                &[],
            )),
            ScAddress::Contract(contract_id) => Ok(contract_id),
        }
    }

    pub(crate) fn contract_id_from_address(
        &self,
        address: AddressObject,
    ) -> Result<Hash, HostError> {
        self.visit_obj(address, |addr: &ScAddress| {
            self.contract_id_from_scaddress(addr.metered_clone(self.budget_ref())?)
        })
    }
}
