use core::cmp::min;
use std::rc::Rc;

use crate::{
    budget::AsBudget,
    err,
    host::metered_clone::{MeteredAlloc, MeteredClone},
    storage::{InstanceStorageMap, Storage},
    vm::VersionedContractCodeCostInputs,
    xdr::{
        AccountEntry, AccountId, Asset, BytesM, ContractCodeEntry, ContractDataDurability,
        ContractDataEntry, ContractExecutable, ContractId, ContractIdPreimage, ExtensionPoint,
        Hash, HashIdPreimage, HashIdPreimageContractId, LedgerEntry, LedgerEntryData,
        LedgerEntryExt, LedgerKey, LedgerKeyAccount, LedgerKeyContractCode, LedgerKeyContractData,
        LedgerKeyTrustLine, PublicKey, ScAddress, ScContractInstance, ScErrorCode, ScErrorType,
        ScMap, ScVal, Signer, SignerKey, ThresholdIndexes, TrustLineAsset, Uint256,
    },
    AddressObject, Env, ErrorHandler, Host, HostError, StorageType, U32Val, Val,
};

impl Host {
    pub(crate) fn with_mut_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Storage) -> Result<U, HostError>,
    {
        f(&mut *self.try_borrow_storage_mut()?)
    }

    /// Immutable accessor to the instance storage of the currently running
    /// contract.
    /// Performs lazy initialization of instance storage on access.
    pub(crate) fn with_instance_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&InstanceStorageMap) -> Result<U, HostError>,
    {
        self.with_current_context_mut(|ctx| {
            self.maybe_init_instance_storage(ctx)?;
            f(ctx.storage.as_ref().ok_or_else(|| {
                self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "missing instance storage",
                    &[],
                )
            })?)
        })
    }

    /// Mutable accessor to the instance storage of the currently running
    /// contract.
    /// Performs lazy initialization of instance storage on access.
    pub(crate) fn with_mut_instance_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut InstanceStorageMap) -> Result<U, HostError>,
    {
        self.with_current_context_mut(|ctx| {
            self.maybe_init_instance_storage(ctx)?;
            let storage = ctx.storage.as_mut().ok_or_else(|| {
                self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "missing instance storage",
                    &[],
                )
            })?;
            // Consider any mutable access to be modifying the instance storage.
            // This way we would provide consistent footprint (RO for read-only
            // ops using `with_instance_storage` and RW for potentially
            // mutating ops using `with_mut_instance_storage`).
            storage.is_modified = true;
            f(storage)
        })
    }

    pub(crate) fn contract_instance_ledger_key(
        &self,
        contract_id: &ContractId,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let contract_id = contract_id.metered_clone(self)?;
        Rc::metered_new(
            LedgerKey::ContractData(LedgerKeyContractData {
                key: ScVal::LedgerKeyContractInstance,
                durability: ContractDataDurability::Persistent,
                contract: ScAddress::Contract(contract_id),
            }),
            self,
        )
    }

    pub(crate) fn extract_contract_instance_from_ledger_entry(
        &self,
        entry: &LedgerEntry,
    ) -> Result<ScContractInstance, HostError> {
        match &entry.data {
            LedgerEntryData::ContractData(e) => match &e.val {
                ScVal::ContractInstance(instance) => instance.metered_clone(self),
                _ => Err(self.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "ledger entry for contract instance does not contain contract instance",
                    &[],
                )),
            },
            _ => Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "expected ContractData ledger entry",
                &[],
            )),
        }
    }

    // Notes on metering: retrieving from storage covered. Rest are free.
    pub(crate) fn retrieve_contract_instance_from_storage(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<ScContractInstance, HostError> {
        self.try_borrow_storage_mut()?
            .with_ledger_entry(key, self, |opt| match opt {
                Some(entry) => self.extract_contract_instance_from_ledger_entry(entry),
                None => Err(self.storage_error_missing_value(key, None)),
            })
    }

    pub(crate) fn contract_code_ledger_key(
        &self,
        wasm_hash: &Hash,
    ) -> Result<Rc<LedgerKey>, HostError> {
        let wasm_hash = wasm_hash.metered_clone(self)?;
        Rc::metered_new(
            LedgerKey::ContractCode(LedgerKeyContractCode { hash: wasm_hash }),
            self,
        )
    }

    pub(crate) fn retrieve_wasm_from_storage(
        &self,
        wasm_hash: &Hash,
    ) -> Result<(BytesM, VersionedContractCodeCostInputs), HostError> {
        let key = self.contract_code_ledger_key(wasm_hash)?;
        self.try_borrow_storage_mut()?
            .with_ledger_entry(&key, self, |opt| match opt {
                Some(entry) => match &entry.data {
                    LedgerEntryData::ContractCode(e) => {
                        let code = e.code.metered_clone(self)?;
                        let costs = match &e.ext {
                            crate::xdr::ContractCodeEntryExt::V0 => {
                                VersionedContractCodeCostInputs::V0 {
                                    wasm_bytes: code.len(),
                                }
                            }
                            crate::xdr::ContractCodeEntryExt::V1(v1) => {
                                VersionedContractCodeCostInputs::V1(
                                    v1.cost_inputs.metered_clone(self.as_budget())?,
                                )
                            }
                        };
                        Ok((code, costs))
                    }
                    e => Err(err!(
                        self,
                        (ScErrorType::Storage, ScErrorCode::InternalError),
                        "ledger entry is not contract code",
                        e.name()
                    )),
                },
                None => Err(self.storage_error_missing_value(&key, None)),
            })
    }

    pub(crate) fn wasm_exists(&self, wasm_hash: &Hash) -> Result<bool, HostError> {
        let key = self.contract_code_ledger_key(wasm_hash)?;
        self.try_borrow_storage_mut()?.has(&key, self, None)
    }

    /// Creates a new contract instance in the ledger.
    ///
    /// The contract must not exist already.
    ///
    /// Notes on metering: covered by create_entry.
    pub(crate) fn create_contract_instance(
        &self,
        executable: ContractExecutable,
        instance_storage: Option<ScMap>,
        contract_id: ContractId,
        key: &Rc<LedgerKey>,
    ) -> Result<(), HostError> {
        let data = ContractDataEntry {
            contract: ScAddress::Contract(contract_id.metered_clone(self)?),
            key: ScVal::LedgerKeyContractInstance,
            val: ScVal::ContractInstance(ScContractInstance {
                executable,
                storage: instance_storage,
            }),
            durability: ContractDataDurability::Persistent,
            ext: ExtensionPoint::V0,
        };
        let entry = Host::new_contract_data(self, data)?;
        let live_until = Some(self.get_min_live_until_ledger(ContractDataDurability::Persistent)?);
        self.try_borrow_storage_mut()?
            .create_entry(key, &entry, live_until, self)?;
        Ok(())
    }

    /// Provides mutable access to a contract instance via a callback.
    ///
    /// Notes on metering: covered by modify_ledger_entry.
    pub(crate) fn modify_contract_instance<F, R>(
        &self,
        key: &Rc<LedgerKey>,
        f: F,
    ) -> Result<R, HostError>
    where
        F: FnOnce(&mut ScContractInstance) -> Result<R, HostError>,
    {
        self.try_borrow_storage_mut()?
            .modify_ledger_entry(key, self, |entry_opt| {
                let entry = entry_opt.ok_or_else(|| {
                    self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "expected contract instance entry to exist",
                        &[],
                    )
                })?;
                if let LedgerEntryData::ContractData(ref mut data_entry) = entry.data {
                    if let ScVal::ContractInstance(ref mut instance) = data_entry.val {
                        f(instance)
                    } else {
                        Err(self.err(
                            ScErrorType::Storage,
                            ScErrorCode::InternalError,
                            "expected ScVal::ContractInstance for contract instance",
                            &[],
                        ))
                    }
                } else {
                    Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "expected ContractData ledger entry",
                        &[],
                    ))
                }
            })
    }

    pub(crate) fn extend_contract_code_ttl_from_contract_id(
        &self,
        instance_key: Rc<LedgerKey>,
        threshold: u32,
        extend_to: u32,
    ) -> Result<(), HostError> {
        match self
            .retrieve_contract_instance_from_storage(&instance_key)?
            .executable
        {
            ContractExecutable::Wasm(wasm_hash) => {
                let key = self.contract_code_ledger_key(&wasm_hash)?;
                self.try_borrow_storage_mut()?
                    .extend_ttl(self, key, threshold, extend_to, None)?;
            }
            ContractExecutable::StellarAsset => {}
        }
        Ok(())
    }

    pub(crate) fn extend_contract_instance_ttl_from_contract_id(
        &self,
        instance_key: Rc<LedgerKey>,
        threshold: u32,
        extend_to: u32,
    ) -> Result<(), HostError> {
        self.try_borrow_storage_mut()?.extend_ttl(
            self,
            instance_key.metered_clone(self)?,
            threshold,
            extend_to,
            None,
        )?;
        Ok(())
    }

    pub(crate) fn extend_contract_code_ttl_v2(
        &self,
        instance_key: &Rc<LedgerKey>,
        extend_to: u32,
        min_extension: u32,
        max_extension: u32,
    ) -> Result<(), HostError> {
        match self
            .retrieve_contract_instance_from_storage(instance_key)?
            .executable
        {
            ContractExecutable::Wasm(wasm_hash) => {
                let key = self.contract_code_ledger_key(&wasm_hash)?;
                self.try_borrow_storage_mut()?.extend_ttl_v2(
                    self,
                    key,
                    extend_to,
                    min_extension,
                    max_extension,
                    None,
                )?;
            }
            ContractExecutable::StellarAsset => {}
        }
        Ok(())
    }

    pub(crate) fn extend_contract_instance_ttl_v2(
        &self,
        instance_key: Rc<LedgerKey>,
        extend_to: u32,
        min_extension: u32,
        max_extension: u32,
    ) -> Result<(), HostError> {
        self.try_borrow_storage_mut()?.extend_ttl_v2(
            self,
            instance_key,
            extend_to,
            min_extension,
            max_extension,
            None,
        )?;
        Ok(())
    }

    // metering: covered by components
    pub(crate) fn get_full_contract_id_preimage(
        &self,
        init_preimage: ContractIdPreimage,
    ) -> Result<HashIdPreimage, HostError> {
        Ok(HashIdPreimage::ContractId(HashIdPreimageContractId {
            network_id: self
                .hash_from_bytesobj_input("network_id", self.get_ledger_network_id()?)?,
            contract_id_preimage: init_preimage,
        }))
    }

    // notes on metering: `with_ledger_entry` from storage is covered.
    pub(crate) fn load_account(&self, account_id: AccountId) -> Result<AccountEntry, HostError> {
        let acc = self.to_account_key(account_id)?;
        self.with_mut_storage(|storage| {
            storage.with_ledger_entry(&acc, self, |opt| match opt {
                Some(entry) => match &entry.data {
                    LedgerEntryData::Account(ae) => ae.metered_clone(self),
                    e => Err(err!(
                        self,
                        (ScErrorType::Storage, ScErrorCode::InternalError),
                        "ledger entry is not account",
                        e.name()
                    )),
                },
                None => Err(self.storage_error_missing_value(&acc, None)),
            })
        })
    }

    pub(crate) fn to_account_key(&self, account_id: AccountId) -> Result<Rc<LedgerKey>, HostError> {
        Rc::metered_new(LedgerKey::Account(LedgerKeyAccount { account_id }), self)
    }

    pub(crate) fn create_asset_4(&self, asset_code: [u8; 4], issuer: AccountId) -> Asset {
        use crate::xdr::{AlphaNum4, AssetCode4};
        Asset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(asset_code),
            issuer,
        })
    }

    pub(crate) fn create_asset_12(&self, asset_code: [u8; 12], issuer: AccountId) -> Asset {
        use crate::xdr::{AlphaNum12, AssetCode12};
        Asset::CreditAlphanum12(AlphaNum12 {
            asset_code: AssetCode12(asset_code),
            issuer,
        })
    }

    pub(crate) fn to_trustline_key(
        &self,
        account_id: AccountId,
        asset: TrustLineAsset,
    ) -> Result<Rc<LedgerKey>, HostError> {
        Rc::metered_new(
            LedgerKey::Trustline(LedgerKeyTrustLine { account_id, asset }),
            self,
        )
    }

    pub(crate) fn get_signer_weight_from_account(
        &self,
        target_signer: Uint256,
        account: &AccountEntry,
    ) -> Result<u8, HostError> {
        if account.account_id
            == AccountId(PublicKey::PublicKeyTypeEd25519(
                target_signer.metered_clone(self)?,
            ))
        {
            // Target signer is the master key, so return the master weight
            let Some(threshold) = account
                .thresholds
                .0
                .get(ThresholdIndexes::MasterWeight as usize)
            else {
                return Err(self.error(
                    (ScErrorType::Value, ScErrorCode::InternalError).into(),
                    "unexpected thresholds-array size",
                    &[],
                ));
            };
            Ok(*threshold)
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
                                &[U32Val::from(weight).to_val()],
                            )
                        });
                    }
                }
            }
            // We didn't find the target signer, return 0 weight to indicate that.
            Ok(0u8)
        }
    }

    pub(crate) fn new_contract_data(
        &self,
        data: ContractDataEntry,
    ) -> Result<Rc<LedgerEntry>, HostError> {
        Rc::metered_new(
            LedgerEntry {
                // This is modified to the appropriate value on the core side during
                // commiting the ledger transaction.
                last_modified_ledger_seq: 0,
                data: LedgerEntryData::ContractData(data),
                ext: LedgerEntryExt::V0,
            },
            self,
        )
    }

    pub(crate) fn new_contract_code(
        &self,
        data: ContractCodeEntry,
    ) -> Result<Rc<LedgerEntry>, HostError> {
        Rc::metered_new(
            LedgerEntry {
                // This is modified to the appropriate value on the core side during
                // commiting the ledger transaction.
                last_modified_ledger_seq: 0,
                data: LedgerEntryData::ContractCode(data),
                ext: LedgerEntryExt::V0,
            },
            self,
        )
    }

    pub(crate) fn contract_id_from_scaddress(
        &self,
        address: ScAddress,
    ) -> Result<ContractId, HostError> {
        match address {
            ScAddress::Contract(contract_id) => Ok(contract_id),
            _ => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::InvalidInput,
                "not a contract address",
                &[],
            )),
        }
    }

    pub(crate) fn contract_id_from_address(
        &self,
        address: AddressObject,
    ) -> Result<ContractId, HostError> {
        self.visit_obj(address, |addr: &ScAddress| {
            self.contract_id_from_scaddress(addr.metered_clone(self)?)
        })
    }

    /// Helper to construct a storage key from a Val and StorageType.
    /// Factors out the common pattern of converting StorageType to durability and building the key.
    pub(crate) fn storage_key_and_durability(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<(Rc<LedgerKey>, ContractDataDurability), HostError> {
        let durability: ContractDataDurability = t.try_into()?;
        let key = self.storage_key_from_val(k, durability)?;
        Ok((key, durability))
    }

    /// Tries to get contract data, returning None if not found.
    pub(crate) fn try_get_contract_data(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<Option<Val>, HostError> {
        let (key, _) = self.storage_key_and_durability(k, t)?;
        self.try_borrow_storage_mut()?
            .with_contract_data_val(&key, self, Some(k), |opt| Ok(opt))
    }

    /// Gets contract data from storage.
    pub(super) fn get_contract_data_from_ledger(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<Val, HostError> {
        let (key, _) = self.storage_key_and_durability(k, t)?;
        self.try_borrow_storage_mut()?
            .with_contract_data_val(&key, self, Some(k), |opt| {
                opt.ok_or_else(|| self.storage_error_missing_value(&key, Some(k)))
            })
    }

    /// Deletes contract data by marking it as deleted in storage.
    /// Uses depth-aware writing for automatic rollback on frame failure.
    pub(super) fn del_contract_data_from_ledger(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<(), HostError> {
        let (key, _) = self.storage_key_and_durability(k, t)?;
        self.try_borrow_storage_mut()?.del(&key, self, Some(k))
    }

    /// Checks if contract data exists in storage.
    /// Reads directly from storage which tracks values at each frame depth.
    pub(super) fn has_contract_data_in_ledger(
        &self,
        k: Val,
        t: StorageType,
    ) -> Result<bool, HostError> {
        let (key, _) = self.storage_key_and_durability(k, t)?;
        self.try_borrow_storage_mut()?.has(&key, self, Some(k))
    }

    /// Extends the TTL for contract data in storage.
    /// Uses the storage layer's extend_ttl which handles depth-aware TTL updates.
    pub(super) fn extend_contract_data_ttl_in_ledger(
        &self,
        k: Val,
        t: StorageType,
        threshold: u32,
        extend_to: u32,
    ) -> Result<(), HostError> {
        let (key, _) = self.storage_key_and_durability(k, t)?;
        self.try_borrow_storage_mut()?
            .extend_ttl(self, key, threshold, extend_to, Some(k))
    }
}

#[cfg(any(test, feature = "testutils"))]
use crate::crypto;
#[cfg(any(test, feature = "testutils"))]
use crate::storage::{AccessType, EntryWithLiveUntil};

#[cfg(any(test, feature = "testutils"))]
impl Host {
    /// Writes an arbitrary ledger entry to storage, overwriting any existing
    /// entry with the same key.
    pub fn add_ledger_entry(
        &self,
        key: &Rc<LedgerKey>,
        val: &Rc<soroban_env_common::xdr::LedgerEntry>,
        live_until_ledger: Option<u32>,
    ) -> Result<(), HostError> {
        use crate::storage::Storage;
        self.with_mut_storage(|storage| {
            Storage::check_supported_ledger_entry_type(val)?;
            // Delete existing entry first if it exists.
            if storage.has(key, self, None)? {
                storage.del(key, self, None)?;
            }
            storage.create_entry(key, val, live_until_ledger, self)
        })
    }

    /// Reads an arbitrary ledger entry from the storage.
    ///
    /// Returns `None` if the entry does not exist.
    pub fn get_ledger_entry(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<Option<EntryWithLiveUntil>, HostError> {
        self.with_mut_storage(|storage| storage.try_get_full(key, self, None))
    }

    /// Returns all the ledger entries stored in the storage as key-value pairs.
    #[allow(clippy::type_complexity)]
    pub fn get_stored_entries(
        &self,
    ) -> Result<Vec<(Rc<LedgerKey>, Option<Rc<LedgerEntry>>)>, HostError> {
        self.with_mut_storage(|storage| {
            Ok(storage
                .map
                .iter_non_metered()
                .map(|(k, storage_entry)| {
                    (
                        Rc::clone(k),
                        storage_entry
                            .current_value(self)
                            .ok()
                            .flatten()
                            .map(|e| e.to_ledger_entry(k, self).unwrap()),
                    )
                })
                .collect())
        })
    }

    // Performs the necessary setup to access the provided ledger key/entry in
    // enforcing storage mode.
    pub fn setup_storage_entry(
        &self,
        key: Rc<LedgerKey>,
        val: Option<(Rc<soroban_env_common::xdr::LedgerEntry>, Option<u32>)>,
        access_type: AccessType,
    ) -> Result<(), HostError> {
        use crate::ledger_info::get_key_durability;
        use crate::storage::{StorageEntry, StorageLedgerEntryData};
        self.with_mut_storage(|storage| {
            // Convert to StorageLedgerEntryData + live_until
            let entry = match val {
                Some((e, live_until)) => {
                    let data = StorageLedgerEntryData::from_ledger_entry(&e, self)?;
                    Some((data, live_until))
                }
                None => None,
            };
            // Access type is embedded in the StorageEntry
            let has_ttl = get_key_durability(&key).is_some();
            let storage_entry = StorageEntry::new(access_type, has_ttl, entry);
            storage.map.insert(key, storage_entry, self.as_budget())?;
            Ok(())
        })
    }

    // Performs the necessary setup to access all the entries in provided
    // footprint entries in enforcing mode.
    // "testutils" are not covered by budget metering.
    pub fn setup_storage_footprint(
        &self,
        footprint_entries: Vec<(Rc<LedgerKey>, AccessType)>,
    ) -> Result<(), HostError> {
        for (key, access_type) in footprint_entries {
            self.setup_storage_entry(key, None, access_type)?;
        }
        Ok(())
    }

    // Checks whether the given contract has a special 'dummy' executable
    // that marks contracts created with `register_test_contract`.
    pub(crate) fn is_test_contract_executable(
        &self,
        contract_id: &ContractId,
    ) -> Result<bool, HostError> {
        let key = self.contract_instance_ledger_key(contract_id)?;
        let instance = self.retrieve_contract_instance_from_storage(&key)?;
        let test_contract_executable = ContractExecutable::Wasm(
            crypto::sha256_hash_from_bytes(&[], self)?
                .try_into()
                .map_err(|_| {
                    self.err(
                        ScErrorType::Value,
                        ScErrorCode::InternalError,
                        "unexpected hash length",
                        &[],
                    )
                })?,
        );
        Ok(test_contract_executable == instance.executable)
    }

    #[cfg(test)]
    pub(crate) fn create_tl_asset_4(
        &self,
        asset_code: [u8; 4],
        issuer: AccountId,
    ) -> TrustLineAsset {
        use crate::xdr::{AlphaNum4, AssetCode4};
        TrustLineAsset::CreditAlphanum4(AlphaNum4 {
            asset_code: AssetCode4(asset_code),
            issuer,
        })
    }

    #[cfg(test)]
    pub(crate) fn create_tl_asset_12(
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
}
