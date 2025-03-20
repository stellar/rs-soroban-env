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
        ContractDataEntry, ContractExecutable, ContractIdPreimage, ExtensionPoint, Hash,
        HashIdPreimage, HashIdPreimageContractId, LedgerEntry, LedgerEntryData, LedgerEntryExt,
        LedgerKey, LedgerKeyAccount, LedgerKeyContractCode, LedgerKeyContractData,
        LedgerKeyTrustLine, PublicKey, ScAddress, ScContractInstance, ScErrorCode, ScErrorType,
        ScMap, ScVal, Signer, SignerKey, ThresholdIndexes, TrustLineAsset, Uint256,
    },
    AddressObject, Env, Host, HostError, StorageType, U32Val, Val,
};

impl Host {
    pub fn with_mut_storage<F, U>(&self, f: F) -> Result<U, HostError>
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
        contract_id: &Hash,
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

    // Notes on metering: retrieving from storage covered. Rest are free.
    pub(crate) fn retrieve_contract_instance_from_storage(
        &self,
        key: &Rc<LedgerKey>,
    ) -> Result<ScContractInstance, HostError> {
        let entry = self
            .try_borrow_storage_mut()?
            .get_with_host(key, self, None)?;
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
        match &self
            .try_borrow_storage_mut()?
            .get_with_host(&key, self, None)?
            .data
        {
            LedgerEntryData::ContractCode(e) => {
                let code = e.code.metered_clone(self)?;
                let costs = match &e.ext {
                    crate::xdr::ContractCodeEntryExt::V0 => VersionedContractCodeCostInputs::V0 {
                        wasm_bytes: code.len(),
                    },
                    crate::xdr::ContractCodeEntryExt::V1(v1) => {
                        VersionedContractCodeCostInputs::V1(
                            v1.cost_inputs.metered_clone(self.as_budget())?,
                        )
                    }
                };
                Ok((code, costs))
            }
            _ => Err(err!(
                self,
                (ScErrorType::Storage, ScErrorCode::InternalError),
                "expected ContractCode ledger entry",
                *wasm_hash
            )),
        }
    }

    pub(crate) fn wasm_exists(&self, wasm_hash: &Hash) -> Result<bool, HostError> {
        let key = self.contract_code_ledger_key(wasm_hash)?;
        self.try_borrow_storage_mut()?
            .has_with_host(&key, self, None)
    }

    // Stores the contract instance specified with its parts (executable and
    // storage).
    // When either of parts is `None`, the old value is preserved (when
    // existent).
    // `executable` has to be present for newly created contract instances.
    // Notes on metering: `from_host_obj` and `put` to storage covered, rest are free.
    pub(crate) fn store_contract_instance(
        &self,
        executable: Option<ContractExecutable>,
        instance_storage: Option<ScMap>,
        contract_id: Hash,
        key: &Rc<LedgerKey>,
    ) -> Result<(), HostError> {
        if self
            .try_borrow_storage_mut()?
            .has_with_host(key, self, None)?
        {
            let (current, live_until_ledger) = self
                .try_borrow_storage_mut()?
                .get_with_live_until_ledger(key, self, None)?;
            let mut current = (*current).metered_clone(self)?;

            if let LedgerEntryData::ContractData(ref mut entry) = current.data {
                if let ScVal::ContractInstance(ref mut instance) = entry.val {
                    if let Some(executable) = executable {
                        instance.executable = executable;
                    }
                    if let Some(storage) = instance_storage {
                        instance.storage = Some(storage);
                    }
                } else {
                    return Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "expected ScVal::ContractInstance for contract instance",
                        &[],
                    ));
                }
            } else {
                return Err(self.err(
                    ScErrorType::Storage,
                    ScErrorCode::InternalError,
                    "expected DataEntry for contract instance",
                    &[],
                ));
            }

            self.try_borrow_storage_mut()?.put_with_host(
                &key,
                &Rc::metered_new(current, self)?,
                live_until_ledger,
                self,
                None,
            )?;
        } else {
            let data = ContractDataEntry {
                contract: ScAddress::Contract(contract_id.metered_clone(self)?),
                key: ScVal::LedgerKeyContractInstance,
                val: ScVal::ContractInstance(ScContractInstance {
                    executable: executable.ok_or_else(|| {
                        self.err(
                            ScErrorType::Context,
                            ScErrorCode::InternalError,
                            "can't initialize contract without executable",
                            &[],
                        )
                    })?,
                    storage: instance_storage,
                }),
                durability: ContractDataDurability::Persistent,
                ext: ExtensionPoint::V0,
            };
            self.try_borrow_storage_mut()?.put_with_host(
                key,
                &Host::new_contract_data(self, data)?,
                Some(self.get_min_live_until_ledger(ContractDataDurability::Persistent)?),
                self,
                None,
            )?;
        }
        Ok(())
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

    // notes on metering: `get` from storage is covered. Rest are free.
    pub(crate) fn load_account(&self, account_id: AccountId) -> Result<AccountEntry, HostError> {
        let acc = self.to_account_key(account_id)?;
        self.with_mut_storage(
            |storage| match &storage.get_with_host(&acc, self, None)?.data {
                LedgerEntryData::Account(ae) => ae.metered_clone(self),
                e => Err(err!(
                    self,
                    (ScErrorType::Storage, ScErrorCode::InternalError),
                    "ledger entry is not account",
                    e.name()
                )),
            },
        )
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

    pub(crate) fn modify_ledger_entry_data(
        &self,
        original_entry: &LedgerEntry,
        new_data: LedgerEntryData,
    ) -> Result<Rc<LedgerEntry>, HostError> {
        Rc::metered_new(
            LedgerEntry {
                // This is modified to the appropriate value on the core side during
                // commiting the ledger transaction.
                last_modified_ledger_seq: 0,
                data: new_data,
                ext: original_entry.ext.metered_clone(self)?,
            },
            self,
        )
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
            self.contract_id_from_scaddress(addr.metered_clone(self)?)
        })
    }

    pub(super) fn put_contract_data_into_ledger(
        &self,
        k: Val,
        v: Val,
        t: StorageType,
    ) -> Result<(), HostError> {
        let durability: ContractDataDurability = t.try_into()?;
        let key = self.storage_key_from_val(k, durability)?;
        // Currently the storage stores the whole ledger entries, while this
        // operation might only modify the internal `ScVal` value. Thus we
        // need to only overwrite the value in case if there is already an
        // existing ledger entry value for the key in the storage.
        if self
            .try_borrow_storage_mut()?
            .has_with_host(&key, self, Some(k))?
        {
            let (current, live_until_ledger) = self
                .try_borrow_storage_mut()?
                .get_with_live_until_ledger(&key, self, Some(k))?;
            let mut current = (*current).metered_clone(self)?;
            match current.data {
                LedgerEntryData::ContractData(ref mut entry) => {
                    entry.val = self.from_host_val(v)?;
                }
                _ => {
                    return Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "expected DataEntry",
                        &[],
                    ));
                }
            }
            self.try_borrow_storage_mut()?.put_with_host(
                &key,
                &Rc::metered_new(current, self)?,
                live_until_ledger,
                self,
                Some(k),
            )?;
        } else {
            let data = ContractDataEntry {
                contract: ScAddress::Contract(self.get_current_contract_id_internal()?),
                key: self.from_host_val(k)?,
                val: self.from_host_val(v)?,
                durability,
                ext: ExtensionPoint::V0,
            };
            self.try_borrow_storage_mut()?.put_with_host(
                &key,
                &Host::new_contract_data(self, data)?,
                Some(self.get_min_live_until_ledger(durability)?),
                self,
                Some(k),
            )?;
        }

        Ok(())
    }
}

#[cfg(any(test, feature = "testutils"))]
use crate::crypto;
#[cfg(any(test, feature = "testutils"))]
use crate::storage::{AccessType, Footprint};

#[cfg(any(test, feature = "testutils"))]
impl Host {
    // Writes an arbitrary ledger entry to storage.
    pub fn add_ledger_entry(
        &self,
        key: &Rc<LedgerKey>,
        val: &Rc<soroban_env_common::xdr::LedgerEntry>,
        live_until_ledger: Option<u32>,
    ) -> Result<(), HostError> {
        self.with_mut_storage(|storage| {
            storage.put_with_host(key, val, live_until_ledger, self, None)
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
        self.with_mut_storage(|storage| {
            storage
                .footprint
                .record_access(&key, access_type, self.as_budget())?;
            storage.map = storage.map.insert(key, val, self.as_budget())?;
            Ok(())
        })
    }

    // Performs the necessary setup to access all the entries in provided
    // footprint in enforcing mode.
    // "testutils" are not covered by budget metering.
    pub fn setup_storage_footprint(&self, footprint: Footprint) -> Result<(), HostError> {
        for (key, access_type) in footprint.0.map {
            self.setup_storage_entry(key, None, access_type)?;
        }
        Ok(())
    }

    // Checks whether the given contract has a special 'dummy' executable
    // that marks contracts created with `register_test_contract`.
    pub(crate) fn is_test_contract_executable(
        &self,
        contract_id: &Hash,
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
