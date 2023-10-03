use crate::{
    budget::AsBudget,
    err,
    host::{
        metered_clone::{MeteredAlloc, MeteredClone},
        metered_write_xdr, ContractReentryMode, CreateContractArgs,
    },
    xdr::{
        Asset, ContractCodeEntry, ContractDataDurability, ContractExecutable, ContractIdPreimage,
        ContractIdPreimageFromAddress, ExtensionPoint, Hash, LedgerKey, LedgerKeyContractCode,
        ScAddress, ScErrorCode, ScErrorType,
    },
    AddressObject, BytesObject, Host, HostError, Symbol, TryFromVal, Vm,
};
use std::rc::Rc;

impl Host {
    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: Hash,
        contract_executable: ContractExecutable,
    ) -> Result<(), HostError> {
        let storage_key = self.contract_instance_ledger_key(&contract_id)?;
        if self
            .try_borrow_storage_mut()?
            .has(&storage_key, self.as_budget())
            .map_err(|e| self.decorate_contract_instance_storage_error(e, &contract_id))?
        {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::ExistingValue,
                "contract already exists",
                &[self
                    .add_host_object(self.scbytes_from_hash(&contract_id)?)?
                    .into()],
            ));
        }
        // Make sure the contract code exists. Without this check it would be
        // possible to accidentally create a contract that never may be invoked
        // (just by providing a bad hash).
        if let ContractExecutable::Wasm(wasm_hash) = &contract_executable {
            if !self.wasm_exists(wasm_hash)? {
                return Err(err!(
                    self,
                    (ScErrorType::Storage, ScErrorCode::MissingValue),
                    "Wasm does not exist",
                    *wasm_hash
                ));
            }
        }
        self.store_contract_instance(Some(contract_executable), None, contract_id, &storage_key)?;
        Ok(())
    }

    fn maybe_initialize_asset_token(
        &self,
        contract_id: &Hash,
        id_preimage: &ContractIdPreimage,
    ) -> Result<(), HostError> {
        if let ContractIdPreimage::Asset(asset) = id_preimage {
            let mut asset_bytes: Vec<u8> = Default::default();
            metered_write_xdr(self.budget_ref(), asset, &mut asset_bytes)?;
            self.call_n_internal(
                contract_id,
                Symbol::try_from_val(self, &"init_asset")?,
                &[self
                    .add_host_object(self.scbytes_from_vec(asset_bytes)?)?
                    .into()],
                ContractReentryMode::Prohibited,
                false,
            )?;
            Ok(())
        } else {
            Ok(())
        }
    }

    pub(crate) fn create_contract_internal(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgs,
    ) -> Result<AddressObject, HostError> {
        let has_deployer = deployer.is_some();
        if has_deployer {
            self.try_borrow_authorization_manager()?
                .push_create_contract_host_fn_frame(self, args.metered_clone(self)?)?;
        }
        // Make sure that even in case of operation failure we still pop the
        // stack frame.
        // This is hacky, but currently this is the only instance where we need
        // to manually manage auth manager frames (we don't need to authorize
        // any other host fns and it doesn't seem useful to create extra frames
        // for them just to make auth work in a single case).
        let res = self.create_contract_with_optional_auth(deployer, args);
        if has_deployer {
            self.try_borrow_authorization_manager()?.pop_frame(self)?;
        }
        res
    }

    fn create_contract_with_optional_auth(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgs,
    ) -> Result<AddressObject, HostError> {
        if let Some(deployer_address) = deployer {
            self.try_borrow_authorization_manager()?.require_auth(
                self,
                deployer_address,
                Default::default(),
            )?;
        }

        let id_preimage =
            self.get_full_contract_id_preimage(args.contract_id_preimage.metered_clone(self)?)?;
        let hash_id = Hash(self.metered_hash_xdr(&id_preimage)?);
        self.create_contract_with_id(hash_id.metered_clone(self)?, args.executable)?;
        self.maybe_initialize_asset_token(&hash_id, &args.contract_id_preimage)?;
        self.add_host_object(ScAddress::Contract(hash_id))
    }

    pub(crate) fn get_contract_id_hash(
        &self,
        deployer: AddressObject,
        salt: BytesObject,
    ) -> Result<Hash, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| addr.metered_clone(self))?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });

        let id_preimage =
            self.get_full_contract_id_preimage(contract_id_preimage.metered_clone(self)?)?;
        Ok(Hash(self.metered_hash_xdr(&id_preimage)?))
    }

    pub(crate) fn get_asset_contract_id_hash(&self, asset: Asset) -> Result<Hash, HostError> {
        let id_preimage = self.get_full_contract_id_preimage(ContractIdPreimage::Asset(asset))?;
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        Ok(Hash(id_arr))
    }

    pub(crate) fn upload_contract_wasm(&self, wasm: Vec<u8>) -> Result<BytesObject, HostError> {
        let hash_bytes: [u8; 32] = crypto::sha256_hash_from_bytes(wasm.as_slice(), self)?
            .try_into()
            .map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InternalError,
                    "unexpected hash length",
                    &[],
                )
            })?;

        // Check size before instantiation.
        let wasm_bytes_m: crate::xdr::BytesM = wasm.try_into().map_err(|_| {
            self.err(
                ScErrorType::Value,
                ScErrorCode::ExceededLimit,
                "Wasm code is too large",
                &[],
            )
        })?;

        // Instantiate a temporary / throwaway VM using this wasm. This will do
        // both quick checks like "does this wasm have the right protocol number
        // to run on this network" and also a full parse-and-link pass to check
        // that the wasm is basically not garbage. It might still fail to run
        // but it will at least instantiate. This might seem a bit heavyweight
        // but really "instantiating a VM" is mostly just "parsing the module
        // and doing those checks" anyway. Revisit in the future if you want to
        // try to split these costs up some.
        if cfg!(any(test, feature = "testutils")) && wasm_bytes_m.as_slice().is_empty() {
            // Allow a zero-byte contract when testing, as this is used to make
            // native test contracts behave like wasm. They will never be
            // instantiated, this is just to exercise their storage logic.
        } else {
            let _check_vm = Vm::new(
                self,
                Hash(hash_bytes.metered_clone(self)?),
                wasm_bytes_m.as_slice(),
            )?;
        }

        let hash_obj = self.add_host_object(self.scbytes_from_slice(hash_bytes.as_slice())?)?;
        let code_key = Rc::metered_new(
            LedgerKey::ContractCode(LedgerKeyContractCode {
                hash: Hash(hash_bytes.metered_clone(self)?),
            }),
            self,
        )?;
        if !self
            .try_borrow_storage_mut()?
            .has(&code_key, self.as_budget())
            .map_err(|e| self.decorate_contract_code_storage_error(e, &Hash(hash_bytes)))?
        {
            self.with_mut_storage(|storage| {
                let data = ContractCodeEntry {
                    hash: Hash(hash_bytes),
                    ext: ExtensionPoint::V0,
                    code: wasm_bytes_m,
                };
                storage.put(
                    &code_key,
                    &Host::new_contract_code(self, data)?,
                    Some(self.get_min_expiration_ledger(ContractDataDurability::Persistent)?),
                    self.as_budget(),
                )
            })?;
        }
        Ok(hash_obj)
    }
}

use super::crypto;
#[cfg(any(test, feature = "testutils"))]
use super::ContractFunctionSet;

// "testutils" is not covered by budget metering.
#[cfg(any(test, feature = "testutils"))]
impl Host {
    pub fn register_test_contract(
        &self,
        contract_address: AddressObject,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        let contract_id = self.contract_id_from_address(contract_address)?;
        let instance_key = self.contract_instance_ledger_key(&contract_id)?;
        // Test contract might be overriding an already registered Wasm
        // contract, in which case we should preserve the instance entry.
        if self
            .retrieve_contract_instance_from_storage(&instance_key)
            .is_err()
        {
            // Use empty Wasm as default executable. It shouldn't be observable,
            // but allows exercising bump logic and make operations like Wasm
            // update more meaningful.
            let wasm_hash_obj = self.upload_contract_wasm(vec![])?;
            let wasm_hash = self.hash_from_bytesobj_input("wasm_hash", wasm_hash_obj)?;
            self.store_contract_instance(
                Some(ContractExecutable::Wasm(wasm_hash)),
                None,
                contract_id.clone(),
                &instance_key,
            )?;
        };
        let mut contracts = self.try_borrow_contracts_mut()?;
        contracts.insert(contract_id, contract_fns);
        Ok(())
    }
}
