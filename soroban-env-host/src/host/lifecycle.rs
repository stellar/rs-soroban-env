use crate::{
    crypto, err,
    host::{
        metered_clone::{MeteredAlloc, MeteredClone},
        metered_write_xdr, ContractReentryMode,
    },
    vm::Vm,
    xdr::{
        Asset, ContractCodeEntry, ContractDataDurability, ContractExecutable, ContractIdPreimage,
        ContractIdPreimageFromAddress, CreateContractArgsV2, ExtensionPoint, Hash, LedgerKey,
        LedgerKeyContractCode, ScAddress, ScErrorCode, ScErrorType,
    },
    AddressObject, BytesObject, Host, HostError, Symbol, TryFromVal, TryIntoVal, Val,
};
use std::rc::Rc;

const CONSTRUCTOR_FUNCTION_NAME: &str = "__constructor";
const CONSTRUCTOR_SUPPORT_PROTOCOL: u32 = 22;

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
            .has_with_host(&storage_key, self, None)?
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

    fn call_constructor(
        &self,
        contract_id: &Hash,
        constructor_args: Vec<Val>,
    ) -> Result<(), HostError> {
        // Wasms built for the protocol versions before constructor support
        // are always treated as having a default no-op constructor with 0
        // arguments.
        let contract_protocol = self.get_contract_protocol_version(&contract_id)?;
        if contract_protocol < CONSTRUCTOR_SUPPORT_PROTOCOL {
            if constructor_args.is_empty() {
                return Ok(());
            }
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InvalidAction,
                "trying to call non-default constructor on a contract that doesn't support constructors (built prior to protocol 22)",
                &[],
            ));
        }
        let res = self
            .call_n_internal(
                contract_id,
                CONSTRUCTOR_FUNCTION_NAME.try_into_val(self)?,
                constructor_args.as_slice(),
                CallParams {
                    reentry_mode: ContractReentryMode::Prohibited,
                    internal_host_call: true,
                    // Allow 0-argument constructors to be missing, but don't allow passing any arguments
                    // into a contract without constructor.
                    treat_missing_function_as_noop: constructor_args.is_empty(),
                },
            )
            .map_err(|err| {
                // Convert any recoverable error to 'generic' host error
                // in order to not accidentally leak the constructor errors
                // to the upstream contracts.
                if err.is_recoverable() {
                    // Also log the original error for diagnostics.
                    self.err(
                        ScErrorType::Context,
                        ScErrorCode::InvalidAction,
                        "constructor invocation has failed with error",
                        &[err.error.to_val()],
                    )
                } else {
                    err
                }
            })?;
        if !res.is_void() {
            return Err(self.err(
                ScErrorType::Value,
                ScErrorCode::UnexpectedType,
                "constructor returned non-void value",
                &[res],
            ));
        }
        Ok(())
    }

    fn maybe_initialize_stellar_asset_contract(
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
                CallParams::default_external_call(),
            )?;
            Ok(())
        } else {
            Ok(())
        }
    }

    pub(crate) fn create_contract_internal(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgsV2,
        constructor_args: Vec<Val>,
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
        let res = self.create_contract_with_optional_auth(deployer, args, constructor_args);
        if has_deployer {
            self.try_borrow_authorization_manager()?
                .pop_frame(self, None)?;
        }
        res
    }

    fn create_contract_with_optional_auth(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgsV2,
        constructor_args: Vec<Val>,
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
        let contract_id = Hash(self.metered_hash_xdr(&id_preimage)?);
        self.create_contract_with_id(contract_id.metered_clone(self)?, args.executable.clone())?;
        self.maybe_initialize_stellar_asset_contract(&contract_id, &args.contract_id_preimage)?;
        if matches!(args.executable, ContractExecutable::Wasm(_)) {
            self.call_constructor(&contract_id, constructor_args)?;
        }
        self.add_host_object(ScAddress::Contract(contract_id))
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

        let mut ext = crate::xdr::ContractCodeEntryExt::V0;

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
            // At this point we do a secondary parse on what we've checked to be a valid
            // module in order to extract a refined cost model, which we'll store in the
            // code entry's ext field, for future parsing and instantiations.
            _check_vm.module.cost_inputs.charge_for_parsing(self)?;
            ext = crate::xdr::ContractCodeEntryExt::V1(crate::xdr::ContractCodeEntryV1 {
                ext: ExtensionPoint::V0,
                cost_inputs: crate::vm::ParsedModule::extract_refined_contract_cost_inputs(
                    self,
                    wasm_bytes_m.as_slice(),
                )?,
            });
        }

        let hash_obj = self.add_host_object(self.scbytes_from_slice(hash_bytes.as_slice())?)?;
        let code_key = Rc::metered_new(
            LedgerKey::ContractCode(LedgerKeyContractCode {
                hash: Hash(hash_bytes.metered_clone(self)?),
            }),
            self,
        )?;

        let mut storage = self.try_borrow_storage_mut()?;

        // We will definitely put the contract in the ledger if it isn't there yet.
        #[allow(unused_mut)]
        let mut should_put_contract = !storage.has_with_host(&code_key, self, None)?;

        // We may also, in the cache-supporting protocol, overwrite the contract if its ext field changed.
        if !should_put_contract {
            let entry = storage.get_with_host(&code_key, self, None)?;
            if let crate::xdr::LedgerEntryData::ContractCode(ContractCodeEntry {
                ext: old_ext,
                ..
            }) = &entry.data
            {
                should_put_contract = *old_ext != ext;
            }
        }

        if should_put_contract {
            let data = ContractCodeEntry {
                hash: Hash(hash_bytes),
                ext,
                code: wasm_bytes_m,
            };
            storage.put_with_host(
                &code_key,
                &Host::new_contract_code(self, data)?,
                Some(self.get_min_live_until_ledger(ContractDataDurability::Persistent)?),
                self,
                None,
            )?;
        }
        Ok(hash_obj)
    }
}

use super::frame::CallParams;
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
        #[cfg(any(test, feature = "testutils"))]
        let _invocation_meter_scope = self.maybe_meter_invocation()?;

        use crate::Env;
        self.register_test_contract_with_constructor(
            contract_address,
            contract_fns,
            self.vec_new()?,
        )
    }

    pub fn register_test_contract_with_constructor(
        &self,
        contract_address: AddressObject,
        contract_fns: Rc<dyn ContractFunctionSet>,
        constructor_args: crate::VecObject,
    ) -> Result<(), HostError> {
        #[cfg(any(test, feature = "testutils"))]
        let _invocation_meter_scope = self.maybe_meter_invocation()?;

        let contract_id = self.contract_id_from_address(contract_address)?;
        let instance_key = self.contract_instance_ledger_key(&contract_id)?;
        let wasm_hash_obj = self.upload_contract_wasm(vec![])?;
        let wasm_hash = self.hash_from_bytesobj_input("wasm_hash", wasm_hash_obj)?;
        // Use the empty Wasm as an executable to a) mark that the contract
        // calls should be dispatched via provided `contract_fns` and b) have
        // the same ledger entries as for 'real' contracts that consist of Wasm
        // entry and the instance entry, so that instance-related host functions
        // work properly.
        self.store_contract_instance(
            Some(ContractExecutable::Wasm(wasm_hash)),
            None,
            contract_id.clone(),
            &instance_key,
        )?;
        self.try_borrow_contracts_mut()?
            .insert(contract_id.clone(), contract_fns);

        self.call_constructor(&contract_id, self.call_args_from_obj(constructor_args)?)
    }

    // This is a test utility that allows calling constructor on a contract that
    // already exists in the storage. It is incorrect to call this
    // on a properly instantiated contract (as it must have already had the
    // constructor executed), but is useful to support manually instantiated
    // contracts, i.e. those that were created by writing directly into storage.
    pub fn call_constructor_for_stored_contract_unsafe(
        &self,
        contract_id: &Hash,
        constructor_args: crate::VecObject,
    ) -> Result<(), HostError> {
        self.call_constructor(&contract_id, self.call_args_from_obj(constructor_args)?)
    }
}
