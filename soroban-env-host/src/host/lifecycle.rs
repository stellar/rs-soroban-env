#[cfg(any(test, feature = "testutils"))]
use sha2::Sha256;
#[cfg(any(test, feature = "testutils"))]
use soroban_env_common::Env;

use crate::{
    crypto,
    host::{
        metered_clone::{MeteredAlloc, MeteredClone},
        metered_write_xdr, ContractReentryMode,
    },
    vm::Vm,
    xdr::{
        Asset, ContractCodeEntry, ContractDataDurability, ContractExecutable, ContractId,
        ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgsV2, ExtensionPoint,
        Hash, LedgerKey, LedgerKeyContractCode, ScAddress, ScErrorCode, ScErrorType, VecM,
    },
    AddressObject, BytesObject, Host, HostError, Symbol, TryFromVal, TryIntoVal, Val, VecObject,
};
use std::rc::Rc;

const CONSTRUCTOR_FUNCTION_NAME: &str = "__constructor";
const CONSTRUCTOR_SUPPORT_PROTOCOL: u32 = 22;

impl Host {
    pub(crate) fn create_contract_from_obj_inputs(
        &self,
        deployer: AddressObject,
        executable: ContractExecutable,
        salt: BytesObject,
        constructor_args: Option<VecObject>,
    ) -> Result<AddressObject, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| addr.metered_clone(self))?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });
        let (constructor_args, constructor_args_vec) = if let Some(v) = constructor_args {
            (self.vecobject_to_scval_vec(v)?, self.call_args_from_obj(v)?)
        } else {
            (VecM::default(), vec![])
        };
        let args = CreateContractArgsV2 {
            contract_id_preimage,
            executable,
            constructor_args,
        };
        self.create_contract_internal(Some(deployer), args, constructor_args_vec)
    }

    pub(crate) fn update_current_contract_executable(
        &self,
        new_executable: ContractExecutable,
    ) -> Result<(), HostError> {
        let curr_contract_id = self.get_current_contract_id_internal()?;
        let key = self.contract_instance_ledger_key(&curr_contract_id)?;
        let old_instance = self.retrieve_contract_instance_from_storage(&key)?;
        self.emit_update_contract_event(&old_instance.executable, &new_executable)?;
        self.store_contract_instance(Some(new_executable), None, curr_contract_id, &key)?;
        Ok(())
    }

    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: ContractId,
        contract_executable: ContractExecutable,
    ) -> Result<(), HostError> {
        let storage_key = self.contract_instance_ledger_key(&contract_id)?;
        if self
            .try_borrow_storage_mut()?
            .has(&storage_key, self, None)?
        {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::ExistingValue,
                "contract already exists",
                &[self
                    .add_host_object(self.scbytes_from_hash(&contract_id.0)?)?
                    .into()],
            ));
        }
        // Make sure the executable exists. Without this check it would be
        // possible to accidentally create a contract that never may be invoked
        // (just by providing a bad hash / reference).
        match &contract_executable {
            ContractExecutable::Wasm(wasm_hash) => {
                self.verify_wasm_exists(wasm_hash)?;
            }
            ContractExecutable::ExternalRef(external_ref) => {
                // Only the existence of the reference entry is validated: the
                // entry's value is guaranteed to be a valid Wasm hash by the
                // protocol.
                self.verify_executable_ref_entry_exists(external_ref, None)?;
            }
            ContractExecutable::StellarAsset => {}
        }
        self.store_contract_instance(Some(contract_executable), None, contract_id, &storage_key)?;
        Ok(())
    }

    fn call_constructor(
        &self,
        contract_id: &ContractId,
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
        contract_id: &ContractId,
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

        // Validate that the ID preimage type matches the executable type:
        // - Address preimage must pair with Wasm or ExternalRef executable
        // - Asset preimage must pair with StellarAsset executable
        match (&args.contract_id_preimage, &args.executable) {
            (ContractIdPreimage::Address(_), ContractExecutable::Wasm(_))
            | (ContractIdPreimage::Address(_), ContractExecutable::ExternalRef(_))
            | (ContractIdPreimage::Asset(_), ContractExecutable::StellarAsset) => Ok(()),
            (ContractIdPreimage::Address(_), ContractExecutable::StellarAsset) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "address preimage is not allowed for StellarAsset executable",
                &[],
            )),
            (ContractIdPreimage::Asset(_), ContractExecutable::Wasm(_))
            | (ContractIdPreimage::Asset(_), ContractExecutable::ExternalRef(_)) => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "asset preimage is not allowed for Wasm or external reference executable",
                &[],
            )),
        }?;

        let id_preimage =
            self.get_full_contract_id_preimage(args.contract_id_preimage.metered_clone(self)?)?;
        let contract_id = ContractId(Hash(self.metered_hash_xdr(&id_preimage)?));

        // Manually snapshot and rollback the storage in test mode. Normally
        // we don't need to worry about host functions performing rollbacks,
        // because they must belong to a frame. However, in the unit tests it's
        // possible for the users to call create_contract host functions
        // directly (e.g. to test the constructor logic), and lack of rollback
        // would incorrectly leave the storage in a modified state
        // after a failed contract creation.
        // This rollback is harmless in case if this has been called from within
        // a frame, as it will be overridden with the default rollback
        // machinery.
        #[cfg(any(test, feature = "testutils"))]
        let storage_snapshot = self.try_borrow_storage()?.map.clone();

        let res = self.create_instance_and_call_constructor(&contract_id, &args, constructor_args);

        #[cfg(any(test, feature = "testutils"))]
        if res.is_err() {
            self.try_borrow_storage_mut()?.map = storage_snapshot;
        }

        res?;

        self.add_host_object(ScAddress::Contract(contract_id))
    }

    fn create_instance_and_call_constructor(
        &self,
        contract_id: &ContractId,
        args: &CreateContractArgsV2,
        constructor_args: Vec<Val>,
    ) -> Result<(), HostError> {
        self.create_contract_with_id(
            contract_id.metered_clone(self)?,
            args.executable.metered_clone(self)?,
        )?;
        self.maybe_initialize_stellar_asset_contract(contract_id, &args.contract_id_preimage)?;
        // Wasm-backed contracts (whether via a direct Wasm hash or an external
        // reference that resolves to one) run their `__constructor`.
        if matches!(
            args.executable,
            ContractExecutable::Wasm(_) | ContractExecutable::ExternalRef(_)
        ) {
            self.call_constructor(contract_id, constructor_args)?;
        }
        Ok(())
    }

    pub(crate) fn get_contract_id_hash(
        &self,
        deployer: AddressObject,
        salt: BytesObject,
    ) -> Result<ContractId, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| addr.metered_clone(self))?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });

        let id_preimage =
            self.get_full_contract_id_preimage(contract_id_preimage.metered_clone(self)?)?;
        Ok(ContractId(Hash(self.metered_hash_xdr(&id_preimage)?)))
    }

    pub(crate) fn get_asset_contract_id_hash(&self, asset: Asset) -> Result<ContractId, HostError> {
        let id_preimage = self.get_full_contract_id_preimage(ContractIdPreimage::Asset(asset))?;
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        Ok(ContractId(Hash(id_arr)))
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
        let ext = {
            let _check_vm = Vm::new(
                self,
                ContractId(Hash(hash_bytes.metered_clone(self)?)),
                wasm_bytes_m.as_slice(),
            )?;
            // At this point we do a secondary parse on what we've checked to be a valid
            // module in order to extract a refined cost model, which we'll store in the
            // code entry's ext field, for future parsing and instantiations.
            _check_vm.module.cost_inputs.charge_for_parsing(self)?;
            crate::xdr::ContractCodeEntryExt::V1(crate::xdr::ContractCodeEntryV1 {
                ext: ExtensionPoint::V0,
                cost_inputs: crate::vm::ParsedModule::extract_refined_contract_cost_inputs(
                    self,
                    wasm_bytes_m.as_slice(),
                )?,
            })
        };

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
        let mut should_put_contract = !storage.has(&code_key, self, None)?;

        // We may also, in the cache-supporting protocol, overwrite the contract if its ext field changed.
        if !should_put_contract {
            let entry = storage.get(&code_key, self, None)?;
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
            storage.put(
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
#[cfg(any(test, feature = "testutils"))]
use std::collections::BTreeMap;
#[cfg(any(test, feature = "testutils"))]
use std::fmt::Write;

// Native test contracts have a corresponding contract code entry, but instead
// of the actual Wasm, their code is either `TEST_CONTRACT_WASM_OVERRIDE_PREFIX`
// followed by the hex-encoded Wasm hash the contract is registered under (for
// test contracts that were registered under an arbitrary Wasm hash), or
// `TEST_CONTRACT_WASM_FOR_CONTRACT_ID_PREFIX` followed by the contract address
// strkey (for test contracts that were registered for a specific contract
// address without an explicit Wasm hash registration).
#[cfg(any(test, feature = "testutils"))]
const TEST_CONTRACT_WASM_OVERRIDE_PREFIX: &str = "test_contract_override_for_wasm_hash_";
#[cfg(any(test, feature = "testutils"))]
const TEST_CONTRACT_WASM_FOR_CONTRACT_ID_PREFIX: &str = "test_contract_for_contract_id_";
// Registry that ties the native test contracts to their corresponding contract
// code entries via Wasm hashes.
#[cfg(any(test, feature = "testutils"))]
#[derive(Clone, Default)]
pub(crate) struct TestContractRegistry {
    // Maps the Wasm hash of the test contract code entry to the corresponding
    // contract function set.
    fns_by_wasm_hash: BTreeMap<Hash, Rc<dyn ContractFunctionSet>>,
}

#[cfg(any(test, feature = "testutils"))]
impl TestContractRegistry {
    // Registers a test contract under the provided Wasm hash, overriding the
    // function set previously registered for that hash (if any).
    pub(crate) fn register_test_contract(
        &mut self,
        wasm_hash: Hash,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) {
        self.fns_by_wasm_hash.insert(wasm_hash, contract_fns);
    }

    // Returns true if a native test contract is registered under the given
    // Wasm hash.
    pub(crate) fn is_test_contract_wasm(&self, wasm_hash: &Hash) -> bool {
        self.fns_by_wasm_hash.contains_key(wasm_hash)
    }

    // Returns the contract function set for a given Wasm hash, if any is
    // registered, `None` otherwise.
    pub(crate) fn get_contract_fn_set(
        &self,
        wasm_hash: &Hash,
    ) -> Option<Rc<dyn ContractFunctionSet>> {
        self.fns_by_wasm_hash.get(wasm_hash).cloned()
    }
}

// "testutils" is not covered by budget metering.
#[cfg(any(test, feature = "testutils"))]
impl Host {
    /// Registers the native test contract under the provided Wasm hash.
    ///
    /// After this call every contract instance that refers to `wasm_hash` as
    /// its executable is dispatched to `contract_fns`.
    ///
    /// Unlike `upload_contract_wasm` this always overrides the function set
    /// that is registered for the given Wasm hash, so a previously uploaded
    /// Wasm will not be used to execute the contract calls anymore.
    ///
    /// If the storage doesn't have a contract code entry for `wasm_hash` yet,
    /// a stub entry is created. Note, that the stub entry does not conform to
    /// the invariant that the code entry's hash is the hash of its Wasm blob.
    ///
    /// An already existing code entry is left untouched and its contents are
    /// never examined.
    pub fn register_native_contract_as_wasm(
        &self,
        contract_fns: Rc<dyn ContractFunctionSet>,
        wasm_hash: BytesObject,
    ) -> Result<(), HostError> {
        self.register_native_contract_as_wasm_internal(contract_fns, Some(wasm_hash), None)?;
        Ok(())
    }

    fn register_native_contract_as_wasm_internal(
        &self,
        contract_fns: Rc<dyn ContractFunctionSet>,
        wasm_hash: Option<BytesObject>,
        contract_address: Option<ScAddress>,
    ) -> Result<Hash, HostError> {
        let _invocation_meter_scope = self.maybe_meter_invocation(
            crate::host::invocation_metering::MeteringInvocation::WasmUploadEntryPoint,
        );
        assert!(wasm_hash.is_some() != contract_address.is_some());

        let (wasm, wasm_hash) = if let Some(wasm_hash) = wasm_hash {
            let wasm_hash = self.hash_from_bytesobj_input("wasm_hash", wasm_hash)?;
            let hash_hex: String = wasm_hash.0.iter().fold(
                String::with_capacity(wasm_hash.0.len() * 2),
                |mut output, b| {
                    let _ = write!(&mut output, "{b:02x}");
                    output
                },
            );
            let wasm = format!("{TEST_CONTRACT_WASM_OVERRIDE_PREFIX}{hash_hex}").into_bytes();
            (wasm, wasm_hash)
        } else if let Some(contract_address) = contract_address {
            let address_strkey = self.non_muxed_sc_address_to_strkey(&contract_address)?;
            let wasm =
                format!("{TEST_CONTRACT_WASM_FOR_CONTRACT_ID_PREFIX}{address_strkey}").into_bytes();
            let wasm_hash = Hash(<Sha256 as sha2::Digest>::digest(wasm.as_slice()).into());
            (wasm, wasm_hash)
        } else {
            unreachable!()
        };

        self.try_borrow_test_contract_registry_mut()?
            .register_test_contract(wasm_hash.clone(), contract_fns);

        let code_key = self.contract_code_ledger_key(&wasm_hash)?;
        if !self.try_borrow_storage_mut()?.has(&code_key, self, None)? {
            self.store_test_contract_code_entry(wasm_hash.clone(), wasm)?;
        }
        Ok(wasm_hash)
    }

    // Writes a stub contract code entry for a native test contract into the
    // storage.
    fn store_test_contract_code_entry(
        &self,
        wasm_hash: Hash,
        wasm: Vec<u8>,
    ) -> Result<(), HostError> {
        // This writes the entry similarly to `upload_contract_wasm`, but
        // without all the Wasm validation/cost logic.
        // This is unfortunately inconvenient to deduplicate with
        // `upload_contract_wasm`, as it would require too many feature-gated
        // branches and would make the production code path hard to read and
        // maintain.
        // We also skip most of the metering and error handling here for
        // brevity, there is really no point in avoiding crashes on internal
        // errors (because it's a test-only path), and trying to emulate the
        // real Wasm upload costs (the bulk of cost comes from the Wasm parsing
        // and VM instantiation which we can't emulate for native contracts).
        let code_key = Rc::new(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: wasm_hash.clone(),
        }));
        let data = ContractCodeEntry {
            hash: wasm_hash,
            ext: crate::xdr::ContractCodeEntryExt::V0,
            code: wasm.try_into().unwrap(),
        };
        self.try_borrow_storage_mut()?.put(
            &code_key,
            &Host::new_contract_code(self, data)?,
            Some(self.get_min_live_until_ledger(ContractDataDurability::Persistent)?),
            self,
            None,
        )
    }

    pub fn register_test_contract(
        &self,
        contract_address: AddressObject,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        #[cfg(any(test, feature = "testutils"))]
        let _invocation_meter_scope = self.maybe_meter_invocation(
            crate::host::invocation_metering::MeteringInvocation::CreateContractEntryPoint,
        );

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
        let _invocation_meter_scope = self.maybe_meter_invocation(
            crate::host::invocation_metering::MeteringInvocation::CreateContractEntryPoint,
        );

        let contract_id = self.contract_id_from_address(contract_address)?;
        let instance_key = self.contract_instance_ledger_key(&contract_id)?;
        let wasm_hash = self.register_native_contract_as_wasm_internal(
            contract_fns,
            None,
            Some(ScAddress::Contract(contract_id.clone())),
        )?;

        // Snapshot and roll back the storage around the constructor call for
        // the same reason as in `create_contract_with_optional_auth`: the
        // instance is stored before the constructor runs, and a failed
        // constructor must not leave it behind.
        let storage_snapshot = self.try_borrow_storage()?.map.clone();
        let res = self
            .store_contract_instance(
                Some(ContractExecutable::Wasm(wasm_hash)),
                None,
                contract_id.clone(),
                &instance_key,
            )
            .and_then(|_| {
                self.call_constructor(&contract_id, self.call_args_from_obj(constructor_args)?)
            });
        if res.is_err() {
            self.try_borrow_storage_mut()?.map = storage_snapshot;
        }
        res
    }

    // This is a test utility that allows calling constructor on a contract that
    // already exists in the storage. It is incorrect to call this
    // on a properly instantiated contract (as it must have already had the
    // constructor executed), but is useful to support manually instantiated
    // contracts, i.e. those that were created by writing directly into storage.
    pub fn call_constructor_for_stored_contract_unsafe(
        &self,
        contract_id: &ContractId,
        constructor_args: crate::VecObject,
    ) -> Result<(), HostError> {
        self.call_constructor(&contract_id, self.call_args_from_obj(constructor_args)?)
    }
}
