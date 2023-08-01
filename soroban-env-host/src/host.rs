#![allow(unused_variables)]
#![allow(dead_code)]

use core::{cell::RefCell, cmp::Ordering, fmt::Debug};
use std::rc::Rc;

use crate::{
    auth::{AuthorizationManager, RecordedAuthPayload},
    budget::{AsBudget, Budget},
    err,
    events::{diagnostic::DiagnosticLevel, Events, InternalEventsBuffer},
    host_object::{HostMap, HostObject, HostObjectType, HostVec},
    impl_bignum_host_fns_rhs_u32, impl_wrapping_obj_from_num, impl_wrapping_obj_to_num,
    num::*,
    storage::{InstanceStorageMap, Storage},
    xdr::{
        int128_helpers, AccountId, Asset, ContractCodeEntry, ContractCostType, ContractDataEntry,
        ContractEventType, ContractExecutable, CreateContractArgs, Duration, ExtensionPoint, Hash,
        LedgerEntryData, LedgerKey, LedgerKeyContractCode, PublicKey, ScAddress, ScBytes,
        ScErrorType, ScString, ScSymbol, ScVal, TimePoint,
    },
    AddressObject, Bool, BytesObject, ConversionError, Error, I128Object, I256Object, MapObject,
    StorageType, StringObject, SymbolObject, SymbolSmall, SymbolStr, TryFromVal, U128Object,
    U256Object, U32Val, U64Val, VecObject, VmCaller, VmCallerEnv, Void, I256, U256,
};

use crate::Vm;
use crate::{EnvBase, Object, Symbol, Val};

pub(crate) mod comparison;
mod conversion;
mod crypto;
mod data_helper;
pub(crate) mod declared_size;
pub(crate) mod error;
pub(crate) mod frame;
pub(crate) mod invoker_type;
pub(crate) mod ledger_info_helper;
mod mem_helper;
pub(crate) mod metered_clone;
pub(crate) mod metered_map;
pub(crate) mod metered_vector;
pub(crate) mod metered_xdr;
mod num;
mod prng;
pub use prng::{Seed, SEED_BYTES};
mod validity;
pub use error::HostError;
use soroban_env_common::xdr::{
    ContractCodeEntryBody, ContractDataDurability, ContractDataEntryBody, ContractDataEntryData,
    ContractEntryBodyType, ContractIdPreimage, ContractIdPreimageFromAddress, ScContractInstance,
    ScErrorCode, MASK_CONTRACT_DATA_FLAGS_V20,
};

use self::{
    frame::{Context, ContractReentryMode},
    metered_vector::MeteredVector,
    prng::Prng,
};
use self::{
    metered_clone::{MeteredClone, MeteredContainer},
    metered_xdr::metered_write_xdr,
};
use crate::impl_bignum_host_fns;
use crate::Compare;
#[cfg(any(test, feature = "testutils"))]
use crate::TryIntoVal;
#[cfg(any(test, feature = "testutils"))]
pub use frame::ContractFunctionSet;
pub(crate) use frame::Frame;
#[cfg(any(test, feature = "testutils"))]
use soroban_env_common::xdr::SorobanAuthorizedInvocation;

/// Defines the maximum depth for recursive calls in the host, i.e. `Val` conversion, comparison,
/// and deep clone, to prevent stack overflow.
///
/// Similar to the `xdr::DEFAULT_XDR_RW_DEPTH_LIMIT`, `DEFAULT_HOST_DEPTH_LIMIT` is also a proxy
/// to the stack depth limit, and its purpose is to prevent the program from
/// hitting the maximum stack size allowed by Rust, which would result in an unrecoverable `SIGABRT`.
///
/// The difference is the `DEFAULT_HOST_DEPTH_LIMIT`guards the recursion paths via the `Env` and
/// the `Budget`, i.e., conversion, comparison and deep clone. The limit is checked at specific
/// points of the recursion path, e.g. when `Val` is encountered, to minimize noise. So the
/// "actual stack depth"/"host depth" factor will typically be larger, and thus the
/// `DEFAULT_HOST_DEPTH_LIMIT` here is set to a smaller value.
pub const DEFAULT_HOST_DEPTH_LIMIT: u32 = 100;

/// Temporary helper for denoting a slice of guest memory, as formed by
/// various bytes operations.
pub(crate) struct VmSlice {
    vm: Rc<Vm>,
    pos: u32,
    len: u32,
}

#[derive(Debug, Clone, Default)]
pub struct LedgerInfo {
    pub protocol_version: u32,
    pub sequence_number: u32,
    pub timestamp: u64,
    pub network_id: [u8; 32],
    pub base_reserve: u32,
    pub min_temp_entry_expiration: u32,
    pub min_persistent_entry_expiration: u32,
    pub max_entry_expiration: u32,
    pub autobump_ledgers: u32,
}

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    source_account: RefCell<Option<AccountId>>,
    ledger: RefCell<Option<LedgerInfo>>,
    pub(crate) objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    pub(crate) context: RefCell<Vec<Context>>,
    // Note: budget is refcounted and is _not_ deep-cloned when you call HostImpl::deep_clone,
    // mainly because it's not really possible to achieve (the same budget is connected to many
    // metered sub-objects) but also because it's plausible that the person calling deep_clone
    // actually wants their clones to be metered by "the same" total budget
    pub(crate) budget: Budget,
    pub(crate) events: RefCell<InternalEventsBuffer>,
    authorization_manager: RefCell<AuthorizationManager>,
    pub(crate) diagnostic_level: RefCell<DiagnosticLevel>,
    pub(crate) base_prng: RefCell<Option<Prng>>,
    // Note: we're not going to charge metering for testutils because it's out of the scope
    // of what users will be charged for in production -- it's scaffolding for testing a contract,
    // but shouldn't be charged to the contract itself (and will never be compiled-in to
    // production hosts)
    #[cfg(any(test, feature = "testutils"))]
    contracts: RefCell<std::collections::HashMap<Hash, Rc<dyn ContractFunctionSet>>>,
    // Store a copy of the `AuthorizationManager` for the last host function
    // invocation. In order to emulate the production behavior in tests, we reset
    // authorization manager after every invocation (as it's not meant to be
    // shared between invocations).
    // This enables test-only functions that allow checking if the authorization
    // has happened or has been recorded.
    #[cfg(any(test, feature = "testutils"))]
    previous_authorization_manager: RefCell<Option<AuthorizationManager>>,
}
// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Clone)]
pub struct Host(pub(crate) Rc<HostImpl>);

#[allow(clippy::derivable_impls)]
impl Default for Host {
    fn default() -> Self {
        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        let _client = tracy_client::Client::start();
        Self(Default::default())
    }
}

macro_rules! impl_checked_borrow_helpers {
    ($field:ident, $t:ty, $borrow:ident, $borrow_mut:ident) => {
        impl Host {
            pub(crate) fn $borrow(&self) -> Result<std::cell::Ref<'_, $t>, HostError> {
                use crate::host::error::TryBorrowOrErr;
                self.0.$field.try_borrow_or_err_with(
                    self,
                    concat!("host.0.", stringify!($field), ".try_borrow failed"),
                )
            }
            pub(crate) fn $borrow_mut(&self) -> Result<std::cell::RefMut<'_, $t>, HostError> {
                use crate::host::error::TryBorrowOrErr;
                self.0.$field.try_borrow_mut_or_err_with(
                    self,
                    concat!("host.0.", stringify!($field), ".try_borrow_mut failed"),
                )
            }
        }
    };
}

impl_checked_borrow_helpers!(
    source_account,
    Option<AccountId>,
    try_borrow_source_account,
    try_borrow_source_account_mut
);
impl_checked_borrow_helpers!(
    ledger,
    Option<LedgerInfo>,
    try_borrow_ledger,
    try_borrow_ledger_mut
);
impl_checked_borrow_helpers!(
    objects,
    Vec<HostObject>,
    try_borrow_objects,
    try_borrow_objects_mut
);
impl_checked_borrow_helpers!(storage, Storage, try_borrow_storage, try_borrow_storage_mut);
impl_checked_borrow_helpers!(
    context,
    Vec<Context>,
    try_borrow_context,
    try_borrow_context_mut
);
impl_checked_borrow_helpers!(
    events,
    InternalEventsBuffer,
    try_borrow_events,
    try_borrow_events_mut
);
impl_checked_borrow_helpers!(
    authorization_manager,
    AuthorizationManager,
    try_borrow_authorization_manager,
    try_borrow_authorization_manager_mut
);
impl_checked_borrow_helpers!(
    diagnostic_level,
    DiagnosticLevel,
    try_borrow_diagnostic_level,
    try_borrow_diagnostic_level_mut
);
impl_checked_borrow_helpers!(
    base_prng,
    Option<Prng>,
    try_borrow_base_prng,
    try_borrow_base_prng_mut
);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(contracts, std::collections::HashMap<Hash, Rc<dyn ContractFunctionSet>>, try_borrow_contracts, try_borrow_contracts_mut);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(
    previous_authorization_manager,
    Option<AuthorizationManager>,
    try_borrow_previous_authorization_manager,
    try_borrow_previous_authorization_manager_mut
);

impl Debug for HostImpl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "HostImpl(...)")
    }
}

impl Debug for Host {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Host({:x})", Rc::<HostImpl>::as_ptr(&self.0) as usize)
    }
}

impl Host {
    /// Constructs a new [`Host`] that will use the provided [`Storage`] for
    /// contract-data access functions such as
    /// [`Env::get_contract_data`].
    pub fn with_storage_and_budget(storage: Storage, budget: Budget) -> Self {
        #[cfg(all(not(target_family = "wasm"), feature = "tracy"))]
        let _client = tracy_client::Client::start();
        Self(Rc::new(HostImpl {
            source_account: RefCell::new(None),
            ledger: RefCell::new(None),
            objects: Default::default(),
            storage: RefCell::new(storage),
            context: Default::default(),
            budget,
            events: Default::default(),
            authorization_manager: RefCell::new(
                AuthorizationManager::new_enforcing_without_authorizations(),
            ),
            diagnostic_level: Default::default(),
            base_prng: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            contracts: Default::default(),
            #[cfg(any(test, feature = "testutils"))]
            previous_authorization_manager: RefCell::new(None),
        }))
    }

    pub fn set_source_account(&self, source_account: AccountId) -> Result<(), HostError> {
        *self.try_borrow_source_account_mut()? = Some(source_account);
        Ok(())
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn remove_source_account(&self) -> Result<(), HostError> {
        *self.try_borrow_source_account_mut()? = None;
        Ok(())
    }

    #[cfg(test)]
    pub(crate) fn source_account_id(&self) -> Result<Option<AccountId>, HostError> {
        Ok(self.try_borrow_source_account()?.clone())
    }

    pub fn source_account_address(&self) -> Result<Option<AddressObject>, HostError> {
        if let Some(acc) = self.try_borrow_source_account()?.as_ref() {
            Ok(Some(self.add_host_object(ScAddress::Account(
                acc.metered_clone(self.budget_ref())?,
            ))?))
        } else {
            Ok(None)
        }
    }

    pub fn switch_to_recording_auth(&self) -> Result<(), HostError> {
        *self.try_borrow_authorization_manager_mut()? = AuthorizationManager::new_recording();
        Ok(())
    }

    pub fn set_authorization_entries(
        &self,
        auth_entries: Vec<soroban_env_common::xdr::SorobanAuthorizationEntry>,
    ) -> Result<(), HostError> {
        let new_auth_manager = AuthorizationManager::new_enforcing(self, auth_entries)?;
        *self.try_borrow_authorization_manager_mut()? = new_auth_manager;
        Ok(())
    }

    pub fn set_base_prng_seed(&self, seed: prng::Seed) -> Result<(), HostError> {
        *self.try_borrow_base_prng_mut()? = Some(Prng::new_from_seed(seed));
        Ok(())
    }

    pub fn set_ledger_info(&self, info: LedgerInfo) -> Result<(), HostError> {
        *self.try_borrow_ledger_mut()? = Some(info);
        Ok(())
    }

    pub fn with_ledger_info<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&LedgerInfo) -> Result<T, HostError>,
    {
        match self.try_borrow_ledger()?.as_ref() {
            None => Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "missing ledger info",
                &[],
            )),
            Some(li) => f(li),
        }
    }

    pub fn with_mut_ledger_info<F>(&self, mut f: F) -> Result<(), HostError>
    where
        F: FnMut(&mut LedgerInfo),
    {
        match self.try_borrow_ledger_mut()?.as_mut() {
            None => Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "missing ledger info",
                &[],
            )),
            Some(li) => {
                f(li);
                Ok(())
            }
        }
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    pub(crate) fn with_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(Budget) -> Result<T, HostError>,
    {
        f(self.0.budget.clone())
    }

    pub(crate) fn budget_ref(&self) -> &Budget {
        &self.0.budget
    }

    pub fn budget_cloned(&self) -> Budget {
        self.0.budget.clone()
    }

    pub fn charge_budget(&self, ty: ContractCostType, input: Option<u64>) -> Result<(), HostError> {
        self.0.budget.clone().charge(ty, input)
    }

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

    /// Accept a _unique_ (refcount = 1) host reference and destroy the
    /// underlying [`HostImpl`], returning its finalized components containing
    /// processing side effects  to the caller as a tuple wrapped in `Ok(...)`.
    pub fn try_finish(self) -> Result<(Storage, Events), HostError> {
        let events = self.try_borrow_events()?.externalize(&self)?;
        Rc::try_unwrap(self.0)
            .map(|host_impl| {
                let storage = host_impl.storage.into_inner();
                (storage, events)
            })
            .map_err(|_| {
                Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError).into()
            })
    }

    /// Invokes the reserved `__check_auth` function on a provided contract.
    ///
    /// This is useful for testing the custom account contracts. Otherwise, the
    /// host prohibits calling `__check_auth` outside of internal implementation
    /// of `require_auth[_for_args]` calls.
    #[cfg(any(test, feature = "testutils"))]
    pub fn call_account_contract_check_auth(
        &self,
        contract: BytesObject,
        args: VecObject,
    ) -> Result<Val, HostError> {
        use crate::native_contract::account_contract::ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME;

        let contract_id = self.hash_from_bytesobj_input("contract", contract)?;
        let args = self.call_args_from_obj(args)?;
        let res = self.call_n_internal(
            &contract_id,
            ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME.try_into_val(self)?,
            args.as_slice(),
            ContractReentryMode::Prohibited,
            true,
        );
        if let Err(e) = &res {
            self.with_events_mut(|events| {
                self.err_diagnostics(
                    events,
                    e.error,
                    "check auth invocation for a custom account contract failed",
                    &[],
                )
            })?;
        }
        res
    }

    #[cfg(any(test, feature = "testutils"))]
    /// Returns the current state of the authorization manager.
    ///
    /// Use this in conjunction with `set_auth_manager` to do authorized
    /// operations without breaking the current authorization state (useful for
    /// preserving the auth state while doing the generic test setup).
    pub fn snapshot_auth_manager(&self) -> Result<AuthorizationManager, HostError> {
        Ok(self.try_borrow_authorization_manager()?.clone())
    }

    /// Replaces authorization manager with the provided new instance.
    ///
    /// Use this in conjunction with `snapshot_auth_manager` to do authorized
    /// operations without breaking the current authorization state (useful for
    /// preserving the auth state while doing the generic test setup).
    #[cfg(any(test, feature = "testutils"))]
    pub fn set_auth_manager(&self, auth_manager: AuthorizationManager) -> Result<(), HostError> {
        *self.try_borrow_authorization_manager_mut()? = auth_manager;
        Ok(())
    }

    // Testing interface to create values directly for later use via Env functions.
    // It needs to be a `pub` method because benches are considered a separate crate.
    pub fn inject_val(&self, v: &ScVal) -> Result<Val, HostError> {
        self.to_host_val(v).map(Into::into)
    }

    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: Hash,
        contract_executable: ContractExecutable,
    ) -> Result<(), HostError> {
        let storage_key = self.contract_instance_ledger_key(&contract_id)?;
        if self
            .try_borrow_storage_mut()?
            .has(&storage_key, self.as_budget())?
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
        let instance = ScContractInstance {
            executable: contract_executable,
            storage: Default::default(),
        };
        self.store_contract_instance(instance, contract_id, &storage_key)?;
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

    fn create_contract_internal(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgs,
    ) -> Result<AddressObject, HostError> {
        let has_deployer = deployer.is_some();
        if has_deployer {
            self.try_borrow_authorization_manager()?
                .push_create_contract_host_fn_frame(self, args.metered_clone(self.budget_ref())?)?;
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

        let id_preimage = self.get_full_contract_id_preimage(
            args.contract_id_preimage.metered_clone(self.budget_ref())?,
        )?;
        let hash_id = Hash(self.metered_hash_xdr(&id_preimage)?);
        self.create_contract_with_id(hash_id.metered_clone(self.budget_ref())?, args.executable)?;
        self.maybe_initialize_asset_token(&hash_id, &args.contract_id_preimage)?;
        self.add_host_object(ScAddress::Contract(hash_id))
    }

    pub(crate) fn get_contract_id_hash(
        &self,
        deployer: AddressObject,
        salt: BytesObject,
    ) -> Result<Hash, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| {
                addr.metered_clone(self.budget_ref())
            })?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });

        let id_preimage = self.get_full_contract_id_preimage(
            contract_id_preimage.metered_clone(self.budget_ref())?,
        )?;
        Ok(Hash(self.metered_hash_xdr(&id_preimage)?))
    }

    pub(crate) fn get_asset_contract_id_hash(&self, asset: Asset) -> Result<Hash, HostError> {
        let id_preimage = self.get_full_contract_id_preimage(ContractIdPreimage::Asset(asset))?;
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        Ok(Hash(id_arr))
    }

    // "testutils" is not covered by budget metering.
    #[cfg(any(test, feature = "testutils"))]
    pub fn register_test_contract(
        &self,
        contract_address: AddressObject,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        let hash = self.contract_id_from_address(contract_address)?;
        let mut contracts = self.try_borrow_contracts_mut()?;
        contracts.insert(hash, contract_fns);
        Ok(())
    }

    // Writes an arbitrary ledger entry to storage.
    // "testutils" is not covered by budget metering.
    #[cfg(any(test, feature = "testutils"))]
    pub fn add_ledger_entry(
        &self,
        key: &Rc<LedgerKey>,
        val: &Rc<soroban_env_common::xdr::LedgerEntry>,
    ) -> Result<(), HostError> {
        self.with_mut_storage(|storage| storage.put(key, val, self.as_budget()))
    }

    // Returns the authorizations that have been authenticated for the last
    // contract invocation.
    //
    // Authenticated means that either the authorization was authenticated using
    // the actual authorization logic for that authorization in enforced mode,
    // or that it was recorded in recording mode and authorization was assumed
    // successful.
    #[cfg(any(test, feature = "testutils"))]
    pub fn get_authenticated_authorizations(
        &self,
    ) -> Result<Vec<(ScAddress, SorobanAuthorizedInvocation)>, HostError> {
        Ok(self
            .try_borrow_previous_authorization_manager_mut()?
            .as_mut()
            .ok_or_else(|| {
                self.err(
                    ScErrorType::Auth,
                    ScErrorCode::InvalidAction,
                    "previous invocation is missing - no auth data to get",
                    &[],
                )
            })?
            .get_authenticated_authorizations(self))
    }

    fn upload_contract_wasm(&self, wasm: Vec<u8>) -> Result<BytesObject, HostError> {
        let hash_bytes: [u8; 32] = self
            .sha256_hash_from_bytes(wasm.as_slice())?
            .try_into()
            .map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InternalError,
                    "unexpected hash length",
                    &[],
                )
            })?;
        let hash_obj = self.add_host_object(self.scbytes_from_slice(hash_bytes.as_slice())?)?;
        let code_key = Rc::new(LedgerKey::ContractCode(LedgerKeyContractCode {
            hash: Hash(hash_bytes.metered_clone(self.budget_ref())?),
            body_type: ContractEntryBodyType::DataEntry,
        }));
        if !self
            .try_borrow_storage_mut()?
            .has(&code_key, self.as_budget())?
        {
            let body = ContractCodeEntryBody::DataEntry(wasm.try_into().map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::ExceededLimit,
                    "Wasm code is too large",
                    &[],
                )
            })?);

            self.with_mut_storage(|storage| {
                let data = LedgerEntryData::ContractCode(ContractCodeEntry {
                    hash: Hash(hash_bytes),
                    body,
                    ext: ExtensionPoint::V0,
                    expiration_ledger_seq: self
                        .get_min_expiration_ledger(ContractDataDurability::Persistent)?,
                });
                storage.put(
                    &code_key,
                    &Host::ledger_entry_from_data(data),
                    self.as_budget(),
                )
            })?;
        }
        Ok(hash_obj)
    }

    // Returns the recorded per-address authorization payloads that would cover the
    // top-level contract function invocation in the enforcing mode.
    // This should only be called in the recording authorization mode, i.e. only
    // if `switch_to_recording_auth` has been called.
    pub fn get_recorded_auth_payloads(&self) -> Result<Vec<RecordedAuthPayload>, HostError> {
        #[cfg(not(any(test, feature = "testutils")))]
        {
            self.try_borrow_authorization_manager()?
                .get_recorded_auth_payloads(self)
        }
        #[cfg(any(test, feature = "testutils"))]
        {
            self.try_borrow_previous_authorization_manager()?
                .as_ref()
                .ok_or_else(|| {
                    self.err(
                        ScErrorType::Auth,
                        ScErrorCode::InvalidAction,
                        "previous invocation is missing - no auth data to get",
                        &[],
                    )
                })?
                .get_recorded_auth_payloads(self)
        }
    }

    fn symbol_matches(&self, s: &[u8], sym: Symbol) -> Result<bool, HostError> {
        if let Ok(ss) = SymbolSmall::try_from(sym) {
            let sstr: SymbolStr = ss.into();
            let slice: &[u8] = sstr.as_ref();
            self.as_budget()
                .compare(&slice, &s)
                .map(|c| c == Ordering::Equal)
        } else {
            let sobj: SymbolObject = sym.try_into()?;
            self.visit_obj(sobj, |scsym: &ScSymbol| {
                self.as_budget()
                    .compare(&scsym.as_slice(), &s)
                    .map(|c| c == Ordering::Equal)
            })
        }
    }

    fn check_symbol_matches(&self, s: &[u8], sym: Symbol) -> Result<(), HostError> {
        if self.symbol_matches(s, sym)? {
            Ok(())
        } else {
            Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "symbol mismatch",
                &[sym.to_val()],
            ))
        }
    }

    fn put_contract_data_into_ledger(
        &self,
        k: Val,
        v: Val,
        t: StorageType,
        f: Val,
    ) -> Result<(), HostError> {
        let flags: Option<u32> = if f.is_void() {
            None
        } else {
            let val = self.u32_from_rawval_input("f", f)?;
            if ((val as u64) & !MASK_CONTRACT_DATA_FLAGS_V20) != 0 {
                return Err(self.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "invalid flags",
                    &[],
                ));
            }
            Some(val)
        };

        let durability: ContractDataDurability = t.try_into()?;
        let key = self.contract_data_key_from_rawval(k, durability)?;
        if self.try_borrow_storage_mut()?.has(&key, self.as_budget())? {
            let mut current = (*self.try_borrow_storage_mut()?.get(&key, self.as_budget())?)
                .metered_clone(&self.0.budget)?;

            match current.data {
                LedgerEntryData::ContractData(ref mut entry) => match entry.body {
                    ContractDataEntryBody::DataEntry(ref mut data) => {
                        data.val = self.from_host_val(v)?;
                        if let Some(new_flags) = flags {
                            data.flags = new_flags;
                        }
                    }
                    _ => {
                        return Err(self.err(
                            ScErrorType::Storage,
                            ScErrorCode::UnexpectedType,
                            "expected DataEntry",
                            &[],
                        ));
                    }
                },
                _ => {
                    return Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::UnexpectedType,
                        "expected DataEntry",
                        &[],
                    ));
                }
            }
            self.try_borrow_storage_mut()?
                .put(&key, &Rc::new(current), self.as_budget())?;
        } else {
            let body = ContractDataEntryBody::DataEntry(ContractDataEntryData {
                val: self.from_host_val(v)?,
                flags: flags.unwrap_or(0),
            });
            let data = LedgerEntryData::ContractData(ContractDataEntry {
                contract: ScAddress::Contract(self.get_current_contract_id_internal()?),
                key: self.from_host_val(k)?,
                body,
                expiration_ledger_seq: self.get_min_expiration_ledger(durability)?,
                durability,
            });
            self.try_borrow_storage_mut()?.put(
                &key,
                &Host::ledger_entry_from_data(data),
                self.as_budget(),
            )?;
        }

        Ok(())
    }

    // If autobump enabled, autobumps all the entries in the footprint.
    fn maybe_autobump_expiration_of_footprint_entries(&self) -> Result<(), HostError> {
        let Some(autobump_ledgers) = self.with_ledger_info(|li| {
            if li.autobump_ledgers > 0 {
                Ok(Some(li.autobump_ledgers))
            } else {
                Ok(None)
            }
        })? else {
            return Ok(());
        };
        // Need to copy the footprint out of the storage to allow mut borrow of
        // storage.
        let footprint_map = self
            .try_borrow_storage()?
            .footprint
            .0
            .metered_clone(self.budget_ref())?;
        for (key, _) in footprint_map.iter(self.budget_ref())? {
            match key.as_ref() {
                LedgerKey::ContractData(_) | LedgerKey::ContractCode(_) => {
                    if self.try_borrow_storage_mut()?.has(key, self.budget_ref())? {
                        self.try_borrow_storage_mut()?
                            .bump_relative_to_entry_expiration(
                                self,
                                key.clone(),
                                autobump_ledgers,
                            )?;
                    }
                }
                _ => (),
            }
        }
        Ok(())
    }
}

// Notes on metering: these are called from the guest and thus charged on the VM instructions.
impl EnvBase for Host {
    type Error = HostError;

    // This function is somewhat subtle.
    //
    // It exists to allow the client of the (VmCaller)Env interface(s) to
    // essentially _reject_ an error returned by one of the Result-returning
    // methods on the trait, choosing to panic instead. But doing so in some way
    // that the trait defines, rather than calling panic in the client.
    //
    // The only client we expect to _do_ this is a non-builtin user contract
    // compiled natively for local testing (and thus linked directly to `Host`).
    // In a wasm build of a user contract, we already encourage users to think
    // of `Env::Error` as infallible by literally defining `Guest::Error` as the
    // `Infallible` type (which makes sense: we trap the user's VM on such
    // errors, don't resume it at all). But in a non-wasm, native build of a
    // user contract, `Env=Host` and `Env::Error=HostError`, a real type you can
    // observe. So the user might actually have a code path returning from such
    // an error that is suddenly non-dead and receiving an
    // `Env::Error=HostError`, which (to maintain continuity with the VM case)
    // they then _want_ to treat as impossible-to-have-occurred just like
    // `Guest::Error`. They can panic, but that doesn't quite maintain the
    // illusion properly. Instead they should call this method to "reject the
    // error".
    //
    // When such a "rejected error" occurs, we do panic, but only after checking
    // to see if we're in a `TestContract` invocation, and if so storing the
    // error's Error value in that frame, such that `Host::call_n` above can
    // recover the Error when it _catches_ the panic and converts it back to an
    // error.
    //
    // It might seem like we ought to `std::panic::panic_any(e)` here, making
    // the panic carry a `HostError` or `Error` and catching it by dynamic type
    // inspection in the `call_n` catch logic. The reason we don't do so is that
    // `panic_any` will not provide a nice printable value to the `PanicInfo`,
    // it constructs, so when/if the panic makes it to a top-level printout it
    // will display a relatively ugly message like "thread panicked at Box<dyn
    // Any>" to stderr, when it is much more useful to the user if we have it
    // print the result of HostError::Debug, with its glorious status code,
    // site-of-origin backtrace and debug log.
    //
    // To get it to do that, we have to call `panic!()`, not `panic_any`.
    // Personally I think this is a glaring weakness of `panic_any` but we are
    // not in a position to improve it.
    #[cfg(feature = "testutils")]
    fn escalate_error_to_panic(&self, e: Self::Error) -> ! {
        let _ = self.with_current_frame_opt(|f| {
            if let Some(Frame::TestContract(frame)) = f {
                if let Ok(mut panic) = frame.panic.try_borrow_mut() {
                    *panic = Some(e.error);
                }
            }
            Ok(())
        });
        let escalation = self.error(e.error, "escalating error to panic", &[]);
        panic!("{:?}", escalation)
    }

    fn as_mut_any(&mut self) -> &mut dyn std::any::Any {
        todo!()
    }

    fn check_same_env(&self, other: &Self) {
        assert!(Rc::ptr_eq(&self.0, &other.0));
    }

    fn deep_clone(&self) -> Self {
        Host(Rc::new((*self.0).clone()))
    }

    fn bytes_copy_from_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &[u8],
    ) -> Result<BytesObject, HostError> {
        self.memobj_copy_from_slice::<ScBytes>(b, b_pos, slice)
    }

    fn bytes_copy_to_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        self.memobj_copy_to_slice::<ScBytes>(b, b_pos, slice)
    }

    fn string_copy_to_slice(
        &self,
        b: StringObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        self.memobj_copy_to_slice::<ScString>(b, b_pos, slice)
    }

    fn symbol_copy_to_slice(
        &self,
        s: SymbolObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        let len = self.visit_obj(s, |sym: &ScSymbol| Ok(sym.len()))?;
        self.memobj_copy_to_slice::<ScSymbol>(s, b_pos, &mut slice[..len])
    }

    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<BytesObject, HostError> {
        self.add_host_object(self.scbytes_from_slice(mem)?)
    }

    fn string_new_from_slice(&self, s: &str) -> Result<StringObject, HostError> {
        self.add_host_object(ScString(
            self.metered_slice_to_vec(s.as_bytes())?.try_into()?,
        ))
    }

    fn symbol_new_from_slice(&self, s: &str) -> Result<SymbolObject, HostError> {
        for ch in s.chars() {
            SymbolSmall::validate_char(ch)?;
        }
        self.add_host_object(ScSymbol(
            self.metered_slice_to_vec(s.as_bytes())?.try_into()?,
        ))
    }

    fn map_new_from_slices(&self, keys: &[&str], vals: &[Val]) -> Result<MapObject, HostError> {
        Vec::<Symbol>::charge_bulk_init(keys.len() as u64, self.as_budget())?;
        // If only fallible iterators worked better in Rust, we would not need this Vec<...>.
        let mut key_syms: Vec<Symbol> = Vec::with_capacity(keys.len());
        for k in keys.iter() {
            key_syms.push(Symbol::try_from_val(self, k)?);
        }
        for v in vals.iter() {
            self.check_val_integrity(*v)?;
        }
        let pair_iter = key_syms
            .iter()
            .map(|s| s.to_val())
            .zip(vals.iter().cloned());
        let map = HostMap::from_exact_iter(pair_iter, self)?;
        self.add_host_object(map)
    }

    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [Val],
    ) -> Result<Void, HostError> {
        // Main costs are already covered by `visit_obj` and `check_symbol_matches`. Here
        // we charge shallow copy of the values.
        metered_clone::charge_shallow_copy::<Val>(keys.len() as u64, self.as_budget())?;
        if keys.len() != vals.len() {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::UnexpectedSize,
                "differing key and value vector lengths when unpacking map to slice",
                &[],
            ));
        }
        self.visit_obj(map, |hm: &HostMap| {
            if hm.len() != vals.len() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host map and output vector lengths when unpacking map to slice",
                    &[],
                ));
            }

            for (ik, mk) in keys.iter().zip(hm.keys(self)?) {
                let sym: Symbol = mk.try_into()?;
                self.check_symbol_matches(ik.as_bytes(), sym)?;
            }

            for (iv, mv) in vals.iter_mut().zip(hm.values(self)?) {
                *iv = *mv;
            }
            Ok(())
        })?;
        Ok(Val::VOID)
    }

    fn vec_new_from_slice(&self, vals: &[Val]) -> Result<VecObject, Self::Error> {
        let vec = HostVec::from_exact_iter(vals.iter().cloned(), self.budget_ref())?;
        for v in vec.iter() {
            self.check_val_integrity(*v)?;
        }
        self.add_host_object(vec)
    }

    fn vec_unpack_to_slice(&self, vec: VecObject, vals: &mut [Val]) -> Result<Void, Self::Error> {
        self.visit_obj(vec, |hv: &HostVec| {
            if hv.len() != vals.len() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host vector and output vector lengths when unpacking vec to slice",
                    &[],
                ));
            }
            metered_clone::charge_shallow_copy::<Val>(hv.len() as u64, self.as_budget())?;
            vals.copy_from_slice(hv.as_slice());
            Ok(())
        })?;
        Ok(Val::VOID)
    }

    fn symbol_index_in_strs(&self, sym: Symbol, slices: &[&str]) -> Result<U32Val, Self::Error> {
        let mut found = None;
        self.metered_scan_slice_of_slices(slices, |i, slice| {
            if self.symbol_matches(slice.as_bytes(), sym)? && found.is_none() {
                found = Some(i)
            }
            Ok(())
        })?;
        match found {
            None => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "symbol not found in slice of strs",
                &[sym.to_val()],
            )),
            Some(idx) => Ok(U32Val::from(self.usize_to_u32(idx)?)),
        }
    }

    fn log_from_slice(&self, msg: &str, vals: &[Val]) -> Result<Void, HostError> {
        self.log_diagnostics(msg, vals).map(|_| Void::from(()))
    }
}

impl VmCallerEnv for Host {
    type VmUserState = Host;

    // Notes on metering: covered by the components
    fn log_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        msg_pos: U32Val,
        msg_len: U32Val,
        vals_pos: U32Val,
        vals_len: U32Val,
    ) -> Result<Void, HostError> {
        if self.is_debug()? {
            self.as_budget().with_free_budget(|| {
                let VmSlice { vm, pos, len } = self.decode_vmslice(msg_pos, msg_len)?;
                let mut msg: Vec<u8> = vec![0u8; len as usize];
                self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, &mut msg)?;
                let msg = String::from_utf8_lossy(&msg);

                let VmSlice { vm, pos, len } = self.decode_vmslice(vals_pos, vals_len)?;
                let mut vals: Vec<Val> = vec![Val::VOID.to_val(); len as usize];
                self.metered_vm_read_vals_from_linear_memory::<8, Val>(
                    vmcaller,
                    &vm,
                    pos,
                    vals.as_mut_slice(),
                    |buf| self.relative_to_absolute(Val::from_payload(u64::from_le_bytes(*buf))),
                )?;

                self.log_diagnostics(&msg, &vals)
            })?;
        }
        Ok(Val::VOID)
    }

    // Notes on metering: covered by the components
    fn get_invoking_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<AddressObject, HostError> {
        let invoking_contract_hash = self.get_invoking_contract_internal()?;
        self.add_host_object(ScAddress::Contract(invoking_contract_hash))
    }

    // Metered: covered by `visit` and `metered_cmp`.
    fn obj_cmp(&self, _vmcaller: &mut VmCaller<Host>, a: Val, b: Val) -> Result<i64, HostError> {
        let res = match unsafe {
            match (Object::try_from(a), Object::try_from(b)) {
                // We were given two objects: compare them.
                (Ok(a), Ok(b)) => self.unchecked_visit_val_obj(a, |ao| {
                    // They might each be None but that's ok, None compares less than Some.
                    self.unchecked_visit_val_obj(b, |bo| Ok(Some(self.compare(&ao, &bo)?)))
                })?,

                // We were given an object and a non-object: first fetch the object.
                (Ok(a), Err(_)) => self.unchecked_visit_val_obj(a, |ao| match ao {
                    // If the object is actually missing, it's less than any non-object.
                    None => Ok(Some(Ordering::Less)),
                    // If the object is present, try a small-value comparison.
                    Some(aobj) => aobj.try_compare_to_small(self.as_budget(), b),
                })?,
                // Same as previous case, but reversed.
                (Err(_), Ok(b)) => self.unchecked_visit_val_obj(b, |bo| match bo {
                    // So we reverse the relative order of the "missing object" case.
                    None => Ok(Some(Ordering::Greater)),
                    // And reverse the result of a successful small-value comparison.
                    Some(bobj) => Ok(match bobj.try_compare_to_small(self.as_budget(), a)? {
                        Some(Ordering::Less) => Some(Ordering::Greater),
                        Some(Ordering::Greater) => Some(Ordering::Less),
                        other => other,
                    }),
                })?,
                // We should have been given at least one object.
                (Err(_), Err(_)) => {
                    return Err(self.err(
                        ScErrorType::Value,
                        ScErrorCode::UnexpectedType,
                        "two non-object args to obj_cmp",
                        &[a, b],
                    ))
                }
            }
        } {
            // If any of the above got us a result, great, use it.
            Some(res) => res,

            // Otherwise someone gave us an object and a non-paired value (not a small-value
            // case of the same type). Order these by their ScValType.
            None => {
                let atype = a.get_tag().get_scval_type();
                let btype = b.get_tag().get_scval_type();
                if atype == btype {
                    // This shouldn't have happened, but if it does there's a logic error.
                    return Err(self.err(
                        ScErrorType::Value,
                        ScErrorCode::InternalError,
                        "equal-tagged values rejected by small-value obj_cmp",
                        &[a, b],
                    ));
                }
                atype.cmp(&btype)
            }
        };
        // Finally, translate Ordering::Foo to a number to return to caller.
        Ok(match res {
            Ordering::Less => -1,
            Ordering::Equal => 0,
            Ordering::Greater => 1,
        })
    }

    fn contract_event(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        topics: VecObject,
        data: Val,
    ) -> Result<Void, HostError> {
        self.record_contract_event(ContractEventType::Contract, topics, data)?;
        Ok(Val::VOID)
    }

    // Notes on metering: covered by the components.
    fn get_current_contract_address(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<AddressObject, HostError> {
        self.add_host_object(ScAddress::Contract(
            self.get_current_contract_id_internal()?,
        ))
    }

    impl_wrapping_obj_from_num!(obj_from_u64, u64, u64);
    impl_wrapping_obj_to_num!(obj_to_u64, u64, u64);
    impl_wrapping_obj_from_num!(obj_from_i64, i64, i64);
    impl_wrapping_obj_to_num!(obj_to_i64, i64, i64);
    impl_wrapping_obj_from_num!(timepoint_obj_from_u64, TimePoint, u64);
    impl_wrapping_obj_to_num!(timepoint_obj_to_u64, TimePoint, u64);
    impl_wrapping_obj_from_num!(duration_obj_from_u64, Duration, u64);
    impl_wrapping_obj_to_num!(duration_obj_to_u64, Duration, u64);

    fn obj_from_u128_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        hi: u64,
        lo: u64,
    ) -> Result<U128Object, Self::Error> {
        self.add_host_object(int128_helpers::u128_from_pieces(hi, lo))
    }

    fn obj_to_u128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok(int128_helpers::u128_lo(*u)))
    }

    fn obj_to_u128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok(int128_helpers::u128_hi(*u)))
    }

    fn obj_from_i128_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        hi: i64,
        lo: u64,
    ) -> Result<I128Object, Self::Error> {
        self.add_host_object(int128_helpers::i128_from_pieces(hi, lo))
    }

    fn obj_to_i128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |i: &i128| Ok(int128_helpers::i128_lo(*i)))
    }

    fn obj_to_i128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<i64, Self::Error> {
        self.visit_obj(obj, move |i: &i128| Ok(int128_helpers::i128_hi(*i)))
    }

    fn obj_from_u256_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        hi_hi: u64,
        hi_lo: u64,
        lo_hi: u64,
        lo_lo: u64,
    ) -> Result<U256Object, Self::Error> {
        self.add_host_object(u256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
    }

    fn u256_val_from_be_bytes(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        bytes: BytesObject,
    ) -> Result<U256Val, HostError> {
        let num = self.visit_obj(bytes, move |b: &ScBytes| {
            Ok(U256::from_be_bytes(self.fixed_length_bytes_from_slice(
                "U256 bytes",
                b.as_slice(),
            )?))
        })?;
        U256Val::try_from_val(self, &num).map_err(|_| ConversionError.into())
    }

    fn u256_val_to_be_bytes(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        val: U256Val,
    ) -> Result<BytesObject, HostError> {
        if let Ok(so) = U256Small::try_from(val) {
            self.add_host_object(self.scbytes_from_slice(&U256::from(so).to_be_bytes())?)
        } else {
            let obj = val.try_into()?;
            let scb = self.visit_obj(obj, move |u: &U256| {
                self.scbytes_from_slice(&u.to_be_bytes())
            })?;
            self.add_host_object(scb)
        }
    }

    fn obj_to_u256_hi_hi(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |u: &U256| {
            let (hi_hi, _, _, _) = u256_into_pieces(*u);
            Ok(hi_hi)
        })
    }

    fn obj_to_u256_hi_lo(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |u: &U256| {
            let (_, hi_lo, _, _) = u256_into_pieces(*u);
            Ok(hi_lo)
        })
    }

    fn obj_to_u256_lo_hi(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |u: &U256| {
            let (_, _, lo_hi, _) = u256_into_pieces(*u);
            Ok(lo_hi)
        })
    }

    fn obj_to_u256_lo_lo(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |u: &U256| {
            let (_, _, _, lo_lo) = u256_into_pieces(*u);
            Ok(lo_lo)
        })
    }

    fn obj_from_i256_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        hi_hi: i64,
        hi_lo: u64,
        lo_hi: u64,
        lo_lo: u64,
    ) -> Result<I256Object, Self::Error> {
        self.add_host_object(i256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
    }

    fn i256_val_from_be_bytes(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        bytes: BytesObject,
    ) -> Result<I256Val, HostError> {
        let num = self.visit_obj(bytes, move |b: &ScBytes| {
            Ok(I256::from_be_bytes(self.fixed_length_bytes_from_slice(
                "I256 bytes",
                b.as_slice(),
            )?))
        })?;
        I256Val::try_from_val(self, &num).map_err(|_| ConversionError.into())
    }

    fn i256_val_to_be_bytes(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        val: I256Val,
    ) -> Result<BytesObject, HostError> {
        if let Ok(so) = I256Small::try_from(val) {
            self.add_host_object(self.scbytes_from_slice(&I256::from(so).to_be_bytes())?)
        } else {
            let obj = val.try_into()?;
            let scb = self.visit_obj(obj, move |i: &I256| {
                self.scbytes_from_slice(&i.to_be_bytes())
            })?;
            self.add_host_object(scb)
        }
    }

    fn obj_to_i256_hi_hi(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<i64, HostError> {
        self.visit_obj(obj, move |i: &I256| {
            let (hi_hi, _, _, _) = i256_into_pieces(*i);
            Ok(hi_hi)
        })
    }

    fn obj_to_i256_hi_lo(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |i: &I256| {
            let (_, hi_lo, _, _) = i256_into_pieces(*i);
            Ok(hi_lo)
        })
    }

    fn obj_to_i256_lo_hi(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |i: &I256| {
            let (_, _, lo_hi, _) = i256_into_pieces(*i);
            Ok(lo_hi)
        })
    }

    fn obj_to_i256_lo_lo(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, move |i: &I256| {
            let (_, _, _, lo_lo) = i256_into_pieces(*i);
            Ok(lo_lo)
        })
    }

    impl_bignum_host_fns!(u256_add, checked_add, U256, U256Val, Int256AddSub);
    impl_bignum_host_fns!(u256_sub, checked_sub, U256, U256Val, Int256AddSub);
    impl_bignum_host_fns!(u256_mul, checked_mul, U256, U256Val, Int256Mul);
    impl_bignum_host_fns!(u256_div, checked_div, U256, U256Val, Int256Div);
    impl_bignum_host_fns_rhs_u32!(u256_pow, checked_pow, U256, U256Val, Int256Pow);
    impl_bignum_host_fns_rhs_u32!(u256_shl, checked_shl, U256, U256Val, Int256Shift);
    impl_bignum_host_fns_rhs_u32!(u256_shr, checked_shr, U256, U256Val, Int256Shift);

    impl_bignum_host_fns!(i256_add, checked_add, I256, I256Val, Int256AddSub);
    impl_bignum_host_fns!(i256_sub, checked_sub, I256, I256Val, Int256AddSub);
    impl_bignum_host_fns!(i256_mul, checked_mul, I256, I256Val, Int256Mul);
    impl_bignum_host_fns!(i256_div, checked_div, I256, I256Val, Int256Div);
    impl_bignum_host_fns_rhs_u32!(i256_pow, checked_pow, I256, I256Val, Int256Pow);
    impl_bignum_host_fns_rhs_u32!(i256_shl, checked_shl, I256, I256Val, Int256Shift);
    impl_bignum_host_fns_rhs_u32!(i256_shr, checked_shr, I256, I256Val, Int256Shift);

    fn map_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<MapObject, HostError> {
        self.add_host_object(HostMap::new())
    }

    fn map_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
        v: Val,
    ) -> Result<MapObject, HostError> {
        self.check_val_integrity(k)?;
        self.check_val_integrity(v)?;
        let mnew = self.visit_obj(m, |hm: &HostMap| hm.insert(k, v, self))?;
        self.add_host_object(mnew)
    }

    fn map_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<Val, HostError> {
        self.visit_obj(m, move |hm: &HostMap| {
            hm.get(&k, self)?.copied().ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::MissingValue,
                    "map key not found",
                    &[m.to_val(), k],
                )
            })
        })
    }

    fn map_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<MapObject, HostError> {
        match self.visit_obj(m, |hm: &HostMap| hm.remove(&k, self))? {
            Some((mnew, _)) => Ok(self.add_host_object(mnew)?),
            None => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::MissingValue,
                "map key not found",
                &[m.to_val(), k],
            )),
        }
    }

    fn map_len(&self, _vmcaller: &mut VmCaller<Host>, m: MapObject) -> Result<U32Val, HostError> {
        let len = self.visit_obj(m, |hm: &HostMap| Ok(hm.len()))?;
        self.usize_to_u32val(len)
    }

    fn map_has(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<Bool, HostError> {
        self.visit_obj(m, move |hm: &HostMap| Ok(hm.contains_key(&k, self)?.into()))
    }

    fn map_prev_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<Val, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_prev(&k, self)? {
                Ok(*pk)
            } else {
                // We return Ok(error) here to indicate "the end of iteration".
                Ok(
                    Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                        .to_val(),
                )
            }
        })
    }

    fn map_next_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<Val, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_next(&k, self)? {
                Ok(*pk)
            } else {
                // We return Ok(error) here to indicate "the end of iteration".
                Ok(
                    Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                        .to_val(),
                )
            }
        })
    }

    fn map_min_key(&self, _vmcaller: &mut VmCaller<Host>, m: MapObject) -> Result<Val, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_min(self)? {
            Some((pk, pv)) => Ok(*pk),
            None => Ok(
                Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds).to_val(),
            ),
        })
    }

    fn map_max_key(&self, _vmcaller: &mut VmCaller<Host>, m: MapObject) -> Result<Val, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_max(self)? {
            Some((pk, pv)) => Ok(*pk),
            None => Ok(
                Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds).to_val(),
            ),
        })
    }

    fn map_keys(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<VecObject, HostError> {
        let vec = self.visit_obj(m, |hm: &HostMap| {
            HostVec::from_exact_iter(hm.keys(self)?.cloned(), self.budget_ref())
        })?;
        self.add_host_object(vec)
    }

    fn map_values(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<VecObject, HostError> {
        let vec = self.visit_obj(m, |hm: &HostMap| {
            HostVec::from_exact_iter(hm.values(self)?.cloned(), self.budget_ref())
        })?;
        self.add_host_object(vec)
    }

    fn map_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        keys_pos: U32Val,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<MapObject, HostError> {
        // Step 1: extract all key symbols.
        let VmSlice {
            vm,
            pos: keys_pos,
            len,
        } = self.decode_vmslice(keys_pos, len)?;
        Vec::<Symbol>::charge_bulk_init(len as u64, self.as_budget())?;
        let mut key_syms: Vec<Symbol> = Vec::with_capacity(len as usize);
        self.metered_vm_scan_slices_in_linear_memory(
            vmcaller,
            &vm,
            keys_pos,
            len as usize,
            |n, slice| {
                self.charge_budget(ContractCostType::VmMemRead, Some(slice.len() as u64))?;
                let scsym = ScSymbol(slice.try_into()?);
                let sym = Symbol::try_from(self.to_host_val(&ScVal::Symbol(scsym))?)?;
                key_syms.push(sym);
                Ok(())
            },
        )?;

        // Step 2: extract all val Vals.
        let vals_pos: u32 = vals_pos.into();
        Vec::<Val>::charge_bulk_init(len as u64, self.as_budget())?;
        let mut vals: Vec<Val> = vec![Val::VOID.into(); len as usize];
        self.metered_vm_read_vals_from_linear_memory::<8, Val>(
            vmcaller,
            &vm,
            vals_pos,
            vals.as_mut_slice(),
            |buf| self.relative_to_absolute(Val::from_payload(u64::from_le_bytes(*buf))),
        )?;
        for v in vals.iter() {
            self.check_val_integrity(*v)?;
        }

        // Step 3: turn pairs into a map.
        let pair_iter = key_syms
            .iter()
            .map(|s| s.to_val())
            .zip(vals.iter().cloned());
        let map = HostMap::from_exact_iter(pair_iter, self)?;
        self.add_host_object(map)
    }

    fn map_unpack_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        map: MapObject,
        keys_pos: U32Val,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        let VmSlice {
            vm,
            pos: keys_pos,
            len,
        } = self.decode_vmslice(keys_pos, len)?;
        self.visit_obj(map, |mapobj: &HostMap| {
            // Step 1: check all key symbols.
            self.metered_vm_scan_slices_in_linear_memory(
                vmcaller,
                &vm,
                keys_pos,
                len as usize,
                |n, slice| {
                    let sym = Symbol::try_from(
                        mapobj
                            .map
                            .get(n)
                            .ok_or_else(|| {
                                self.err(
                                    ScErrorType::Object,
                                    ScErrorCode::IndexBounds,
                                    "vector out of bounds while unpacking map to linear memory",
                                    &[],
                                )
                            })?
                            .0,
                    )?;
                    self.check_symbol_matches(slice, sym)?;
                    Ok(())
                },
            )?;

            // Step 2: write all vals.
            self.metered_vm_write_vals_to_linear_memory(
                vmcaller,
                &vm,
                vals_pos.into(),
                mapobj.map.as_slice(),
                |pair| {
                    Ok(u64::to_le_bytes(
                        self.absolute_to_relative(pair.1)?.get_payload(),
                    ))
                },
            )?;
            Ok(())
        })?;

        Ok(Val::VOID)
    }

    fn vec_new(&self, _vmcaller: &mut VmCaller<Host>, c: Val) -> Result<VecObject, HostError> {
        // NB: we ignore capacity because vectors are immutable
        // and there's no reuse, we always size them exactly.
        let _capacity: usize = if c.is_void() {
            0
        } else {
            self.usize_from_rawval_u32_input("c", c)?
        };
        self.add_host_object(HostVec::new())
    }

    fn vec_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
        x: Val,
    ) -> Result<VecObject, HostError> {
        let i: u32 = i.into();
        self.check_val_integrity(x)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_lt_bound(i, hv.len())?;
            hv.set(i as usize, x, self.as_budget())
        })?;
        self.add_host_object(vnew)
    }

    fn vec_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
    ) -> Result<Val, HostError> {
        let i: u32 = i.into();
        self.visit_obj(v, move |hv: &HostVec| {
            hv.get(i as usize, self.as_budget()).map(|r| *r)
        })
    }

    fn vec_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
    ) -> Result<VecObject, HostError> {
        let i: u32 = i.into();
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_lt_bound(i, hv.len())?;
            hv.remove(i as usize, self.as_budget())
        })?;
        self.add_host_object(vnew)
    }

    fn vec_len(&self, _vmcaller: &mut VmCaller<Host>, v: VecObject) -> Result<U32Val, HostError> {
        let len = self.visit_obj(v, |hv: &HostVec| Ok(hv.len()))?;
        self.usize_to_u32val(len)
    }

    fn vec_push_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<VecObject, HostError> {
        self.check_val_integrity(x)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.push_front(x, self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_pop_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_front(self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_push_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<VecObject, HostError> {
        self.check_val_integrity(x)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.push_back(x, self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_pop_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_back(self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_front(&self, _vmcaller: &mut VmCaller<Host>, v: VecObject) -> Result<Val, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.front(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_back(&self, _vmcaller: &mut VmCaller<Host>, v: VecObject) -> Result<Val, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.back(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_insert(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
        x: Val,
    ) -> Result<VecObject, HostError> {
        let i: u32 = i.into();
        self.check_val_integrity(x)?;
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            self.validate_index_le_bound(i, hv.len())?;
            hv.insert(i as usize, x, self.as_budget())
        })?;
        self.add_host_object(vnew)
    }

    fn vec_append(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v1: VecObject,
        v2: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v1, |hv1: &HostVec| {
            self.visit_obj(v2, |hv2: &HostVec| {
                if hv1.len() > u32::MAX as usize - hv2.len() {
                    Err(self.err_arith_overflow())
                } else {
                    hv1.append(hv2, self.as_budget())
                }
            })
        })?;
        self.add_host_object(vnew)
    }

    fn vec_slice(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        start: U32Val,
        end: U32Val,
    ) -> Result<VecObject, HostError> {
        let start: u32 = start.into();
        let end: u32 = end.into();
        let vnew = self.visit_obj(v, move |hv: &HostVec| {
            let range = self.valid_range_from_start_end_bound(start, end, hv.len())?;
            hv.slice(range, self.as_budget())
        })?;
        self.add_host_object(vnew)
    }

    fn vec_first_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<Val, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.first_index_of(|other| self.compare(&x, other), self.as_budget())? {
                    Some(u) => self.usize_to_u32val(u)?.into(),
                    None => Val::VOID.into(),
                },
            )
        })
    }

    fn vec_last_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<Val, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.last_index_of(|other| self.compare(&x, other), self.as_budget())? {
                    Some(u) => self.usize_to_u32val(u)?.into(),
                    None => Val::VOID.into(),
                },
            )
        })
    }

    fn vec_binary_search(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            let res = hv.binary_search_by(|probe| self.compare(probe, &x), self.as_budget())?;
            self.u64_from_binary_search_result(res)
        })
    }

    fn vec_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<VecObject, HostError> {
        let VmSlice { vm, pos, len } = self.decode_vmslice(vals_pos, len)?;
        Vec::<Val>::charge_bulk_init(len as u64, self.as_budget())?;
        let mut vals: Vec<Val> = vec![Val::VOID.to_val(); len as usize];
        self.metered_vm_read_vals_from_linear_memory::<8, Val>(
            vmcaller,
            &vm,
            pos,
            vals.as_mut_slice(),
            |buf| self.relative_to_absolute(Val::from_payload(u64::from_le_bytes(*buf))),
        )?;
        for v in vals.iter() {
            self.check_val_integrity(*v)?;
        }
        self.add_host_object(HostVec::from_vec(vals)?)
    }

    fn vec_unpack_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vec: VecObject,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        let VmSlice { vm, pos, len } = self.decode_vmslice(vals_pos, len)?;
        self.visit_obj(vec, |vecobj: &HostVec| {
            self.metered_vm_write_vals_to_linear_memory(
                vmcaller,
                &vm,
                vals_pos.into(),
                vecobj.as_slice(),
                |x| {
                    Ok(u64::to_le_bytes(
                        self.absolute_to_relative(*x)?.get_payload(),
                    ))
                },
            )
        })?;
        Ok(Val::VOID)
    }

    // Notes on metering: covered by components
    fn put_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        v: Val,
        t: StorageType,
        f: Val,
    ) -> Result<Void, HostError> {
        self.check_val_integrity(k)?;
        self.check_val_integrity(v)?;
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                self.put_contract_data_into_ledger(k, v, t, f)?
            }
            StorageType::Instance => self.with_mut_instance_storage(|s| {
                s.map = s.map.insert(k, v, self)?;
                Ok(())
            })?,
        };

        Ok(Val::VOID)
    }

    // Notes on metering: covered by components
    fn has_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        t: StorageType,
    ) -> Result<Bool, HostError> {
        let res = match t {
            StorageType::Temporary | StorageType::Persistent => {
                let key = self.storage_key_from_rawval(k, t.try_into()?)?;
                self.try_borrow_storage_mut()?.has(&key, self.as_budget())?
            }
            StorageType::Instance => {
                self.with_instance_storage(|s| Ok(s.map.get(&k, self)?.is_some()))?
            }
        };

        Ok(Val::from_bool(res))
    }

    // Notes on metering: covered by components
    fn get_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        t: StorageType,
    ) -> Result<Val, HostError> {
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                let key = self.storage_key_from_rawval(k, t.try_into()?)?;
                let entry = self.try_borrow_storage_mut()?.get(&key, self.as_budget())?;
                match &entry.data {
                    LedgerEntryData::ContractData(ContractDataEntry { body, .. }) => match body {
                        ContractDataEntryBody::DataEntry(data) => Ok(self.to_host_val(&data.val)?),
                        _ => Err(self.err(
                            ScErrorType::Storage,
                            ScErrorCode::UnexpectedType,
                            "expected DataEntry",
                            &[],
                        )),
                    },
                    _ => Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::UnexpectedType,
                        "expected contract data ledger entry",
                        &[],
                    )),
                }
            }
            StorageType::Instance => self.with_instance_storage(|s| {
                s.map
                    .get(&k, self)?
                    .ok_or_else(|| {
                        self.err(
                            ScErrorType::Storage,
                            ScErrorCode::MissingValue,
                            "key is missing from instance storage",
                            &[k],
                        )
                    })
                    .copied()
            }),
        }
    }

    // Notes on metering: covered by components
    fn del_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        t: StorageType,
    ) -> Result<Void, HostError> {
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                let key = self.contract_data_key_from_rawval(k, t.try_into()?)?;
                self.try_borrow_storage_mut()?.del(&key, self.as_budget())?;
            }
            StorageType::Instance => {
                self.with_mut_instance_storage(|s| {
                    if let Some((new_map, _)) = s.map.remove(&k, self)? {
                        s.map = new_map;
                    }
                    Ok(())
                })?;
            }
        }

        Ok(Val::VOID)
    }

    // Notes on metering: covered by components
    fn bump_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        t: StorageType,
        min: U32Val,
    ) -> Result<Void, HostError> {
        if matches!(t, StorageType::Instance) {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidAction,
                "instance storage should be bumped via `bump_current_contract_instance_and_code` function only",
                &[],
            ))?;
        }
        let key = self.contract_data_key_from_rawval(k, t.try_into()?)?;
        self.try_borrow_storage_mut()?.bump(self, key, min.into())?;
        Ok(Val::VOID)
    }

    fn bump_current_contract_instance_and_code(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        min: U32Val,
    ) -> Result<Void, HostError> {
        let contract_id = self.get_current_contract_id_internal()?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        self.try_borrow_storage_mut()?
            .bump(self, key.clone(), min.into())?;
        match self
            .retrieve_contract_instance_from_storage(&key)?
            .executable
        {
            ContractExecutable::Wasm(wasm_hash) => {
                let key = self.contract_code_ledger_key(&wasm_hash)?;
                self.try_borrow_storage_mut()?.bump(self, key, min.into())?;
            }
            ContractExecutable::Token => {}
        }
        Ok(Val::VOID)
    }

    fn bump_contract_instance_and_code(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        contract: AddressObject,
        min: U32Val,
    ) -> Result<Void, Self::Error> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        self.try_borrow_storage_mut()?
            .bump(self, key.clone(), min.into())?;
        match self
            .retrieve_contract_instance_from_storage(&key)?
            .executable
        {
            ContractExecutable::Wasm(wasm_hash) => {
                let key = self.contract_code_ledger_key(&wasm_hash)?;
                self.try_borrow_storage_mut()?.bump(self, key, min.into())?;
            }
            ContractExecutable::Token => {}
        }
        Ok(Val::VOID)
    }

    // Notes on metering: covered by the components.
    fn create_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        deployer: AddressObject,
        wasm_hash: BytesObject,
        salt: BytesObject,
    ) -> Result<AddressObject, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| {
                addr.metered_clone(self.budget_ref())
            })?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });
        let executable =
            ContractExecutable::Wasm(self.hash_from_bytesobj_input("wasm_hash", wasm_hash)?);
        let args = CreateContractArgs {
            contract_id_preimage,
            executable,
        };
        self.create_contract_internal(Some(deployer), args)
    }

    // Notes on metering: covered by the components.
    fn create_asset_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        serialized_asset: BytesObject,
    ) -> Result<AddressObject, HostError> {
        let asset: Asset = self.metered_from_xdr_obj(serialized_asset)?;
        let contract_id_preimage = ContractIdPreimage::Asset(asset);
        let executable = ContractExecutable::Token;
        let args = CreateContractArgs {
            contract_id_preimage,
            executable,
        };
        // Asset contracts don't need any deployer authorization (they're tied
        // to the asset issuers instead).
        self.create_contract_internal(None, args)
    }

    // Notes on metering: covered by the components.
    fn get_contract_id(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        deployer: AddressObject,
        salt: BytesObject,
    ) -> Result<AddressObject, HostError> {
        let hash_id = self.get_contract_id_hash(deployer, salt)?;
        self.add_host_object(ScAddress::Contract(hash_id))
    }

    // Notes on metering: covered by the components.
    fn get_asset_contract_id(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        serialized_asset: BytesObject,
    ) -> Result<AddressObject, HostError> {
        let asset: Asset = self.metered_from_xdr_obj(serialized_asset)?;
        let hash_id = self.get_asset_contract_id_hash(asset)?;
        self.add_host_object(ScAddress::Contract(hash_id))
    }

    fn upload_wasm(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        wasm: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let wasm_vec = self.visit_obj(wasm, |bytes: &ScBytes| {
            bytes.as_vec().metered_clone(self.budget_ref())
        })?;
        self.upload_contract_wasm(wasm_vec)
    }

    fn update_current_contract_wasm(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        hash: BytesObject,
    ) -> Result<Void, HostError> {
        let wasm_hash = self.hash_from_bytesobj_input("wasm_hash", hash)?;
        if !self.wasm_exists(&wasm_hash)? {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::MissingValue,
                "Wasm does not exist",
                &[hash.to_val()],
            ));
        }
        let curr_contract_id = self.get_current_contract_id_internal()?;
        let key = self.contract_instance_ledger_key(&curr_contract_id)?;
        let mut instance = self.retrieve_contract_instance_from_storage(&key)?;
        let new_executable = ContractExecutable::Wasm(wasm_hash);
        self.emit_update_contract_event(&instance.executable, &new_executable)?;
        instance.executable = new_executable;
        self.store_contract_instance(instance, curr_contract_id, &key)?;
        Ok(Val::VOID)
    }

    // Notes on metering: here covers the args unpacking. The actual VM work is changed at lower layers.
    fn call(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        contract_address: AddressObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<Val, HostError> {
        let argvec = self.call_args_from_obj(args)?;
        // this is the recommended path of calling a contract, with `reentry`
        // always set `ContractReentryMode::Prohibited`
        let res = self.call_n_internal(
            &self.contract_id_from_address(contract_address)?,
            func,
            argvec.as_slice(),
            ContractReentryMode::Prohibited,
            false,
        );
        if let Err(e) = &res {
            self.with_events_mut(|events| {
                self.err_diagnostics(
                    events,
                    e.error,
                    "contract call failed",
                    &[func.to_val(), args.to_val()],
                )
            })?;
        }
        res
    }

    // Notes on metering: covered by the components.
    fn try_call(
        &self,
        vmcaller: &mut VmCaller<Host>,
        contract_address: AddressObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<Val, HostError> {
        let argvec = self.call_args_from_obj(args)?;
        // this is the "loosened" path of calling a contract.
        // TODO: A `reentry` flag will be passed from `try_call` into here.
        // For now, we are passing in `ContractReentryMode::Prohibited` to disable
        // reentry.
        let res = self.call_n_internal(
            &self.contract_id_from_address(contract_address)?,
            func,
            argvec.as_slice(),
            ContractReentryMode::Prohibited,
            false,
        );
        match res {
            Ok(rv) => Ok(rv),
            Err(e) => {
                self.with_events_mut(|events| {
                    self.err_diagnostics(
                        events,
                        e.error,
                        "contract try_call failed",
                        &[func.to_val(), args.to_val()],
                    )
                })?;
                // Only allow to gracefully handle the recoverable errors.
                // Non-recoverable errors should still cause guest to panic and
                // abort execution.
                if e.is_recoverable() {
                    Ok(e.error.to_val())
                } else {
                    Err(e)
                }
            }
        }
    }

    // Notes on metering: covered by components
    fn serialize_to_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: Val,
    ) -> Result<BytesObject, HostError> {
        let scv = self.from_host_val(v)?;
        let mut buf = Vec::<u8>::new();
        metered_write_xdr(self.budget_ref(), &scv, &mut buf)?;
        self.add_host_object(self.scbytes_from_vec(buf)?)
    }

    // Notes on metering: covered by components
    fn deserialize_from_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<Val, HostError> {
        let scv = self.visit_obj(b, |hv: &ScBytes| {
            self.metered_from_xdr::<ScVal>(hv.as_slice())
        })?;
        self.to_host_val(&scv)
    }

    fn string_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        s: StringObject,
        s_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        self.memobj_copy_to_linear_memory::<ScString>(vmcaller, s, s_pos, lm_pos, len)?;
        Ok(Val::VOID)
    }

    fn symbol_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        s: SymbolObject,
        s_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        self.memobj_copy_to_linear_memory::<ScSymbol>(vmcaller, s, s_pos, lm_pos, len)?;
        Ok(Val::VOID)
    }

    fn bytes_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        b_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        self.memobj_copy_to_linear_memory::<ScBytes>(vmcaller, b, b_pos, lm_pos, len)?;
        Ok(Val::VOID)
    }

    fn bytes_copy_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        b_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<BytesObject, HostError> {
        self.memobj_copy_from_linear_memory::<ScBytes>(vmcaller, b, b_pos, lm_pos, len)
    }

    fn bytes_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<BytesObject, HostError> {
        self.memobj_new_from_linear_memory::<ScBytes>(vmcaller, lm_pos, len)
    }

    fn string_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<StringObject, HostError> {
        self.memobj_new_from_linear_memory::<ScString>(vmcaller, lm_pos, len)
    }

    fn symbol_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<SymbolObject, HostError> {
        self.memobj_new_from_linear_memory::<ScSymbol>(vmcaller, lm_pos, len)
    }

    fn symbol_index_in_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        sym: Symbol,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<U32Val, HostError> {
        let VmSlice { vm, pos, len } = self.decode_vmslice(lm_pos, len)?;
        let mut found = None;
        self.metered_vm_scan_slices_in_linear_memory(
            vmcaller,
            &vm,
            pos,
            len as usize,
            |i, slice| {
                if self.symbol_matches(slice, sym)? {
                    if found.is_none() {
                        found = Some(self.usize_to_u32(i)?)
                    }
                }
                Ok(())
            },
        )?;
        match found {
            None => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "symbol not found in linear memory slices",
                &[sym.to_val()],
            )),
            Some(idx) => Ok(U32Val::from(idx)),
        }
    }

    // Notes on metering: covered by `add_host_object`
    fn bytes_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<BytesObject, HostError> {
        self.add_host_object(self.scbytes_from_vec(Vec::<u8>::new())?)
    }

    // Notes on metering: `get_mut` is free
    fn bytes_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        iv: U32Val,
        u: U32Val,
    ) -> Result<BytesObject, HostError> {
        let i: u32 = iv.into();
        let u = self.u8_from_u32val_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            let mut vnew: Vec<u8> = hv.metered_clone(&self.0.budget)?.into();
            match vnew.get_mut(i as usize) {
                None => Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::IndexBounds,
                    "bytes_put out of bounds",
                    &[iv.to_val()],
                )),
                Some(v) => {
                    *v = u;
                    Ok(ScBytes(vnew.try_into()?))
                }
            }
        })?;
        self.add_host_object(vnew)
    }

    // Notes on metering: `get` is free
    fn bytes_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        iv: U32Val,
    ) -> Result<U32Val, HostError> {
        let i: u32 = iv.into();
        self.visit_obj(b, |hv: &ScBytes| {
            hv.get(i as usize)
                .map(|u| Into::<U32Val>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| {
                    self.err(
                        ScErrorType::Object,
                        ScErrorCode::IndexBounds,
                        "bytes_get out of bounds",
                        &[iv.to_val()],
                    )
                })
        })
    }

    fn bytes_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        i: U32Val,
    ) -> Result<BytesObject, HostError> {
        let i: u32 = i.into();
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            self.validate_index_lt_bound(i, hv.len())?;
            let mut vnew: Vec<u8> = hv.metered_clone(&self.0.budget)?.into();
            // len > i has been verified above but use saturating_sub just in case
            let n_elts = (hv.len() as u64).saturating_sub(i as u64);
            // remove elements incurs the cost of moving bytes, it does not incur
            // allocation/deallocation
            metered_clone::charge_shallow_copy::<u8>(n_elts, self.as_budget())?;
            vnew.remove(i as usize);
            Ok(ScBytes(vnew.try_into()?))
        })?;
        self.add_host_object(vnew)
    }

    // Notes on metering: `len` is free
    fn bytes_len(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<U32Val, HostError> {
        let len = self.visit_obj(b, |hv: &ScBytes| Ok(hv.len()))?;
        self.usize_to_u32val(len)
    }

    // Notes on metering: `len` is free
    fn string_len(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: StringObject,
    ) -> Result<U32Val, HostError> {
        let len = self.visit_obj(b, |hv: &ScString| Ok(hv.len()))?;
        self.usize_to_u32val(len)
    }

    // Notes on metering: `len` is free
    fn symbol_len(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: SymbolObject,
    ) -> Result<U32Val, HostError> {
        let len = self.visit_obj(b, |hv: &ScSymbol| Ok(hv.len()))?;
        self.usize_to_u32val(len)
    }

    // Notes on metering: `push` is free
    fn bytes_push(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        u: U32Val,
    ) -> Result<BytesObject, HostError> {
        let u = self.u8_from_u32val_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            // we allocate the new vector to be able to hold `len + 1` bytes, so that the push
            // will not trigger a reallocation, causing data to be cloned twice.
            let len = hv.len().saturating_add(1);
            metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            let mut vnew: Vec<u8> = Vec::with_capacity(len);
            vnew.extend_from_slice(hv.as_slice());
            vnew.push(u);
            Ok(ScBytes(vnew.try_into()?))
        })?;
        self.add_host_object(vnew)
    }

    // Notes on metering: `pop` is free
    fn bytes_pop(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            let mut vnew: Vec<u8> = hv.metered_clone(self.as_budget())?.into();
            // Popping will not trigger reallocation. Here we don't charge anything since this is
            // just a `len` reduction.
            if vnew.pop().is_none() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::IndexBounds,
                    "bytes_pop out of bounds",
                    &[],
                ));
            }
            Ok(ScBytes(vnew.try_into()?))
        })?;
        self.add_host_object(vnew)
    }

    // Notes on metering: `first` is free
    fn bytes_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<U32Val, HostError> {
        self.visit_obj(b, |hv: &ScBytes| {
            hv.first()
                .map(|u| Into::<U32Val>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| {
                    self.err(
                        ScErrorType::Object,
                        ScErrorCode::IndexBounds,
                        "bytes_front out of bounds",
                        &[],
                    )
                })
        })
    }

    // Notes on metering: `last` is free
    fn bytes_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<U32Val, HostError> {
        self.visit_obj(b, |hv: &ScBytes| {
            hv.last()
                .map(|u| Into::<U32Val>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| {
                    self.err(
                        ScErrorType::Object,
                        ScErrorCode::IndexBounds,
                        "bytes_back out of bounds",
                        &[],
                    )
                })
        })
    }

    fn bytes_insert(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        i: U32Val,
        u: U32Val,
    ) -> Result<BytesObject, HostError> {
        let i: u32 = i.into();
        let u = self.u8_from_u32val_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            self.validate_index_le_bound(i, hv.len())?;
            // we allocate the new vector to be able to hold `len + 1` bytes, so that the push
            // will not trigger a reallocation, causing data to be cloned twice.
            let len = hv.len().saturating_add(1);
            metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            let mut vnew: Vec<u8> = Vec::with_capacity(len);
            vnew.extend_from_slice(hv.as_slice());
            // insert will cause the memcpy by shifting all the values at and after `i`.
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            vnew.insert(i as usize, u);
            Ok(ScBytes(vnew.try_into()?))
        })?;
        self.add_host_object(vnew)
    }

    fn bytes_append(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b1: BytesObject,
        b2: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let vnew = self.visit_obj(b1, |sb1: &ScBytes| {
            self.visit_obj(b2, |sb2: &ScBytes| {
                if sb2.len() > u32::MAX as usize - sb1.len() {
                    return Err(self.err_arith_overflow());
                }
                // we allocate large enough memory to hold the new combined vector, so that
                // allocation only happens once, and charge for it upfront.
                // we already checked above that `len` will not overflow, here using
                // saturating_add just in case.
                let len = sb1.len().saturating_add(sb2.len());
                metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
                metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
                let mut vnew: Vec<u8> = Vec::with_capacity(len);
                vnew.extend_from_slice(sb1.as_slice());
                vnew.extend_from_slice(sb2.as_slice());
                Ok(vnew)
            })
        })?;
        self.add_host_object(ScBytes(vnew.try_into()?))
    }

    fn bytes_slice(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        start: U32Val,
        end: U32Val,
    ) -> Result<BytesObject, HostError> {
        let start: u32 = start.into();
        let end: u32 = end.into();
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            let range = self.valid_range_from_start_end_bound(start, end, hv.len())?;
            self.metered_slice_to_vec(&hv.as_slice()[range])
        })?;
        self.add_host_object(self.scbytes_from_vec(vnew)?)
    }

    // Notes on metering: covered by components.
    fn compute_hash_sha256(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let hash = self.sha256_hash_from_bytesobj_input(x)?;
        self.add_host_object(self.scbytes_from_vec(hash)?)
    }

    // Notes on metering: covered by components.
    fn compute_hash_keccak256(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        x: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let hash = self.keccak256_hash_from_bytesobj_input(x)?;
        self.add_host_object(self.scbytes_from_vec(hash)?)
    }

    // Notes on metering: covered by components.
    fn verify_sig_ed25519(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: BytesObject,
        x: BytesObject,
        s: BytesObject,
    ) -> Result<Void, HostError> {
        let public_key = self.ed25519_pub_key_from_bytesobj_input(k)?;
        let sig = self.ed25519_signature_from_bytesobj_input("sig", s)?;
        let res = self.visit_obj(x, |payload: &ScBytes| {
            self.verify_sig_ed25519_internal(payload.as_slice(), &public_key, &sig)
        });
        Ok(res?.into())
    }

    fn recover_key_ecdsa_secp256k1(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        msg_digest: BytesObject,
        signature: BytesObject,
        recovery_id: U32Val,
    ) -> Result<BytesObject, HostError> {
        let sig = self.secp256k1_signature_from_bytesobj_input(signature)?;
        let rid = self.secp256k1_recovery_id_from_u32val(recovery_id)?;
        let hash = self.hash_from_bytesobj_input("msg_digest", msg_digest)?;
        self.recover_key_ecdsa_secp256k1_internal(&hash, &sig, rid)
    }

    fn get_ledger_version(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U32Val, Self::Error> {
        self.with_ledger_info(|li| Ok(li.protocol_version.into()))
    }

    fn get_ledger_sequence(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U32Val, Self::Error> {
        self.with_ledger_info(|li| Ok(li.sequence_number.into()))
    }

    fn get_ledger_timestamp(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U64Val, Self::Error> {
        self.with_ledger_info(|li| Ok(self.add_host_object(li.timestamp)?.into()))
    }

    fn get_ledger_network_id(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<BytesObject, Self::Error> {
        self.with_ledger_info(|li| {
            self.add_host_object(self.scbytes_from_slice(li.network_id.as_slice())?)
        })
    }

    fn get_current_call_stack(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<VecObject, HostError> {
        let contexts = self.try_borrow_context()?;

        let get_host_val_tuple = |id: &Hash, function: &Symbol| -> Result<[Val; 2], HostError> {
            let addr_val = self
                .add_host_object(ScAddress::Contract(id.metered_clone(self.as_budget())?))?
                .into();
            let function_val = (*function).into();
            Ok([addr_val, function_val])
        };

        let mut outer = Vec::with_capacity(contexts.len());
        for context in contexts.iter() {
            let vals = match &context.frame {
                Frame::ContractVM { vm, fn_name, .. } => {
                    get_host_val_tuple(&vm.contract_id, fn_name)?
                }
                Frame::HostFunction(_) => continue,
                Frame::Token(id, function, ..) => get_host_val_tuple(id, function)?,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => get_host_val_tuple(&tc.id, &tc.func)?,
            };
            let inner = MeteredVector::from_array(&vals, self.as_budget())?;
            outer.push(self.add_host_object(inner)?.into());
        }
        self.add_host_object(HostVec::from_vec(outer)?)
    }

    fn fail_with_error(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        error: Error,
    ) -> Result<Void, Self::Error> {
        if error.is_type(ScErrorType::Contract) {
            Err(self.error(
                error,
                "failing with contract error",
                &[U32Val::from(error.get_code()).to_val()],
            ))
        } else {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::UnexpectedType,
                "contract attempted to fail with non-ContractError status code",
                &[error.to_val()],
            ))
        }
    }

    fn dummy0(&self, vmcaller: &mut VmCaller<Self::VmUserState>) -> Result<Val, Self::Error> {
        Ok(().into())
    }

    fn require_auth_for_args(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
        args: VecObject,
    ) -> Result<Void, Self::Error> {
        let args = self.visit_obj(args, |a: &HostVec| a.to_vec(self.budget_ref()))?;
        Ok(self
            .try_borrow_authorization_manager()?
            .require_auth(self, address, args)?
            .into())
    }

    fn require_auth(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<Void, Self::Error> {
        let args = self.with_current_frame(|f| {
            let args = match f {
                Frame::ContractVM { args, .. } => args,
                Frame::HostFunction(_) => {
                    return Err(self.err(
                        ScErrorType::Context,
                        ScErrorCode::InternalError,
                        "require_auth is not suppported for host fns",
                        &[],
                    ))
                }
                Frame::Token(_, _, args, _) => args,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(c) => &c.args,
            };
            args.metered_clone(self.budget_ref())
        })?;

        Ok(self
            .try_borrow_authorization_manager()?
            .require_auth(self, address, args)?
            .into())
    }

    fn authorize_as_curr_contract(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        auth_entries: VecObject,
    ) -> Result<Void, HostError> {
        Ok(self
            .try_borrow_authorization_manager()?
            .add_invoker_contract_auth(self, auth_entries)?
            .into())
    }

    fn account_public_key_to_address(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        pk_bytes: BytesObject,
    ) -> Result<AddressObject, Self::Error> {
        let account_id = self.account_id_from_bytesobj(pk_bytes)?;
        self.add_host_object(ScAddress::Account(account_id))
    }

    fn contract_id_to_address(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        contract_id_bytes: BytesObject,
    ) -> Result<AddressObject, Self::Error> {
        let contract_id = self.hash_from_bytesobj_input("contract_id", contract_id_bytes)?;
        self.add_host_object(ScAddress::Contract(contract_id))
    }

    fn address_to_account_public_key(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<Val, Self::Error> {
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;
        match addr {
            ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(pk))) => Ok(self
                .add_host_object(ScBytes(self.metered_slice_to_vec(&pk.0)?.try_into()?))?
                .into()),
            ScAddress::Contract(_) => Ok(().into()),
        }
    }

    fn address_to_contract_id(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<Val, Self::Error> {
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;
        match addr {
            ScAddress::Account(_) => Ok(().into()),
            ScAddress::Contract(Hash(h)) => Ok(self
                .add_host_object(ScBytes(self.metered_slice_to_vec(&h)?.try_into()?))?
                .into()),
        }
    }

    fn prng_reseed(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        seed: BytesObject,
    ) -> Result<Void, Self::Error> {
        self.visit_obj(seed, |bytes: &ScBytes| {
            let slice: &[u8] = bytes.as_ref();
            self.charge_budget(ContractCostType::HostMemCpy, Some(prng::SEED_BYTES as u64))?;
            if let Ok(seed32) = slice.try_into() {
                self.with_current_prng(|prng| {
                    *prng = Prng::new_from_seed(seed32);
                    Ok(())
                })?;
                Ok(Val::VOID)
            } else if let Ok(len) = u32::try_from(slice.len()) {
                Err(self.err(
                    ScErrorType::Value,
                    ScErrorCode::UnexpectedSize,
                    "Unexpected size of BytesObject in prng_reseed",
                    &[U32Val::from(len).to_val()],
                ))
            } else {
                Err(self.err(
                    ScErrorType::Value,
                    ScErrorCode::UnexpectedSize,
                    "Unexpected size of BytesObject in prng_reseed",
                    &[],
                ))
            }
        })
    }

    fn prng_bytes_new(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        length: U32Val,
    ) -> Result<BytesObject, Self::Error> {
        self.add_host_object(
            self.with_current_prng(|prng| prng.bytes_new(length.into(), self.as_budget()))?,
        )
    }

    fn prng_u64_in_inclusive_range(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        lo: u64,
        hi: u64,
    ) -> Result<u64, Self::Error> {
        self.with_current_prng(|prng| prng.u64_in_inclusive_range(lo..=hi, self.as_budget()))
    }

    fn prng_vec_shuffle(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        vec: VecObject,
    ) -> Result<VecObject, Self::Error> {
        let vnew = self.visit_obj(vec, |v: &HostVec| {
            self.with_current_prng(|prng| prng.vec_shuffle(v, self.as_budget()))
        })?;
        self.add_host_object(vnew)
    }
}

#[cfg(any(test, feature = "testutils"))]
pub(crate) mod testutils {
    use std::cell::Cell;
    use std::panic::{catch_unwind, set_hook, take_hook, UnwindSafe};
    use std::sync::Once;

    /// Catch panics while suppressing the default panic hook that prints to the
    /// console.
    ///
    /// For the purposes of test reporting we don't want every panicking (but
    /// caught) contract call to print to the console. This requires overriding
    /// the panic hook, a global resource. This is an awkward thing to do with
    /// tests running in parallel.
    ///
    /// This function lazily performs a one-time wrapping of the existing panic
    /// hook. It then uses a thread local variable to track contract call depth.
    /// If a panick occurs during a contract call the original hook is not
    /// called, otherwise it is called.
    pub fn call_with_suppressed_panic_hook<C, R>(closure: C) -> std::thread::Result<R>
    where
        C: FnOnce() -> R + UnwindSafe,
    {
        thread_local! {
            static TEST_CONTRACT_CALL_COUNT: Cell<u64> = Cell::new(0);
        }

        static WRAP_PANIC_HOOK: Once = Once::new();

        WRAP_PANIC_HOOK.call_once(|| {
            let existing_panic_hook = take_hook();
            set_hook(Box::new(move |info| {
                let calling_test_contract = TEST_CONTRACT_CALL_COUNT.with(|c| c.get() != 0);
                if !calling_test_contract {
                    existing_panic_hook(info)
                }
            }))
        });

        TEST_CONTRACT_CALL_COUNT.with(|c| {
            let old_count = c.get();
            let new_count = old_count.checked_add(1).expect("overflow");
            c.set(new_count);
        });

        let res = catch_unwind(closure);

        TEST_CONTRACT_CALL_COUNT.with(|c| {
            let old_count = c.get();
            let new_count = old_count.checked_sub(1).expect("overflow");
            c.set(new_count);
        });

        res
    }
}
