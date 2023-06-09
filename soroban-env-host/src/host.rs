#![allow(unused_variables)]
#![allow(dead_code)]

use core::{cell::RefCell, cmp::Ordering, fmt::Debug};
use std::rc::Rc;

use crate::{
    auth::{AuthorizationManager, RecordedAuthPayload},
    budget::{AsBudget, Budget},
    err,
    events::{diagnostic::DiagnosticLevel, Events, InternalEventsBuffer},
    host_object::{HostMap, HostObject, HostVec},
    num::{i256_from_pieces, i256_into_pieces, u256_from_pieces, u256_into_pieces},
    storage::Storage,
    xdr::{
        int128_helpers, AccountId, Asset, ContractCodeEntry, ContractCostType, ContractDataEntry,
        ContractEventType, CreateContractArgs, ExtensionPoint, Hash, LedgerEntryData, LedgerKey,
        LedgerKeyContractCode, PublicKey, ScAddress, ScBytes, ScContractExecutable, ScErrorType,
        ScString, ScSymbol, ScVal,
    },
    AddressObject, Bool, BytesObject, Error, I128Object, I256Object, I64Object, MapObject,
    StringObject, SymbolObject, SymbolSmall, SymbolStr, TryFromVal, U128Object, U256Object, U32Val,
    U64Object, U64Val, VecObject, VmCaller, VmCallerEnv, Void, I256, U256,
};

use crate::Vm;
use crate::{EnvBase, Object, RawVal, Symbol};

pub(crate) mod comparison;
mod conversion;
mod data_helper;
pub(crate) mod declared_size;
pub(crate) mod error;
pub(crate) mod frame;
pub(crate) mod invoker_type;
mod mem_helper;
pub(crate) mod metered_clone;
pub(crate) mod metered_map;
pub(crate) mod metered_vector;
pub(crate) mod metered_xdr;
mod prng;
pub use prng::{Seed, SEED_BYTES};
mod validity;
pub use error::HostError;
use soroban_env_common::xdr::{ContractIdPreimage, ContractIdPreimageFromAddress, ScErrorCode};

use self::metered_clone::MeteredClone;
use self::{
    frame::{Context, ContractReentryMode},
    metered_vector::MeteredVector,
    prng::Prng,
};
use crate::Compare;
#[cfg(any(test, feature = "testutils"))]
use crate::TryIntoVal;
pub(crate) use frame::Frame;
#[cfg(any(test, feature = "testutils"))]
pub use frame::{ContractFunctionSet, TestContractFrame};
#[cfg(any(test, feature = "testutils"))]
use soroban_env_common::xdr::SorobanAuthorizedInvocation;

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
#[derive(Default, Clone)]
pub struct Host(pub(crate) Rc<HostImpl>);

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
        Self(Rc::new(HostImpl {
            source_account: RefCell::new(None),
            ledger: RefCell::new(None),
            objects: Default::default(),
            storage: RefCell::new(storage),
            context: Default::default(),
            budget: budget.clone(),
            events: Default::default(),
            authorization_manager: RefCell::new(
                AuthorizationManager::new_enforcing_without_authorizations(budget),
            ),
            diagnostic_level: Default::default(),
            base_prng: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            contracts: Default::default(),
            #[cfg(any(test, feature = "testutils"))]
            previous_authorization_manager: RefCell::new(None),
        }))
    }

    pub fn set_source_account(&self, source_account: AccountId) {
        *self.0.source_account.borrow_mut() = Some(source_account);
    }

    #[cfg(any(test, feature = "testutils"))]
    pub fn remove_source_account(&self) {
        *self.0.source_account.borrow_mut() = None;
    }

    pub fn source_account(&self) -> Option<AccountId> {
        self.0.source_account.borrow().clone()
    }

    pub fn switch_to_recording_auth(&self) {
        *self.0.authorization_manager.borrow_mut() =
            AuthorizationManager::new_recording(self.budget_cloned());
    }

    pub fn set_authorization_entries(
        &self,
        auth_entries: Vec<soroban_env_common::xdr::SorobanAuthorizationEntry>,
    ) -> Result<(), HostError> {
        let new_auth_manager = AuthorizationManager::new_enforcing(self, auth_entries)?;
        *self.0.authorization_manager.borrow_mut() = new_auth_manager;
        Ok(())
    }

    pub fn set_base_prng_seed(&self, seed: prng::Seed) {
        *self.0.base_prng.borrow_mut() = Some(Prng::new_from_seed(seed))
    }

    pub fn set_ledger_info(&self, info: LedgerInfo) {
        *self.0.ledger.borrow_mut() = Some(info)
    }

    pub fn with_ledger_info<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&LedgerInfo) -> Result<T, HostError>,
    {
        match self.0.ledger.borrow().as_ref() {
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
        match self.0.ledger.borrow_mut().as_mut() {
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
        f(&mut self.0.storage.borrow_mut())
    }

    /// Accept a _unique_ (refcount = 1) host reference and destroy the
    /// underlying [`HostImpl`], returning its constituent components to the
    /// caller as a tuple wrapped in `Ok(...)`. If the provided host reference
    /// is not unique, returns `Err(self)`.
    pub fn try_finish(self) -> Result<(Storage, Budget, Events), (Self, HostError)> {
        let events = self
            .0
            .events
            .borrow()
            .externalize(&self)
            .map_err(|e| (self.clone(), e))?;

        // TODO: find a better error status to represent "internal logic error". Here the error
        // means the Rc does not have a unique strong reference.
        Rc::try_unwrap(self.0)
            .map(|host_impl| {
                let storage = host_impl.storage.into_inner();
                let budget = host_impl.budget;
                (storage, budget, events)
            })
            .map_err(|e| {
                (
                    Host(e),
                    Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError)
                        .into(),
                )
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
    ) -> Result<RawVal, HostError> {
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
    pub fn snapshot_auth_manager(&self) -> AuthorizationManager {
        self.0.authorization_manager.borrow().clone()
    }

    /// Replaces authorization manager with the provided new instance.
    ///
    /// Use this in conjunction with `snapshot_auth_manager` to do authorized
    /// operations without breaking the current authorization state (useful for
    /// preserving the auth state while doing the generic test setup).
    #[cfg(any(test, feature = "testutils"))]
    pub fn set_auth_manager(&self, auth_manager: AuthorizationManager) {
        *self.0.authorization_manager.borrow_mut() = auth_manager;
    }

    // Testing interface to create values directly for later use via Env functions.
    // It needs to be a `pub` method because benches are considered a separate crate.
    pub fn inject_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.to_host_val(v).map(Into::into)
    }

    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: Hash,
        contract_executable: ScContractExecutable,
    ) -> Result<(), HostError> {
        let storage_key = self.contract_executable_ledger_key(&contract_id)?;
        if self
            .0
            .storage
            .borrow_mut()
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
        if let ScContractExecutable::WasmRef(wasm_hash) = &contract_executable {
            if !self.contract_code_exists(wasm_hash)? {
                return Err(err!(
                    self,
                    (ScErrorType::Storage, ScErrorCode::MissingValue),
                    "Wasm does not exist",
                    *wasm_hash
                ));
            }
        }
        self.store_contract_executable(contract_executable, contract_id, &storage_key)?;
        Ok(())
    }

    fn maybe_initialize_asset_token(
        &self,
        contract_id: &Hash,
        id_preimage: &ContractIdPreimage,
    ) -> Result<(), HostError> {
        if let ContractIdPreimage::Asset(asset) = id_preimage {
            let mut asset_bytes: Vec<u8> = Default::default();
            self.metered_write_xdr(asset, &mut asset_bytes)?;
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
            self.0
                .authorization_manager
                .borrow_mut()
                .push_create_contract_host_fn_frame(args.metered_clone(self.budget_ref())?);
        }
        // Make sure that even in case of operation failure we still pop the
        // stack frame.
        // This is hacky, but currently this is the only instance where we need
        // to manually manage auth manager frames (we don't need to authorize
        // any other host fns and it doesn't seem useful to create extra frames
        // for them just to make auth work in a single case).
        let res = self.create_contract_with_optional_auth(deployer, args);
        if has_deployer {
            self.0.authorization_manager.borrow_mut().pop_frame();
        }
        res
    }

    fn create_contract_with_optional_auth(
        &self,
        deployer: Option<AddressObject>,
        args: CreateContractArgs,
    ) -> Result<AddressObject, HostError> {
        if let Some(deployer_address) = deployer {
            let sc_addr = self.visit_obj(deployer_address, |addr: &ScAddress| {
                addr.metered_clone(self.budget_ref())
            })?;
            self.0.authorization_manager.borrow_mut().require_auth(
                self,
                deployer_address.get_handle(),
                sc_addr,
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

    pub(crate) fn get_contract_id_from_asset(&self, asset: Asset) -> Result<Hash, HostError> {
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
        let mut contracts = self.0.contracts.borrow_mut();
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
            .0
            .previous_authorization_manager
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| {
                self.err(
                    ScErrorType::Auth,
                    ScErrorCode::InternalError,
                    "previous invocation is missing - no auth data to get",
                    &[],
                )
            })?
            .get_authenticated_authorizations())
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
        }));
        if !self
            .0
            .storage
            .borrow_mut()
            .has(&code_key, self.as_budget())?
        {
            self.with_mut_storage(|storage| {
                let data = LedgerEntryData::ContractCode(ContractCodeEntry {
                    hash: Hash(hash_bytes),
                    code: wasm.try_into().map_err(|_| {
                        self.err(
                            ScErrorType::Value,
                            ScErrorCode::ExceededLimit,
                            "Wasm code is too large",
                            &[],
                        )
                    })?,
                    ext: ExtensionPoint::V0,
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

    pub(crate) fn verify_sig_ed25519_internal(
        &self,
        payload: &[u8],
        public_key: &ed25519_dalek::PublicKey,
        sig: &ed25519_dalek::Signature,
    ) -> Result<(), HostError> {
        use ed25519_dalek::Verifier;
        self.charge_budget(
            ContractCostType::VerifyEd25519Sig,
            Some(payload.len() as u64),
        )?;
        public_key.verify(payload, sig).map_err(|_| {
            self.err(
                ScErrorType::Crypto,
                ScErrorCode::InvalidInput,
                "failed ED25519 verification",
                &[],
            )
        })
    }

    // Returns the recorded per-address authorization payloads that would cover the
    // top-level contract function invocation in the enforcing mode.
    // This should only be called in the recording authorization mode, i.e. only
    // if `switch_to_recording_auth` has been called.
    pub fn get_recorded_auth_payloads(&self) -> Result<Vec<RecordedAuthPayload>, HostError> {
        #[cfg(not(any(test, feature = "testutils")))]
        {
            self.0
                .authorization_manager
                .borrow()
                .get_recorded_auth_payloads()
        }
        #[cfg(any(test, feature = "testutils"))]
        {
            self.0
                .previous_authorization_manager
                .borrow()
                .as_ref()
                .ok_or_else(|| {
                    self.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "previous invocation is missing - no auth data to get",
                        &[],
                    )
                })?
                .get_recorded_auth_payloads()
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
                &[sym.to_raw()],
            ))
        }
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
                *frame.panic.borrow_mut() = Some(e.error);
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
        self.add_host_object(ScString(s.as_bytes().to_vec().try_into()?))
    }

    fn symbol_new_from_slice(&self, s: &str) -> Result<SymbolObject, HostError> {
        for ch in s.chars() {
            SymbolSmall::validate_char(ch)?;
        }
        self.add_host_object(ScSymbol(s.as_bytes().to_vec().try_into()?))
    }

    fn map_new_from_slices(&self, keys: &[&str], vals: &[RawVal]) -> Result<MapObject, HostError> {
        metered_clone::charge_container_bulk_init_with_elts::<Vec<Symbol>, Symbol>(
            keys.len() as u64,
            self.as_budget(),
        )?;
        // If only fallible iterators worked better in Rust, we would not need this Vec<...>.
        let mut key_syms: Vec<Symbol> = Vec::with_capacity(keys.len());
        for k in keys.iter() {
            key_syms.push(Symbol::try_from_val(self, k)?);
        }
        let pair_iter = key_syms
            .iter()
            .map(|s| s.to_raw())
            .zip(vals.iter().cloned());
        let map = HostMap::from_exact_iter(pair_iter, self)?;
        self.add_host_object(map)
    }

    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [RawVal],
    ) -> Result<Void, HostError> {
        // Main costs are already covered by `visit_obj` and `check_symbol_matches`. Here
        // we charge shallow copy of the values.
        metered_clone::charge_shallow_copy::<RawVal>(keys.len() as u64, self.as_budget())?;
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
        Ok(RawVal::VOID)
    }

    fn vec_new_from_slice(&self, vals: &[RawVal]) -> Result<VecObject, Self::Error> {
        let map = HostVec::from_exact_iter(vals.iter().cloned(), self.budget_ref())?;
        self.add_host_object(map)
    }

    fn vec_unpack_to_slice(
        &self,
        vec: VecObject,
        vals: &mut [RawVal],
    ) -> Result<Void, Self::Error> {
        self.visit_obj(vec, |hv: &HostVec| {
            if hv.len() != vals.len() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host vector and output vector lengths when unpacking vec to slice",
                    &[],
                ));
            }
            metered_clone::charge_shallow_copy::<RawVal>(hv.len() as u64, self.as_budget())?;
            vals.copy_from_slice(hv.as_slice());
            Ok(())
        })?;
        Ok(RawVal::VOID)
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
                &[sym.to_raw()],
            )),
            Some(idx) => Ok(U32Val::from(self.usize_to_u32(idx)?)),
        }
    }

    fn log_from_slice(&self, msg: &str, vals: &[RawVal]) -> Result<Void, HostError> {
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
        if self.is_debug() {
            self.as_budget().with_free_budget(|| {
                let VmSlice { vm, pos, len } = self.decode_vmslice(vmcaller, msg_pos, msg_len)?;
                let mut msg: Vec<u8> = vec![0u8; len as usize];
                self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, &mut msg)?;
                let msg = String::from_utf8_lossy(&msg);

                let VmSlice { vm, pos, len } = self.decode_vmslice(vmcaller, vals_pos, vals_len)?;
                let mut vals: Vec<RawVal> = vec![RawVal::VOID.to_raw(); len as usize];
                self.metered_vm_read_vals_from_linear_memory::<8, RawVal>(
                    vmcaller,
                    &vm,
                    pos,
                    vals.as_mut_slice(),
                    |buf| RawVal::from_payload(u64::from_le_bytes(*buf)),
                )?;

                self.log_diagnostics(&msg, &vals)
            })?;
        }
        Ok(RawVal::VOID)
    }

    // Notes on metering: covered by the components
    fn get_invoking_contract(&self, _vmcaller: &mut VmCaller<Host>) -> Result<Object, HostError> {
        let invoking_contract_hash = self.get_invoking_contract_internal()?;
        Ok(self
            .add_host_object(self.scbytes_from_hash(&invoking_contract_hash)?)?
            .into())
    }

    // Metered: covered by `visit` and `metered_cmp`.
    fn obj_cmp(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        a: RawVal,
        b: RawVal,
    ) -> Result<i64, HostError> {
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
        data: RawVal,
    ) -> Result<Void, HostError> {
        self.record_contract_event(ContractEventType::Contract, topics, data)?;
        Ok(RawVal::VOID)
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

    // Notes on metering: covered by `add_host_object`.
    fn obj_from_u64(&self, _vmcaller: &mut VmCaller<Host>, u: u64) -> Result<U64Object, HostError> {
        self.add_host_object(u)
    }

    // Notes on metering: covered by `visit_obj`.
    fn obj_to_u64(&self, _vmcaller: &mut VmCaller<Host>, obj: U64Object) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &u64| Ok(*u))
    }

    // Notes on metering: covered by `add_host_object`.
    fn obj_from_i64(&self, _vmcaller: &mut VmCaller<Host>, i: i64) -> Result<I64Object, HostError> {
        self.add_host_object(i)
    }

    // Notes on metering: covered by `visit_obj`.
    fn obj_to_i64(&self, _vmcaller: &mut VmCaller<Host>, obj: I64Object) -> Result<i64, HostError> {
        self.visit_obj(obj, |i: &i64| Ok(*i))
    }

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

    fn map_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<MapObject, HostError> {
        self.add_host_object(HostMap::new()?)
    }

    fn map_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
        v: RawVal,
    ) -> Result<MapObject, HostError> {
        let mnew = self.visit_obj(m, |hm: &HostMap| hm.insert(k, v, self))?;
        self.add_host_object(mnew)
    }

    fn map_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, move |hm: &HostMap| {
            hm.get(&k, self)?.copied().ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::MissingValue,
                    "map key not found",
                    &[m.to_raw(), k],
                )
            })
        })
    }

    fn map_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
    ) -> Result<MapObject, HostError> {
        match self.visit_obj(m, |hm: &HostMap| hm.remove(&k, self))? {
            Some((mnew, _)) => Ok(self.add_host_object(mnew)?),
            None => Err(self.err(
                ScErrorType::Object,
                ScErrorCode::MissingValue,
                "map key not found",
                &[m.to_raw(), k],
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
        k: RawVal,
    ) -> Result<Bool, HostError> {
        self.visit_obj(m, move |hm: &HostMap| Ok(hm.contains_key(&k, self)?.into()))
    }

    fn map_prev_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_prev(&k, self)? {
                Ok(*pk)
            } else {
                // We return Ok(error) here to indicate "the end of iteration".
                Ok(
                    Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                        .to_raw(),
                )
            }
        })
    }

    fn map_next_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            if let Some((pk, _)) = hm.get_next(&k, self)? {
                Ok(*pk)
            } else {
                // We return Ok(error) here to indicate "the end of iteration".
                Ok(
                    Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds)
                        .to_raw(),
                )
            }
        })
    }

    fn map_min_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_min(self)? {
            Some((pk, pv)) => Ok(*pk),
            None => Ok(
                Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds).to_raw(),
            ),
        })
    }

    fn map_max_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| match hm.get_max(self)? {
            Some((pk, pv)) => Ok(*pk),
            None => Ok(
                Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds).to_raw(),
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
        } = self.decode_vmslice(vmcaller, keys_pos, len)?;
        // covers `Vec::with_capacity` and `len` pushes
        metered_clone::charge_container_bulk_init_with_elts::<Vec<Symbol>, Symbol>(
            len as u64,
            self.as_budget(),
        )?;
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

        // Step 2: extract all val RawVals.
        let vals_pos: u32 = vals_pos.into();
        metered_clone::charge_container_bulk_init_with_elts::<Vec<Symbol>, Symbol>(
            len as u64,
            self.as_budget(),
        )?;
        let mut vals: Vec<RawVal> = vec![RawVal::VOID.into(); len as usize];
        self.metered_vm_read_vals_from_linear_memory::<8, RawVal>(
            vmcaller,
            &vm,
            vals_pos,
            vals.as_mut_slice(),
            |buf| RawVal::from_payload(u64::from_le_bytes(*buf)),
        )?;

        // Step 3: turn pairs into a map.
        let pair_iter = key_syms
            .iter()
            .map(|s| s.to_raw())
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
        } = self.decode_vmslice(vmcaller, keys_pos, len)?;
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
                |pair| u64::to_le_bytes(pair.1.get_payload()),
            )?;
            Ok(())
        })?;

        Ok(RawVal::VOID)
    }

    fn vec_new(&self, _vmcaller: &mut VmCaller<Host>, c: RawVal) -> Result<VecObject, HostError> {
        let capacity: usize = if c.is_void() {
            0
        } else {
            self.usize_from_rawval_u32_input("c", c)?
        };
        // TODO: optimize the vector based on capacity
        self.add_host_object(HostVec::new()?)
    }

    fn vec_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
        x: RawVal,
    ) -> Result<VecObject, HostError> {
        let i: u32 = i.into();
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
    ) -> Result<RawVal, HostError> {
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
        x: RawVal,
    ) -> Result<VecObject, HostError> {
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
        x: RawVal,
    ) -> Result<VecObject, HostError> {
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

    fn vec_front(&self, _vmcaller: &mut VmCaller<Host>, v: VecObject) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.front(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_back(&self, _vmcaller: &mut VmCaller<Host>, v: VecObject) -> Result<RawVal, HostError> {
        self.visit_obj(v, |hv: &HostVec| {
            hv.back(self.as_budget()).map(|hval| *hval)
        })
    }

    fn vec_insert(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        i: U32Val,
        x: RawVal,
    ) -> Result<VecObject, HostError> {
        let i: u32 = i.into();
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
        x: RawVal,
    ) -> Result<RawVal, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.first_index_of(|other| self.compare(&x, other), self.as_budget())? {
                    Some(u) => self.usize_to_u32val(u)?.into(),
                    None => RawVal::VOID.into(),
                },
            )
        })
    }

    fn vec_last_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: RawVal,
    ) -> Result<RawVal, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.last_index_of(|other| self.compare(&x, other), self.as_budget())? {
                    Some(u) => self.usize_to_u32val(u)?.into(),
                    None => RawVal::VOID.into(),
                },
            )
        })
    }

    fn vec_binary_search(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: RawVal,
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
        let VmSlice { vm, pos, len } = self.decode_vmslice(vmcaller, vals_pos, len)?;
        metered_clone::charge_container_bulk_init_with_elts::<Vec<Symbol>, Symbol>(
            len as u64,
            self.as_budget(),
        )?;
        let mut vals: Vec<RawVal> = vec![RawVal::VOID.to_raw(); len as usize];
        self.metered_vm_read_vals_from_linear_memory::<8, RawVal>(
            vmcaller,
            &vm,
            pos,
            vals.as_mut_slice(),
            |buf| RawVal::from_payload(u64::from_le_bytes(*buf)),
        )?;
        self.add_host_object(HostVec::from_vec(vals)?)
    }

    fn vec_unpack_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vec: VecObject,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        let VmSlice { vm, pos, len } = self.decode_vmslice(vmcaller, vals_pos, len)?;
        self.visit_obj(vec, |vecobj: &HostVec| {
            self.metered_vm_write_vals_to_linear_memory(
                vmcaller,
                &vm,
                vals_pos.into(),
                vecobj.as_slice(),
                |x| u64::to_le_bytes(x.get_payload()),
            )
        })?;
        Ok(RawVal::VOID)
    }

    // Notes on metering: covered by components
    fn put_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
        v: RawVal,
    ) -> Result<Void, HostError> {
        let key = self.contract_data_key_from_rawval(k)?;
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: self.get_current_contract_id_internal()?,
            key: self.from_host_val(k)?,
            val: self.from_host_val(v)?,
        });
        self.0.storage.borrow_mut().put(
            &key,
            &Host::ledger_entry_from_data(data),
            self.as_budget(),
        )?;
        Ok(RawVal::VOID)
    }

    // Notes on metering: covered by components
    fn has_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<Bool, HostError> {
        let key = self.storage_key_from_rawval(k)?;
        let res = self.0.storage.borrow_mut().has(&key, self.as_budget())?;
        Ok(RawVal::from_bool(res))
    }

    // Notes on metering: covered by components
    fn get_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.storage_key_from_rawval(k)?;
        let entry = self.0.storage.borrow_mut().get(&key, self.as_budget())?;
        match &entry.data {
            LedgerEntryData::ContractData(ContractDataEntry {
                contract_id,
                key,
                val,
            }) => Ok(self.to_host_val(val)?),
            _ => Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::UnexpectedType,
                "expected contract data ledger entry",
                &[],
            )),
        }
    }

    // Notes on metering: covered by components
    fn del_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<Void, HostError> {
        let key = self.contract_data_key_from_rawval(k)?;
        self.0.storage.borrow_mut().del(&key, self.as_budget())?;
        Ok(RawVal::VOID)
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
            salt: self.uint256_from_bytesobj_input("contract_id_salt", salt)?,
        });
        let executable =
            ScContractExecutable::WasmRef(self.hash_from_bytesobj_input("wasm_hash", wasm_hash)?);
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
        let executable = ScContractExecutable::Token;
        let args = CreateContractArgs {
            contract_id_preimage,
            executable,
        };
        // Asset contracts don't need any deployer authorization (they're tied
        // to the asset issuers instead).
        self.create_contract_internal(None, args)
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
        if !self.contract_code_exists(&wasm_hash)? {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::MissingValue,
                "WASM does not exist",
                &[hash.to_raw()],
            ));
        }
        let curr_contract_id = self.get_current_contract_id_internal()?;
        let key = self.contract_executable_ledger_key(&curr_contract_id)?;
        let old_executable = self.retrieve_contract_executable_from_storage(&key)?;
        let new_executable = ScContractExecutable::WasmRef(wasm_hash);
        self.emit_update_contract_event(&old_executable, &new_executable)?;
        self.store_contract_executable(new_executable, curr_contract_id, &key)?;
        Ok(RawVal::VOID)
    }

    // Notes on metering: here covers the args unpacking. The actual VM work is changed at lower layers.
    fn call(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        contract_address: AddressObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<RawVal, HostError> {
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
                    &[func.to_raw(), args.to_raw()],
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
    ) -> Result<RawVal, HostError> {
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
                        &[func.to_raw(), args.to_raw()],
                    )
                })?;
                Ok(e.error.to_raw())
            }
        }
    }

    // Notes on metering: covered by components
    fn serialize_to_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: RawVal,
    ) -> Result<BytesObject, HostError> {
        let scv = self.from_host_val(v)?;
        let mut buf = Vec::<u8>::new();
        self.metered_write_xdr(&scv, &mut buf)?;
        self.add_host_object(self.scbytes_from_vec(buf)?)
    }

    // Notes on metering: covered by components
    fn deserialize_from_bytes(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
    ) -> Result<RawVal, HostError> {
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
        Ok(RawVal::VOID)
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
        Ok(RawVal::VOID)
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
        Ok(RawVal::VOID)
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
        let VmSlice { vm, pos, len } = self.decode_vmslice(vmcaller, lm_pos, len)?;
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
                &[sym.to_raw()],
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
                    &[iv.to_raw()],
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
                        &[iv.to_raw()],
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
            metered_clone::charge_heap_alloc::<u8>(range.len() as u64, self.as_budget())?;
            metered_clone::charge_shallow_copy::<u8>(range.len() as u64, self.as_budget())?;
            Ok(hv.as_slice()[range].to_vec())
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
    fn verify_sig_ed25519(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: BytesObject,
        x: BytesObject,
        s: BytesObject,
    ) -> Result<Void, HostError> {
        let public_key = self.ed25519_pub_key_from_bytesobj_input(k)?;
        let sig = self.signature_from_bytesobj_input("sig", s)?;
        let res = self.visit_obj(x, |payload: &ScBytes| {
            self.verify_sig_ed25519_internal(payload.as_slice(), &public_key, &sig)
        });
        Ok(res?.into())
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
        let contexts = self.0.context.borrow();

        let get_host_val_tuple = |id: &Hash, function: &Symbol| -> Result<[RawVal; 2], HostError> {
            let id_val = self.add_host_object(self.scbytes_from_hash(id)?)?.into();
            let function_val = (*function).into();
            Ok([id_val, function_val])
        };

        let mut outer = Vec::with_capacity(contexts.len());
        for context in contexts.iter() {
            let vals = match &context.frame {
                Frame::ContractVM(vm, function, _) => {
                    get_host_val_tuple(&vm.contract_id, &function)?
                }
                Frame::HostFunction(_) => continue,
                Frame::Token(id, function, _) => get_host_val_tuple(id, function)?,
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
                &[U32Val::from(error.get_code()).to_raw()],
            ))
        } else {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::UnexpectedType,
                "contract attempted to fail with non-ContractError status code",
                &[error.to_raw()],
            ))
        }
    }

    fn dummy0(&self, vmcaller: &mut VmCaller<Self::VmUserState>) -> Result<RawVal, Self::Error> {
        Ok(().into())
    }

    fn require_auth_for_args(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
        args: VecObject,
    ) -> Result<RawVal, Self::Error> {
        let sc_addr = self.visit_obj(address, |addr: &ScAddress| {
            addr.metered_clone(self.budget_ref())
        })?;

        Ok(self
            .0
            .authorization_manager
            .borrow_mut()
            .require_auth(
                self,
                address.get_handle(),
                sc_addr,
                self.call_args_to_scvec(args)?,
            )?
            .into())
    }

    fn require_auth(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<RawVal, Self::Error> {
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;
        let args = self.with_current_frame(|f| {
            let args = match f {
                Frame::ContractVM(_, _, args) => args,
                Frame::HostFunction(_) => {
                    return Err(self.err(
                        ScErrorType::Context,
                        ScErrorCode::InvalidAction,
                        "require_auth is not suppported for host fns",
                        &[],
                    ))
                }
                Frame::Token(_, _, args) => args,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(c) => &c.args,
            };
            self.rawvals_to_scvec(&args)
        })?;

        Ok(self
            .0
            .authorization_manager
            .borrow_mut()
            .require_auth(self, address.get_handle(), addr, args)?
            .into())
    }

    fn get_current_contract_id(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
    ) -> Result<BytesObject, Self::Error> {
        let id = self.get_current_contract_id_internal()?;
        self.add_host_object(ScBytes(id.0.to_vec().try_into()?))
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
    ) -> Result<RawVal, Self::Error> {
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;
        match addr {
            ScAddress::Account(AccountId(PublicKey::PublicKeyTypeEd25519(pk))) => Ok(self
                .add_host_object(ScBytes(pk.0.to_vec().try_into()?))?
                .into()),
            ScAddress::Contract(_) => Ok(().into()),
        }
    }

    fn address_to_contract_id(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<RawVal, Self::Error> {
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;
        match addr {
            ScAddress::Account(_) => Ok(().into()),
            ScAddress::Contract(Hash(h)) => Ok(self
                .add_host_object(ScBytes(h.to_vec().try_into()?))?
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
                Ok(RawVal::VOID)
            } else if let Ok(len) = u32::try_from(slice.len()) {
                Err(self.err(
                    ScErrorType::Value,
                    ScErrorCode::UnexpectedSize,
                    "Unexpected size of BytesObject in prng_reseed",
                    &[U32Val::from(len).to_raw()],
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
