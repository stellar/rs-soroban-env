use core::{cell::RefCell, cmp::Ordering, fmt::Debug};
use std::rc::Rc;

use crate::{
    auth::AuthorizationManager,
    budget::{AsBudget, Budget},
    events::{diagnostic::DiagnosticLevel, Events, InternalEventsBuffer},
    host_object::{HostMap, HostObject, HostVec},
    impl_bignum_host_fns, impl_bignum_host_fns_rhs_u32, impl_wrapping_obj_from_num,
    impl_wrapping_obj_to_num,
    num::*,
    storage::Storage,
    vm::ModuleCache,
    xdr::{
        int128_helpers, AccountId, Asset, ContractCostType, ContractEventType, ContractExecutable,
        ContractIdPreimage, ContractIdPreimageFromAddress, CreateContractArgsV2, Duration, Hash,
        LedgerEntryData, PublicKey, ScAddress, ScBytes, ScErrorCode, ScErrorType, ScString,
        ScSymbol, ScVal, TimePoint, Uint256,
    },
    AddressObject, Bool, BytesObject, Compare, ConversionError, EnvBase, Error, LedgerInfo,
    MapObject, Object, StorageType, StringObject, Symbol, SymbolObject, TryFromVal, Val, VecObject,
    VmCaller, VmCallerEnv, Void,
};

mod comparison;
mod conversion;
mod data_helper;
mod declared_size;
pub(crate) mod error;
pub(crate) mod frame;
pub(crate) mod ledger_info_helper;
pub(crate) mod lifecycle;
mod mem_helper;
pub(crate) mod metered_clone;
pub(crate) mod metered_hash;
pub(crate) mod metered_map;
pub(crate) mod metered_vector;
pub(crate) mod metered_xdr;
mod num;
pub(crate) mod prng;
pub(crate) mod trace;
mod validity;

pub use error::HostError;
use frame::CallParams;
pub use prng::{Seed, SEED_BYTES};
pub use trace::{TraceEvent, TraceHook, TraceRecord, TraceState};

use self::{
    frame::{Context, ContractReentryMode},
    mem_helper::MemFnArgs,
    metered_clone::{MeteredClone, MeteredContainer},
    metered_xdr::metered_write_xdr,
    prng::Prng,
};

use crate::host::error::TryBorrowOrErr;
#[cfg(any(test, feature = "testutils"))]
pub use frame::ContractFunctionSet;
pub(crate) use frame::Frame;
#[cfg(any(test, feature = "recording_mode"))]
use rand_chacha::ChaCha20Rng;
use soroban_env_common::SymbolSmall;

#[cfg(any(test, feature = "testutils"))]
#[derive(Clone, Copy)]
pub enum ContractInvocationEvent {
    Start,
    Finish,
}

#[cfg(any(test, feature = "testutils"))]
pub type ContractInvocationHook = Rc<dyn for<'a> Fn(&'a Host, ContractInvocationEvent) -> ()>;

#[cfg(any(test, feature = "testutils"))]
#[derive(Clone, Default)]
pub struct CoverageScoreboard {
    pub vm_to_vm_calls: usize,
}

// The soroban 22.x host only supports protocol 22 and later, having
// adopted a new version of wasmi with a new fuel metering system, it
// cannot accurately replay earlier contracts. Earlier protocols
// must run on Soroban 21.x or earlier.

pub(crate) const MIN_LEDGER_PROTOCOL_VERSION: u32 = 22;

#[derive(Clone, Default)]
struct HostImpl {
    module_cache: RefCell<Option<ModuleCache>>,
    shared_linker: RefCell<Option<wasmi::Linker<Host>>>,
    source_account: RefCell<Option<AccountId>>,
    ledger: RefCell<Option<LedgerInfo>>,
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    context_stack: RefCell<Vec<Context>>,
    // Note: budget is refcounted and is _not_ deep-cloned when you call HostImpl::deep_clone,
    // mainly because it's not really possible to achieve (the same budget is connected to many
    // metered sub-objects) but also because it's plausible that the person calling deep_clone
    // actually wants their clones to be metered by "the same" total budget
    // FIXME: deep_clone is gone, maybe Budget should not be separately refcounted?
    budget: Budget,
    events: RefCell<InternalEventsBuffer>,
    authorization_manager: RefCell<AuthorizationManager>,
    // Note: to reduce the risk of future maintainers accidentally adding a new
    // way of observing the diagnostic level (which may vary between different
    // replicas of the host, thus causing divergence) there are no borrow
    // helpers for it and the only method to use it is inside the
    // `with_debug_mode` callback that switches to the shadow budget.
    diagnostic_level: RefCell<DiagnosticLevel>,
    base_prng: RefCell<Option<Prng>>,
    // Auth-recording mode generates pseudorandom nonces to populate its output.
    // We'd like these to be deterministic from one run to the next, but also
    // completely isolated from any use of the user-accessible PRNGs (either
    // base or local) such that contracts behave exactly the same whether or not
    // they're recording auth. Therefore this task gets its own PRNG, seeded when
    // the base PRNG is seeded (as a derived PRNG).
    #[cfg(any(test, feature = "recording_mode"))]
    recording_auth_nonce_prng: RefCell<Option<ChaCha20Rng>>,
    // Some tests _of the host_ rely on pseudorandom _input_ data. For these cases we attach
    // yet another unmetered PRNG to the host.
    #[cfg(any(test, feature = "testutils"))]
    test_prng: RefCell<Option<ChaCha20Rng>>,
    // Note: we're not going to charge metering for testutils because it's out of the scope
    // of what users will be charged for in production -- it's scaffolding for testing a contract,
    // but shouldn't be charged to the contract itself (and will never be compiled-in to
    // production hosts)
    #[cfg(any(test, feature = "testutils"))]
    contracts: RefCell<std::collections::BTreeMap<Hash, Rc<dyn ContractFunctionSet>>>,
    // Store a copy of the `AuthorizationManager` for the last host function
    // invocation. In order to emulate the production behavior in tests, we reset
    // authorization manager after every invocation (as it's not meant to be
    // shared between invocations).
    // This enables test-only functions that allow checking if the authorization
    // has happened or has been recorded.
    #[cfg(any(test, feature = "testutils"))]
    previous_authorization_manager: RefCell<Option<AuthorizationManager>>,
    // Store a hook that we will call with various lifecycle events during
    // the host's execution. No guarantees are made about the stability of this
    // interface, it exists strictly for internal testing of the host.
    #[doc(hidden)]
    trace_hook: RefCell<Option<TraceHook>>,
    // Store a simple contract invocation hook for public usage.
    // The hook triggers when the top-level contract invocation
    // starts and when it ends.
    #[doc(hidden)]
    #[cfg(any(test, feature = "testutils"))]
    top_contract_invocation_hook: RefCell<Option<ContractInvocationHook>>,

    // A utility to help us measure certain key events we're interested
    // in observing the coverage of. Only written-to, never read, it
    // exists only so that we can observe in aggregated code-coverage
    // measurements whether the lines of code that write to its fields are
    // covered.
    #[doc(hidden)]
    #[cfg(any(test, feature = "testutils"))]
    coverage_scoreboard: RefCell<CoverageScoreboard>,

    #[doc(hidden)]
    #[cfg(any(test, feature = "recording_mode"))]
    suppress_diagnostic_events: RefCell<bool>,

    // This flag marks the call of `build_module_cache` that would happen
    // in enforcing mode. In recording mode we need to use this flag to
    // determine whether we need to rebuild module cache after the host
    // invocation has been done.
    #[doc(hidden)]
    #[cfg(any(test, feature = "recording_mode"))]
    need_to_build_module_cache: RefCell<bool>,
}

// Host is a newtype on Rc<HostImpl> so we can impl Env for it below.
#[derive(Clone)]
pub struct Host(Rc<HostImpl>);

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
            #[allow(dead_code, unused_imports)]
            pub(crate) fn $borrow(&self) -> Result<std::cell::Ref<'_, $t>, HostError> {
                use crate::host::error::TryBorrowOrErr;
                self.0.$field.try_borrow_or_err_with(
                    self,
                    concat!("host.0.", stringify!($field), ".try_borrow failed"),
                )
            }
            #[allow(dead_code, unused_imports)]
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
    module_cache,
    Option<ModuleCache>,
    try_borrow_module_cache,
    try_borrow_module_cache_mut
);
impl_checked_borrow_helpers!(
    shared_linker,
    Option<wasmi::Linker<Host>>,
    try_borrow_linker,
    try_borrow_linker_mut
);
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
    context_stack,
    Vec<Context>,
    try_borrow_context_stack,
    try_borrow_context_stack_mut
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

// Note: diagnostic_mode borrow helpers are _not_ defined here to reduce the
// risk of future maintainers accidentally revealing any way of observing the
// diagnostic level in user code (which may vary between different replicas of
// the host).

impl_checked_borrow_helpers!(
    base_prng,
    Option<Prng>,
    try_borrow_base_prng,
    try_borrow_base_prng_mut
);

#[cfg(any(test, feature = "recording_mode"))]
impl_checked_borrow_helpers!(
    recording_auth_nonce_prng,
    Option<ChaCha20Rng>,
    try_borrow_recording_auth_nonce_prng,
    try_borrow_recording_auth_nonce_prng_mut
);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(
    test_prng,
    Option<ChaCha20Rng>,
    try_borrow_test_prng,
    try_borrow_test_prng_mut
);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(contracts, std::collections::BTreeMap<Hash, Rc<dyn ContractFunctionSet>>, try_borrow_contracts, try_borrow_contracts_mut);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(
    previous_authorization_manager,
    Option<AuthorizationManager>,
    try_borrow_previous_authorization_manager,
    try_borrow_previous_authorization_manager_mut
);

impl_checked_borrow_helpers!(
    trace_hook,
    Option<TraceHook>,
    try_borrow_trace_hook,
    try_borrow_trace_hook_mut
);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(
    top_contract_invocation_hook,
    Option<ContractInvocationHook>,
    try_borrow_top_contract_invocation_hook,
    try_borrow_top_contract_invocation_hook_mut
);

#[cfg(any(test, feature = "testutils"))]
impl_checked_borrow_helpers!(
    coverage_scoreboard,
    CoverageScoreboard,
    try_borrow_coverage_scoreboard,
    try_borrow_coverage_scoreboard_mut
);

#[cfg(any(test, feature = "recording_mode"))]
impl_checked_borrow_helpers!(
    suppress_diagnostic_events,
    bool,
    try_borrow_suppress_diagnostic_events,
    try_borrow_suppress_diagnostic_events_mut
);

#[cfg(any(test, feature = "recording_mode"))]
impl_checked_borrow_helpers!(
    need_to_build_module_cache,
    bool,
    try_borrow_need_to_build_module_cache,
    try_borrow_need_to_build_module_cache_mut
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
            module_cache: RefCell::new(None),
            shared_linker: RefCell::new(None),
            source_account: RefCell::new(None),
            ledger: RefCell::new(None),
            objects: Default::default(),
            storage: RefCell::new(storage),
            context_stack: Default::default(),
            budget,
            events: Default::default(),
            authorization_manager: RefCell::new(
                AuthorizationManager::new_enforcing_without_authorizations(),
            ),
            diagnostic_level: Default::default(),
            base_prng: RefCell::new(None),
            #[cfg(any(test, feature = "recording_mode"))]
            recording_auth_nonce_prng: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            test_prng: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            contracts: Default::default(),
            #[cfg(any(test, feature = "testutils"))]
            previous_authorization_manager: RefCell::new(None),
            trace_hook: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            top_contract_invocation_hook: RefCell::new(None),
            #[cfg(any(test, feature = "testutils"))]
            coverage_scoreboard: Default::default(),
            #[cfg(any(test, feature = "recording_mode"))]
            suppress_diagnostic_events: RefCell::new(false),
            #[cfg(any(test, feature = "recording_mode"))]
            need_to_build_module_cache: RefCell::new(false),
        }))
    }

    pub fn build_module_cache_if_needed(&self) -> Result<(), HostError> {
        if self.try_borrow_module_cache()?.is_none() {
            let cache = ModuleCache::new(self)?;
            let linker = cache.make_linker(self)?;
            *self.try_borrow_module_cache_mut()? = Some(cache);
            *self.try_borrow_linker_mut()? = Some(linker);
        }
        Ok(())
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub fn in_storage_recording_mode(&self) -> Result<bool, HostError> {
        if let crate::storage::FootprintMode::Recording(_) = self.try_borrow_storage()?.mode {
            Ok(true)
        } else {
            Ok(false)
        }
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub fn clear_module_cache(&self) -> Result<(), HostError> {
        *self.try_borrow_module_cache_mut()? = None;
        *self.try_borrow_linker_mut()? = None;
        Ok(())
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub fn rebuild_module_cache(&self) -> Result<(), HostError> {
        self.clear_module_cache()?;
        self.build_module_cache_if_needed()
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

    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn source_account_id(&self) -> Result<Option<AccountId>, HostError> {
        self.try_borrow_source_account()?.metered_clone(self)
    }

    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn with_test_prng<T>(
        &self,
        f: impl FnOnce(&mut ChaCha20Rng) -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        let mut opt = self.try_borrow_test_prng_mut()?;
        if let Some(p) = opt.as_mut() {
            f(p)
        } else {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "missing test PRNG",
                &[],
            ))
        }
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub(crate) fn with_recording_auth_nonce_prng<T>(
        &self,
        f: impl FnOnce(&mut ChaCha20Rng) -> Result<T, HostError>,
    ) -> Result<T, HostError> {
        let mut opt = self.try_borrow_recording_auth_nonce_prng_mut()?;
        if let Some(p) = opt.as_mut() {
            f(p)
        } else {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "missing recording-auth nonce PRNG",
                &[],
            ))
        }
    }

    pub fn source_account_address(&self) -> Result<Option<AddressObject>, HostError> {
        if let Some(acc) = self.try_borrow_source_account()?.as_ref() {
            Ok(Some(self.add_host_object(ScAddress::Account(
                acc.metered_clone(self)?,
            ))?))
        } else {
            Ok(None)
        }
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub fn switch_to_enforcing_storage(&self) -> Result<(), HostError> {
        self.with_mut_storage(|storage| {
            storage.mode = crate::storage::FootprintMode::Enforcing;
            Ok(())
        })
    }

    #[cfg(any(test, feature = "recording_mode"))]
    pub fn switch_to_recording_auth(&self, disable_non_root_auth: bool) -> Result<(), HostError> {
        *self.try_borrow_authorization_manager_mut()? =
            AuthorizationManager::new_recording(disable_non_root_auth);
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

    #[allow(unused_variables)]
    pub fn set_base_prng_seed(&self, seed: prng::Seed) -> Result<(), HostError> {
        let mut base_prng = Prng::new_from_seed(seed, self.budget_ref())?;
        // NB: we must _create_ these PRNGs whether or not we're in a build that
        // stores / reveals them, so that the base_prng is left in the same state
        // regardless of build configuration.
        let recording_auth_nonce_prng = base_prng.unmetered_raw_sub_prng();
        let test_prng = base_prng.unmetered_raw_sub_prng();
        #[cfg(any(test, feature = "testutils"))]
        {
            *self.try_borrow_test_prng_mut()? = Some(test_prng);
        }
        #[cfg(any(test, feature = "recording_mode"))]
        {
            *self.try_borrow_recording_auth_nonce_prng_mut()? = Some(recording_auth_nonce_prng);
        }
        *self.try_borrow_base_prng_mut()? = Some(base_prng);
        Ok(())
    }

    pub fn set_ledger_info(&self, info: LedgerInfo) -> Result<(), HostError> {
        *self.try_borrow_ledger_mut()? = Some(info);
        self.check_ledger_protocol_supported()
    }

    pub(crate) fn check_ledger_protocol_supported(&self) -> Result<(), HostError> {
        use soroban_env_common::meta;
        let proto = self.get_ledger_protocol_version()?;
        // There are some protocol-gating tests that want to register
        // old-protocol contracts and run them in the new host. We allow this in
        // test mode -- technically old contracts should _run_ -- but we don't
        // allow it in production because it risks replaying an old contract
        // with the new VM and thereby (subtly!) replaying its execution costs
        // wrong.
        #[cfg(not(test))]
        if proto < MIN_LEDGER_PROTOCOL_VERSION {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "ledger protocol version too old for host",
                &[proto.into()],
            ));
        }
        if proto > meta::INTERFACE_VERSION.protocol {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "ledger protocol version too new for host",
                &[proto.into()],
            ));
        }
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

    pub fn get_ledger_protocol_version(&self) -> Result<u32, HostError> {
        self.with_ledger_info(|li| Ok(li.protocol_version))
    }

    pub(crate) fn budget_ref(&self) -> &Budget {
        &self.0.budget
    }

    pub fn budget_cloned(&self) -> Budget {
        self.0.budget.clone()
    }

    pub fn charge_budget(&self, ty: ContractCostType, input: Option<u64>) -> Result<(), HostError> {
        self.0.budget.charge(ty, input)
    }

    pub fn set_shadow_budget_limits(&self, cpu: u64, mem: u64) -> Result<(), HostError> {
        self.0.budget.set_shadow_limits(cpu, mem)
    }

    pub fn set_diagnostic_level(&self, diagnostic_level: DiagnosticLevel) -> Result<(), HostError> {
        *self.0.diagnostic_level.try_borrow_mut_or_err()? = diagnostic_level;
        Ok(())
    }

    // As above, avoids having to import DiagnosticLevel.
    pub fn enable_debug(&self) -> Result<(), HostError> {
        self.set_diagnostic_level(DiagnosticLevel::Debug)
    }

    /// Wraps a `budget.with_shadow_mode` call with a check against the
    /// diagnostic level. This wrapper should be used for any work that is part
    /// of the production workflow but in debug mode, i.e. diagnostic related
    /// work (logging, or any operations on diagnostic events).
    ///
    /// Note: to help minimize the risk of divergence based on accidental
    /// observation of the diagnostic level in any context _other_ than this
    /// callback, we make two things at least inconvenient enough to maybe cause
    /// people to come to this comment and read it:
    ///
    ///   1. We avoid having any other direct way of observing the flag.
    ///   2. We eat all errors and return no values from the closure.
    ///
    /// If you need to observe a value from the execution of debug mode, you can
    /// of course still mutate a mutable reference pointing outside the closure,
    /// but be _absolutely certain_ any observation you thereby make of the
    /// debug-level _only_ flows into other functions that are themselves
    /// debug-mode-guarded and/or only write results into debug state (eg.
    /// diagnostic events).
    pub(crate) fn with_debug_mode<F>(&self, f: F)
    where
        F: FnOnce() -> Result<(), HostError>,
    {
        if let Ok(cell) = self.0.diagnostic_level.try_borrow_or_err() {
            if matches!(*cell, DiagnosticLevel::Debug) {
                return self.budget_ref().with_shadow_mode(f);
            }
        }
    }

    /// Calls the provided function while ensuring that no diagnostic events are
    /// recorded.
    /// This is useful for emulating operations only for the sake of budget
    /// accounting in recording mode.
    #[cfg(any(test, feature = "recording_mode"))]
    pub(crate) fn with_suppressed_diagnostic_events<F>(&self, f: F) -> Result<(), HostError>
    where
        F: FnOnce() -> Result<(), HostError>,
    {
        *self.try_borrow_suppress_diagnostic_events_mut()? = true;
        f()?;
        *self.try_borrow_suppress_diagnostic_events_mut()? = false;
        Ok(())
    }

    /// Returns whether the Host can be finished by calling
    /// [`Host::try_finish`].
    ///
    /// Returns true if the host reference is unique, refcount = 1.
    pub fn can_finish(&self) -> bool {
        Rc::strong_count(&self.0) == 1
    }

    /// Accept a _unique_ (refcount = 1) host reference and destroy the
    /// underlying [`HostImpl`], returning its finalized components containing
    /// processing side effects  to the caller as a tuple wrapped in `Ok(...)`.
    ///
    /// Use [`Host::can_finish`] to determine before calling the function if it
    /// will succeed.
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

    fn create_contract_impl(
        &self,
        deployer: AddressObject,
        wasm_hash: BytesObject,
        salt: BytesObject,
        constructor_args: Option<VecObject>,
    ) -> Result<AddressObject, HostError> {
        let contract_id_preimage = ContractIdPreimage::Address(ContractIdPreimageFromAddress {
            address: self.visit_obj(deployer, |addr: &ScAddress| addr.metered_clone(self))?,
            salt: self.u256_from_bytesobj_input("contract_id_salt", salt)?,
        });
        let executable =
            ContractExecutable::Wasm(self.hash_from_bytesobj_input("wasm_hash", wasm_hash)?);
        let (constructor_args, constructor_args_vec) = if let Some(v) = constructor_args {
            (
                self.vecobject_to_scval_vec(v)?.to_vec(),
                self.call_args_from_obj(v)?,
            )
        } else {
            (vec![], vec![])
        };
        let args = CreateContractArgsV2 {
            contract_id_preimage,
            executable,
            constructor_args: constructor_args.try_into().map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InternalError,
                    "couldn't convert constructor args vector to XDR",
                    &[],
                )
            })?,
        };
        self.create_contract_internal(Some(deployer), args, constructor_args_vec)
    }
}

macro_rules! call_trace_env_call {
    ($self:expr, $($arg:expr),*) => {
        if $self.tracing_enabled()
        {
            $self.trace_env_call(function_short_name!(), &[$(&$arg),*])?;
        }
    };
}

macro_rules! call_trace_env_ret {
    ($self:expr, $arg:expr) => {{
        if $self.tracing_enabled() {
            let dyn_res: Result<&dyn core::fmt::Debug, &HostError> = match &$arg {
                Ok(ref ok) => Ok(ok),
                Err(err) => Err(err),
            };
            $self.trace_env_ret(function_short_name!(), &dyn_res)?;
        }
    }};
}

// Notes on metering: these are called from the guest and thus charged on the VM instructions.
impl EnvBase for Host {
    type Error = HostError;

    fn error_from_error_val(&self, e: soroban_env_common::Error) -> Self::Error {
        self.error(e, "promoting Error to HostError", &[])
    }

    fn check_obj_integrity(&self, obj: Object) -> Result<(), HostError> {
        use crate::{xdr, Tag};
        self.visit_obj_untyped(obj, |hobj| match (hobj, obj.to_val().get_tag()) {
            (HostObject::Vec(_), Tag::VecObject)
            | (HostObject::Map(_), Tag::MapObject)
            | (HostObject::U64(_), Tag::U64Object)
            | (HostObject::I64(_), Tag::I64Object)
            | (HostObject::TimePoint(_), Tag::TimepointObject)
            | (HostObject::Duration(_), Tag::DurationObject)
            | (HostObject::U128(_), Tag::U128Object)
            | (HostObject::I128(_), Tag::I128Object)
            | (HostObject::U256(_), Tag::U256Object)
            | (HostObject::I256(_), Tag::I256Object)
            | (HostObject::Bytes(_), Tag::BytesObject)
            | (HostObject::String(_), Tag::StringObject)
            | (HostObject::Symbol(_), Tag::SymbolObject)
            | (HostObject::Address(_), Tag::AddressObject) => Ok(()),
            _ => Err(self.err(
                xdr::ScErrorType::Value,
                xdr::ScErrorCode::InvalidInput,
                "mis-tagged object reference",
                &[],
            )),
        })
    }

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
    // user contract, `Env=Host` and `Env::Error=HostError`, an inhabited type
    // you can observe. So the user might actually have a code path returning
    // from such an error that is suddenly non-dead and receiving an
    // `Env::Error=HostError`, which (to maintain continuity with the VM case)
    // they then _want_ to treat as impossible-to-have-occurred just like
    // `Guest::Error`. They can panic, but that doesn't quite maintain the
    // illusion properly. Instead they should call this method to "reject the
    // error".
    //
    // When such a "rejected error" occurs, we do panic, but only after checking
    // to see if we're in a `TestContract` invocation, and if so storing the
    // error's Error value in that frame, such that `Host::call_n` can recover
    // the Error when it _catches_ the panic and converts it back to an error.
    //
    // It might seem like we ought to `std::panic::panic_any(e)` here, making
    // the panic carry a `HostError` or `Error` and catching it by dynamic type
    // inspection in the `call_n` catch logic. The reason we don't do so is that
    // `panic_any` will not provide a nice printable value to the `PanicInfo`,
    // it constructs, so when/if the panic makes it to a top-level printout it
    // will display a relatively ugly message like "thread panicked at Box<dyn
    // Any>" to stderr, when it is much more useful to the user if we have it
    // print the result of HostError::Debug, with its glorious Error,
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

    fn augment_err_result<T>(&self, mut x: Result<T, Self::Error>) -> Result<T, Self::Error> {
        if let Err(e) = &mut x {
            if e.info.is_none() {
                e.info = self.maybe_get_debug_info()
            }
        }
        x
    }

    fn tracing_enabled(&self) -> bool {
        match self.try_borrow_trace_hook() {
            Ok(hook) => hook.is_some(),
            Err(_) => false,
        }
    }

    fn trace_env_call(&self, fname: &'static str, args: &[&dyn Debug]) -> Result<(), HostError> {
        self.call_any_lifecycle_hook(TraceEvent::EnvCall(fname, args))
    }

    fn trace_env_ret(
        &self,
        fname: &'static str,
        res: &Result<&dyn Debug, &HostError>,
    ) -> Result<(), HostError> {
        self.call_any_lifecycle_hook(TraceEvent::EnvRet(fname, res))
    }

    fn check_same_env(&self, other: &Self) -> Result<(), Self::Error> {
        if Rc::ptr_eq(&self.0, &other.0) {
            Ok(())
        } else {
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "check_same_env on different Hosts",
                &[],
            ))
        }
    }

    fn bytes_copy_from_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &[u8],
    ) -> Result<BytesObject, HostError> {
        call_trace_env_call!(self, b, b_pos, slice.len());
        let res = self.memobj_copy_from_slice::<ScBytes>(b, b_pos, slice);
        call_trace_env_ret!(self, res);
        res
    }

    fn bytes_copy_to_slice(
        &self,
        b: BytesObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        call_trace_env_call!(self, b, b_pos, slice.len());
        let res = self.memobj_copy_to_slice::<ScBytes>(b, b_pos, slice);
        call_trace_env_ret!(self, res);
        res
    }

    fn string_copy_to_slice(
        &self,
        b: StringObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        call_trace_env_call!(self, b, b_pos, slice.len());
        let res = self.memobj_copy_to_slice::<ScString>(b, b_pos, slice);
        call_trace_env_ret!(self, res);
        res
    }

    fn symbol_copy_to_slice(
        &self,
        s: SymbolObject,
        b_pos: U32Val,
        slice: &mut [u8],
    ) -> Result<(), HostError> {
        call_trace_env_call!(self, s, b_pos, slice.len());
        let res = self.memobj_copy_to_slice::<ScSymbol>(s, b_pos, slice);
        call_trace_env_ret!(self, res);
        res
    }

    fn bytes_new_from_slice(&self, mem: &[u8]) -> Result<BytesObject, HostError> {
        call_trace_env_call!(self, mem.len());
        let res = self.add_host_object(self.scbytes_from_slice(mem)?);
        call_trace_env_ret!(self, res);
        res
    }

    fn string_new_from_slice(&self, s: &[u8]) -> Result<StringObject, HostError> {
        call_trace_env_call!(self, s.len());
        let res = self.add_host_object(ScString(self.metered_slice_to_vec(s)?.try_into()?));
        call_trace_env_ret!(self, res);
        res
    }

    fn symbol_new_from_slice(&self, s: &[u8]) -> Result<SymbolObject, HostError> {
        call_trace_env_call!(self, s.len());
        // Note: this whole function could be replaced by `ScSymbol::try_from_bytes`
        // in order to avoid duplication. It is duplicated in order to support
        // a slightly different check order for the sake of observation consistency.
        self.charge_budget(ContractCostType::MemCmp, Some(s.len() as u64))?;
        for b in s {
            SymbolSmall::validate_byte(*b).map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "byte is not allowed in Symbol",
                    &[(*b as u32).into()],
                )
            })?;
        }
        let res = self.add_host_object(ScSymbol(self.metered_slice_to_vec(s)?.try_into()?));
        call_trace_env_ret!(self, res);
        res
    }

    fn map_new_from_slices(&self, keys: &[&str], vals: &[Val]) -> Result<MapObject, HostError> {
        call_trace_env_call!(self, keys.len());
        if keys.len() != vals.len() {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::UnexpectedSize,
                "differing key and value slice lengths when creating map from slices",
                &[],
            ));
        }
        Vec::<(Val, Val)>::charge_bulk_init_cpy(keys.len() as u64, self)?;
        let map_vec = keys
            .iter()
            .zip(vals.iter().copied())
            .map(|(key_str, val)| {
                let sym = Symbol::try_from_val(self, key_str)?;
                self.check_val_integrity(val)?;
                Ok((sym.to_val(), val))
            })
            .collect::<Result<Vec<(Val, Val)>, HostError>>()?;
        let map = HostMap::from_map(map_vec, self)?;
        let res = self.add_host_object(map);
        call_trace_env_ret!(self, res);
        res
    }

    fn map_unpack_to_slice(
        &self,
        map: MapObject,
        keys: &[&str],
        vals: &mut [Val],
    ) -> Result<Void, HostError> {
        call_trace_env_call!(self, map, keys.len());
        if keys.len() != vals.len() {
            return Err(self.err(
                ScErrorType::Object,
                ScErrorCode::UnexpectedSize,
                "differing key and value slice lengths when unpacking map to slice",
                &[],
            ));
        }
        self.visit_obj(map, |hm: &HostMap| {
            if hm.len() != vals.len() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host map and output slice lengths when unpacking map to slice",
                    &[],
                ));
            }

            for (ik, mk) in keys.iter().zip(hm.keys(self)?) {
                let sym: Symbol = mk.try_into()?;
                self.check_symbol_matches(ik.as_bytes(), sym)?;
            }

            metered_clone::charge_shallow_copy::<Val>(keys.len() as u64, self)?;
            for (iv, mv) in vals.iter_mut().zip(hm.values(self)?) {
                *iv = *mv;
            }
            Ok(())
        })?;
        let res = Ok(Val::VOID);
        call_trace_env_ret!(self, res);
        res
    }

    fn vec_new_from_slice(&self, vals: &[Val]) -> Result<VecObject, Self::Error> {
        call_trace_env_call!(self, vals.len());
        let vec = HostVec::from_exact_iter(vals.iter().cloned(), self.budget_ref())?;
        for v in vec.iter() {
            self.check_val_integrity(*v)?;
        }
        let res = self.add_host_object(vec);
        call_trace_env_ret!(self, res);
        res
    }

    fn vec_unpack_to_slice(&self, vec: VecObject, vals: &mut [Val]) -> Result<Void, Self::Error> {
        call_trace_env_call!(self, vec, vals.len());
        self.visit_obj(vec, |hv: &HostVec| {
            if hv.len() != vals.len() {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host vector and output vector lengths when unpacking vec to slice",
                    &[],
                ));
            }
            metered_clone::charge_shallow_copy::<Val>(hv.len() as u64, self)?;
            vals.copy_from_slice(hv.as_slice());
            Ok(())
        })?;
        let res = Ok(Val::VOID);
        call_trace_env_ret!(self, res);
        res
    }

    fn symbol_index_in_strs(&self, sym: Symbol, slices: &[&str]) -> Result<U32Val, Self::Error> {
        call_trace_env_call!(self, sym, slices.len());
        let mut found = None;
        for (i, slice) in slices.iter().enumerate() {
            if self.symbol_matches(slice.as_bytes(), sym)? && found.is_none() {
                found = Some(i)
            }
        }
        let res = match found {
            None => Err(self.err(
                ScErrorType::Value,
                ScErrorCode::InvalidInput,
                "symbol not found in slice of strs",
                &[sym.to_val()],
            )),
            Some(idx) => Ok(U32Val::from(self.usize_to_u32(idx)?)),
        };
        call_trace_env_ret!(self, res);
        res
    }

    fn log_from_slice(&self, msg: &str, vals: &[Val]) -> Result<Void, HostError> {
        call_trace_env_call!(self, msg.len(), vals.len());
        self.log_diagnostics(msg, vals);
        let res = Ok(Void::from(()));
        call_trace_env_ret!(self, res);
        res
    }

    fn check_protocol_version_lower_bound(&self, lower: u32) -> Result<(), Self::Error> {
        self.with_ledger_info(|li| {
            let proto = li.protocol_version;
            if proto < lower {
                Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::IndexBounds,
                    "ledger protocol {} is less than specified lower bound {}",
                    &[Val::from_u32(proto).into(), Val::from_u32(lower).into()],
                ))
            } else {
                Ok(())
            }
        })
    }

    fn check_protocol_version_upper_bound(&self, upper: u32) -> Result<(), Self::Error> {
        self.with_ledger_info(|li| {
            let proto = li.protocol_version;
            if proto > upper {
                Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::IndexBounds,
                    "ledger protocol {} is larger than specified upper bound {}",
                    &[Val::from_u32(proto).into(), Val::from_u32(upper).into()],
                ))
            } else {
                Ok(())
            }
        })
    }
}

impl VmCallerEnv for Host {
    type VmUserState = Host;

    // region: "context" module functions

    // Notes on metering: covered by the components
    fn log_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        msg_pos: U32Val,
        msg_len: U32Val,
        vals_pos: U32Val,
        vals_len: U32Val,
    ) -> Result<Void, HostError> {
        self.with_debug_mode(|| {
            let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(msg_pos, msg_len)?;
            Vec::<u8>::charge_bulk_init_cpy(len as u64, self)?;
            let mut msg: Vec<u8> = vec![0u8; len as usize];
            self.metered_vm_read_bytes_from_linear_memory(vmcaller, &vm, pos, &mut msg)?;
            // `String::from_utf8_lossy` iternally allocates a `String` which is a `Vec<u8>`
            Vec::<u8>::charge_bulk_init_cpy(len as u64, self)?;
            let msg = String::from_utf8_lossy(&msg);

            let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(vals_pos, vals_len)?;
            Vec::<Val>::charge_bulk_init_cpy((len as u64).saturating_add(1), self)?;
            let mut vals: Vec<Val> = vec![Val::VOID.to_val(); len as usize];
            // charge for conversion from bytes to `Val`s
            self.charge_budget(
                ContractCostType::MemCpy,
                Some((len as u64).saturating_mul(8)),
            )?;
            self.metered_vm_read_vals_from_linear_memory::<8, Val>(
                vmcaller,
                &vm,
                pos,
                vals.as_mut_slice(),
                |buf| self.relative_to_absolute(Val::from_payload(u64::from_le_bytes(*buf))),
            )?;
            self.log_diagnostics(&msg, &vals);
            Ok(())
        });

        Ok(Val::VOID)
    }

    // Metered: covered by `visit`.
    fn obj_cmp(&self, _vmcaller: &mut VmCaller<Host>, a: Val, b: Val) -> Result<i64, HostError> {
        let res = match {
            match (Object::try_from(a), Object::try_from(b)) {
                // We were given two objects: compare them.
                (Ok(a), Ok(b)) => self.visit_obj_untyped(a, |ao| {
                    // They might each be None but that's ok, None compares less than Some.
                    self.visit_obj_untyped(b, |bo| Ok(Some(self.compare(&ao, &bo)?)))
                })?,

                // We were given an object and a non-object: try a small-value comparison.
                (Ok(a), Err(_)) => self
                    .visit_obj_untyped(a, |aobj| aobj.try_compare_to_small(self.as_budget(), b))?,
                // Same as previous case, but reversing the resulting order.
                (Err(_), Ok(b)) => self.visit_obj_untyped(b, |bobj| {
                    let ord = bobj.try_compare_to_small(self.as_budget(), a)?;
                    Ok(match ord {
                        Some(Ordering::Less) => Some(Ordering::Greater),
                        Some(Ordering::Greater) => Some(Ordering::Less),
                        other => other,
                    })
                })?,
                // We should have been given at least one object.
                (Err(_), Err(_)) => {
                    return Err(self.err(
                        ScErrorType::Value,
                        ScErrorCode::UnexpectedType,
                        "two non-object args to obj_cmp",
                        &[a, b],
                    ));
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

    fn get_ledger_version(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U32Val, Self::Error> {
        Ok(self.get_ledger_protocol_version()?.into())
    }

    fn get_ledger_sequence(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U32Val, Self::Error> {
        self.with_ledger_info(|li| Ok(li.sequence_number.into()))
    }

    fn get_ledger_timestamp(&self, _vmcaller: &mut VmCaller<Host>) -> Result<U64Val, Self::Error> {
        self.with_ledger_info(|li| Ok(U64Val::try_from_val(self, &li.timestamp)?))
    }

    fn fail_with_error(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
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
                "contract attempted to fail with non-ContractError error code",
                &[error.to_val()],
            ))
        }
    }

    fn get_ledger_network_id(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<BytesObject, Self::Error> {
        self.with_ledger_info(|li| {
            // FIXME: cache this and a few other such IDs: https://github.com/stellar/rs-soroban-env/issues/681
            self.add_host_object(self.scbytes_from_slice(li.network_id.as_slice())?)
        })
    }

    // Notes on metering: covered by the components.
    fn get_current_contract_address(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<AddressObject, HostError> {
        // FIXME: cache this and a few other such IDs: https://github.com/stellar/rs-soroban-env/issues/681
        self.add_host_object(ScAddress::Contract(
            self.get_current_contract_id_internal()?,
        ))
    }

    fn get_max_live_until_ledger(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<U32Val, Self::Error> {
        Ok(self.max_live_until_ledger()?.into())
    }

    // endregion: "context" module functions

    // region: "int" module functions

    impl_wrapping_obj_from_num!(obj_from_u64, u64, U64Object, u64);
    impl_wrapping_obj_to_num!(obj_to_u64, u64, U64Object, u64);
    impl_wrapping_obj_from_num!(obj_from_i64, i64, I64Object, i64);
    impl_wrapping_obj_to_num!(obj_to_i64, i64, I64Object, i64);
    impl_wrapping_obj_from_num!(timepoint_obj_from_u64, TimePoint, TimepointObject, u64);
    impl_wrapping_obj_to_num!(timepoint_obj_to_u64, TimePoint, TimepointObject, u64);
    impl_wrapping_obj_from_num!(duration_obj_from_u64, Duration, DurationObject, u64);
    impl_wrapping_obj_to_num!(duration_obj_to_u64, Duration, DurationObject, u64);

    fn obj_from_u128_pieces(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        hi: u64,
        lo: u64,
    ) -> Result<U128Object, Self::Error> {
        self.add_host_object(int128_helpers::u128_from_pieces(hi, lo))
    }

    fn obj_to_u128_lo64(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, |u: &u128| Ok(int128_helpers::u128_lo(*u)))
    }

    fn obj_to_u128_hi64(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, |u: &u128| Ok(int128_helpers::u128_hi(*u)))
    }

    fn obj_from_i128_pieces(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        hi: i64,
        lo: u64,
    ) -> Result<I128Object, Self::Error> {
        self.add_host_object(int128_helpers::i128_from_pieces(hi, lo))
    }

    fn obj_to_i128_lo64(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, |i: &i128| Ok(int128_helpers::i128_lo(*i)))
    }

    fn obj_to_i128_hi64(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<i64, Self::Error> {
        self.visit_obj(obj, |i: &i128| Ok(int128_helpers::i128_hi(*i)))
    }

    fn obj_from_u256_pieces(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        hi_hi: u64,
        hi_lo: u64,
        lo_hi: u64,
        lo_lo: u64,
    ) -> Result<U256Object, Self::Error> {
        self.add_host_object(u256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
    }

    fn u256_val_from_be_bytes(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        bytes: BytesObject,
    ) -> Result<U256Val, HostError> {
        let num = self.visit_obj(bytes, |b: &ScBytes| {
            Ok(U256::from_be_bytes(self.fixed_length_bytes_from_slice(
                "U256 bytes",
                b.as_slice(),
            )?))
        })?;
        self.map_err(U256Val::try_from_val(self, &num))
    }

    fn u256_val_to_be_bytes(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        val: U256Val,
    ) -> Result<BytesObject, HostError> {
        if let Ok(so) = U256Small::try_from(val) {
            self.add_host_object(self.scbytes_from_slice(&U256::from(so).to_be_bytes())?)
        } else {
            let obj = val.try_into()?;
            let scb = self.visit_obj(obj, |u: &U256| self.scbytes_from_slice(&u.to_be_bytes()))?;
            self.add_host_object(scb)
        }
    }

    fn obj_to_u256_hi_hi(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &U256| {
            let (hi_hi, _, _, _) = u256_into_pieces(*u);
            Ok(hi_hi)
        })
    }

    fn obj_to_u256_hi_lo(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &U256| {
            let (_, hi_lo, _, _) = u256_into_pieces(*u);
            Ok(hi_lo)
        })
    }

    fn obj_to_u256_lo_hi(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &U256| {
            let (_, _, lo_hi, _) = u256_into_pieces(*u);
            Ok(lo_hi)
        })
    }

    fn obj_to_u256_lo_lo(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |u: &U256| {
            let (_, _, _, lo_lo) = u256_into_pieces(*u);
            Ok(lo_lo)
        })
    }

    fn obj_from_i256_pieces(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        hi_hi: i64,
        hi_lo: u64,
        lo_hi: u64,
        lo_lo: u64,
    ) -> Result<I256Object, Self::Error> {
        self.add_host_object(i256_from_pieces(hi_hi, hi_lo, lo_hi, lo_lo))
    }

    fn i256_val_from_be_bytes(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        bytes: BytesObject,
    ) -> Result<I256Val, HostError> {
        let num = self.visit_obj(bytes, |b: &ScBytes| {
            Ok(I256::from_be_bytes(self.fixed_length_bytes_from_slice(
                "I256 bytes",
                b.as_slice(),
            )?))
        })?;
        I256Val::try_from_val(self, &num).map_err(|_| ConversionError.into())
    }

    fn i256_val_to_be_bytes(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        val: I256Val,
    ) -> Result<BytesObject, HostError> {
        if let Ok(so) = I256Small::try_from(val) {
            self.add_host_object(self.scbytes_from_slice(&I256::from(so).to_be_bytes())?)
        } else {
            let obj = val.try_into()?;
            let scb = self.visit_obj(obj, |i: &I256| self.scbytes_from_slice(&i.to_be_bytes()))?;
            self.add_host_object(scb)
        }
    }

    fn obj_to_i256_hi_hi(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<i64, HostError> {
        self.visit_obj(obj, |i: &I256| {
            let (hi_hi, _, _, _) = i256_into_pieces(*i);
            Ok(hi_hi)
        })
    }

    fn obj_to_i256_hi_lo(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |i: &I256| {
            let (_, hi_lo, _, _) = i256_into_pieces(*i);
            Ok(hi_lo)
        })
    }

    fn obj_to_i256_lo_hi(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |i: &I256| {
            let (_, _, lo_hi, _) = i256_into_pieces(*i);
            Ok(lo_hi)
        })
    }

    fn obj_to_i256_lo_lo(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I256Object,
    ) -> Result<u64, HostError> {
        self.visit_obj(obj, |i: &I256| {
            let (_, _, _, lo_lo) = i256_into_pieces(*i);
            Ok(lo_lo)
        })
    }

    impl_bignum_host_fns!(u256_add, checked_add, U256, U256Val, Int256AddSub);
    impl_bignum_host_fns!(u256_sub, checked_sub, U256, U256Val, Int256AddSub);
    impl_bignum_host_fns!(u256_mul, checked_mul, U256, U256Val, Int256Mul);
    impl_bignum_host_fns!(u256_div, checked_div, U256, U256Val, Int256Div);
    impl_bignum_host_fns!(
        u256_rem_euclid,
        checked_rem_euclid,
        U256,
        U256Val,
        Int256Div
    );
    impl_bignum_host_fns_rhs_u32!(u256_pow, checked_pow, U256, U256Val, Int256Pow);
    impl_bignum_host_fns_rhs_u32!(u256_shl, checked_shl, U256, U256Val, Int256Shift);
    impl_bignum_host_fns_rhs_u32!(u256_shr, checked_shr, U256, U256Val, Int256Shift);

    impl_bignum_host_fns!(i256_add, checked_add, I256, I256Val, Int256AddSub);
    impl_bignum_host_fns!(i256_sub, checked_sub, I256, I256Val, Int256AddSub);
    impl_bignum_host_fns!(i256_mul, checked_mul, I256, I256Val, Int256Mul);
    impl_bignum_host_fns!(i256_div, checked_div, I256, I256Val, Int256Div);
    impl_bignum_host_fns!(
        i256_rem_euclid,
        checked_rem_euclid,
        I256,
        I256Val,
        Int256Div
    );
    impl_bignum_host_fns_rhs_u32!(i256_pow, checked_pow, I256, I256Val, Int256Pow);
    impl_bignum_host_fns_rhs_u32!(i256_shl, checked_shl, I256, I256Val, Int256Shift);
    impl_bignum_host_fns_rhs_u32!(i256_shr, checked_shr, I256, I256Val, Int256Shift);

    // endregion: "int" module functions
    // region: "map" module functions

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
        let mnew = self.visit_obj(m, |hm: &HostMap| hm.insert(k, v, self))?;
        self.add_host_object(mnew)
    }

    fn map_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: Val,
    ) -> Result<Val, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            hm.get(&k, self)?.copied().ok_or_else(|| {
                self.err(
                    ScErrorType::Object,
                    ScErrorCode::MissingValue,
                    "map key not found in map_get",
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
                "map key not found in map_del",
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
        self.visit_obj(m, |hm: &HostMap| Ok(hm.contains_key(&k, self)?.into()))
    }

    fn map_key_by_pos(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        i: U32Val,
    ) -> Result<Val, HostError> {
        let i: u32 = i.into();
        self.visit_obj(m, |hm: &HostMap| {
            hm.get_at_index(i as usize, self).map(|r| r.0)
        })
    }

    fn map_val_by_pos(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        i: U32Val,
    ) -> Result<Val, HostError> {
        let i: u32 = i.into();
        self.visit_obj(m, |hm: &HostMap| {
            hm.get_at_index(i as usize, self).map(|r| r.1)
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
        let MemFnArgs {
            vm,
            pos: keys_pos,
            len,
        } = self.get_mem_fn_args(keys_pos, len)?;
        let mut key_syms = Vec::<Symbol>::with_metered_capacity(len as usize, self)?;
        self.metered_vm_scan_slices_in_linear_memory(
            vmcaller,
            &vm,
            keys_pos,
            len as usize,
            |_n, slice| {
                // Optimization note: this does an unnecessary `ScVal` roundtrip.
                // We should just use `Symbol::try_from_val` on the slice instead.
                self.charge_budget(ContractCostType::MemCpy, Some(slice.len() as u64))?;
                let scsym = ScSymbol(slice.try_into()?);
                let sym = Symbol::try_from(self.to_valid_host_val(&ScVal::Symbol(scsym))?)?;
                key_syms.push(sym);
                Ok(())
            },
        )?;

        // Step 2: extract all val Vals.
        let vals_pos: u32 = vals_pos.into();
        Vec::<Val>::charge_bulk_init_cpy(len as u64, self)?;
        let mut vals: Vec<Val> = vec![Val::VOID.into(); len as usize];
        // charge for conversion from bytes to `Val`s
        self.charge_budget(
            ContractCostType::MemCpy,
            Some((len as u64).saturating_mul(8)),
        )?;
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
        let MemFnArgs {
            vm,
            pos: keys_pos,
            len,
        } = self.get_mem_fn_args(keys_pos, len)?;
        self.visit_obj(map, |mapobj: &HostMap| {
            if mapobj.len() != len as usize {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host map and output slice lengths when unpacking map to linear memory",
                    &[],
                ));
            }
            // Step 1: check all key symbols.
            self.metered_vm_scan_slices_in_linear_memory(
                vmcaller,
                &vm,
                keys_pos,
                len as usize,
                |n, slice| {
                    let sym = Symbol::try_from(
                        mapobj.get_at_index(n, self).map_err(|he|
                            if he.error.is_type(ScErrorType::Budget) {
                                he
                            } else {
                                self.err(
                                    ScErrorType::Object,
                                    ScErrorCode::IndexBounds,
                                    "vector out of bounds while unpacking map to linear memory",
                                    &[],
                                )
                            }
                        )?.0
                    )?;
                    self.check_symbol_matches(slice, sym)?;
                    Ok(())
                },
            )?;

            // Step 2: write all vals.
            // charges memcpy of converting map entries into bytes
            self.charge_budget(ContractCostType::MemCpy, Some((len as u64).saturating_mul(8)))?;
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

    // endregion: "map" module functions
    // region: "vec" module functions

    fn vec_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<VecObject, HostError> {
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
        let vnew = self.visit_obj(v, |hv: &HostVec| {
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
        self.visit_obj(v, |hv: &HostVec| {
            self.validate_index_lt_bound(i, hv.len())?;
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
        let vnew = self.visit_obj(v, |hv: &HostVec| {
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
        let vnew = self.visit_obj(v, |hv: &HostVec| hv.push_front(x, self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_pop_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, |hv: &HostVec| hv.pop_front(self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_push_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: Val,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, |hv: &HostVec| hv.push_back(x, self.as_budget()))?;
        self.add_host_object(vnew)
    }

    fn vec_pop_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, |hv: &HostVec| hv.pop_back(self.as_budget()))?;
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
        let vnew = self.visit_obj(v, |hv: &HostVec| {
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
        let vnew = self.visit_obj(v, |hv: &HostVec| {
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
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(vals_pos, len)?;
        Vec::<Val>::charge_bulk_init_cpy(len as u64, self)?;
        let mut vals: Vec<Val> = vec![Val::VOID.to_val(); len as usize];
        // charge for conversion from bytes to `Val`s
        self.charge_budget(
            ContractCostType::MemCpy,
            Some((len as u64).saturating_mul(8)),
        )?;
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
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(vals_pos, len)?;
        self.visit_obj(vec, |vecobj: &HostVec| {
            if vecobj.len() != len as usize {
                return Err(self.err(
                    ScErrorType::Object,
                    ScErrorCode::UnexpectedSize,
                    "differing host vector and output vector lengths when unpacking vec to linear memory",
                    &[],
                ));
            }
            // charges memcpy of converting vec entries into bytes
            self.charge_budget(ContractCostType::MemCpy, Some((len as u64).saturating_mul(8)))?;
            self.metered_vm_write_vals_to_linear_memory(
                vmcaller,
                &vm,
                pos,
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

    // endregion: "vec" module functions
    // region: "ledger" module functions

    // Notes on metering: covered by components
    fn put_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        v: Val,
        t: StorageType,
    ) -> Result<Void, HostError> {
        match t {
            StorageType::Temporary | StorageType::Persistent => {
                self.put_contract_data_into_ledger(k, v, t)?
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
                let key = self.storage_key_from_val(k, t.try_into()?)?;
                self.try_borrow_storage_mut()?
                    .has_with_host(&key, self, Some(k))?
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
                let key = self.storage_key_from_val(k, t.try_into()?)?;
                let entry = self
                    .try_borrow_storage_mut()?
                    .get_with_host(&key, self, Some(k))?;
                match &entry.data {
                    LedgerEntryData::ContractData(e) => Ok(self.to_valid_host_val(&e.val)?),
                    _ => Err(self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
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
                let key = self.storage_key_from_val(k, t.try_into()?)?;
                self.try_borrow_storage_mut()?
                    .del_with_host(&key, self, Some(k))?;
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
    fn extend_contract_data_ttl(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: Val,
        t: StorageType,
        threshold: U32Val,
        extend_to: U32Val,
    ) -> Result<Void, HostError> {
        if matches!(t, StorageType::Instance) {
            return Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidAction,
                "instance storage should be extended via `extend_current_contract_instance_and_code_ttl` function only",
                &[],
            ))?;
        }
        let key = self.storage_key_from_val(k, t.try_into()?)?;
        self.try_borrow_storage_mut()?.extend_ttl(
            self,
            key,
            threshold.into(),
            extend_to.into(),
            Some(k),
        )?;
        Ok(Val::VOID)
    }

    fn extend_current_contract_instance_and_code_ttl(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        threshold: U32Val,
        extend_to: U32Val,
    ) -> Result<Void, HostError> {
        let contract_id = self.get_current_contract_id_internal()?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        self.extend_contract_instance_ttl_from_contract_id(
            key.clone(),
            threshold.into(),
            extend_to.into(),
        )?;
        self.extend_contract_code_ttl_from_contract_id(key, threshold.into(), extend_to.into())?;
        Ok(Val::VOID)
    }

    fn extend_contract_instance_ttl(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        contract: AddressObject,
        threshold: U32Val,
        extend_to: U32Val,
    ) -> Result<Void, Self::Error> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;

        self.extend_contract_instance_ttl_from_contract_id(
            key,
            threshold.into(),
            extend_to.into(),
        )?;

        Ok(Val::VOID)
    }

    fn extend_contract_instance_and_code_ttl(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        contract: AddressObject,
        threshold: U32Val,
        extend_to: U32Val,
    ) -> Result<Void, Self::Error> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        self.extend_contract_instance_ttl_from_contract_id(
            key.clone(),
            threshold.into(),
            extend_to.into(),
        )?;
        self.extend_contract_code_ttl_from_contract_id(key, threshold.into(), extend_to.into())?;
        Ok(Val::VOID)
    }

    fn extend_contract_code_ttl(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        contract: AddressObject,
        threshold: U32Val,
        extend_to: U32Val,
    ) -> Result<Void, Self::Error> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;

        self.extend_contract_code_ttl_from_contract_id(key, threshold.into(), extend_to.into())?;

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
        self.create_contract_impl(deployer, wasm_hash, salt, None)
    }

    fn create_contract_with_constructor(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        deployer: AddressObject,
        wasm_hash: BytesObject,
        salt: BytesObject,
        constructor_args: VecObject,
    ) -> Result<AddressObject, HostError> {
        self.create_contract_impl(deployer, wasm_hash, salt, Some(constructor_args))
    }

    // Notes on metering: covered by the components.
    fn create_asset_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        serialized_asset: BytesObject,
    ) -> Result<AddressObject, HostError> {
        let asset: Asset = self.metered_from_xdr_obj(serialized_asset)?;
        let contract_id_preimage = ContractIdPreimage::Asset(asset);
        let executable = ContractExecutable::StellarAsset;
        let args = CreateContractArgsV2 {
            contract_id_preimage,
            executable,
            constructor_args: Default::default(),
        };
        // Asset contracts don't need any deployer authorization (they're tied
        // to the asset issuers instead).
        self.create_contract_internal(None, args, vec![])
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
        let wasm_vec =
            self.visit_obj(wasm, |bytes: &ScBytes| bytes.as_vec().metered_clone(self))?;
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
        let old_instance = self.retrieve_contract_instance_from_storage(&key)?;
        let new_executable = ContractExecutable::Wasm(wasm_hash);
        self.emit_update_contract_event(&old_instance.executable, &new_executable)?;
        self.store_contract_instance(Some(new_executable), None, curr_contract_id, &key)?;
        Ok(Val::VOID)
    }

    // endregion: "ledger" module functions
    // region: "call" module functions

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
            CallParams::default_external_call(),
        );
        if let Err(e) = &res {
            self.error(
                e.error,
                "contract call failed",
                &[func.to_val(), args.to_val()],
            );
        }
        res
    }

    // Notes on metering: covered by the components.
    fn try_call(
        &self,
        _vmcaller: &mut VmCaller<Host>,
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
            CallParams::default_external_call(),
        );
        match res {
            Ok(rv) => Ok(rv),
            Err(e) => {
                self.error(
                    e.error,
                    "contract try_call failed",
                    &[func.to_val(), args.to_val()],
                );
                // Only allow to gracefully handle the recoverable errors.
                // Non-recoverable errors should still cause guest to panic and
                // abort execution.
                if e.is_recoverable() {
                    // Pass contract error _codes_ through, while switching
                    // from Err(ce) to Ok(ce), i.e. recovering.
                    if e.error.is_type(ScErrorType::Contract) {
                        Ok(e.error.to_val())
                    } else {
                        // Narrow all the remaining host errors down to a single
                        // error type. We don't want to expose the granular host
                        // errors to the guest, consistently with how every
                        // other host function works. This reduces the risk of
                        // implementation being 'locked' into specific error
                        // codes due to them being exposed to the guest and
                        // hashed into blockchain.
                        // The granular error codes are still observable with
                        // diagnostic events.
                        Ok(Error::from_type_and_code(
                            ScErrorType::Context,
                            ScErrorCode::InvalidAction,
                        )
                        .to_val())
                    }
                } else {
                    Err(e)
                }
            }
        }
    }

    // endregion: "call" module functions
    // region: "buf" module functions

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
        // Metering bug: the representation check is not metered,
        // so if the value is not valid, we won't charge anything for
        // walking the `ScVal`. Since `to_host_val` performs validation
        // and has proper metering, next protocol version should just
        // call `to_host_val` directly.
        if Val::can_represent_scval_recursive(&scv) {
            self.to_host_val(&scv)
        } else {
            Err(self.err(
                ScErrorType::Value,
                ScErrorCode::UnexpectedType,
                "Deserialized ScVal type cannot be represented as Val",
                &[(scv.discriminant() as i32).into()],
            ))
        }
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

    // Metering: covered by `metered_vm_scan_slices_in_linear_memory` and `symbol_matches`.
    fn symbol_index_in_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        sym: Symbol,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<U32Val, HostError> {
        let MemFnArgs { vm, pos, len } = self.get_mem_fn_args(lm_pos, len)?;
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
                ScErrorCode::MissingValue,
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            let mut vnew: Vec<u8> = hv.metered_clone(self)?.into();
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
                .map(|u| U32Val::from(u32::from(*u)))
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            self.validate_index_lt_bound(i, hv.len())?;
            let mut vnew: Vec<u8> = hv.metered_clone(self)?.into();
            // len > i has been verified above but use checked_sub just in case
            let n_elts = (hv.len() as u64).checked_sub(i as u64).ok_or_else(|| {
                Error::from_type_and_code(ScErrorType::Context, ScErrorCode::InternalError)
            })?;
            // remove elements incurs the cost of moving bytes, it does not incur
            // allocation/deallocation
            metered_clone::charge_shallow_copy::<u8>(n_elts, self)?;
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            // we allocate the new vector to be able to hold `len + 1` bytes, so that the push
            // will not trigger a reallocation, causing data to be cloned twice.
            let len = self.validate_usize_sum_fits_in_u32(hv.len(), 1)?;
            let mut vnew = Vec::<u8>::with_metered_capacity(len, self)?;
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            let mut vnew: Vec<u8> = hv.metered_clone(self)?.into();
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
                .map(|u| U32Val::from(u32::from(*u)))
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
                .map(|u| U32Val::from(u32::from(*u)))
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            self.validate_index_le_bound(i, hv.len())?;
            // we allocate the new vector to be able to hold `len + 1` bytes, so that the insert
            // will not trigger a reallocation, causing data to be cloned twice.
            let len = self.validate_usize_sum_fits_in_u32(hv.len(), 1)?;
            let mut vnew = Vec::<u8>::with_metered_capacity(len, self)?;
            vnew.extend_from_slice(hv.as_slice());
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
                // we allocate large enough memory to hold the new combined vector, so that
                // allocation only happens once, and charge for it upfront.
                let len = self.validate_usize_sum_fits_in_u32(sb1.len(), sb2.len())?;
                let mut vnew = Vec::<u8>::with_metered_capacity(len, self)?;
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
        let vnew = self.visit_obj(b, |hv: &ScBytes| {
            let range = self.valid_range_from_start_end_bound(start, end, hv.len())?;
            self.metered_slice_to_vec(
                &hv.as_slice()
                    .get(range)
                    .ok_or_else(|| self.err_oob_object_index(None))?,
            )
        })?;
        self.add_host_object(self.scbytes_from_vec(vnew)?)
    }

    // endregion: "buf" module functions
    // region: "crypto" module functions

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
        let verifying_key = self.ed25519_pub_key_from_bytesobj_input(k)?;
        let sig = self.ed25519_signature_from_bytesobj_input("sig", s)?;
        let res = self.visit_obj(x, |payload: &ScBytes| {
            self.verify_sig_ed25519_internal(payload.as_slice(), &verifying_key, &sig)
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
        let sig = self.ecdsa_signature_from_bytesobj_input::<k256::Secp256k1>(signature)?;
        let rid = self.secp256k1_recovery_id_from_u32val(recovery_id)?;
        let hash = self.hash_from_bytesobj_input("msg_digest", msg_digest)?;
        let rk = self.recover_key_ecdsa_secp256k1_internal(&hash, &sig, rid)?;
        self.add_host_object(rk)
    }

    fn verify_sig_ecdsa_secp256r1(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        public_key: BytesObject,
        msg_digest: BytesObject,
        signature: BytesObject,
    ) -> Result<Void, HostError> {
        let pk = self.secp256r1_public_key_from_bytesobj_input(public_key)?;
        let sig = self.ecdsa_signature_from_bytesobj_input::<p256::NistP256>(signature)?;
        let msg_hash = self.hash_from_bytesobj_input("msg_digest", msg_digest)?;
        let res = self.secp256r1_verify_signature(&pk, &msg_hash, &sig)?;
        Ok(res.into())
    }

    fn bls_g1_add(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        p0: BytesObject,
        p1: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let p0_bytes = self.visit_obj(p0, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G1 point",
                    &[],
                )
            })
        })?;
        let p1_bytes = self.visit_obj(p1, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G1 point",
                    &[],
                )
            })
        })?;
        let result = self.bls_g1_add_raw_internal(&p0_bytes, &p1_bytes)?;
        self.add_host_object(self.scbytes_from_vec(result.to_vec())?)
    }

    fn bls_g1_mul(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        scalar: BytesObject,
        p1: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let scalar_bytes = self.visit_obj(scalar, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "scalar value out of range for G1 multiplication",
                    &[],
                )
            })
        })?;
        let p1_bytes = self.visit_obj(p1, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G1 point",
                    &[],
                )
            })
        })?;
        let result = self.bls_g1_mul_raw_internal(scalar_bytes, &p1_bytes)?;
        self.add_host_object(self.scbytes_from_vec(result.to_vec())?)
    }

    fn bls_g2_add(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        p0: BytesObject,
        p1: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let p0_bytes = self.visit_obj(p0, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G2 point",
                    &[],
                )
            })
        })?;
        let p1_bytes = self.visit_obj(p1, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G2 point",
                    &[],
                )
            })
        })?;
        let result = self.bls_g2_add_raw_internal(&p0_bytes, &p1_bytes)?;
        self.add_host_object(self.scbytes_from_vec(result.to_vec())?)
    }

    fn bls_g2_mul(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        scalar: BytesObject,
        p1: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let scalar_bytes = self.visit_obj(scalar, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "scalar value out of range for G2 multiplication",
                    &[],
                )
            })
        })?;
        let p1_bytes = self.visit_obj(p1, |bytes: &ScBytes| {
            bytes.as_slice().try_into().map_err(|_| {
                self.err(
                    ScErrorType::Crypto,
                    ScErrorCode::InvalidInput,
                    "invalid length for G2 point",
                    &[],
                )
            })
        })?;
        let result = self.bls_g2_mul_raw_internal(scalar_bytes, &p1_bytes)?;
        self.add_host_object(self.scbytes_from_vec(result.to_vec())?)
    }

    // endregion: "crypto" module functions
    // region: "test" module functions

    fn dummy0(&self, _vmcaller: &mut VmCaller<Self::VmUserState>) -> Result<Val, Self::Error> {
        Ok(().into())
    }

    fn protocol_gated_dummy(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
    ) -> Result<Val, Self::Error> {
        Ok(().into())
    }

    // endregion: "test" module functions
    // region: "address" module functions

    fn require_auth_for_args(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
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
        _vmcaller: &mut VmCaller<Self::VmUserState>,
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
                    ));
                }
                Frame::StellarAssetContract(_, _, args, _) => args,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(c) => &c.args,
            };
            args.metered_clone(self)
        })?;

        Ok(self
            .try_borrow_authorization_manager()?
            .require_auth(self, address, args)?
            .into())
    }

    fn authorize_as_curr_contract(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        auth_entries: VecObject,
    ) -> Result<Void, HostError> {
        Ok(self
            .try_borrow_authorization_manager()?
            .add_invoker_contract_auth_with_curr_contract_as_invoker(self, auth_entries)?
            .into())
    }

    fn address_to_strkey(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        address: AddressObject,
    ) -> Result<StringObject, Self::Error> {
        let strkey = self.visit_obj(address, |addr: &ScAddress| {
            // Approximate the strkey encoding cost with two vector allocations:
            // one for the payload size (32-byte key/hash + 3 bytes for
            // version/checksum) and  another one for the base32 encoding of
            // the payload.
            const PAYLOAD_LEN: u64 = 32 + 3;
            Vec::<u8>::charge_bulk_init_cpy(PAYLOAD_LEN + (PAYLOAD_LEN * 8 + 4) / 5, self)?;
            let strkey = match addr {
                ScAddress::Account(acc_id) => {
                    let AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(ed25519))) = acc_id;
                    let strkey = stellar_strkey::Strkey::PublicKeyEd25519(
                        stellar_strkey::ed25519::PublicKey(ed25519.metered_clone(self)?),
                    );
                    strkey
                }
                ScAddress::Contract(Hash(h)) => stellar_strkey::Strkey::Contract(
                    stellar_strkey::Contract(h.metered_clone(self)?),
                ),
            };
            Ok(strkey.to_string())
        })?;
        self.add_host_object(ScString(strkey.try_into()?))
    }

    fn strkey_to_address(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        strkey_obj: Val,
    ) -> Result<AddressObject, Self::Error> {
        let strkey_obj = Object::try_from(strkey_obj).map_err(|_| {
            self.err(
                ScErrorType::Value,
                ScErrorCode::UnexpectedType,
                "strkey is not an object",
                &[strkey_obj],
            )
        })?;
        let sc_addr = self.visit_obj_untyped(strkey_obj, |key_obj: &HostObject| {
            let key = match key_obj {
                HostObject::Bytes(b) => b.as_slice(),
                HostObject::String(s) => s.as_slice(),
                _ => {
                    return Err(self.err(
                        ScErrorType::Value,
                        ScErrorCode::UnexpectedType,
                        "strkey is not a string or bytes object",
                        &[strkey_obj.to_val()],
                    ));
                }
            };
            const PAYLOAD_LEN: u64 = 32 + 3;
            let expected_key_len = (PAYLOAD_LEN * 8 + 4) / 5;
            if expected_key_len != key.len() as u64 {
                return Err(self.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "unexpected strkey length",
                    &[strkey_obj.to_val()],
                ));
            }
            // Charge for the key copy to string.
            Vec::<u8>::charge_bulk_init_cpy(key.len() as u64, self)?;
            let key_str = String::from_utf8_lossy(key);
            // Approximate the decoding cost as two vector allocations for the
            // expected payload length (the strkey library does one extra copy).
            Vec::<u8>::charge_bulk_init_cpy(PAYLOAD_LEN + PAYLOAD_LEN, self)?;
            let strkey = stellar_strkey::Strkey::from_string(&key_str).map_err(|_| {
                self.err(
                    ScErrorType::Value,
                    ScErrorCode::InvalidInput,
                    "couldn't process the string as strkey",
                    &[strkey_obj.to_val()],
                )
            })?;
            match strkey {
                stellar_strkey::Strkey::PublicKeyEd25519(pk) => Ok(ScAddress::Account(AccountId(
                    PublicKey::PublicKeyTypeEd25519(Uint256(pk.0)),
                ))),

                stellar_strkey::Strkey::Contract(c) => Ok(ScAddress::Contract(Hash(c.0))),
                _ => {
                    return Err(self.err(
                        ScErrorType::Value,
                        ScErrorCode::InvalidInput,
                        "incorrect strkey type",
                        &[strkey_obj.to_val()],
                    ));
                }
            }
        })?;
        self.add_host_object(sc_addr)
    }

    // endregion: "address" module functions
    // region: "prng" module functions

    fn prng_reseed(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        seed: BytesObject,
    ) -> Result<Void, Self::Error> {
        self.visit_obj(seed, |bytes: &ScBytes| {
            let slice: &[u8] = bytes.as_ref();
            self.charge_budget(ContractCostType::MemCpy, Some(prng::SEED_BYTES))?;
            if let Ok(seed32) = slice.try_into() {
                self.with_current_prng(|prng| {
                    *prng = Prng::new_from_seed(seed32, self.budget_ref())?;
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
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        length: U32Val,
    ) -> Result<BytesObject, Self::Error> {
        self.add_host_object(
            self.with_current_prng(|prng| prng.bytes_new(length.into(), self.as_budget()))?,
        )
    }

    fn prng_u64_in_inclusive_range(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        lo: u64,
        hi: u64,
    ) -> Result<u64, Self::Error> {
        self.with_current_prng(|prng| prng.u64_in_inclusive_range(lo..=hi, self.as_budget()))
    }

    fn prng_vec_shuffle(
        &self,
        _vmcaller: &mut VmCaller<Self::VmUserState>,
        vec: VecObject,
    ) -> Result<VecObject, Self::Error> {
        let vnew = self.visit_obj(vec, |v: &HostVec| {
            self.with_current_prng(|prng| prng.vec_shuffle(v, self.as_budget()))
        })?;
        self.add_host_object(vnew)
    }
    // endregion: "prng" module functions
}

#[cfg(feature = "bench")]
impl Host {
    // Testing interface to create values directly for later use via Env functions.
    // It needs to be a `pub` method because benches are considered a separate crate.
    pub fn inject_val(&self, v: &ScVal) -> Result<Val, HostError> {
        self.to_host_val(v).map(Into::into)
    }
}

#[cfg(any(test, feature = "testutils"))]
impl Host {
    /// Sets a hook to track top-level contract invocations.
    /// The hook triggers right before the top-level contract invocation
    /// starts and right after it ends.
    /// 'Top-level contract invocation' happens when the host creates
    /// the first context frame that belongs to a contract, which includes
    /// both direct host function calls (`call`/`try_call`), and test
    /// utilities such as `with_test_contract_frame` or
    /// `call_account_contract_check_auth`.
    pub fn set_top_contract_invocation_hook(
        &self,
        hook: Option<ContractInvocationHook>,
    ) -> Result<(), HostError> {
        *self.try_borrow_top_contract_invocation_hook_mut()? = hook;
        Ok(())
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    #[allow(dead_code)]
    pub fn with_budget<T, F>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(Budget) -> Result<T, HostError>,
    {
        f(self.0.budget.clone())
    }

    /// Returns the ledger number until a contract with given address lives
    /// (inclusive).
    pub fn get_contract_instance_live_until_ledger(
        &self,
        contract: AddressObject,
    ) -> Result<u32, HostError> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        let (_, live_until) = self
            .try_borrow_storage_mut()?
            .get_with_live_until_ledger(&key, self, None)?;
        live_until.ok_or_else(|| {
            self.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected contract instance without TTL",
                &[contract.into()],
            )
        })
    }

    /// Returns the ledger number until contract code entry for contract with
    /// given address lives (inclusive).
    pub fn get_contract_code_live_until_ledger(
        &self,
        contract: AddressObject,
    ) -> Result<u32, HostError> {
        let contract_id = self.contract_id_from_address(contract)?;
        let key = self.contract_instance_ledger_key(&contract_id)?;
        match self
            .retrieve_contract_instance_from_storage(&key)?
            .executable
        {
            ContractExecutable::Wasm(wasm_hash) => {
                let key = self.contract_code_ledger_key(&wasm_hash)?;
                let (_, live_until) = self
                    .try_borrow_storage_mut()?
                    .get_with_live_until_ledger(&key, self, None)?;
                live_until.ok_or_else(|| {
                    self.err(
                        ScErrorType::Storage,
                        ScErrorCode::InternalError,
                        "unexpected contract code without TTL for a contract",
                        &[contract.into()],
                    )
                })
            }
            ContractExecutable::StellarAsset => Err(self.err(
                ScErrorType::Storage,
                ScErrorCode::InvalidInput,
                "Stellar Asset Contracts don't have contract code",
                &[],
            )),
        }
    }

    /// Returns the ledger number until a current contract's data entry
    /// with given key and storage type lives (inclusive).
    /// Instance storage type is not supported by this function, use
    /// `get_contract_instance_live_until_ledger` instead.
    pub fn get_contract_data_live_until_ledger(
        &self,
        key: Val,
        storage_type: StorageType,
    ) -> Result<u32, HostError> {
        let ledger_key = match storage_type {
            StorageType::Temporary | StorageType::Persistent => {
                self.storage_key_from_val(key, storage_type.try_into()?)?
            }
            StorageType::Instance => {
                return Err(self.err(
                    ScErrorType::Storage,
                    ScErrorCode::InvalidAction,
                    "`get_contract_data_live_until_ledger` doesn't support instance storage, use `get_contract_instance_live_until_ledger` instead.",
                    &[],
                ));
            }
        };
        let (_, live_until) = self.try_borrow_storage_mut()?.get_with_live_until_ledger(
            &ledger_key,
            self,
            Some(key),
        )?;
        live_until.ok_or_else(|| {
            self.err(
                ScErrorType::Storage,
                ScErrorCode::InternalError,
                "unexpected contract data without TTL",
                &[key],
            )
        })
    }
}

impl Host {
    #[allow(dead_code)]
    pub(crate) fn set_trace_hook(&self, hook: Option<TraceHook>) -> Result<(), HostError> {
        self.call_any_lifecycle_hook(TraceEvent::End)?;
        *self.try_borrow_trace_hook_mut()? = hook;
        self.call_any_lifecycle_hook(TraceEvent::Begin)?;
        Ok(())
    }

    pub(crate) fn call_any_lifecycle_hook(&self, event: TraceEvent) -> Result<(), HostError> {
        match &*self.try_borrow_trace_hook()? {
            Some(hook) => hook(self, event),
            None => Ok(()),
        }
    }
}
