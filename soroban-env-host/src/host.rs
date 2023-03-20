#![allow(unused_variables)]
#![allow(dead_code)]

use core::cell::RefCell;
use core::cmp::Ordering;
use core::fmt::Debug;
use std::rc::Rc;

use soroban_env_common::{
    xdr::{
        AccountId, Asset, ContractCodeEntry, ContractDataEntry, ContractEventType, ContractId,
        CreateContractArgs, ExtensionPoint, Hash, HashIdPreimage, HostFunction, HostFunctionType,
        InstallContractCodeArgs, Int128Parts, LedgerEntryData, LedgerKey, LedgerKeyContractCode,
        PublicKey, ScAddress, ScBytes, ScContractExecutable, ScHostContextErrorCode,
        ScHostFnErrorCode, ScHostObjErrorCode, ScHostStorageErrorCode, ScHostValErrorCode, ScMap,
        ScMapEntry, ScStatusType, ScString, ScSymbol, ScUnknownErrorCode, ScVal, ScVec, Uint256,
    },
    AddressObject, Bool, BytesObject, Convert, I128Object, I64Object, MapObject, ScValObjRef,
    ScValObject, Status, StringObject, SymbolObject, SymbolSmall, TryFromVal, TryIntoVal,
    U128Object, U32Val, U64Object, U64Val, VecObject, VmCaller, VmCallerEnv, Void, I256, U256,
};

use crate::budget::{AsBudget, Budget, CostType};
use crate::events::{
    DebugError, DebugEvent, Events, InternalContractEvent, InternalEvent, InternalEventsBuffer,
};
use crate::storage::{Storage, StorageMap};
use crate::{
    auth::{AuthorizationManager, AuthorizationManagerSnapshot, RecordedAuthPayload},
    native_contract::account_contract::ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME,
};

use crate::host_object::{HostMap, HostObject, HostObjectType, HostVec};
use crate::SymbolStr;
#[cfg(feature = "vm")]
use crate::Vm;
use crate::{EnvBase, Object, RawVal, Symbol};

pub(crate) mod comparison;
mod conversion;
mod data_helper;
pub(crate) mod declared_size;
mod diagnostics_helper;
mod err_helper;
mod error;
pub(crate) mod invoker_type;
mod mem_helper;
pub(crate) mod metered_clone;
pub(crate) mod metered_map;
pub(crate) mod metered_vector;
pub(crate) mod metered_xdr;
mod validity;
pub use error::HostError;

use self::metered_vector::MeteredVector;
use self::{invoker_type::InvokerType, metered_clone::MeteredClone};
use crate::Compare;

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
// Notes on metering: `RollbackPoint` are metered under Frame operations
#[derive(Clone)]
struct RollbackPoint {
    storage: StorageMap,
    events: usize,
    auth: Option<AuthorizationManagerSnapshot>,
}

#[cfg(any(test, feature = "testutils"))]
pub trait ContractFunctionSet {
    fn call(&self, func: &Symbol, host: &Host, args: &[RawVal]) -> Option<RawVal>;
}

#[cfg(any(test, feature = "testutils"))]
#[derive(Debug, Clone)]
pub struct TestContractFrame {
    pub id: Hash,
    pub func: Symbol,
    pub args: Vec<RawVal>,
    panic: Rc<RefCell<Option<Status>>>,
}

#[cfg(any(test, feature = "testutils"))]
impl TestContractFrame {
    pub fn new(id: Hash, func: Symbol, args: Vec<RawVal>) -> Self {
        Self {
            id,
            func,
            args,
            panic: Rc::new(RefCell::new(None)),
        }
    }
}

/// Holds contextual information about a single invocation, either
/// a reference to a contract [`Vm`] or an enclosing [`HostFunction`]
/// invocation.
///
/// Frames are arranged into a stack in [`HostImpl::context`], and are pushed
/// with [`Host::push_frame`], which returns a [`FrameGuard`] that will
/// pop the frame on scope-exit.
///
/// Frames are also the units of (sub-)transactions: each frame captures
/// the host state when it is pushed, and the [`FrameGuard`] will either
/// commit or roll back that state when it pops the stack.
#[derive(Clone)]
pub(crate) enum Frame {
    #[cfg(feature = "vm")]
    ContractVM(Rc<Vm>, Symbol, Vec<RawVal>),
    HostFunction(HostFunctionType),
    Token(Hash, Symbol, Vec<RawVal>),
    #[cfg(any(test, feature = "testutils"))]
    TestContract(TestContractFrame),
}

/// Determines the re-entry mode for calling a contract.
pub(crate) enum ContractReentryMode {
    /// Re-entry is completely prohibited.
    Prohibited,
    /// Re-entry is allowed, but only directly into the same contract (i.e. it's
    /// possible for a contract to do a self-call via host).
    SelfAllowed,
    /// Re-entry is fully allowed.
    Allowed,
}

/// Temporary helper for denoting a slice of guest memory, as formed by
/// various bytes operations.
#[cfg(feature = "vm")]
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

#[derive(Clone)]
pub enum DiagnosticLevel {
    None,
    Debug,
}

impl Default for DiagnosticLevel {
    fn default() -> Self {
        DiagnosticLevel::None
    }
}

#[derive(Clone, Default)]
pub(crate) struct HostImpl {
    source_account: RefCell<Option<AccountId>>,
    ledger: RefCell<Option<LedgerInfo>>,
    objects: RefCell<Vec<HostObject>>,
    storage: RefCell<Storage>,
    pub(crate) context: RefCell<Vec<Frame>>,
    // Note: budget is refcounted and is _not_ deep-cloned when you call HostImpl::deep_clone,
    // mainly because it's not really possible to achieve (the same budget is connected to many
    // metered sub-objects) but also because it's plausible that the person calling deep_clone
    // actually wants their clones to be metered by "the same" total budget
    pub(crate) budget: Budget,
    pub(crate) events: RefCell<InternalEventsBuffer>,
    authorization_manager: RefCell<AuthorizationManager>,
    diagnostic_level: RefCell<DiagnosticLevel>,
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
    // This enables test-only functions like `verify_top_authorization`
    // that allow checking if the authorization has been recorded.
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

impl Convert<&Object, ScValObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: &Object) -> Result<ScValObject, Self::Error> {
        self.from_host_obj(*ob)
    }
}

impl Convert<Object, ScValObject> for Host {
    type Error = HostError;
    fn convert(&self, ob: Object) -> Result<ScValObject, Self::Error> {
        self.from_host_obj(ob)
    }
}

impl<'a> Convert<&ScValObjRef<'a>, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: &ScValObjRef<'a>) -> Result<Object, Self::Error> {
        self.to_host_obj(ob)
    }
}

impl<'a> Convert<ScValObjRef<'a>, Object> for Host {
    type Error = HostError;
    fn convert(&self, ob: ScValObjRef<'a>) -> Result<Object, Self::Error> {
        self.to_host_obj(&ob)
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
        auth_entries: Vec<soroban_env_common::xdr::ContractAuth>,
    ) -> Result<(), HostError> {
        let new_auth_manager = AuthorizationManager::new_enforcing(self, auth_entries)?;
        *self.0.authorization_manager.borrow_mut() = new_auth_manager;
        Ok(())
    }

    pub fn set_ledger_info(&self, info: LedgerInfo) {
        *self.0.ledger.borrow_mut() = Some(info)
    }

    pub fn with_ledger_info<F, T>(&self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&LedgerInfo) -> Result<T, HostError>,
    {
        match self.0.ledger.borrow().as_ref() {
            None => Err(self.err_general("missing ledger info")),
            Some(li) => f(li),
        }
    }

    pub fn with_mut_ledger_info<F>(&self, mut f: F) -> Result<(), HostError>
    where
        F: FnMut(&mut LedgerInfo),
    {
        match self.0.ledger.borrow_mut().as_mut() {
            None => Err(self.err_general("missing ledger info")),
            Some(li) => Ok(f(li)),
        }
    }

    /// Helper for mutating the [`Budget`] held in this [`Host`], either to
    /// allocate it on contract creation or to deplete it on callbacks from
    /// the VM or host functions.
    pub fn with_budget<T, F>(&self, f: F) -> T
    where
        F: FnOnce(Budget) -> T,
    {
        f(self.0.budget.clone())
    }

    pub(crate) fn budget_ref(&self) -> &Budget {
        &self.0.budget
    }

    pub fn budget_cloned(&self) -> Budget {
        self.0.budget.clone()
    }

    pub fn charge_budget(&self, ty: CostType, input: u64) -> Result<(), HostError> {
        self.0.budget.clone().charge(ty, input)
    }

    pub fn set_diagnostic_level(&self, diagnostic_level: DiagnosticLevel) {
        *self.0.diagnostic_level.borrow_mut() = diagnostic_level;
    }

    pub fn is_debug(&self) -> bool {
        matches!(*self.0.diagnostic_level.borrow(), DiagnosticLevel::Debug)
    }

    pub(crate) fn get_events_mut<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut InternalEventsBuffer) -> Result<U, HostError>,
    {
        f(&mut *self.0.events.borrow_mut())
    }

    /// Records a debug event. This in itself is not necessarily an error; it
    /// might just be some contextual event we want to put in a debug log for
    /// diagnostic purpopses. The return value from this is therefore () when
    /// the event is recorded successfully, even if the event itself
    /// _represented_ some other error. This function only returns Err(...) when
    /// there was a failure to record the event, such as when budget is
    /// exceeded.
    pub fn record_debug_event<T>(&self, src: T) -> Result<(), HostError>
    where
        DebugEvent: From<T>,
    {
        // We want to record an event _before_ we charge the budget, to maximize
        // the chance we return "what the contract was doing when it ran out of
        // gas" in cases it does. This does mean in that one case we'll exceed
        // the gas limit a tiny amount (one event-worth) but it's not something
        // users can harm us with nor does it observably effect the order the
        // contract runs out of gas in; this is an atomic action from the
        // contract's perspective.
        let event: DebugEvent = src.into();
        self.get_events_mut(|events| {
            Ok(events.record(InternalEvent::Debug(event), self.as_budget()))
        })?
    }

    // Records a contract event.
    pub(crate) fn record_contract_event(
        &self,
        type_: ContractEventType,
        topics: VecObject,
        data: RawVal,
    ) -> Result<(), HostError> {
        self.validate_contract_event_topics(topics)?;
        let ce = InternalContractEvent {
            type_,
            contract_id: self.bytesobj_from_internal_contract_id()?,
            topics,
            data,
        };
        self.get_events_mut(|events| {
            Ok(events.record(InternalEvent::Contract(ce), self.as_budget()))
        })?
    }

    pub fn with_mut_storage<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Storage) -> Result<U, HostError>,
    {
        f(&mut *self.0.storage.borrow_mut())
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
            .map_err(|e| (Host(e), ScUnknownErrorCode::General.into()))
    }

    /// Helper function for [`Host::with_frame`] below. Pushes a new [`Frame`]
    /// on the context stack, returning a [`RollbackPoint`] such that if
    /// operation fails, it can be used to roll the [`Host`] back to the state
    /// it had before its associated [`Frame`] was pushed.
    fn push_frame(&self, frame: Frame) -> Result<RollbackPoint, HostError> {
        // This is a bit hacky, as it relies on re-borrow to occur only during
        // the account contract invocations. Instead we should probably call it
        // in more explicitly different fashion and check if we're calling it
        // instead of a borrow check.
        let mut auth_snapshot = None;
        if let Ok(mut auth_manager) = self.0.authorization_manager.try_borrow_mut() {
            auth_manager.push_frame(self, &frame)?;
            auth_snapshot = Some(auth_manager.snapshot());
        }

        self.0.context.borrow_mut().push(frame);
        Ok(RollbackPoint {
            storage: self.0.storage.borrow().map.clone(),
            events: self.0.events.borrow().vec.len(),
            auth: auth_snapshot,
        })
    }

    /// Helper function for [`Host::with_frame`] below. Pops a [`Frame`] off
    /// the current context and optionally rolls back the [`Host`]'s objects
    /// and storage map to the state in the provided [`RollbackPoint`].
    fn pop_frame(&self, orp: Option<RollbackPoint>) -> Result<(), HostError> {
        self.0
            .context
            .borrow_mut()
            .pop()
            .expect("unmatched host frame push/pop");
        // This is a bit hacky, as it relies on re-borrow to occur only doing
        // the account contract invocations. Instead we should probably call it
        // in more explicitly different fashion and check if we're calling it
        // instead of a borrow check.
        if let Ok(mut auth_manager) = self.0.authorization_manager.try_borrow_mut() {
            auth_manager.pop_frame();
        }

        if self.0.context.borrow().is_empty() {
            // When there are no frames left, emulate authentication for the
            // recording auth mode. This is a no-op for the enforcing mode.
            self.0
                .authorization_manager
                .borrow_mut()
                .maybe_emulate_authentication(self)?;
            // Empty call stack in tests means that some contract function call
            // has been finished and hence the authorization manager can be reset.
            // In non-test scenarios, there should be no need to ever reset
            // the authorization manager as the host instance shouldn't be
            // shared between the contract invocations.
            #[cfg(any(test, feature = "testutils"))]
            {
                *self.0.previous_authorization_manager.borrow_mut() =
                    Some(self.0.authorization_manager.borrow().clone());
                self.0.authorization_manager.borrow_mut().reset();
            }
        }

        if let Some(rp) = orp {
            self.0.storage.borrow_mut().map = rp.storage;
            self.0.events.borrow_mut().rollback(rp.events)?;
            if let Some(auth_rp) = rp.auth {
                self.0.authorization_manager.borrow_mut().rollback(auth_rp);
            }
        }
        Ok(())
    }

    /// Applies a function to the top [`Frame`] of the context stack. Returns
    /// [`HostError`] if the context stack is empty, otherwise returns result of
    /// function call.
    // Notes on metering: aquiring the current frame is cheap and not charged.
    /// Metering happens in the passed-in closure where actual work is being done.
    fn with_current_frame<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&Frame) -> Result<U, HostError>,
    {
        f(self
            .0
            .context
            .borrow()
            .last()
            .ok_or_else(|| self.err(DebugError::new(ScHostContextErrorCode::NoContractRunning)))?)
    }

    /// Same as [`Self::with_current_frame`] but passes `None` when there is no current
    /// frame, rather than logging an error.
    fn with_current_frame_opt<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(Option<&Frame>) -> Result<U, HostError>,
    {
        f(self.0.context.borrow().last())
    }

    /// Pushes a [`Frame`], runs a closure, and then pops the frame, rolling back
    /// if the closure returned an error. Returns the result that the closure
    /// returned (or any error caused during the frame push/pop).
    // Notes on metering: `GuardFrame` charges on the work done on protecting the `context`.
    // It does not cover the cost of the actual closure call. The closure needs to be
    // metered separately.
    pub(crate) fn with_frame<F>(&self, frame: Frame, f: F) -> Result<RawVal, HostError>
    where
        F: FnOnce() -> Result<RawVal, HostError>,
    {
        self.charge_budget(CostType::GuardFrame, 1)?;
        let start_depth = self.0.context.borrow().len();
        let rp = self.push_frame(frame)?;
        let res = f();
        let res = match res {
            Ok(v) if v.is::<Status>() => {
                let st: Status = v.try_into()?;
                if st.is_ok() {
                    // Transform Status::OK => Ok(())
                    Ok(RawVal::VOID.into())
                } else {
                    // All other Status::N => Err(N)
                    Err(self.err_status(st))
                }
            }
            res => res,
        };
        if res.is_err() {
            // Pop and rollback on error.
            self.pop_frame(Some(rp))?;
        } else {
            // Just pop on success.
            self.pop_frame(None)?;
        }
        // Every push and pop should be matched; if not there is a bug.
        let end_depth = self.0.context.borrow().len();
        assert_eq!(start_depth, end_depth);
        res
    }

    /// Pushes a test contract [`Frame`], runs a closure, and then pops the
    /// frame, rolling back if the closure returned an error. Returns the result
    /// that the closure returned (or any error caused during the frame
    /// push/pop). Used for testing.
    #[cfg(any(test, feature = "testutils"))]
    pub fn with_test_contract_frame<F>(
        &self,
        id: Hash,
        func: Symbol,
        f: F,
    ) -> Result<RawVal, HostError>
    where
        F: FnOnce() -> Result<RawVal, HostError>,
    {
        self.with_frame(
            Frame::TestContract(TestContractFrame::new(id, func, vec![])),
            f,
        )
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
            let evt = DebugEvent::new()
                .msg("check auth invocation for a custom account contract resulted in error {}")
                .arg::<RawVal>(e.status.into());
            self.record_debug_event(evt)?;
        }
        res
    }

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    pub(crate) fn get_current_contract_id_opt_internal(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame(|frame| match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm, _, _) => Ok(Some(vm.contract_id.metered_clone(&self.0.budget)?)),
            Frame::HostFunction(_) => Ok(None),
            Frame::Token(id, _, _) => Ok(Some(id.metered_clone(&self.0.budget)?)),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => Ok(Some(tc.id.clone())),
        })
    }

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    pub(crate) fn get_current_contract_id_internal(&self) -> Result<Hash, HostError> {
        if let Some(id) = self.get_current_contract_id_opt_internal()? {
            Ok(id)
        } else {
            Err(self.err_general("Current context has no contract ID"))
        }
    }

    // Notes on metering: closure call needs to be metered separatedly. `VisitObject` only covers
    // the cost of visiting an object.
    pub(crate) unsafe fn unchecked_visit_val_obj<F, U>(
        &self,
        obj: impl Into<Object>,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(Option<&HostObject>) -> Result<U, HostError>,
    {
        self.charge_budget(CostType::VisitObject, 1)?;
        let r = self.0.objects.borrow();
        let obj: Object = obj.into();
        let handle: u32 = obj.get_handle();
        f(r.get(handle as usize))
    }

    // Notes on metering: object visiting part is covered by unchecked_visit_val_obj. Closure function
    // needs to be metered separately.
    pub(crate) fn visit_obj<HOT: HostObjectType, F, U>(
        &self,
        obj: HOT::Wrapper,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(&HOT) -> Result<U, HostError>,
    {
        unsafe {
            self.unchecked_visit_val_obj(obj, |hopt| match hopt {
                None => Err(self.err_status(ScHostObjErrorCode::UnknownReference)),
                Some(hobj) => match HOT::try_extract(hobj) {
                    None => Err(self.err_status(ScHostObjErrorCode::UnexpectedType)),
                    Some(hot) => f(hot),
                },
            })
        }
    }

    // Testing interface to create values directly for later use via Env functions.
    // It needs to be a `pub` method because benches are considered a separate crate.
    pub fn inject_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.to_host_val(v).map(Into::into)
    }

    pub fn get_events(&self) -> Result<Events, HostError> {
        self.0.events.borrow().externalize(&self)
    }

    pub(crate) fn from_host_val(&self, val: RawVal) -> Result<ScVal, HostError> {
        // Charges a single unit to for the RawVal -> ScVal conversion.
        // The actual conversion logic occurs in the `common` crate, which
        // translates a u64 into another form defined by the xdr.
        // For an `Object`, the actual structural conversion (such as byte
        // cloning) occurs in `from_host_obj` and is metered there.
        self.charge_budget(CostType::ValXdrConv, 1)?;
        ScVal::try_from_val(self, &val)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn to_host_val(&self, v: &ScVal) -> Result<RawVal, HostError> {
        self.charge_budget(CostType::ValXdrConv, 1)?;
        v.try_into_val(self)
            .map_err(|_| self.err_status(ScHostValErrorCode::UnknownError))
    }

    pub(crate) fn from_host_obj(&self, ob: impl Into<Object>) -> Result<ScValObject, HostError> {
        unsafe {
            self.unchecked_visit_val_obj(ob.into(), |ob| {
                // This accounts for conversion of "primitive" objects (e.g U64)
                // and the "shell" of a complex object (ScMap). Any non-trivial
                // work such as byte cloning, has to be accounted for and
                // metered in indivial match arms.
                self.charge_budget(CostType::ValXdrConv, 1)?;
                let val = match ob {
                    None => {
                        return Err(self.err_status(ScHostObjErrorCode::UnknownReference));
                    }
                    Some(ho) => match ho {
                        HostObject::Vec(vv) => {
                            metered_clone::charge_heap_alloc::<ScVal>(
                                vv.len() as u64,
                                self.as_budget(),
                            )?;
                            let sv = vv.iter().map(|e| self.from_host_val(*e)).collect::<Result<
                                Vec<ScVal>,
                                HostError,
                            >>(
                            )?;
                            ScVal::Vec(Some(ScVec(self.map_err(sv.try_into())?)))
                        }
                        HostObject::Map(mm) => {
                            metered_clone::charge_heap_alloc::<ScMapEntry>(
                                mm.len() as u64,
                                self.as_budget(),
                            )?;
                            let mut mv = Vec::with_capacity(mm.len());
                            for (k, v) in mm.iter(self)? {
                                let key = self.from_host_val(*k)?;
                                let val = self.from_host_val(*v)?;
                                mv.push(ScMapEntry { key, val });
                            }
                            ScVal::Map(Some(ScMap(self.map_err(mv.try_into())?)))
                        }
                        HostObject::U64(u) => ScVal::U64(*u),
                        HostObject::I64(i) => ScVal::I64(*i),
                        HostObject::TimePoint(tp) => {
                            ScVal::Timepoint(tp.metered_clone(self.as_budget())?)
                        }
                        HostObject::Duration(d) => {
                            ScVal::Duration(d.metered_clone(self.as_budget())?)
                        }
                        HostObject::U128(u) => ScVal::U128(Int128Parts {
                            lo: *u as u64,
                            hi: (*u >> 64) as u64,
                        }),
                        HostObject::I128(u) => {
                            let u = *u as u128;
                            ScVal::I128(Int128Parts {
                                lo: u as u64,
                                hi: (u >> 64) as u64,
                            })
                        }
                        HostObject::U256(u) => ScVal::U256(Uint256(u.to_be_bytes())),
                        HostObject::I256(i) => ScVal::I256(Uint256(i.to_be_bytes())),
                        HostObject::Bytes(b) => ScVal::Bytes(b.metered_clone(self.as_budget())?),
                        HostObject::String(s) => ScVal::String(s.metered_clone(self.as_budget())?),
                        HostObject::Symbol(s) => ScVal::Symbol(s.metered_clone(self.as_budget())?),
                        HostObject::ContractExecutable(cc) => {
                            ScVal::ContractExecutable(cc.metered_clone(self.as_budget())?)
                        }
                        HostObject::Address(addr) => {
                            ScVal::Address(addr.metered_clone(self.as_budget())?)
                        }
                        HostObject::NonceKey(nk) => {
                            ScVal::LedgerKeyNonce(nk.metered_clone(self.as_budget())?)
                        }
                    },
                };
                Ok(ScValObject::unchecked_from_val(val))
            })
        }
    }

    pub(crate) fn to_host_obj<'a>(&self, ob: &ScValObjRef<'a>) -> Result<Object, HostError> {
        self.charge_budget(CostType::ValXdrConv, 1)?;
        let val: &ScVal = (*ob).into();
        match val {
            ScVal::Vec(Some(v)) => {
                metered_clone::charge_heap_alloc::<RawVal>(v.len() as u64, self.as_budget())?;
                let mut vv = Vec::with_capacity(v.len());
                for e in v.iter() {
                    vv.push(self.to_host_val(e)?)
                }
                Ok(self
                    .add_host_object(HostVec::from_vec(vv, self.as_budget())?)?
                    .into())
            }
            ScVal::Map(Some(m)) => {
                metered_clone::charge_heap_alloc::<(RawVal, RawVal)>(
                    m.len() as u64,
                    self.as_budget(),
                )?;
                let mut mm = Vec::with_capacity(m.len());
                for pair in m.iter() {
                    let k = self.to_host_val(&pair.key)?;
                    let v = self.to_host_val(&pair.val)?;
                    mm.push((k, v))
                }
                Ok(self.add_host_object(HostMap::from_map(mm, self)?)?.into())
            }
            ScVal::Vec(None) | ScVal::Map(None) => {
                Err(self.err_status(ScHostValErrorCode::MissingObject))
            }
            ScVal::U64(u) => Ok(self.add_host_object(*u)?.into()),
            ScVal::I64(i) => Ok(self.add_host_object(*i)?.into()),
            ScVal::Timepoint(t) => Ok(self
                .add_host_object(t.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::Duration(d) => Ok(self
                .add_host_object(d.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::U128(u) => Ok(self
                .add_host_object(u.lo as u128 | ((u.hi as u128) << 64))?
                .into()),
            ScVal::I128(i) => Ok(self
                .add_host_object((i.lo as u128 | ((i.hi as u128) << 64)) as i128)?
                .into()),
            ScVal::U256(u) => Ok(self
                .add_host_object(U256::from_be_bytes(u.0.metered_clone(self.as_budget())?))?
                .into()),
            ScVal::I256(i) => Ok(self
                .add_host_object(I256::from_be_bytes(i.0.metered_clone(self.as_budget())?))?
                .into()),
            ScVal::Bytes(b) => Ok(self
                .add_host_object(b.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::String(s) => Ok(self
                .add_host_object(s.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::Symbol(s) => Ok(self
                .add_host_object(s.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::ContractExecutable(cc) => Ok(self
                .add_host_object(cc.metered_clone(self.as_budget())?)?
                .into()),
            ScVal::LedgerKeyNonce(_) => {
                Err(self.err_general("nonce keys aren't allowed to be used directly"))
            }
            ScVal::Address(addr) => Ok(self
                .add_host_object(addr.metered_clone(self.as_budget())?)?
                .into()),

            ScVal::Bool(_)
            | ScVal::Void
            | ScVal::Status(_)
            | ScVal::U32(_)
            | ScVal::I32(_)
            | ScVal::LedgerKeyContractExecutable => {
                Err(self.err_status(ScHostObjErrorCode::UnexpectedType))
            }
        }
    }

    /// Moves a value of some type implementing [`HostObjectType`] into the host's
    /// object array, returning a [`HostObj`] containing the new object's array
    /// index, tagged with the [`xdr::ScObjectType`].
    pub(crate) fn add_host_object<HOT: HostObjectType>(
        &self,
        hot: HOT,
    ) -> Result<HOT::Wrapper, HostError> {
        let prev_len = self.0.objects.borrow().len();
        if prev_len > u32::MAX as usize {
            return Err(self.err_status(ScHostObjErrorCode::ObjectCountExceedsU32Max));
        }
        // charge for the new host object, which is just the amortized cost of a single
        // `HostObject` allocation
        metered_clone::charge_heap_alloc::<HostObject>(1, self.as_budget())?;
        self.0.objects.borrow_mut().push(HOT::inject(hot));
        let handle = prev_len as u32;
        Ok(HOT::new_from_handle(handle))
    }

    // Notes on metering: this is covered by the called components.
    fn create_contract_with_id(
        &self,
        contract_id: BytesObject,
        contract_source: ScContractExecutable,
    ) -> Result<(), HostError> {
        let new_contract_id = self.hash_from_bytesobj_input("id_obj", contract_id)?;
        let storage_key = self.contract_source_ledger_key(&new_contract_id)?;
        if self
            .0
            .storage
            .borrow_mut()
            .has(&storage_key, self.as_budget())?
        {
            return Err(self.err_general("Contract already exists"));
        }
        // Make sure the contract code exists. With immutable contracts and
        // without this check it would be possible to accidentally create a
        // contract that never may be invoked (just by providing a bad hash).
        if let ScContractExecutable::WasmRef(wasm_hash) = &contract_source {
            let wasm_storage_key = self.contract_code_ledger_key(wasm_hash)?;
            if !self
                .0
                .storage
                .borrow_mut()
                .has(&wasm_storage_key, self.as_budget())?
            {
                return Err(self.err_general("Contract code was not installed"));
            }
        }
        self.store_contract_source(contract_source, new_contract_id, &storage_key)?;
        Ok(())
    }

    fn maybe_initialize_asset_token(
        &self,
        contract_id: BytesObject,
        id_preimage: HashIdPreimage,
    ) -> Result<(), HostError> {
        if let HashIdPreimage::ContractIdFromAsset(asset_preimage) = id_preimage {
            let mut asset_bytes: Vec<u8> = Default::default();
            self.metered_write_xdr(&asset_preimage.asset, &mut asset_bytes)?;
            self.call_n(
                contract_id,
                Symbol::try_from_val(self, &"init_asset")?,
                &[self
                    .add_host_object(self.scbytes_from_vec(asset_bytes)?)?
                    .into()],
                ContractReentryMode::Prohibited,
            )?;
            Ok(())
        } else {
            Ok(())
        }
    }

    fn create_contract_with_id_preimage(
        &self,
        contract_source: ScContractExecutable,
        id_preimage: HashIdPreimage,
    ) -> Result<BytesObject, HostError> {
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        let id_obj = self.add_host_object(self.scbytes_from_hash(&Hash(id_arr))?)?;
        self.create_contract_with_id(id_obj, contract_source.metered_clone(self.budget_ref())?)?;
        self.maybe_initialize_asset_token(id_obj, id_preimage)?;
        Ok(id_obj)
    }

    pub(crate) fn get_contract_id_from_asset(&self, asset: Asset) -> Result<Hash, HostError> {
        let id_preimage = self.id_preimage_from_asset(asset)?;
        let id_arr: [u8; 32] = self.metered_hash_xdr(&id_preimage)?;
        Ok(Hash(id_arr))
    }

    // Notes on metering: this is covered by the called components.
    fn call_contract_fn(
        &self,
        id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        // Create key for storage
        let storage_key = self.contract_source_ledger_key(id)?;
        match self.retrieve_contract_source_from_storage(&storage_key)? {
            #[cfg(feature = "vm")]
            ScContractExecutable::WasmRef(wasm_hash) => {
                let code_entry = self.retrieve_contract_code_from_storage(&wasm_hash)?;
                let vm = Vm::new(
                    self,
                    id.metered_clone(&self.0.budget)?,
                    code_entry.code.as_slice(),
                )?;
                vm.invoke_function_raw(self, func, args)
            }
            #[cfg(not(feature = "vm"))]
            ScContractExecutable::WasmRef(_) => Err(self.err_general("could not dispatch")),
            ScContractExecutable::Token => self.with_frame(
                Frame::Token(id.clone(), func.clone(), args.to_vec()),
                || {
                    use crate::native_contract::{NativeContract, Token};
                    Token.call(func, self, args)
                },
            ),
        }
    }

    fn call_n(
        &self,
        id: BytesObject,
        func: Symbol,
        args: &[RawVal],
        reentry_mode: ContractReentryMode,
    ) -> Result<RawVal, HostError> {
        let id = self.hash_from_bytesobj_input("contract", id)?;
        self.call_n_internal(&id, func, args, reentry_mode, false)
    }

    // Notes on metering: this is covered by the called components.
    pub(crate) fn call_n_internal(
        &self,
        id: &Hash,
        func: Symbol,
        args: &[RawVal],
        reentry_mode: ContractReentryMode,
        internal_host_call: bool,
    ) -> Result<RawVal, HostError> {
        // Internal host calls may call some special functions that otherwise
        // aren't allowed to be called.
        if !internal_host_call {
            if SymbolStr::try_from_val(self, &func)?.to_string().as_str()
                == ACCOUNT_CONTRACT_CHECK_AUTH_FN_NAME
            {
                return Err(self.err_status_msg(
                    ScHostContextErrorCode::UnknownError,
                    "can't invoke a custom account contract directly",
                ));
            }
        }
        if !matches!(reentry_mode, ContractReentryMode::Allowed) {
            let mut is_last_non_host_frame = true;
            for f in self.0.context.borrow().iter().rev() {
                let exist_id = match f {
                    #[cfg(feature = "vm")]
                    Frame::ContractVM(vm, _, _) => &vm.contract_id,
                    Frame::Token(id, _, _) => id,
                    #[cfg(any(test, feature = "testutils"))]
                    Frame::TestContract(tc) => &tc.id,
                    Frame::HostFunction(_) => continue,
                };
                if id == exist_id {
                    if matches!(reentry_mode, ContractReentryMode::SelfAllowed)
                        && is_last_non_host_frame
                    {
                        is_last_non_host_frame = false;
                        continue;
                    }
                    return Err(self.err_status_msg(
                        // TODO: proper error code
                        ScHostContextErrorCode::UnknownError,
                        "Contract re-entry is not allowed",
                    ));
                }
                is_last_non_host_frame = false;
            }
        }

        self.fn_call_diagnostics(&id, &func, args)?;

        // "testutils" is not covered by budget metering.
        #[cfg(any(test, feature = "testutils"))]
        {
            // This looks a little un-idiomatic, but this avoids maintaining a borrow of
            // self.0.contracts. Implementing it as
            //
            //     if let Some(cfs) = self.0.contracts.borrow().get(&id).cloned() { ... }
            //
            // maintains a borrow of self.0.contracts, which can cause borrow errors.
            let cfs_option = self.0.contracts.borrow().get(&id).cloned();
            if let Some(cfs) = cfs_option {
                let frame = TestContractFrame::new(id.clone(), func.clone(), args.to_vec());
                let panic = frame.panic.clone();
                return self.with_frame(Frame::TestContract(frame), || {
                    use std::any::Any;
                    use std::panic::AssertUnwindSafe;
                    type PanicVal = Box<dyn Any + Send>;

                    // We're directly invoking a native rust contract here,
                    // which we allow only in local testing scenarios, and we
                    // want it to behave as close to the way it would behave if
                    // the contract were actually compiled to WASM and running
                    // in a VM.
                    //
                    // In particular: if the contract function panics, if it
                    // were WASM it would cause the VM to trap, so we do
                    // something "as similar as we can" in the native case here,
                    // catch the native panic and attempt to continue by
                    // translating the panic back to an error, so that
                    // `with_frame` will rollback the host to its pre-call state
                    // (as best it can) and propagate the error to its caller
                    // (which might be another contract doing try_call).
                    //
                    // This is somewhat best-effort, but it's compiled-out when
                    // building a host for production use, so we're willing to
                    // be a bit forgiving.
                    let closure = AssertUnwindSafe(move || cfs.call(&func, self, args));
                    let res: Result<Option<RawVal>, PanicVal> = testutils::call_with_suppressed_panic_hook(closure);
                    match res {
                        Ok(Some(rawval)) => {
                            self.fn_return_diagnostics(id, &func, &rawval)?;
                            Ok(rawval)
                        },
                        Ok(None) => Err(self.err(
                            DebugError::general()
                                .msg("error '{}': calling unknown contract function '{}'")
                                .arg::<RawVal>(func.into()),
                        )),
                        Err(panic_payload) => {
                            // Return an error indicating the contract function
                            // panicked. If if was a panic generated by a
                            // Env-upgraded HostError, it had its status
                            // captured by VmCallerEnv::escalate_error_to_panic:
                            // fish the Status stored in the frame back out and
                            // propagate it.
                            let func: RawVal = func.into();
                            let mut status: Status = ScUnknownErrorCode::General.into();
                            let mut event = DebugEvent::new().msg("caught panic from contract function '{}'").arg(func);

                            if let Some(st) = *panic.borrow() {
                                status = st;
                                event = DebugEvent::new().msg("caught panic from contract function '{}', propagating escalated error '{}'").arg(func).arg(st.to_raw());
                            }
                            // If we're allowed to record dynamic strings (which happens in
                            // native test configurations), also log the panic payload into
                            // the .
                            else if cfg!(feature = "hostfn_log_fmt_values") {
                                if let Some(str) = panic_payload.downcast_ref::<&str>() {
                                    let msg: String = format!("caught panic '{}' from contract function '{:?}'", str, func);
                                    event = DebugEvent::new().msg(msg);
                                } else if let Some(str) = panic_payload.downcast_ref::<String>() {
                                    let msg: String = format!("caught panic '{}' from contract function '{:?}'", str, func);
                                    event = DebugEvent::new().msg(msg);
                                }
                            }
                            Err(self.err(DebugError{event, status}))
                        }
                    }
                });
            }
        }

        let res = self.call_contract_fn(&id, &func, args);

        match &res {
            Ok(res) => self.fn_return_diagnostics(id, &func, &res)?,
            Err(err) => {}
        }

        return res;
    }

    // Notes on metering: covered by the called components.
    fn invoke_function_raw(&self, hf: HostFunction) -> Result<RawVal, HostError> {
        let hf_type = hf.discriminant();
        //TODO: should the create_* methods below return a RawVal instead of Object to avoid this conversion?
        match hf {
            HostFunction::InvokeContract(args) => {
                if let [ScVal::Bytes(scbytes), ScVal::Symbol(scsym), rest @ ..] = args.as_slice() {
                    self.with_frame(Frame::HostFunction(hf_type), || {
                        // Metering: conversions to host objects are covered. Cost of collecting
                        // RawVals into Vec is ignored. Since 1. RawVals are cheap to clone 2. the
                        // max number of args is fairly limited.
                        let object = self.add_host_object(scbytes.clone())?;
                        let symbol: Symbol = scsym.as_slice().try_into_val(self)?;
                        let args = self.scvals_to_rawvals(rest)?;
                        // since the `HostFunction` frame must be the bottom of the call stack,
                        // reentry is irrelevant, we always pass in `ContractReentryMode::Prohibited`.
                        self.call_n(object, symbol, &args[..], ContractReentryMode::Prohibited)
                    })
                } else {
                    Err(self.err_status_msg(
                        ScHostFnErrorCode::InputArgsWrongLength,
                        "unexpected arguments to 'call' host function",
                    ))
                }
            }
            HostFunction::CreateContract(args) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.create_contract(args).map(|obj| <RawVal>::from(obj))
                }),
            HostFunction::InstallContractCode(args) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.install_contract(args).map(|obj| <RawVal>::from(obj))
                }),
        }
    }

    // Notes on metering: covered by the called components.
    pub fn invoke_function(&self, hf: HostFunction) -> Result<ScVal, HostError> {
        let rv = self.invoke_function_raw(hf)?;
        self.from_host_val(rv)
    }

    // "testutils" is not covered by budget metering.
    #[cfg(any(test, feature = "testutils"))]
    pub fn register_test_contract(
        &self,
        contract_id: BytesObject,
        contract_fns: Rc<dyn ContractFunctionSet>,
    ) -> Result<(), HostError> {
        let hash = self.hash_from_bytesobj_input("contract_id", contract_id)?;
        let mut contracts = self.0.contracts.borrow_mut();
        if !contracts.contains_key(&hash) {
            contracts.insert(hash, contract_fns);
            Ok(())
        } else {
            Err(self.err_general("vtable already exists"))
        }
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

    // Returns the top-level authorizations that have been recorded for the last
    // contract invocation.
    // More technically, 'top-level' means that these invocations were the first
    // in the call tree to have called `require_auth` (i.e. they're not
    // necessarily invocations of the top-level contract that has been invoked).
    #[cfg(any(test, feature = "testutils"))]
    pub fn get_recorded_top_authorizations(
        &self,
    ) -> Result<Vec<(ScAddress, Hash, ScSymbol, ScVec)>, HostError> {
        Ok(self
            .0
            .previous_authorization_manager
            .borrow_mut()
            .as_mut()
            .ok_or_else(|| {
                self.err_general("previous invocation is missing - no auth data to get")
            })?
            .get_recorded_top_authorizations())
    }

    /// Records a `System` contract event. `topics` is expected to be a `SCVec`
    /// length <= 4 that cannot contain `Vec`, `Map`, or `Bytes` with length > 32
    /// On success, returns an `SCStatus::Ok`.
    pub fn system_event(&self, topics: VecObject, data: RawVal) -> Result<RawVal, HostError> {
        self.record_contract_event(ContractEventType::System, topics, data)?;
        Ok(Status::OK.into())
    }

    fn create_contract(&self, args: CreateContractArgs) -> Result<BytesObject, HostError> {
        let id_preimage = match args.contract_id {
            ContractId::Asset(asset) => self.id_preimage_from_asset(asset)?,
            ContractId::SourceAccount(salt) => self.id_preimage_from_source_account(salt)?,
            ContractId::Ed25519PublicKey(key_with_signature) => {
                let signature_payload_preimage = self.create_contract_args_hash_preimage(
                    args.source.metered_clone(&self.budget_ref())?,
                    key_with_signature.salt.metered_clone(self.budget_ref())?,
                )?;
                let signature_payload = self.metered_hash_xdr(&signature_payload_preimage)?;
                self.verify_sig_ed25519_internal(
                    &signature_payload,
                    &self.ed25519_pub_key_from_bytes(&key_with_signature.key.0)?,
                    &self.signature_from_bytes(
                        "create_contract_sig",
                        &key_with_signature.signature.0,
                    )?,
                )?;
                self.id_preimage_from_ed25519(key_with_signature.key, key_with_signature.salt)?
            }
        };
        self.create_contract_with_id_preimage(args.source, id_preimage)
    }

    fn install_contract(&self, args: InstallContractCodeArgs) -> Result<BytesObject, HostError> {
        let hash_bytes = self.metered_hash_xdr(&args)?;
        let hash_obj = self.add_host_object(self.scbytes_from_hash(&Hash(hash_bytes))?)?;
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
                    code: args.code,
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
        self.charge_budget(CostType::VerifyEd25519Sig, payload.len() as u64)?;
        public_key
            .verify(payload, &sig)
            .map_err(|_| self.err_general("Failed ED25519 verification"))
    }

    pub(crate) fn get_invoking_contract_internal(&self) -> Result<Hash, HostError> {
        let frames = self.0.context.borrow();
        // the previous frame must exist and must be a contract
        let hash = match frames.as_slice() {
            [.., f2, _] => match f2 {
                #[cfg(feature = "vm")]
                Frame::ContractVM(vm, _, _) => Ok(vm.contract_id.metered_clone(&self.0.budget)?),
                Frame::HostFunction(_) => Err(self.err_general("invoker is not a contract")),
                Frame::Token(id, _, _) => Ok(id.clone()),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => Ok(tc.id.clone()), // no metering
            },
            _ => Err(self.err_general("no frames to derive the invoker from")),
        }?;
        Ok(hash)
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
                .ok_or(self.err_general("previous invocation is missing - no payloads recorded"))?
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
            Err(self.err_general("symbol mismatch"))
        }
    }

    // Metering: mostly free or already covered by components (e.g. err_general)
    fn get_invoker_type(&self) -> Result<u64, HostError> {
        let frames = self.0.context.borrow();
        // If the previous frame exists and is a contract, return its ID, otherwise return
        // the account invoking.
        let st = match frames.as_slice() {
            // There are always two frames when WASM is executed in the VM.
            [.., f2, _] => match f2 {
                #[cfg(feature = "vm")]
                Frame::ContractVM(_, _, _) => Ok(InvokerType::Contract),
                Frame::HostFunction(_) => Ok(InvokerType::Account),
                Frame::Token(id, _, _) => Ok(InvokerType::Contract),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(_) => Ok(InvokerType::Contract),
            },
            // In tests contracts are executed with a single frame.
            // TODO: Investigate this discrepancy: https://github.com/stellar/rs-soroban-env/issues/485.
            [f1] => Ok(InvokerType::Account),
            _ => Err(self.err_general("no frames to derive the invoker from")),
        }?;
        Ok(st as u64)
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
    // error's Status value in that frame, such that `Host::call_n` above can
    // recover the Status when it _catches_ the panic and converts it back to an
    // error.
    //
    // It might seem like we ought to `std::panic::panic_any(e)` here, making
    // the panic carry a `HostError` or `Status` and catching it by dynamic type
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
                *frame.panic.borrow_mut() = Some(e.status);
            }
            Ok(())
        });
        let escalation = self.err_status_msg(e.status, "escalating error '{}' to panic");
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
        self.add_host_object(ScSymbol(s.as_bytes().to_vec().try_into()?))
    }

    fn map_new_from_slices(&self, keys: &[&str], vals: &[RawVal]) -> Result<MapObject, HostError> {
        // FIXME: this is not the right cost to charge here, but we need to charge
        // something for now.
        self.charge_budget(CostType::VecNew, keys.len() as u64)?;
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
        // FIXME: this is not the right cost to charge here, but we need to charge
        // something for now.
        self.charge_budget(CostType::VecNew, keys.len() as u64)?;
        if keys.len() != vals.len() {
            return Err(self.err_status(ScHostFnErrorCode::InputArgsWrongLength));
        }
        self.visit_obj(map, |hm: &HostMap| {
            if hm.len() != vals.len() {
                return Err(self.err_status(ScHostFnErrorCode::InputArgsWrongLength));
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
        // FIXME: this is not the right cost to charge here, but we need to charge
        // something for now.
        self.charge_budget(CostType::VecNew, vals.len() as u64)?;
        self.visit_obj(vec, |hv: &HostVec| {
            if hv.len() != vals.len() {
                return Err(self.err_status(ScHostFnErrorCode::InputArgsWrongLength));
            }
            vals.copy_from_slice(hv.as_slice());
            Ok(())
        })?;
        Ok(RawVal::VOID)
    }

    fn symbol_index_in_strs(&self, sym: Symbol, slices: &[&str]) -> Result<U32Val, Self::Error> {
        let mut found = None;
        self.metered_scan_slice_of_slices(slices, |i, slice| {
            if self.symbol_matches(slice.as_bytes(), sym)? {
                if found.is_none() {
                    found = Some(i)
                }
            }
            Ok(())
        })?;
        match found {
            None => Err(self.err_status(ScHostFnErrorCode::InputArgsInvalid)),
            Some(idx) => Ok(U32Val::from(self.usize_to_u32(idx)?)),
        }
    }

    fn log_static_fmt_val(&self, fmt: &'static str, v: RawVal) -> Result<(), HostError> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(v))
    }

    fn log_static_fmt_static_str(
        &self,
        fmt: &'static str,
        s: &'static str,
    ) -> Result<(), HostError> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(s))
    }

    fn log_static_fmt_val_static_str(
        &self,
        fmt: &'static str,
        v: RawVal,
        s: &'static str,
    ) -> Result<(), HostError> {
        self.record_debug_event(DebugEvent::new().msg(fmt).arg(v).arg(s))
    }

    fn log_static_fmt_general(
        &self,
        fmt: &'static str,
        vals: &[RawVal],
        strs: &[&'static str],
    ) -> Result<(), HostError> {
        let mut evt = DebugEvent::new().msg(fmt);
        for v in vals {
            evt = evt.arg(*v)
        }
        for s in strs {
            evt = evt.arg(*s)
        }
        self.record_debug_event(evt)
    }
}

impl VmCallerEnv for Host {
    type VmUserState = Host;

    // Notes on metering: covered by the components
    fn log_value(&self, _vmcaller: &mut VmCaller<Host>, v: RawVal) -> Result<Void, HostError> {
        self.record_debug_event(DebugEvent::new().arg(v))?;
        Ok(RawVal::VOID)
    }

    fn log_fmt_values(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        fmt: StringObject,
        args: VecObject,
    ) -> Result<Void, HostError> {
        if cfg!(feature = "hostfn_log_fmt_values") {
            let fmt: String = self
                .visit_obj(fmt, move |hv: &ScString| {
                    Ok(String::from_utf8(hv.clone().into()))
                })?
                .map_err(|_| {
                    self.err_general("log_fmt_values fmt string contains is invalid utf8")
                })?;
            let args: HostVec = self.visit_obj(args, move |hv: &HostVec| Ok(hv.clone()))?;
            self.record_debug_event(DebugEvent::new().msg(fmt).args(args.iter().cloned()))?;
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
                (Err(_), Err(_)) => return Err(self.err_general("two non-object args to obj_cmp")),
            }
        } {
            // If any of the above got us a result, great, use it.
            Some(res) => res,

            // Otherwise someone gave us an object and a non-paired value (not a small-value
            // case of the same type). Order hese by their tags.
            None => {
                let atag = a.get_tag();
                let btag = b.get_tag();
                if atag == btag {
                    // This shouldn't have happened, but if it does there's a logic error.
                    return Err(
                        self.err_general("equal-tagged values rejected by small-value obj_cmp")
                    );
                }
                a.get_tag().cmp(&b.get_tag())
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
        Ok(RawVal::VOID.into())
    }

    // Notes on metering: covered by the components.
    fn get_current_contract_address(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<AddressObject, HostError> {
        Ok(self
            .add_host_object(ScAddress::Contract(
                self.get_current_contract_id_internal()?,
            ))?
            .into())
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
        lo: u64,
        hi: u64,
    ) -> Result<U128Object, Self::Error> {
        let u: u128 = ((hi as u128) << 64) | lo as u128;
        self.add_host_object(u)
    }

    fn obj_to_u128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok(*u as u64))
    }

    fn obj_to_u128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: U128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &u128| Ok((*u >> 64) as u64))
    }

    fn obj_from_i128_pieces(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        lo: u64,
        hi: u64,
    ) -> Result<I128Object, Self::Error> {
        // NB: always do assembly/disassembly as unsigned, to avoid sign extension.
        let u: u128 = ((hi as u128) << 64) | lo as u128;
        let i: i128 = u as i128;
        self.add_host_object(i)
    }

    fn obj_to_i128_lo64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &i128| Ok((*u as u128) as u64))
    }

    fn obj_to_i128_hi64(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        obj: I128Object,
    ) -> Result<u64, Self::Error> {
        self.visit_obj(obj, move |u: &i128| Ok(((*u as u128) >> 64) as u64))
    }

    fn map_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<MapObject, HostError> {
        self.add_host_object(HostMap::new(self)?)
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
            hm.get(&k, self)?
                .map(|v| *v)
                .ok_or_else(|| self.err_general("map key not found")) // FIXME: need error code
        })
    }

    fn map_del(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
        k: RawVal,
    ) -> Result<MapObject, HostError> {
        match self.visit_obj(m, |hm: &HostMap| hm.remove(&k, self))? {
            Some((mnew, _)) => Ok(self.add_host_object(mnew)?.into()),
            None => Err(self.err_general("map key not found")),
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
                Ok(Status::UNKNOWN_ERROR.to_raw())
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
                Ok(Status::UNKNOWN_ERROR.to_raw())
            }
        })
    }

    fn map_min_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            match hm.get_min(self)? {
                Some((pk, pv)) => Ok(*pk),
                None => Ok(Status::UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
            }
        })
    }

    fn map_max_key(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        m: MapObject,
    ) -> Result<RawVal, HostError> {
        self.visit_obj(m, |hm: &HostMap| {
            match hm.get_max(self)? {
                Some((pk, pv)) => Ok(*pk),
                None => Ok(Status::UNKNOWN_ERROR.to_raw()), //FIXME: replace with the actual status code
            }
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
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            // Step 1: extract all key symbols.
            let VmSlice {
                vm,
                pos: keys_pos,
                len,
            } = self.decode_vmslice(keys_pos, len)?;
            self.charge_budget(CostType::VecNew, len as u64)?;
            let mut key_syms: Vec<Symbol> = Vec::with_capacity(len as usize);
            self.metered_vm_scan_slices_in_linear_memory(
                vmcaller,
                &vm,
                keys_pos,
                len as usize,
                |n, slice| {
                    self.charge_budget(CostType::VmMemRead, slice.len() as u64)?;
                    let scsym = ScSymbol(slice.try_into()?);
                    let sym = Symbol::try_from(self.to_host_val(&ScVal::Symbol(scsym))?)?;
                    key_syms.push(sym);
                    Ok(())
                },
            )?;

            // Step 2: extract all val RawVals.
            let vals_pos: u32 = vals_pos.into();
            self.charge_budget(CostType::VecNew, len as u64)?;
            let mut vals: Vec<RawVal> = vec![RawVal::VOID.into(); len as usize];
            self.metered_vm_read_vals_from_linear_memory::<8, RawVal>(
                vmcaller,
                &vm,
                vals_pos,
                vals.as_mut_slice(),
                |buf| RawVal::from_payload(u64::from_le_bytes(buf.clone())),
            )?;

            // Step 3: turn pairs into a map.
            let pair_iter = key_syms
                .iter()
                .map(|s| s.to_raw())
                .zip(vals.iter().cloned());
            let map = HostMap::from_exact_iter(pair_iter, self)?;
            self.add_host_object(map)
        }
    }

    fn map_unpack_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        map: MapObject,
        keys_pos: U32Val,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
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
                                .ok_or(ScHostObjErrorCode::VecIndexOutOfBound)?
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
    }

    fn vec_new(&self, _vmcaller: &mut VmCaller<Host>, c: RawVal) -> Result<VecObject, HostError> {
        let capacity: usize = if c.is_void() {
            0
        } else {
            self.usize_from_rawval_u32_input("c", c)?
        };
        // TODO: optimize the vector based on capacity
        self.add_host_object(HostVec::new(self.budget_ref())?)
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
        Ok(self.add_host_object(vnew)?.into())
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
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop_front(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_front(self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_push_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: RawVal,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.push_back(x, self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_pop_back(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
    ) -> Result<VecObject, HostError> {
        let vnew = self.visit_obj(v, move |hv: &HostVec| hv.pop_back(self.as_budget()))?;
        Ok(self.add_host_object(vnew)?.into())
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
        Ok(self.add_host_object(vnew)?.into())
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
                    Err(self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow"))
                } else {
                    hv1.append(hv2, self.as_budget())
                }
            })
        })?;
        Ok(self.add_host_object(vnew)?.into())
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
        Ok(self.add_host_object(vnew)?.into())
    }

    fn vec_first_index_of(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        v: VecObject,
        x: RawVal,
    ) -> Result<RawVal, Self::Error> {
        self.visit_obj(v, |hv: &HostVec| {
            Ok(
                match hv.first_index_of(|other| self.compare(&x, other), &self.as_budget())? {
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
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(vals_pos, len)?;
            self.charge_budget(CostType::VecNew, len as u64)?;
            let mut vals: Vec<RawVal> = vec![RawVal::VOID.to_raw(); len as usize];
            self.metered_vm_read_vals_from_linear_memory::<8, RawVal>(
                vmcaller,
                &vm,
                pos,
                vals.as_mut_slice(),
                |buf| RawVal::from_payload(u64::from_le_bytes(buf.clone())),
            )?;
            self.add_host_object(HostVec::from_vec(vals, self.as_budget())?)
        }
    }

    fn vec_unpack_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        vec: VecObject,
        vals_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            let VmSlice { vm, pos, len } = self.decode_vmslice(vals_pos, len)?;
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
    }

    // Notes on metering: covered by components
    fn put_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
        v: RawVal,
    ) -> Result<RawVal, HostError> {
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
        Ok(().into())
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
            }) => Ok(self.to_host_val(&val)?.into()),
            _ => Err(self.err_status_msg(
                ScHostStorageErrorCode::ExpectContractData,
                "expected contract data",
            )),
        }
    }

    // Notes on metering: covered by components
    fn del_contract_data(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        k: RawVal,
    ) -> Result<RawVal, HostError> {
        let key = self.contract_data_key_from_rawval(k)?;
        self.0.storage.borrow_mut().del(&key, self.as_budget())?;
        Ok(().into())
    }

    // Notes on metering: covered by the components.
    fn create_contract_from_contract(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        wasm_hash: BytesObject,
        salt: BytesObject,
    ) -> Result<BytesObject, HostError> {
        let contract_id = self.get_current_contract_id_internal()?;
        let salt = self.uint256_from_bytesobj_input("salt", salt)?;

        let code =
            ScContractExecutable::WasmRef(self.hash_from_bytesobj_input("wasm_hash", wasm_hash)?);
        let id_preimage = self.id_preimage_from_contract(contract_id, salt)?;
        self.create_contract_with_id_preimage(code, id_preimage)
    }

    // Notes on metering: here covers the args unpacking. The actual VM work is changed at lower layers.
    fn call(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        contract: BytesObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<RawVal, HostError> {
        let args = self.call_args_from_obj(args)?;
        // this is the recommended path of calling a contract, with `reentry`
        // always set `ContractReentryMode::Prohibited`
        let res = self.call_n(
            contract,
            func,
            args.as_slice(),
            ContractReentryMode::Prohibited,
        );
        if let Err(e) = &res {
            let evt = DebugEvent::new()
                .msg("contract call invocation resulted in error {}")
                .arg::<RawVal>(e.status.into());
            self.record_debug_event(evt)?;
        }
        res
    }

    // Notes on metering: covered by the components.
    fn try_call(
        &self,
        vmcaller: &mut VmCaller<Host>,
        contract: BytesObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<RawVal, HostError> {
        let args = self.call_args_from_obj(args)?;
        // this is the "loosened" path of calling a contract.
        // TODO: A `reentry` flag will be passed from `try_call` into here.
        // For now, we are passing in `ContractReentryMode::Prohibited` to disable
        // reentry.
        let res = self.call_n(
            contract,
            func,
            args.as_slice(),
            ContractReentryMode::Prohibited,
        );
        match res {
            Ok(rv) => Ok(rv),
            Err(e) => {
                let status: RawVal = e.status.into();
                let evt = DebugEvent::new()
                    .msg("contract call invocation resulted in error {}")
                    .arg(status);
                self.record_debug_event(evt)?;
                Ok(status)
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
        Ok(self.add_host_object(self.scbytes_from_vec(buf)?)?.into())
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
        Ok(self.to_host_val(&scv)?.into())
    }

    fn string_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        s: StringObject,
        s_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            self.memobj_copy_to_linear_memory::<ScString>(vmcaller, s, s_pos, lm_pos, len)?;
            Ok(RawVal::VOID)
        }
    }

    fn symbol_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        s: SymbolObject,
        s_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            self.memobj_copy_to_linear_memory::<ScSymbol>(vmcaller, s, s_pos, lm_pos, len)?;
            Ok(RawVal::VOID)
        }
    }

    fn bytes_copy_to_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        b_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<Void, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            self.memobj_copy_to_linear_memory::<ScBytes>(vmcaller, b, b_pos, lm_pos, len)?;
            Ok(RawVal::VOID)
        }
    }

    fn bytes_copy_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        b_pos: U32Val,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<BytesObject, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
            self.memobj_copy_from_linear_memory::<ScBytes>(vmcaller, b, b_pos, lm_pos, len)
        }
    }

    fn bytes_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<BytesObject, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        self.memobj_new_from_linear_memory::<ScBytes>(vmcaller, lm_pos, len)
    }

    fn string_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<StringObject, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        self.memobj_new_from_linear_memory::<ScString>(vmcaller, lm_pos, len)
    }

    fn symbol_new_from_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<SymbolObject, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        self.memobj_new_from_linear_memory::<ScSymbol>(vmcaller, lm_pos, len)
    }

    fn symbol_index_in_linear_memory(
        &self,
        vmcaller: &mut VmCaller<Host>,
        sym: Symbol,
        lm_pos: U32Val,
        len: U32Val,
    ) -> Result<U32Val, HostError> {
        #[cfg(not(feature = "vm"))]
        unimplemented!();
        #[cfg(feature = "vm")]
        {
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
                None => Err(self.err_status(ScHostFnErrorCode::InputArgsInvalid)),
                Some(idx) => Ok(U32Val::from(idx)),
            }
        }
    }

    // Notes on metering: covered by `add_host_object`
    fn bytes_new(&self, _vmcaller: &mut VmCaller<Host>) -> Result<BytesObject, HostError> {
        Ok(self
            .add_host_object(self.scbytes_from_vec(Vec::<u8>::new())?)?
            .into())
    }

    // Notes on metering: `get_mut` is free
    fn bytes_put(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        i: U32Val,
        u: U32Val,
    ) -> Result<BytesObject, HostError> {
        let i: u32 = i.into();
        let u = self.u8_from_u32val_input("u", u)?;
        let vnew = self.visit_obj(b, move |hv: &ScBytes| {
            let mut vnew: Vec<u8> = hv.metered_clone(&self.0.budget)?.into();
            match vnew.get_mut(i as usize) {
                None => Err(self.err_status(ScHostObjErrorCode::VecIndexOutOfBound)),
                Some(v) => {
                    *v = u;
                    Ok(ScBytes(vnew.try_into()?))
                }
            }
        })?;
        Ok(self.add_host_object(vnew)?.into())
    }

    // Notes on metering: `get` is free
    fn bytes_get(
        &self,
        _vmcaller: &mut VmCaller<Host>,
        b: BytesObject,
        i: U32Val,
    ) -> Result<U32Val, HostError> {
        let i: u32 = i.into();
        self.visit_obj(b, |hv: &ScBytes| {
            hv.get(i as usize)
                .map(|u| Into::<U32Val>::into(Into::<u32>::into(*u)))
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
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
        Ok(self.add_host_object(vnew)?.into())
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
            let len = hv.len() + 1;
            metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            let mut vnew: Vec<u8> = Vec::with_capacity(len);
            vnew.extend_from_slice(hv.as_slice());
            vnew.push(u);
            Ok(ScBytes(vnew.try_into()?))
        })?;
        Ok(self.add_host_object(vnew)?.into())
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
                return Err(self.err_status(ScHostObjErrorCode::VecIndexOutOfBound));
            }
            Ok(ScBytes(vnew.try_into()?))
        })?;
        Ok(self.add_host_object(vnew)?.into())
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
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
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
                .ok_or_else(|| self.err_status(ScHostObjErrorCode::VecIndexOutOfBound))
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
            let len = hv.len() + 1;
            metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            let mut vnew: Vec<u8> = Vec::with_capacity(len);
            vnew.extend_from_slice(hv.as_slice());
            // insert will cause the memcpy by shifting all the values at and after `i`.
            metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
            vnew.insert(i as usize, u);
            Ok(ScBytes(vnew.try_into()?))
        })?;
        Ok(self.add_host_object(vnew)?.into())
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
                    return Err(
                        self.err_status_msg(ScHostFnErrorCode::InputArgsInvalid, "u32 overflow")
                    );
                }
                // we allocate large enough memory to hold the new combined vector, so that allocation
                // only happens once, and charge for it upfront.
                let len = sb1.len() + sb2.len();
                metered_clone::charge_heap_alloc::<u8>(len as u64, self.as_budget())?;
                metered_clone::charge_shallow_copy::<u8>(len as u64, self.as_budget())?;
                let mut vnew: Vec<u8> = Vec::with_capacity(len);
                vnew.extend_from_slice(sb1.as_slice());
                vnew.extend_from_slice(sb2.as_slice());
                Ok(vnew)
            })
        })?;
        Ok(self.add_host_object(ScBytes(vnew.try_into()?))?.into())
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
        x: BytesObject,
        k: BytesObject,
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
        Ok(self
            .with_ledger_info(|li| {
                self.add_host_object(self.scbytes_from_slice(li.network_id.as_slice())?)
            })?
            .into())
    }

    fn get_current_call_stack(
        &self,
        _vmcaller: &mut VmCaller<Host>,
    ) -> Result<VecObject, HostError> {
        let frames = self.0.context.borrow();

        let get_host_val_tuple = |id: &Hash, function: &Symbol| -> Result<[RawVal; 2], HostError> {
            let id_val = self.add_host_object(self.scbytes_from_hash(id)?)?.into();
            let function_val = function.clone().into();
            Ok([id_val, function_val])
        };

        let mut outer = Vec::with_capacity(frames.len());
        for frame in frames.iter() {
            let vals = match frame {
                #[cfg(feature = "vm")]
                Frame::ContractVM(vm, function, _) => {
                    get_host_val_tuple(&vm.contract_id, &function)?
                }
                Frame::HostFunction(_) => continue,
                Frame::Token(id, function, _) => get_host_val_tuple(&id, &function)?,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => get_host_val_tuple(&tc.id, &tc.func)?,
            };
            let inner = MeteredVector::from_array(&vals, self.as_budget())?;
            outer.push(self.add_host_object(inner)?.into());
        }
        Ok(self
            .add_host_object(HostVec::from_vec(outer, self.as_budget())?)?
            .into())
    }

    fn fail_with_status(
        &self,
        vmcaller: &mut VmCaller<Self::VmUserState>,
        status: Status,
    ) -> Result<Void, Self::Error> {
        if status.is_type(ScStatusType::ContractError) {
            Err(self.err_status_msg(status, "failing with contract error status code '{}'"))
        } else {
            Err(self.err_status_msg(
                ScHostValErrorCode::UnexpectedValType,
                "contract attempted to fail with non-ContractError status code",
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
        let addr = self.visit_obj(address, |addr: &ScAddress| Ok(addr.clone()))?;

        Ok(self
            .0
            .authorization_manager
            .borrow_mut()
            .require_auth(
                self,
                address.get_handle(),
                addr,
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
                #[cfg(feature = "vm")]
                Frame::ContractVM(_, _, args) => args,
                Frame::HostFunction(_) => {
                    return Err(self.err_general("require_auth is not suppported for host fns"))
                }
                Frame::Token(_, _, args) => args,
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(c) => &c.args,
            };
            Ok(self.rawvals_to_scvec(args.iter())?)
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
}

#[cfg(any(test, feature = "testutils"))]
mod testutils {
    use crate::RawVal;
    use std::any::Any;
    use std::cell::Cell;
    use std::panic::UnwindSafe;
    use std::panic::{catch_unwind, set_hook, take_hook};
    use std::sync::Once;

    type PanicVal = Box<dyn Any + Send>;

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
    pub fn call_with_suppressed_panic_hook<C>(closure: C) -> Result<Option<RawVal>, PanicVal>
    where
        C: FnOnce() -> Option<RawVal> + UnwindSafe,
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

        let res: Result<Option<RawVal>, PanicVal> = catch_unwind(closure);

        TEST_CONTRACT_CALL_COUNT.with(|c| {
            let old_count = c.get();
            let new_count = old_count.checked_sub(1).expect("overflow");
            c.set(new_count);
        });

        res
    }
}
