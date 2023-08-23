use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use rand::Rng;
use soroban_env_common::xdr::{
    ContractDataEntry, CreateContractArgs, HashIdPreimage, HashIdPreimageSorobanAuthorization,
    InvokeContractArgs, LedgerEntry, LedgerEntryData, LedgerEntryExt, ScAddress, ScErrorCode,
    ScErrorType, ScNonceKey, ScVal, SorobanAuthorizationEntry, SorobanAuthorizedFunction,
    SorobanCredentials,
};
use soroban_env_common::{AddressObject, Compare, Symbol, TryFromVal, TryIntoVal, Val, VecObject};

use crate::budget::{AsBudget, Budget};
use crate::host::error::TryBorrowOrErr;
use crate::host::metered_clone::{MeteredAlloc, MeteredClone, MeteredContainer, MeteredIterator};
use crate::host::Frame;
use crate::host_object::HostVec;
use crate::native_contract::account_contract::{
    check_account_authentication, check_account_contract_auth,
};
use crate::native_contract::invoker_contract_auth::invoker_contract_auth_to_authorized_invocation;
use crate::{Host, HostError};

use super::xdr;
use super::xdr::Hash;

// Authorization manager encapsulates host-based authentication & authorization
// framework.
// This supports enforcing authentication & authorization of the contract
// invocation trees as well as recording the authorization requirements in
// simulated environments (such as tests or preflight).
#[derive(Clone)]
pub struct AuthorizationManager {
    // Mode of operation of this AuthorizationManager. This can't be changed; in
    // order to switch the mode a new instance of AuthorizationManager has to
    // be created.
    mode: AuthorizationMode,
    // Per-address trackers of authorized invocations.
    // Every tracker takes care about a single rooted invocation tree for some
    // address. There can be multiple trackers per address.
    // The internal structure of this field is build in such a way that trackers
    // can be borrowed mutably independently, while still allowing for
    // modification of the `account_trackers` vec itself.
    account_trackers: RefCell<Vec<RefCell<AccountAuthorizationTracker>>>,
    // Per-address trackers for authorization performed by the contracts at
    // execution time (as opposed to signature-based authorization for accounts).
    // Contract authorizations are always enforced independently of the `mode`,
    // as they are self-contained and fully defined by the contract logic.
    invoker_contract_trackers: RefCell<Vec<InvokerContractAuthorizationTracker>>,
    // Current call stack consisting only of the contract invocations (i.e. not
    // the host functions).
    call_stack: RefCell<Vec<AuthStackFrame>>,
}

macro_rules! impl_checked_borrow_helpers {
    ($field:ident, $t:ty, $borrow:ident, $borrow_mut:ident) => {
        impl AuthorizationManager {
            #[allow(dead_code)]
            fn $borrow(&self, host: &Host) -> Result<std::cell::Ref<'_, $t>, HostError> {
                use crate::host::error::TryBorrowOrErr;
                self.$field.try_borrow_or_err_with(
                    host,
                    concat!(
                        "authorization_manager.",
                        stringify!($field),
                        ".try_borrow failed"
                    ),
                )
            }
            fn $borrow_mut(&self, host: &Host) -> Result<std::cell::RefMut<'_, $t>, HostError> {
                use crate::host::error::TryBorrowOrErr;
                self.$field.try_borrow_mut_or_err_with(
                    host,
                    concat!(
                        "authorization_manager.",
                        stringify!($field),
                        ".try_borrow_mut failed"
                    ),
                )
            }
        }
    };
}

impl_checked_borrow_helpers!(
    account_trackers,
    Vec<RefCell<AccountAuthorizationTracker>>,
    try_borrow_account_trackers,
    try_borrow_account_trackers_mut
);

impl_checked_borrow_helpers!(
    invoker_contract_trackers,
    Vec<InvokerContractAuthorizationTracker>,
    try_borrow_invoker_contract_trackers,
    try_borrow_invoker_contract_trackers_mut
);

impl_checked_borrow_helpers!(
    call_stack,
    Vec<AuthStackFrame>,
    try_borrow_call_stack,
    try_borrow_call_stack_mut
);

// The authorization payload recorded for an address in the recording
// authorization mode.
#[derive(Debug)]
pub struct RecordedAuthPayload {
    pub address: Option<ScAddress>,
    pub nonce: Option<i64>,
    pub invocation: xdr::SorobanAuthorizedInvocation,
}

// Snapshot of `AuthorizationManager` to use when performing the callstack
// rollbacks.
pub struct AuthorizationManagerSnapshot {
    account_trackers_snapshot: AccountTrackersSnapshot,
    invoker_contract_tracker_root_snapshots: Vec<AuthorizedInvocationSnapshot>,
    // This is set only for the recording mode.
    tracker_by_address_handle: Option<HashMap<u32, usize>>,
}

// Snapshot of the `account_trackers` in `AuthorizationManager`.
enum AccountTrackersSnapshot {
    // In enforcing mode we only need to snapshot the mutable part of the
    // trackers.
    // `None` means that the tracker is currently in authentication process and
    // shouldn't be modified (as the tracker can't be used to authenticate
    // itself).
    Enforcing(Vec<Option<AccountAuthorizationTrackerSnapshot>>),
    // In recording mode snapshot the whole vector, as we create trackers
    // lazily and hence the outer vector itself might change.
    Recording(Vec<RefCell<AccountAuthorizationTracker>>),
}

#[derive(Clone)]
enum AuthorizationMode {
    Enforcing,
    Recording(RecordingAuthInfo),
}

// Additional AuthorizationManager fields needed only for the recording mode.
#[derive(Clone)]
struct RecordingAuthInfo {
    // Maps the `Address` object identifiers to the respective tracker indices
    // in `trackers`
    // This allows to disambiguate between the addresses that have the same
    // value, but are specified as two different objects (e.g. as two different
    // contract function inputs).
    tracker_by_address_handle: RefCell<HashMap<u32, usize>>,
    // Whether to allow root authorized invocation to not match the root
    // contract invocation.
    disable_non_root_auth: bool,
}

impl RecordingAuthInfo {
    fn try_borrow_tracker_by_address_handle(
        &self,
        host: &Host,
    ) -> Result<std::cell::Ref<'_, HashMap<u32, usize>>, HostError> {
        self.tracker_by_address_handle.try_borrow_or_err_with(
            host,
            "recording_auth_info.tracker_by_address_handle.try_borrow failed",
        )
    }
    fn try_borrow_tracker_by_address_handle_mut(
        &self,
        host: &Host,
    ) -> Result<std::cell::RefMut<'_, HashMap<u32, usize>>, HostError> {
        self.tracker_by_address_handle.try_borrow_mut_or_err_with(
            host,
            "recording_auth_info.tracker_by_address_handle.try_borrow_mut failed",
        )
    }
}

// Helper for matching the 'sparse' tree of authorized invocations to the actual
// call tree.
#[derive(Clone)]
struct InvocationTracker {
    // Root of the authorized invocation tree.
    // The authorized invocation tree only contains the contract invocations
    // that explicitly require authorization on behalf of the address.
    root_authorized_invocation: AuthorizedInvocation,
    // Call stack that tracks the current walk in the tree of authorized
    // invocations.
    // This is set to `None` if the invocation didn't require authorization on
    // behalf of the address.
    // When not `None` this is an index of the authorized invocation in the
    // parent's `sub_invocations` vector or 0 for the
    // `root_authorized_invocation`.
    invocation_id_in_call_stack: Vec<Option<usize>>,
    // If root invocation is exhuasted, the index of the stack frame where it
    // was exhausted (i.e. index in `invocation_id_in_call_stack`).
    root_exhausted_frame: Option<usize>,
    // Indicates whether this tracker is fully processed, i.e. the authorized
    // root frame has been exhausted and then popped from the stack.
    is_fully_processed: bool,
}

// Stores all the authorizations that are authorized by an address.
// In the enforcing mode this performs authentication and makes sure that only
// pre-authorized invocations can happen on behalf of the `address`.
// In the recording mode this will record the invocations that are authorized
// on behalf of the address.
#[derive(Clone)]
pub(crate) struct AccountAuthorizationTracker {
    // Tracked address.
    address: AddressObject,
    // Helper for matching the tree that address authorized to the invocation
    // tree.
    invocation_tracker: InvocationTracker,
    // Value representing the signature created by the address to authorize
    // the invocations tracked here.
    signature: Val,
    // Indicates whether this tracker is still valid. If invalidated once, this
    // can't be used to authorize anything anymore
    is_valid: bool,
    // Indicates whether this is a tracker for the transaction invoker.
    is_invoker: bool,
    // Indicates whether authentication has already succesfully happened.
    authenticated: bool,
    // Indicates whether nonce still needs to be verified and consumed.
    need_nonce: bool,
    // The value of nonce authorized by the address with its expiration ledger.
    // Must not exist in the ledger.
    nonce: Option<(i64, u32)>,
}

pub(crate) struct AccountAuthorizationTrackerSnapshot {
    invocation_tracker_root_snapshot: AuthorizedInvocationSnapshot,
    authenticated: bool,
    need_nonce: bool,
}

// Stores all the authorizations peformed by contracts at runtime.
#[derive(Clone)]
pub(crate) struct InvokerContractAuthorizationTracker {
    contract_address: AddressObject,
    invocation_tracker: InvocationTracker,
}

#[derive(Clone)]
pub(crate) enum AuthStackFrame {
    Contract(ContractInvocation),
    CreateContractHostFn(CreateContractArgs),
}

#[derive(Clone)]
pub(crate) struct ContractInvocation {
    pub(crate) contract_address: AddressObject,
    pub(crate) function_name: Symbol,
}

#[derive(Clone)]
pub(crate) struct ContractFunction {
    pub(crate) contract_address: AddressObject,
    pub(crate) function_name: Symbol,
    pub(crate) args: Vec<Val>,
}

#[derive(Clone)]
pub(crate) enum AuthorizedFunction {
    ContractFn(ContractFunction),
    CreateContractHostFn(CreateContractArgs),
}

// A single node in the authorized invocation tree.
// This represents an invocation and all it's authorized sub-invocations.
#[derive(Clone)]
pub(crate) struct AuthorizedInvocation {
    pub(crate) function: AuthorizedFunction,
    pub(crate) sub_invocations: Vec<AuthorizedInvocation>,
    // Indicates that this invocation has been already used in the
    // enforcing mode. Exhausted authorizations can't be reused.
    // In the recording mode this is immediately set to `true` (as the
    // authorizations are recorded when they actually happen).
    is_exhausted: bool,
}

// Snapshot of `AuthorizedInvocation` that contains only mutable fields.
pub(crate) struct AuthorizedInvocationSnapshot {
    is_exhausted: bool,
    sub_invocations: Vec<AuthorizedInvocationSnapshot>,
}

impl Compare<ContractFunction> for Host {
    type Error = HostError;

    // metering: covered by host
    fn compare(
        &self,
        a: &ContractFunction,
        b: &ContractFunction,
    ) -> Result<std::cmp::Ordering, Self::Error> {
        let ord = self.compare(&a.contract_address, &b.contract_address)?;
        if !ord.is_eq() {
            return Ok(ord);
        }
        let ord = self.compare(&a.function_name, &b.function_name)?;
        if !ord.is_eq() {
            return Ok(ord);
        }
        self.compare(&a.args, &b.args)
    }
}

impl Compare<AuthorizedFunction> for Host {
    type Error = HostError;

    // metering: covered by components
    fn compare(
        &self,
        a: &AuthorizedFunction,
        b: &AuthorizedFunction,
    ) -> Result<std::cmp::Ordering, Self::Error> {
        match (a, b) {
            (AuthorizedFunction::ContractFn(f1), AuthorizedFunction::ContractFn(f2)) => {
                self.compare(f1, f2)
            }
            (
                AuthorizedFunction::CreateContractHostFn(c1),
                AuthorizedFunction::CreateContractHostFn(c2),
            ) => self.compare(c1, c2),
            (AuthorizedFunction::ContractFn(_), AuthorizedFunction::CreateContractHostFn(_)) => {
                Ok(std::cmp::Ordering::Less)
            }
            (AuthorizedFunction::CreateContractHostFn(_), AuthorizedFunction::ContractFn(_)) => {
                Ok(std::cmp::Ordering::Greater)
            }
        }
    }
}

impl AuthStackFrame {
    // metering: covered
    fn to_authorized_function(
        &self,
        host: &Host,
        args: Vec<Val>,
    ) -> Result<AuthorizedFunction, HostError> {
        match self {
            AuthStackFrame::Contract(contract_frame) => {
                Ok(AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: contract_frame.contract_address,
                    function_name: contract_frame.function_name.metered_clone(host)?,
                    args,
                }))
            }
            AuthStackFrame::CreateContractHostFn(args) => Ok(
                AuthorizedFunction::CreateContractHostFn(args.metered_clone(host)?),
            ),
        }
    }
}

impl AuthorizedFunction {
    // metering: covered by the host
    fn from_xdr(host: &Host, xdr_fn: SorobanAuthorizedFunction) -> Result<Self, HostError> {
        Ok(match xdr_fn {
            SorobanAuthorizedFunction::ContractFn(xdr_contract_fn) => {
                AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: host.add_host_object(xdr_contract_fn.contract_address)?,
                    function_name: Symbol::try_from_val(host, &xdr_contract_fn.function_name)?,
                    args: host.scvals_to_rawvals(xdr_contract_fn.args.as_slice())?,
                })
            }
            SorobanAuthorizedFunction::CreateContractHostFn(xdr_args) => {
                AuthorizedFunction::CreateContractHostFn(xdr_args)
            }
        })
    }

    // metering: covered by the host
    fn to_xdr(&self, host: &Host) -> Result<SorobanAuthorizedFunction, HostError> {
        match self {
            AuthorizedFunction::ContractFn(contract_fn) => {
                let function_name_sc_val = host.from_host_val(contract_fn.function_name.into())?;
                let function_name = if let ScVal::Symbol(s) = function_name_sc_val {
                    s
                } else {
                    return Err(host.err(
                        ScErrorType::Object,
                        ScErrorCode::InternalError,
                        "unexpected non-symbol function name",
                        &[],
                    ));
                };
                Ok(SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                    contract_address: host.scaddress_from_address(contract_fn.contract_address)?,
                    function_name,
                    args: host.rawvals_to_sc_val_vec(contract_fn.args.as_slice())?,
                }))
            }
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => {
                Ok(SorobanAuthorizedFunction::CreateContractHostFn(
                    create_contract_args.metered_clone(host)?,
                ))
            }
        }
    }

    // metering: free
    fn to_xdr_non_metered(&self, host: &Host) -> Result<xdr::SorobanAuthorizedFunction, HostError> {
        match self {
            AuthorizedFunction::ContractFn(contract_fn) => {
                Ok(SorobanAuthorizedFunction::ContractFn(InvokeContractArgs {
                    contract_address: host
                        .visit_obj(contract_fn.contract_address, |addr: &ScAddress| {
                            Ok(addr.clone())
                        })?,
                    function_name: contract_fn.function_name.try_into_val(host)?,
                    // why is this intentionally non-metered? the visit_obj above is metered.
                    args: host.rawvals_to_sc_val_vec_non_metered(contract_fn.args.as_slice())?,
                }))
            }
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => Ok(
                SorobanAuthorizedFunction::CreateContractHostFn(create_contract_args.clone()),
            ),
        }
    }
}

impl AuthorizedInvocation {
    // metering: covered
    fn from_xdr(
        host: &Host,
        xdr_invocation: xdr::SorobanAuthorizedInvocation,
    ) -> Result<Self, HostError> {
        let sub_invocations_xdr = xdr_invocation.sub_invocations.into_vec();
        let sub_invocations = sub_invocations_xdr
            .into_iter()
            .map(|a| AuthorizedInvocation::from_xdr(host, a))
            .metered_collect::<Result<Vec<_>, _>>(host)??;
        Ok(Self {
            function: AuthorizedFunction::from_xdr(host, xdr_invocation.function)?,
            sub_invocations,
            is_exhausted: false,
        })
    }

    // metering: free
    pub(crate) fn new(
        function: AuthorizedFunction,
        sub_invocations: Vec<AuthorizedInvocation>,
    ) -> Self {
        Self {
            function,
            sub_invocations,
            is_exhausted: false,
        }
    }

    // metering: free
    fn new_recording(function: AuthorizedFunction) -> Self {
        Self {
            function,
            sub_invocations: vec![],
            is_exhausted: true,
        }
    }

    // metering: covered
    fn to_xdr(&self, host: &Host) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        Ok(xdr::SorobanAuthorizedInvocation {
            function: self.function.to_xdr(host)?,
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr(host))
                .metered_collect::<Result<Vec<xdr::SorobanAuthorizedInvocation>, HostError>>(host)??
                .try_into()
                .map_err(|_| HostError::from((ScErrorType::Auth, ScErrorCode::InternalError)))?,
        })
    }

    // Non-metered conversion should only be used for the recording preflight
    // runs or testing.
    // metering: free
    fn to_xdr_non_metered(
        &self,
        host: &Host,
        exhausted_sub_invocations_only: bool,
    ) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        Ok(xdr::SorobanAuthorizedInvocation {
            function: self.function.to_xdr_non_metered(host)?,
            sub_invocations: self
                .sub_invocations
                .iter()
                .filter(|i| i.is_exhausted || !exhausted_sub_invocations_only)
                .map(|i| i.to_xdr_non_metered(host, exhausted_sub_invocations_only))
                .collect::<Result<Vec<xdr::SorobanAuthorizedInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from((ScErrorType::Auth, ScErrorCode::InternalError)))?,
        })
    }

    // Walks a path in the tree defined by `invocation_id_in_call_stack` and
    // returns the last visited authorized node.
    // metering: free
    fn last_authorized_invocation_mut(
        &mut self,
        invocation_id_in_call_stack: &Vec<Option<usize>>,
        call_stack_id: usize,
    ) -> &mut AuthorizedInvocation {
        // Start walking the stack from `call_stack_id`. We trust the callers to
        // hold the invariant that `invocation_id_in_call_stack[call_stack_id - 1]`
        // corresponds to this invocation tree, so that the next non-`None` child
        // corresponds to the child of the current tree.
        for (i, id) in invocation_id_in_call_stack
            .iter()
            .enumerate()
            .skip(call_stack_id)
        {
            if let Some(id) = id {
                // We trust the caller to have the correct sub-invocation
                // indices.
                return self.sub_invocations[*id]
                    .last_authorized_invocation_mut(invocation_id_in_call_stack, i + 1);
            }
            // Skip `None` invocations as they don't require authorization.
        }
        self
    }

    // metering: covered
    fn snapshot(&self, budget: &Budget) -> Result<AuthorizedInvocationSnapshot, HostError> {
        Ok(AuthorizedInvocationSnapshot {
            is_exhausted: self.is_exhausted,
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.snapshot(budget))
                .metered_collect::<Result<Vec<AuthorizedInvocationSnapshot>, HostError>>(
                    budget,
                )??,
        })
    }

    // metering: free
    fn rollback(&mut self, snapshot: &AuthorizedInvocationSnapshot) -> Result<(), HostError> {
        self.is_exhausted = snapshot.is_exhausted;
        if self.sub_invocations.len() != snapshot.sub_invocations.len() {
            // This would be a bug.
            return Err((ScErrorType::Auth, ScErrorCode::InternalError).into());
        }
        for (i, sub_invocation) in self.sub_invocations.iter_mut().enumerate() {
            sub_invocation.rollback(&snapshot.sub_invocations[i])?;
        }
        Ok(())
    }
}

impl Default for AuthorizationManager {
    fn default() -> Self {
        Self::new_enforcing_without_authorizations()
    }
}

impl AuthorizationManager {
    // Creates a new enforcing `AuthorizationManager` from the given
    // authorization entries.
    // This should be created once per top-level invocation.
    // metering: covered
    pub(crate) fn new_enforcing(
        host: &Host,
        auth_entries: Vec<SorobanAuthorizationEntry>,
    ) -> Result<Self, HostError> {
        Vec::<AccountAuthorizationTracker>::charge_bulk_init_cpy(auth_entries.len() as u64, host)?;
        let mut trackers = Vec::with_capacity(auth_entries.len());
        for auth_entry in auth_entries {
            trackers.push(RefCell::new(
                AccountAuthorizationTracker::from_authorization_entry(host, auth_entry)?,
            ));
        }
        Ok(Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: RefCell::new(vec![]),
            account_trackers: RefCell::new(trackers),
            invoker_contract_trackers: RefCell::new(vec![]),
        })
    }

    // Creates a new enforcing `AuthorizationManager` that doesn't allow any
    // authorizations.
    // This is useful as a safe default mode.
    // metering: free
    pub(crate) fn new_enforcing_without_authorizations() -> Self {
        Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: RefCell::new(vec![]),
            account_trackers: RefCell::new(vec![]),
            invoker_contract_trackers: RefCell::new(vec![]),
        }
    }

    // Creates a new recording `AuthorizationManager`.
    // All the authorization requirements will be recorded and can then be
    // retrieved using `get_recorded_auth_payloads`.
    // metering: free
    pub(crate) fn new_recording(disable_non_root_auth: bool) -> Self {
        Self {
            mode: AuthorizationMode::Recording(RecordingAuthInfo {
                tracker_by_address_handle: Default::default(),
                disable_non_root_auth,
            }),
            call_stack: RefCell::new(vec![]),
            account_trackers: RefCell::new(vec![]),
            invoker_contract_trackers: RefCell::new(vec![]),
        }
    }

    // Require the `address` to have authorized the current contract invocation
    // with provided args and within the current context (i.e. the current
    // authorized call stack and for the current network).
    // In the recording mode this stores the auth requirement instead of
    // verifying it.
    // metering: covered
    pub(crate) fn require_auth(
        &self,
        host: &Host,
        address: AddressObject,
        args: Vec<Val>,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("require auth");
        let authorized_function = self
            .try_borrow_call_stack(host)?
            .last()
            .ok_or_else(|| {
                host.err(
                    ScErrorType::Auth,
                    ScErrorCode::InternalError,
                    "unexpected require_auth outside of valid frame",
                    &[],
                )
            })?
            .to_authorized_function(host, args)?;

        self.require_auth_internal(host, address, authorized_function)
    }

    // metering: covered
    pub(crate) fn add_invoker_contract_auth(
        &self,
        host: &Host,
        auth_entries: VecObject,
    ) -> Result<(), HostError> {
        let auth_entries =
            host.visit_obj(auth_entries, |e: &HostVec| e.to_vec(host.budget_ref()))?;
        let mut trackers = self.try_borrow_invoker_contract_trackers_mut(host)?;
        Vec::<Val>::charge_bulk_init_cpy(auth_entries.len() as u64, host)?;
        trackers.reserve(auth_entries.len());
        for e in auth_entries {
            trackers.push(InvokerContractAuthorizationTracker::new(host, e)?)
        }
        Ok(())
    }

    // metering: covered by components
    fn verify_contract_invoker_auth(
        &self,
        host: &Host,
        address: AddressObject,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        {
            let call_stack = self.try_borrow_call_stack(host)?;
            // If stack has just one call there can't be invoker.
            if call_stack.len() < 2 {
                return Ok(false);
            }

            // Try matching the direct invoker contract first. It is considered to
            // have authorized any direct calls.
            let invoker_frame = &call_stack[call_stack.len() - 2];
            if let AuthStackFrame::Contract(invoker_contract) = invoker_frame {
                if host
                    .compare(&invoker_contract.contract_address, &address)?
                    .is_eq()
                {
                    return Ok(true);
                }
            }
        }
        let mut invoker_contract_trackers = self.try_borrow_invoker_contract_trackers_mut(host)?;
        // If there is no direct invoker, there still might be a valid
        // sub-contract call authorization from another invoker higher up the
        // stack. Note, that invoker contract trackers consider the direct frame
        // to never require auth (any `require_auth` calls would be matched by
        // logic above).
        for tracker in invoker_contract_trackers.iter_mut() {
            if host.compare(&tracker.contract_address, &address)?.is_eq()
                && tracker.maybe_authorize_invocation(host, function)?
            {
                return Ok(true);
            }
        }

        return Ok(false);
    }

    // metering: covered by components
    fn require_auth_enforcing(
        &self,
        host: &Host,
        address: AddressObject,
        function: &AuthorizedFunction,
    ) -> Result<(), HostError> {
        // Find if there is already an active tracker for this address that has
        // not been matched for the current frame. If there is such tracker,
        // this authorization has to be matched with an already active tracker.
        // This prevents matching sets of disjoint authorization entries to
        // a tree of calls.
        let mut has_active_tracker = false;
        for tracker in self.try_borrow_account_trackers(host)?.iter() {
            if let Ok(tracker) = tracker.try_borrow() {
                // If address doesn't match, just skip the tracker.
                if host.compare(&tracker.address, &address)?.is_eq()
                    && tracker.is_active()
                    && !tracker.current_frame_is_already_matched()
                {
                    has_active_tracker = true;
                    break;
                }
            }
        }

        // Iterate all the trackers and try to find one that
        // fullfills the authorization requirement.
        for tracker in self.try_borrow_account_trackers(host)?.iter() {
            // Tracker can only be borrowed by the authorization manager itself.
            // The only scenario in which re-borrow might occur is when
            // `require_auth` is called within `__check_auth` call. The tracker
            // that called `__check_auth` would be already borrowed in such
            // scenario.
            // We allow such call patterns in general, but we don't allow using
            // tracker to verify auth for itself, i.e. we don't allow something
            // like address.require_auth()->address_contract.__check_auth()
            // ->address.require_auth(). Thus we simply skip the trackers that
            // have already been borrowed.
            if let Ok(mut tracker) = tracker.try_borrow_mut() {
                // If address doesn't match, just skip the tracker.
                if !host.compare(&tracker.address, &address)?.is_eq() {
                    continue;
                }
                match tracker.maybe_authorize_invocation(host, function, !has_active_tracker) {
                    // If tracker doesn't have a matching invocation,
                    // just skip it (there could still be another
                    // tracker  that matches it).
                    Ok(false) => continue,
                    // Found a matching authorization.
                    Ok(true) => return Ok(()),
                    // Found a matching authorization, but another
                    // requirement hasn't been fullfilled (for
                    // example, incorrect authentication or nonce).
                    Err(e) => return Err(e),
                }
            }
        }
        // No matching tracker found, hence the invocation isn't
        // authorized.
        Err(host.err(
            ScErrorType::Auth,
            ScErrorCode::InvalidAction,
            "Unauthorized function call for address",
            &[address.to_val()],
        ))
    }

    // metering: covered
    fn require_auth_internal(
        &self,
        host: &Host,
        address: AddressObject,
        function: AuthorizedFunction,
    ) -> Result<(), HostError> {
        // For now we give a blanket approval of the invoker contract to any
        // calls it made, but never to the deeper calls. It's possible
        // to eventually add a capability to pre-authorize arbitrary call
        // stacks on behalf of the contract.
        if self.verify_contract_invoker_auth(host, address, &function)? {
            return Ok(());
        }

        match &self.mode {
            AuthorizationMode::Enforcing => self.require_auth_enforcing(host, address, &function),
            // metering: free for recording
            AuthorizationMode::Recording(recording_info) => {
                let address_obj_handle = address.get_handle();
                let existing_tracker_id = recording_info
                    .try_borrow_tracker_by_address_handle(host)?
                    .get(&address_obj_handle)
                    .copied();
                if let Some(tracker_id) = existing_tracker_id {
                    // The tracker should not be borrowed recursively in
                    // recording mode, as we don't call `__check_auth` in this
                    // flow.
                    if let Ok(mut tracker) =
                        self.try_borrow_account_trackers(host)?[tracker_id].try_borrow_mut()
                    {
                        // The recording invariant is that trackers are created
                        // with the first authorized invocation, which means
                        // that when their stack no longer has authorized
                        // invocation, then we've popped frames past its root
                        // and hence need to create a new tracker.
                        if !tracker.has_authorized_invocations_in_stack() {
                            recording_info
                                .try_borrow_tracker_by_address_handle_mut(host)?
                                .remove(&address_obj_handle);
                        } else {
                            return tracker.record_invocation(host, function);
                        }
                    } else {
                        return Err(host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected recursive tracker borrow in recording mode",
                            &[],
                        ));
                    }
                }
                if recording_info.disable_non_root_auth
                    && self.try_borrow_call_stack(host)?.len() != 1
                {
                    return Err(host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InvalidAction,
                        "[recording authorization only] encountered authorization not tied \
                        to the root contract invocation for an address. Use `require_auth()` \
                        in the top invocation or enable non-root authorization.",
                        &[address.into()],
                    ));
                }
                // If a tracker for the new tree doesn't exist yet, create
                // it and initialize with the current invocation.
                self.try_borrow_account_trackers_mut(host)?
                    .push(RefCell::new(AccountAuthorizationTracker::new_recording(
                        host,
                        address,
                        function,
                        self.try_borrow_call_stack(host)?.len(),
                    )?));
                recording_info
                    .try_borrow_tracker_by_address_handle_mut(host)?
                    .insert(
                        address_obj_handle,
                        self.try_borrow_account_trackers(host)?.len() - 1,
                    );
                Ok(())
            }
        }
    }

    // Returns a snapshot of `AuthorizationManager` to use for rollback.
    // metering: covered
    pub(crate) fn snapshot(&self, host: &Host) -> Result<AuthorizationManagerSnapshot, HostError> {
        let _span = tracy_span!("snapshot auth");
        let account_trackers_snapshot = match &self.mode {
            AuthorizationMode::Enforcing => {
                let len = self.try_borrow_account_trackers(host)?.len();
                let mut snapshots = Vec::with_capacity(len);
                Vec::<Option<AccountAuthorizationTrackerSnapshot>>::charge_bulk_init_cpy(
                    len as u64, host,
                )?;
                for t in self.try_borrow_account_trackers(host)?.iter() {
                    let sp = if let Ok(tracker) = t.try_borrow() {
                        Some(tracker.snapshot(host.as_budget())?)
                    } else {
                        // If tracker is borrowed, snapshotting it is a no-op
                        // (it can't change until we release it higher up the
                        // stack).
                        None
                    };
                    snapshots.push(sp);
                }
                AccountTrackersSnapshot::Enforcing(snapshots)
            }
            AuthorizationMode::Recording(_) => {
                // All trackers should be avaialable to borrow for copy as in
                // recording mode we can't have recursive authorization.
                // metering: free for recording
                AccountTrackersSnapshot::Recording(self.try_borrow_account_trackers(host)?.clone())
            }
        };
        let invoker_contract_tracker_root_snapshots = self
            .try_borrow_invoker_contract_trackers(host)?
            .iter()
            .map(|t| {
                t.invocation_tracker
                    .root_authorized_invocation
                    .snapshot(host.as_budget())
            })
            .metered_collect::<Result<Vec<AuthorizedInvocationSnapshot>, HostError>>(host)??;
        let tracker_by_address_handle = match &self.mode {
            AuthorizationMode::Enforcing => None,
            AuthorizationMode::Recording(recording_info) => Some(
                // metering: free for recording
                recording_info
                    .try_borrow_tracker_by_address_handle(host)?
                    .clone(),
            ),
        };
        Ok(AuthorizationManagerSnapshot {
            account_trackers_snapshot,
            invoker_contract_tracker_root_snapshots,
            tracker_by_address_handle,
        })
    }

    // Rolls back this `AuthorizationManager` to the snapshot state.
    // metering: covered
    pub(crate) fn rollback(
        &self,
        host: &Host,
        snapshot: AuthorizationManagerSnapshot,
    ) -> Result<(), HostError> {
        let _span = tracy_span!("rollback auth");
        match snapshot.account_trackers_snapshot {
            AccountTrackersSnapshot::Enforcing(trackers_snapshot) => {
                let trackers = self.try_borrow_account_trackers(host)?;
                if trackers.len() != trackers_snapshot.len() {
                    return Err(host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected bad auth snapshot",
                        &[],
                    ));
                }
                for (i, tracker) in trackers.iter().enumerate() {
                    if let Some(tracker_snapshot) = &trackers_snapshot[i] {
                        tracker
                            .try_borrow_mut()
                            .map_err(|_| {
                                host.err(
                                    ScErrorType::Auth,
                                    ScErrorCode::InternalError,
                                    "unexpected bad auth snapshot",
                                    &[],
                                )
                            })?
                            .rollback(&tracker_snapshot)?;
                    }
                }
            }
            AccountTrackersSnapshot::Recording(s) => {
                *self.try_borrow_account_trackers_mut(host)? = s;
            }
        }

        let invoker_trackers = self.try_borrow_invoker_contract_trackers_mut(host)?;
        if invoker_trackers.len() != snapshot.invoker_contract_tracker_root_snapshots.len() {
            return Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected bad auth snapshot",
                &[],
            ));
        }

        if let Some(tracker_by_address_handle) = snapshot.tracker_by_address_handle {
            match &self.mode {
                AuthorizationMode::Recording(recording_info) => {
                    *recording_info.try_borrow_tracker_by_address_handle_mut(host)? =
                        tracker_by_address_handle;
                }
                AuthorizationMode::Enforcing => (),
            }
        }
        Ok(())
    }

    // metering: covered
    fn push_tracker_frame(&self, host: &Host) -> Result<(), HostError> {
        for tracker in self.try_borrow_account_trackers(host)?.iter() {
            // Skip already borrowed trackers, these must be in the middle of
            // authentication and hence don't need stack to be updated.
            if let Ok(mut tracker) = tracker.try_borrow_mut() {
                tracker.push_frame(host.as_budget())?;
            }
        }
        for tracker in self
            .try_borrow_invoker_contract_trackers_mut(host)?
            .iter_mut()
        {
            tracker.push_frame(host.as_budget())?;
        }
        Ok(())
    }

    // metering: covered
    pub(crate) fn push_create_contract_host_fn_frame(
        &self,
        host: &Host,
        args: CreateContractArgs,
    ) -> Result<(), HostError> {
        Vec::<CreateContractArgs>::charge_bulk_init_cpy(1, host)?;
        self.try_borrow_call_stack_mut(host)?
            .push(AuthStackFrame::CreateContractHostFn(args));
        self.push_tracker_frame(host)
    }

    // Records a new call stack frame.
    // This should be called for every `Host` `push_frame`.
    // metering: covered
    pub(crate) fn push_frame(&self, host: &Host, frame: &Frame) -> Result<(), HostError> {
        let _span = tracy_span!("push auth frame");
        let (contract_id, function_name) = match frame {
            Frame::ContractVM { vm, fn_name, .. } => {
                (vm.contract_id.metered_clone(host)?, *fn_name)
            }
            // Skip the top-level host function stack frames as they don't
            // contain all the necessary information.
            // Use the respective push (like
            // `push_create_contract_host_fn_frame`) functions instead to push
            // the frame with the required info.
            Frame::HostFunction(_) => return Ok(()),
            Frame::Token(id, fn_name, ..) => (id.metered_clone(host)?, *fn_name),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (tc.id.metered_clone(host)?, tc.func),
        };
        // Currently only contracts might appear in the call stack.
        let contract_address = host.add_host_object(ScAddress::Contract(contract_id))?;
        Vec::<ContractInvocation>::charge_bulk_init_cpy(1, host)?;
        self.try_borrow_call_stack_mut(host)?
            .push(AuthStackFrame::Contract(ContractInvocation {
                contract_address,
                function_name,
            }));

        self.push_tracker_frame(host)
    }

    // Pops a call stack frame.
    // This should be called for every `Host` `pop_frame`.
    // metering: covered
    pub(crate) fn pop_frame(&self, host: &Host) -> Result<(), HostError> {
        let _span = tracy_span!("pop auth frame");
        {
            let mut call_stack = self.try_borrow_call_stack_mut(host)?;
            // Currently we don't push host function call frames, hence this may be
            // called with empty stack. We trust the Host to keep things correct,
            // i.e. that only host function frames are ignored this way.
            if call_stack.is_empty() {
                return Ok(());
            }
            call_stack.pop();
        }
        for tracker in self.try_borrow_account_trackers(host)?.iter() {
            // Skip already borrowed trackers, these must be in the middle of
            // authentication and hence don't need stack to be updated.
            if let Ok(mut tracker) = tracker.try_borrow_mut() {
                tracker.pop_frame();
            }
        }

        let mut invoker_contract_trackers = self.try_borrow_invoker_contract_trackers_mut(host)?;
        for tracker in invoker_contract_trackers.iter_mut() {
            tracker.pop_frame();
        }
        // Pop invoker contract trackers that went out of scope. The invariant
        // is that tracker only exists for the next sub-contract call (or until
        // the tracker's frame itself is popped). Thus trackers form a stack
        // where the shorter lifetime trackers are at the top.
        while let Some(last) = invoker_contract_trackers.last() {
            if last.is_out_of_scope() {
                invoker_contract_trackers.pop();
            } else {
                break;
            }
        }
        Ok(())
    }

    // Returns the recorded per-address authorization payloads that would cover the
    // top-level contract function invocation in the enforcing mode.
    // Should only be called in the recording mode.
    // metering: free, recording mode
    pub(crate) fn get_recorded_auth_payloads(
        &self,
        host: &Host,
    ) -> Result<Vec<RecordedAuthPayload>, HostError> {
        match &self.mode {
            AuthorizationMode::Enforcing => Err(HostError::from((
                ScErrorType::Auth,
                ScErrorCode::InternalError,
            ))),
            AuthorizationMode::Recording(_) => Ok(self
                .try_borrow_account_trackers(host)?
                .iter()
                .map(|tracker| tracker.try_borrow_or_err()?.get_recorded_auth_payload(host))
                .collect::<Result<Vec<RecordedAuthPayload>, HostError>>()?),
        }
    }

    // For recording mode, emulates authentication that would normally happen in
    // the enforcing mode.
    // This helps to build a more realistic footprint and produce more correct
    // meterting data for the recording mode.
    // No-op in the enforcing mode.
    // metering: covered
    pub(crate) fn maybe_emulate_authentication(&self, host: &Host) -> Result<(), HostError> {
        match &self.mode {
            AuthorizationMode::Enforcing => Ok(()),
            AuthorizationMode::Recording(_) => {
                for tracker in self.try_borrow_account_trackers(host)?.iter() {
                    tracker
                        .try_borrow_mut_or_err()?
                        .emulate_authentication(host)?;
                }
                Ok(())
            }
        }
    }

    // Returns a 'reset' instance of `AuthorizationManager` that has the same
    // mode, but no data.
    // metering: free, testutils
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn reset(&mut self) {
        *self = match &self.mode {
            AuthorizationMode::Enforcing => {
                AuthorizationManager::new_enforcing_without_authorizations()
            }
            AuthorizationMode::Recording(rec_info) => {
                AuthorizationManager::new_recording(rec_info.disable_non_root_auth)
            }
        }
    }

    // Returns all authorizations that have been authenticated for the
    // last contract invocation.
    // metering: free, testutils
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn get_authenticated_authorizations(
        &self,
        host: &Host,
    ) -> Vec<(ScAddress, xdr::SorobanAuthorizedInvocation)> {
        self.account_trackers
            .borrow()
            .iter()
            .filter(|t| t.borrow().authenticated)
            .map(|t| {
                (
                    host.scaddress_from_address(t.borrow().address).unwrap(),
                    t.borrow()
                        .invocation_tracker
                        .root_authorized_invocation
                        .to_xdr_non_metered(host, true)
                        .unwrap(),
                )
            })
            .collect()
    }
}

impl InvocationTracker {
    // metering: covered by components
    fn from_xdr(
        host: &Host,
        root_invocation: xdr::SorobanAuthorizedInvocation,
    ) -> Result<Self, HostError> {
        Ok(Self {
            root_authorized_invocation: AuthorizedInvocation::from_xdr(host, root_invocation)?,
            invocation_id_in_call_stack: vec![],
            root_exhausted_frame: None,
            is_fully_processed: false,
        })
    }

    // metering: free
    fn new(root_authorized_invocation: AuthorizedInvocation) -> Self {
        Self {
            root_authorized_invocation,
            invocation_id_in_call_stack: vec![],
            root_exhausted_frame: None,
            is_fully_processed: false,
        }
    }

    // metering: free for recording
    fn new_recording(function: AuthorizedFunction, current_stack_len: usize) -> Self {
        // Create the stack of `None` leading to the current invocation to
        // represent invocations that didn't need authorization on behalf of
        // the tracked address.
        let mut invocation_id_in_call_stack = vec![None; current_stack_len - 1];
        // Add the id for the current(root) invocation.
        invocation_id_in_call_stack.push(Some(0));
        let root_exhausted_frame = Some(invocation_id_in_call_stack.len() - 1);
        Self {
            root_authorized_invocation: AuthorizedInvocation::new_recording(function),
            invocation_id_in_call_stack,
            root_exhausted_frame,
            is_fully_processed: false,
        }
    }

    // Walks a path in the tree defined by `invocation_id_in_call_stack` and
    // returns the last visited authorized node.
    // metering: free
    fn last_authorized_invocation_mut(&mut self) -> Option<&mut AuthorizedInvocation> {
        for i in 0..self.invocation_id_in_call_stack.len() {
            if self.invocation_id_in_call_stack[i].is_some() {
                return Some(
                    self.root_authorized_invocation
                        .last_authorized_invocation_mut(&self.invocation_id_in_call_stack, i + 1),
                );
            }
        }
        None
    }

    // metering: covered
    fn push_frame(&mut self, budget: &Budget) -> Result<(), HostError> {
        Vec::<usize>::charge_bulk_init_cpy(1, budget)?;
        self.invocation_id_in_call_stack.push(None);
        Ok(())
    }

    // metering: free
    fn pop_frame(&mut self) {
        self.invocation_id_in_call_stack.pop();
        if let Some(root_exhausted_frame) = self.root_exhausted_frame {
            if root_exhausted_frame >= self.invocation_id_in_call_stack.len() {
                self.is_fully_processed = true;
            }
        }
    }

    // metering: free
    fn is_empty(&self) -> bool {
        self.invocation_id_in_call_stack.is_empty()
    }

    // metering: free
    fn is_active(&self) -> bool {
        self.root_authorized_invocation.is_exhausted && !self.is_fully_processed
    }

    // metering: free
    fn current_frame_is_already_matched(&self) -> bool {
        match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        }
    }

    // Tries to match the provided invocation to the authorized sub-invocation
    // of the current tree and push it to the call stack.
    // Returns `true` if the match has been found for the first time per current
    // frame.
    // Metering: covered by components
    fn maybe_push_matching_invocation_frame(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
        allow_matching_root: bool,
    ) -> Result<bool, HostError> {
        if self.current_frame_is_already_matched() {
            return Ok(false);
        }
        let mut frame_index = None;
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            for (i, sub_invocation) in curr_invocation.sub_invocations.iter_mut().enumerate() {
                if !sub_invocation.is_exhausted
                    && host.compare(&sub_invocation.function, function)?.is_eq()
                {
                    frame_index = Some(i);
                    sub_invocation.is_exhausted = true;
                    break;
                }
            }
        } else if !self.root_authorized_invocation.is_exhausted
            && allow_matching_root
            && host
                .compare(&self.root_authorized_invocation.function, &function)?
                .is_eq()
        {
            frame_index = Some(0);
            self.root_authorized_invocation.is_exhausted = true;
            self.root_exhausted_frame = Some(self.invocation_id_in_call_stack.len() - 1);
        }
        if frame_index.is_some() {
            *self.invocation_id_in_call_stack.last_mut().unwrap() = frame_index;
        }
        Ok(frame_index.is_some())
    }

    // Records the invocation in this tracker.
    // This is needed for the recording mode only.
    // This assumes that the address matching is correctly performed before
    // calling this.
    // metering: free for recording
    fn record_invocation(
        &mut self,
        host: &Host,
        function: AuthorizedFunction,
    ) -> Result<(), HostError> {
        let frame_is_already_authorized = match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        };
        if frame_is_already_authorized {
            return Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::ExistingValue,
                "frame is already authorized",
                &[],
            ));
        }
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            curr_invocation
                .sub_invocations
                .push(AuthorizedInvocation::new_recording(function));
            *self.invocation_id_in_call_stack.last_mut().unwrap() =
                Some(curr_invocation.sub_invocations.len() - 1);
        } else {
            // This would be a bug
            return Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected missing authorized invocation",
                &[],
            ));
        }
        Ok(())
    }

    // metering: free
    fn has_matched_invocations_in_stack(&self) -> bool {
        self.invocation_id_in_call_stack.iter().any(|i| i.is_some())
    }

    // metering: covered
    fn snapshot(&self, budget: &Budget) -> Result<AuthorizedInvocationSnapshot, HostError> {
        self.root_authorized_invocation.snapshot(budget)
    }

    // metering: covered
    fn rollback(&mut self, snapshot: &AuthorizedInvocationSnapshot) -> Result<(), HostError> {
        self.root_authorized_invocation.rollback(snapshot)?;
        // Invocation can only be rolled back from 'exhausted' to
        // 'non-exhausted' state (as there is no other way to go from
        // 'exhausted' state back to 'non-exhausted' state).
        if !self.root_authorized_invocation.is_exhausted {
            self.root_exhausted_frame = None;
            self.is_fully_processed = false;
        }
        Ok(())
    }
}

impl AccountAuthorizationTracker {
    // Metering: covered by the host and components
    fn from_authorization_entry(
        host: &Host,
        auth_entry: SorobanAuthorizationEntry,
    ) -> Result<Self, HostError> {
        let (address, nonce, signature) = match auth_entry.credentials {
            SorobanCredentials::SourceAccount => (
                host.source_account_address()?.ok_or_else(|| {
                    host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "source account is missing when setting auth entries",
                        &[],
                    )
                })?,
                None,
                Val::VOID.into(),
            ),
            SorobanCredentials::Address(address_creds) => (
                host.add_host_object(address_creds.address)?,
                Some((
                    address_creds.nonce,
                    address_creds.signature_expiration_ledger,
                )),
                host.to_host_val(&address_creds.signature)?,
            ),
        };
        let is_invoker = nonce.is_none();
        Ok(Self {
            address,
            invocation_tracker: InvocationTracker::from_xdr(host, auth_entry.root_invocation)?,
            signature,
            authenticated: false,
            need_nonce: !is_invoker,
            is_invoker,
            nonce,
            is_valid: true,
        })
    }

    // metering: free, since this is recording mode only
    fn new_recording(
        host: &Host,
        address: AddressObject,
        function: AuthorizedFunction,
        current_stack_len: usize,
    ) -> Result<Self, HostError> {
        if current_stack_len == 0 {
            // This would be a bug.
            return Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected empty stack in recording auth",
                &[],
            ));
        }
        // If the invoker account is known, set it to `None`, so that the final
        // recorded payload wouldn't contain the address. This makes it easier
        // to use more optimal payload when only invoker auth is used.
        let is_invoker = if let Some(source_acc) = host.source_account_address()? {
            host.compare(&source_acc, &address)?.is_eq()
        } else {
            false
        };
        let nonce = if !is_invoker {
            let random_nonce: i64 = rand::thread_rng().gen_range(0..=i64::MAX);
            host.consume_nonce(address, random_nonce, 0)?;
            Some((random_nonce, 0))
        } else {
            None
        };
        // Create the stack of `None` leading to the current invocation to
        // represent invocations that didn't need authorization on behalf of
        // the tracked address.
        let mut invocation_id_in_call_stack = vec![None; current_stack_len - 1];
        // Add the id for the current(root) invocation.
        invocation_id_in_call_stack.push(Some(0));
        Ok(Self {
            address,
            invocation_tracker: InvocationTracker::new_recording(function, current_stack_len),
            signature: Val::VOID.into(),
            is_valid: true,
            authenticated: true,
            need_nonce: false,
            is_invoker,
            nonce,
        })
    }

    // Tries to find and enforce the provided invocation with this tracker and
    // lazily performs authentication when needed.
    // This is needed for the enforcing mode only.
    // This assumes that the address matching is correctly performed before
    // calling this.
    // Returns true/false based on whether the invocation is found in the
    // tracker. Returns error if invocation has been found, but the tracker
    // itself is not valid (failed authentication or nonce check).
    // metering: covered
    fn maybe_authorize_invocation(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
        allow_matching_root: bool,
    ) -> Result<bool, HostError> {
        if !self.is_valid {
            return Ok(false);
        }
        if !self
            .invocation_tracker
            .maybe_push_matching_invocation_frame(host, function, allow_matching_root)?
        {
            // The call isn't found in the currently tracked tree or is already
            // authorized in it.
            // That doesn't necessarily mean it's unauthorized (it can be
            // authorized in a different tracker).
            return Ok(false);
        }
        if !self.authenticated {
            let authenticate_res = self
                .authenticate(host)
                .map_err(|err| {
                    // Convert any contract errors to auth errors so that it's
                    // not possible to confuse them for the errors of the
                    // contract that has called `require_auth`.
                    if err.error.is_type(ScErrorType::Contract) {
                        host.err(
                            ScErrorType::Auth,
                            ScErrorCode::InvalidAction,
                            "failed account authentication",
                            &[self.address.into(), err.error.to_val()],
                        )
                    } else {
                        err
                    }
                })
                .and_then(|_| self.verify_and_consume_nonce(host));
            if let Some(err) = authenticate_res.err() {
                self.is_valid = false;
                return Err(err);
            }
            self.authenticated = true;
        }
        Ok(true)
    }

    // Records the invocation in this tracker.
    // This is needed for the recording mode only.
    // This assumes that the address matching is correctly performed before
    // calling this.
    // metering: free for recording
    fn record_invocation(
        &mut self,
        host: &Host,
        function: AuthorizedFunction,
    ) -> Result<(), HostError> {
        self.invocation_tracker.record_invocation(host, function)
    }

    // Build the authorization payload from the invocations recorded in this
    // tracker.
    // metering: free for recording
    fn get_recorded_auth_payload(&self, host: &Host) -> Result<RecordedAuthPayload, HostError> {
        Ok(RecordedAuthPayload {
            address: if !self.is_invoker {
                Some(host.visit_obj(self.address, |a: &ScAddress| Ok(a.clone()))?)
            } else {
                None
            },
            invocation: self
                .invocation_tracker
                .root_authorized_invocation
                .to_xdr_non_metered(host, false)?,
            nonce: self.nonce.map(|(nonce, _)| nonce),
        })
    }

    // Checks if there is at least one authorized invocation in the current call
    // stack.
    // metering: free
    fn has_authorized_invocations_in_stack(&self) -> bool {
        self.invocation_tracker.has_matched_invocations_in_stack()
    }

    // metering: covered
    fn root_invocation_to_xdr(
        &self,
        host: &Host,
    ) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        self.invocation_tracker
            .root_authorized_invocation
            .to_xdr(host)
    }

    // metering: covered
    fn push_frame(&mut self, budget: &Budget) -> Result<(), HostError> {
        self.invocation_tracker.push_frame(budget)
    }

    // metering: covered
    fn pop_frame(&mut self) {
        self.invocation_tracker.pop_frame();
    }

    // metering: covered
    fn verify_and_consume_nonce(&mut self, host: &Host) -> Result<(), HostError> {
        if !self.need_nonce {
            return Ok(());
        }
        self.need_nonce = false;
        if let Some((nonce, expiration_ledger)) = &self.nonce {
            let ledger_seq = host.with_ledger_info(|li| Ok(li.sequence_number))?;
            if ledger_seq > *expiration_ledger {
                return Err(host.err(
                    ScErrorType::Auth,
                    ScErrorCode::InvalidInput,
                    "signature has expired",
                    &[
                        self.address.into(),
                        ledger_seq.try_into_val(host)?,
                        expiration_ledger.try_into_val(host)?,
                    ],
                ));
            }
            let max_expiration_ledger = host.max_expiration_ledger()?;
            if *expiration_ledger > max_expiration_ledger {
                return Err(host.err(
                    ScErrorType::Auth,
                    ScErrorCode::InvalidInput,
                    "signature expiration is too late",
                    &[
                        self.address.into(),
                        max_expiration_ledger.try_into_val(host)?,
                        expiration_ledger.try_into_val(host)?,
                    ],
                ));
            }

            return host.consume_nonce(self.address, *nonce, *expiration_ledger);
        }
        Err(host.err(
            ScErrorType::Auth,
            ScErrorCode::InternalError,
            "unexpected nonce verification state",
            &[],
        ))
    }

    // Computes the payload that has to be signed in order to authenticate
    // the authorized invocation tree corresponding to this tracker.
    // metering: covered by components
    fn get_signature_payload(&self, host: &Host) -> Result<[u8; 32], HostError> {
        let (nonce, expiration_ledger) = self.nonce.ok_or_else(|| {
            host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected missing nonce",
                &[],
            )
        })?;
        let payload_preimage =
            HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                network_id: Hash(host.with_ledger_info(|li| li.network_id.metered_clone(host))?),
                nonce,
                signature_expiration_ledger: expiration_ledger,
                invocation: self.root_invocation_to_xdr(host)?,
            });

        host.metered_hash_xdr(&payload_preimage)
    }

    // metering: covered by the hsot
    fn authenticate(&self, host: &Host) -> Result<(), HostError> {
        if self.is_invoker {
            return Ok(());
        }

        let sc_addr = host.scaddress_from_address(self.address)?;
        // TODO: there should also be a mode where a dummy payload is used
        // instead (for enforcing mode preflight).
        let payload = self.get_signature_payload(host)?;
        match sc_addr {
            ScAddress::Account(acc) => {
                check_account_authentication(host, acc, &payload, self.signature)?;
            }
            ScAddress::Contract(acc_contract) => {
                check_account_contract_auth(
                    host,
                    &acc_contract,
                    &payload,
                    self.signature,
                    &self.invocation_tracker.root_authorized_invocation,
                )?;
            }
        }
        Ok(())
    }

    // Emulates authentication for the recording mode.
    // metering: covered
    fn emulate_authentication(&self, host: &Host) -> Result<(), HostError> {
        if self.is_invoker {
            return Ok(());
        }
        let sc_addr = host.scaddress_from_address(self.address)?;
        // Compute the real payload for the sake of metering, but don't use it.
        let _payload = self.get_signature_payload(host)?;
        match sc_addr {
            ScAddress::Account(acc) => {
                let _account = host.load_account(acc)?;
            }
            // Skip custom accounts for now - emulating authentication for
            // them requires a dummy signature.
            ScAddress::Contract(_) => (),
        }
        Ok(())
    }

    // metering: covered
    fn snapshot(&self, budget: &Budget) -> Result<AccountAuthorizationTrackerSnapshot, HostError> {
        Ok(AccountAuthorizationTrackerSnapshot {
            invocation_tracker_root_snapshot: self.invocation_tracker.snapshot(budget)?,
            // We need to snapshot authenctioncation and nocne-related fields as
            // during the rollback the nonce might be 'un-consumed' during
            // the storage rollback. We don't snapshot `is_valid` since this is
            // not recoverable and nonce is never consumed for invalid
            // authorizations.
            authenticated: self.authenticated,
            need_nonce: self.need_nonce,
        })
    }

    // metering: covered
    fn rollback(
        &mut self,
        snapshot: &AccountAuthorizationTrackerSnapshot,
    ) -> Result<(), HostError> {
        self.invocation_tracker
            .rollback(&snapshot.invocation_tracker_root_snapshot)?;
        self.authenticated = snapshot.authenticated;
        self.need_nonce = snapshot.need_nonce;
        Ok(())
    }

    // metering: free
    fn is_active(&self) -> bool {
        self.is_valid && self.invocation_tracker.is_active()
    }

    // metering: free
    fn current_frame_is_already_matched(&self) -> bool {
        self.invocation_tracker.current_frame_is_already_matched()
    }
}

impl InvokerContractAuthorizationTracker {
    // metering: covered by components
    fn new(host: &Host, invoker_auth_entry: Val) -> Result<Self, HostError> {
        let invoker_sc_addr = ScAddress::Contract(host.get_current_contract_id_internal()?);
        let authorized_invocation = invoker_contract_auth_to_authorized_invocation(
            host,
            &invoker_sc_addr,
            invoker_auth_entry,
        )?;
        let invocation_tracker = InvocationTracker::new(authorized_invocation);
        Ok(Self {
            contract_address: host.add_host_object(invoker_sc_addr)?,
            invocation_tracker,
        })
    }

    // metering: covered
    fn push_frame(&mut self, budget: &Budget) -> Result<(), HostError> {
        self.invocation_tracker.push_frame(budget)
    }

    // metering: covered
    fn pop_frame(&mut self) {
        self.invocation_tracker.pop_frame();
    }

    // metering: free
    fn is_out_of_scope(&self) -> bool {
        self.invocation_tracker.is_empty()
    }

    // metering: covered
    fn maybe_authorize_invocation(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        // Authorization is successful if function is just matched by the
        // tracker. No authentication is needed.
        self.invocation_tracker
            .maybe_push_matching_invocation_frame(host, function, true)
    }
}

impl Host {
    // metering: covered by components
    fn consume_nonce(
        &self,
        address: AddressObject,
        nonce: i64,
        expiration_ledger: u32,
    ) -> Result<(), HostError> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey { nonce });
        let sc_address = self.scaddress_from_address(address)?;
        let nonce_key = self.storage_key_for_address(
            sc_address.metered_clone(self)?,
            nonce_key_scval.metered_clone(self)?,
            xdr::ContractDataDurability::Temporary,
        )?;
        let expiration_ledger = expiration_ledger
            .max(self.get_min_expiration_ledger(xdr::ContractDataDurability::Temporary)?);
        self.with_mut_storage(|storage| {
            if storage.has(&nonce_key, self.budget_ref())? {
                return Err(self.err(
                    ScErrorType::Auth,
                    ScErrorCode::ExistingValue,
                    "nonce already exists for address",
                    &[address.into()],
                ));
            }
            let data = LedgerEntryData::ContractData(ContractDataEntry {
                contract: sc_address,
                key: nonce_key_scval,
                val: ScVal::Void,
                durability: xdr::ContractDataDurability::Temporary,
                ext: xdr::ExtensionPoint::V0,
            });
            let entry = LedgerEntry {
                last_modified_ledger_seq: 0,
                data,
                ext: LedgerEntryExt::V0,
            };
            storage.put(
                &nonce_key,
                &Rc::metered_new(entry, self)?,
                Some(expiration_ledger),
                self.budget_ref(),
            )
        })
    }
}

// metering: free for testutils
#[cfg(any(test, feature = "testutils"))]
impl PartialEq for RecordedAuthPayload {
    fn eq(&self, other: &Self) -> bool {
        self.address == other.address
            && self.invocation == other.invocation
            // Compare nonces only by presence of the value - recording mode
            // generates random nonces.
            && self.nonce.is_some() == other.nonce.is_some()
    }
}
