use std::collections::HashMap;
use std::rc::Rc;

use rand::Rng;
use soroban_env_common::xdr::{
    ContractDataEntry, ContractDataEntryBody, ContractDataEntryData, CreateContractArgs,
    HashIdPreimage, HashIdPreimageSorobanAuthorization, LedgerEntry, LedgerEntryData,
    LedgerEntryExt, ScAddress, ScErrorCode, ScErrorType, ScNonceKey, ScVal,
    SorobanAuthorizationEntry, SorobanAuthorizedContractFunction, SorobanAuthorizedFunction,
    SorobanCredentials,
};
use soroban_env_common::{AddressObject, Compare, Symbol, TryFromVal, TryIntoVal, Val, VecObject};

use crate::host::metered_clone::{charge_container_bulk_init_with_elts, MeteredClone};
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
    account_trackers: Vec<AccountAuthorizationTracker>,
    // Per-address trackers for authorization performed by the contracts at
    // execution time (as opposed to signature-based authorization for accounts).
    // Contract authorizations are always enforced independently of the `mode`,
    // as they are self-contained and fully defined by the contract logic.
    invoker_contract_trackers: Vec<InvokerContractAuthorizationTracker>,
    // Current call stack consisting only of the contract invocations (i.e. not
    // the host functions).
    call_stack: Vec<AuthStackFrame>,
}

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
#[derive(Clone)]
pub struct AuthorizationManagerSnapshot {
    // AccountAuthorizationTracker has some immutable parts, but it is safer to
    // just rollback everything. If this is an issue, then the
    // AccountAuthorizationTracker should probably be separated into 'mutable'
    // and 'immutable' parts.
    account_trackers: Vec<AccountAuthorizationTracker>,
    invoker_contract_trackers: Vec<InvokerContractAuthorizationTracker>,
    tracker_by_address_handle: Option<HashMap<u32, usize>>,
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
    tracker_by_address_handle: HashMap<u32, usize>,
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
}

// Stores all the authorizations that are authorized by an address.
// In the enforcing mode this performs authentication and makes sure that only
// pre-authorized invocations can happen on behalf of the `address`.
// In the recording mode this will record the invocations that are authorized
// on behalf of the address.
#[derive(Clone)]
struct AccountAuthorizationTracker {
    // Tracked address. If `None`, then lazily set to the transaction source
    // account's (i.e. invoker's) address is used.
    address: Option<AddressObject>,
    invocation_tracker: InvocationTracker,
    // Arguments representing the signature(s) made by the address to authorize
    // the invocations tracked here.
    signature_args: Vec<Val>,
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

// Stores all the authorizations peformed by contracts at runtime.
#[derive(Clone)]
struct InvokerContractAuthorizationTracker {
    contract_address: AddressObject,
    invocation_tracker: InvocationTracker,
}

#[derive(Clone)]
enum AuthStackFrame {
    Contract(ContractInvocation),
    CreateContractHostFn(CreateContractArgs),
}

#[derive(Clone)]
struct ContractInvocation {
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

impl Compare<ContractFunction> for Host {
    type Error = HostError;

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
    fn to_authorized_function(
        &self,
        host: &Host,
        args: Vec<Val>,
    ) -> Result<AuthorizedFunction, HostError> {
        match self {
            AuthStackFrame::Contract(contract_frame) => {
                Ok(AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: contract_frame
                        .contract_address
                        .metered_clone(host.budget_ref())?,
                    function_name: contract_frame
                        .function_name
                        .metered_clone(host.budget_ref())?,
                    args,
                }))
            }
            AuthStackFrame::CreateContractHostFn(args) => Ok(
                AuthorizedFunction::CreateContractHostFn(args.metered_clone(host.budget_ref())?),
            ),
        }
    }
}

impl AuthorizedFunction {
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
                Ok(SorobanAuthorizedFunction::ContractFn(
                    SorobanAuthorizedContractFunction {
                        contract_address: host
                            .scaddress_from_address(contract_fn.contract_address)?,
                        function_name,
                        args: host.rawvals_to_scvec(contract_fn.args.as_slice())?,
                    },
                ))
            }
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => {
                Ok(SorobanAuthorizedFunction::CreateContractHostFn(
                    create_contract_args.metered_clone(host.budget_ref())?,
                ))
            }
        }
    }

    fn to_xdr_non_metered(&self, host: &Host) -> Result<xdr::SorobanAuthorizedFunction, HostError> {
        match self {
            AuthorizedFunction::ContractFn(contract_fn) => Ok(
                SorobanAuthorizedFunction::ContractFn(SorobanAuthorizedContractFunction {
                    contract_address: host
                        .visit_obj(contract_fn.contract_address, |addr: &ScAddress| {
                            Ok(addr.clone())
                        })?,
                    function_name: contract_fn.function_name.try_into_val(host)?,
                    args: host.rawvals_to_scvec_non_metered(contract_fn.args.as_slice())?,
                }),
            ),
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => Ok(
                SorobanAuthorizedFunction::CreateContractHostFn(create_contract_args.clone()),
            ),
        }
    }
}

impl AuthorizedInvocation {
    fn from_xdr(
        host: &Host,
        xdr_invocation: xdr::SorobanAuthorizedInvocation,
    ) -> Result<Self, HostError> {
        let sub_invocations_xdr = xdr_invocation.sub_invocations.into_vec();
        let sub_invocations = sub_invocations_xdr
            .into_iter()
            .map(|a| AuthorizedInvocation::from_xdr(host, a))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            function: AuthorizedFunction::from_xdr(host, xdr_invocation.function)?,
            sub_invocations,
            is_exhausted: false,
        })
    }

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

    fn new_recording(function: AuthorizedFunction) -> Self {
        Self {
            function,
            sub_invocations: vec![],
            is_exhausted: true,
        }
    }

    fn to_xdr(&self, host: &Host) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        charge_container_bulk_init_with_elts::<
            Vec<xdr::SorobanAuthorizedInvocation>,
            xdr::SorobanAuthorizedInvocation,
        >(self.sub_invocations.len() as u64, host.budget_ref())?;
        Ok(xdr::SorobanAuthorizedInvocation {
            function: self.function.to_xdr(host)?,
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr(host))
                .collect::<Result<Vec<xdr::SorobanAuthorizedInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from((ScErrorType::Auth, ScErrorCode::InternalError)))?,
        })
    }

    // Non-metered conversion should only be used for the recording preflight
    // runs or testing.
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
    fn last_authorized_invocation_mut(
        &mut self,
        invocation_id_in_call_stack: &Vec<Option<usize>>,
        call_stack_id: usize,
    ) -> &mut AuthorizedInvocation {
        // Start walking the stack from `call_stack_id`. We trust the callers to
        // hold the invariant that `invocation_id_in_call_stack[call_stack_id - 1]`
        // corresponds to this invocation tree, so that the next non-`None` child
        // corresponds to the child of the current tree.
        for i in call_stack_id..invocation_id_in_call_stack.len() {
            if let Some(id) = invocation_id_in_call_stack[i] {
                // We trust the caller to have the correct sub-invocation
                // indices.
                return self.sub_invocations[id]
                    .last_authorized_invocation_mut(invocation_id_in_call_stack, i + 1);
            }
            // Skip `None` invocations as they don't require authorization.
        }
        self
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
    pub(crate) fn new_enforcing(
        host: &Host,
        auth_entries: Vec<SorobanAuthorizationEntry>,
    ) -> Result<Self, HostError> {
        let mut trackers = vec![];
        for auth_entry in auth_entries {
            trackers.push(AccountAuthorizationTracker::from_authorization_entry(
                host, auth_entry,
            )?);
        }
        Ok(Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: vec![],
            account_trackers: trackers,
            invoker_contract_trackers: vec![],
        })
    }

    // Creates a new enforcing `AuthorizationManager` that doesn't allow any
    // authorizations.
    // This is useful as a safe default mode.
    pub(crate) fn new_enforcing_without_authorizations() -> Self {
        Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: vec![],
            account_trackers: vec![],
            invoker_contract_trackers: vec![],
        }
    }

    // Creates a new recording `AuthorizationManager`.
    // All the authorization requirements will be recorded and can then be
    // retrieved using `get_recorded_auth_payloads`.
    pub(crate) fn new_recording() -> Self {
        Self {
            mode: AuthorizationMode::Recording(RecordingAuthInfo {
                tracker_by_address_handle: Default::default(),
            }),
            call_stack: vec![],
            account_trackers: vec![],
            invoker_contract_trackers: vec![],
        }
    }

    // Require the `address` to have authorized the current contract invocation
    // with provided args and within the current context (i.e. the current
    // authorized call stack and for the current network).
    // In the recording mode this stores the auth requirement instead of
    // verifying it.
    pub(crate) fn require_auth(
        &mut self,
        host: &Host,
        address: AddressObject,
        args: Vec<Val>,
    ) -> Result<(), HostError> {
        let curr_invocation = self.call_stack.last().ok_or_else(|| {
            host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected require_auth outside of valid frame",
                &[],
            )
        })?;
        self.require_auth_internal(
            host,
            address,
            curr_invocation.to_authorized_function(host, args)?,
        )
    }

    pub(crate) fn add_invoker_contract_auth(
        &mut self,
        host: &Host,
        auth_entries: VecObject,
    ) -> Result<(), HostError> {
        let auth_entries =
            host.visit_obj(auth_entries, |e: &HostVec| e.to_vec(host.budget_ref()))?;
        for e in auth_entries {
            self.invoker_contract_trackers
                .push(InvokerContractAuthorizationTracker::new(host, e)?)
        }
        Ok(())
    }

    fn verify_contract_invoker_auth(
        &mut self,
        host: &Host,
        address: AddressObject,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        // If stack has just one call there can't be invoker.
        if self.call_stack.len() < 2 {
            return Ok(false);
        }

        // Try matching the direct invoker contract first. It is considered to
        // have authorized any direct calls.
        let invoker_frame = &self.call_stack[self.call_stack.len() - 2];
        if let AuthStackFrame::Contract(invoker_contract) = invoker_frame {
            if host
                .compare(&invoker_contract.contract_address, &address)?
                .is_eq()
            {
                return Ok(true);
            }
        }
        // If there is no direct invoker, there still might be a valid
        // sub-contract call authorization from another invoker higher up the
        // stack. Note, that invoker contract trackers consider the direct frame
        // to never require auth (any `require_auth` calls would be matched by
        // logic above).
        for tracker in &mut self.invoker_contract_trackers {
            if host.compare(&tracker.contract_address, &address)?.is_eq()
                && tracker.maybe_authorize_invocation(host, function)?
            {
                return Ok(true);
            }
        }

        return Ok(false);
    }

    fn require_auth_enforcing(
        &mut self,
        host: &Host,
        address: AddressObject,
        function: &AuthorizedFunction,
    ) -> Result<(), HostError> {
        // Iterate all the trackers and try to find one that
        // fullfills the authorization requirement.
        for tracker in &mut self.account_trackers {
            let address_matches = if let Some(addr) = &tracker.address {
                host.compare(addr, &address)?.is_eq()
            } else {
                // Lazily fill the address for the invoker trackers,
                // so that it's possible to create the auth manager
                // without knowing the invoker beforehand and also
                // to not keep calling into `source_account` function.
                let source_addr = host.source_account_address()?.ok_or_else(|| {
                    host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected missing invoker in auth manager",
                        &[],
                    )
                })?;
                let source_matches = host.compare(&source_addr, &address)?.is_eq();
                tracker.address = Some(source_addr);
                source_matches
            };
            // If address doesn't match, just skip the tracker.
            if !address_matches {
                continue;
            }
            match tracker.maybe_authorize_invocation(host, function) {
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
        // No matching tracker found, hence the invocation isn't
        // authorized.
        Err(HostError::from((
            ScErrorType::Auth,
            ScErrorCode::InvalidAction,
        )))
    }

    fn require_auth_internal(
        &mut self,
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

        match &mut self.mode {
            AuthorizationMode::Enforcing => self.require_auth_enforcing(host, address, &function),
            AuthorizationMode::Recording(recording_info) => {
                let address_obj_handle = address.get_handle();
                if let Some(tracker_id) = recording_info
                    .tracker_by_address_handle
                    .get(&address_obj_handle)
                {
                    let tracker = &mut self.account_trackers[*tracker_id];
                    // The recording invariant is that trackers are created
                    // with the first authorized invocation, which means
                    // that when their stack no longer has authorized
                    // invocation, then we've popped frames past its root
                    // and hence need to create a new tracker.
                    if !tracker.has_authorized_invocations_in_stack() {
                        recording_info
                            .tracker_by_address_handle
                            .remove(&address_obj_handle);
                    } else {
                        return self.account_trackers[*tracker_id]
                            .record_invocation(host, function);
                    }
                }
                // If a tracker for the new tree doesn't exist yet, create
                // it and initialize with the current invocation.
                self.account_trackers
                    .push(AccountAuthorizationTracker::new_recording(
                        host,
                        address,
                        function,
                        self.call_stack.len(),
                    )?);
                recording_info
                    .tracker_by_address_handle
                    .insert(address_obj_handle, self.account_trackers.len() - 1);
                Ok(())
            }
        }
    }

    // Returns a snapshot of `AuthorizationManager` to use for rollback.
    pub(crate) fn snapshot(&self) -> AuthorizationManagerSnapshot {
        let tracker_by_address_handle = match &self.mode {
            AuthorizationMode::Enforcing => None,
            AuthorizationMode::Recording(recording_info) => {
                Some(recording_info.tracker_by_address_handle.clone())
            }
        };
        AuthorizationManagerSnapshot {
            account_trackers: self.account_trackers.clone(),
            invoker_contract_trackers: self.invoker_contract_trackers.clone(),
            tracker_by_address_handle,
        }
    }

    // Rolls back this `AuthorizationManager` to the snapshot state.
    pub(crate) fn rollback(&mut self, snapshot: AuthorizationManagerSnapshot) {
        self.account_trackers = snapshot.account_trackers;
        self.invoker_contract_trackers = snapshot.invoker_contract_trackers;
        if let Some(tracker_by_address_handle) = snapshot.tracker_by_address_handle {
            match &mut self.mode {
                AuthorizationMode::Recording(recording_info) => {
                    recording_info.tracker_by_address_handle = tracker_by_address_handle;
                }
                AuthorizationMode::Enforcing => (),
            }
        }
    }

    pub(crate) fn push_create_contract_host_fn_frame(&mut self, args: CreateContractArgs) {
        self.call_stack
            .push(AuthStackFrame::CreateContractHostFn(args));
        for tracker in &mut self.account_trackers {
            tracker.push_frame();
        }
    }

    // Records a new call stack frame.
    // This should be called for every `Host` `push_frame`.
    pub(crate) fn push_frame(&mut self, host: &Host, frame: &Frame) -> Result<(), HostError> {
        let (contract_id, function_name) = match frame {
            Frame::ContractVM(vm, fn_name, ..) => {
                (vm.contract_id.metered_clone(host.budget_ref())?, *fn_name)
            }
            // Skip the top-level host function stack frames as they don't
            // contain all the necessary information.
            // Use the respective push (like
            // `push_create_contract_host_fn_frame`) functions instead to push
            // the frame with the required info.
            Frame::HostFunction(_) => return Ok(()),
            Frame::Token(id, fn_name, ..) => (id.metered_clone(host.budget_ref())?, *fn_name),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (tc.id.clone(), tc.func),
        };
        // Currently only contracts might appear in the call stack.
        let contract_address = host.add_host_object(ScAddress::Contract(contract_id))?;
        self.call_stack
            .push(AuthStackFrame::Contract(ContractInvocation {
                contract_address,
                function_name,
            }));
        for tracker in &mut self.account_trackers {
            tracker.push_frame();
        }
        for tracker in &mut self.invoker_contract_trackers {
            tracker.push_frame();
        }
        Ok(())
    }

    // Pops a call stack frame.
    // This should be called for every `Host` `pop_frame`.
    pub(crate) fn pop_frame(&mut self) {
        // Currently we don't push host function call frames, hence this may be
        // called with empty stack. We trust the Host to keep things correct,
        // i.e. that only host function frames are ignored this way.
        // Eventually we may want to also authorize host fns via this, so this
        // won't be needed.
        if self.call_stack.is_empty() {
            return;
        }
        self.call_stack.pop();
        for tracker in &mut self.account_trackers {
            tracker.pop_frame();
        }

        let mut i = 0;
        while i < self.invoker_contract_trackers.len() {
            self.invoker_contract_trackers[i].pop_frame();
            if self.invoker_contract_trackers[i].is_out_of_scope() {
                self.invoker_contract_trackers.swap_remove(i);
            } else {
                i += 1;
            }
        }
    }

    // Returns the recorded per-address authorization payloads that would cover the
    // top-level contract function invocation in the enforcing mode.
    // Should only be called in the recording mode.
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
                .account_trackers
                .iter()
                .map(|tracker| tracker.get_recorded_auth_payload(host))
                .collect::<Result<Vec<RecordedAuthPayload>, HostError>>()?),
        }
    }

    // For recording mode, emulates authentication that would normally happen in
    // the enforcing mode.
    // This helps to build a more realistic footprint and produce more correct
    // meterting data for the recording mode.
    // No-op in the enforcing mode.
    pub(crate) fn maybe_emulate_authentication(&self, host: &Host) -> Result<(), HostError> {
        match &self.mode {
            AuthorizationMode::Enforcing => Ok(()),
            AuthorizationMode::Recording(_) => {
                for tracker in &self.account_trackers {
                    tracker.emulate_authentication(host)?;
                }
                Ok(())
            }
        }
    }

    // Returns a 'reset' instance of `AuthorizationManager` that has the same
    // mode, but no data.
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn reset(&mut self) {
        *self = match self.mode {
            AuthorizationMode::Enforcing => {
                AuthorizationManager::new_enforcing_without_authorizations()
            }
            AuthorizationMode::Recording(_) => AuthorizationManager::new_recording(),
        }
    }

    // Returns all authorizations that have been authenticated for the
    // last contract invocation.
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn get_authenticated_authorizations(
        &self,
        host: &Host,
    ) -> Vec<(ScAddress, xdr::SorobanAuthorizedInvocation)> {
        self.account_trackers
            .iter()
            .filter(|t| t.authenticated && t.is_exhausted())
            .filter_map(|t| {
                // Ignore authorizations without an address as they are implied,
                // less useful as a test utility, and not succinctly capturable
                // in the list of tuples. This is a tradeoff between offering up
                // all authorizations vs the authorizations developers will
                // mostly care about at the benefit of making this list easier
                // to use.
                t.address.as_ref().map(|a| {
                    (
                        host.scaddress_from_address(*a).unwrap(),
                        t.invocation_tracker
                            .root_authorized_invocation
                            .to_xdr_non_metered(host, true)
                            .unwrap(),
                    )
                })
            })
            .collect()
    }
}

impl InvocationTracker {
    fn from_xdr(
        host: &Host,
        root_invocation: xdr::SorobanAuthorizedInvocation,
    ) -> Result<Self, HostError> {
        Ok(Self {
            root_authorized_invocation: AuthorizedInvocation::from_xdr(host, root_invocation)?,
            invocation_id_in_call_stack: vec![],
        })
    }

    fn new(root_authorized_invocation: AuthorizedInvocation) -> Self {
        Self {
            root_authorized_invocation,
            invocation_id_in_call_stack: vec![],
        }
    }

    fn new_recording(function: AuthorizedFunction, current_stack_len: usize) -> Self {
        // Create the stack of `None` leading to the current invocation to
        // represent invocations that didn't need authorization on behalf of
        // the tracked address.
        let mut invocation_id_in_call_stack = vec![None; current_stack_len - 1];
        // Add the id for the current(root) invocation.
        invocation_id_in_call_stack.push(Some(0));
        Self {
            root_authorized_invocation: AuthorizedInvocation::new_recording(function),
            invocation_id_in_call_stack,
        }
    }

    // Walks a path in the tree defined by `invocation_id_in_call_stack` and
    // returns the last visited authorized node.
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

    fn push_frame(&mut self) {
        self.invocation_id_in_call_stack.push(None);
    }

    fn pop_frame(&mut self) {
        self.invocation_id_in_call_stack.pop();
    }

    fn is_empty(&self) -> bool {
        self.invocation_id_in_call_stack.is_empty()
    }

    // Tries to match the provided invocation to the authorized sub-invocation
    // of the current tree and push it to the call stack.
    // Returns `true` if the match has been found for the first time per current
    // frame.
    fn maybe_push_matching_invocation_frame(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        let frame_is_already_matched = match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        };
        if frame_is_already_matched {
            return Ok(false);
        }
        let mut frame_index = None;
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            for i in 0..curr_invocation.sub_invocations.len() {
                let sub_invocation = &mut curr_invocation.sub_invocations[i];
                if !sub_invocation.is_exhausted
                    && host.compare(&sub_invocation.function, function)?.is_eq()
                {
                    frame_index = Some(i);
                    sub_invocation.is_exhausted = true;
                    break;
                }
            }
        } else if !self.root_authorized_invocation.is_exhausted
            && host
                .compare(&self.root_authorized_invocation.function, &function)?
                .is_eq()
        {
            frame_index = Some(0);
            self.root_authorized_invocation.is_exhausted = true;
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

    fn has_matched_invocations_in_stack(&self) -> bool {
        self.invocation_id_in_call_stack.iter().any(|i| i.is_some())
    }
}

impl AccountAuthorizationTracker {
    fn from_authorization_entry(
        host: &Host,
        auth_entry: SorobanAuthorizationEntry,
    ) -> Result<Self, HostError> {
        let (address, nonce, signature_args) = match auth_entry.credentials {
            SorobanCredentials::SourceAccount => (None, None, vec![]),
            SorobanCredentials::Address(address_creds) => (
                Some(host.add_host_object(address_creds.address)?),
                Some((
                    address_creds.nonce,
                    address_creds.signature_expiration_ledger,
                )),
                host.scvals_to_rawvals(address_creds.signature_args.0.as_slice())?,
            ),
        };
        let is_invoker = address.is_none();
        Ok(Self {
            address,
            invocation_tracker: InvocationTracker::from_xdr(host, auth_entry.root_invocation)?,
            signature_args,
            authenticated: false,
            need_nonce: !is_invoker,
            is_invoker,
            nonce,
            is_valid: true,
        })
    }

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
            let random_nonce: i64 = rand::thread_rng().gen_range(0, i64::MAX);
            host.consume_nonce(address.metered_clone(host.budget_ref())?, random_nonce, 0)?;
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
            address: Some(address),
            invocation_tracker: InvocationTracker::new_recording(function, current_stack_len),
            signature_args: Default::default(),
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
    fn maybe_authorize_invocation(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        if !self.is_valid {
            return Ok(false);
        }
        if !self
            .invocation_tracker
            .maybe_push_matching_invocation_frame(host, function)?
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
                            &[err.error.to_val()],
                        )
                    } else {
                        err
                    }
                })
                .and_then(|_| self.verify_nonce(host));
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
    fn record_invocation(
        &mut self,
        host: &Host,
        function: AuthorizedFunction,
    ) -> Result<(), HostError> {
        self.invocation_tracker.record_invocation(host, function)
    }

    // Build the authorization payload from the invocations recorded in this
    // tracker.
    fn get_recorded_auth_payload(&self, host: &Host) -> Result<RecordedAuthPayload, HostError> {
        Ok(RecordedAuthPayload {
            address: if let Some(addr) = self.address {
                Some(host.visit_obj(addr, |a: &ScAddress| Ok(a.clone()))?)
            } else {
                if !self.is_invoker {
                    return Err(host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected missing address for non-invoker",
                        &[],
                    ));
                }
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
    fn has_authorized_invocations_in_stack(&self) -> bool {
        self.invocation_tracker.has_matched_invocations_in_stack()
    }

    fn root_invocation_to_xdr(
        &self,
        host: &Host,
    ) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        self.invocation_tracker
            .root_authorized_invocation
            .to_xdr(host)
    }

    fn push_frame(&mut self) {
        self.invocation_tracker.push_frame();
    }

    fn pop_frame(&mut self) {
        self.invocation_tracker.pop_frame();
    }

    fn verify_nonce(&mut self, host: &Host) -> Result<(), HostError> {
        if !self.need_nonce {
            return Ok(());
        }
        self.need_nonce = false;
        if let Some((nonce, expiration_ledger)) = &self.nonce {
            if host.with_ledger_info(|li| Ok(li.sequence_number))? > *expiration_ledger {
                return Err(host.err(
                    ScErrorType::Auth,
                    ScErrorCode::InvalidInput,
                    "signature has expired",
                    &[],
                ));
            }
            if let Some(addr) = self.address {
                return host.consume_nonce(addr, *nonce, *expiration_ledger);
            }
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
                network_id: Hash(
                    host.with_ledger_info(|li| li.network_id.metered_clone(host.budget_ref()))?,
                ),
                nonce,
                signature_expiration_ledger: expiration_ledger,
                invocation: self.root_invocation_to_xdr(host)?,
            });

        host.metered_hash_xdr(&payload_preimage)
    }

    fn authenticate(&self, host: &Host) -> Result<(), HostError> {
        if self.is_invoker {
            return Ok(());
        }

        if let Some(address) = &self.address {
            let sc_addr = host.scaddress_from_address(*address)?;
            // TODO: there should also be a mode where a dummy payload is used
            // instead (for enforcing mode preflight).
            let payload = self.get_signature_payload(host)?;
            match sc_addr {
                ScAddress::Account(acc) => {
                    check_account_authentication(host, acc, &payload, &self.signature_args)?;
                }
                ScAddress::Contract(acc_contract) => {
                    check_account_contract_auth(
                        host,
                        &acc_contract,
                        &payload,
                        &self.signature_args,
                        &self.invocation_tracker.root_authorized_invocation,
                    )?;
                }
            }
            Ok(())
        } else {
            Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected missing address to authenticate",
                &[],
            ))
        }
    }

    // Emulates authentication for the recording mode.
    fn emulate_authentication(&self, host: &Host) -> Result<(), HostError> {
        if self.is_invoker {
            return Ok(());
        }
        if let Some(address) = &self.address {
            let sc_addr = host.scaddress_from_address(*address)?;
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
        } else {
            Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected missing address to emulate authentication",
                &[],
            ))
        }
    }

    #[cfg(any(test, feature = "testutils"))]
    fn is_exhausted(&self) -> bool {
        self.invocation_tracker
            .root_authorized_invocation
            .is_exhausted
    }
}

impl InvokerContractAuthorizationTracker {
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

    fn push_frame(&mut self) {
        self.invocation_tracker.push_frame();
    }

    fn pop_frame(&mut self) {
        self.invocation_tracker.pop_frame();
    }

    fn is_out_of_scope(&self) -> bool {
        self.invocation_tracker.is_empty()
    }

    fn maybe_authorize_invocation(
        &mut self,
        host: &Host,
        function: &AuthorizedFunction,
    ) -> Result<bool, HostError> {
        // Authorization is successful if function is just matched by the
        // tracker. No authentication is needed.
        self.invocation_tracker
            .maybe_push_matching_invocation_frame(host, function)
    }
}

impl Host {
    fn consume_nonce(
        &self,
        address: AddressObject,
        nonce: i64,
        expiration_ledger: u32,
    ) -> Result<(), HostError> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey { nonce });
        let sc_address = self.scaddress_from_address(address)?;
        let nonce_key = self.storage_key_for_address(
            sc_address.metered_clone(self.budget_ref())?,
            nonce_key_scval.metered_clone(self.budget_ref())?,
            xdr::ContractDataDurability::Temporary,
        );
        self.with_mut_storage(|storage| {
            if storage.has(&nonce_key, self.budget_ref())? {
                return Err(self.err(
                    ScErrorType::Auth,
                    ScErrorCode::ExistingValue,
                    "nonce already exists",
                    &[],
                ));
            }
            let body = ContractDataEntryBody::DataEntry(ContractDataEntryData {
                val: ScVal::Void,
                flags: 0,
            });
            let data = LedgerEntryData::ContractData(ContractDataEntry {
                contract: sc_address,
                key: nonce_key_scval,
                body,
                expiration_ledger_seq: expiration_ledger,
                durability: xdr::ContractDataDurability::Temporary,
            });
            let entry = LedgerEntry {
                last_modified_ledger_seq: 0,
                data,
                ext: LedgerEntryExt::V0,
            };
            storage.put(&nonce_key, &Rc::new(entry), self.budget_ref())
        })
    }
}

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
