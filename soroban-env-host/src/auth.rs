use std::collections::HashMap;
use std::rc::Rc;

use soroban_env_common::xdr::{
    ContractDataEntry, ContractDataEntryBody, ContractDataEntryData, CreateContractArgs,
    HashIdPreimage, HashIdPreimageSorobanAuthorization, LedgerEntry, LedgerEntryData,
    LedgerEntryExt, ScAddress, ScErrorCode, ScErrorType, ScNonceKey, ScSymbol, ScVal,
    SorobanAuthorizationEntry, SorobanAuthorizedContractFunction, SorobanAuthorizedFunction,
    SorobanCredentials,
};
use soroban_env_common::RawVal;

use crate::budget::Budget;
use crate::host::metered_clone::MeteredClone;
use crate::host::Frame;
use crate::native_contract::account_contract::{
    check_account_authentication, check_account_contract_auth,
};
use crate::{Host, HostError};

use super::xdr;
use super::xdr::{Hash, ScVec};

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
    trackers: Vec<AuthorizationTracker>,
    // Current call stack consisting only of the contract invocations (i.e. not
    // the host functions).
    call_stack: Vec<AuthStackFrame>,
    budget: Budget,
}

// The authorization payload recorded for an address in the recording
// authorization mode.
#[derive(Eq, PartialEq, Debug)]
pub struct RecordedAuthPayload {
    pub address: Option<ScAddress>,
    pub nonce: Option<u64>,
    pub invocation: xdr::SorobanAuthorizedInvocation,
}

// Snapshot of `AuthorizationManager` to use when performing the callstack
// rollbacks.
#[derive(Clone)]
pub struct AuthorizationManagerSnapshot {
    // AuthorizationTracker has some immutable parts, but it is safer to just
    // rollback everything. If this is an issue, then the AuthorizationTracker should
    // probably be separated into 'mutable' and 'immutable' parts.
    trackers: Vec<AuthorizationTracker>,
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

// Stores all the authorizations that are authorized by an address.
// In the enforcing mode this performs authentication and makes sure that only
// pre-authorized invocations can happen on behalf of the `address`.
// In the recording mode this will record the invocations that are authorized
// on behalf of the address.
#[derive(Clone)]
struct AuthorizationTracker {
    // Tracked address. If `None`, then lazily set to the transaction source
    // account's (i.e. invoker's) address is used.
    address: Option<ScAddress>,
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
    // Arguments representing the signature(s) made by the address to authorize
    // the invocations tracked here.
    signature_args: Vec<RawVal>,
    // Indicates whether this tracker is still valid. If invalidated once, this
    // can't be used to authorize anything anymore
    is_valid: bool,
    // Indicates whether this is a tracker for the transaction invoker.
    is_invoker: bool,
    // Indicates whether authentication has already succesfully happened.
    authenticated: bool,
    // Indicates whether nonce still needs to be verified and consumed.
    need_nonce: bool,
    // The value of nonce authorized by the address. Must match the stored
    // nonce value.
    nonce: Option<u64>,
}

#[derive(Clone)]
enum AuthStackFrame {
    Contract(ContractInvocation),
    CreateContractHostFn(CreateContractArgs),
}

#[derive(Clone)]
struct ContractInvocation {
    pub(crate) contract_address: ScAddress,
    pub(crate) function_name: ScSymbol,
}

#[derive(Clone, Eq, PartialEq)]
pub(crate) struct ContractFunction {
    pub(crate) contract_address: ScAddress,
    pub(crate) function_name: ScSymbol,
    pub(crate) args: ScVec,
}

#[derive(Clone, Eq, PartialEq)]
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

impl AuthStackFrame {
    fn to_authorized_function(
        &self,
        args: ScVec,
        budget: &Budget,
    ) -> Result<AuthorizedFunction, HostError> {
        match self {
            AuthStackFrame::Contract(contract_frame) => {
                Ok(AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: contract_frame.contract_address.metered_clone(budget)?,
                    function_name: contract_frame.function_name.metered_clone(budget)?,
                    args,
                }))
            }
            AuthStackFrame::CreateContractHostFn(args) => Ok(
                AuthorizedFunction::CreateContractHostFn(args.metered_clone(budget)?),
            ),
        }
    }
}

impl AuthorizedFunction {
    fn from_xdr(xdr_fn: SorobanAuthorizedFunction) -> Self {
        match xdr_fn {
            SorobanAuthorizedFunction::ContractFn(xdr_contract_fn) => {
                AuthorizedFunction::ContractFn(ContractFunction {
                    contract_address: xdr_contract_fn.contract_address,
                    function_name: xdr_contract_fn.function_name,
                    args: xdr_contract_fn.args,
                })
            }
            SorobanAuthorizedFunction::CreateContractHostFn(xdr_args) => {
                AuthorizedFunction::CreateContractHostFn(xdr_args)
            }
        }
    }

    fn to_xdr(&self, budget: &Budget) -> Result<SorobanAuthorizedFunction, HostError> {
        match self {
            AuthorizedFunction::ContractFn(contract_fn) => Ok(
                SorobanAuthorizedFunction::ContractFn(SorobanAuthorizedContractFunction {
                    contract_address: contract_fn.contract_address.metered_clone(budget)?,
                    function_name: contract_fn.function_name.metered_clone(budget)?,
                    args: contract_fn.args.metered_clone(budget)?,
                }),
            ),
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => {
                Ok(SorobanAuthorizedFunction::CreateContractHostFn(
                    create_contract_args.metered_clone(budget)?,
                ))
            }
        }
    }

    fn to_xdr_non_metered(&self) -> xdr::SorobanAuthorizedFunction {
        match self {
            AuthorizedFunction::ContractFn(contract_fn) => {
                SorobanAuthorizedFunction::ContractFn(SorobanAuthorizedContractFunction {
                    contract_address: contract_fn.contract_address.clone(),
                    function_name: contract_fn.function_name.clone(),
                    args: contract_fn.args.clone(),
                })
            }
            AuthorizedFunction::CreateContractHostFn(create_contract_args) => {
                SorobanAuthorizedFunction::CreateContractHostFn(create_contract_args.clone())
            }
        }
    }
}

impl AuthorizedInvocation {
    fn from_xdr(xdr_invocation: xdr::SorobanAuthorizedInvocation) -> Result<Self, HostError> {
        let sub_invocations_xdr = xdr_invocation.sub_invocations.into_vec();
        let sub_invocations = sub_invocations_xdr
            .into_iter()
            .map(AuthorizedInvocation::from_xdr)
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            function: AuthorizedFunction::from_xdr(xdr_invocation.function),
            sub_invocations,
            is_exhausted: false,
        })
    }

    fn to_xdr(&self, budget: &Budget) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        Ok(xdr::SorobanAuthorizedInvocation {
            function: self.function.to_xdr(budget)?,
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr(budget))
                .collect::<Result<Vec<xdr::SorobanAuthorizedInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from((ScErrorType::Auth, ScErrorCode::InternalError)))?,
        })
    }

    fn new_recording(function: AuthorizedFunction) -> Self {
        Self {
            function,
            sub_invocations: vec![],
            is_exhausted: true,
        }
    }

    // Non-metered conversion should only be used for the recording preflight
    // runs.
    fn to_xdr_non_metered(&self) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        Ok(xdr::SorobanAuthorizedInvocation {
            function: self.function.to_xdr_non_metered(),
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr_non_metered())
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
        Self::new_enforcing_without_authorizations(Budget::default())
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
            trackers.push(AuthorizationTracker::from_authorization_entry(
                host, auth_entry,
            )?);
        }
        Ok(Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: vec![],
            budget: host.budget_cloned(),
            trackers,
        })
    }

    // Creates a new enforcing `AuthorizationManager` that doesn't allow any
    // authorizations.
    // This is useful as a safe default mode.
    pub(crate) fn new_enforcing_without_authorizations(budget: Budget) -> Self {
        Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: vec![],
            budget,
            trackers: vec![],
        }
    }

    // Creates a new recording `AuthorizationManager`.
    // All the authorization requirements will be recorded and can then be
    // retrieved using `get_recorded_auth_payloads`.
    pub(crate) fn new_recording(budget: Budget) -> Self {
        Self {
            mode: AuthorizationMode::Recording(RecordingAuthInfo {
                tracker_by_address_handle: Default::default(),
            }),
            call_stack: vec![],
            budget,
            trackers: vec![],
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
        address_obj_handle: u32,
        address: ScAddress,
        args: ScVec,
    ) -> Result<(), HostError> {
        let curr_invocation = self.call_stack.last().ok_or_else(|| {
            host.err(
                ScErrorType::Auth,
                ScErrorCode::InternalError,
                "unexpected require_auth outside of contract",
                &[],
            )
        })?;
        self.require_auth_internal(
            host,
            address_obj_handle,
            address,
            curr_invocation.to_authorized_function(args, &self.budget)?,
        )
    }

    fn verify_contract_invoker_auth(&self, contract_id: &ScAddress) -> bool {
        if self.call_stack.len() < 2 {
            return false;
        }
        let invoker_frame = &self.call_stack[self.call_stack.len() - 2];
        if let AuthStackFrame::Contract(invoker_contract) = invoker_frame {
            return contract_id == &invoker_contract.contract_address;
        }
        return false;
    }

    fn require_auth_enforcing(
        &mut self,
        host: &Host,
        address: &ScAddress,
        function: &AuthorizedFunction,
    ) -> Result<(), HostError> {
        // Iterate all the trackers and try to find one that
        // fullfills the authorization requirement.
        for tracker in &mut self.trackers {
            let address_matches = if let Some(addr) = &tracker.address {
                addr == address
            } else {
                // Lazily fill the address for the invoker trackers,
                // so that it's possible to create the auth manager
                // without knowing the invoker beforehand and also
                // to not keep calling into `source_account` function.
                let source_addr = ScAddress::Account(host.source_account().ok_or_else(|| {
                    host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected missing invoker in auth manager",
                        &[],
                    )
                })?);
                let source_matches = &source_addr == address;
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
        address_obj_handle: u32,
        address: ScAddress,
        function: AuthorizedFunction,
    ) -> Result<(), HostError> {
        // For now we give a blanket approval of the invoker contract to any
        // calls it made, but never to the deeper calls. It's possible
        // to eventually add a capability to pre-authorize arbitrary call
        // stacks on behalf of the contract.
        if self.verify_contract_invoker_auth(&address) {
            return Ok(());
        }

        match &mut self.mode {
            AuthorizationMode::Enforcing => self.require_auth_enforcing(host, &address, &function),
            AuthorizationMode::Recording(recording_info) => {
                if let Some(tracker_id) = recording_info
                    .tracker_by_address_handle
                    .get(&address_obj_handle)
                {
                    let tracker = &mut self.trackers[*tracker_id];
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
                        return self.trackers[*tracker_id].record_invocation(host, function);
                    }
                }
                // If a tracker for the new tree doesn't exist yet, create
                // it and initialize with the current invocation.
                self.trackers.push(AuthorizationTracker::new_recording(
                    host,
                    address,
                    function,
                    self.call_stack.len(),
                )?);
                recording_info
                    .tracker_by_address_handle
                    .insert(address_obj_handle, self.trackers.len() - 1);
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
            trackers: self.trackers.clone(),
            tracker_by_address_handle,
        }
    }

    // Rolls back this `AuthorizationManager` to the snapshot state.
    pub(crate) fn rollback(&mut self, snapshot: AuthorizationManagerSnapshot) {
        self.trackers = snapshot.trackers;
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
        for tracker in &mut self.trackers {
            tracker.push_frame();
        }
    }

    // Records a new call stack frame.
    // This should be called for every `Host` `push_frame`.
    pub(crate) fn push_frame(&mut self, host: &Host, frame: &Frame) -> Result<(), HostError> {
        let (contract_id, function_name) = match frame {
            Frame::ContractVM(vm, fn_name, _) => {
                (vm.contract_id.metered_clone(&self.budget)?, *fn_name)
            }
            // Skip the top-level host function stack frames as they don't
            // contain all the necessary information.
            // Use the respective push (like
            // `push_create_contract_host_fn_frame`) functions instead to push
            // the frame with the required info.
            Frame::HostFunction(_) => return Ok(()),
            Frame::Token(id, fn_name, _) => (id.metered_clone(&self.budget)?, *fn_name),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (tc.id.clone(), tc.func),
        };
        // Currently only contracts might appear in the call stack.
        let contract_address = ScAddress::Contract(contract_id);
        let Ok(ScVal::Symbol(function_name)) = host.from_host_val(function_name.to_raw()) else {
            return Err(host.err(ScErrorType::Auth, ScErrorCode::InternalError, "frame function name conversion failed", &[]))
        };
        self.call_stack
            .push(AuthStackFrame::Contract(ContractInvocation {
                contract_address,
                function_name,
            }));
        for tracker in &mut self.trackers {
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
        for tracker in &mut self.trackers {
            tracker.pop_frame();
        }
    }

    // Returns the recorded per-address authorization payloads that would cover the
    // top-level contract function invocation in the enforcing mode.
    // Should only be called in the recording mode.
    pub(crate) fn get_recorded_auth_payloads(&self) -> Result<Vec<RecordedAuthPayload>, HostError> {
        match &self.mode {
            AuthorizationMode::Enforcing => Err(HostError::from((
                ScErrorType::Auth,
                ScErrorCode::InternalError,
            ))),
            AuthorizationMode::Recording(_) => Ok(self
                .trackers
                .iter()
                .map(|tracker| tracker.get_recorded_auth_payload())
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
                for tracker in &self.trackers {
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
                AuthorizationManager::new_enforcing_without_authorizations(self.budget.clone())
            }
            AuthorizationMode::Recording(_) => {
                AuthorizationManager::new_recording(self.budget.clone())
            }
        }
    }

    // Returns all authorizations that have been authenticated for the
    // last contract invocation.
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn get_authenticated_authorizations(
        &self,
    ) -> Vec<(ScAddress, xdr::SorobanAuthorizedInvocation)> {
        self.trackers
            .iter()
            .filter(|t| t.authenticated)
            .filter_map(|t| {
                // Ignore authorizations without an address as they are implied,
                // less useful as a test utility, and not succinctly capturable
                // in the list of tuples. This is a tradeoff between offering up
                // all authorizations vs the authorizations developers will
                // mostly care about at the benefit of making this list easier
                // to use.
                t.address.as_ref().map(|a| {
                    fn add_recursively_to_auths(
                        a: &ScAddress,
                        auths: &mut Vec<(ScAddress, xdr::SorobanAuthorizedInvocation)>,
                        i: &AuthorizedInvocation,
                    ) {
                        // Is exhausted indicates if the auth was in fact
                        // consumed by a call to require_auth.
                        if i.is_exhausted {
                            auths.push((a.clone(), i.to_xdr_non_metered().unwrap()));
                        }
                        for sub in &i.sub_invocations {
                            add_recursively_to_auths(a, auths, sub);
                        }
                    }
                    let mut auths = vec![];
                    add_recursively_to_auths(a, &mut auths, &t.root_authorized_invocation);
                    auths
                })
            })
            .flatten()
            .collect()
    }
}

impl AuthorizationTracker {
    fn from_authorization_entry(
        host: &Host,
        auth_entry: SorobanAuthorizationEntry,
    ) -> Result<Self, HostError> {
        let (address, nonce, signature_args) = match auth_entry.credentials {
            SorobanCredentials::SourceAccount => (None, None, vec![]),
            SorobanCredentials::Address(address_creds) => (
                Some(address_creds.address),
                Some(address_creds.nonce),
                host.scvals_to_rawvals(address_creds.signature_args.0.as_slice())?,
            ),
        };
        let is_invoker = address.is_none();
        Ok(Self {
            address,
            root_authorized_invocation: AuthorizedInvocation::from_xdr(auth_entry.root_invocation)?,
            signature_args,
            authenticated: false,
            need_nonce: !is_invoker,
            is_invoker,
            nonce,
            invocation_id_in_call_stack: vec![],
            is_valid: true,
        })
    }

    fn new_recording(
        host: &Host,
        address: ScAddress,
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
        let is_invoker = if let Some(source_acc) = host.source_account() {
            ScAddress::Account(source_acc) == address
        } else {
            false
        };
        let nonce = if !is_invoker {
            Some(host.read_and_consume_nonce(address.metered_clone(host.budget_ref())?, &function)?)
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
            root_authorized_invocation: AuthorizedInvocation::new_recording(function),
            invocation_id_in_call_stack,
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
        let frame_is_already_authorized = match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        };
        if frame_is_already_authorized || !self.maybe_push_matching_invocation_frame(function) {
            // The call isn't found in the currently tracked tree or is already
            // authorized in it.
            // That doesn't necessarily mean it's unauthorized (it can be
            // authorized in a different tracker).
            return Ok(false);
        }
        if !self.authenticated {
            let authenticate_res = self
                .authenticate(host)
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

    // Build the authorization payload from the invocations recorded in this
    // tracker.
    fn get_recorded_auth_payload(&self) -> Result<RecordedAuthPayload, HostError> {
        Ok(RecordedAuthPayload {
            address: if self.is_invoker {
                None
            } else {
                self.address.clone()
            },
            invocation: self.root_authorized_invocation.to_xdr_non_metered()?,
            nonce: self.nonce,
        })
    }

    // Checks if there is at least one authorized invocation in the current call
    // stack.
    fn has_authorized_invocations_in_stack(&self) -> bool {
        self.invocation_id_in_call_stack.iter().any(|i| i.is_some())
    }

    fn invocation_to_xdr(
        &self,
        budget: &Budget,
    ) -> Result<xdr::SorobanAuthorizedInvocation, HostError> {
        self.root_authorized_invocation.to_xdr(budget)
    }

    fn push_frame(&mut self) {
        self.invocation_id_in_call_stack.push(None);
    }

    fn pop_frame(&mut self) {
        self.invocation_id_in_call_stack.pop();
    }

    // Consumes nonce if the nonce check is still needed.
    // Returns nonce if it has been consumed.
    // Note, that for the invoker nonce is never needed.
    fn maybe_consume_nonce(&mut self, host: &Host) -> Result<Option<u64>, HostError> {
        if !self.need_nonce {
            return Ok(None);
        }
        self.need_nonce = false;
        if let Some(addr) = &self.address {
            Ok(Some(host.read_and_consume_nonce(
                addr.metered_clone(host.budget_ref())?,
                &self.root_authorized_invocation.function,
            )?))
        } else {
            Ok(None)
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

    // Tries to match the provided invocation to the authorized sub-invocation
    // of the current tree and push it to the call stack.
    // Returns `true` if the match has been found.
    fn maybe_push_matching_invocation_frame(&mut self, function: &AuthorizedFunction) -> bool {
        let mut frame_index = None;
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            for i in 0..curr_invocation.sub_invocations.len() {
                let sub_invocation = &mut curr_invocation.sub_invocations[i];
                // TODO: the equality comparison should be metered.
                if !sub_invocation.is_exhausted && &sub_invocation.function == function {
                    frame_index = Some(i);
                    sub_invocation.is_exhausted = true;
                    break;
                }
            }
        } else if !self.root_authorized_invocation.is_exhausted
            // TODO: the equality comparison should be metered.
            && &self.root_authorized_invocation.function == function
        {
            frame_index = Some(0);
            self.root_authorized_invocation.is_exhausted = true;
        }
        if frame_index.is_some() {
            *self.invocation_id_in_call_stack.last_mut().unwrap() = frame_index;
        }
        frame_index.is_some()
    }

    fn verify_nonce(&mut self, host: &Host) -> Result<(), HostError> {
        let nonce_is_correct = if let Some(nonce) = self.maybe_consume_nonce(host)? {
            if let Some(tracker_nonce) = self.nonce {
                tracker_nonce == nonce
            } else {
                // If the nonce isn't set in the tracker, but is required, then
                // it's incorrect.
                false
            }
        } else {
            // Nonce is either already checked or not needed in the first place.
            true
        };
        if nonce_is_correct {
            Ok(())
        } else {
            Err(host.err(
                ScErrorType::Auth,
                ScErrorCode::InvalidInput,
                "nonce is incorrect",
                &[],
            ))
        }
    }

    // Computes the payload that has to be signed in order to authenticate
    // the authorized invocation tree corresponding to this tracker.
    fn get_signature_payload(&self, host: &Host) -> Result<[u8; 32], HostError> {
        let payload_preimage =
            HashIdPreimage::SorobanAuthorization(HashIdPreimageSorobanAuthorization {
                network_id: Hash(
                    host.with_ledger_info(|li| li.network_id.metered_clone(host.budget_ref()))?,
                ),
                invocation: self.invocation_to_xdr(host.budget_ref())?,
                nonce: self.nonce.ok_or_else(|| {
                    host.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected missing nonce",
                        &[],
                    )
                })?,
            });

        host.metered_hash_xdr(&payload_preimage)
    }

    fn authenticate(&self, host: &Host) -> Result<(), HostError> {
        if self.is_invoker {
            return Ok(());
        }
        if let Some(address) = &self.address {
            // TODO: there should also be a mode where a dummy payload is used
            // instead (for enforcing mode preflight).
            let payload = self.get_signature_payload(host)?;
            match address {
                ScAddress::Account(acc) => {
                    check_account_authentication(host, acc, &payload, &self.signature_args)?;
                }
                ScAddress::Contract(acc_contract) => {
                    check_account_contract_auth(
                        host,
                        acc_contract,
                        &payload,
                        &self.signature_args,
                        &self.root_authorized_invocation,
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
            // Compute the real payload for the sake of metering, but don't use it.
            let _payload = self.get_signature_payload(host)?;
            match address {
                ScAddress::Account(acc) => {
                    let _account = host.load_account(acc.clone())?;
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
}

impl Host {
    #[cfg(test)]
    pub(crate) fn read_nonce(
        &self,
        address: ScAddress,
        function: &AuthorizedFunction,
    ) -> Result<u64, HostError> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey {
            nonce_address: address,
        });
        let contract_id = match function {
            AuthorizedFunction::ContractFn(contract_fn) => self.contract_id_from_scaddress(
                contract_fn
                    .contract_address
                    .metered_clone(self.budget_ref())?,
            )?,
            AuthorizedFunction::CreateContractHostFn(_) => {
                // Use the special contract id for all the host fn nonces for
                // now. This should soon be replaced by non-autoincrement nonce
                // that is independent of the authorized function.
                Hash([255; 32])
            }
        };
        let nonce_key = self.storage_key_for_contract(
            contract_id,
            nonce_key_scval.metered_clone(self.budget_ref())?,
            xdr::ContractDataType::Unique,
        );
        let curr_nonce: u64 =
            if self.with_mut_storage(|storage| storage.has(&nonce_key, self.budget_ref()))? {
                let entry =
                    self.with_mut_storage(|storage| storage.get(&nonce_key, self.budget_ref()))?;
                match &entry.data {
                    LedgerEntryData::ContractData(data_entry) => match &data_entry.body {
                        ContractDataEntryBody::DataEntry(data) => match data.val {
                            ScVal::U64(val) => val,
                            _ => {
                                return Err(self.err(
                                    ScErrorType::Auth,
                                    ScErrorCode::UnexpectedType,
                                    "unexpected nonce entry type",
                                    &[],
                                ));
                            }
                        },
                        _ => {
                            return Err(self.err(
                                ScErrorType::Auth,
                                ScErrorCode::UnexpectedType,
                                "expected DataEntry type",
                                &[],
                            ));
                        }
                    },
                    _ => {
                        return Err(self.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "unexpected missing nonce entry",
                            &[],
                        ))
                    }
                }
            } else {
                0
            };
        Ok(curr_nonce)
    }

    fn read_and_consume_nonce(
        &self,
        address: ScAddress,
        function: &AuthorizedFunction,
    ) -> Result<u64, HostError> {
        let nonce_key_scval = ScVal::LedgerKeyNonce(ScNonceKey {
            nonce_address: address,
        });
        let contract_id = match function {
            AuthorizedFunction::ContractFn(contract_fn) => self.contract_id_from_scaddress(
                contract_fn
                    .contract_address
                    .metered_clone(self.budget_ref())?,
            )?,
            AuthorizedFunction::CreateContractHostFn(_) => {
                // Use the special contract id for all the host fn nonces for
                // now. This should soon be replaced by non-autoincrement nonce
                // that is independent of the authorized function.
                Hash([255; 32])
            }
        };
        let nonce_key = self.storage_key_for_contract(
            contract_id.metered_clone(self.budget_ref())?,
            nonce_key_scval.metered_clone(self.budget_ref())?,
            xdr::ContractDataType::Unique,
        );

        if self.with_mut_storage(|storage| storage.has(&nonce_key, self.budget_ref()))? {
            let mut entry = (*self
                .with_mut_storage(|storage| storage.get(&nonce_key, self.budget_ref()))?)
            .clone();
            match entry.data {
                LedgerEntryData::ContractData(ref mut data_entry) => match data_entry.body {
                    ContractDataEntryBody::DataEntry(ref mut data) => match &mut data.val {
                        ScVal::U64(v) => {
                            let curr_nonce = *v;
                            *v = *v + 1;

                            self.with_mut_storage(|storage| {
                                storage.put(&nonce_key, &Rc::new(entry), self.budget_ref())
                            })?;
                            Ok(curr_nonce)
                        }
                        _ => {
                            return Err(self.err(
                                ScErrorType::Auth,
                                ScErrorCode::UnexpectedType,
                                "unexpected nonce entry type",
                                &[],
                            ));
                        }
                    },
                    _ => {
                        return Err(self.err(
                            ScErrorType::Auth,
                            ScErrorCode::InternalError,
                            "expected DataEntry",
                            &[],
                        ))
                    }
                },
                _ => {
                    return Err(self.err(
                        ScErrorType::Auth,
                        ScErrorCode::InternalError,
                        "unexpected missing nonce entry",
                        &[],
                    ))
                }
            }
        } else {
            let body = ContractDataEntryBody::DataEntry(ContractDataEntryData {
                val: ScVal::U64(1),
                flags: 0,
            });

            let data = LedgerEntryData::ContractData(ContractDataEntry {
                contract_id: contract_id.metered_clone(self.budget_ref())?,
                key: nonce_key_scval,
                body,
                expiration_ledger_seq: self.with_ledger_info(|li| Ok(li.sequence_number))? + 4096, //TODO: use constant? Ideally we'd use the settings
                type_: xdr::ContractDataType::Unique,
            });
            let entry = LedgerEntry {
                last_modified_ledger_seq: 0,
                data,
                ext: LedgerEntryExt::V0,
            };
            self.with_mut_storage(|storage| {
                storage.put(&nonce_key, &Rc::new(entry), self.budget_ref())
            })?;

            Ok(0)
        }
    }
}
