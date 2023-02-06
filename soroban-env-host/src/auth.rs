use std::collections::HashMap;

use soroban_env_common::xdr::{
    ContractAuth, ContractDataEntry, HashIdPreimage, HashIdPreimageContractAuth, LedgerEntry,
    LedgerEntryData, LedgerEntryExt, ScAddress, ScObject, ScVal,
};
use soroban_env_common::{RawVal, Symbol};

use crate::budget::Budget;
use crate::host::metered_clone::MeteredClone;
use crate::host::Frame;
use crate::native_contract::account_contract::{
    check_account_authentication, check_account_contract_auth,
};
use crate::{Host, HostError};

use super::xdr;
use super::xdr::{Hash, ScUnknownErrorCode, ScVec};

// Authorization manager encapsulates host-based authentication & authorization
// framework.
// This supports enforcing authentication & authorization of the contract
// invocation trees as well as recording the authorization requirements in
// simulated environments (such as tests or preflight).
#[derive(Clone)]
pub(crate) struct AuthorizationManager {
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
    call_stack: Vec<ContractInvocation>,
    budget: Budget,
}

// The authorization payload recorded for an address in the recording
// authorization mode.
#[derive(Eq, PartialEq, Debug)]
pub struct RecordedAuthPayload {
    pub address: Option<ScAddress>,
    pub nonce: Option<u64>,
    pub invocation: xdr::AuthorizedInvocation,
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

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ContractInvocation {
    pub(crate) contract_id: Hash,
    pub(crate) function_name: Symbol,
}

// A single node in the authorized invocation tree.
// This represents an invocation and all it's authorized sub-invocations.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct AuthorizedInvocation {
    pub(crate) contract_id: Hash,
    pub(crate) function_name: Symbol,
    pub(crate) args: ScVec,
    pub(crate) sub_invocations: Vec<AuthorizedInvocation>,
    // Indicates that this invocation has been already used in the
    // enforcing mode. Exhausted authorizations can't be reused.
    // In the recording mode this is immediately set to `true` (as the
    // authorizations are recorded when the actually happen).
    is_exhausted: bool,
}

impl AuthorizedInvocation {
    fn from_xdr(xdr_invocation: xdr::AuthorizedInvocation) -> Result<Self, HostError> {
        let sub_invocations_xdr = xdr_invocation.sub_invocations.to_vec();
        let sub_invocations = sub_invocations_xdr
            .into_iter()
            .map(|i| AuthorizedInvocation::from_xdr(i))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self {
            contract_id: xdr_invocation.contract_id.clone(),
            function_name: Symbol::try_from(xdr_invocation.function_name)?,
            args: xdr_invocation.args.clone(),
            sub_invocations,
            is_exhausted: false,
        })
    }

    fn to_xdr(&self, budget: &Budget) -> Result<xdr::AuthorizedInvocation, HostError> {
        Ok(xdr::AuthorizedInvocation {
            contract_id: self.contract_id.metered_clone(budget)?,
            // This ideally should be infallible
            function_name: self
                .function_name
                .to_string()
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
            args: self.args.metered_clone(budget)?,
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr(budget))
                .collect::<Result<Vec<xdr::AuthorizedInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
        })
    }

    fn new_recording(contract_id: &Hash, function_name: Symbol, args: ScVec) -> Self {
        Self {
            contract_id: contract_id.clone(),
            function_name,
            args,
            sub_invocations: vec![],
            is_exhausted: true,
        }
    }

    // Non-metered conversion should only be used for the recording preflight
    // runs.
    fn to_xdr_non_metered(&self) -> Result<xdr::AuthorizedInvocation, HostError> {
        Ok(xdr::AuthorizedInvocation {
            contract_id: self.contract_id.clone(),
            // This ideally should be infallible
            function_name: self
                .function_name
                .to_string()
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
            args: self.args.clone(),
            sub_invocations: self
                .sub_invocations
                .iter()
                .map(|i| i.to_xdr_non_metered())
                .collect::<Result<Vec<xdr::AuthorizedInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
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
    // This should be created once per top-level contract invocation.
    pub(crate) fn new_enforcing(
        host: &Host,
        auth_entries: Vec<ContractAuth>,
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
    // retrieved using `get_recorded_signature_payloads`.
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
        if let ScAddress::Contract(contract_addr) = &address {
            // For now we give a blanket approval of the invoker contract to any
            // calls it made, but never to the deeper calls. It's possible
            // to eventually add a capability to pre-authorize arbitrary call
            // stacks on behalf of the contract.
            if let Ok(invoker_contract) = host.get_invoking_contract_internal() {
                if &invoker_contract == contract_addr {
                    return Ok(());
                }
            }
        }

        if let Some(curr_invocation) = self.call_stack.last() {
            match &mut self.mode {
                AuthorizationMode::Enforcing => {
                    // Iterate all the trackers and try to find one that
                    // fullfills the authorization requirement.
                    for tracker in &mut self.trackers {
                        let address_matches = if let Some(addr) = &tracker.address {
                            addr == &address
                        } else {
                            // Lazily fill the address for the invoker trackers,
                            // so that it's possible to create the auth manager
                            // without knowing the invoker beforehand and also
                            // to not keep calling into `source_account` function.
                            let source_addr = ScAddress::Account(host.source_account()?);
                            let source_matches = source_addr == address;
                            tracker.address = Some(source_addr);
                            source_matches
                        };
                        // If address doesn't match, just skip the tracker.
                        if address_matches {
                            match tracker.maybe_authorize_invocation(
                                host,
                                &curr_invocation.contract_id,
                                curr_invocation.function_name,
                                &args,
                            ) {
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
                    Err(host.err_general("invocation is not authorized"))
                }
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
                            return self.trackers[*tracker_id].record_invocation(
                                host,
                                &curr_invocation.contract_id,
                                curr_invocation.function_name,
                                args,
                            );
                        }
                    }
                    // If a tracker for the new tree doesn't exist yet, create
                    // it and initialize with the current invocation.
                    self.trackers.push(AuthorizationTracker::new_recording(
                        host,
                        address,
                        &curr_invocation.contract_id,
                        curr_invocation.function_name,
                        args,
                        self.call_stack.len(),
                    )?);
                    recording_info
                        .tracker_by_address_handle
                        .insert(address_obj_handle, self.trackers.len() - 1);
                    Ok(())
                }
            }
        } else {
            // This would be a bug
            Err(ScUnknownErrorCode::General.into())
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

    // Records a new call stack frame.
    // This should be called for every `Host` `push_frame`.
    pub(crate) fn push_frame(&mut self, frame: &Frame) -> Result<(), HostError> {
        let (contract_id, function_name) = match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm, fn_name, _) => {
                (vm.contract_id.metered_clone(&self.budget)?, fn_name.clone())
            }
            // Just skip the host function stack frames for now.
            // We could also make this included into the authorized stack to
            // generalize all the host function invocations.
            Frame::HostFunction(_) => return Ok(()),
            Frame::Token(id, fn_name, _) => (id.metered_clone(&self.budget)?, fn_name.clone()),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (tc.id.clone(), tc.func.clone()),
        };
        self.call_stack.push(ContractInvocation {
            contract_id,
            function_name,
        });
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
            AuthorizationMode::Enforcing => return Err(ScUnknownErrorCode::General.into()),
            AuthorizationMode::Recording(_recording_info) => Ok(self
                .trackers
                .iter()
                .map(|tracker| tracker.get_recorded_auth_payload())
                .collect::<Result<Vec<RecordedAuthPayload>, HostError>>()?),
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

    // Verify that the top-level authorization has happened for the given
    // address and invocation arguments.
    // This also keeps track of verifications that already happened.
    #[cfg(any(test, feature = "testutils"))]
    pub(crate) fn verify_top_authorization(
        &mut self,
        address: &ScAddress,
        contract_id: &Hash,
        function_name: &Symbol,
        args: &ScVec,
    ) -> bool {
        match &mut self.mode {
            AuthorizationMode::Enforcing => {
                panic!("verifying the authorization is only available for recording-mode auth")
            }
            AuthorizationMode::Recording(_) => {
                for tracker in &mut self.trackers {
                    if tracker.verify_top_authorization(address, contract_id, function_name, args) {
                        return true;
                    }
                }
                return false;
            }
        }
    }
}

impl AuthorizationTracker {
    fn from_authorization_entry(host: &Host, auth_entry: ContractAuth) -> Result<Self, HostError> {
        let is_invoker = auth_entry.address_with_nonce.is_none();
        let (address, nonce) = if let Some(address_with_nonce) = auth_entry.address_with_nonce {
            (
                Some(address_with_nonce.address),
                Some(address_with_nonce.nonce),
            )
        } else {
            (None, None)
        };
        Ok(Self {
            address,
            root_authorized_invocation: AuthorizedInvocation::from_xdr(auth_entry.root_invocation)?,
            signature_args: host.scvals_to_rawvals(auth_entry.signature_args.0.as_slice())?,
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
        contract_id: &Hash,
        function_name: Symbol,
        args: ScVec,
        current_stack_len: usize,
    ) -> Result<Self, HostError> {
        if current_stack_len == 0 {
            // This would be a bug.
            return Err(host.err_general("unexpected empty stack in recording auth"));
        }
        // If the invoker account is known, set it to `None`, so that the final
        // recorded payload wouldn't contain the address. This makes it easier
        // to use more optimal payload when only invoker auth is used.
        let is_invoker = if let Ok(source_acc) = host.source_account() {
            ScAddress::Account(source_acc) == address
        } else {
            false
        };
        let mut nonce = None;

        let address = if is_invoker {
            None
        } else {
            nonce = Some(host.read_and_consume_nonce(&contract_id, &address)?);
            Some(address)
        };
        let is_invoker = address.is_none();
        // Create the stack of `None` leading to the current invocation to
        // represent invocations that didn't need authorization on behalf of
        // the tracked address.
        let mut invocation_id_in_call_stack = vec![None; current_stack_len - 1];
        // Add the id for the current(root) invocation.
        invocation_id_in_call_stack.push(Some(0));
        Ok(Self {
            address,
            root_authorized_invocation: AuthorizedInvocation::new_recording(
                contract_id,
                function_name,
                args,
            ),
            invocation_id_in_call_stack,
            signature_args: Default::default(),
            is_valid: true,
            authenticated: true,
            need_nonce: !is_invoker,
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
        contract_id: &Hash,
        function_name: Symbol,
        args: &ScVec,
    ) -> Result<bool, HostError> {
        if !self.is_valid {
            return Ok(false);
        }
        let frame_is_already_authorized = match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        };
        if frame_is_already_authorized
            || !self.maybe_push_matching_invocation_frame(contract_id, function_name, args)
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
                .and_then(|_| self.verify_nonce(host));
            if let Some(err) = authenticate_res.err() {
                self.is_valid = false;
                return Err(err);
            }
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
        contract_id: &Hash,
        function_name: Symbol,
        args: ScVec,
    ) -> Result<(), HostError> {
        let frame_is_already_authorized = match self.invocation_id_in_call_stack.last() {
            Some(Some(_)) => true,
            _ => false,
        };
        if frame_is_already_authorized {
            return Err(host.err_general("duplicate authorizations are not allowed"));
        }
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            curr_invocation
                .sub_invocations
                .push(AuthorizedInvocation::new_recording(
                    contract_id,
                    function_name,
                    args,
                ));
            *self.invocation_id_in_call_stack.last_mut().unwrap() =
                Some(curr_invocation.sub_invocations.len() - 1);
        } else {
            // This would be a bug
            return Err(host.err_general("unexpected missing authorized invocation"));
        }
        Ok(())
    }

    // Build the authorization payload from the invocations recorded in this
    // tracker.
    fn get_recorded_auth_payload(&self) -> Result<RecordedAuthPayload, HostError> {
        Ok(RecordedAuthPayload {
            address: self.address.clone(),
            invocation: self.root_authorized_invocation.to_xdr_non_metered()?,
            nonce: self.nonce,
        })
    }

    // Checks if there is at least one authorized invocation in the current call
    // stack.
    fn has_authorized_invocations_in_stack(&self) -> bool {
        self.invocation_id_in_call_stack.iter().any(|i| i.is_some())
    }

    fn invocation_to_xdr(&self, budget: &Budget) -> Result<xdr::AuthorizedInvocation, HostError> {
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
                &self.root_authorized_invocation.contract_id,
                addr,
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
    fn maybe_push_matching_invocation_frame(
        &mut self,
        contract_id: &Hash,
        function_name: Symbol,
        args: &ScVec,
    ) -> bool {
        let mut frame_index = None;
        if let Some(curr_invocation) = self.last_authorized_invocation_mut() {
            for i in 0..curr_invocation.sub_invocations.len() {
                let sub_invocation = &mut curr_invocation.sub_invocations[i];
                if !sub_invocation.is_exhausted
                    && &sub_invocation.contract_id == contract_id
                    && sub_invocation.function_name == function_name
                    && &sub_invocation.args == args
                {
                    frame_index = Some(i);
                    sub_invocation.is_exhausted = true;
                    break;
                }
            }
        } else {
            if !self.root_authorized_invocation.is_exhausted
                && &self.root_authorized_invocation.contract_id == contract_id
                && self.root_authorized_invocation.function_name == function_name
                && &self.root_authorized_invocation.args == args
            {
                frame_index = Some(0);
                self.root_authorized_invocation.is_exhausted = true;
            }
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
            Err(ScUnknownErrorCode::General.into())
        }
    }

    // Computes the payload that has to be signed in order to authenticate
    // the authorized invocation tree corresponding to this tracker.
    fn get_signature_payload(&self, host: &Host) -> Result<[u8; 32], HostError> {
        let payload_preimage = HashIdPreimage::ContractAuth(HashIdPreimageContractAuth {
            network_id: Hash(
                host.with_ledger_info(|li| li.network_id.metered_clone(host.budget_ref()))?,
            ),
            invocation: self.invocation_to_xdr(host.budget_ref())?,
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
                    check_account_authentication(host, &acc, &payload, &self.signature_args)?;
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
        } else {
            return Err(host.err_general("missing address to authenticate"));
        }
        Ok(())
    }

    // Checks whether the provided top-level authorized invocation happened
    // for this tracker.
    // This also makes sure double verification of the same invocation is not
    // possible.
    #[cfg(any(test, feature = "testutils"))]
    fn verify_top_authorization(
        &mut self,
        address: &ScAddress,
        contract_id: &Hash,
        function_name: &Symbol,
        args: &ScVec,
    ) -> bool {
        // The recording invariant is that every recorded authorization is
        // immediately exhausted, so during the verification we revert the
        // 'exhausted' flags back to 'false' values in order to prevent
        // verifying the same authorization twice.
        if !self.root_authorized_invocation.is_exhausted {
            return false;
        }
        let is_matching = self.address.as_ref().unwrap() == address
            && &self.root_authorized_invocation.contract_id == contract_id
            && &self.root_authorized_invocation.function_name == function_name
            && &self.root_authorized_invocation.args == args;
        if is_matching {
            self.root_authorized_invocation.is_exhausted = false;
        }
        is_matching
    }
}

impl Host {
    #[cfg(test)]
    pub(crate) fn read_nonce(
        &self,
        contract_id: &Hash,
        address: &ScAddress,
    ) -> Result<u64, HostError> {
        let nonce_key_scval = ScVal::Object(Some(ScObject::NonceKey(
            address.metered_clone(self.budget_ref())?,
        )));
        let nonce_key = self.storage_key_for_contract(
            contract_id.metered_clone(self.budget_ref())?,
            nonce_key_scval,
        );
        let curr_nonce: u64 =
            if self.with_mut_storage(|storage| storage.has(&nonce_key, self.budget_ref()))? {
                let sc_val = self.with_mut_storage(|storage| {
                    match storage.get(&nonce_key, self.budget_ref())?.data {
                        LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
                        _ => Err(self.err_general("unexpected missing nonce entry")),
                    }
                })?;
                match sc_val {
                    ScVal::Object(Some(ScObject::U64(val))) => val,
                    _ => {
                        return Err(self.err_general("unexpected nonce entry type"));
                    }
                }
            } else {
                0
            };
        Ok(curr_nonce)
    }

    fn read_and_consume_nonce(
        &self,
        contract_id: &Hash,
        address: &ScAddress,
    ) -> Result<u64, HostError> {
        let nonce_key_scval = ScVal::Object(Some(ScObject::NonceKey(
            address.metered_clone(self.budget_ref())?,
        )));
        let nonce_key = self.storage_key_for_contract(
            contract_id.metered_clone(self.budget_ref())?,
            nonce_key_scval.clone(),
        );
        let curr_nonce: u64 =
            if self.with_mut_storage(|storage| storage.has(&nonce_key, self.budget_ref()))? {
                let sc_val = self.with_mut_storage(|storage| {
                    match storage.get(&nonce_key, self.budget_ref())?.data {
                        LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
                        _ => Err(self.err_general("unexpected missing nonce entry")),
                    }
                })?;
                match sc_val {
                    ScVal::Object(Some(ScObject::U64(val))) => val,
                    _ => {
                        return Err(self.err_general("unexpected nonce entry type"));
                    }
                }
            } else {
                0
            };
        let data = LedgerEntryData::ContractData(ContractDataEntry {
            contract_id: contract_id.metered_clone(self.budget_ref())?,
            key: nonce_key_scval,
            val: ScVal::Object(Some(ScObject::U64(curr_nonce + 1))),
        });
        let entry = LedgerEntry {
            last_modified_ledger_seq: 0,
            data,
            ext: LedgerEntryExt::V0,
        };
        self.with_mut_storage(|storage| storage.put(&nonce_key, &entry, self.budget_ref()))?;
        Ok(curr_nonce)
    }
}
