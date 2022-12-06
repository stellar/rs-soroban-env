use std::cmp::Ordering;

use soroban_env_common::xdr::{
    ContractDataEntry, EnvelopeType, HashIdPreimage, HashIdPreimageContractAuth, LedgerEntry,
    LedgerEntryData, LedgerEntryExt, ScAccount, ScAccountId, ScAddress, ScObject, ScVal, StringM,
};
use soroban_env_common::{CheckedEnv, RawVal, Symbol};

use crate::budget::Budget;
use crate::host::metered_clone::MeteredClone;
use crate::host::Frame;
use crate::native_contract::account_contract::check_account_authentication;
use crate::{Host, HostError};

use super::xdr;
use super::xdr::{Hash, ScUnknownErrorCode, ScVec, Uint256};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum HostAccount {
    AbstractAccount(AbstractAccountHandle),
    InvokerContract(Hash),
}

#[derive(Clone, PartialEq, Eq, PartialOrd)]
pub(crate) struct AbstractAccountHandle {
    pub(crate) address: ScAddress,
    index: u32,
}

#[derive(Clone)]
struct AbstractAccount {
    account_id: ScAccountId,
    address: ScAddress,
    authorized_invocations: Vec<AuthorizedInvocation>,
    signature_args: Vec<RawVal>,
    authorized_for_frame: Vec<Option<bool>>,
    authenticated: bool,
    need_nonce: bool,
    #[cfg(feature = "testutils")]
    last_recorded_invocations: Vec<AuthorizedInvocation>,
}

#[derive(Clone, PartialEq, Eq)]
enum AuthorizationMode {
    Enforcing,
    Recording,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct ContractInvocation {
    pub(crate) contract_id: Hash,
    pub(crate) function_name: Symbol,
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
struct AuthorizedInvocation {
    call_stack: Vec<ContractInvocation>,
    top_args: ScVec,
    nonce: Option<u64>,
}

#[derive(Clone)]
pub struct AuthorizationManager {
    mode: AuthorizationMode,
    accounts: Vec<AbstractAccount>,
    call_stack: Vec<ContractInvocation>,
    budget: Budget,
}

// This is to just be able to be a member of HostImpl and actually should never
// be used.
impl Default for AuthorizationManager {
    fn default() -> Self {
        Self::new_enforcing(Budget::default())
    }
}

impl ContractInvocation {
    fn from_xdr(xdr_invocation: xdr::ContractInvocation) -> Result<Self, HostError> {
        Ok(Self {
            contract_id: xdr_invocation.contract_id,
            function_name: Symbol::try_from(xdr_invocation.function_name)?,
        })
    }

    fn to_xdr(&self, budget: &Budget) -> Result<xdr::ContractInvocation, HostError> {
        Ok(xdr::ContractInvocation {
            contract_id: self.contract_id.metered_clone(budget)?,
            function_name: self
                .function_name
                .to_string()
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
        })
    }
}

impl AuthorizedInvocation {
    fn from_xdr(xdr_invocation: xdr::AuthorizedInvocation) -> Result<Self, HostError> {
        let call_stack = xdr_invocation
            .call_stack
            .into_vec()
            .into_iter()
            .map(|x| ContractInvocation::from_xdr(x))
            .collect::<Result<Vec<_>, _>>()?;
        if call_stack.is_empty() {
            return Err(ScUnknownErrorCode::General.into());
        }
        Ok(Self {
            call_stack,
            top_args: xdr_invocation.top_args,
            nonce: xdr_invocation.nonce,
        })
    }

    fn to_xdr(&self, budget: &Budget) -> Result<xdr::AuthorizedInvocation, HostError> {
        Ok(xdr::AuthorizedInvocation {
            call_stack: self
                .call_stack
                .iter()
                .map(|c| c.to_xdr(budget))
                .collect::<Result<Vec<xdr::ContractInvocation>, HostError>>()?
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
            // TODO: should be metered?
            top_args: self.top_args.clone(),
            nonce: self.nonce.clone(),
        })
    }

    fn new(
        call_stack: Vec<ContractInvocation>,
        top_args: ScVec,
        nonce: Option<u64>,
    ) -> Result<Self, HostError> {
        if call_stack.is_empty() {
            return Err(ScUnknownErrorCode::General.into());
        }
        Ok(Self {
            call_stack,
            top_args,
            nonce,
        })
    }

    fn eq_without_nonce(&self, other: &AuthorizedInvocation) -> bool {
        return self.call_stack == other.call_stack && self.top_args == other.top_args;
    }
}

impl AuthorizationManager {
    pub fn new_enforcing(budget: Budget) -> Self {
        Self {
            mode: AuthorizationMode::Enforcing,
            call_stack: vec![],
            budget,
            accounts: vec![],
        }
    }

    pub fn new_recording(budget: Budget) -> Self {
        Self {
            mode: AuthorizationMode::Recording,
            call_stack: vec![],
            budget,
            accounts: vec![],
        }
    }

    #[cfg(feature = "testutils")]
    pub(crate) fn reset(&mut self, host: &Host) -> Result<(), HostError> {
        match self.mode {
            // The usefullness of the enforcing mode in tests is questionable,
            // but in any case the right thing is to invalidate all the accounts,
            // so that no authorization persists. This also has to be accompanied
            // by the AccountHandle host object invalidation - not sure how to
            // properly do that (maybe store the ref to AuthorizationManager and
            // change the manager instance every reset?).
            AuthorizationMode::Enforcing => {
                self.accounts.clear();
            }
            AuthorizationMode::Recording => {
                for account in &mut self.accounts {
                    account.reset_recording(host)?;
                }
            }
        };
        self.call_stack.clear();
        Ok(())
    }

    pub(crate) fn add_account(
        &mut self,
        host: &Host,
        sc_account: ScAccount,
    ) -> Result<HostAccount, HostError> {
        let address = match &sc_account.account_id {
            ScAccountId::BuiltinClassicAccount(acc) => {
                ScAddress::ClassicAccount(acc.metered_clone(&self.budget)?)
            }
            ScAccountId::BuiltinEd25519(key) => {
                ScAddress::Ed25519(key.metered_clone(&self.budget)?)
            }
            ScAccountId::BuiltinInvoker => ScAddress::ClassicAccount(host.source_account()?),
            ScAccountId::GenericAccount(id) => ScAddress::Contract(id.metered_clone(&self.budget)?),
        };
        self.accounts.push(AbstractAccount::from_xdr(
            host,
            sc_account,
            address.metered_clone(&self.budget)?,
        )?);
        let index = self.accounts.len() - 1;

        Ok(HostAccount::AbstractAccount(AbstractAccountHandle {
            index: index
                .try_into()
                .map_err(|_| HostError::from(ScUnknownErrorCode::General))?,
            address,
        }))
    }

    pub(crate) fn push_frame(&mut self, frame: &Frame) -> Result<(), HostError> {
        let (contract_id, function_name) = match frame {
            #[cfg(feature = "vm")]
            Frame::ContractVM(vm, fn_name) => {
                (vm.contract_id.metered_clone(&self.budget)?, fn_name.clone())
            }
            // TODO: we could also make this included into the stack to generalize
            // auth to host fn invocations too; not sure how to properly identify
            // them though
            Frame::HostFunction(_) => return Ok(()),
            Frame::Token(id, fn_name) => (id.metered_clone(&self.budget)?, fn_name.clone()),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (tc.id.clone(), tc.func.clone()),
        };
        self.call_stack.push(ContractInvocation {
            contract_id,
            function_name,
        });
        for account in &mut self.accounts {
            account.push_frame();
        }
        Ok(())
    }

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
        for account in &mut self.accounts {
            account.pop_frame();
        }
    }

    // pub(crate) fn account_to_xdr(
    //     &self,
    //     account_handle: &AccountHandle,
    // ) -> Result<ScObject, HostError> {
    //     self.get_account(account_handle)?.to_xdr(&self.budget)
    // }

    pub(crate) fn authorize(
        &mut self,
        host: &Host,
        account: &HostAccount,
        args: ScVec,
    ) -> Result<(), HostError> {
        match account {
            HostAccount::AbstractAccount(account_handle) => {
                // This is unnecessary clone, need to work around borrow checker...
                let call_stack = self.call_stack.clone();
                // Note: metering for the recording mode is rather misleading, as it
                // wouldn't do exactly the same ops as the enforcing mode and also
                // can't be used outside of the preflight. Maybe recording shouldn't be
                // metered at all, though that still may be misleading.
                match self.mode {
                    AuthorizationMode::Enforcing => self
                        .get_mut_account(account_handle)?
                        .authorize_invocation(host, call_stack.as_slice(), args),
                    AuthorizationMode::Recording => self
                        .get_mut_account(account_handle)?
                        .record_invocation(host, call_stack.as_slice(), args),
                }
            }
            // For the contract auth always enforce that the invoker contract
            // has authorized the current contract call no matter the mode.
            // Maybe there should be a way to authorize sub-contract calls on
            // behalf of the contract in order to make the contract behavior
            // consistent with the account behavior.
            HostAccount::InvokerContract(invoker_id) => {
                if invoker_id.0 == host.get_invoking_contract_internal()?.0 {
                    Ok(())
                } else {
                    Err(host.err_general("TODO"))
                }
            }
        }
    }

    #[cfg(feature = "testutils")]
    pub(crate) fn verify_last_authorization(
        &mut self,
        host: &Host,
        account: &HostAccount,
        call_stack: Vec<ContractInvocation>,
        args: ScVec,
    ) -> Result<bool, HostError> {
        if !matches!(self.mode, AuthorizationMode::Recording) {
            panic!("verifying the authorization is only available for recording-mode auth");
        }
        match account {
            HostAccount::AbstractAccount(account_handle) => self
                .get_mut_account(account_handle)?
                .verify_last_authorization(call_stack, args),
            HostAccount::InvokerContract(_) => {
                Err(host
                    .err_general("test auth verification is not available for invoker accounts"))
            }
        }
    }

    fn get_account(
        &self,
        account_handle: &AbstractAccountHandle,
    ) -> Result<&AbstractAccount, HostError> {
        self.accounts
            .get(account_handle.index as usize)
            .ok_or(ScUnknownErrorCode::General.into())
    }

    fn get_mut_account(
        &mut self,
        account_handle: &AbstractAccountHandle,
    ) -> Result<&mut AbstractAccount, HostError> {
        self.accounts
            .get_mut(account_handle.index as usize)
            .ok_or(ScUnknownErrorCode::General.into())
    }
}

impl AbstractAccount {
    fn from_xdr(host: &Host, sc_account: ScAccount, address: ScAddress) -> Result<Self, HostError> {
        // Never consume nonce for the invoker. This is a bit ugly, as during
        // preflight we don't know if the account is invoker or not (maybe we should?),
        // hence the authorized invocations will always contain nonces. So invoker
        // will sign the nonces, but never consume them. This doesn't seem unsafe
        // (as replay protection is guaranteed via classic seq nums), but it's
        // also hacky. At least the source account client can clear nonces in
        // 'invoker' mode, but in order for this to be efficient, nonces would
        // need to be also removed from the footprint...
        let need_nonce = sc_account.account_id != ScAccountId::BuiltinInvoker;
        Ok(Self {
            account_id: sc_account.account_id,
            address,
            authorized_invocations: sc_account
                .invocations
                .to_vec()
                .into_iter()
                .map(|x| AuthorizedInvocation::from_xdr(x))
                .collect::<Result<Vec<_>, _>>()?,
            signature_args: host.scvals_to_rawvals(sc_account.signature_args.as_slice())?,
            authorized_for_frame: vec![],
            authenticated: false,
            need_nonce,
            #[cfg(feature = "testutils")]
            last_recorded_invocations: vec![],
        })
    }

    #[cfg(feature = "testutils")]
    fn reset_recording(&mut self, host: &Host) -> Result<(), HostError> {
        let last_recorded_invocations = self
            .authorized_invocations
            .clone()
            .into_iter()
            .map(|mut i| {
                i.nonce = None;
                i
            })
            .collect();
        let sc_account = ScAccount {
            account_id: self.account_id.clone(),
            invocations: vec![].try_into().unwrap(),
            signature_args: ScVec(vec![].try_into().unwrap()),
        };
        *self = AbstractAccount::from_xdr(host, sc_account, self.address.clone())?;
        self.last_recorded_invocations = last_recorded_invocations;
        Ok(())
    }

    fn to_xdr(&self, budget: &Budget) -> Result<ScObject, HostError> {
        Ok(ScObject::Account(ScAccount {
            account_id: self.account_id.metered_clone(budget)?,
            invocations: self.invocations_to_xdr(budget)?,
            // Return no signatures here as this is intended to only be called
            // for the recording mode (i.e. we only need account XDR *before*
            // signing it).
            signature_args: Default::default(),
        }))
    }

    fn invocations_to_xdr(
        &self,
        budget: &Budget,
    ) -> Result<xdr::VecM<xdr::AuthorizedInvocation>, HostError> {
        self.authorized_invocations
            .iter()
            .map(|ai| ai.to_xdr(budget))
            .collect::<Result<Vec<xdr::AuthorizedInvocation>, HostError>>()?
            .try_into()
            .map_err(|_| HostError::from(ScUnknownErrorCode::General))
    }

    fn push_frame(&mut self) {
        if let Some(authorized_for_curr_frame) = self.authorized_for_frame.last_mut() {
            // Don't allow authorizing the account after something has been called
            // on its behalf. This allows to have consistent stack traces:
            // E.g. if contract A calls B and C on behalf of the same account, then
            // the authorized must be either A, A->B, A->C (in case if A call
            // needs to be authorized) or B, C (in case if A call doesn't need
            // to be authorized), but not B, A, A->C (if authorization on A has
            // been called after calling B).
            if authorized_for_curr_frame.is_none() {
                *authorized_for_curr_frame = Some(false);
            }
        }
        self.authorized_for_frame.push(None);
    }

    fn pop_frame(&mut self) {
        self.authorized_for_frame.pop();
        // Require nonce for the root contract of every independent call tree.
        let has_authorized_frames = self
            .authorized_for_frame
            .iter()
            .find(|f| f == &&Some(true))
            .is_some();
        if !has_authorized_frames {
            self.need_nonce = true;
        }
    }

    fn mark_frame_authorization(&mut self) -> Result<(), HostError> {
        if let Some(authorized_for_curr_frame) = self.authorized_for_frame.last_mut() {
            if authorized_for_curr_frame.is_none() {
                *authorized_for_curr_frame = Some(true);
                Ok(())
            } else {
                // Maybe we should allow to call authorize multiple times, but
                // only the first call would actually be checked/recorded...
                Err(ScUnknownErrorCode::General.into())
            }
        } else {
            // This would be an internal error, maybe should panic instead
            Err(ScUnknownErrorCode::General.into())
        }
    }

    fn get_authorized_call_stack(
        &self,
        call_stack: &[ContractInvocation],
    ) -> Result<Vec<ContractInvocation>, HostError> {
        let mut res = vec![];
        if call_stack.len() != self.authorized_for_frame.len() {
            // This would be an internal error, maybe should panic instead
            return Err(ScUnknownErrorCode::General.into());
        }
        for i in 0..self.authorized_for_frame.len() {
            if self.authorized_for_frame[i] == Some(true) {
                res.push(call_stack[i].clone());
            }
        }
        Ok(res)
    }

    fn maybe_consume_nonce(
        &mut self,
        host: &Host,
        top_invocation: &ContractInvocation,
    ) -> Result<Option<u64>, HostError> {
        if !self.need_nonce {
            return Ok(None);
        }
        self.need_nonce = false;
        Ok(Some(host.read_and_consume_nonce(
            &top_invocation.contract_id,
            &self.address,
        )?))
    }

    fn authorize_invocation(
        &mut self,
        host: &Host,
        call_stack: &[ContractInvocation],
        top_args: ScVec,
    ) -> Result<(), HostError> {
        self.maybe_authenticate(host)?;
        self.mark_frame_authorization()?;
        let nonce = self.maybe_consume_nonce(host, &call_stack[0])?;
        let expected_invocation = AuthorizedInvocation::new(
            self.get_authorized_call_stack(call_stack)?,
            top_args,
            nonce,
        )?;
        if let Some(index) = self.authorized_invocations.iter().position(|v| {
            // Second part of the invoker hack: ignore nonce during invocation
            // comparison, as we never consume it, while allow to be present
            // in the authorized invocation.
            if matches!(self.account_id, ScAccountId::BuiltinInvoker) {
                v.eq_without_nonce(&expected_invocation)
            } else {
                v == &expected_invocation
            }
        }) {
            self.authorized_invocations.swap_remove(index);
            Ok(())
        } else {
            Err(ScUnknownErrorCode::General.into())
        }
    }

    fn record_invocation(
        &mut self,
        host: &Host,
        call_stack: &[ContractInvocation],
        top_args: ScVec,
    ) -> Result<(), HostError> {
        self.mark_frame_authorization()?;
        let nonce = self.maybe_consume_nonce(host, &call_stack[0])?;
        self.authorized_invocations.push(AuthorizedInvocation::new(
            self.get_authorized_call_stack(call_stack)?,
            top_args,
            nonce,
        )?);
        Ok(())
    }

    #[cfg(feature = "testutils")]
    fn verify_last_authorization(
        &mut self,
        call_stack: Vec<ContractInvocation>,
        top_args: ScVec,
    ) -> Result<bool, HostError> {
        let expected_invocation = AuthorizedInvocation::new(call_stack, top_args, None)?;
        if let Some(index) = self
            .last_recorded_invocations
            .iter()
            .position(|v| v == &expected_invocation)
        {
            self.last_recorded_invocations.swap_remove(index);
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn get_signature_payload(&self, host: &Host) -> Result<[u8; 32], HostError> {
        let payload_preimage = HashIdPreimage::ContractAuth(HashIdPreimageContractAuth {
            // TODO: should this be network id? Passphrase seems easier to digest
            // for signing
            network_passphrase: host
                .with_ledger_info(|li| li.network_passphrase.metered_clone(host.budget_ref()))?
                .try_into()
                .map_err(|_| host.err_general(""))?,
            invocations: self.invocations_to_xdr(host.budget_ref())?,
        });

        host.metered_hash_xdr(&payload_preimage)
    }

    fn maybe_authenticate(&mut self, host: &Host) -> Result<(), HostError> {
        if self.authenticated {
            return Ok(());
        }
        if self.account_id == ScAccountId::BuiltinInvoker {
            return if self.signature_args.is_empty() {
                Ok(())
            } else {
                Err(host.err_general("TODO: better error reporting"))
            };
        }
        // TODO: there should also be a mode where a dummy payload is used instead
        // (for preflight with auth enforced).
        let payload = self.get_signature_payload(host)?;
        if let ScAccountId::GenericAccount(generic_acc) = &self.account_id {
            todo!();
        } else {
            check_account_authentication(
                host,
                &self.account_id,
                &payload,
                self.signature_args.clone(),
            )?;
        }

        self.authenticated = true;
        Ok(())
    }
}

impl Ord for AbstractAccountHandle {
    fn cmp(&self, other: &Self) -> Ordering {
        self.index.cmp(&other.index)
    }
}

// impl Ord for AbstractAccountHandle {
//     fn cmp(&self, other: &Self) -> Ordering {
//         self.index.cmp(&other.index)
//     }
// }

impl Host {
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
        let curr_nonce: u64 = if self.with_mut_storage(|storage| storage.has(&nonce_key))? {
            let sc_val = self.with_mut_storage(|storage| match storage.get(&nonce_key)?.data {
                LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
                _ => Err(self.err_general("")),
            })?;
            match sc_val {
                ScVal::Object(Some(ScObject::U64(val))) => val,
                _ => {
                    return Err(self.err_general(""));
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
        let curr_nonce: u64 = if self.with_mut_storage(|storage| storage.has(&nonce_key))? {
            let sc_val = self.with_mut_storage(|storage| match storage.get(&nonce_key)?.data {
                LedgerEntryData::ContractData(ContractDataEntry { val, .. }) => Ok(val),
                _ => Err(self.err_general("")),
            })?;
            match sc_val {
                ScVal::Object(Some(ScObject::U64(val))) => val,
                _ => {
                    return Err(self.err_general(""));
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
        self.with_mut_storage(|storage| storage.put(&nonce_key, &entry))?;
        Ok(curr_nonce)
    }
}
