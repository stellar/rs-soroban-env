use soroban_env_common::{
    xdr::{ContractIdPreimage, ScAddress, ScContractInstance, ScErrorCode, ScErrorType},
    AddressObject,
};

use crate::{
    auth::AuthorizationManagerSnapshot,
    budget::AsBudget,
    err,
    storage::{InstanceStorageMap, StorageMap},
    xdr::{ContractExecutable, Hash, HostFunction, HostFunctionType, ScVal},
    Error, Host, HostError, Object, Symbol, SymbolStr, TryFromVal, TryIntoVal, Val,
    DEFAULT_HOST_DEPTH_LIMIT,
};

#[cfg(any(test, feature = "testutils"))]
use core::cell::RefCell;
use std::rc::Rc;

use crate::Vm;

use super::{
    metered_clone::{MeteredClone, MeteredContainer, MeteredIterator},
    prng::Prng,
};

/// Determines the re-entry mode for calling a contract.
pub(crate) enum ContractReentryMode {
    /// Re-entry is completely prohibited.
    Prohibited,
    /// Re-entry is allowed, but only directly into the same contract (i.e. it's
    /// possible for a contract to do a self-call via host).
    SelfAllowed,
    /// Re-entry is fully allowed.
    #[allow(dead_code)]
    Allowed,
}

/// All the contract functions starting with double underscore are considered
/// to be reserved by the Soroban host and can't be directly called by another
/// contracts.
const RESERVED_CONTRACT_FN_PREFIX: &str = "__";

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
// Notes on metering: `RollbackPoint` are metered under Frame operations
// #[derive(Clone)]
pub(super) struct RollbackPoint {
    storage: StorageMap,
    events: usize,
    auth: AuthorizationManagerSnapshot,
}

#[cfg(any(test, feature = "testutils"))]
pub trait ContractFunctionSet {
    fn call(&self, func: &Symbol, host: &Host, args: &[Val]) -> Option<Val>;
}

#[cfg(any(test, feature = "testutils"))]
#[derive(Debug, Clone)]
pub(crate) struct TestContractFrame {
    pub(crate) id: Hash,
    pub(crate) func: Symbol,
    pub(crate) args: Vec<Val>,
    pub(crate) panic: Rc<RefCell<Option<Error>>>,
    pub(crate) instance: ScContractInstance,
}

#[cfg(feature = "testutils")]
impl std::hash::Hash for TestContractFrame {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
        self.func.hash(state);
        self.args.hash(state);
        if let Some(panic) = self.panic.borrow().as_ref() {
            panic.hash(state);
        }
        self.instance.hash(state);
    }
}

#[cfg(any(test, feature = "testutils"))]
impl TestContractFrame {
    pub fn new(id: Hash, func: Symbol, args: Vec<Val>, instance: ScContractInstance) -> Self {
        Self {
            id,
            func,
            args,
            panic: Rc::new(RefCell::new(None)),
            instance,
        }
    }
}

/// Context pairs a variable-case [`Frame`] enum with state that's common to all
/// cases (eg. a [`Prng`]).
#[derive(Clone)]
#[cfg_attr(feature = "testutils", derive(Hash))]
pub(crate) struct Context {
    pub(crate) frame: Frame,
    pub(crate) prng: Option<Prng>,
    pub(crate) storage: Option<InstanceStorageMap>,
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
#[cfg_attr(feature = "testutils", derive(Hash))]
pub(crate) enum Frame {
    ContractVM {
        vm: Rc<Vm>,
        fn_name: Symbol,
        args: Vec<Val>,
        instance: ScContractInstance,
        relative_objects: Vec<Object>,
    },
    HostFunction(HostFunctionType),
    StellarAssetContract(Hash, Symbol, Vec<Val>, ScContractInstance),
    #[cfg(any(test, feature = "testutils"))]
    TestContract(TestContractFrame),
}

impl Host {
    /// Returns if the host currently has a frame on the stack.
    ///
    /// A frame being on the stack indicates that a contract is currently
    /// executing.
    pub fn has_frame(&self) -> Result<bool, HostError> {
        self.with_current_frame_opt(|opt| Ok(opt.is_some()))
    }

    /// Helper function for [`Host::with_frame`] below. Pushes a new [`Context`]
    /// on the context stack, returning a [`RollbackPoint`] such that if
    /// operation fails, it can be used to roll the [`Host`] back to the state
    /// it had before its associated [`Context`] was pushed.
    pub(super) fn push_context(&self, ctx: Context) -> Result<RollbackPoint, HostError> {
        let _span = tracy_span!("push context");
        let auth_manager = self.try_borrow_authorization_manager()?;
        let auth_snapshot = auth_manager.snapshot(self)?;
        auth_manager.push_frame(self, &ctx.frame)?;
        Vec::<Context>::charge_bulk_init_cpy(1, self.as_budget())?;
        self.try_borrow_context_stack_mut()?.push(ctx);
        Ok(RollbackPoint {
            storage: self.try_borrow_storage()?.map.metered_clone(self)?,
            events: self.try_borrow_events()?.vec.len(),
            auth: auth_snapshot,
        })
    }

    /// Helper function for [`Host::with_frame`] below. Pops a [`Context`] off
    /// the current context stack and optionally rolls back the [`Host`]'s objects
    /// and storage map to the state in the provided [`RollbackPoint`].
    pub(super) fn pop_context(&self, orp: Option<RollbackPoint>) -> Result<Context, HostError> {
        let _span = tracy_span!("pop context");

        let ctx = self.try_borrow_context_stack_mut()?.pop();
        self.try_borrow_authorization_manager()?.pop_frame(self)?;

        #[cfg(any(test, feature = "recording_auth"))]
        if self.try_borrow_context_stack()?.is_empty() {
            // When there are no contexts left, emulate authentication for the
            // recording auth mode. This is a no-op for the enforcing mode.
            self.try_borrow_authorization_manager()?
                .maybe_emulate_authentication(self)?;
        }

        if let Some(rp) = orp {
            self.try_borrow_storage_mut()?.map = rp.storage;
            self.try_borrow_events_mut()?.rollback(rp.events)?;
            self.try_borrow_authorization_manager()?
                .rollback(self, rp.auth)?;
        }
        ctx.ok_or_else(|| {
            self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "unmatched host context push/pop",
                &[],
            )
        })
    }

    /// Applies a function to the top [`Frame`] of the context stack. Returns
    /// [`HostError`] if the context stack is empty, otherwise returns result of
    /// function call.
    //
    // Notes on metering: aquiring the current frame is cheap and not charged.
    // Metering happens in the passed-in closure where actual work is being done.
    pub(super) fn with_current_frame<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&Frame) -> Result<U, HostError>,
    {
        let Ok(context_guard) = self.0.context_stack.try_borrow() else {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "context is already borrowed",
                &[],
            ));
        };

        if let Some(context) = context_guard.last() {
            f(&context.frame)
        } else {
            drop(context_guard);
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::MissingValue,
                "no contract running",
                &[],
            ))
        }
    }

    /// Applies a function to a mutable reference to the top [`Context`] of the
    /// context stack. Returns [`HostError`] if the context stack is empty,
    /// otherwise returns result of function call.
    //
    // Notes on metering: aquiring the current frame is cheap and not charged.
    // Metering happens in the passed-in closure where actual work is being done.
    pub(super) fn with_current_context_mut<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Context) -> Result<U, HostError>,
    {
        let Ok(mut context_guard) = self.0.context_stack.try_borrow_mut() else {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "context is already borrowed",
                &[],
            ));
        };
        if let Some(context) = context_guard.last_mut() {
            f(context)
        } else {
            drop(context_guard);
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::MissingValue,
                "no contract running",
                &[],
            ))
        }
    }

    /// Same as [`Self::with_current_frame`] but passes `None` when there is no current
    /// frame, rather than logging an error.
    pub(crate) fn with_current_frame_opt<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(Option<&Frame>) -> Result<U, HostError>,
    {
        let Ok(context_guard) = self.0.context_stack.try_borrow() else {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "context is already borrowed",
                &[],
            ));
        };
        if let Some(context) = context_guard.last() {
            f(Some(&context.frame))
        } else {
            drop(context_guard);
            f(None)
        }
    }

    pub(crate) fn with_current_frame_relative_object_table<F, U>(
        &self,
        f: F,
    ) -> Result<U, HostError>
    where
        F: FnOnce(&mut Vec<Object>) -> Result<U, HostError>,
    {
        self.with_current_context_mut(|ctx| {
            if let Frame::ContractVM {
                relative_objects, ..
            } = &mut ctx.frame
            {
                f(relative_objects)
            } else {
                Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "accessing relative object table in non-VM frame",
                    &[],
                ))
            }
        })
    }

    pub(crate) fn with_current_prng<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&mut Prng) -> Result<U, HostError>,
    {
        // We need to not hold the context borrow-guard over multiple
        // error-generating calls here, since they will re-borrow the context to
        // report any error. Instead we mem::take the context's PRNG into a
        // local variable, and then put it back when we're done.
        let mut curr_prng_opt =
            self.with_current_context_mut(|ctx| Ok(std::mem::take(&mut ctx.prng)))?;
        let res: Result<U, HostError>;
        if let Some(p) = &mut curr_prng_opt {
            res = f(p)
        } else {
            let mut base_guard = self.try_borrow_base_prng_mut()?;
            if let Some(base) = base_guard.as_mut() {
                match base.sub_prng(self.as_budget()) {
                    Ok(mut sub_prng) => {
                        res = f(&mut sub_prng);
                        curr_prng_opt = Some(sub_prng);
                    }
                    Err(e) => res = Err(e),
                }
            } else {
                res = Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::MissingValue,
                    "host base PRNG was not seeded",
                    &[],
                ))
            }
        }
        // Put the (possibly newly-initialized frame PRNG-option back)
        self.with_current_context_mut(|ctx| {
            ctx.prng = curr_prng_opt;
            Ok(())
        })?;
        res
    }

    /// Pushes a [`Frame`], runs a closure, and then pops the frame, rolling back
    /// if the closure returned an error. Returns the result that the closure
    /// returned (or any error caused during the frame push/pop).
    pub(crate) fn with_frame<F>(&self, frame: Frame, f: F) -> Result<Val, HostError>
    where
        F: FnOnce() -> Result<Val, HostError>,
    {
        let start_depth = self.try_borrow_context_stack()?.len();
        if start_depth as u32 >= DEFAULT_HOST_DEPTH_LIMIT {
            return Err(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::ExceededLimit,
            )
            .into());
        }
        let ctx = Context {
            frame,
            prng: None,
            storage: None,
        };
        let rp = self.push_context(ctx)?;
        #[cfg(feature = "testutils")]
        {
            // We do this _after_ the context is pushed, in order to let the
            // observation code assume a context exists
            if let Some(ctx) = self.try_borrow_context_stack()?.last() {
                self.call_any_lifecycle_hook(crate::host::HostLifecycleEvent::PushCtx(ctx))?;
            }
        }
        #[cfg(any(test, feature = "testutils"))]
        let mut is_top_contract_invocation = false;
        #[cfg(any(test, feature = "testutils"))]
        {
            if self.try_borrow_context_stack()?.len() == 1 {
                if let Some(ctx) = self.try_borrow_context_stack()?.first() {
                    match ctx.frame {
                        // Don't call the contract invocation hook for
                        // the host functions.
                        Frame::HostFunction(_) => (),
                        // Everything else is some sort of contract call.
                        _ => {
                            is_top_contract_invocation = true;
                            if let Some(contract_invocation_hook) =
                                self.try_borrow_top_contract_invocation_hook()?.as_ref()
                            {
                                contract_invocation_hook(
                                    self,
                                    crate::host::ContractInvocationEvent::Start,
                                );
                            }
                        }
                    }
                }
            }
        }

        let res = f();
        let mut res = if let Ok(v) = res {
            // If a contract function happens to have signature Result<...,
            // Code> its Wasm ABI encoding will be ambiguous: if it exits with
            // Err(Code) it'll wind up exiting the Wasm VM "successfully" with a
            // Val that's of type Error, we'll get Ok(Error) here. To allow this
            // to work and avoid losing errors, we define _any_ successful
            // return of Ok(Error) as "a contract failure"; contracts aren't
            // allowed to return Ok(Error) and have it considered actually-ok.
            //
            // (If we were called from try_call, it will actually turn this
            // Err(ScErrorType::Contract) back into Ok(ScErrorType::Contract)
            // since that is a "recoverable" type of error.)
            if let Ok(err) = Error::try_from(v) {
                // Unfortunately there are still two sub-cases to consider. One
                // is when a contract returns Ok(Error) with
                // ScErrorType::Contract, which is allowed and legitimate and
                // "how a contract would signal a Result::Err(Code) as described
                // above". In this (good) case we propagate the Error they
                // provided, just switching it from Ok(Error) to Err(Error)
                // indicating that the contract "failed" with this Error.
                //
                // The second (bad) case is when the contract returns Ok(Error)
                // with a non-ScErrorType::Contract. This might be some kind of
                // mistake on their part but it might also be an attempt at
                // spoofing error reporting, by claiming some subsystem of the
                // host failed when it really didn't. In particular if a
                // contract wants to forcibly fail a caller that did `try_call`,
                // the contract could spoof-return an unrecoverable Error code
                // like InternalError or BudgetExceeded. We want to deny all
                // such cases, so we just define them as illegal returns, and
                // report them all as a specific error type of and description
                // our own choosing: not a contract's own logic failing, but a
                // contract failing to live up to a postcondition we're
                // enforcing of "never returning this sort of error code".
                if err.is_type(ScErrorType::Contract) {
                    Err(self.error(
                        err,
                        "escalating Ok(ScErrorType::Contract) frame-exit to Err",
                        &[],
                    ))
                } else {
                    Err(self.err(
                        ScErrorType::Context,
                        ScErrorCode::InvalidAction,
                        "frame-exit with Ok(Error) carrying a non-ScErrorType::Contract Error",
                        &[err.to_val()],
                    ))
                }
            } else {
                Ok(v)
            }
        } else {
            res
        };

        // We try flushing instance storage at the end of the frame if nothing
        // else failed. Unfortunately flushing instance storage is _itself_
        // fallible in a variety of ways, and if it fails we want to roll back
        // everything else.
        if res.is_ok() {
            if let Err(e) = self.persist_instance_storage() {
                res = Err(e)
            }
        }
        #[cfg(feature = "testutils")]
        {
            // We do this _before_ the context is popped, in order to let the
            // observation code assume a context exists
            if let Some(ctx) = self.try_borrow_context_stack()?.last() {
                self.call_any_lifecycle_hook(crate::host::HostLifecycleEvent::PopCtx(&ctx, &res))?;
            }
        }
        if res.is_err() {
            // Pop and rollback on error.
            self.pop_context(Some(rp))?
        } else {
            // Just pop on success.
            self.pop_context(None)?
        };
        // Every push and pop should be matched; if not there is a bug.
        let end_depth = self.try_borrow_context_stack()?.len();
        if start_depth != end_depth {
            return Err(err!(
                self,
                (ScErrorType::Context, ScErrorCode::InternalError),
                "frame-depth mismatch",
                start_depth,
                end_depth
            ));
        }
        #[cfg(any(test, feature = "testutils"))]
        if end_depth == 0 {
            // Empty call stack in tests means that some contract function call
            // has been finished and hence the authorization manager can be reset.
            // In non-test scenarios, there should be no need to ever reset
            // the authorization manager as the host instance shouldn't be
            // shared between the contract invocations.
            *self.try_borrow_previous_authorization_manager_mut()? =
                Some(self.try_borrow_authorization_manager()?.clone());
            self.try_borrow_authorization_manager_mut()?.reset();

            // Call the contract invocation hook for contract invocations only.
            if is_top_contract_invocation {
                if let Some(top_contract_invocation_hook) =
                    self.try_borrow_top_contract_invocation_hook()?.as_ref()
                {
                    top_contract_invocation_hook(
                        self,
                        crate::host::ContractInvocationEvent::Finish,
                    );
                }
            }
        }
        res
    }

    /// Inspects the frame at the top of the context and returns the contract ID
    /// if it exists. Returns `Ok(None)` if the context stack is empty or has a
    /// non-contract frame on top.
    pub(crate) fn get_current_contract_id_opt_internal(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame_opt(|opt_frame| match opt_frame {
            Some(frame) => match frame {
                Frame::ContractVM { vm, .. } => Ok(Some(vm.contract_id.metered_clone(self)?)),
                Frame::HostFunction(_) => Ok(None),
                Frame::StellarAssetContract(id, ..) => Ok(Some(id.metered_clone(self)?)),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => Ok(Some(tc.id.metered_clone(self)?)),
            },
            None => Ok(None),
        })
    }

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    pub(crate) fn get_current_contract_id_internal(&self) -> Result<Hash, HostError> {
        if let Some(id) = self.get_current_contract_id_opt_internal()? {
            Ok(id)
        } else {
            // This should only ever happen if we try to access the contract ID
            // from a HostFunction frame (meaning before a contract is running).
            // Doing so is a logic bug on our part.
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "Current context has no contract ID",
                &[],
            ))
        }
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
    ) -> Result<Val, HostError>
    where
        F: FnOnce() -> Result<Val, HostError>,
    {
        self.with_frame(
            Frame::TestContract(self.create_test_contract_frame(id, func, vec![])?),
            f,
        )
    }

    #[cfg(any(test, feature = "testutils"))]
    fn create_test_contract_frame(
        &self,
        id: Hash,
        func: Symbol,
        args: Vec<Val>,
    ) -> Result<TestContractFrame, HostError> {
        let instance_key = self.contract_instance_ledger_key(&id)?;
        let instance = self.retrieve_contract_instance_from_storage(&instance_key)?;
        Ok(TestContractFrame::new(
            id.clone(),
            func,
            args.to_vec(),
            instance,
        ))
    }

    // Notes on metering: this is covered by the called components.
    fn call_contract_fn(&self, id: &Hash, func: &Symbol, args: &[Val]) -> Result<Val, HostError> {
        // Create key for storage
        let storage_key = self.contract_instance_ledger_key(id)?;
        let instance = self
            .retrieve_contract_instance_from_storage(&storage_key)
            .map_err(|e| self.decorate_contract_instance_storage_error(e, &id))?;
        Vec::<Val>::charge_bulk_init_cpy(args.len() as u64, self.as_budget())?;
        let args_vec = args.to_vec();
        match &instance.executable {
            ContractExecutable::Wasm(wasm_hash) => {
                let code_entry = self.retrieve_wasm_from_storage(&wasm_hash)?;
                let vm = Vm::new(self, id.metered_clone(self)?, code_entry.as_slice())?;
                let relative_objects = Vec::new();
                self.with_frame(
                    Frame::ContractVM {
                        vm: Rc::clone(&vm),
                        fn_name: *func,
                        args: args_vec,
                        instance,
                        relative_objects,
                    },
                    || vm.invoke_function_raw(self, func, args),
                )
            }
            ContractExecutable::StellarAsset => self.with_frame(
                Frame::StellarAssetContract(id.metered_clone(self)?, *func, args_vec, instance),
                || {
                    use crate::builtin_contracts::{BuiltinContract, StellarAssetContract};
                    StellarAssetContract.call(func, self, args)
                },
            ),
        }
    }

    // Notes on metering: this is covered by the called components.
    pub(crate) fn call_n_internal(
        &self,
        id: &Hash,
        func: Symbol,
        args: &[Val],
        reentry_mode: ContractReentryMode,
        internal_host_call: bool,
    ) -> Result<Val, HostError> {
        // Internal host calls may call some special functions that otherwise
        // aren't allowed to be called.
        if !internal_host_call
            && SymbolStr::try_from_val(self, &func)?
                .to_string()
                .as_str()
                .starts_with(RESERVED_CONTRACT_FN_PREFIX)
        {
            return Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InvalidAction,
                "can't invoke a reserved function directly",
                &[func.to_val()],
            ));
        }
        if !matches!(reentry_mode, ContractReentryMode::Allowed) {
            let mut is_last_non_host_frame = true;
            for ctx in self.try_borrow_context_stack()?.iter().rev() {
                let exist_id = match &ctx.frame {
                    Frame::ContractVM { vm, .. } => &vm.contract_id,
                    Frame::StellarAssetContract(id, ..) => id,
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
                    return Err(self.err(
                        ScErrorType::Context,
                        ScErrorCode::InvalidAction,
                        "Contract re-entry is not allowed",
                        &[],
                    ));
                }
                is_last_non_host_frame = false;
            }
        }

        self.fn_call_diagnostics(id, &func, args);

        // Try dispatching the contract to the compiled-in registred
        // implmentation. Only the contracts with the special (empty) executable
        // are dispatched in this way, so that it's possible to switch the
        // compiled-in implementation back to Wasm via
        // `update_current_contract_wasm`.
        // "testutils" is not covered by budget metering.
        #[cfg(any(test, feature = "testutils"))]
        if self.is_test_contract_executable(id)? {
            // This looks a little un-idiomatic, but this avoids maintaining a borrow of
            // self.0.contracts. Implementing it as
            //
            //     if let Some(cfs) = self.try_borrow_contracts()?.get(&id).cloned() { ... }
            //
            // maintains a borrow of self.0.contracts, which can cause borrow errors.
            let cfs_option = self.try_borrow_contracts()?.get(&id).cloned();
            if let Some(cfs) = cfs_option {
                let frame = self.create_test_contract_frame(id.clone(), func, args.to_vec())?;
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
                    let res: Result<Option<Val>, PanicVal> =
                        crate::testutils::call_with_suppressed_panic_hook(closure);
                    match res {
                        Ok(Some(val)) => {
                            self.fn_return_diagnostics(id, &func, &val);
                            Ok(val)
                        }
                        Ok(None) => Err(self.err(
                            ScErrorType::Context,
                            ScErrorCode::MissingValue,
                            "calling unknown contract function",
                            &[func.to_val()],
                        )),
                        Err(panic_payload) => {
                            // Return an error indicating the contract function
                            // panicked.
                            //
                            // If it was a panic generated by a Env-upgraded
                            // HostError, it had its `Error` captured by
                            // `VmCallerEnv::escalate_error_to_panic`: fish the
                            // `Error` stored in the frame back out and
                            // propagate it.
                            //
                            // If it was a panic generated by user code calling
                            // panic!(...) we won't retrieve such a stored
                            // `Error`. Since we're trying to emulate
                            // what-the-VM-would-do here, and the VM traps with
                            // an unreachable error on contract panic, we
                            // generate same error (by converting a wasm
                            // trap-unreachable code). It's a little weird
                            // because we're not actually running a VM, but we
                            // prioritize emulation fidelity over honesty here.
                            let mut error: Error =
                                Error::from(wasmi::core::TrapCode::UnreachableCodeReached);

                            let mut recovered_error_from_panic_refcell = false;
                            if let Ok(panic) = panic.try_borrow() {
                                if let Some(err) = *panic {
                                    recovered_error_from_panic_refcell = true;
                                    error = err;
                                }
                            }

                            // If we didn't manage to recover a structured error
                            // code from the frame's refcell, and we're allowed
                            // to record dynamic strings (which happens when
                            // diagnostics are active), and we got a panic
                            // payload of a simple string, log that panic
                            // payload into the diagnostic event buffer. This
                            // code path will get hit when contracts do
                            // `panic!("some string")` in native testing mode.
                            if !recovered_error_from_panic_refcell {
                                self.with_debug_mode(|| {
                                    if let Some(str) = panic_payload.downcast_ref::<&str>() {
                                        let msg: String = format!(
                                            "caught panic '{}' from contract function '{:?}'",
                                            str, func
                                        );
                                        let _ = self.log_diagnostics(&msg, args);
                                    } else if let Some(str) = panic_payload.downcast_ref::<String>()
                                    {
                                        let msg: String = format!(
                                            "caught panic '{}' from contract function '{:?}'",
                                            str, func
                                        );
                                        let _ = self.log_diagnostics(&msg, args);
                                    };
                                    Ok(())
                                })
                            }
                            Err(self.error(error, "caught error from function", &[]))
                        }
                    }
                });
            }
        }

        let res = self.call_contract_fn(id, &func, args);

        match &res {
            Ok(res) => self.fn_return_diagnostics(id, &func, res),
            Err(_err) => {}
        }

        res
    }

    // Notes on metering: covered by the called components.
    fn invoke_function_raw(&self, hf: HostFunction) -> Result<Val, HostError> {
        let hf_type = hf.discriminant();
        match hf {
            HostFunction::InvokeContract(invoke_args) => {
                self.with_frame(Frame::HostFunction(hf_type), || {
                    // Metering: conversions to host objects are covered.
                    let ScAddress::Contract(ref contract_id) = invoke_args.contract_address else {
                        return Err(self.err(
                            ScErrorType::Value,
                            ScErrorCode::UnexpectedType,
                            "invoked address doesn't belong to a contract",
                            &[],
                        ));
                    };
                    let function_name: Symbol = invoke_args.function_name.try_into_val(self)?;
                    let args = self.scvals_to_val_vec(invoke_args.args.as_slice())?;
                    // since the `HostFunction` frame must be the bottom of the call stack,
                    // reentry is irrelevant, we always pass in `ContractReentryMode::Prohibited`.
                    self.call_n_internal(
                        contract_id,
                        function_name,
                        args.as_slice(),
                        ContractReentryMode::Prohibited,
                        false,
                    )
                })
            }
            HostFunction::CreateContract(args) => {
                self.with_frame(Frame::HostFunction(hf_type), || {
                    let deployer: Option<AddressObject> = match &args.contract_id_preimage {
                        ContractIdPreimage::Address(preimage_from_addr) => Some(
                            self.add_host_object(preimage_from_addr.address.metered_clone(self)?)?,
                        ),
                        ContractIdPreimage::Asset(_) => None,
                    };
                    self.create_contract_internal(deployer, args)
                        .map(<Val>::from)
                })
            }
            HostFunction::UploadContractWasm(wasm) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.upload_contract_wasm(wasm.to_vec()).map(<Val>::from)
                }),
        }
    }

    // Notes on metering: covered by the called components.
    pub fn invoke_function(&self, hf: HostFunction) -> Result<ScVal, HostError> {
        let rv = self.invoke_function_raw(hf)?;
        self.from_host_val(rv)
    }

    pub(crate) fn maybe_init_instance_storage(&self, ctx: &mut Context) -> Result<(), HostError> {
        // Lazily initialize the storage on first access - it's not free and
        // not every contract will use it.
        if ctx.storage.is_some() {
            return Ok(());
        }
        let storage_map = match &ctx.frame {
            Frame::ContractVM { instance, .. } => &instance.storage,
            Frame::HostFunction(_) => {
                return Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::InvalidAction,
                    "can't access instance storage from host function",
                    &[],
                ));
            }
            Frame::StellarAssetContract(_, _, _, instance) => &instance.storage,
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(t) => &t.instance.storage,
        };

        ctx.storage = Some(InstanceStorageMap::from_map(
            storage_map.as_ref().map_or_else(
                || Ok(vec![]),
                |m| {
                    m.iter()
                        .map(|i| Ok((self.to_host_val(&i.key)?, self.to_host_val(&i.val)?)))
                        .metered_collect::<Result<Vec<(Val, Val)>, HostError>>(self)?
                },
            )?,
            self,
        )?);
        Ok(())
    }

    // Make the in-memory instance storage persist into the `Storage` by writing
    // its updated contents into corresponding `ContractData` ledger entry.
    fn persist_instance_storage(&self) -> Result<(), HostError> {
        let updated_instance_storage = self.with_current_context_mut(|ctx| {
            if let Some(storage) = &ctx.storage {
                if !storage.is_modified {
                    return Ok(None);
                }
                Ok(Some(self.host_map_to_scmap(&storage.map)?))
            } else {
                Ok(None)
            }
        })?;
        if updated_instance_storage.is_some() {
            let contract_id = self.get_current_contract_id_internal().map_err(|_| {
                self.err(
                    ScErrorType::Context,
                    ScErrorCode::InternalError,
                    "unexpected missing contract for instance storage",
                    &[],
                )
            })?;
            let key = self.contract_instance_ledger_key(&contract_id)?;

            self.store_contract_instance(None, updated_instance_storage, contract_id, &key)?;
        }
        Ok(())
    }
}
