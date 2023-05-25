use soroban_env_common::{
    xdr::{ContractIdPreimage, ScAddress, ScErrorCode, ScErrorType},
    AddressObject,
};

use crate::{
    auth::AuthorizationManagerSnapshot,
    budget::AsBudget,
    err,
    storage::StorageMap,
    xdr::{ContractCostType, Hash, HostFunction, HostFunctionType, ScContractExecutable, ScVal},
    Error, Host, HostError, RawVal, Symbol, SymbolStr, TryFromVal, TryIntoVal,
};

#[cfg(any(test, feature = "testutils"))]
use crate::host::testutils;
#[cfg(any(test, feature = "testutils"))]
use core::cell::RefCell;
use std::rc::Rc;

use crate::Vm;

use super::{invoker_type::InvokerType, metered_clone::MeteredClone, prng::Prng};

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

/// All the contract functions starting with double underscore are considered
/// to be reserved by the Soroban host and can't be directly called by another
/// contracts.
const RESERVED_CONTRACT_FN_PREFIX: &str = "__";

/// Saves host state (storage and objects) for rolling back a (sub-)transaction
/// on error. A helper type used by [`FrameGuard`].
// Notes on metering: `RollbackPoint` are metered under Frame operations
#[derive(Clone)]
pub(super) struct RollbackPoint {
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
    pub(super) panic: Rc<RefCell<Option<Error>>>,
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

/// Context pairs a variable-case [`Frame`] enum with state that's common to all
/// cases (eg. a [`Prng`]).
#[derive(Clone)]
pub(crate) struct Context {
    pub(crate) frame: Frame,
    prng: Option<Prng>,
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
    ContractVM(Rc<Vm>, Symbol, Vec<RawVal>),
    HostFunction(HostFunctionType),
    Token(Hash, Symbol, Vec<RawVal>),
    #[cfg(any(test, feature = "testutils"))]
    TestContract(TestContractFrame),
}

impl Host {
    /// Helper function for [`Host::with_frame`] below. Pushes a new [`Frame`]
    /// on the context stack, returning a [`RollbackPoint`] such that if
    /// operation fails, it can be used to roll the [`Host`] back to the state
    /// it had before its associated [`Frame`] was pushed.
    pub(super) fn push_frame(&self, frame: Frame) -> Result<RollbackPoint, HostError> {
        // This is a bit hacky, as it relies on re-borrow to occur only during
        // the account contract invocations. Instead we should probably call it
        // in more explicitly different fashion and check if we're calling it
        // instead of a borrow check.
        let mut auth_snapshot = None;
        if let Ok(mut auth_manager) = self.0.authorization_manager.try_borrow_mut() {
            auth_manager.push_frame(self, &frame)?;
            auth_snapshot = Some(auth_manager.snapshot());
        }
        let ctx = Context { frame, prng: None };
        self.0.context.borrow_mut().push(ctx);
        Ok(RollbackPoint {
            storage: self.0.storage.borrow().map.clone(),
            events: self.0.events.borrow().vec.len(),
            auth: auth_snapshot,
        })
    }

    /// Helper function for [`Host::with_frame`] below. Pops a [`Frame`] off
    /// the current context and optionally rolls back the [`Host`]'s objects
    /// and storage map to the state in the provided [`RollbackPoint`].
    pub(super) fn pop_frame(&self, orp: Option<RollbackPoint>) -> Result<(), HostError> {
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
    //
    // Notes on metering: aquiring the current frame is cheap and not charged.
    // Metering happens in the passed-in closure where actual work is being done.
    pub(super) fn with_current_frame<F, U>(&self, f: F) -> Result<U, HostError>
    where
        F: FnOnce(&Frame) -> Result<U, HostError>,
    {
        let Ok(context_guard) = self.0.context.try_borrow() else {
            return Err(self.err(ScErrorType::Context, ScErrorCode::InternalError, "context is already borrowed", &[]));
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
        let Ok(mut context_guard) = self.0.context.try_borrow_mut() else {
            return Err(self.err(ScErrorType::Context, ScErrorCode::InternalError, "context is already borrowed", &[]));
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
        let Ok(context_guard) = self.0.context.try_borrow() else {
            return Err(self.err(ScErrorType::Context, ScErrorCode::InternalError, "context is already borrowed", &[]));
        };
        if let Some(context) = context_guard.last() {
            f(Some(&context.frame))
        } else {
            drop(context_guard);
            f(None)
        }
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
            let mut base_guard = self.0.base_prng.borrow_mut();
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
    // Notes on metering: `GuardFrame` charges on the work done on protecting the `context`.
    // It does not cover the cost of the actual closure call. The closure needs to be
    // metered separately.
    pub(crate) fn with_frame<F>(&self, frame: Frame, f: F) -> Result<RawVal, HostError>
    where
        F: FnOnce() -> Result<RawVal, HostError>,
    {
        self.charge_budget(ContractCostType::GuardFrame, None)?;
        let start_depth = self.0.context.borrow().len();
        let rp = self.push_frame(frame)?;
        let res = f();
        let res = if let Ok(v) = res {
            if let Ok(err) = Error::try_from(v) {
                Err(self.error(err, "escalating Ok(Error) frame-exit to Err(Error)", &[]))
            } else {
                Ok(v)
            }
        } else {
            res
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

    /// Returns [`Hash`] contract ID from the VM frame at the top of the context
    /// stack, or a [`HostError`] if the context stack is empty or has a non-VM
    /// frame at its top.
    pub(crate) fn get_current_contract_id_opt_internal(&self) -> Result<Option<Hash>, HostError> {
        self.with_current_frame(|frame| match frame {
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
            Err(self.err(
                ScErrorType::Context,
                ScErrorCode::MissingValue,
                "Current context has no contract ID",
                &[],
            ))
        }
    }

    pub(crate) fn get_invoking_contract_internal(&self) -> Result<Hash, HostError> {
        let frames = self.0.context.borrow();
        // the previous frame must exist and must be a contract
        let hash = match frames.as_slice() {
            [.., c2, _] => match &c2.frame {
                Frame::ContractVM(vm, _, _) => Ok(vm.contract_id.metered_clone(&self.0.budget)?),
                Frame::HostFunction(_) => Err(self.err(
                    ScErrorType::Context,
                    ScErrorCode::UnexpectedType,
                    "invoker is not a contract",
                    &[],
                )),
                Frame::Token(id, _, _) => Ok(id.clone()),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(tc) => Ok(tc.id.clone()), // no metering
            },
            _ => Err(self.err(
                ScErrorType::Context,
                ScErrorCode::MissingValue,
                "no frames to derive the invoker from",
                &[],
            )),
        }?;
        Ok(hash)
    }

    // Metering: mostly free or already covered by components (e.g. err_general)
    pub(super) fn get_invoker_type(&self) -> Result<u64, HostError> {
        let frames = self.0.context.borrow();
        // If the previous frame exists and is a contract, return its ID, otherwise return
        // the account invoking.
        let st = match frames.as_slice() {
            // There are always two frames when WASM is executed in the VM.
            [.., c2, _] => match &c2.frame {
                Frame::ContractVM(_, _, _) => Ok(InvokerType::Contract),
                Frame::HostFunction(_) => Ok(InvokerType::Account),
                Frame::Token(id, _, _) => Ok(InvokerType::Contract),
                #[cfg(any(test, feature = "testutils"))]
                Frame::TestContract(_) => Ok(InvokerType::Contract),
            },
            // In tests contracts are executed with a single frame.
            // TODO: Investigate this discrepancy: https://github.com/stellar/rs-soroban-env/issues/485.
            [_] => Ok(InvokerType::Account),
            _ => Err(self.err(
                ScErrorType::Context,
                ScErrorCode::InternalError,
                "no frames to derive the invoker from",
                &[],
            )),
        }?;
        Ok(st as u64)
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

    // Notes on metering: this is covered by the called components.
    fn call_contract_fn(
        &self,
        id: &Hash,
        func: &Symbol,
        args: &[RawVal],
    ) -> Result<RawVal, HostError> {
        // Create key for storage
        let storage_key = self.contract_executable_ledger_key(id)?;
        match self.retrieve_contract_executable_from_storage(&storage_key)? {
            ScContractExecutable::WasmRef(wasm_hash) => {
                let code_entry = self.retrieve_wasm_from_storage(&wasm_hash)?;
                let vm = Vm::new(
                    self,
                    id.metered_clone(&self.0.budget)?,
                    code_entry.as_slice(),
                )?;
                vm.invoke_function_raw(self, func, args)
            }
            ScContractExecutable::Token => {
                self.with_frame(Frame::Token(id.clone(), *func, args.to_vec()), || {
                    use crate::native_contract::{NativeContract, Token};
                    Token.call(func, self, args)
                })
            }
        }
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
                &[func.to_raw()],
            ));
        }
        if !matches!(reentry_mode, ContractReentryMode::Allowed) {
            let mut is_last_non_host_frame = true;
            for ctx in self.0.context.borrow().iter().rev() {
                let exist_id = match &ctx.frame {
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

        self.fn_call_diagnostics(id, &func, args)?;

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
                let frame = TestContractFrame::new(id.clone(), func, args.to_vec());
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
                    let res: Result<Option<RawVal>, PanicVal> =
                        testutils::call_with_suppressed_panic_hook(closure);
                    match res {
                        Ok(Some(rawval)) => {
                            self.fn_return_diagnostics(id, &func, &rawval)?;
                            Ok(rawval)
                        }
                        Ok(None) => Err(self.err(
                            ScErrorType::Context,
                            ScErrorCode::MissingValue,
                            "calling unknown contract function",
                            &[func.to_raw()],
                        )),
                        Err(panic_payload) => {
                            // Return an error indicating the contract function
                            // panicked. If if was a panic generated by a
                            // Env-upgraded HostError, it had its status
                            // captured by VmCallerEnv::escalate_error_to_panic:
                            // fish the Error stored in the frame back out and
                            // propagate it.
                            let func: RawVal = func.into();
                            let mut error: Error = Error::from_type_and_code(
                                ScErrorType::Context,
                                ScErrorCode::InternalError,
                            );

                            if let Some(err) = *panic.borrow() {
                                error = err;
                            }
                            // If we're allowed to record dynamic strings (which happens
                            // when diagnostics are active), also log the panic payload into
                            // the Debug buffer.
                            else if self.is_debug() {
                                if let Some(str) = panic_payload.downcast_ref::<&str>() {
                                    let msg: String = format!(
                                        "caught panic '{}' from contract function '{:?}'",
                                        str, func
                                    );
                                    let _ = self.log_diagnostics(&msg, args);
                                } else if let Some(str) = panic_payload.downcast_ref::<String>() {
                                    let msg: String = format!(
                                        "caught panic '{}' from contract function '{:?}'",
                                        str, func
                                    );
                                    let _ = self.log_diagnostics(&msg, args);
                                }
                            }
                            Err(self.error(error, "caught error from function", &[]))
                        }
                    }
                });
            }
        }

        let res = self.call_contract_fn(id, &func, args);

        match &res {
            Ok(res) => self.fn_return_diagnostics(id, &func, res)?,
            Err(err) => {}
        }

        res
    }

    // Notes on metering: covered by the called components.
    fn invoke_function_raw(&self, hf: HostFunction) -> Result<RawVal, HostError> {
        let hf_type = hf.discriminant();
        match hf {
            HostFunction::InvokeContract(args) => {
                if let [ScVal::Address(ScAddress::Contract(contract_id)), ScVal::Symbol(scsym), rest @ ..] =
                    args.as_slice()
                {
                    self.with_frame(Frame::HostFunction(hf_type), || {
                        // Metering: conversions to host objects are covered. Cost of collecting
                        // RawVals into Vec is ignored. Since 1. RawVals are cheap to clone 2. the
                        // max number of args is fairly limited.

                        let symbol: Symbol = scsym.as_slice().try_into_val(self)?;
                        let args = self.scvals_to_rawvals(rest)?;
                        // since the `HostFunction` frame must be the bottom of the call stack,
                        // reentry is irrelevant, we always pass in `ContractReentryMode::Prohibited`.
                        self.call_n_internal(
                            contract_id,
                            symbol,
                            &args[..],
                            ContractReentryMode::Prohibited,
                            false,
                        )
                    })
                } else {
                    Err(err!(
                        self,
                        (ScErrorType::Context, ScErrorCode::UnexpectedSize),
                        "unexpected number of arguments to 'call' host function",
                        args.len()
                    ))
                }
            }
            HostFunction::CreateContract(args) => {
                self.with_frame(Frame::HostFunction(hf_type), || {
                    let deployer: Option<AddressObject> = match &args.contract_id_preimage {
                        ContractIdPreimage::Address(preimage_from_addr) => Some(
                            self.add_host_object(
                                preimage_from_addr
                                    .address
                                    .metered_clone(self.budget_ref())?,
                            )?,
                        ),
                        ContractIdPreimage::Asset(_) => None,
                    };
                    self.create_contract_internal(deployer, args)
                        .map(<RawVal>::from)
                })
            }
            HostFunction::UploadContractWasm(wasm) => self
                .with_frame(Frame::HostFunction(hf_type), || {
                    self.upload_contract_wasm(wasm.to_vec()).map(<RawVal>::from)
                }),
        }
    }

    // Notes on metering: covered by the called components.
    pub fn invoke_function(&self, hf: HostFunction) -> Result<ScVal, HostError> {
        let rv = self.invoke_function_raw(hf)?;
        self.from_host_val(rv)
    }
}
