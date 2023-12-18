#![allow(dead_code)]

use crate::{
    budget::AsBudget, events::InternalEvent, host::Frame, host::HostLifecycleEvent,
    xdr::ContractExecutable, Host, Symbol, SymbolObject, SymbolSmall,
};
use std::{collections::hash_map::DefaultHasher, fmt::Display, hash::Hasher};

use super::metered_hash::MeteredHash;

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) enum Step {
    Begin,
    Push {
        contract: String,
        func: String,
        args: Vec<String>,
    },
    Pop {
        contract: String,
        func: String,
        result: String,
    },
    Call(String, Vec<String>),
    Ret(String, Result<String, String>),
    End,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub(crate) struct Observation {
    pub(crate) step: Step,
    cpu_insns: u64,
    mem_bytes: u64,
    local_prng_hash: u64,
    base_prng_hash: u64,
    local_objs_size: usize,
    local_objs_hash: u64,
    global_objs_size: usize,
    global_objs_hash: u64,
    vm_mem_size: usize,
    vm_mem_hash: u64,
    vm_exports_size: usize,
    vm_exports_hash: u64,
    events_size: usize,
    events_hash: u64,
    instance_storage_size: usize,
    instance_storage_hash: u64,
    ledger_storage_size: usize,
    ledger_storage_hash: u64,
    storage_footprint_size: usize,
    storage_footprint_hash: u64,
    auth_stack_size: usize,
    auth_stack_hash: u64,
    auth_trackers_size: usize,
    auth_trackers_hash: u64,
    context_stack_size: usize,
    context_stack_hash: u64,
}

impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Step::Push {
                contract,
                func,
                args,
            } => {
                write!(f, "push {contract}:{func}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Step::Pop {
                contract,
                func,
                result,
            } => {
                write!(f, "pop {contract}:{func} -> {result}")
            }
            Step::Call(hostfn, args) => {
                write!(f, "call {}(", hostfn)?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Step::Ret(hostfn, Ok(ok)) => {
                write!(f, "ret {} -> Ok({})", hostfn, ok)
            }
            Step::Ret(hostfn, Err(err)) => {
                write!(f, "ret {} -> Err({})", hostfn, err)
            }
            Step::Begin => write!(f, "begin"),
            Step::End => write!(f, "end"),
        }
    }
}

impl Host {
    fn hash_one<H: MeteredHash>(&self, h: &H) -> u64 {
        let mut state = DefaultHasher::default();
        self.budget_ref()
            .with_shadow_mode(|| h.metered_hash(&mut state, self.budget_ref()));
        state.finish()
    }

    fn hash_iter<H: MeteredHash>(&self, h: impl Iterator<Item = H>) -> u64 {
        let mut state = DefaultHasher::default();
        self.budget_ref().with_shadow_mode(|| {
            for i in h {
                i.metered_hash(&mut state, self.budget_ref())?;
            }
            Ok(())
        });
        state.finish()
    }

    fn hash_optional<H: MeteredHash>(&self, h: &Option<H>) -> u64 {
        if let Some(x) = h {
            self.hash_one(x)
        } else {
            0
        }
    }

    fn base_prng_hash(&self) -> u64 {
        if let Ok(prng) = self.try_borrow_base_prng() {
            self.hash_optional(&*prng)
        } else {
            0
        }
    }
    fn local_prng_hash(&self) -> u64 {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                return self.hash_optional(&ctx.prng);
            }
        }
        0
    }
    fn local_objs_size(&self) -> usize {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM {
                    relative_objects, ..
                } = &ctx.frame
                {
                    return relative_objects.len();
                }
            }
        }
        0
    }
    fn local_objs_hash(&self) -> u64 {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM {
                    relative_objects, ..
                } = &ctx.frame
                {
                    return self
                        .hash_iter(relative_objects.iter().map(|x| x.to_val().get_payload()));
                }
            }
        }
        0
    }
    fn global_objs_size(&self) -> usize {
        if let Ok(objs) = self.try_borrow_objects() {
            objs.len()
        } else {
            0
        }
    }
    fn global_objs_hash(&self) -> u64 {
        if let Ok(objs) = self.try_borrow_objects() {
            self.hash_iter(objs.iter())
        } else {
            0
        }
    }
    fn memory_hash_and_size(&self) -> (u64, usize) {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM { vm, .. } = &ctx.frame {
                    if let Ok(pair) = vm.memory_hash_and_size(self.budget_ref()) {
                        return pair;
                    }
                }
            }
        }
        (0, 0)
    }
    fn events_size(&self) -> usize {
        if let Ok(evts) = self.try_borrow_events() {
            evts.vec
                .iter()
                .filter(|(e, _)| match e {
                    InternalEvent::Contract(_) => true,
                    InternalEvent::Diagnostic(_) => false,
                })
                .count()
        } else {
            0
        }
    }
    fn events_hash(&self) -> u64 {
        if let Ok(evts) = self.try_borrow_events() {
            self.hash_iter(evts.vec.iter().filter(|(e, _)| match e {
                InternalEvent::Contract(_) => true,
                InternalEvent::Diagnostic(_) => false,
            }))
        } else {
            0
        }
    }
    fn instance_storage_size(&self) -> usize {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Some(storage) = &ctx.storage {
                    return storage.map.len();
                }
            }
        }
        0
    }
    fn instance_storage_hash(&self) -> u64 {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Some(storage) = &ctx.storage {
                    return self.hash_one(&storage.map);
                }
            }
        }
        0
    }
    fn ledger_storage_size(&self) -> usize {
        if let Ok(store) = self.try_borrow_storage() {
            store.map.len()
        } else {
            0
        }
    }
    fn ledger_storage_hash(&self) -> u64 {
        if let Ok(store) = self.try_borrow_storage() {
            self.hash_one(&store.map)
        } else {
            0
        }
    }

    fn auth_stack_size(&self) -> usize {
        if let Ok(auth_mgr) = self.try_borrow_authorization_manager() {
            auth_mgr.stack_size()
        } else {
            0
        }
    }
    fn auth_stack_hash(&self) -> u64 {
        if let Ok(auth_mgr) = self.try_borrow_authorization_manager() {
            if let Ok(hash) = auth_mgr.stack_hash(self.budget_ref()) {
                return hash;
            }
        }
        0
    }
    fn auth_trackers_hash_and_size(&self) -> (u64, usize) {
        if let Ok(auth_mgr) = self.try_borrow_authorization_manager() {
            if let Ok(pair) = auth_mgr.trackers_hash_and_size(self.budget_ref()) {
                return pair;
            }
        }
        (0, 0)
    }
    fn context_stack_size(&self) -> usize {
        if let Ok(frames) = self.try_borrow_context_stack() {
            frames.len()
        } else {
            0
        }
    }
    fn context_stack_hash(&self) -> u64 {
        if let Ok(frames) = self.try_borrow_context_stack() {
            self.hash_iter(frames.iter())
        } else {
            0
        }
    }
    fn storage_footprint_size(&self) -> usize {
        if let Ok(storage) = self.try_borrow_storage() {
            storage.footprint.0.len()
        } else {
            0
        }
    }
    fn storage_footprint_hash(&self) -> u64 {
        if let Ok(storage) = self.try_borrow_storage() {
            self.hash_one(&storage.footprint.0)
        } else {
            0
        }
    }

    fn vm_exports_hash_and_size(&self) -> (u64, usize) {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM { vm, .. } = &ctx.frame {
                    if let Ok(pair) = vm.exports_hash_and_size(self.budget_ref()) {
                        return pair;
                    }
                }
            }
        }
        (0, 0)
    }
}

impl Step {
    pub(crate) fn from_host_lifecycle_event(evt: HostLifecycleEvent<'_>) -> Self {
        match evt {
            HostLifecycleEvent::PushCtx(context) => {
                let (contract, func, args) = Self::frame_contract_func_and_args(&context.frame);
                Step::Push {
                    contract,
                    func,
                    args,
                }
            }
            HostLifecycleEvent::PopCtx(context, result) => {
                let (contract, func, _) = Self::frame_contract_func_and_args(&context.frame);
                // We only want to see the error code here, not debuginfo.
                let result = format!("{:?}", result.clone().map_err(|he| he.error));
                Step::Pop {
                    contract,
                    func,
                    result,
                }
            }
            HostLifecycleEvent::EnvCall(fname, args) => {
                Step::Call(fname.to_string(), args.iter().cloned().collect())
            }
            HostLifecycleEvent::EnvRet(fname, res) => Step::Ret(fname.to_string(), res.clone()),
        }
    }
    fn short_hash(hash: &crate::xdr::Hash) -> String {
        let Ok(word) = <[u8; 4]>::try_from(&hash.0[0..4]) else {
            return String::new();
        };
        format!("{:4.4x}", u32::from_be_bytes(word))
    }
    fn exec(exec: &ContractExecutable) -> String {
        match exec {
            ContractExecutable::Wasm(hash) => Self::short_hash(hash),
            ContractExecutable::StellarAsset => "SAC".to_string(),
        }
    }
    fn sym(s: &Symbol) -> String {
        if let Ok(small) = SymbolSmall::try_from(*s) {
            small.to_string()
        } else if let Ok(obj) = SymbolObject::try_from(*s) {
            format!("sym#{}", obj.get_handle())
        } else {
            format!("{:?}", s)
        }
    }
    fn frame_contract_func_and_args(frame: &Frame) -> (String, String, Vec<String>) {
        match frame {
            Frame::ContractVM {
                fn_name,
                args,
                instance,
                ..
            } => (
                format!("VM:{}", Self::exec(&instance.executable)),
                Self::sym(fn_name).to_string(),
                args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
            // TODO: this isn't ideal, we should capture the args in Frame::HostFunction
            Frame::HostFunction(ty) => (format!("{:?}", ty), "".to_string(), vec![]),
            Frame::StellarAssetContract(id, fn_name, args, _) => (
                format!("SAC:{}", Self::short_hash(id)),
                Self::sym(fn_name).to_string(),
                args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
            #[cfg(any(test, feature = "testutils"))]
            Frame::TestContract(tc) => (
                format!("TEST:{}", Self::short_hash(&tc.id)),
                Self::sym(&tc.func).to_string(),
                tc.args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
        }
    }
}

impl Observation {
    fn render_hash(hash: u64) -> String {
        // Truncate the hash to u32, to fit on a line.
        if hash == 0 {
            "-".to_string()
        } else {
            format!("{:4.4x}", hash as u32)
        }
    }

    fn render_size_and_hash(size: usize, hash: u64) -> String {
        if size == 0 {
            "-".to_string()
        } else {
            format!("{}@{}", size, Self::render_hash(hash))
        }
    }

    // Render needs to return a (key,value) pair for step number `n` in a given
    // test. The returned key needs to be unique and should probably start with
    // a rendering of the number `n`.
    pub(crate) fn render(&self, n: usize) -> (String, String) {
        (
            format!("{:4.4} {}", n, self.step),
            format!(
                "cpu:{}, mem:{}, prngs:{}/{}, objs:{}/{}, vm:{}/{}, evt:{}, store:{}/{}, foot:{}, stk:{}, auth:{}/{}",
                self.cpu_insns,
                self.mem_bytes,
                Self::render_hash(self.local_prng_hash),
                Self::render_hash(self.base_prng_hash),
                Self::render_size_and_hash(self.local_objs_size, self.local_objs_hash),
                Self::render_size_and_hash(self.global_objs_size, self.global_objs_hash),
                Self::render_size_and_hash(self.vm_mem_size, self.vm_mem_hash),
                Self::render_size_and_hash(self.vm_exports_size, self.vm_exports_hash),
                Self::render_size_and_hash(self.events_size, self.events_hash),
                Self::render_size_and_hash(self.instance_storage_size, self.instance_storage_hash),
                Self::render_size_and_hash(self.ledger_storage_size, self.ledger_storage_hash),
                Self::render_size_and_hash(self.storage_footprint_size, self.storage_footprint_hash),
                Self::render_size_and_hash(self.context_stack_size, self.context_stack_hash),
                Self::render_size_and_hash(self.auth_stack_size, self.auth_stack_hash),
                Self::render_size_and_hash(self.auth_trackers_size, self.auth_trackers_hash),
            ),
        )
    }
    pub(crate) fn observe(host: &Host, step: Step) -> Option<Self> {
        let budget = host.budget_ref();
        if budget.ensure_shadow_cpu_limit_factor(100).is_err() {
            return None;
        }
        let mut obs = None;
        budget.with_shadow_mode(|| {
            let (vm_mem_hash, vm_mem_size) = host.memory_hash_and_size();
            let (vm_exports_hash, vm_exports_size) = host.vm_exports_hash_and_size();
            let (auth_trackers_hash, auth_trackers_size) = host.auth_trackers_hash_and_size();

            obs = Some(Observation {
                step,
                cpu_insns: host.as_budget().get_cpu_insns_consumed()?,
                mem_bytes: host.as_budget().get_mem_bytes_consumed()?,
                local_prng_hash: host.local_prng_hash(),
                base_prng_hash: host.base_prng_hash(),
                local_objs_size: host.local_objs_size(),
                local_objs_hash: host.local_objs_hash(),
                global_objs_size: host.global_objs_size(),
                global_objs_hash: host.global_objs_hash(),
                vm_mem_size,
                vm_mem_hash,
                vm_exports_size,
                vm_exports_hash,
                events_size: host.events_size(),
                events_hash: host.events_hash(),
                instance_storage_size: host.instance_storage_size(),
                instance_storage_hash: host.instance_storage_hash(),
                ledger_storage_size: host.ledger_storage_size(),
                ledger_storage_hash: host.ledger_storage_hash(),
                storage_footprint_size: host.storage_footprint_size(),
                storage_footprint_hash: host.storage_footprint_hash(),
                context_stack_size: host.context_stack_size(),
                context_stack_hash: host.context_stack_hash(),
                auth_stack_size: host.auth_stack_size(),
                auth_stack_hash: host.auth_stack_hash(),
                auth_trackers_size,
                auth_trackers_hash,
            });
            Ok(())
        });
        obs
    }
}
