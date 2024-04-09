#![allow(dead_code)]
use crate::{
    budget::AsBudget,
    events::InternalEvent,
    host::{
        metered_hash::{CountingHasher, MeteredHash, MeteredHashXdr},
        Context, Frame,
    },
    Host, HostError, Val,
};
use std::{fmt::Debug, hash::Hasher, rc::Rc};

// Formatting TraceEvents and TraceStates is done in a submodule.
mod fmt;

// When capturing a TraceState, we want to ensure that the shadow budget is much higher
// than normal; not so high that it will run forever but high enough that we can manage
// to actually record the quantity of detail that tracing records (eg. hashing everything
// in the host on every host function call and return!).
const TRACE_STATE_SHADOW_CPU_LIMIT_FACTOR: u64 = 500;
const TRACE_STATE_SHADOW_MEM_LIMIT_FACTOR: u64 = 30;

pub type TraceHook = Rc<dyn for<'a> Fn(&'a Host, TraceEvent<'a>) -> Result<(), HostError>>;

pub enum TraceEvent<'a> {
    Begin,
    #[allow(private_interfaces)]
    PushCtx(&'a Context),
    #[allow(private_interfaces)]
    PopCtx(&'a Context, &'a Result<Val, &'a HostError>),
    EnvCall(&'static str, &'a [&'a dyn Debug]),
    EnvRet(&'static str, &'a Result<&'a dyn Debug, &'a HostError>),
    End,
}

/// A TraceRecord is simply a pair of a [TraceEvent] and a [TraceState] with a
/// convenience constructor that tries to build the [TraceState] and returns the
/// [TraceEvent] in a `Result::Err` if it fails (eg. if the shadow budget is
/// exhausted).
pub struct TraceRecord<'a> {
    pub event: TraceEvent<'a>,
    pub state: TraceState,
}

impl<'a> TraceRecord<'a> {
    pub fn new(host: &Host, event: TraceEvent<'a>) -> Result<Self, TraceEvent<'a>> {
        match TraceState::new(host) {
            Some(state) => Ok(Self { event, state }),
            None => Err(event),
        }
    }
}

/// TraceState holds a summary of the state of a Soroban host at a particular moment.
/// It is relatively expensive to capture, and is captured under the shadow budget.
/// If the shadow budget is exhausted, capturing the TraceState will fail (which is
/// why its constructor returns an `Option`).
pub struct TraceState {
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

impl TraceState {
    pub fn new(host: &Host) -> Option<Self> {
        let budget = host.budget_ref();
        if budget
            .ensure_shadow_cpu_limit_factor(TRACE_STATE_SHADOW_CPU_LIMIT_FACTOR)
            .is_err()
        {
            return None;
        }
        if budget
            .ensure_shadow_mem_limit_factor(TRACE_STATE_SHADOW_MEM_LIMIT_FACTOR)
            .is_err()
        {
            return None;
        }
        let mut state = None;
        budget.with_shadow_mode(|| {
            let (vm_mem_hash, vm_mem_size) = host.memory_hash_and_size();
            let (vm_exports_hash, vm_exports_size) = host.vm_exports_hash_and_size();
            let (auth_trackers_hash, auth_trackers_size) = host.auth_trackers_hash_and_size();
            state = Some(TraceState {
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
        state
    }
}

impl Host {
    fn hash_one<H: MeteredHash>(&self, h: &H) -> u64 {
        let mut state = CountingHasher::default();
        self.budget_ref()
            .with_shadow_mode(|| h.metered_hash(&mut state, self.budget_ref()));
        state.finish()
    }

    fn hash_iter<H: MeteredHash>(&self, h: impl Iterator<Item = H>) -> u64 {
        let mut state = CountingHasher::default();
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
        if let Ok(prng) = self.0.base_prng.try_borrow() {
            self.hash_optional(&*prng)
        } else {
            0
        }
    }
    fn local_prng_hash(&self) -> u64 {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
            if let Some(ctx) = ctxs.last() {
                return self.hash_optional(&ctx.prng);
            }
        }
        0
    }
    fn local_objs_size(&self) -> usize {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
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
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
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
        if let Ok(objs) = self.0.objects.try_borrow() {
            objs.len()
        } else {
            0
        }
    }
    fn global_objs_hash(&self) -> u64 {
        if let Ok(objs) = self.0.objects.try_borrow() {
            self.hash_iter(objs.iter())
        } else {
            0
        }
    }
    fn memory_hash_and_size(&self) -> (u64, usize) {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
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
        if let Ok(evts) = self.0.events.try_borrow() {
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
        if let Ok(evts) = self.0.events.try_borrow() {
            self.hash_iter(evts.vec.iter().filter(|(e, _)| match e {
                InternalEvent::Contract(_) => true,
                InternalEvent::Diagnostic(_) => false,
            }))
        } else {
            0
        }
    }
    fn instance_storage_size(&self) -> usize {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
            if let Some(ctx) = ctxs.last() {
                if let Some(storage) = &ctx.storage {
                    return storage.map.len();
                }
            }
        }
        0
    }
    fn instance_storage_hash(&self) -> u64 {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
            if let Some(ctx) = ctxs.last() {
                if let Some(storage) = &ctx.storage {
                    return self.hash_one(&storage.map);
                }
            }
        }
        0
    }
    fn ledger_storage_size(&self) -> usize {
        if let Ok(store) = self.0.storage.try_borrow() {
            store.map.len()
        } else {
            0
        }
    }
    fn ledger_storage_hash(&self) -> u64 {
        // Storage is full of XDR types that contain ExtensionPoints. Due to the way
        // ExtensionPoint works (it changes size in Rust, defeating its identity in XDR)
        // we can't use normal Rust hashing for this type. Instead we manually walk the
        // map and hash the XDR of all the XDR-bits in it.
        if let Ok(store) = self.0.storage.try_borrow() {
            let mut state = CountingHasher::default();
            let budget = self.budget_ref();
            budget.with_shadow_mode(|| {
                store.map.len().metered_hash(&mut state, budget)?;
                for (k, v) in store.map.iter(budget)? {
                    k.metered_hash_xdr(&mut state, budget)?;
                    match v {
                        Some((entry, ttl)) => {
                            0.metered_hash(&mut state, budget)?;
                            entry.metered_hash_xdr(&mut state, budget)?;
                            ttl.metered_hash(&mut state, budget)?;
                        }
                        None => {
                            1.metered_hash(&mut state, budget)?;
                        }
                    }
                }
                Ok(())
            });
            state.finish()
        } else {
            0
        }
    }

    fn auth_stack_size(&self) -> usize {
        if let Ok(auth_mgr) = self.0.authorization_manager.try_borrow() {
            auth_mgr.stack_size()
        } else {
            0
        }
    }
    fn auth_stack_hash(&self) -> u64 {
        if let Ok(auth_mgr) = self.0.authorization_manager.try_borrow() {
            if let Ok(hash) = auth_mgr.stack_hash(self.budget_ref()) {
                return hash;
            }
        }
        0
    }
    fn auth_trackers_hash_and_size(&self) -> (u64, usize) {
        if let Ok(auth_mgr) = self.0.authorization_manager.try_borrow() {
            if let Ok(pair) = auth_mgr.trackers_hash_and_size(self.budget_ref()) {
                return pair;
            }
        }
        (0, 0)
    }
    fn context_stack_size(&self) -> usize {
        if let Ok(frames) = self.0.context_stack.try_borrow() {
            frames.len()
        } else {
            0
        }
    }
    fn context_stack_hash(&self) -> u64 {
        if let Ok(frames) = self.0.context_stack.try_borrow() {
            self.hash_iter(frames.iter())
        } else {
            0
        }
    }
    fn storage_footprint_size(&self) -> usize {
        if let Ok(storage) = self.0.storage.try_borrow() {
            storage.footprint.0.len()
        } else {
            0
        }
    }
    fn storage_footprint_hash(&self) -> u64 {
        // See commentary on ExtensionPoint above in ledger_storage_hash.
        if let Ok(storage) = self.0.storage.try_borrow() {
            let mut state = CountingHasher::default();
            let budget = self.budget_ref();
            budget.with_shadow_mode(|| {
                storage.footprint.0.len().metered_hash(&mut state, budget)?;
                for (k, v) in storage.footprint.0.iter(budget)? {
                    k.metered_hash_xdr(&mut state, budget)?;
                    v.metered_hash(&mut state, budget)?;
                }
                Ok(())
            });
            state.finish()
        } else {
            0
        }
    }

    fn vm_exports_hash_and_size(&self) -> (u64, usize) {
        if let Ok(ctxs) = self.0.context_stack.try_borrow() {
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
