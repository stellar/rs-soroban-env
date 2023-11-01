// This is a test-support module that exists to help us notice when we change
// the semantics of the host unintentionally. It works by recording a trace of
// "observations" at each "step" of a running test. Steps mostly correspond to
// the host lifecycle events -- frame push/pop and host function calls -- as
// well as a step at the end of the test.
//
// Observations include a short summary of everything a contract _might_
// observe: local and global objects, local and global storage, events, PRNGs,
// guest linear memory, and budgets. The results are stored in maps and saved to
// disk in a file called "observations.json" which should exist in the crate
// root.
//
// If observations differ from previous runs, it's considered an error. If you
// want to make an intentional change, run with UPDATE_OBSERVATIONS=1.

use crate::{
    budget::AsBudget,
    events::{EventError, InternalEvent},
    host::{Frame, HostLifecycleEvent},
    host_object::{HostMap, HostVec},
    storage::StorageMap,
    xdr::{self, ContractExecutable},
    Host, HostError, Symbol, SymbolObject, SymbolSmall,
};
use serde::{Deserialize, Serialize};
use std::{
    collections::{hash_map::DefaultHasher, BTreeMap},
    env,
    fmt::Display,
    fs::File,
    hash::{Hash, Hasher},
    path::PathBuf,
    rc::Rc,
    sync::{Mutex, OnceLock},
};

const FILENAME: &'static str = "observations.json";

fn full_path() -> PathBuf {
    let root: PathBuf = env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR environment variable is required to be set")
        .into();
    root.join(FILENAME)
}

fn update_observations() -> bool {
    env::var("UPDATE_OBSERVATIONS").is_ok()
}

impl Observations {
    fn new() -> Self {
        Self(Mutex::new(BTreeMap::new()))
    }

    fn load() -> Self {
        println!("reading {}", full_path().display());
        let file = File::open(&full_path()).expect(&format!("unable to open {FILENAME}"));
        serde_json::from_reader(file).expect(&format!("failed to parse {FILENAME}"))
    }

    fn save(&self) {
        println!("writing {}", full_path().display());
        let file = File::create(&full_path()).expect(&format!("unable to create {FILENAME}"));
        serde_json::to_writer_pretty(file, self).expect(&format!("error writing {FILENAME}"))
    }

    // Check records the new observation by appending it to the vector
    // associated with `name` in the new observations map. When the `End` event
    // fires, it also compares the now-complete new observations for `name`
    // against the last recorded observations in the old observation map, and if
    // they differ it prints the indexes at which observations changed. If it's
    // _not_ in update_observations mode (i.e. it's enforcing) it also calls
    // assert_eq! on the observations at this point, which will cause an
    // observed test to fail if there were differences from the old recording.
    fn check(old: &Observations, new: &Observations, name: &'static str, obs: Observation) {
        let name = if let Some((_, rest)) = name.split_once("::") {
            rest.to_string()
        } else {
            name.to_string()
        };

        let is_end = obs.step == Step::End;

        let mut disagreement: Option<(usize, String, String)> = None;

        let mut new_guard = new.0.lock().unwrap();
        let new_map = new_guard.entry(name.clone()).or_default();
        let (id, obs) = obs.render(new_map.len());
        new_map.insert(id.clone(), obs);

        if is_end {
            let mut old_guard = old.0.lock().unwrap();
            let old_map = old_guard.entry(id.clone()).or_default();
            if old_map.len() != new_map.len() {
                println!("old and new observations of {name} have different lengths");
                disagreement = Some((
                    old_map.len(),
                    old_map.len().to_string(),
                    new_map.len().to_string(),
                ));
            }

            for (i, ((old_key, old_val), (new_key, new_val))) in
                old_map.iter().zip(new_map.iter()).enumerate()
            {
                if old_key != new_key {
                    println!("observation key {i} of {name} changed since last recording");
                    disagreement = Some((i, old_key.clone(), new_key.clone()));
                }
                if old_val != new_val {
                    println!("observation val {i} of {name} changed since last recording");
                    disagreement = Some((i, old_val.clone(), new_val.clone()));
                }
            }
            // Drop the guard before any panic, so we don't poison the lock.
            drop(old_guard);
            drop(new_guard);
            if let Some((i, old, new)) = disagreement {
                if !update_observations() {
                    assert_eq!(
                        old, new,
                        "\n\nobservation {i} of {name} changed since last recording\n\
                                note: if this is intentional, re-run with UPDATE_OBSERVATIONS=1\n"
                    );
                }
            }
        }
    }
}

extern "C" {
    fn atexit(hook: extern "C" fn()) -> std::os::raw::c_int;
}
extern "C" fn save_hook() {
    if update_observations() {
        get_new_obs().save();
    }
}

static OLD_OBS: OnceLock<Observations> = OnceLock::new();
static NEW_OBS: OnceLock<Observations> = OnceLock::new();

fn get_old_obs() -> &'static Observations {
    OLD_OBS.get_or_init(|| Observations::load())
}

fn get_new_obs() -> &'static Observations {
    NEW_OBS.get_or_init(|| {
        if update_observations() {
            unsafe {
                atexit(save_hook);
            }
        }
        Observations::new()
    })
}

#[derive(Debug, Serialize, Deserialize)]
struct Observations(Mutex<BTreeMap<String, BTreeMap<String, String>>>);

impl Clone for Observations {
    fn clone(&self) -> Self {
        Self(Mutex::new(self.0.lock().unwrap().clone()))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Step {
    Enter {
        contract: String,
        func: String,
        args: Vec<String>,
    },
    Leave {
        contract: String,
        func: String,
        result: String,
    },
    Call(String, Vec<String>),
    Ret(String, String),
    End,
}

impl Display for Step {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Step::Enter {
                contract,
                func,
                args,
            } => {
                write!(f, "enter {contract}:{func}(")?;
                for (i, arg) in args.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", arg)?;
                }
                write!(f, ")")
            }
            Step::Leave {
                contract,
                func,
                result,
            } => {
                write!(f, "leave {contract}:{func} -> {result}")
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
            Step::Ret(hostfn, result) => {
                write!(f, "ret {} -> {}", hostfn, result)
            }
            Step::End => write!(f, "end"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
struct Observation {
    step: Step,
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
    events_size: usize,
    events_hash: u64,
    instance_storage_size: usize,
    instance_storage_hash: u64,
    ledger_storage_size: usize,
    ledger_storage_hash: u64,
}

fn hash_one<H: Hash>(h: &H) -> u64 {
    let mut state = DefaultHasher::default();
    h.hash(&mut state);
    state.finish()
}

fn hash_iter<H: Hash>(h: impl Iterator<Item = H>) -> u64 {
    let mut state = DefaultHasher::default();
    h.for_each(|i| i.hash(&mut state));
    state.finish()
}

fn hash_optional<H: Hash>(h: &Option<H>) -> u64 {
    if let Some(x) = h {
        hash_one(x)
    } else {
        0
    }
}

impl Hash for HostVec {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.vec.len().hash(state);
        hash_iter(self.vec.iter().map(|x| x.get_payload()));
    }
}

impl Hash for HostMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.len().hash(state);
        hash_iter(
            self.map
                .iter()
                .map(|(x, y)| (x.get_payload(), y.get_payload())),
        );
    }
}

impl Hash for StorageMap {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.map.hash(state);
    }
}

impl Hash for InternalEvent {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            InternalEvent::Contract(c) => {
                c.type_.hash(state);
                match c.contract_id {
                    Some(cid) => cid.to_val().get_payload().hash(state),
                    None => 0.hash(state),
                }
                c.data.get_payload().hash(state);
                c.topics.to_val().get_payload().hash(state);
            }
            InternalEvent::Diagnostic(_) => (),
        }
    }
}

impl Hash for EventError {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            EventError::FromFailedCall => 0.hash(state),
            EventError::FromSuccessfulCall => 1.hash(state),
        }
    }
}

impl Host {
    fn base_prng_hash(&self) -> u64 {
        if let Ok(prng) = self.try_borrow_base_prng() {
            hash_optional(&*prng)
        } else {
            0
        }
    }
    fn local_prng_hash(&self) -> u64 {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                return hash_optional(&ctx.prng);
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
                    return hash_iter(relative_objects.iter().map(|x| x.to_val().get_payload()));
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
            hash_iter(objs.iter())
        } else {
            0
        }
    }
    fn memory_size(&self) -> usize {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM { vm, .. } = &ctx.frame {
                    if let Some(mem) = vm.memory {
                        if let Ok(len) =
                            vm.with_vmcaller(|vmcaller| Ok(mem.data(vmcaller.try_ref()?).len()))
                        {
                            return len;
                        }
                    }
                }
            }
        }
        0
    }
    fn memory_hash(&self) -> u64 {
        if let Ok(ctxs) = self.try_borrow_context_stack() {
            if let Some(ctx) = ctxs.last() {
                if let Frame::ContractVM { vm, .. } = &ctx.frame {
                    if let Some(mem) = vm.memory {
                        if let Ok(hash) = vm
                            .with_vmcaller(|vmcaller| Ok(hash_one(&mem.data(vmcaller.try_ref()?))))
                        {
                            return hash;
                        }
                    }
                }
            }
        }
        0
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
            hash_iter(evts.vec.iter().filter(|(e, _)| match e {
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
                    return hash_one(&storage.map);
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
            hash_one(&store.map)
        } else {
            0
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
        if size == 0 && hash == 0 {
            "-".to_string()
        } else {
            format!("{}@{}", size, Self::render_hash(hash))
        }
    }

    // Render needs to return a (key,value) pair for step number `n` in a given
    // test. The returned key needs to be unique and should probably start with
    // a rendering of the number `n`.
    fn render(&self, n: usize) -> (String, String) {
        (
            format!("{:4.4} {}", n, self.step),
            format!(
                "cpu:{}, mem:{}, prngs:{}/{}, objs:{}/{}, vmem:{}, evt:{}, store:{}/{}",
                self.cpu_insns,
                self.mem_bytes,
                Self::render_hash(self.local_prng_hash),
                Self::render_hash(self.base_prng_hash),
                Self::render_size_and_hash(self.local_objs_size, self.local_objs_hash),
                Self::render_size_and_hash(self.global_objs_size, self.global_objs_hash),
                Self::render_size_and_hash(self.vm_mem_size, self.vm_mem_hash),
                Self::render_size_and_hash(self.events_size, self.events_hash),
                Self::render_size_and_hash(self.instance_storage_size, self.instance_storage_hash),
                Self::render_size_and_hash(self.ledger_storage_size, self.ledger_storage_hash)
            ),
        )
    }

    fn observe(host: &Host, evt: Option<HostLifecycleEvent<'_>>) -> Self {
        let step = if let Some(e) = evt {
            Step::from_host_lifecycle_event(host, e)
        } else {
            Step::End
        };
        Observation {
            step,
            cpu_insns: host.as_budget().get_cpu_insns_consumed().unwrap(),
            mem_bytes: host.as_budget().get_mem_bytes_consumed().unwrap(),
            local_prng_hash: host.local_prng_hash(),
            base_prng_hash: host.base_prng_hash(),
            local_objs_size: host.local_objs_size(),
            local_objs_hash: host.local_objs_hash(),
            global_objs_size: host.global_objs_size(),
            global_objs_hash: host.global_objs_hash(),
            vm_mem_size: host.memory_size(),
            vm_mem_hash: host.memory_hash(),
            events_size: host.events_size(),
            events_hash: host.events_hash(),
            instance_storage_size: host.instance_storage_size(),
            instance_storage_hash: host.instance_storage_hash(),
            ledger_storage_size: host.ledger_storage_size(),
            ledger_storage_hash: host.ledger_storage_hash(),
        }
    }
}

#[macro_export]
macro_rules! function_name {
    () => {{
        // This is the canonical hack used to get a function's name,
        // copied from the stdext crate (and stackoverflow).
        fn f() {}
        fn type_name_of<T>(_: T) -> &'static str {
            std::any::type_name::<T>()
        }
        let name = type_name_of(f);
        // `3` is the length of the `::f`.
        &name[..name.len() - 3]
    }};
}

#[macro_export]
macro_rules! observe_host {
    ($host:expr) => {
        $crate::test::observe::ObservedHost::new($crate::function_name!(), $host)
    };
}

pub(crate) struct ObservedHost {
    testname: &'static str,
    host: Host,
}

impl Step {
    fn from_host_lifecycle_event(host: &Host, evt: HostLifecycleEvent<'_>) -> Self {
        match evt {
            HostLifecycleEvent::PushContext => {
                let stack = host.try_borrow_context_stack().expect("borrowing context");
                let context = stack.last().expect("last context");
                let (contract, func, args) = Self::frame_contract_func_and_args(&context.frame);
                Step::Enter {
                    contract,
                    func,
                    args,
                }
            }
            HostLifecycleEvent::PopContext(context, result) => {
                let (contract, func, _) = Self::frame_contract_func_and_args(&context.frame);
                // We only want to see the error code here, not debuginfo.
                let result = format!("{:?}", result.clone().map_err(|he| he.error));
                Step::Leave {
                    contract,
                    func,
                    result,
                }
            }
            HostLifecycleEvent::VmCall(fname, args) => {
                Step::Call(fname.to_string(), args.iter().cloned().collect())
            }
            HostLifecycleEvent::VmReturn(fname, ret) => {
                Step::Ret(fname.to_string(), format!("{:?}", ret))
            }
        }
    }
    fn short_hash(hash: &xdr::Hash) -> String {
        let word: [u8; 4] = hash.0[0..4]
            .try_into()
            .expect("extracting 4 bytes from hash");
        format!("{:4.4x}", u32::from_be_bytes(word))
    }
    fn exec(exec: &ContractExecutable) -> String {
        match exec {
            ContractExecutable::Wasm(hash) => Self::short_hash(hash),
            ContractExecutable::StellarAsset => "SAC".to_string(),
        }
    }
    fn sym(s: &Symbol) -> String {
        if let Ok(small) = SymbolSmall::try_from(s.clone()) {
            small.to_string()
        } else if let Ok(obj) = SymbolObject::try_from(s.clone()) {
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
                format!("{}", Self::sym(fn_name)),
                args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
            // TODO: this isn't ideal, we should capture the args in Frame::HostFunction
            Frame::HostFunction(ty) => (format!("{:?}", ty), "".to_string(), vec![]),
            Frame::StellarAssetContract(id, fn_name, args, _) => (
                format!("SAC:{}", Self::short_hash(id)),
                format!("{}", Self::sym(fn_name)),
                args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
            Frame::TestContract(tc) => (
                format!("TEST:{}", Self::short_hash(&tc.id)),
                format!("{}", Self::sym(&tc.func)),
                tc.args.iter().map(|arg| format!("{:?}", arg)).collect(),
            ),
        }
    }
}

fn make_obs_hook(
    testname: &'static str,
) -> Rc<dyn for<'a> Fn(&'a Host, HostLifecycleEvent<'a>) -> Result<(), HostError>> {
    Rc::new(move |host, evt| {
        let obs = Observation::observe(host, Some(evt));
        Observations::check(get_old_obs(), get_new_obs(), testname, obs);
        Ok(())
    })
}

impl ObservedHost {
    pub fn new(testname: &'static str, host: Host) -> Self {
        let hook = make_obs_hook(testname);
        host.set_lifecycle_event_hook(Some(hook))
            .expect("installing host lifecycle hook");
        Self { testname, host }
    }
}

impl std::ops::Deref for ObservedHost {
    type Target = Host;

    fn deref(&self) -> &Self::Target {
        &self.host
    }
}

impl Drop for ObservedHost {
    fn drop(&mut self) {
        self.host
            .set_lifecycle_event_hook(None)
            .expect("resetting host lifecycle hook");
        let obs = Observation::observe(&self.host, None);
        Observations::check(get_old_obs(), get_new_obs(), self.testname, obs)
    }
}
