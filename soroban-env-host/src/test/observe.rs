// This is a test-support module that exists to help us notice when we change
// the semantics of the host unintentionally. It works by recording a trace of
// "observations" at each "step" of a running test. Steps mostly correspond to
// the host lifecycle events -- frame push/pop and host function calls -- as
// well as a step at the end of the test.
//
// Observations include a short summary of everything a contract _might_
// observe: local and global objects, local and global storage, events, PRNGs,
// guest linear memory, and budgets. The results are stored in maps and saved to
// disk in files called "observations/some_testname.json" based on the testnames
// passed in.
//
// If observations differ from previous runs, it's considered an error. If you
// want to make an intentional change, run with UPDATE_OBSERVATIONS=1.

#![cfg_attr(any(feature = "next", not(feature = "testutils")), allow(dead_code))]

#[cfg(all(not(feature = "next"), feature = "testutils"))]
use crate::host::{error::HostError, HostLifecycleEvent};

use crate::{
    budget::AsBudget,
    events::{EventError, InternalEvent},
    host::Frame,
    host_object::{HostMap, HostVec},
    storage::StorageMap,
    xdr::{self, ContractExecutable},
    Host, Symbol, SymbolObject, SymbolSmall,
};
use itertools::Itertools;
use std::{
    cell::RefCell,
    collections::{hash_map::DefaultHasher, BTreeMap},
    env,
    fmt::Display,
    fs::File,
    hash::{Hash, Hasher},
    path::PathBuf,
    rc::Rc,
};

fn full_path(testname: &str) -> PathBuf {
    let testname = if let Some((_, rest)) = testname.split_once("::") {
        rest.to_string()
    } else {
        testname.to_string()
    };
    let filename = testname.split("::").join("__");

    let root: PathBuf = match env::var("CARGO_MANIFEST_DIR") {
        Ok(dir) => dir.into(),
        Err(_) => env::current_dir()
            .expect("unable to get current dir")
            .join("soroban-env-host"),
    };
    root.join("observations")
        .join(filename)
        .with_extension("json")
}

fn update_observations() -> bool {
    env::var("UPDATE_OBSERVATIONS").is_ok()
}

fn diff_line(last: &String, new: &String) -> String {
    last.split(',')
        .zip(new.split(','))
        .filter(|(a, b)| a != b)
        .map(|(_, b)| b)
        .join(",")
}

impl Observations {
    fn load(testname: &str) -> Self {
        let path = full_path(testname);
        let obs: BTreeMap<String, String> = if path.exists() {
            println!("reading {}", path.display());
            let file = File::open(&path).expect(&format!("unable to open {}", path.display()));
            serde_json::from_reader(file).expect(&format!("failed to parse {}", path.display()))
        } else {
            BTreeMap::new()
        };
        Self(obs)
    }

    fn save(&self, testname: &str) {
        let path = full_path(testname);
        println!("writing {}", path.display());
        let file = File::create(&path).expect(&format!("unable to create {}", path.display()));
        serde_json::to_writer_pretty(file, &self.0)
            .expect(&format!("error writing {}", path.display()));
    }

    // Check records the new observation by appending it to the vector
    // associated with `name` in the new observations map. When the `End` event
    // fires, it also compares the now-complete new observations for `name`
    // against the last recorded observations in the old observation map, and if
    // they differ it prints the indexes at which observations changed. If it's
    // _not_ in update_observations mode (i.e. it's enforcing) it also calls
    // assert_eq! on the observations at this point, which will cause an
    // observed test to fail if there were differences from the old recording.
    fn check(old: &Observations, new: &mut Observations, name: &'static str, ob: Observation) {
        let mut disagreement: Option<(usize, String, String)> = None;

        if ob.step == Step::Begin {
            new.0.clear();
        }

        // We use a pseudo-entry to track the last-written entry while building
        // a map to enable each non-begin-or-end stored line to be a diff from
        // the last line's full state, for greater compactness and legibility.
        const PREV_FULL: &str = "___PREV_FULL";
        let prev = new.0.remove(PREV_FULL).unwrap_or_default();
        let (id, full) = ob.render(new.0.len());
        new.0.insert(PREV_FULL.to_string(), full.clone());
        if ob.step == Step::Begin || ob.step == Step::End {
            new.0.insert(id.clone(), full);
        } else {
            let diff = diff_line(&prev, &full);
            new.0.insert(id.clone(), diff);
        }

        if ob.step == Step::End {
            new.0.remove(PREV_FULL);
            if old.0.len() != new.0.len() {
                println!("old and new observations of {name} have different lengths");
                disagreement = Some((
                    old.0.len(),
                    old.0.len().to_string(),
                    new.0.len().to_string(),
                ));
            }

            for (i, ((old_key, old_val), (new_key, new_val))) in
                old.0.iter().zip(new.0.iter()).enumerate()
            {
                if old_key != new_key {
                    println!("observation key {i} of {name} changed since last recording");
                    disagreement = Some((i, old_key.clone(), new_key.clone()));
                    break;
                }
                if old_val != new_val {
                    println!("observation val {i} of {name} changed since last recording");
                    disagreement = Some((i, old_val.clone(), new_val.clone()));
                    break;
                }
            }
            if update_observations() {
                new.save(&name);
            } else {
                if let Some((i, old, new)) = disagreement {
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

#[derive(Debug, Default, Clone)]
struct Observations(BTreeMap<String, String>);

#[derive(Clone, Debug, PartialEq, Eq)]
enum Step {
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
        if size == 0 {
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

    fn observe(host: &Host, step: Step) -> Self {
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

pub(crate) struct ObservedHost {
    testname: &'static str,
    old_obs: Rc<RefCell<Observations>>,
    new_obs: Rc<RefCell<Observations>>,
    host: Host,
}

impl Step {
    #[cfg(all(not(feature = "next"), feature = "testutils"))]
    fn from_host_lifecycle_event(evt: HostLifecycleEvent<'_>) -> Self {
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

impl ObservedHost {
    #[cfg(any(feature = "next", not(feature = "testutils")))]
    pub(crate) fn new(testname: &'static str, host: Host) -> Self {
        let old_obs = Rc::new(RefCell::new(Observations::default()));
        let new_obs = Rc::new(RefCell::new(Observations::default()));
        Self {
            old_obs,
            new_obs,
            testname,
            host,
        }
    }

    #[cfg(all(not(feature = "next"), feature = "testutils"))]
    pub(crate) fn new(testname: &'static str, host: Host) -> Self {
        let old_obs = Rc::new(RefCell::new(Observations::load(testname)));
        let new_obs = Rc::new(RefCell::new(Observations::default()));
        let oh = Self {
            old_obs,
            new_obs,
            testname,
            host,
        };
        oh.observe_and_check(Step::Begin);
        oh.host
            .set_lifecycle_event_hook(Some(oh.make_obs_hook()))
            .expect("installing host lifecycle hook");
        oh
    }

    #[cfg(all(not(feature = "next"), feature = "testutils"))]
    fn make_obs_hook(
        &self,
    ) -> Rc<dyn for<'a> Fn(&'a Host, HostLifecycleEvent<'a>) -> Result<(), HostError>> {
        let old_obs = self.old_obs.clone();
        let new_obs = self.new_obs.clone();
        let testname = self.testname;
        Rc::new(move |host, evt| {
            let step = Step::from_host_lifecycle_event(evt);
            let ob = Observation::observe(host, step);
            Observations::check(&old_obs.borrow(), &mut new_obs.borrow_mut(), testname, ob);
            Ok(())
        })
    }

    fn observe_and_check(&self, step: Step) {
        let ob = Observation::observe(&self.host, step);
        Observations::check(
            &self.old_obs.borrow(),
            &mut self.new_obs.borrow_mut(),
            self.testname,
            ob,
        );
    }
}

impl std::ops::Deref for ObservedHost {
    type Target = Host;

    fn deref(&self) -> &Self::Target {
        &self.host
    }
}

#[cfg(all(not(feature = "next"), feature = "testutils"))]
impl Drop for ObservedHost {
    fn drop(&mut self) {
        self.host
            .set_lifecycle_event_hook(None)
            .expect("resetting host lifecycle hook");
        self.observe_and_check(Step::End)
    }
}
