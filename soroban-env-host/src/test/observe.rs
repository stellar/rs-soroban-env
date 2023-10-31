use crate::{budget::AsBudget, Host};
use serde::{Deserialize, Serialize};
use std::{
    collections::{btree_map::Entry, hash_map::DefaultHasher, BTreeMap},
    env,
    fs::File,
    hash::Hasher,
    path::PathBuf,
    sync::{Mutex, OnceLock},
};

const FILENAME: &'static str = "observations.json";

fn full_path() -> PathBuf {
    let root: PathBuf = env::var("CARGO_MANIFEST_DIR")
        .expect("CARGO_MANIFEST_DIR environment variable is required to be set")
        .into();
    root.join(FILENAME)
}

fn should_update_observations() -> bool {
    env::var("UPDATE_OBSERVATIONS").is_ok()
}

impl Observations {
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

    fn check(&self, name: &'static str, host: &Host) {
        let name = if let Some((_, rest)) = name.split_once("::") {
            rest.to_string()
        } else {
            name.to_string()
        };

        let ob = Observation::observe(host);
        let mut guard = self.0.lock().unwrap();
        let entry = guard.entry(name.clone());
        if should_update_observations() {
            match entry {
                Entry::Vacant(v) => {
                    println!("recording new observation for {name}");
                    v.insert(ob);
                }
                Entry::Occupied(mut oc) => {
                    if oc.get() == &ob {
                        println!("saw unchanged observation for {name}");
                    } else {
                        println!("saw different observation for {name}");
                        oc.insert(ob);
                    }
                }
            }
        } else {
            match entry {
                Entry::Vacant(_) => {
                    panic!("missing observation for {name}")
                }
                Entry::Occupied(oc) => {
                    if oc.get() == &ob {
                        println!("found expected observation for {name}")
                    } else {
                        panic!("unexpected observation for {name}: got {ob:?} but expected {oc:?}")
                    }
                }
            }
        }
    }
}

extern "C" {
    fn atexit(hook: extern "C" fn()) -> std::os::raw::c_int;
}
extern "C" fn save_hook() {
    if should_update_observations() {
        get_observations().save();
    }
}

static CELL: OnceLock<Observations> = OnceLock::new();
fn get_observations() -> &'static Observations {
    CELL.get_or_init(|| {
        if should_update_observations() {
            unsafe {
                atexit(save_hook);
            }
        }
        Observations::load()
    })
}

#[derive(Serialize, Deserialize)]
struct Observations(Mutex<BTreeMap<String, Observation>>);

#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
struct Observation {
    cpu_insns: u64,
    mem_bytes: u64,
    base_prng: u64,
}

fn hash_one<H: std::hash::Hash>(h: &H) -> u64 {
    let mut state = DefaultHasher::default();
    h.hash(&mut state);
    state.finish()
}

fn hash_optional<H: std::hash::Hash>(h: &Option<H>) -> u64 {
    if let Some(x) = h {
        hash_one(x)
    } else {
        0
    }
}

impl Observation {
    fn observe(host: &Host) -> Self {
        let base_prng = hash_optional(&*host.try_borrow_base_prng().unwrap());
        Observation {
            cpu_insns: host.as_budget().get_cpu_insns_consumed().unwrap(),
            mem_bytes: host.as_budget().get_mem_bytes_consumed().unwrap(),
            base_prng,
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
        $crate::test::observe::ObservedHost {
            testname: $crate::function_name!(),
            host: $host,
        }
    };
}

pub(crate) struct ObservedHost {
    pub(crate) testname: &'static str,
    pub(crate) host: Host,
}

impl std::ops::Deref for ObservedHost {
    type Target = Host;

    fn deref(&self) -> &Self::Target {
        &self.host
    }
}

impl Drop for ObservedHost {
    fn drop(&mut self) {
        get_observations().check(self.testname, &self.host)
    }
}
