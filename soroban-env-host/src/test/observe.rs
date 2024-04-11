// This is a test-support module that exists to help us notice when we change
// the semantics of the host unintentionally. It works by hooking into the
// host's tracing system, and recording the trace of a test into a reference
// file"observations/some_testname.json" associated with each named test.
//
// If observations differ from previous runs, it's considered an error. If you
// want to make an intentional change, run with UPDATE_OBSERVATIONS=1.

#![cfg_attr(any(feature = "next", not(feature = "testutils")), allow(dead_code))]
#![cfg_attr(
    any(feature = "next", not(feature = "testutils")),
    allow(unused_imports)
)]

use crate::{
    host::{
        error::HostError,
        trace::{TraceEvent, TraceRecord},
    },
    Host,
};

use itertools::Itertools;
use std::{cell::RefCell, collections::BTreeMap, env, fs::File, path::PathBuf, rc::Rc};

fn full_path(protocol: u32, testname: &str) -> PathBuf {
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
        .join(protocol.to_string())
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

#[cfg(all(not(feature = "next"), feature = "testutils"))]
impl Observations {
    fn load(protocol: u32, testname: &str) -> Self {
        let path = full_path(protocol, testname);
        let obs: BTreeMap<String, String> = if path.exists() {
            println!("reading {}", path.display());
            let file = File::open(&path).expect(&format!("unable to open {}", path.display()));
            serde_json::from_reader(file).expect(&format!("failed to parse {}", path.display()))
        } else {
            BTreeMap::new()
        };
        Self(obs)
    }

    fn save(&self, protocol: u32, testname: &str) {
        let path = full_path(protocol, testname);
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
    fn check(
        old: &Observations,
        new: &mut Observations,
        protocol: u32,
        name: &'static str,
        tr: TraceRecord,
    ) {
        let mut disagreement: Option<(usize, String, String)> = None;

        if tr.event.is_begin() {
            new.0.clear();
        }

        // We use a pseudo-entry to track the last-written entry while building
        // a map to enable each non-begin-or-end stored line to be a diff from
        // the last line's full state, for greater compactness and legibility.
        const PREV_FULL: &str = "___PREV_FULL";
        let prev = new.0.remove(PREV_FULL).unwrap_or_default();

        let key = format!("{:4.4} {}", new.0.len(), tr.event);
        let value = format!("{}", tr.state);

        new.0.insert(PREV_FULL.to_string(), value.clone());
        if tr.event.is_begin() || tr.event.is_end() {
            new.0.insert(key.clone(), value);
        } else {
            let diff = diff_line(&prev, &value);
            new.0.insert(key.clone(), diff);
        }

        if tr.event.is_end() {
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
                new.save(protocol, &name);
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

pub(crate) struct ObservedHost {
    testname: &'static str,
    old_obs: Rc<RefCell<Observations>>,
    new_obs: Rc<RefCell<Observations>>,
    host: Host,
    protocol: u32,
}

impl ObservedHost {
    #[cfg(any(feature = "next", not(feature = "testutils")))]
    pub(crate) fn new(testname: &'static str, host: Host) -> Self {
        let protocol = 0;
        let old_obs = Rc::new(RefCell::new(Observations::default()));
        let new_obs = Rc::new(RefCell::new(Observations::default()));
        Self {
            old_obs,
            new_obs,
            testname,
            host,
            protocol,
        }
    }

    #[cfg(all(not(feature = "next"), feature = "testutils"))]
    pub(crate) fn new(testname: &'static str, host: Host) -> Self {
        let protocol = Host::current_test_protocol();
        let old_obs = Rc::new(RefCell::new(Observations::load(protocol, testname)));
        let new_obs = Rc::new(RefCell::new(Observations::default()));
        let oh = Self {
            old_obs,
            new_obs,
            testname,
            host,
            protocol,
        };
        oh.host
            .set_trace_hook(Some(oh.make_obs_hook()))
            .expect("installing host lifecycle hook");
        oh
    }

    #[cfg(all(not(feature = "next"), feature = "testutils"))]
    fn make_obs_hook(
        &self,
    ) -> Rc<dyn for<'a> Fn(&'a Host, TraceEvent<'a>) -> Result<(), HostError>> {
        let old_obs = self.old_obs.clone();
        let new_obs = self.new_obs.clone();
        let testname = self.testname;
        let protocol = self.protocol;
        Rc::new(move |host, evt| {
            let tr = TraceRecord::new(host, evt).expect("observing host");
            Observations::check(
                &old_obs.borrow(),
                &mut new_obs.borrow_mut(),
                protocol,
                testname,
                tr,
            );
            Ok(())
        })
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
            .set_trace_hook(None)
            .expect("resetting host lifecycle hook");
    }
}
