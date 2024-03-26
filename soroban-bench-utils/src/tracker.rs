use std::{
    alloc::System,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
    time::Instant,
};
use tracking_allocator::{
    AllocationGroupToken, AllocationGuard, AllocationRegistry, AllocationTracker, Allocator,
};

use self::cpu::InstructionCounter;

#[global_allocator]
static GLOBAL: Allocator<System> = Allocator::system();

#[derive(Clone)]
struct MemTracker(Arc<AtomicU64>);
impl AllocationTracker for MemTracker {
    fn allocated(
        &self,
        _addr: usize,
        _object_input: usize,
        wrapped_input: usize,
        _group_id: tracking_allocator::AllocationGroupId,
    ) {
        self.0.fetch_add(wrapped_input as u64, Ordering::SeqCst);
    }

    // We do not count memory deallocation because we are trying to estimate the worst-case
    // memory cost under a variety of possible memory allocation models. This mimics the arena
    // model, where memory deallocations do not happen until the very end. This is not perfect
    // as any transient objects are counted as actual memory cost, but it is the worst-case
    // estimate.
    fn deallocated(
        &self,
        _addr: usize,
        _object_input: usize,
        _wrapped_input: usize,
        _source_group_id: tracking_allocator::AllocationGroupId,
        _current_group_id: tracking_allocator::AllocationGroupId,
    ) {
        // No-Op, see comment above.
        ()
    }
}

#[cfg(target_os = "linux")]
mod cpu {
    pub struct InstructionCounter(perf_event::Counter);
    impl InstructionCounter {
        pub fn new() -> Self {
            InstructionCounter(
                perf_event::Builder::new()
                    .build()
                    .expect("perf_event::Builder::new().build()"),
            )
        }
        pub fn begin(&mut self) {
            self.0.reset().expect("perf_event::Counter::reset");
            self.0.enable().expect("perf_event::Counter::enable");
        }
        pub fn end_and_count(&mut self) -> u64 {
            self.0.disable().expect("perf_event::Counter::disable");
            let tandc = self
                .0
                .read_count_and_time()
                .expect("perf_event::Counter::read_count_and_time");
            if tandc.time_enabled == tandc.time_running {
                tandc.count
            } else {
                panic!("time enabled != time running")
            }
        }
    }
}

#[cfg(target_os = "macos")]
#[path = "."]
mod cpu {

    #[path = "rusagev4.rs"]
    mod rusagev4;

    pub struct InstructionCounter(u64);
    impl InstructionCounter {
        fn get() -> u64 {
            use rusagev4::*;
            use std::os::raw::c_int;
            let mut ri = rusage_info_v4::default();
            let pid: c_int = std::process::id() as c_int;
            let ptr: *mut rusage_info_v4 = &mut ri;
            let ret: c_int = unsafe { proc_pid_rusage(pid, RUSAGE_INFO_V4 as c_int, ptr.cast()) };
            assert!(ret == 0, "proc_pid_rusage failed");
            ri.ri_instructions
        }
        pub fn new() -> Self {
            InstructionCounter(Self::get())
        }
        pub fn begin(&mut self) {
            self.0 = Self::get();
        }

        pub fn end_and_count(&mut self) -> u64 {
            let curr = Self::get();
            curr - self.0
        }
    }
}

#[cfg(not(any(target_os = "linux", target_os = "macos")))]
mod cpu {
    pub struct InstructionCounter;
    impl InstructionCounter {
        pub fn new() -> Self {
            InstructionCounter
        }
        pub fn begin(&mut self) {}
        pub fn end_and_count(&mut self) -> u64 {
            0
        }
    }
}

pub struct HostTracker<'a> {
    cpu_insn_counter: InstructionCounter,
    mem_tracker: MemTracker,
    start_time: Instant,
    alloc_guard: Option<AllocationGuard<'a>>,
    #[cfg(feature = "tracy")]
    tracy_span: Option<tracy_client::Span>,
}

impl<'a> HostTracker<'a> {
    pub fn new() -> Self {
        // Setup the instrumentation
        let cpu_insn_counter = cpu::InstructionCounter::new();
        let mem_tracker = MemTracker(Arc::new(AtomicU64::new(0)));
        AllocationRegistry::set_global_tracker(mem_tracker.clone())
            .expect("no other global tracker should be set yet");
        AllocationRegistry::enable_tracking();

        HostTracker {
            cpu_insn_counter,
            mem_tracker,
            start_time: Instant::now(),
            alloc_guard: None,
            #[cfg(feature = "tracy")]
            tracy_span: None,
        }
    }

    pub fn start(&mut self, token: Option<&'a mut AllocationGroupToken>) {
        // start the mem measurement
        #[cfg(feature = "tracy")]
        {
            self.tracy_span = Some(tracy_span!("tracker active"));
        }
        self.mem_tracker.0.store(0, Ordering::SeqCst);
        self.alloc_guard = if let Some(t) = token {
            Some(t.enter())
        } else {
            None
        };

        // start the cpu measurement
        self.start_time = Instant::now();
        self.cpu_insn_counter.begin();
    }

    pub fn stop(mut self) -> (u64, u64, u64) {
        // collect the metrics
        let cpu_insns = self.cpu_insn_counter.end_and_count();
        let stop_time = Instant::now();
        if let Some(g) = self.alloc_guard {
            drop(g)
        }
        let mem_bytes = self.mem_tracker.0.load(Ordering::SeqCst);
        let time_nsecs = stop_time.duration_since(self.start_time).as_nanos() as u64;
        self.alloc_guard = None;
        #[cfg(feature = "tracy")]
        {
            self.tracy_span = None
        }
        AllocationRegistry::disable_tracking();
        unsafe {
            AllocationRegistry::clear_global_tracker();
        }

        (cpu_insns, mem_bytes, time_nsecs)
    }
}
