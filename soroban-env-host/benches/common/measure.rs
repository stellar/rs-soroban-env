use rand::{rngs::StdRng, Rng, SeedableRng};
use soroban_env_host::{budget::CostType, Host};
use std::{
    alloc::System,
    ops::Range,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
    time::Instant,
};
use tabwriter::{Alignment, TabWriter};
use tracking_allocator::{AllocationGroupToken, AllocationRegistry, AllocationTracker, Allocator};

use super::{fit_model, FPCostModel, FPPoint};

#[global_allocator]
static GLOBAL: Allocator<System> = Allocator::system();

#[derive(Clone)]
pub struct MemTracker(Arc<AtomicU64>);
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

    fn deallocated(
        &self,
        _addr: usize,
        _object_input: usize,
        wrapped_input: usize,
        _source_group_id: tracking_allocator::AllocationGroupId,
        _current_group_id: tracking_allocator::AllocationGroupId,
    ) {
        self.0.fetch_sub(wrapped_input as u64, Ordering::SeqCst);
    }
}

#[derive(Clone, Debug, Default)]
pub struct Measurement {
    pub input: u64,
    pub cpu_insns: u64,
    pub mem_bytes: u64,
    pub time_nsecs: u64,
}

#[derive(Clone, Debug, Default)]
pub struct Measurements(pub Vec<Measurement>);
impl Measurements {
    /// Subtracts the values of the first measurement from all subsequent
    /// measurements, to eliminate constant factors; this should only be done if
    /// you're sure you don't want to capture those constant factors in a later
    /// cost model.
    pub fn subtract_baseline(&mut self) {
        match self.0.split_first_mut() {
            None => (),
            Some((first, rest)) => {
                for e in rest {
                    e.input = e.input.saturating_sub(first.input);
                    e.cpu_insns = e.cpu_insns.saturating_sub(first.cpu_insns);
                    e.mem_bytes = e.mem_bytes.saturating_sub(first.mem_bytes);
                    e.time_nsecs = e.time_nsecs.saturating_sub(first.time_nsecs);
                }
                *first = Measurement::default();
            }
        }
    }

    pub fn report_histogram<F>(&self, out_name: &str, get_output: F)
    where
        F: Fn(&Measurement) -> u64,
    {
        use thousands::Separable;
        let points: Vec<(f32, f32)> = self
            .0
            .iter()
            .enumerate()
            .map(|(i, m)| (i as f32, get_output(m) as f32))
            .collect();
        let ymin = points.iter().map(|(_, y)| *y).reduce(f32::min).unwrap();
        let ymax = points.iter().map(|(_, y)| *y).reduce(f32::max).unwrap();

        if ymin == ymax {
            return;
        }
        let hist = textplots::utils::histogram(&points, ymin, ymax, 30);

        let in_min = self
            .0
            .iter()
            .map(|m| m.input as f32)
            .reduce(f32::min)
            .unwrap();
        let in_max = self
            .0
            .iter()
            .map(|m| m.input as f32)
            .reduce(f32::max)
            .unwrap();

        use textplots::{Chart, Plot, Shape};
        println!(
            "cost input: min {}; max {}; max/min = {}",
            in_min,
            in_max,
            in_max / in_min.max(1.0)
        );
        println!(
            "{} output: min {}; max {}; max/min = {}",
            out_name,
            ymin.separate_with_commas(),
            ymax.separate_with_commas(),
            ymax / ymin.max(1.0)
        );
        Chart::new(180, 60, ymin - 100.0, ymax + 100.0)
            .lineplot(&Shape::Bars(&hist))
            .nice();
    }

    pub fn report_table(&self) {
        use std::io::Write;
        use thousands::Separable;
        let mut tw = TabWriter::new(vec![])
            .padding(5)
            .alignment(Alignment::Right);

        writeln!(
            &mut tw,
            "input\tcpu insns\tmem bytes\ttime nsecs\tinsns/input\tbytes/input\tnsecs/input"
        )
        .unwrap();
        for m in self.0.iter() {
            writeln!(
                &mut tw,
                "{}\t{}\t{}\t{}ns\t{}\t{}\t{}ns",
                m.input.separate_with_commas(),
                m.cpu_insns.separate_with_commas(),
                m.mem_bytes.separate_with_commas(),
                m.time_nsecs.separate_with_commas(),
                (m.cpu_insns / m.input.max(1)).separate_with_commas(),
                (m.mem_bytes / m.input.max(1)).separate_with_commas(),
                (m.time_nsecs / m.input.max(1)).separate_with_commas()
            )
            .unwrap();
        }
        tw.flush().unwrap();
        eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());
    }

    pub fn fit_model_to_cpu(&self) -> FPCostModel {
        let data = self
            .0
            .iter()
            .map(|m| FPPoint {
                x: m.input as f64,
                y: m.cpu_insns as f64,
            })
            .collect();
        fit_model(&data)
    }

    pub fn fit_model_to_mem(&self) -> FPCostModel {
        let data = self
            .0
            .iter()
            .map(|m| FPPoint {
                x: m.input as f64,
                y: m.mem_bytes as f64,
            })
            .collect();
        fit_model(&data)
    }
}

/// HostCostMeasurement is an interface to measuring the memory and CPU
/// costs of a CostType: usually a block of WASM bytecode or a host function.
pub trait HostCostMeasurement: Sized {
    /// The type of cost we're measuring.
    const COST_TYPE: CostType;

    /// Number of times the harness calls `run`, used to divide the resulting
    /// measured values. Defaults to 1 but if you find your measurements are
    /// finishing too fast and you're getting noise or zero-sized measurements,
    /// set to some larger value and then use in a loop inside `run`.
    const RUN_ITERATIONS: u64 = 1;

    /// Initialize a new instance of a HostMeasurement at a given input _hint_, for
    /// the run; the HostMeasurement can choose a precise input for a given hint
    /// and use it during `run`; the precise input will be extracted at the end
    /// using `get_input`.
    ///
    /// All setup should happen in the `new` function here; `run` should be
    /// reserved for the "actual event" you're trying to measure.
    fn new_random_case(host: &Host, rng: &mut StdRng, input: u64) -> Self;

    /// Initialize a best-case (cheapest) instance. Defaults to random with input=1.
    fn new_best_case(host: &Host, rng: &mut StdRng) -> Self {
        Self::new_random_case(host, rng, 1)
    }

    /// Initialize a worst-case (most-expensive) instance at a given size. Defaults to random.
    fn new_worst_case(host: &Host, rng: &mut StdRng, input: u64) -> Self {
        Self::new_random_case(host, rng, input)
    }

    /// Run the HostMeasurement. This method is called under CPU-and-memory tracking
    /// machinery, so anything that happens during it will be considered part of
    /// the cost for running the HostMeasurement at the returned input. Will be
    /// called with iter set to each number in 0..RUN_ITERATIONS.
    fn run(&mut self, iter: u64, host: &Host);

    /// Return the _actual_ input chosen, rather than the hint passed to `new`.
    /// Defaults to asking the host, which you might need to override if the host
    /// was not actually involved in the measurement.
    fn get_input(&self, host: &Host) -> u64 {
        host.with_budget(|budget| budget.get_input(Self::COST_TYPE))
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
            self.0.read().expect("perf_event::Counter::read")
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

fn measure_costs_inner<HCM: HostCostMeasurement, F>(
    mut next_hcm: F,
) -> Result<Measurements, std::io::Error>
where
    F: FnMut(&Host) -> Option<HCM>,
{
    let mut cpu_insn_counter = cpu::InstructionCounter::new();
    let mem_tracker = MemTracker(Arc::new(AtomicU64::new(0)));
    AllocationRegistry::set_global_tracker(mem_tracker.clone())
        .expect("no other global tracker should be set yet");
    AllocationRegistry::enable_tracking();
    let mut alloc_group_token =
        AllocationGroupToken::register().expect("failed to register allocation group");
    let mut ret = Vec::new();
    eprintln!("\nMeasuring costs for CostType::{:?}\n", HCM::COST_TYPE);
    loop {
        let host = Host::default();
        host.with_budget(|budget| budget.reset_unlimited());
        let mut m = match next_hcm(&host) {
            Some(m) => m,
            None => break,
        };
        let start = Instant::now();
        mem_tracker.0.store(0, Ordering::SeqCst);
        let alloc_guard = alloc_group_token.enter();
        cpu_insn_counter.begin();
        for iter in 0..HCM::RUN_ITERATIONS {
            m.run(iter, &host);
        }
        let cpu_insns = cpu_insn_counter.end_and_count() / HCM::RUN_ITERATIONS;
        drop(alloc_guard);
        let stop = Instant::now();
        let input = m.get_input(&host);
        let mem_bytes = mem_tracker.0.load(Ordering::SeqCst) / HCM::RUN_ITERATIONS;
        let time_nsecs = stop.duration_since(start).as_nanos() as u64 / HCM::RUN_ITERATIONS;
        ret.push(Measurement {
            input,
            cpu_insns,
            mem_bytes,
            time_nsecs,
        })
    }
    AllocationRegistry::disable_tracking();
    unsafe {
        AllocationRegistry::clear_global_tracker();
    }
    Ok(Measurements(ret))
}

pub fn measure_worst_case_costs<HCM: HostCostMeasurement>(
    step_range: Range<u64>,
) -> Result<Measurements, std::io::Error> {
    let mut it = step_range;
    let mut rng = StdRng::from_seed([0xff; 32]);
    measure_costs_inner::<HCM, _>(|host| {
        it.next()
            .map(|input| HCM::new_worst_case(host, &mut rng, input))
    })
}

pub fn measure_cost_variation<HCM: HostCostMeasurement>(
    large_input: u64,
) -> Result<Measurements, std::io::Error> {
    let count = 100;
    let mut i = 0;
    let mut rng = StdRng::from_seed([0xff; 32]);
    measure_costs_inner::<HCM, _>(|host| {
        i += 1;
        match i {
            1 => Some(HCM::new_best_case(host, &mut rng)),
            2 => Some(HCM::new_worst_case(host, &mut rng, large_input)),
            n if n < count => {
                let input = rng.gen_range(1, 2 + large_input);
                Some(HCM::new_random_case(host, &mut rng, input))
            }
            _ => None,
        }
    })
}
