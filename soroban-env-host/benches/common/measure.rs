use rand::{rngs::StdRng, Rng, SeedableRng};
use soroban_env_host::{budget::AsBudget, cost_runner::CostRunner, Host};
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

use super::{fit_model, FPCostModel};

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

#[derive(Clone, Debug, Default)]
pub struct Measurement {
    pub iterations: u64,
    pub inputs: Option<u64>,
    pub cpu_insns: u64,
    pub mem_bytes: u64,
    pub time_nsecs: u64,
}

#[derive(Clone, Debug, Default)]
pub struct Measurements {
    pub baseline: Measurement,
    pub measurements: Vec<Measurement>,
    pub averaged_net_measurements: Vec<Measurement>,
}

impl Measurements {
    // This is the preprocess step to convert raw measurements into `averaged_net_measurements`,
    // ready to be fitted by the linear model.
    // We start from `N_r * ( N_x * (a + b * Option<x>) + Overhead_b)`, first substracts baseline
    // => `N_r * N_x * (a + b * Option<x>)`, then divides it by the iterations `(N_r * N_x)` =>
    // returns `y = a + b * Option<x>` ready to be fitted.
    pub fn preprocess(&mut self) {
        self.averaged_net_measurements = self
            .measurements
            .iter()
            .map(|m| {
                let mut e = m.clone();
                e.inputs = e.inputs.map(|i| i / e.iterations);
                e.cpu_insns = e.cpu_insns.saturating_sub(self.baseline.cpu_insns) / e.iterations;
                e.mem_bytes = e.mem_bytes.saturating_sub(self.baseline.mem_bytes) / e.iterations;
                e.time_nsecs = e.time_nsecs.saturating_sub(self.baseline.time_nsecs) / e.iterations;
                e
            })
            .collect()
    }

    pub fn report_histogram<F>(&self, out_name: &str, get_output: F)
    where
        F: Fn(&Measurement) -> u64,
    {
        use thousands::Separable;
        let points: Vec<(f32, f32)> = self
            .measurements
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
            .measurements
            .iter()
            .map(|m| m.inputs.unwrap_or(0) as f32)
            .reduce(f32::min)
            .unwrap();
        let in_max = self
            .measurements
            .iter()
            .map(|m| m.inputs.unwrap_or(0) as f32)
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
        // data must be preprocessed
        assert_eq!(
            self.measurements.len(),
            self.averaged_net_measurements.len()
        );

        use std::io::Write;
        use thousands::Separable;
        let mut tw = TabWriter::new(vec![])
            .padding(5)
            .alignment(Alignment::Right);

        writeln!(&mut tw, "iterations\tinputs\tcpu_insns_total\tcpu_insns_base\tmem_bytes_total\tmem_bytes_base\ttime_ns_total\ttime_ns_base\tave_net_input\tave_net_insns\tave_net_bytes\tave_net_ns\tave_net_insn_per_input\tave_net_bytes_per_input").unwrap();
        for (mes, net) in self
            .measurements
            .iter()
            .zip(self.averaged_net_measurements.iter())
        {
            writeln!(
                &mut tw,
                "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
                mes.iterations.separate_with_commas(),
                mes.inputs.unwrap_or(0).separate_with_commas(),
                mes.cpu_insns.separate_with_commas(),
                self.baseline.cpu_insns.separate_with_commas(),
                mes.mem_bytes.separate_with_commas(),
                self.baseline.mem_bytes.separate_with_commas(),
                mes.time_nsecs.separate_with_commas(),
                self.baseline.time_nsecs.separate_with_commas(),
                net.inputs.unwrap_or(0).separate_with_commas(),
                net.cpu_insns.separate_with_commas(),
                net.mem_bytes.separate_with_commas(),
                net.time_nsecs.separate_with_commas(),
                (net.cpu_insns / net.inputs.unwrap_or(0).max(1)).separate_with_commas(),
                (net.mem_bytes / net.inputs.unwrap_or(0).max(1)).separate_with_commas(),
            )
            .unwrap();
        }
        tw.flush().unwrap();
        eprintln!("{}", String::from_utf8(tw.into_inner().unwrap()).unwrap());
    }

    pub fn fit_model_to_cpu(&self) -> FPCostModel {
        // data must be preprocessed
        assert_eq!(
            self.measurements.len(),
            self.averaged_net_measurements.len()
        );

        let (x, y): (Vec<_>, Vec<_>) = self
            .averaged_net_measurements
            .iter()
            .skip(1) // for some reason first data point is often flakey, skip it
            .map(|m| (m.inputs.unwrap_or(0), m.cpu_insns))
            .unzip();

        fit_model(x, y)
    }

    pub fn fit_model_to_mem(&self) -> FPCostModel {
        // data must be preprocessed
        assert_eq!(
            self.measurements.len(),
            self.averaged_net_measurements.len()
        );

        let (x, y): (Vec<_>, Vec<_>) = self
            .averaged_net_measurements
            .iter()
            .skip(1) // for some reason first data point is often flakey, skip it
            .map(|m| (m.inputs.unwrap_or(0), m.mem_bytes))
            .unzip();

        fit_model(x, y)
    }
}

/// HostCostMeasurement (HCM) is an interface to measuring the memory and CPU
/// costs of a CostType: usually a block of WASM bytecode or a host function.
/// All types of costs are modeled by the linear function (see [`CostModel`]):
///
///     f(x) = N_x * (a + b * Option<x>)                                    [1]
///
/// The `N_x` here is batch size if the host is doing `batched_charge` for the
/// corresponding `x`.  The goal of the HCM is to record the relation of x and
/// f(x) in order for  a, b -- the constant and linear cost parameters -- to be
/// extracted. In the ideal setup, we pass in an array of samples with various
/// inputs {x}, The host do the exact amount of work designed for the CostType
/// on the samples and we record the work done {f(x)}, and pass {x, f(x)} into
/// a model fitter to extract a and b.
///
/// The actual measurement setup, due to its limitations, is actually measuring
///
///     g(x) = N_r * (f(x) + Overhead_b + Overhead_s)                       [2]
///
/// where f(x) is the target cost we want to measure, `N_r` is the number of
/// iterations each sample run is repeated (to average out the measurement
/// noise) i.e. `CostRunner::RUN_ITERATIONS`. `Overhead_b` is the overhead cost
/// due to the benchmark setup and `Overhead_s` is the overhead cost due to
/// each sample setup. Both `Overhead_b` and `Overhead_s` are unavoidable (due
/// to the bench setup), and cannot be averaged out since they scale with run
/// iterations. Therefore in addition to the `run` function (which records
/// `g(x)`), HCM also requires a `run_baseline` function which measures
/// `N_r * Overhead_b` and `get_insns_overhead_per_sample` which gets the
/// `Overhead_s` info from a sample. In the end both of these overheads are
/// subtracted away and we get `N * (a + b * Option<x>)` where `N == N_r * N_x`
/// is the number of `iterations` reported by the host for the `CostModel`.
pub trait HostCostMeasurement: Sized {
    /// The type of host runner we're using. Uniquely identifies a `CostType`.
    type Runner: CostRunner;

    /// Step size of the measurement to scale the input by. For a short-running measurement,
    /// setting this constant accumulates multiple runs into a single sample point, thus helps
    /// distingushing the trend from measurement noise.
    const STEP_SIZE: u64 = 1000;

    /// Initialize a new instance of a HostMeasurement at a given input _hint_, for
    /// the run; the HostMeasurement can choose a precise input for a given hint
    /// and use it during `run`; the precise input will be extracted at the end
    /// using `get_input`.
    ///
    /// All setup should happen in the `new` function here; `run` should be
    /// reserved for the "actual event" you're trying to measure.
    fn new_random_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType;

    /// Initialize a best-case (cheapest) instance. Defaults to random with input=1.
    fn new_best_case(host: &Host, rng: &mut StdRng) -> <Self::Runner as CostRunner>::SampleType {
        Self::new_random_case(host, rng, 1)
    }

    /// Baseline case is absolute minimal cost instance. It is used for estimating for the
    /// "background noise" of the measurement. The best-case is the cheapest but valid instance.
    /// The baseline-case may not be an actual valid case.
    fn new_baseline_case(
        host: &Host,
        rng: &mut StdRng,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Self::new_random_case(host, rng, 0)
    }

    /// Initialize a worst-case (most-expensive) instance at a given size. Defaults to random.
    fn new_worst_case(
        host: &Host,
        rng: &mut StdRng,
        input: u64,
    ) -> <Self::Runner as CostRunner>::SampleType {
        Self::new_random_case(host, rng, input)
    }

    fn run_baseline(
        host: &Host,
        samples: Vec<<Self::Runner as CostRunner>::SampleType>,
        recycled_samples: &mut Vec<<Self::Runner as CostRunner>::RecycledType>,
    ) {
        <Self::Runner as CostRunner>::run_baseline(host, samples, recycled_samples)
    }

    fn run(
        host: &Host,
        samples: Vec<<Self::Runner as CostRunner>::SampleType>,
        recycled_samples: &mut Vec<<Self::Runner as CostRunner>::RecycledType>,
    ) {
        <Self::Runner as CostRunner>::run(host, samples, recycled_samples)
    }

    fn get_tracker(host: &Host) -> (u64, Option<u64>) {
        <Self::Runner as CostRunner>::get_tracker(host)
    }

    // This is kind of a hack to account for the additional cpu_insn overhead
    // that are both significant (scales with the input) and not accounted for
    // by the `run_baseline`. So far the only use case for this is for
    // calibrating individual wasm instructions, where the target wasm instruction
    // needs to be accompanied by some boilerplate. For example to measure the
    // cpu_insns of `drop`, we need to do N * (`push`+`drop`), the `push` scales with
    // N and cannot accounted for by the baseline.
    // This part is the `Overhead_s` part of equation [2]. Defaults to 0.
    fn get_insns_overhead_per_sample(
        _host: &Host,
        _sample: &<Self::Runner as CostRunner>::SampleType,
    ) -> u64 {
        0
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

fn harness<HCM: HostCostMeasurement, R>(
    host: &Host,
    mem_tracker: &MemTracker,
    cpu_insn_counter: &mut cpu::InstructionCounter,
    alloc_group_token: &mut AllocationGroupToken,
    runner: &mut R,
    samples: Vec<<<HCM as HostCostMeasurement>::Runner as CostRunner>::SampleType>,
) -> Measurement
where
    R: FnMut(
        &Host,
        Vec<<<HCM as HostCostMeasurement>::Runner as CostRunner>::SampleType>,
        &mut Vec<<<HCM as HostCostMeasurement>::Runner as CostRunner>::RecycledType>,
    ),
{
    let mut recycled_samples = Vec::with_capacity(samples.len());
    host.as_budget().reset_unlimited();

    // start the cpu and mem measurement
    mem_tracker.0.store(0, Ordering::SeqCst);
    let alloc_guard = alloc_group_token.enter();
    let start = Instant::now();
    cpu_insn_counter.begin();
    runner(host, samples, &mut recycled_samples);
    // collect the metrics
    let cpu_insns = cpu_insn_counter.end_and_count();
    let stop = Instant::now();
    drop(alloc_guard);
    // Note: the `iterations` here is not same as `RUN_ITERATIONS`. This is the `N` part of the
    // cost model, which is `RUN_ITERATIONS` * "model iterations from the sample"
    let (iterations, inputs) = HCM::get_tracker(&host);
    let mem_bytes = mem_tracker.0.load(Ordering::SeqCst);
    let time_nsecs = stop.duration_since(start).as_nanos() as u64;
    Measurement {
        iterations,
        inputs,
        cpu_insns,
        mem_bytes,
        time_nsecs,
    }
}

fn measure_costs_inner<HCM: HostCostMeasurement, F, R>(
    mut next_sample: F,
    mut runner: R,
) -> Result<Vec<Measurement>, std::io::Error>
where
    F: FnMut(&Host) -> Option<<HCM::Runner as CostRunner>::SampleType>,
    R: FnMut(
        &Host,
        Vec<<HCM::Runner as CostRunner>::SampleType>,
        &mut Vec<<HCM::Runner as CostRunner>::RecycledType>,
    ),
{
    let mut cpu_insn_counter = cpu::InstructionCounter::new();
    let mem_tracker = MemTracker(Arc::new(AtomicU64::new(0)));
    AllocationRegistry::set_global_tracker(mem_tracker.clone())
        .expect("no other global tracker should be set yet");
    AllocationRegistry::enable_tracking();
    let mut alloc_group_token =
        AllocationGroupToken::register().expect("failed to register allocation group");
    let mut ret = Vec::new();
    eprintln!(
        "\nMeasuring costs for CostType::{:?}\n",
        <HCM::Runner as CostRunner>::COST_TYPE
    );
    loop {
        // prepare the measurement
        let host = Host::default();
        host.as_budget().reset_unlimited(); // it is possible to exceed the default limits
        let sample = match next_sample(&host) {
            Some(s) => s,
            None => break,
        };
        let samples = (0..<HCM::Runner as CostRunner>::RUN_ITERATIONS)
            .map(|_| sample.clone())
            .collect();
        // This part is the `N_r * Overhead_s` part of equation [2].
        // This is 0 unless we are doing wasm-insn level calibration
        let samples_cpu_insns_overhead = <HCM::Runner as CostRunner>::RUN_ITERATIONS
            .saturating_mul(HCM::get_insns_overhead_per_sample(&host, &sample));

        let mut mes = harness::<HCM, _>(
            &host,
            &mem_tracker,
            &mut cpu_insn_counter,
            &mut alloc_group_token,
            &mut runner,
            samples,
        );
        mes.cpu_insns -= samples_cpu_insns_overhead;
        // the return result contains `N_r * (f(x) + Overhead_b)` (see equation [2])
        ret.push(mes);
    }
    AllocationRegistry::disable_tracking();
    unsafe {
        AllocationRegistry::clear_global_tracker();
    }
    Ok(ret)
}

// Takes each value in the input range, creates a worst-case sample with that input,
// and run the measurement for a number of iterations (defined by the runner).
// Also runs a single baseline sample for the same number of iterations. Return both
// the measurements and the baseline.
pub fn measure_worst_case_costs<HCM: HostCostMeasurement>(
    mut input_range: Range<u64>,
) -> Result<Measurements, std::io::Error> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    // run baseline measure
    let mut base_range = std::iter::once(0);
    let baseline = measure_costs_inner::<HCM, _, _>(
        |host| {
            base_range
                .next()
                .map(|_| HCM::new_baseline_case(host, &mut rng))
        },
        &HCM::run_baseline,
    )?
    .first()
    .expect("baseline is empty")
    .clone();
    // run the actual measurements
    let measurements = measure_costs_inner::<HCM, _, _>(
        |host| {
            input_range
                .next()
                .map(|input| HCM::new_worst_case(host, &mut rng, input))
        },
        HCM::run,
    )?;
    Ok(Measurements {
        baseline,
        measurements,
        averaged_net_measurements: Default::default(),
    })
}

pub fn measure_cost_variation<HCM: HostCostMeasurement>(
    large_input: u64,
) -> Result<Measurements, std::io::Error> {
    let count = 100;
    let mut i = 0;
    let mut rng = StdRng::from_seed([0xff; 32]);

    // run baseline measure
    let baseline = measure_costs_inner::<HCM, _, _>(
        |host| {
            std::iter::once(0)
                .next()
                .map(|_| HCM::new_baseline_case(host, &mut rng))
        },
        &HCM::run_baseline,
    )?
    .first()
    .expect("baseline is empty")
    .clone();

    // run the actual measurements
    let measurements = measure_costs_inner::<HCM, _, _>(
        |host| {
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
        },
        HCM::run,
    )?;
    Ok(Measurements {
        baseline,
        measurements,
        averaged_net_measurements: Default::default(),
    })
}
