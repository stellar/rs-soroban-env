use std::{
    alloc::System,
    ops::Range,
    sync::{
        atomic::{AtomicU64, Ordering},
        Arc,
    },
    time::Instant,
};
use stellar_contract_env_host::{budget::CostType, Host};
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
        _object_size: usize,
        wrapped_size: usize,
        _group_id: tracking_allocator::AllocationGroupId,
    ) {
        self.0.fetch_add(wrapped_size as u64, Ordering::SeqCst);
    }

    fn deallocated(
        &self,
        _addr: usize,
        _object_size: usize,
        wrapped_size: usize,
        _source_group_id: tracking_allocator::AllocationGroupId,
        _current_group_id: tracking_allocator::AllocationGroupId,
    ) {
        self.0.fetch_sub(wrapped_size as u64, Ordering::SeqCst);
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

    pub fn report(&self) {
        use std::io::Write;
        use thousands::Separable;
        let mut tw = TabWriter::new(vec![])
            .padding(5)
            .alignment(Alignment::Right);

        write!(
            &mut tw,
            "input\tcpu insns\tmem bytes\ttime nsecs\tinsns/input\tbytes/input\tnsecs/input\n"
        )
        .unwrap();
        for m in self.0.iter() {
            write!(
                &mut tw,
                "{}\t{}\t{}\t{}ns\t{}\t{}\t{}ns\n",
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
pub trait HostCostMeasurement {
    /// The type of cost we're measuring.
    const COST_TYPE: CostType;

    /// Initialize a new instance of a HostMeasurement at a given input _hint_, for
    /// the run; the HostMeasurement can choose a precise input for a given hint
    /// and use it during `run`; the precise input will be extracted at the end
    /// using `get_input`.
    ///
    /// All setup should happen in the `new` function here; `run` should be
    /// reserved for the "actual event" you're trying to measure.
    fn new(host: &Host, input_hint: u64) -> Self;

    /// Run the HostMeasurement. This method is called under CPU-and-memory tracking
    /// machinery, so anything that happens during it will be considered part of
    /// the cost for running the HostMeasurement at the returned input.
    fn run(&mut self, host: &Host);

    /// Return the _actual_ input chosen, rather than the hint passed to `new`.
    fn get_input(&self, host: &Host) -> u64 {
        host.get_budget(|budget| budget.get_input(Self::COST_TYPE))
    }
}

pub fn measure_costs<HCM: HostCostMeasurement>(
    step_range: Range<u64>,
) -> Result<Measurements, std::io::Error> {
    let mut cpu_counter = perf_event::Builder::new().build()?;
    let mem_tracker = MemTracker(Arc::new(AtomicU64::new(0)));
    let _ = AllocationRegistry::set_global_tracker(mem_tracker.clone())
        .expect("no other global tracker should be set yet");
    AllocationRegistry::enable_tracking();
    let mut alloc_group_token =
        AllocationGroupToken::register().expect("failed to register allocation group");
    let mut ret = Vec::new();
    eprintln!("\nMeasuring costs for CostType::{:?}\n", HCM::COST_TYPE);
    for input_hint in step_range {
        let host = Host::default();
        host.get_budget_mut(|budget| budget.reset_unlimited());
        let mut m = HCM::new(&host, input_hint);
        let start = Instant::now();
        mem_tracker.0.store(0, Ordering::SeqCst);
        let alloc_guard = alloc_group_token.enter();
        cpu_counter.reset()?;
        cpu_counter.enable()?;
        m.run(&host);
        cpu_counter.disable()?;
        drop(alloc_guard);
        let stop = Instant::now();
        let input = m.get_input(&host);
        let cpu_insns = cpu_counter.read()?;
        let mem_bytes = mem_tracker.0.load(Ordering::SeqCst);
        let time_nsecs = stop.duration_since(start).as_nanos() as u64;
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
