#[allow(unused)]
use super::wasm_insn_exec::{
    wasm_module_with_n_data_segments, wasm_module_with_n_elem_segments, wasm_module_with_n_exports,
    wasm_module_with_n_globals, wasm_module_with_n_imports, wasm_module_with_n_insns,
    wasm_module_with_n_internal_funcs, wasm_module_with_n_table_entries, wasm_module_with_n_types,
};
use crate::common::{util, HostCostMeasurement};
use rand::{rngs::StdRng, Rng};
use soroban_env_host::{
    cost_runner::{
        VmInstantiationDataSegmentsRun, VmInstantiationElemSegmentsRun, VmInstantiationExportsRun,
        VmInstantiationFunctionsRun, VmInstantiationGlobalsRun, VmInstantiationImportsRun,
        VmInstantiationInstructionsRun, VmInstantiationRun, VmInstantiationSample,
        VmInstantiationTableEntriesRun, VmInstantiationTypesRun,
    },
    xdr, Host, Vm,
};

pub(crate) struct VmInstantiationMeasure;
pub(crate) struct VmInstantiationInstructionsMeasure;
pub(crate) struct VmInstantiationFunctionsMeasure;
pub(crate) struct VmInstantiationGlobalsMeasure;
pub(crate) struct VmInstantiationTableEntriesMeasure;
pub(crate) struct VmInstantiationTypesMeasure;
pub(crate) struct VmInstantiationDataSegmentsMeasure;
pub(crate) struct VmInstantiationElemSegmentsMeasure;
pub(crate) struct VmInstantiationImportsMeasure;
pub(crate) struct VmInstantiationExportsMeasure;

// This measures the cost of instantiating a host::Vm on a variety of possible
// wasm modules, of different sizes. The input value should be the size of the
// module, though for now we're just selecting modules from the fixed example
// repertoire. Costs should be linear.
macro_rules! impl_measurement_for_instantiation_cost_type {
    ($RUNNER:ty, $MEASURE:ty, $BUILD:ident, $HAS_INPUTS:expr, $MAGNITUDE:expr) => {
        impl HostCostMeasurement for $MEASURE {
            type Runner = $RUNNER;

            fn new_best_case(_host: &Host, _rng: &mut StdRng) -> VmInstantiationSample {
                let id: xdr::Hash = [0; 32].into();
                let wasm: Vec<u8> = soroban_test_wasms::ADD_I32.into();
                VmInstantiationSample {
                    id: Some(id),
                    wasm,
                    cost_inputs: None,
                }
            }

            fn new_worst_case(host: &Host, _rng: &mut StdRng, input: u64) -> VmInstantiationSample {
                let id: xdr::Hash = [0; 32].into();
                let n = (Self::INPUT_BASE_SIZE + input * $MAGNITUDE) as usize;
                let wasm = $BUILD(n);
                let cost_inputs = if $HAS_INPUTS {
                    let vm = Vm::new(host, id.clone(), &wasm[..], None).unwrap();
                    Some(vm.get_contract_code_cost_inputs())
                } else {
                    None
                };
                VmInstantiationSample {
                    id: Some(id),
                    wasm,
                    cost_inputs,
                }
            }

            fn new_random_case(
                host: &Host,
                rng: &mut StdRng,
                _input: u64,
            ) -> VmInstantiationSample {
                let id: xdr::Hash = [0; 32].into();
                let idx = rng.gen_range(0..=10) % util::TEST_WASMS.len();
                let wasm: Vec<u8> = util::TEST_WASMS[idx].into();
                let cost_inputs = if $HAS_INPUTS {
                    let vm = Vm::new(host, id.clone(), &wasm[..], None).unwrap();
                    Some(vm.get_contract_code_cost_inputs())
                } else {
                    None
                };
                VmInstantiationSample {
                    id: Some(id),
                    wasm,
                    cost_inputs,
                }
            }
        }
    };
}

impl_measurement_for_instantiation_cost_type!(
    VmInstantiationRun,
    VmInstantiationMeasure,
    wasm_module_with_n_internal_funcs,
    false,
    30
);

impl_measurement_for_instantiation_cost_type!(
    VmInstantiationInstructionsRun,
    VmInstantiationInstructionsMeasure,
    wasm_module_with_n_insns,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationFunctionsRun,
    VmInstantiationFunctionsMeasure,
    wasm_module_with_n_internal_funcs,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationGlobalsRun,
    VmInstantiationGlobalsMeasure,
    wasm_module_with_n_globals,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationTableEntriesRun,
    VmInstantiationTableEntriesMeasure,
    wasm_module_with_n_table_entries,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationTypesRun,
    VmInstantiationTypesMeasure,
    wasm_module_with_n_types,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationDataSegmentsRun,
    VmInstantiationDataSegmentsMeasure,
    wasm_module_with_n_data_segments,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationElemSegmentsRun,
    VmInstantiationElemSegmentsMeasure,
    wasm_module_with_n_elem_segments,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationImportsRun,
    VmInstantiationImportsMeasure,
    wasm_module_with_n_imports,
    true,
    30
);
impl_measurement_for_instantiation_cost_type!(
    VmInstantiationExportsRun,
    VmInstantiationExportsMeasure,
    wasm_module_with_n_exports,
    true,
    30
);
