use crate::{
    cost_runner::{CostRunner, CostType},
    vm::ParsedModule,
    xdr::{ContractCostType::VmInstantiation, Hash},
    Vm,
};
use std::{hint::black_box, rc::Rc};

pub struct VmInstantiationRun;

#[derive(Clone)]
pub struct VmInstantiationSample {
    pub id: Option<Hash>,
    pub wasm: Vec<u8>,
    pub module: Rc<ParsedModule>,
}

macro_rules! impl_costrunner_for_instantiation_cost_type {
    ($RUNNER:ty, $COST:ident) => {
        impl CostRunner for $RUNNER {
            const COST_TYPE: CostType = CostType::Contract($COST);

            const RUN_ITERATIONS: u64 = 10;

            type SampleType = VmInstantiationSample;

            type RecycledType = (Option<Rc<Vm>>, Vec<u8>);

            fn run_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                #[cfg(feature = "next")]
                let vm = black_box(
                    Vm::from_parsed_module(host, sample.id.unwrap(), sample.module).unwrap(),
                );
                #[cfg(not(feature = "next"))]
                let vm = black_box(
                    Vm::new_with_cost_inputs(
                        host,
                        sample.id.unwrap(),
                        &sample.wasm[..],
                        sample.module.cost_inputs,
                    )
                    .unwrap(),
                );
                (Some(vm), sample.wasm)
            }

            fn run_baseline_iter(
                host: &crate::Host,
                _iter: u64,
                sample: Self::SampleType,
            ) -> Self::RecycledType {
                black_box(host.charge_budget($COST, Some(0)).unwrap());
                black_box((None, sample.wasm))
            }
        }
    };
}

// Protocol 20 coarse cost model
impl_costrunner_for_instantiation_cost_type!(VmInstantiationRun, VmInstantiation);

// Protocol 21 refined cost model.
#[cfg(feature = "next")]
pub use v21::*;
#[cfg(feature = "next")]
mod v21 {
    use super::*;
    use crate::vm::ParsedModule;
    use crate::xdr::ContractCostType::{
        InstantiateWasmDataSegments, InstantiateWasmElemSegments, InstantiateWasmExports,
        InstantiateWasmFunctions, InstantiateWasmGlobals, InstantiateWasmImports,
        InstantiateWasmInstructions, InstantiateWasmMemoryPages, InstantiateWasmTableEntries,
        InstantiateWasmTypes, ParseWasmDataSegments, ParseWasmElemSegments, ParseWasmExports,
        ParseWasmFunctions, ParseWasmGlobals, ParseWasmImports, ParseWasmInstructions,
        ParseWasmMemoryPages, ParseWasmTableEntries, ParseWasmTypes,
    };

    macro_rules! impl_costrunner_for_parse_cost_type {
        ($RUNNER:ty, $COST:ident) => {
            impl CostRunner for $RUNNER {
                const COST_TYPE: CostType = CostType::Contract($COST);

                const RUN_ITERATIONS: u64 = 10;

                type SampleType = VmInstantiationSample;

                type RecycledType = (Option<Rc<ParsedModule>>, Vec<u8>);

                fn run_iter(
                    host: &crate::Host,
                    _iter: u64,
                    sample: Self::SampleType,
                ) -> Self::RecycledType {
                    let module = black_box(Rc::new(
                        ParsedModule::new(
                            host,
                            sample.module.module.engine(),
                            &sample.wasm[..],
                            sample.module.cost_inputs.clone(),
                        )
                        .unwrap(),
                    ));
                    (Some(module), sample.wasm)
                }

                fn run_baseline_iter(
                    host: &crate::Host,
                    _iter: u64,
                    sample: Self::SampleType,
                ) -> Self::RecycledType {
                    black_box(host.charge_budget($COST, Some(0)).unwrap());
                    black_box((None, sample.wasm))
                }
            }
        };
    }

    pub struct ParseWasmInstructionsRun;
    pub struct ParseWasmFunctionsRun;
    pub struct ParseWasmGlobalsRun;
    pub struct ParseWasmTableEntriesRun;
    pub struct ParseWasmTypesRun;
    pub struct ParseWasmDataSegmentsRun;
    pub struct ParseWasmElemSegmentsRun;
    pub struct ParseWasmImportsRun;
    pub struct ParseWasmExportsRun;
    pub struct ParseWasmMemoryPagesRun;

    pub struct InstantiateWasmInstructionsRun;
    pub struct InstantiateWasmFunctionsRun;
    pub struct InstantiateWasmGlobalsRun;
    pub struct InstantiateWasmTableEntriesRun;
    pub struct InstantiateWasmTypesRun;
    pub struct InstantiateWasmDataSegmentsRun;
    pub struct InstantiateWasmElemSegmentsRun;
    pub struct InstantiateWasmImportsRun;
    pub struct InstantiateWasmExportsRun;
    pub struct InstantiateWasmMemoryPagesRun;

    impl_costrunner_for_parse_cost_type!(ParseWasmInstructionsRun, ParseWasmInstructions);
    impl_costrunner_for_parse_cost_type!(ParseWasmFunctionsRun, ParseWasmFunctions);
    impl_costrunner_for_parse_cost_type!(ParseWasmGlobalsRun, ParseWasmGlobals);
    impl_costrunner_for_parse_cost_type!(ParseWasmTableEntriesRun, ParseWasmTableEntries);
    impl_costrunner_for_parse_cost_type!(ParseWasmTypesRun, ParseWasmTypes);
    impl_costrunner_for_parse_cost_type!(ParseWasmDataSegmentsRun, ParseWasmDataSegments);
    impl_costrunner_for_parse_cost_type!(ParseWasmElemSegmentsRun, ParseWasmElemSegments);
    impl_costrunner_for_parse_cost_type!(ParseWasmImportsRun, ParseWasmImports);
    impl_costrunner_for_parse_cost_type!(ParseWasmExportsRun, ParseWasmExports);
    impl_costrunner_for_parse_cost_type!(ParseWasmMemoryPagesRun, ParseWasmMemoryPages);

    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmInstructionsRun,
        InstantiateWasmInstructions
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmFunctionsRun,
        InstantiateWasmFunctions
    );
    impl_costrunner_for_instantiation_cost_type!(InstantiateWasmGlobalsRun, InstantiateWasmGlobals);
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmTableEntriesRun,
        InstantiateWasmTableEntries
    );
    impl_costrunner_for_instantiation_cost_type!(InstantiateWasmTypesRun, InstantiateWasmTypes);
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmDataSegmentsRun,
        InstantiateWasmDataSegments
    );
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmElemSegmentsRun,
        InstantiateWasmElemSegments
    );
    impl_costrunner_for_instantiation_cost_type!(InstantiateWasmImportsRun, InstantiateWasmImports);
    impl_costrunner_for_instantiation_cost_type!(InstantiateWasmExportsRun, InstantiateWasmExports);
    impl_costrunner_for_instantiation_cost_type!(
        InstantiateWasmMemoryPagesRun,
        InstantiateWasmMemoryPages
    );
}
