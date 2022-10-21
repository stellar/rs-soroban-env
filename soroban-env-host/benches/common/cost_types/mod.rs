mod bigint_div_rem;
mod compute_ed25519_pubkey;
mod compute_sha256_hash;
mod ed25519_scalar_mul;
mod host_obj_alloc_slot;
mod im_map_immut_entry;
mod im_map_mut_entry;
mod im_vec_immut_entry;
mod im_vec_mut_entry;
mod im_vec_new;
mod scmap_to_host_map;
mod scvec_to_host_vec;
mod verify_ed25519_sig;
mod vm_instantiation;
mod wasm_insn_exec;

mod util;

use std::collections::BTreeSet;

use soroban_env_host::budget::CostType;

use crate::common::HostCostMeasurement;

pub trait Benchmark {
    fn bench<HCM: HostCostMeasurement>() -> std::io::Result<()>;
}

fn get_explicit_bench_names() -> Option<Vec<String>> {
    let bare_args: Vec<_> = std::env::args().filter(|x| !x.starts_with("-")).collect();
    match bare_args.len() {
        0 | 1 => None,
        _ => Some(bare_args[1..].into()),
    }
}

fn should_run<HCM: HostCostMeasurement>() -> bool {
    if let Some(bench_names) = get_explicit_bench_names() {
        let name = format!("{:?}", HCM::COST_TYPE)
            .split("::")
            .last()
            .unwrap()
            .to_string();
        bench_names.into_iter().find(|arg| *arg == name).is_some()
    } else {
        true
    }
}

fn call_bench<B: Benchmark, HCM: HostCostMeasurement>(
    costs: &mut BTreeSet<CostType>,
) -> std::io::Result<()> {
    if should_run::<HCM>() {
        B::bench::<HCM>()?;
        costs.insert(HCM::COST_TYPE);
    }
    Ok(())
}

pub fn for_each_host_cost_measurement<B: Benchmark>() -> std::io::Result<()> {
    let mut costs: BTreeSet<CostType> = BTreeSet::new();

    call_bench::<B, bigint_div_rem::BigIntDivRemRun>(&mut costs)?;
    call_bench::<B, compute_ed25519_pubkey::ComputeEd25519PubKeyRun>(&mut costs)?;
    call_bench::<B, compute_sha256_hash::ComputeSha256HashRun>(&mut costs)?;
    call_bench::<B, ed25519_scalar_mul::EdwardsPointCurve25519ScalarMulRun>(&mut costs)?;
    call_bench::<B, host_obj_alloc_slot::HostObjAllocSlotRun>(&mut costs)?;
    call_bench::<B, im_map_immut_entry::ImMapImmutEntryRun>(&mut costs)?;
    call_bench::<B, im_map_mut_entry::ImMapMutEntryRun>(&mut costs)?;
    call_bench::<B, im_vec_immut_entry::ImVecImmutEntryRun>(&mut costs)?;
    call_bench::<B, im_vec_mut_entry::ImVecMutEntryRun>(&mut costs)?;
    call_bench::<B, im_vec_new::ImVecNewRun>(&mut costs)?;
    call_bench::<B, scmap_to_host_map::ScMapToHostMapRun>(&mut costs)?;
    call_bench::<B, scvec_to_host_vec::ScVecToHostVecRun>(&mut costs)?;
    call_bench::<B, verify_ed25519_sig::VerifyEd25519SigRun>(&mut costs)?;
    call_bench::<B, vm_instantiation::VmInstantiationRun>(&mut costs)?;
    call_bench::<B, wasm_insn_exec::WasmInsnExecRun>(&mut costs)?;

    if get_explicit_bench_names().is_none() {
        for cost in CostType::variants() {
            if !costs.contains(cost) {
                eprintln!("warning: missing cost measurement for {:?}", cost);
            }
        }
    }
    Ok(())
}
