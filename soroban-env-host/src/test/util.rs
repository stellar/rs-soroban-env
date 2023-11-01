use std::{cell::RefCell, collections::BTreeMap, rc::Rc};

use rand::RngCore;
use soroban_env_common::{
    xdr::{
        AccountEntry, AccountId, ContractCostType, LedgerEntry, LedgerEntryData, LedgerKey,
        PublicKey, ScAddress, ScErrorCode, ScErrorType, ScVal, ScVec, Uint256,
    },
    AddressObject, BytesObject, Env, EnvBase, Symbol, Val, VecObject,
};
use soroban_synth_wasm::{Arity, ModEmitter, Operand};

use crate::{
    budget::{AsBudget, Budget},
    host::HostLifecycleEvent,
    storage::{SnapshotSource, Storage},
    xdr, Error, Host, HostError, LedgerInfo,
};

use soroban_bench_utils::HostTracker;

// Test utilities for the host, used in various tests in sub-modules.
pub(crate) trait AsScVal {
    fn as_scval(&self) -> ScVal;
}

impl AsScVal for u32 {
    fn as_scval(&self) -> ScVal {
        ScVal::U32(*self)
    }
}

impl AsScVal for i32 {
    fn as_scval(&self) -> ScVal {
        ScVal::I32(*self)
    }
}

impl AsScVal for ScVec {
    fn as_scval(&self) -> ScVal {
        ScVal::Vec(Some(self.clone()))
    }
}

pub(crate) fn generate_account_id(host: &Host) -> AccountId {
    AccountId(PublicKey::PublicKeyTypeEd25519(Uint256(
        generate_bytes_array(host),
    )))
}

pub(crate) fn generate_bytes_array(host: &Host) -> [u8; 32] {
    let mut bytes: [u8; 32] = Default::default();
    host.with_test_prng(|chacha| Ok(chacha.fill_bytes(&mut bytes)))
        .unwrap();
    bytes
}

pub(crate) fn wasm_module_with_4n_insns(n: usize) -> Vec<u8> {
    let mut fe = ModEmitter::new().func(Arity(1), 0);
    let arg = fe.args[0];
    fe.push(Operand::Const64(1));
    for i in 0..n {
        fe.push(arg);
        fe.push(Operand::Const64(i as i64));
        fe.i64_mul();
        fe.i64_add();
    }
    fe.drop();
    fe.push(Symbol::try_from_small_str("pass").unwrap());
    fe.finish_and_export("test").finish()
}

pub(crate) struct MockSnapshotSource(BTreeMap<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>);

impl MockSnapshotSource {
    pub(crate) fn new() -> Self {
        Self(BTreeMap::<Rc<LedgerKey>, (Rc<LedgerEntry>, Option<u32>)>::new())
    }
}
impl SnapshotSource for MockSnapshotSource {
    fn get(&self, key: &Rc<LedgerKey>) -> Result<(Rc<LedgerEntry>, Option<u32>), HostError> {
        if let Some(val) = self.0.get(key) {
            Ok((Rc::clone(&val.0), val.1))
        } else {
            Err(Error::from_type_and_code(ScErrorType::Storage, ScErrorCode::MissingValue).into())
        }
    }

    fn has(&self, key: &Rc<LedgerKey>) -> Result<bool, HostError> {
        Ok(self.0.contains_key(key))
    }
}

#[allow(dead_code)]
impl Host {
    pub(crate) fn test_budget(self, cpu: u64, mem: u64) -> Self {
        self.with_budget(|budget| {
            budget.reset_limits(cpu, mem)?; // something big but finite that we may exceed
            budget.reset_models()?;
            Ok(())
        })
        .unwrap();
        self
    }

    pub(crate) fn enable_model(
        self,
        ty: ContractCostType,
        const_cpu: u64,
        lin_cpu: u64,
        const_mem: u64,
        lin_mem: u64,
    ) -> Self {
        self.with_budget(|budget| {
            budget.override_model_with_unscaled_params(ty, const_cpu, lin_cpu, const_mem, lin_mem)
        })
        .unwrap();
        self
    }

    pub(crate) fn measured_call(
        &self,
        contract: AddressObject,
        func: Symbol,
        args: VecObject,
    ) -> Result<Val, HostError> {
        let _span = tracy_span!("measured_call");
        let budget = self.as_budget();
        budget.reset_unlimited()?;
        let ht = Rc::new(RefCell::new(HostTracker::new()));

        if std::env::var("EXCLUDE_VM_INSTANTIATION").is_ok() {
            let ht2 = ht.clone();
            let budget2 = budget.clone();
            self.set_lifecycle_event_hook(Some(Rc::new(move |evt| {
                match evt {
                    HostLifecycleEvent::VmInstantiated => {
                        budget2.reset_unlimited()?;
                        ht2.borrow_mut().start(None);
                    }
                }
                Ok(())
            })))?;
        } else {
            ht.borrow_mut().start(None);
        }
        let val = self.call(contract, func, args);
        self.set_lifecycle_event_hook(None)?;

        let (cpu_actual, mem_actual, time_nsecs) = Rc::into_inner(ht).unwrap().into_inner().stop();

        let cpu_metered = budget
            .get_cpu_insns_consumed()
            .expect("unable to retrieve cpu consumed");
        let mem_metered = budget
            .get_mem_bytes_consumed()
            .expect("unable to retrieve mem consumed");

        let cpu_diff = (cpu_metered - cpu_actual) as i64;
        let cpu_metered_diff_percent = 100 * cpu_diff / (cpu_metered as i64).max(1);
        let mem_diff = (mem_metered - mem_actual) as i64;
        let mem_metered_diff_percent = 100 * mem_diff / (mem_metered as i64).max(1);
        let metered_insn_nsecs_ratio: f64 = (cpu_metered as f64) / (time_nsecs as f64).max(1.0);
        let actual_insn_nsecs_ratio: f64 = (cpu_actual as f64) / (time_nsecs as f64).max(1.0);

        println!();
        println!(
            "metered cpu insns: {}, actual cpu insns {}, diff: {} ({:.3}%)",
            cpu_metered, cpu_actual, cpu_diff, cpu_metered_diff_percent
        );
        println!(
            "metered mem bytes: {}, actual mem bytes {}, diff: {} ({:.3}%)",
            mem_metered, mem_actual, mem_diff, mem_metered_diff_percent
        );
        println!("time_nsecs: {}", time_nsecs);
        println!(
            "metered cpu_insn/time_nsecs ratio: {:.3}",
            metered_insn_nsecs_ratio
        );
        println!(
            "actual cpu_insn/time_nsecs ratio: {:.3}",
            actual_insn_nsecs_ratio
        );
        println!();

        val
    }
}
