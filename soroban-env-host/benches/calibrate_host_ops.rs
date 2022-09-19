// Run this with
// $ cargo bench calibrate_host_ops -- --nocapture

mod common;

use common::*;
use ed25519_dalek::{Keypair, PublicKey, SecretKey, Signature, Signer, Verifier};
use rand::SeedableRng;
use sha2::{Digest, Sha256};
use soroban_env_host::{
    budget::CostType,
    xdr::{ScMap, ScMapEntry, ScObject, ScVal, ScVec},
    EnvVal, Host, RawVal,
};

struct ScVecToHostVecRun {
    val: ScVal,
}

struct ImVecNewRun {
    count: u64,
    val: ScVal,
}

struct ScMapToHostMapRun {
    val: ScVal,
}

struct ImMapImmutEntryRun {
    map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    size: u64,
}

struct ImMapMutEntryRun {
    map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>>,
    size: u64,
}

struct ImVecImmutEntryRun {
    vec: im_rc::Vector<EnvVal<Host, RawVal>>,
    size: u64,
}

struct ImVecMutEntryRun {
    vec: im_rc::Vector<EnvVal<Host, RawVal>>,
    size: u64,
}

struct HostObjAllocSlotRun {
    count: u64,
    val: ScVal,
}

struct ComputeSha256HashRun {
    buf: Vec<u8>,
}

struct ComputeEd25519PubKeyRun {
    keys: Vec<Vec<u8>>,
}

struct VerifyEd25519SigRun {
    key: PublicKey,
    msg: Vec<u8>,
    sig: Signature,
}

/// Measures the costs of allocating vectors of varying sizes.
impl HostCostMeasurement for ScVecToHostVecRun {
    const COST_TYPE: CostType = CostType::ScVecToHostVec;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let scvec: ScVec = ScVec(
            (0..size)
                .map(|i| ScVal::U32(i as u32))
                .collect::<Vec<ScVal>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { val }
    }

    fn run(&mut self, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}

/// Measures the costs of allocating large numbers of 0-sized vectors.
impl HostCostMeasurement for ImVecNewRun {
    const COST_TYPE: CostType = CostType::ImVecNew;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 1000;
        let scvec: ScVec = ScVec(vec![].try_into().unwrap());
        let val = ScVal::Object(Some(ScObject::Vec(scvec)));
        Self { count: size, val }
    }

    fn run(&mut self, host: &Host) {
        for _ in 0..self.count {
            host.inject_val(&self.val).unwrap();
        }
    }
}

/// Measures the costs of allocating maps of varying sizes.
impl HostCostMeasurement for ScMapToHostMapRun {
    const COST_TYPE: CostType = CostType::ScMapToHostMap;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let scmap: ScMap = ScMap(
            (0..size)
                .map(|i| ScMapEntry {
                    key: ScVal::U32(i as u32),
                    val: ScVal::U32(i as u32),
                })
                .collect::<Vec<ScMapEntry>>()
                .try_into()
                .unwrap(),
        );
        let val = ScVal::Object(Some(ScObject::Map(scmap)));
        Self { val }
    }

    fn run(&mut self, host: &Host) {
        host.inject_val(&self.val).unwrap();
    }
}

/// Measures the costs of accessing maps of varying sizes.
impl HostCostMeasurement for ImMapImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapImmutEntry;

    fn new(host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>> = (0..size)
            .map(|k| {
                let ev = EnvVal {
                    env: host.clone(),
                    val: RawVal::from_u32(k as u32),
                };
                (ev.clone(), ev)
            })
            .collect();
        Self { map, size }
    }

    fn run(&mut self, host: &Host) {
        let ev = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32((self.size / 2) as u32),
        };
        let _ = self.map.get(&ev);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.size as u64
    }
}

impl HostCostMeasurement for ImMapMutEntryRun {
    const COST_TYPE: CostType = CostType::ImMapMutEntry;

    fn new(host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let map: im_rc::OrdMap<EnvVal<Host, RawVal>, EnvVal<Host, RawVal>> = (0..size)
            .map(|k| {
                let ev = EnvVal {
                    env: host.clone(),
                    val: RawVal::from_u32(k as u32),
                };
                (ev.clone(), ev)
            })
            .collect();
        Self { map, size }
    }

    fn run(&mut self, host: &Host) {
        let ev = EnvVal {
            env: host.clone(),
            val: RawVal::from_u32((self.size / 2) as u32),
        };
        let _ = self.map.get_mut(&ev);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.size as u64
    }
}

impl HostCostMeasurement for ImVecImmutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecImmutEntry;

    fn new(host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 1000;
        let vec: im_rc::Vector<EnvVal<Host, RawVal>> = (0..size)
            .map(|k| EnvVal {
                env: host.clone(),
                val: RawVal::from_u32(k as u32),
            })
            .collect();
        Self { vec, size }
    }

    fn run(&mut self, _host: &Host) {
        let _ = self.vec.get((self.size / 2) as usize);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.size as u64
    }
}

impl HostCostMeasurement for ImVecMutEntryRun {
    const COST_TYPE: CostType = CostType::ImVecMutEntry;

    fn new(host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let vec: im_rc::Vector<EnvVal<Host, RawVal>> = (0..size)
            .map(|k| EnvVal {
                env: host.clone(),
                val: RawVal::from_u32(k as u32),
            })
            .collect();
        Self { vec, size }
    }

    fn run(&mut self, _host: &Host) {
        let _ = self.vec.get_mut((self.size / 2) as usize);
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.size as u64
    }
}

/// Measures the costs of allocating large numbers of simple objects.
impl HostCostMeasurement for HostObjAllocSlotRun {
    const COST_TYPE: CostType = CostType::HostObjAllocSlot;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 10000;
        let val = ScVal::Object(Some(ScObject::I64(0)));
        Self { count: size, val }
    }

    fn run(&mut self, host: &Host) {
        for _ in 0..self.count {
            host.inject_val(&self.val).unwrap();
        }
    }
}

impl HostCostMeasurement for ComputeSha256HashRun {
    const COST_TYPE: CostType = CostType::ComputeSha256Hash;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size = size_hint * 100;
        let buf: Vec<u8> = (0..size).map(|n| n as u8).collect();
        Self { buf }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.buf.len() as u64
    }

    fn run(&mut self, _host: &Host) {
        Sha256::digest(&self.buf).as_slice().to_vec();
    }
}

impl HostCostMeasurement for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let mut csprng = rand::rngs::StdRng::from_seed([0xff; 32]);
        let keys = (0..size_hint)
            .map(|_| {
                let secret = SecretKey::generate(&mut csprng);
                let public: PublicKey = (&secret).into();
                public.as_bytes().as_slice().into()
            })
            .collect();
        Self { keys }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.keys.len() as u64
    }

    fn run(&mut self, _host: &Host) {
        for i in self.keys.iter() {
            ed25519_dalek::PublicKey::from_bytes(i.as_slice()).expect("publickey");
        }
    }
}

impl HostCostMeasurement for VerifyEd25519SigRun {
    const COST_TYPE: CostType = CostType::VerifyEd25519Sig;

    fn new(_host: &Host, size_hint: u64) -> Self {
        let size_hint = size_hint * 10000;
        let mut csprng = rand::rngs::StdRng::from_seed([0xff; 32]);
        let keypair: Keypair = Keypair::generate(&mut csprng);
        let key: PublicKey = keypair.public.clone();
        let msg: Vec<u8> = (0..size_hint).map(|x| x as u8).collect();
        let sig: Signature = keypair.sign(msg.as_slice());
        Self { key, msg, sig }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.msg.len() as u64
    }

    fn run(&mut self, _host: &Host) {
        self.key
            .verify(self.msg.as_slice(), &self.sig)
            .expect("verify")
    }
}

fn measure_one<M: HostCostMeasurement>() -> std::io::Result<()> {
    let mut measurements = measure_costs::<M>(0..20)?;
    measurements.subtract_baseline();
    measurements.report();

    if std::env::var("FIT_MODELS").is_ok() {
        measurements.fit_model_to_cpu();
        measurements.fit_model_to_mem();
    }
    Ok(())
}

#[cfg(all(test, any(target_os = "linux", target_os = "macos")))]
fn main() -> std::io::Result<()> {
    env_logger::init();
    measure_one::<ScVecToHostVecRun>()?;
    measure_one::<ScMapToHostMapRun>()?;
    measure_one::<ImVecNewRun>()?;
    measure_one::<ImMapImmutEntryRun>()?;
    measure_one::<ImMapMutEntryRun>()?;
    measure_one::<ImVecImmutEntryRun>()?;
    measure_one::<ImVecMutEntryRun>()?;
    measure_one::<HostObjAllocSlotRun>()?;
    measure_one::<ComputeSha256HashRun>()?;
    measure_one::<ComputeEd25519PubKeyRun>()?;
    measure_one::<VerifyEd25519SigRun>()?;
    Ok(())
}
