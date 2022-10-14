use crate::common::HostCostMeasurement;
use ed25519_dalek::{PublicKey, SecretKey};
use rand::rngs::StdRng;
use soroban_env_host::{budget::CostType, Host};

pub(crate) struct ComputeEd25519PubKeyRun {
    keys: Vec<Vec<u8>>,
}

impl HostCostMeasurement for ComputeEd25519PubKeyRun {
    const COST_TYPE: CostType = CostType::ComputeEd25519PubKey;

    fn new_random_case(_host: &Host, rng: &mut StdRng, input: u64) -> Self {
        let keys = (0..input)
            .map(|_| {
                let secret = SecretKey::generate(rng);
                let public: PublicKey = (&secret).into();
                public.as_bytes().as_slice().into()
            })
            .collect();
        Self { keys }
    }

    fn get_input(&self, _host: &Host) -> u64 {
        self.keys.len() as u64
    }

    fn run(&mut self, _iter: u64, _host: &Host) {
        for i in self.keys.iter() {
            ed25519_dalek::PublicKey::from_bytes(i.as_slice()).expect("publickey");
        }
    }
}
