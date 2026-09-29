use crate::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use soroban_env_host::{Env, EnvBase, Host, HostError, TryIntoVal, Val, U256};

pub enum Target {
    Bn254G1Msm(Box<[[u8; 64]]>, Box<[Num256]>),
    Bn254FrAdd(Num256, Num256),
    Bn254FrSub(Num256, Num256),
    Bn254FrMul(Num256, Num256),
    Bn254FrPow(Num256, u64),
    Bn254FrInv(Num256),
    Bls12G1Curve([u8; 96]),
    Bls12G2Curve([u8; 192]),
    Bn254G1Curve([u8; 64]),
}

impl<'a> Arbitrary<'a> for Target {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let variant: u8 = u.int_in_range(0..=8)?;
        match variant {
            0 => {
                let size: u8 = u.int_in_range(1..=255)?;
                let mut points = Vec::with_capacity(size.into());
                let mut scalars = Vec::with_capacity(size.into());
                for _ in 0..size {
                    points.push(u.arbitrary::<[u8; 64]>()?);
                    scalars.push(u.arbitrary::<Num256>()?);
                }
                Ok(Target::Bn254G1Msm(points.into(), scalars.into()))
            }
            1 => {
                let (num1, num2) = u.arbitrary::<(Num256, Num256)>()?;
                Ok(Target::Bn254FrAdd(num1, num2))
            }
            2 => {
                let (num1, num2) = u.arbitrary::<(Num256, Num256)>()?;
                Ok(Target::Bn254FrSub(num1, num2))
            }
            3 => {
                let (num1, num2) = u.arbitrary::<(Num256, Num256)>()?;
                Ok(Target::Bn254FrMul(num1, num2))
            }
            4 => {
                let (num1, num2) = u.arbitrary::<(Num256, u64)>()?;
                Ok(Target::Bn254FrPow(num1, num2))
            }
            5 => {
                let num = u.arbitrary::<Num256>()?;
                Ok(Target::Bn254FrInv(num))
            }
            6 => {
                let bytes: [u8; 96] = u.arbitrary()?;
                Ok(Target::Bls12G1Curve(bytes))
            }
            7 => {
                let bytes: [u8; 192] = u.arbitrary()?;
                Ok(Target::Bls12G2Curve(bytes))
            }
            8 => {
                let bytes: [u8; 64] = u.arbitrary()?;
                Ok(Target::Bn254G1Curve(bytes))
            }
            _ => unreachable!(),
        }
    }
}

#[derive(Arbitrary)]
pub struct Num256(u128, u128);

impl Num256 {
    pub fn as_u256(&self) -> U256 {
        U256::from_words(self.0, self.1)
    }
}

pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let mut u = Unstructured::new(data);
    let host = Host::test_host();

    let target: Target = u.arbitrary().unwrap();

    let res: Result<Val, HostError> = match target {
        Target::Bn254G1Msm(points, scalars) => {
            let points_val: Vec<Val> = points
                .iter()
                .map(|b| b.try_into_val(&host).unwrap())
                .collect::<Vec<Val>>();
            let scalars_val: Vec<Val> = scalars
                .iter()
                .map(|s| s.as_u256().try_into_val(&host).unwrap())
                .collect::<Vec<Val>>();

            let points_vec = host.vec_new_from_slice(points_val.as_slice()).unwrap();
            let scalars_vec = host.vec_new_from_slice(scalars_val.as_slice()).unwrap();

            host.bn254_g1_msm(points_vec, scalars_vec).map(|x| x.into())
        }
        Target::Bn254FrAdd(num1, num2) => {
            let v1 = num1.as_u256().try_into_val(&host).unwrap();
            let v2 = num2.as_u256().try_into_val(&host).unwrap();
            host.bn254_fr_add(v1, v2).map(|x| x.into())
        }
        Target::Bn254FrSub(num1, num2) => {
            let v1 = num1.as_u256().try_into_val(&host).unwrap();
            let v2 = num2.as_u256().try_into_val(&host).unwrap();
            host.bn254_fr_sub(v1, v2).map(|x| x.into())
        }
        Target::Bn254FrMul(num1, num2) => {
            let v1 = num1.as_u256().try_into_val(&host).unwrap();
            let v2 = num2.as_u256().try_into_val(&host).unwrap();
            host.bn254_fr_mul(v1, v2).map(|x| x.into())
        }
        Target::Bn254FrPow(num, exp) => {
            let v1 = num.as_u256().try_into_val(&host).unwrap();
            let v2 = exp.try_into_val(&host).unwrap();
            host.bn254_fr_pow(v1, v2).map(|x| x.into())
        }
        Target::Bn254FrInv(num) => {
            let v = num.as_u256().try_into_val(&host).unwrap();
            host.bn254_fr_inv(v).map(|x| x.into())
        }
        Target::Bls12G1Curve(bytes) => {
            let v = bytes.try_into_val(&host).unwrap();
            host.bls12_381_g1_is_on_curve(v).map(|x| x.into())
        }
        Target::Bls12G2Curve(bytes) => {
            let v = bytes.try_into_val(&host).unwrap();
            host.bls12_381_g2_is_on_curve(v).map(|x| x.into())
        }
        Target::Bn254G1Curve(bytes) => {
            let v = bytes.try_into_val(&host).unwrap();
            host.bn254_g1_is_on_curve(v).map(|x| x.into())
        }
    };

    if let Err(err) = res {
        if !err.is_recoverable() {
            return FuzzResult::InternalError;
        }
    }

    FuzzResult::Ok
}
