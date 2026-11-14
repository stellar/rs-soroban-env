use crate::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use ark_bn254::{Fr, G1Affine, G1Projective};
use ark_ec::{
    short_weierstrass::{Affine, SWCurveConfig},
    AffineRepr, CurveGroup,
};
use ark_ff::{BigInteger, Field, PrimeField, Zero};
use ark_serialize::CanonicalSerialize;
use soroban_env_host::{
    xdr::{ScErrorCode, ScErrorType},
    Env, EnvBase, Error, Host, HostError, TryFromVal, TryIntoVal, Val, U256,
};
use std::fmt::Debug;

/// An oracle result computed during generation, before calling the host.
pub type Expected<T> = Result<T, Error>;

pub enum Target {
    Bn254G1Msm(Vec<Vec<u8>>, Vec<Num256>, Expected<Vec<u8>>),
    Bn254FrAdd(Num256, Num256, Expected<U256>),
    Bn254FrSub(Num256, Num256, Expected<U256>),
    Bn254FrMul(Num256, Num256, Expected<U256>),
    Bn254FrPow(Num256, u64, Expected<U256>),
    Bn254FrInv(Num256, Expected<U256>),
    Bls12G1Curve(Vec<u8>, Expected<bool>),
    Bls12G2Curve(Vec<u8>, Expected<bool>),
    Bn254G1Curve(Vec<u8>, Expected<bool>),
}

const INVALID_INPUT: Error =
    Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput);

#[derive(Clone, Copy, Debug)]
enum PointCase {
    Valid,
    TooShort,
    TooLong,
    Compressed,
    CompressionFlag,
    InfinityFlagNonzero,
    SortFlag,
    NonCanonicalCoordinate,
}

const BLS_POINT_CASES: &[PointCase] = &[
    PointCase::Valid,
    PointCase::TooShort,
    PointCase::TooLong,
    PointCase::Compressed,
    PointCase::CompressionFlag,
    PointCase::InfinityFlagNonzero,
    PointCase::SortFlag,
    PointCase::NonCanonicalCoordinate,
];
const BN_POINT_CASES: &[PointCase] = &[
    PointCase::Valid,
    PointCase::TooShort,
    PointCase::TooLong,
    PointCase::Compressed,
    PointCase::CompressionFlag,
    PointCase::NonCanonicalCoordinate,
];

#[derive(Clone, Copy, Debug)]
enum MsmCase {
    Valid,
    DifferentLengths,
    EmptyPoints,
    EmptyScalars,
    BothEmpty,
    InvalidPoint(PointCase),
    OffCurvePoint,
}

const MSM_CASES: &[MsmCase] = &[
    MsmCase::Valid,
    MsmCase::DifferentLengths,
    MsmCase::EmptyPoints,
    MsmCase::EmptyScalars,
    MsmCase::BothEmpty,
    MsmCase::InvalidPoint(PointCase::TooShort),
    MsmCase::InvalidPoint(PointCase::TooLong),
    MsmCase::InvalidPoint(PointCase::Compressed),
    MsmCase::OffCurvePoint,
    MsmCase::InvalidPoint(PointCase::CompressionFlag),
    MsmCase::InvalidPoint(PointCase::NonCanonicalCoordinate),
];

// ============================================================================
// Arbitrary impl for Target
// ============================================================================

impl<'a> Arbitrary<'a> for Target {
    fn arbitrary(u: &mut Unstructured<'a>) -> arbitrary::Result<Self> {
        let variant = u.int_in_range(0u8..=8)?;
        match variant {
            0 => generate_msm(u),
            1..=3 => {
                let lhs = u.arbitrary::<Num256>()?;
                let rhs = u.arbitrary::<Num256>()?;
                let (a, b) = (lhs.as_fr(), rhs.as_fr());
                Ok(match variant {
                    1 => Self::Bn254FrAdd(lhs, rhs, Ok(fr_to_u256(a + b))),
                    2 => Self::Bn254FrSub(lhs, rhs, Ok(fr_to_u256(a - b))),
                    3 => Self::Bn254FrMul(lhs, rhs, Ok(fr_to_u256(a * b))),
                    _ => unreachable!(),
                })
            }
            4 => {
                let num = u.arbitrary::<Num256>()?;
                let exp = u.arbitrary::<u64>()?;
                let expected = Ok(fr_to_u256(num.as_fr().pow([exp])));
                Ok(Self::Bn254FrPow(num, exp, expected))
            }
            5 => {
                // Zero includes multiples of r, since inputs are reduced mod r.
                let case = u.int_in_range(0u8..=3)?;
                let num = match case {
                    0 => u.arbitrary::<Num256>()?,
                    1 => Num256::from_u256(U256::ZERO),
                    2 => Num256::from_u256(scalar_modulus()),
                    3 => Num256::from_u256(scalar_modulus() * U256::from(u.int_in_range(2u8..=5)?)),
                    _ => unreachable!(),
                };
                let expected = num.as_fr().inverse().map(fr_to_u256).ok_or(INVALID_INPUT);
                Ok(Self::Bn254FrInv(num, expected))
            }
            6 => {
                let (bytes, expected) = generate_bls_curve_input::<ark_bls12_381::g1::Config>(u)?;
                Ok(Self::Bls12G1Curve(bytes, expected))
            }
            7 => {
                let (bytes, expected) = generate_bls_curve_input::<ark_bls12_381::g2::Config>(u)?;
                Ok(Self::Bls12G2Curve(bytes, expected))
            }
            8 => {
                let case = *u.choose(BN_POINT_CASES)?;
                let (point, on_curve) = generate_curve_point::<ark_bn254::g1::Config>(u)?;
                let bytes = encode_bn_point(&point);
                let expected = match case {
                    PointCase::Valid => Ok(on_curve),
                    _ => Err(INVALID_INPUT),
                };
                Ok(Self::Bn254G1Curve(
                    apply_point_case(bytes, &point, case),
                    expected,
                ))
            }
            _ => unreachable!(),
        }
    }
}

// ============================================================================
// Curve point generation
// ============================================================================

fn generate_bls_curve_input<P: SWCurveConfig>(
    u: &mut Unstructured<'_>,
) -> arbitrary::Result<(Vec<u8>, Expected<bool>)> {
    let case = *u.choose(BLS_POINT_CASES)?;
    let (point, on_curve) = generate_curve_point::<P>(u)?;
    let mut bytes = Vec::new();
    point.serialize_uncompressed(&mut bytes).unwrap();
    let expected = match case {
        PointCase::Valid => Ok(on_curve),
        _ => Err(INVALID_INPUT),
    };
    Ok((apply_point_case(bytes, &point, case), expected))
}

fn apply_point_case<P: SWCurveConfig>(
    mut bytes: Vec<u8>,
    point: &Affine<P>,
    case: PointCase,
) -> Vec<u8> {
    match case {
        PointCase::Valid => (),
        PointCase::TooShort => {
            bytes.pop();
        }
        PointCase::TooLong => bytes.push(0),
        PointCase::Compressed => {
            bytes.clear();
            point.serialize_compressed(&mut bytes).unwrap();
        }
        PointCase::CompressionFlag => bytes[0] |= 0x80,
        PointCase::InfinityFlagNonzero => {
            // Isolate the infinity error from compression/sort flags, and
            // ensure nonzero payload even when the source point is infinity.
            bytes[0] = (bytes[0] & 0x1f) | 0x40;
            let last = bytes.len() - 1;
            bytes[last] |= 1;
        }
        PointCase::SortFlag => bytes[0] |= 0x20,
        PointCase::NonCanonicalCoordinate => {
            // An all-ones y component exceeds either base-field modulus.
            // Clear infinity so even an infinity source reaches y decoding.
            let component_size =
                <<P::BaseField as Field>::BasePrimeField as PrimeField>::MODULUS_BIT_SIZE
                    .div_ceil(8) as usize;
            let start = bytes.len() - component_size;
            bytes[start..].fill(0xff);
            bytes[0] &= !0x40;
        }
    }
    bytes
}

fn generate_curve_point<P: SWCurveConfig>(
    u: &mut Unstructured<'_>,
) -> arbitrary::Result<(Affine<P>, bool)> {
    let on_curve = u.arbitrary::<bool>()?;
    let infinity = u.ratio(1u8, 16)?;
    Ok((sample_curve_point::<P>(u, on_curve, infinity)?, on_curve))
}

// Sample each prime-field component directly, so fuzz mutations control
// coordinates locally. Reduction accepts every full-width byte representation.
fn sample_field<F: Field>(u: &mut Unstructured<'_>) -> arbitrary::Result<F> {
    let component_size = F::BasePrimeField::MODULUS_BIT_SIZE.div_ceil(8) as usize;
    let bytes = u.bytes(component_size * F::extension_degree() as usize)?;
    let components = bytes
        .chunks_exact(component_size)
        .map(F::BasePrimeField::from_be_bytes_mod_order);
    Ok(F::from_base_prime_field_elems(components)
        .expect("sampled exactly the field's extension degree"))
}

fn sample_curve_point<P: SWCurveConfig>(
    u: &mut Unstructured<'_>,
    on_curve: bool,
    infinity: bool,
) -> arbitrary::Result<Affine<P>> {
    if on_curve && infinity {
        return Ok(Affine::identity());
    }
    // Every attempt consumes coordinate bytes; exhausted input ends retries.
    loop {
        let x = sample_field::<P::BaseField>(u)?;
        if on_curve {
            if let Some(point) = Affine::<P>::get_point_from_x_unchecked(x, u.arbitrary::<bool>()?)
            {
                return Ok(point);
            }
        } else {
            let y = sample_field::<P::BaseField>(u)?;
            let point = Affine::<P>::new_unchecked(x, y);
            // BN254 interprets (0, 0) as on-curve infinity.
            if !point.is_on_curve() && (!x.is_zero() || !y.is_zero()) {
                return Ok(point);
            }
        }
    }
}

fn encode_bn_point(point: &G1Affine) -> Vec<u8> {
    let mut bytes = vec![0; 64];
    point.x.serialize_uncompressed(&mut bytes[..32]).unwrap();
    point.y.serialize_uncompressed(&mut bytes[32..]).unwrap();
    bytes[..32].reverse();
    bytes[32..].reverse();
    bytes
}

// ============================================================================
// MSM Generation
// ============================================================================

fn generate_msm(u: &mut Unstructured<'_>) -> arbitrary::Result<Target> {
    let case = *u.choose(MSM_CASES)?;
    let size = u.int_in_range(1u8..=255)? as usize;
    let (point_count, scalar_count) = match case {
        MsmCase::EmptyPoints => (0, size),
        MsmCase::EmptyScalars => (size, 0),
        MsmCase::BothEmpty => (0, 0),
        MsmCase::DifferentLengths if u.arbitrary::<bool>()? => (size, size + 1),
        MsmCase::DifferentLengths => (size + 1, size),
        _ => (size, size),
    };
    let invalid_index = match case {
        MsmCase::InvalidPoint(_) | MsmCase::OffCurvePoint => Some(u.int_in_range(0..=size - 1)?),
        _ => None,
    };
    let mut points = Vec::with_capacity(point_count);
    let mut scalars = Vec::with_capacity(scalar_count);
    let mut sum = G1Projective::zero();

    for index in 0..point_count.max(scalar_count) {
        let point = if index < point_count {
            let on_curve =
                !(matches!(case, MsmCase::OffCurvePoint) && invalid_index == Some(index));
            let infinity = u.ratio(1u8, 16)?;
            let point = sample_curve_point::<ark_bn254::g1::Config>(u, on_curve, infinity)?;
            let bytes = encode_bn_point(&point);
            points.push(match case {
                MsmCase::InvalidPoint(encoding) if invalid_index == Some(index) => {
                    apply_point_case(bytes, &point, encoding)
                }
                _ => bytes,
            });
            Some(point)
        } else {
            None
        };
        if index < scalar_count {
            let scalar = u.arbitrary::<Num256>()?;
            // The success oracle sums individual scalar multiplications;
            // it does not call the host's MSM implementation.
            if matches!(case, MsmCase::Valid) {
                sum += point
                    .expect("valid MSM vectors have equal lengths")
                    .mul_bigint(scalar.as_fr().into_bigint());
            }
            scalars.push(scalar);
        }
    }
    let expected = match case {
        MsmCase::Valid => Ok(encode_bn_point(&sum.into_affine())),
        _ => Err(INVALID_INPUT),
    };
    Ok(Target::Bn254G1Msm(points, scalars, expected))
}

// ============================================================================
// Num256 and Fr utils
// ============================================================================

#[derive(Arbitrary, Clone, Copy, Debug)]
pub struct Num256(u128, u128);

impl Num256 {
    pub fn as_u256(&self) -> U256 {
        U256::from_words(self.0, self.1)
    }

    fn from_u256(value: U256) -> Self {
        let (hi, lo) = value.into_words();
        Self(hi, lo)
    }

    fn as_fr(&self) -> Fr {
        Fr::from_be_bytes_mod_order(&self.as_u256().to_be_bytes())
    }
}

fn fr_to_u256(value: Fr) -> U256 {
    U256::from_be_bytes(value.into_bigint().to_bytes_be().try_into().unwrap())
}

fn scalar_modulus() -> U256 {
    U256::from_be_bytes(Fr::MODULUS.to_bytes_be().try_into().unwrap())
}

// ============================================================================
// Target run impl
// ============================================================================

fn check_result<T: PartialEq + Debug>(
    expected: Expected<T>,
    actual: Result<T, HostError>,
) -> FuzzResult {
    if let Err(err) = &actual {
        if !err.is_recoverable() {
            return FuzzResult::InternalError;
        }
    }
    assert_eq!(actual.map_err(|err| err.error), expected);
    FuzzResult::Ok
}

impl Target {
    fn run(self, host: &Host) -> FuzzResult {
        match self {
            Self::Bn254G1Msm(points, scalars, expected) => {
                let points: Vec<Val> = points
                    .iter()
                    .map(|b| b.as_slice().try_into_val(host).unwrap())
                    .collect();
                let scalars: Vec<Val> = scalars
                    .iter()
                    .map(|s| s.as_u256().try_into_val(host).unwrap())
                    .collect();
                let points = host.vec_new_from_slice(&points).unwrap();
                let scalars = host.vec_new_from_slice(&scalars).unwrap();
                let actual = host
                    .bn254_g1_msm(points, scalars)
                    .map(|v| Vec::<u8>::try_from_val(host, &v).unwrap());
                check_result(expected, actual)
            }
            Self::Bn254FrAdd(lhs, rhs, expected) => {
                let lhs = lhs.as_u256().try_into_val(host).unwrap();
                let rhs = rhs.as_u256().try_into_val(host).unwrap();
                let actual = host
                    .bn254_fr_add(lhs, rhs)
                    .map(|v| U256::try_from_val(host, &Val::from(v)).unwrap());
                check_result(expected, actual)
            }
            Self::Bn254FrSub(lhs, rhs, expected) => {
                let lhs = lhs.as_u256().try_into_val(host).unwrap();
                let rhs = rhs.as_u256().try_into_val(host).unwrap();
                let actual = host
                    .bn254_fr_sub(lhs, rhs)
                    .map(|v| U256::try_from_val(host, &Val::from(v)).unwrap());
                check_result(expected, actual)
            }
            Self::Bn254FrMul(lhs, rhs, expected) => {
                let lhs = lhs.as_u256().try_into_val(host).unwrap();
                let rhs = rhs.as_u256().try_into_val(host).unwrap();
                let actual = host
                    .bn254_fr_mul(lhs, rhs)
                    .map(|v| U256::try_from_val(host, &Val::from(v)).unwrap());
                check_result(expected, actual)
            }
            Self::Bn254FrPow(num, exp, expected) => {
                let num = num.as_u256().try_into_val(host).unwrap();
                let exp = exp.try_into_val(host).unwrap();
                let actual = host
                    .bn254_fr_pow(num, exp)
                    .map(|v| U256::try_from_val(host, &Val::from(v)).unwrap());
                check_result(expected, actual)
            }
            Self::Bn254FrInv(num, expected) => {
                let num = num.as_u256().try_into_val(host).unwrap();
                let actual = host
                    .bn254_fr_inv(num)
                    .map(|v| U256::try_from_val(host, &Val::from(v)).unwrap());
                check_result(expected, actual)
            }
            Self::Bls12G1Curve(bytes, expected) => {
                let point = bytes.as_slice().try_into_val(host).unwrap();
                check_result(
                    expected,
                    host.bls12_381_g1_is_on_curve(point).map(bool::from),
                )
            }
            Self::Bls12G2Curve(bytes, expected) => {
                let point = bytes.as_slice().try_into_val(host).unwrap();
                check_result(
                    expected,
                    host.bls12_381_g2_is_on_curve(point).map(bool::from),
                )
            }
            Self::Bn254G1Curve(bytes, expected) => {
                let point = bytes.as_slice().try_into_val(host).unwrap();
                check_result(expected, host.bn254_g1_is_on_curve(point).map(bool::from))
            }
        }
    }
}

// ============================================================================
// run_fuzz_target
// ============================================================================

pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let target: Target = match Unstructured::new(data).arbitrary() {
        Ok(target) => target,
        Err(_) => return FuzzResult::Reject,
    };
    let host = Host::test_host();
    // These targets check CAP semantics; resource exhaustion must not mask an
    // expected success or the particular invalid-input error under test.
    host.budget_cloned().reset_unlimited().unwrap();
    target.run(&host)
}
