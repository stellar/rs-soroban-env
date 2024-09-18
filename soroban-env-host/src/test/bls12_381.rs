use crate::{
    crypto::bls12_381::{
        FP2_SERIALIZED_SIZE, FP_SERIALIZED_SIZE, G1_SERIALIZED_SIZE, G2_SERIALIZED_SIZE,
    },
    xdr::{ScErrorCode, ScErrorType},
    BytesObject, Env, EnvBase, Host, HostError, U256Val, U32Val, Val, VecObject,
};
use ark_bls12_381::{Fq, Fq2, Fr, G1Affine, G2Affine, FQ_ONE, FQ_ZERO};
use ark_ec::AffineRepr;
use ark_ff::UniformRand;
use ark_serialize::CanonicalSerialize;
use hex::FromHex;
use rand::{rngs::StdRng, SeedableRng};
use serde::Deserialize;
use std::cmp::Ordering;

const MODULUS: &str = "0x1a0111ea397fe69a4b1ba7b6434bacd764774b84f38512bf6730d2a0f6b0f6241eabfffeb153ffffb9feffffffffaaab";

impl Host {
    pub(crate) fn fp_serialize_into_bytesobj(&self, fp: &Fq) -> Result<BytesObject, HostError> {
        let mut buf = [0u8; FP_SERIALIZED_SIZE];
        self.serialize_uncompressed_into_slice::<FP_SERIALIZED_SIZE, Fq>(&fp, &mut buf, "Fp")?;
        buf.reverse();
        self.add_host_object(self.scbytes_from_slice(&buf)?)
    }

    pub(crate) fn fp2_serialize_into_bytesobj(&self, fp2: &Fq2) -> Result<BytesObject, HostError> {
        let mut buf = [0u8; FP2_SERIALIZED_SIZE];
        self.serialize_uncompressed_into_slice::<FP2_SERIALIZED_SIZE, Fq2>(&fp2, &mut buf, "Fp")?;
        buf.reverse();
        self.add_host_object(self.scbytes_from_slice(&buf)?)
    }
}

enum InvalidPointTypes {
    TooManyBytes,
    TooFewBytes,
    CompressionFlagSet,
    InfinityFlagSetBitsNotAllZero,
    SortFlagSet,
    PointNotOnCurve,
    PointNotInSubgroup,
    OutOfRange,
}

#[allow(unused)]
#[derive(Deserialize, Debug)]
struct Field {
    m: String,
    p: String,
}

#[allow(unused)]
#[derive(Deserialize, Debug)]
struct Map {
    name: String,
}

#[derive(Deserialize, Debug)]
struct Point {
    x: String,
    y: String,
}

#[allow(non_snake_case)]
#[derive(Deserialize, Debug)]
struct TestCase {
    P: Point,
    Q0: Point,
    Q1: Point,
    msg: String,
    u: [String; 2],
}

#[allow(unused, non_snake_case)]
#[derive(Deserialize, Debug)]
struct HashToCurveTestSuite {
    L: String,
    Z: String,
    ciphersuite: String,
    curve: String,
    dst: String,
    expand: String,
    field: Field,
    hash: String,
    k: String,
    map: Map,
    randomOracle: bool,
    vectors: Vec<TestCase>,
}

fn parse_hex(s: &str) -> Vec<u8> {
    Vec::from_hex(s.trim_start_matches("0x")).unwrap()
}

fn sample_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.g1_affine_serialize_uncompressed(&G1Affine::rand(rng))
}

fn sample_g1_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq::rand(rng);
        let y = Fq::rand(rng);
        let p = G1Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.g1_affine_serialize_uncompressed(&p);
        }
    }
}

fn sample_g1_not_in_subgroup(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq::rand(rng);
        if let Some(p) = G1Affine::get_point_from_x_unchecked(x, true) {
            assert!(p.is_on_curve());
            if !p.is_in_correct_subgroup_assuming_on_curve() {
                return host.g1_affine_serialize_uncompressed(&p);
            }
        }
    }
}

fn sample_g1_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g1 = sample_g1(host, rng)?;
    host.bytes_copy_from_slice(g1, U32Val::from(0), MODULUS.as_bytes())
}

fn g1_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.g1_affine_serialize_uncompressed(&G1Affine::zero())
}

fn neg_g1(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g1 = host.g1_affine_deserialize_from_bytesobj(bo, true)?;
    host.g1_affine_serialize_uncompressed(&-g1)
}

fn invalid_g1(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G1Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.g1_affine_serialize_uncompressed(&affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(bo, U32Val::from(G1_SERIALIZED_SIZE as u32), U32Val::from(0))
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(G1_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::CompressionFlagSet => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 7)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::InfinityFlagSetBitsNotAllZero => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 6)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::SortFlagSet => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 5)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g1_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => sample_g1_not_in_subgroup(host, rng),
        InvalidPointTypes::OutOfRange => sample_g1_out_of_range(host, rng),
    }
}

fn sample_g2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.g2_affine_serialize_uncompressed(&G2Affine::rand(rng))
}

fn sample_g2_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        let y = Fq2::rand(rng);
        let p = G2Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.g2_affine_serialize_uncompressed(&p);
        }
    }
}

fn sample_g2_not_in_subgroup(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        if let Some(p) = G2Affine::get_point_from_x_unchecked(x, true) {
            assert!(p.is_on_curve());
            if !p.is_in_correct_subgroup_assuming_on_curve() {
                return host.g2_affine_serialize_uncompressed(&p);
            }
        }
    }
}

fn g2_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.g2_affine_serialize_uncompressed(&G2Affine::zero())
}

fn sample_g2_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g2 = sample_g2(host, rng)?;
    host.bytes_copy_from_slice(g2, U32Val::from(0), MODULUS.as_bytes())
}

fn neg_g2(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g2 = host.g2_affine_deserialize_from_bytesobj(bo, true)?;
    host.g2_affine_serialize_uncompressed(&-g2)
}

fn invalid_g2(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G2Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.g2_affine_serialize_uncompressed(&affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(bo, U32Val::from(G2_SERIALIZED_SIZE as u32), U32Val::from(0))
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(G2_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::CompressionFlagSet => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 7)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::InfinityFlagSetBitsNotAllZero => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 6)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::SortFlagSet => {
            let mut first_byte: u32 = host.bytes_get(bo, U32Val::from(0))?.into();
            first_byte = ((first_byte as u8) | (1 << 5)) as u32;
            host.bytes_put(bo, U32Val::from(0), U32Val::from(first_byte))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g2_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => sample_g2_not_in_subgroup(host, rng),
        InvalidPointTypes::OutOfRange => sample_g2_out_of_range(host, rng),
    }
}

fn parse_g2_point_test_case(host: &Host, p: Point) -> Result<BytesObject, HostError> {
    let mut p_bytes = [0u8; 192];
    // the input point format in each coordinate is (c0,c1), each part
    // being a hex string starting '0x'. So we need to split it by comma,
    // flip the two parts, and parse each part (each part is already
    // big-endian, so all we need to do is to strip the prefix)
    let qx: Vec<_> = p.x.split(',').collect();
    let qy: Vec<_> = p.y.split(',').collect();
    p_bytes[0..48].copy_from_slice(&parse_hex(qx[1]));
    p_bytes[48..96].copy_from_slice(&parse_hex(qx[0]));
    p_bytes[96..144].copy_from_slice(&parse_hex(qy[1]));
    p_bytes[144..192].copy_from_slice(&parse_hex(qy[0]));
    host.bytes_new_from_slice(&p_bytes)
}

#[allow(unused)]
fn sample_fp(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let fp = Fq::rand(rng);
    host.fp_serialize_into_bytesobj(&fp)
}

fn invalid_fp(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let fp = Fq::rand(rng);
    match ty {
        InvalidPointTypes::TooManyBytes => {
            let mut buf = [0u8; FP_SERIALIZED_SIZE + 1]; // one extra zero byte
            host.serialize_uncompressed_into_slice::<49, _>(&fp, &mut buf, "test")?;
            host.bytes_new_from_slice(&buf)
        }
        InvalidPointTypes::TooFewBytes => {
            let mut buf = [0u8; FP_SERIALIZED_SIZE];
            host.serialize_uncompressed_into_slice::<FP_SERIALIZED_SIZE, _>(&fp, &mut buf, "test")?;
            host.bytes_new_from_slice(&buf[0..FP_SERIALIZED_SIZE - 1]) // take one less byte
        }
        InvalidPointTypes::OutOfRange => {
            // Fp can only take the range of (0, MODULUS-1)
            let bytes = parse_hex(&MODULUS);
            host.bytes_new_from_slice(bytes.as_slice())
        }
        _ => panic!("not available"),
    }
}

#[allow(unused)]
fn sample_fp2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let fp2 = Fq2::rand(rng);
    host.fp2_serialize_into_bytesobj(&fp2)
}

fn invalid_fp2(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let fp = Fq::rand(rng);
    match ty {
        InvalidPointTypes::TooManyBytes => {
            let mut buf = [0u8; FP2_SERIALIZED_SIZE + 1]; // one extra zero byte
            host.serialize_uncompressed_into_slice::<97, _>(&fp, &mut buf, "test")?;
            host.bytes_new_from_slice(&buf)
        }
        InvalidPointTypes::TooFewBytes => {
            let mut buf = [0u8; FP2_SERIALIZED_SIZE];
            host.serialize_uncompressed_into_slice::<FP2_SERIALIZED_SIZE, _>(
                &fp, &mut buf, "test",
            )?;
            host.bytes_new_from_slice(&buf[0..FP2_SERIALIZED_SIZE - 1]) // take one less byte
        }
        InvalidPointTypes::OutOfRange => {
            // Each Fp can only take the range of (0, MODULUS-1)
            let bytes = parse_hex(&MODULUS);
            host.bytes_new_from_slice(bytes.as_slice())
        }
        _ => panic!("not available"),
    }
}

fn sample_fr(host: &Host, rng: &mut StdRng) -> Result<U256Val, HostError> {
    let obj = host.obj_from_u256_pieces(
        u64::rand(rng),
        u64::rand(rng),
        u64::rand(rng),
        u64::rand(rng),
    )?;
    Ok(obj.into())
}

fn sample_host_vec<const EXPECTED_SIZE: usize, T: UniformRand + CanonicalSerialize>(
    host: &Host,
    vec_len: usize,
    rng: &mut StdRng,
) -> Result<VecObject, HostError> {
    let vals: Vec<Val> = (0..vec_len)
        .into_iter()
        .map(|_| {
            let t = T::rand(rng);
            let mut buf = vec![0; EXPECTED_SIZE];
            host.serialize_uncompressed_into_slice::<EXPECTED_SIZE, _>(&t, &mut buf, "test")
                .unwrap();
            host.bytes_new_from_slice(&buf).unwrap().to_val()
        })
        .collect();
    host.vec_new_from_slice(&vals)
}

fn zero_g1_vec(host: &Host, vec_len: usize) -> Result<VecObject, HostError> {
    let vals: Vec<Val> = (0..vec_len)
        .into_iter()
        .map(|_| g1_zero(host).unwrap().to_val())
        .collect();
    host.vec_new_from_slice(&vals)
}

fn zero_g2_vec(host: &Host, vec_len: usize) -> Result<VecObject, HostError> {
    let vals: Vec<Val> = (0..vec_len)
        .into_iter()
        .map(|_| g2_zero(host).unwrap().to_val())
        .collect();
    host.vec_new_from_slice(&vals)
}

fn sample_fr_vec(host: &Host, vec_len: usize, rng: &mut StdRng) -> Result<VecObject, HostError> {
    let vals: Vec<Val> = (0..vec_len)
        .into_iter()
        .map(|_| {
            host.obj_from_u256_pieces(
                u64::rand(rng),
                u64::rand(rng),
                u64::rand(rng),
                u64::rand(rng),
            )
            .unwrap()
            .to_val()
        })
        .collect();
    host.vec_new_from_slice(&vals)
}

#[test]
fn check_g1_is_in_subgroup() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid point
    {
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::TooManyBytes,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::TooFewBytes,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::CompressionFlagSet,
                &mut rng
            )?,),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                &mut rng
            )?,),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::SortFlagSet,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::PointNotOnCurve,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g1_is_in_subgroup(invalid_g1(
                &host,
                InvalidPointTypes::OutOfRange,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // valid point in subgroup
    {
        for _ in 0..10 {
            assert!(host
                .bls12_381_check_g1_is_in_subgroup(sample_g1(&host, &mut rng)?)?
                .to_val()
                .is_true())
        }
    }
    // infinity point is in subgroup
    {
        assert!(host
            .bls12_381_check_g1_is_in_subgroup(g1_zero(&host)?)?
            .to_val()
            .is_true())
    }
    // out of subgroup
    {
        for _ in 0..10 {
            assert!(host
                .bls12_381_check_g1_is_in_subgroup(invalid_g1(
                    &host,
                    InvalidPointTypes::PointNotInSubgroup,
                    &mut rng
                )?)?
                .to_val()
                .is_false())
        }
    }
    Ok(())
}

#[test]
fn g1_add() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid p1
    {
        let p2 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::CompressionFlagSet, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(
                    &host,
                    InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                    &mut rng
                )?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::SortFlagSet, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        // addition does not require input points to be in the correcct subgroup
        assert!(host
            .bls12_381_g1_add(
                invalid_g1(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?,
                p2
            )
            .is_ok())
    }
    // invalid p2
    {
        let p1 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::CompressionFlagSet, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(
                    &host,
                    InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                    &mut rng
                )?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::SortFlagSet, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        // addition does not require input points to be in the correcct subgroup
        assert!(host
            .bls12_381_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?
            )
            .is_ok());
    }
    // 3. lhs.add(zero) = lhs
    {
        let p1 = sample_g1(&host, &mut rng)?;
        let res = host.bls12_381_g1_add(p1, g1_zero(&host)?)?;
        assert_eq!(host.obj_cmp(p1.into(), res.into())?, Ordering::Equal as i64);
    }
    // 4. zero.add(rhs) = rhs
    {
        let p2 = sample_g1(&host, &mut rng)?;
        let res = host.bls12_381_g1_add(g1_zero(&host)?, p2)?;
        assert_eq!(host.obj_cmp(p2.into(), res.into())?, Ordering::Equal as i64);
    }
    // 5. communitive a + b = b + a
    {
        let a = sample_g1(&host, &mut rng)?;
        let b = sample_g1(&host, &mut rng)?;
        let a_plus_b = host.bls12_381_g1_add(a, b)?;
        let b_plus_a = host.bls12_381_g1_add(b, a)?;
        assert_eq!(
            host.obj_cmp(a_plus_b.into(), b_plus_a.into())?,
            Ordering::Equal as i64
        );
    }
    // 6. associative (a + b) + c = a + (b + c)
    {
        let a = sample_g1(&host, &mut rng)?;
        let b = sample_g1(&host, &mut rng)?;
        let c = sample_g1(&host, &mut rng)?;
        let aplusb = host.bls12_381_g1_add(a, b)?;
        let aplusb_plus_c = host.bls12_381_g1_add(aplusb, c)?;
        let bplusc = host.bls12_381_g1_add(b, c)?;
        let a_plus_bplusc = host.bls12_381_g1_add(a, bplusc)?;
        assert_eq!(
            host.obj_cmp(aplusb_plus_c.into(), a_plus_bplusc.into())?,
            Ordering::Equal as i64
        );
    }
    // 7. a - a = zero
    {
        let a = sample_g1(&host, &mut rng)?;
        let neg_a = neg_g1(a.clone(), &host)?;
        let res = host.bls12_381_g1_add(a, neg_a)?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn g1_mul() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // 2. lhs * 0 = 0
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = host.obj_from_u256_pieces(0, 0, 0, 0)?;
        let res = host.bls12_381_g1_mul(lhs, rhs.into())?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }
    // 3. lhs * 1 = lhs
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = U256Val::from_u32(1);
        let res = host.bls12_381_g1_mul(lhs, rhs.into())?;
        assert_eq!(
            host.obj_cmp(res.into(), lhs.into())?,
            Ordering::Equal as i64
        );
    }
    // 4. associative P * a * b = P * b * a
    {
        let p = sample_g1(&host, &mut rng)?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let pa = host.bls12_381_g1_mul(p, a)?;
        let pab = host.bls12_381_g1_mul(pa, b)?;
        let pb = host.bls12_381_g1_mul(p, b)?;
        let pba = host.bls12_381_g1_mul(pb, a)?;
        assert_eq!(
            host.obj_cmp(pab.into(), pba.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn g1_msm() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // vector lengths are zero
    {
        let vp = host.vec_new()?;
        let vs = host.vec_new()?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector lengths not equal
    {
        let vp = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 2, &mut rng)?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector g1 not valid
    {
        let vp = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            invalid_g1(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g1_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector of zero points result zero
    {
        let vp = host.vec_new_from_slice(&[g1_zero(&host)?.to_val(); 3])?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        let res = host.bls12_381_g1_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g1_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // vector of zero scalars result in zero point
    {
        let vp = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        let vs = host.vec_new_from_slice(&[U256Val::from_u32(0).to_val(); 3])?;
        let res = host.bls12_381_g1_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g1_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // 6. g1 * (1) + g1 (-1) = 0
    {
        let pt = sample_g1(&host, &mut rng)?;
        let zero = g1_zero(&host)?;
        assert_ne!(
            host.obj_cmp(pt.into(), zero.into())?,
            Ordering::Equal as i64
        );
        let neg_pt = neg_g1(pt, &host)?;
        let vp = host.vec_new_from_slice(&[pt.to_val(), neg_pt.to_val()])?;
        let vs = host.vec_new_from_slice(&[U256Val::from_u32(1).to_val(); 2])?;
        let res = host.bls12_381_g1_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g1_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // 7. associative: shuffle points orders results stay the same
    {
        host.budget_ref().reset_default()?;
        let mut vp = vec![
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ];
        let mut vs = vec![
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
        ];
        let ref_res =
            host.bls12_381_g1_msm(host.vec_new_from_slice(&vp)?, host.vec_new_from_slice(&vs)?)?;
        let mut rng = StdRng::from_seed([0xff; 32]);
        let mut shuffle_with_order = |v1: &mut Vec<Val>, v2: &mut Vec<Val>| {
            use rand::seq::SliceRandom;
            assert_eq!(v1.len(), v2.len());
            let mut indices: Vec<usize> = (0..v1.len()).collect();
            indices.shuffle(&mut rng);
            let v1_shuffled: Vec<Val> = indices.iter().map(|&i| v1[i]).collect();
            let v2_shuffled: Vec<Val> = indices.iter().map(|&i| v2[i]).collect();
            *v1 = v1_shuffled;
            *v2 = v2_shuffled;
        };

        for _ in 0..10 {
            shuffle_with_order(&mut vp, &mut vs);
            let vp_obj = host.vec_new_from_slice(&vp)?;
            let vs_obj = host.vec_new_from_slice(&vs)?;
            let res = host.bls12_381_g1_msm(vp_obj, vs_obj)?;
            assert_eq!(
                host.obj_cmp(res.into(), ref_res.into())?,
                Ordering::Equal as i64
            );
        }
    }
    // 8. msm result is same as invidial mul and add
    {
        host.budget_ref().reset_default()?;
        let vp = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 10, &mut rng)?;
        let vs = sample_fr_vec(&host, 10, &mut rng)?;
        let ref_res = host.bls12_381_g1_msm(vp, vs)?;
        let mut res = g1_zero(&host)?;
        for i in 0..10 {
            let p: BytesObject = host.vec_get(vp, U32Val::from(i))?.try_into()?;
            let s: U256Val = host.vec_get(vs, U32Val::from(i))?.try_into()?;
            let rhs = host.bls12_381_g1_mul(p, s)?;
            res = host.bls12_381_g1_add(res, rhs)?;
        }
        assert_eq!(
            host.obj_cmp(res.into(), ref_res.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn map_fp_to_g1() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid fp: wrong length
    {
        let p1 = invalid_fp(&host, InvalidPointTypes::TooFewBytes, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp_to_g1(p1),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        let p2 = invalid_fp(&host, InvalidPointTypes::TooManyBytes, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp_to_g1(p2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        let p3 = invalid_fp(&host, InvalidPointTypes::OutOfRange, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp_to_g1(p3),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // Test cases from https://datatracker.ietf.org/doc/html/rfc9380#name-bls12381g1_xmdsha-256_sswu_
    // To interpret the results, understand the steps it takes to hash a msg to curve
    //   1. u = hash_to_field(msg, 2)
    //   2. Q0 = map_to_curve(u[0])
    //   3. Q1 = map_to_curve(u[1])
    //   4. R = Q0 + Q1 # Point addition
    //   5. P = clear_cofactor(R)
    //   6. return P
    {
        host.budget_ref().reset_default()?;
        let test_map_fp_to_curve_inner = |u: String, q: Point| -> Result<(), HostError> {
            let mut q_bytes = [0u8; 96];
            q_bytes[0..48].copy_from_slice(&parse_hex(&q.x));
            q_bytes[48..].copy_from_slice(&parse_hex(&q.y));
            let g1 = host.bytes_new_from_slice(&q_bytes)?;
            let fp = host.bytes_new_from_slice(&parse_hex(&u))?;
            let res = host.bls12_381_map_fp_to_g1(fp)?;
            assert_eq!(host.obj_cmp(res.into(), g1.into())?, Ordering::Equal as i64);
            Ok(())
        };

        let test_suite: HashToCurveTestSuite = serde_json::from_slice(
            &std::fs::read("./src/test/data/BLS12381G1_XMD_SHA-256_SSWU_RO_.json").unwrap(),
        )
        .unwrap();
        println!("{test_suite:?}");
        for case in test_suite.vectors {
            let [u0, u1] = case.u;
            test_map_fp_to_curve_inner(u0, case.Q0)?;
            test_map_fp_to_curve_inner(u1, case.Q1)?;
        }
    }
    Ok(())
}

#[test]
fn hash_to_g1() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // 1. invalid input dst length = 0
    {
        let dst = host.bytes_new_from_slice(&[])?;
        let msg = host.bytes_new_from_slice("some message".as_bytes())?;
        assert!(HostError::result_matches_err(
            host.bls12_381_hash_to_g1(msg, dst),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 2. invalid input dst length > 255
    {
        let dst = host.bytes_new_from_slice(&[0; 256])?;
        let msg = host.bytes_new_from_slice("some message".as_bytes())?;
        assert!(HostError::result_matches_err(
            host.bls12_381_hash_to_g1(msg, dst),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 3. test vectors from https://datatracker.ietf.org/doc/html/rfc9380#name-bls12381g1_xmdsha-256_sswu_
    {
        let test_suite: HashToCurveTestSuite = serde_json::from_slice(
            &std::fs::read("./src/test/data/BLS12381G1_XMD_SHA-256_SSWU_RO_.json").unwrap(),
        )
        .unwrap();
        let dst = host.bytes_new_from_slice(test_suite.dst.as_bytes())?;
        let parse_g1 = |p: Point| -> Result<BytesObject, HostError> {
            let mut p_bytes = [0u8; 96];
            p_bytes[0..48].copy_from_slice(&parse_hex(&p.x));
            p_bytes[48..].copy_from_slice(&parse_hex(&p.y));
            host.bytes_new_from_slice(&p_bytes)
        };

        for case in test_suite.vectors {
            let msg = host.bytes_new_from_slice(case.msg.as_bytes())?;
            let g1 = host.bls12_381_hash_to_g1(msg, dst)?;
            let g1_ref = parse_g1(case.P)?;
            assert_eq!(
                host.obj_cmp(g1.into(), g1_ref.into())?,
                Ordering::Equal as i64
            );
        }
    }
    Ok(())
}

// g2 tests

#[test]
fn check_g2_is_in_subgroup() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid point
    {
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::TooManyBytes,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::TooFewBytes,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::CompressionFlagSet,
                &mut rng
            )?,),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                &mut rng
            )?,),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::SortFlagSet,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::PointNotOnCurve,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_check_g2_is_in_subgroup(invalid_g2(
                &host,
                InvalidPointTypes::OutOfRange,
                &mut rng
            )?),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // valid point in subgroup
    {
        for _ in 0..10 {
            assert!(host
                .bls12_381_check_g2_is_in_subgroup(sample_g2(&host, &mut rng)?)?
                .to_val()
                .is_true())
        }
    }
    // infinity point is in subgroup
    {
        assert!(host
            .bls12_381_check_g2_is_in_subgroup(g2_zero(&host)?)?
            .to_val()
            .is_true())
    }
    // out of subgroup
    {
        for _ in 0..10 {
            assert!(host
                .bls12_381_check_g2_is_in_subgroup(invalid_g2(
                    &host,
                    InvalidPointTypes::PointNotInSubgroup,
                    &mut rng
                )?)?
                .to_val()
                .is_false())
        }
    }
    Ok(())
}

#[test]
fn g2_add() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid p1
    {
        let p2 = sample_g2(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::TooManyBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::TooFewBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::CompressionFlagSet, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(
                    &host,
                    InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                    &mut rng
                )?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::SortFlagSet, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        // addition does not require input points to be in the correcct subgroup
        assert!(host
            .bls12_381_g2_add(
                invalid_g2(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?,
                p2
            )
            .is_ok());
    }
    // invalid p2
    {
        let p1 = sample_g2(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::TooManyBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::TooFewBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::CompressionFlagSet, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(
                    &host,
                    InvalidPointTypes::InfinityFlagSetBitsNotAllZero,
                    &mut rng
                )?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::SortFlagSet, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        // addition does not require input points to be in the correcct subgroup
        assert!(host
            .bls12_381_g2_add(
                p1,
                invalid_g2(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?
            )
            .is_ok());
    }
    // 3. lhs.add(zero) = lhs
    {
        let p1 = sample_g2(&host, &mut rng)?;
        let res = host.bls12_381_g2_add(p1, g2_zero(&host)?)?;
        assert_eq!(host.obj_cmp(p1.into(), res.into())?, Ordering::Equal as i64);
    }
    // 4. zero.add(rhs) = rhs
    {
        let p2 = sample_g2(&host, &mut rng)?;
        let res = host.bls12_381_g2_add(g2_zero(&host)?, p2)?;
        assert_eq!(host.obj_cmp(p2.into(), res.into())?, Ordering::Equal as i64);
    }
    // 5. communitive a + b = b + a
    {
        let a = sample_g2(&host, &mut rng)?;
        let b = sample_g2(&host, &mut rng)?;
        let a_plus_b = host.bls12_381_g2_add(a, b)?;
        let b_plus_a = host.bls12_381_g2_add(b, a)?;
        assert_eq!(
            host.obj_cmp(a_plus_b.into(), b_plus_a.into())?,
            Ordering::Equal as i64
        );
    }
    // 6. associative (a + b) + c = a + (b + c)
    {
        let a = sample_g2(&host, &mut rng)?;
        let b = sample_g2(&host, &mut rng)?;
        let c = sample_g2(&host, &mut rng)?;
        let aplusb = host.bls12_381_g2_add(a, b)?;
        let aplusb_plus_c = host.bls12_381_g2_add(aplusb, c)?;
        let bplusc = host.bls12_381_g2_add(b, c)?;
        let a_plus_bplusc = host.bls12_381_g2_add(a, bplusc)?;
        assert_eq!(
            host.obj_cmp(aplusb_plus_c.into(), a_plus_bplusc.into())?,
            Ordering::Equal as i64
        );
    }
    // 7. a - a = zero
    {
        let a = sample_g2(&host, &mut rng)?;
        let neg_a = neg_g2(a.clone(), &host)?;
        let res = host.bls12_381_g2_add(a, neg_a)?;
        let zero = g2_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn g2_mul() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // 2. lhs * 0 = 0
    {
        let lhs = sample_g2(&host, &mut rng)?;
        let rhs = host.obj_from_u256_pieces(0, 0, 0, 0)?;
        let res = host.bls12_381_g2_mul(lhs, rhs.into())?;
        let zero = g2_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }
    // 3. lhs * 1 = lhs
    {
        let lhs = sample_g2(&host, &mut rng)?;
        let rhs = U256Val::from_u32(1);
        let res = host.bls12_381_g2_mul(lhs, rhs.into())?;
        assert_eq!(
            host.obj_cmp(res.into(), lhs.into())?,
            Ordering::Equal as i64
        );
    }
    // 4. associative P * a * b = P * b * a
    {
        let p = sample_g2(&host, &mut rng)?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let pa = host.bls12_381_g2_mul(p, a)?;
        let pab = host.bls12_381_g2_mul(pa, b)?;
        let pb = host.bls12_381_g2_mul(p, b)?;
        let pba = host.bls12_381_g2_mul(pb, a)?;
        assert_eq!(
            host.obj_cmp(pab.into(), pba.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn g2_msm() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // vector lengths are zero
    {
        let vp = host.vec_new()?;
        let vs = host.vec_new()?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector lengths not equal
    {
        let vp = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 2, &mut rng)?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector g2 not valid
    {
        let vp = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            invalid_g2(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_g2_msm(vp, vs),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // vector of zero points result zero
    {
        let vp = host.vec_new_from_slice(&[g2_zero(&host)?.to_val(); 3])?;
        let vs = sample_fr_vec(&host, 3, &mut rng)?;
        let res = host.bls12_381_g2_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g2_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // vector of zero scalars result in zero point
    {
        let vp = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 3, &mut rng)?;
        let vs = host.vec_new_from_slice(&[U256Val::from_u32(0).to_val(); 3])?;
        let res = host.bls12_381_g2_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g2_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // 6. g2 * (1) + g2 (-1) = 0
    {
        host.budget_ref().reset_default()?;
        let pt = sample_g2(&host, &mut rng)?;
        let zero = g2_zero(&host)?;
        assert_ne!(
            host.obj_cmp(pt.into(), zero.into())?,
            Ordering::Equal as i64
        );
        let neg_pt = neg_g2(pt, &host)?;
        let vp = host.vec_new_from_slice(&[pt.to_val(), neg_pt.to_val()])?;
        let vs = host.vec_new_from_slice(&[U256Val::from_u32(1).to_val(); 2])?;
        let res = host.bls12_381_g2_msm(vp, vs)?;
        assert_eq!(
            host.obj_cmp(res.into(), g2_zero(&host)?.into())?,
            Ordering::Equal as i64
        );
    }
    // 7. associative: shuffle points orders results stay the same
    {
        let mut vp = vec![
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ];
        let mut vs = vec![
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
            sample_fr(&host, &mut rng)?.to_val(),
        ];
        let ref_res =
            host.bls12_381_g2_msm(host.vec_new_from_slice(&vp)?, host.vec_new_from_slice(&vs)?)?;
        let mut rng = StdRng::from_seed([0xff; 32]);
        let mut shuffle_with_order = |v1: &mut Vec<Val>, v2: &mut Vec<Val>| {
            use rand::seq::SliceRandom;
            assert_eq!(v1.len(), v2.len());
            let mut indices: Vec<usize> = (0..v1.len()).collect();
            indices.shuffle(&mut rng);
            let v1_shuffled: Vec<Val> = indices.iter().map(|&i| v1[i]).collect();
            let v2_shuffled: Vec<Val> = indices.iter().map(|&i| v2[i]).collect();
            *v1 = v1_shuffled;
            *v2 = v2_shuffled;
        };

        for _ in 0..10 {
            host.budget_ref().reset_default()?;
            shuffle_with_order(&mut vp, &mut vs);
            let vp_obj = host.vec_new_from_slice(&vp)?;
            let vs_obj = host.vec_new_from_slice(&vs)?;
            let res = host.bls12_381_g2_msm(vp_obj, vs_obj)?;
            assert_eq!(
                host.obj_cmp(res.into(), ref_res.into())?,
                Ordering::Equal as i64
            );
        }
    }
    // 8. msm result is same as invidial mul and add
    {
        host.budget_ref().reset_default()?;
        let vp = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 5, &mut rng)?;
        let vs = sample_fr_vec(&host, 5, &mut rng)?;
        let ref_res = host.bls12_381_g2_msm(vp, vs)?;
        let mut res = g2_zero(&host)?;
        for i in 0..5 {
            let p: BytesObject = host.vec_get(vp, U32Val::from(i))?.try_into()?;
            let s: U256Val = host.vec_get(vs, U32Val::from(i))?.try_into()?;
            let rhs = host.bls12_381_g2_mul(p, s)?;
            res = host.bls12_381_g2_add(res, rhs)?;
        }
        assert_eq!(
            host.obj_cmp(res.into(), ref_res.into())?,
            Ordering::Equal as i64
        );
    }
    Ok(())
}

#[test]
fn map_fp2_to_g2() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // invalid fp2: wrong length
    {
        let p1 = invalid_fp2(&host, InvalidPointTypes::TooFewBytes, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp2_to_g2(p1),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        let p2 = invalid_fp2(&host, InvalidPointTypes::TooManyBytes, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp2_to_g2(p2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        let p3 = invalid_fp2(&host, InvalidPointTypes::OutOfRange, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_map_fp2_to_g2(p3),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // Test cases from https://datatracker.ietf.org/doc/html/rfc9380#name-bls12381g2_xmdsha-256_sswu_
    // To interpret the results, understand the steps it takes to hash a msg to curve
    //   1. u = hash_to_field(msg, 2)
    //   2. Q0 = map_to_curve(u[0])
    //   3. Q1 = map_to_curve(u[1])
    //   4. R = Q0 + Q1 # Point addition
    //   5. P = clear_cofactor(R)
    //   6. return P
    {
        host.budget_ref().reset_default()?;
        let test_map_fp2_to_curve_inner = |u: String, q: Point| -> Result<(), HostError> {
            let g2 = parse_g2_point_test_case(&host, q)?;
            let mut u_bytes = [0u8; 96];
            let uu: Vec<_> = u.split(',').collect();
            u_bytes[0..48].copy_from_slice(&parse_hex(uu[1]));
            u_bytes[48..96].copy_from_slice(&parse_hex(uu[0]));
            let fp2 = host.bytes_new_from_slice(&u_bytes)?;
            let res = host.bls12_381_map_fp2_to_g2(fp2)?;
            assert_eq!(host.obj_cmp(res.into(), g2.into())?, Ordering::Equal as i64);
            Ok(())
        };

        let test_suite: HashToCurveTestSuite = serde_json::from_slice(
            &std::fs::read("./src/test/data/BLS12381G2_XMD_SHA-256_SSWU_RO_.json").unwrap(),
        )
        .unwrap();
        for case in test_suite.vectors {
            let [u0, u1] = case.u;
            test_map_fp2_to_curve_inner(u0, case.Q0)?;
            test_map_fp2_to_curve_inner(u1, case.Q1)?;
        }
    }
    Ok(())
}

#[test]
fn hash_to_g2() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // 1. invalid input dst length = 0
    {
        let dst = host.bytes_new_from_slice(&[])?;
        let msg = host.bytes_new_from_slice("some message".as_bytes())?;
        assert!(HostError::result_matches_err(
            host.bls12_381_hash_to_g2(msg, dst),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 2. invalid input dst length > 255
    {
        let dst = host.bytes_new_from_slice(&[0; 256])?;
        let msg = host.bytes_new_from_slice("some message".as_bytes())?;
        assert!(HostError::result_matches_err(
            host.bls12_381_hash_to_g2(msg, dst),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 3. test vectors from https://datatracker.ietf.org/doc/html/rfc9380#name-bls12381g2_xmdsha-256_sswu_
    {
        let test_suite: HashToCurveTestSuite = serde_json::from_slice(
            &std::fs::read("./src/test/data/BLS12381G2_XMD_SHA-256_SSWU_RO_.json").unwrap(),
        )
        .unwrap();
        let dst = host.bytes_new_from_slice(test_suite.dst.as_bytes())?;
        for case in test_suite.vectors {
            let msg = host.bytes_new_from_slice(case.msg.as_bytes())?;
            let g2 = host.bls12_381_hash_to_g2(msg, dst)?;
            let g2_ref = parse_g2_point_test_case(&host, case.P)?;
            assert_eq!(
                host.obj_cmp(g2.into(), g2_ref.into())?,
                Ordering::Equal as i64
            );
        }
    }
    Ok(())
}

// pairing checks
#[test]
fn pairing() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // 1. vector lengths don't match
    {
        let vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        let vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 2, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 2. vector length is 0
    {
        let vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 0, &mut rng)?;
        let vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 0, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 3. any g1 is invalid
    {
        let mut vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        vp1 = host.vec_put(
            vp1,
            U32Val::from(1),
            sample_g1_not_in_subgroup(&host, &mut rng)?.to_val(),
        )?;
        let vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 2, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bls12_381_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 4. any g2 is invalid
    {
        let vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        let mut vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 3, &mut rng)?;
        vp2 = host.vec_put(
            vp2,
            U32Val::from(1),
            sample_g2_not_on_curve(&host, &mut rng)?.to_val(),
        )?;
        assert!(HostError::result_matches_err(
            host.bls12_381_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    // 5. e(P, Q+R) = e(P, Q)*e(P, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = neg_g1(p, &host)?;
        let q = sample_g2(&host, &mut rng)?;
        let r = sample_g2(&host, &mut rng)?;
        let q_plus_r = host.bls12_381_g2_add(q, r)?;

        //check e(-P, Q+R)*e(P, Q)*e(P, R) == 1
        let g1_vec = host.vec_new_from_slice(&[neg_p.to_val(), p.to_val(), p.to_val()])?;
        let g2_vec = host.vec_new_from_slice(&[q_plus_r.to_val(), q.to_val(), r.to_val()])?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }
    // 6. e(P+S, R) = e(P, R)*e(S, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let s = sample_g1(&host, &mut rng)?;
        let r = sample_g2(&host, &mut rng)?;
        let neg_r = neg_g2(r, &host)?;
        let p_plus_s = host.bls12_381_g1_add(p, s)?;
        // check e(P+S, -R) * e(P, R)*e(S, R) == 1
        let g1_vec = host.vec_new_from_slice(&[p_plus_s.to_val(), p.to_val(), s.to_val()])?;
        let g2_vec = host.vec_new_from_slice(&[neg_r.to_val(), r.to_val(), r.to_val()])?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 7. e([a]P, [b]Q) = e([b]P, [a]Q) = e([ab]P, Q)= e(P, [ab]Q)
    {
        host.budget_ref().reset_default()?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = neg_g1(p, &host)?;
        let q = sample_g2(&host, &mut rng)?;
        let neg_q = neg_g2(q, &host)?;
        let a_p = host.bls12_381_g1_mul(p, a)?;
        let b_p = host.bls12_381_g1_mul(p, b)?;
        let a_q = host.bls12_381_g2_mul(q, a)?;
        let b_q = host.bls12_381_g2_mul(q, b)?;
        let ab = host.bls12_381_fr_mul(a, b)?;
        let ab_p = host.bls12_381_g1_mul(p, ab)?;
        let ab_q = host.bls12_381_g2_mul(q, ab)?;
        // check e([a]P, [b]Q) * e([b]P, [a]Q) * e([ab]P, -Q) * e(-P, [ab]Q) == 1
        let g1_vec =
            host.vec_new_from_slice(&[a_p.to_val(), b_p.to_val(), ab_p.to_val(), neg_p.to_val()])?;
        let g2_vec =
            host.vec_new_from_slice(&[b_q.to_val(), a_q.to_val(), neg_q.to_val(), ab_q.to_val()])?;
        let res = host.bls12_381_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }
    // 8. any of g1 point is infinity
    {
        host.budget_ref().reset_default()?;
        let mut vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        vp1 = host.vec_put(vp1, U32Val::from(1), g1_zero(&host)?.to_val())?;
        let vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 3, &mut rng)?;
        assert!(host.bls12_381_multi_pairing_check(vp1, vp2).is_ok());
    }
    // 9. any of g2 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = sample_host_vec::<G1_SERIALIZED_SIZE, G1Affine>(&host, 3, &mut rng)?;
        let mut vp2 = sample_host_vec::<G2_SERIALIZED_SIZE, G2Affine>(&host, 3, &mut rng)?;
        vp2 = host.vec_put(vp2, U32Val::from(2), g2_zero(&host)?.to_val())?;
        assert!(host.bls12_381_multi_pairing_check(vp1, vp2).is_ok());
    }
    // 10. entire vector is zero
    {
        host.budget_ref().reset_default()?;
        let vp1 = zero_g1_vec(&host, 5)?;
        let vp2 = zero_g2_vec(&host, 5)?;
        assert!(host.bls12_381_multi_pairing_check(vp1, vp2).is_ok());
    }
    Ok(())
}

// fr arithmetics

// serialization roundtrip
#[test]
fn test_serialization_roundtrip() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // g1
    {
        let g1_roundtrip_check = |g1: &G1Affine, subgroup_check: bool| -> Result<bool, HostError> {
            let bo = host.g1_affine_serialize_uncompressed(&g1)?;
            let g1_back = host.g1_affine_deserialize_from_bytesobj(bo, subgroup_check)?;
            Ok(g1.eq(&g1_back))
        };
        assert!(g1_roundtrip_check(&G1Affine::zero(), true)?);
        assert!(g1_roundtrip_check(&G1Affine::generator(), true)?);
        for _ in 0..20 {
            // on curve and in subgroup
            let g1 = G1Affine::rand(&mut rng);
            assert!(g1_roundtrip_check(&g1, true)?)
        }
        for i in 0..10 {
            // on curve and not in subgroup
            let g1 = G1Affine::get_point_from_x_unchecked(Fq::rand(&mut rng), (i % 2) != 0)
                .unwrap_or(G1Affine::zero());
            assert!(g1_roundtrip_check(&g1, false)?);
            if !g1.is_in_correct_subgroup_assuming_on_curve() {
                assert!(HostError::result_matches_err(
                    g1_roundtrip_check(&g1, true),
                    (ScErrorType::Crypto, ScErrorCode::InvalidInput)
                ));
            }
        }
        for _ in 0..10 {
            // not on curve
            let g1 = G1Affine::new_unchecked(Fq::rand(&mut rng), Fq::rand(&mut rng));
            if g1.is_on_curve() {
                continue;
            }
            assert!(HostError::result_matches_err(
                g1_roundtrip_check(&g1, false),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }
    // g2
    {
        let g2_roundtrip_check = |g2: &G2Affine, subgroup_check: bool| -> Result<bool, HostError> {
            let bo = host.g2_affine_serialize_uncompressed(&g2)?;
            let g2_back = host.g2_affine_deserialize_from_bytesobj(bo, subgroup_check)?;
            Ok(g2.eq(&g2_back))
        };
        assert!(g2_roundtrip_check(&G2Affine::zero(), true)?);
        assert!(g2_roundtrip_check(&G2Affine::generator(), true)?);
        for _ in 0..20 {
            // on curve and in subgroup
            let g2 = G2Affine::rand(&mut rng);
            assert!(g2_roundtrip_check(&g2, true)?)
        }
        for i in 0..10 {
            // on curve and not in subgroup
            let g2 = G2Affine::get_point_from_x_unchecked(Fq2::rand(&mut rng), (i % 2) != 0)
                .unwrap_or(G2Affine::zero());
            assert!(g2_roundtrip_check(&g2, false)?);
            if !g2.is_in_correct_subgroup_assuming_on_curve() {
                assert!(HostError::result_matches_err(
                    g2_roundtrip_check(&g2, true),
                    (ScErrorType::Crypto, ScErrorCode::InvalidInput)
                ));
            }
        }
        for _ in 0..10 {
            // not on curve
            let g2 = G2Affine::new_unchecked(Fq2::rand(&mut rng), Fq2::rand(&mut rng));
            if g2.is_on_curve() {
                continue;
            }
            assert!(HostError::result_matches_err(
                g2_roundtrip_check(&g2, false),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }
    // fp
    {
        let fp_roundtrip_check = |fp: &Fq| -> Result<bool, HostError> {
            let mut buf = [0; FP_SERIALIZED_SIZE];
            host.serialize_uncompressed_into_slice::<FP_SERIALIZED_SIZE, _>(fp, &mut buf, "Fp")?;
            buf.reverse();
            let bo = host.add_host_object(host.scbytes_from_slice(&buf)?)?;
            let fp_back = host.fp_deserialize_from_bytesobj(bo)?;
            Ok(fp.eq(&fp_back))
        };
        assert!(fp_roundtrip_check(&FQ_ZERO)?);
        assert!(fp_roundtrip_check(&FQ_ONE)?);
        for _ in 0..20 {
            assert!(fp_roundtrip_check(&Fq::rand(&mut rng))?)
        }
    }
    // fp2
    {
        let fp2_roundtrip_check = |fp2: &Fq2| -> Result<bool, HostError> {
            let mut buf = [0; FP2_SERIALIZED_SIZE];
            host.serialize_uncompressed_into_slice::<FP2_SERIALIZED_SIZE, _>(fp2, &mut buf, "Fp2")?;
            buf.reverse();
            let bo = host.add_host_object(host.scbytes_from_slice(&buf)?)?;
            let fp2_back = host.fp2_deserialize_from_bytesobj(bo)?;
            Ok(fp2.eq(&fp2_back))
        };
        for _ in 0..20 {
            assert!(fp2_roundtrip_check(&Fq2::rand(&mut rng))?)
        }
    }
    // fr
    {
        let fr_roundtrip_check = |fr: Fr| -> Result<bool, HostError> {
            let uv = host.fr_to_u256val(fr.clone())?;
            let fr_back = host.fr_from_u256val(uv)?;
            Ok(fr == fr_back)
        };
        for _ in 0..20 {
            assert!(fr_roundtrip_check(Fr::rand(&mut rng))?)
        }
    }
    Ok(())
}
