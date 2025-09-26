use crate::{
    crypto::bn254::{BN254_G1_SERIALIZED_SIZE, BN254_G2_SERIALIZED_SIZE},
    xdr::{ScErrorCode, ScErrorType},
    BytesObject, Env, EnvBase, ErrorHandler, Host, HostError, U256Val, U32Val,
};
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G2Affine};
use ark_ec::AffineRepr;
use ark_ff::{BigInteger, PrimeField, UniformRand};
use ark_serialize::CanonicalSerialize;
use core::panic;
use rand::{rngs::StdRng, SeedableRng};
use soroban_env_common::{ConversionError, TryFromVal, U256};
use std::cmp::Ordering;

const MODULUS: &str = "0x2523648240000001BA344D80000000086121000000000013A700000000000013";

/*
TODO:
1. Test G2 zero?
2. Test G2 neg?
*/

enum InvalidPointTypes {
    TooManyBytes,
    TooFewBytes,
    PointNotOnCurve,
    PointNotInSubgroup,
    OutOfRange,
}

fn sample_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::rand(rng))
}

fn sample_g1_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq::rand(rng);
        let y = Fq::rand(rng);
        let p = G1Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.bn254_g1_affine_serialize_uncompressed(&p);
        }
    }
}

fn invalid_g1(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G1Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.bn254_g1_affine_serialize_uncompressed(&affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(
                bo,
                U32Val::from(BN254_G1_SERIALIZED_SIZE as u32),
                U32Val::from(0),
            )
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(BN254_G1_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g1_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => {
            panic!("BN254 G1 points on curve are always in subgroup")
        }
        InvalidPointTypes::OutOfRange => sample_g1_out_of_range(host, rng),
    }
}

fn compressed_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let mut buf: Vec<u8> = Vec::with_capacity(BN254_G1_SERIALIZED_SIZE / 2);
    let g1 = G1Affine::rand(rng);
    g1.serialize_compressed(&mut buf).unwrap();
    host.add_host_object(host.scbytes_from_slice(&buf)?)
}

fn sample_g1_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g1 = sample_g1(host, rng)?;
    host.bytes_copy_from_slice(g1, U32Val::from(0), MODULUS.as_bytes())
}

fn g1_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::zero())
}

#[allow(dead_code)]
fn g1_generator(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::generator())
}

fn neg_g1(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g1 = host.bn254_g1_affine_deserialize_from_bytesobj(bo, true)?;
    host.bn254_g1_affine_serialize_uncompressed(&-g1)
}

fn bn254_g2_affine_serialize_uncompressed(
    host: &Host,
    g2: &G2Affine,
) -> Result<BytesObject, HostError> {
    let mut buf: Vec<u8> = Vec::with_capacity(BN254_G2_SERIALIZED_SIZE);
    g2.serialize_uncompressed(&mut buf).unwrap();
    host.add_host_object(host.scbytes_from_slice(&buf)?)
}

fn sample_g2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    bn254_g2_affine_serialize_uncompressed(host, &G2Affine::rand(rng))
}

fn sample_g2_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        let y = Fq2::rand(rng);
        let p = G2Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return bn254_g2_affine_serialize_uncompressed(host, &p);
        }
    }
}

fn sample_g2_not_in_subgroup(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        if let Some(p) = G2Affine::get_point_from_x_unchecked(x, true) {
            assert!(p.is_on_curve());
            if !p.is_in_correct_subgroup_assuming_on_curve() {
                return bn254_g2_affine_serialize_uncompressed(host, &p);
            }
        }
    }
}

fn sample_g2_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g2 = sample_g2(host, rng)?;
    host.bytes_copy_from_slice(g2, U32Val::from(0), MODULUS.as_bytes())
}

#[allow(dead_code)]
fn g2_zero(host: &Host) -> Result<BytesObject, HostError> {
    bn254_g2_affine_serialize_uncompressed(host, &G2Affine::zero())
}

#[allow(dead_code)]
fn g2_generator(host: &Host) -> Result<BytesObject, HostError> {
    bn254_g2_affine_serialize_uncompressed(host, &G2Affine::generator())
}

fn invalid_g2(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G2Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = bn254_g2_affine_serialize_uncompressed(host, &affine)?;
    match ty {
        InvalidPointTypes::TooManyBytes => {
            // insert an empty byte to the end
            host.bytes_insert(
                bo,
                U32Val::from(BN254_G2_SERIALIZED_SIZE as u32),
                U32Val::from(0),
            )
        }
        InvalidPointTypes::TooFewBytes => {
            // delete the last byte
            host.bytes_del(bo, U32Val::from(BN254_G2_SERIALIZED_SIZE as u32 - 1))
        }
        InvalidPointTypes::PointNotOnCurve => sample_g2_not_on_curve(host, rng),
        InvalidPointTypes::PointNotInSubgroup => sample_g2_not_in_subgroup(host, rng),
        InvalidPointTypes::OutOfRange => sample_g2_out_of_range(host, rng),
    }
}

fn fr_to_u256val(host: &Host, fr: Fr) -> Result<U256Val, HostError> {
    let bytes: [u8; 32] = fr
        .into_bigint()
        .to_bytes_be()
        .try_into()
        .map_err(|_| HostError::from(ConversionError))?;
    let u = U256::from_be_bytes(bytes);
    host.map_err(U256Val::try_from_val(host, &u))
}

fn sample_fr(host: &Host, rng: &mut StdRng) -> Result<U256Val, HostError> {
    fr_to_u256val(host, Fr::rand(rng))
}

#[test]
fn test_bn254_serialization_roundtrip() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5b; 32]);
    let host = observe_host!(Host::test_host());

    let g1 = G1Affine::rand(&mut rng);
    let bytes = host.bn254_g1_affine_serialize_uncompressed(&g1)?;
    let g1_deser = host.bn254_g1_affine_deserialize_from_bytesobj(bytes, true)?;
    assert_eq!(g1, g1_deser);

    let g2 = G2Affine::rand(&mut rng);
    let bytes = bn254_g2_affine_serialize_uncompressed(&host, &g2)?;
    let g2_deser =
        host.bn254_affine_deserialize::<BN254_G2_SERIALIZED_SIZE, _>(bytes, true, "G2")?;
    assert_eq!(g2, g2_deser);
    Ok(())
}

#[test]
fn test_bn254_g1_add() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5b; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    {
        // Add a compressed point - should fail
        let p1 = compressed_g1(&host, &mut rng)?;
        let p2 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(p1, p2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // invalid p1 - test only length validation (which BN254 supports)
    {
        let p2 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                invalid_g1(&host, InvalidPointTypes::OutOfRange, &mut rng)?,
                p2
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // invalid p2
    {
        let p1 = sample_g1(&host, &mut rng)?;
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooManyBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::TooFewBytes, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
        assert!(HostError::result_matches_err(
            host.bn254_g1_add(
                p1,
                invalid_g1(&host, InvalidPointTypes::OutOfRange, &mut rng)?
            ),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 3. lhs.add(zero) = lhs
    {
        let p1 = sample_g1(&host, &mut rng)?;
        let res = host.bn254_g1_add(p1, g1_zero(&host)?)?;
        assert_eq!(host.obj_cmp(p1.into(), res.into())?, Ordering::Equal as i64);
    }

    // 4. zero.add(rhs) = rhs
    {
        let p2 = sample_g1(&host, &mut rng)?;
        let res = host.bn254_g1_add(g1_zero(&host)?, p2)?;
        assert_eq!(host.obj_cmp(p2.into(), res.into())?, Ordering::Equal as i64);
    }

    // 5. commutative a + b = b + a
    {
        let a = sample_g1(&host, &mut rng)?;
        let b = sample_g1(&host, &mut rng)?;
        let a_plus_b = host.bn254_g1_add(a, b)?;
        let b_plus_a = host.bn254_g1_add(b, a)?;
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
        let aplusb = host.bn254_g1_add(a, b)?;
        let aplusb_plus_c = host.bn254_g1_add(aplusb, c)?;
        let bplusc = host.bn254_g1_add(b, c)?;
        let a_plus_bplusc = host.bn254_g1_add(a, bplusc)?;
        assert_eq!(
            host.obj_cmp(aplusb_plus_c.into(), a_plus_bplusc.into())?,
            Ordering::Equal as i64
        );
    }

    // 7. a - a = zero
    {
        let a = sample_g1(&host, &mut rng)?;
        let neg_a = neg_g1(a.clone(), &host)?;
        let res = host.bn254_g1_add(a, neg_a)?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

#[test]
fn test_bn254_g1_mul() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5c; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // 1. lhs * 0 = 0
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = host.obj_from_u256_pieces(0, 0, 0, 0)?;
        let res = host.bn254_g1_mul(lhs, rhs.into())?;
        let zero = g1_zero(&host)?;
        assert_eq!(
            host.obj_cmp(res.into(), zero.into())?,
            Ordering::Equal as i64
        );
    }

    // 2. lhs * 1 = lhs
    {
        let lhs = sample_g1(&host, &mut rng)?;
        let rhs = U256Val::from_u32(1);
        let res = host.bn254_g1_mul(lhs, rhs)?;
        assert_eq!(
            host.obj_cmp(res.into(), lhs.into())?,
            Ordering::Equal as i64
        );
    }

    // 3. associative P * a * b = P * b * a
    {
        let p = sample_g1(&host, &mut rng)?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let pa = host.bn254_g1_mul(p, a)?;
        let pab = host.bn254_g1_mul(pa, b)?;
        let pb = host.bn254_g1_mul(p, b)?;
        let pba = host.bn254_g1_mul(pb, a)?;
        assert_eq!(
            host.obj_cmp(pab.into(), pba.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

#[test]
fn test_bn254_multi_pairing_check() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // Should not error
    {
        let p = sample_g1(&host, &mut rng)?;
        let q = sample_g2(&host, &mut rng)?;

        let vp1 = host.vec_new_from_slice(&[p.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[q.to_val()])?;
        let _result1 = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    // 1. vector lengths don't match
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 2. vector length is 0
    {
        let vp1 = host.vec_new()?;
        let vp2 = host.vec_new()?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 3. any G1 point is invalid (BN254 specific validation)
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            invalid_g1(&host, InvalidPointTypes::PointNotOnCurve, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // 4. any G2 point is invalid (out of subgroup)
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            invalid_g2(&host, InvalidPointTypes::PointNotInSubgroup, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(HostError::result_matches_err(
            host.bn254_multi_pairing_check(vp1, vp2),
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }

    // Should not error
    {
        let p = sample_g1(&host, &mut rng)?;
        let q = sample_g2(&host, &mut rng)?;

        let vp1 = host.vec_new_from_slice(&[p.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[q.to_val()])?;
        let _result1 = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    //TODO: These are not good tests. Replace them!

    // 6. Test pairing with G1 scalar multiplication: e([a]P, Q) should work
    {
        let p = sample_g1(&host, &mut rng)?;
        let q = sample_g2(&host, &mut rng)?;
        let scalar = sample_fr(&host, &mut rng)?;

        let a_p = host.bn254_g1_mul(p, scalar)?;

        // Test pairing with scaled point
        let vp1 = host.vec_new_from_slice(&[a_p.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[q.to_val()])?;
        let _result = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    // 7. Test pairing with G1 addition: e(P+R, Q) should work
    {
        let p = sample_g1(&host, &mut rng)?;
        let r = sample_g1(&host, &mut rng)?;
        let q = sample_g2(&host, &mut rng)?;

        let p_plus_r = host.bn254_g1_add(p, r)?;

        // Test pairing with added points
        let vp1 = host.vec_new_from_slice(&[p_plus_r.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[q.to_val()])?;
        let _result = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    // 8. Test multi-pairing functionality
    {
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            g1_zero(&host)?.to_val(), // Test with G1 zero point
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            g2_zero(&host)?.to_val(), // Test with G2 zero point
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        let _result = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    // 9. Test with generators
    {
        let vp1 = host.vec_new_from_slice(&[g1_generator(&host)?.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[g2_generator(&host)?.to_val()])?;
        let _result = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    // 10. Test with all zero points (edge case)
    {
        let vp1 = host.vec_new_from_slice(&[g1_zero(&host)?.to_val(), g1_zero(&host)?.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[
            g2_zero(&host)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        let _result = host.bn254_multi_pairing_check(vp1, vp2)?;
    }

    Ok(())
}
