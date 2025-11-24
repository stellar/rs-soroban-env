use crate::{
    crypto::bn254::{BN254_G1_SERIALIZED_SIZE, BN254_G2_SERIALIZED_SIZE},
    xdr::{ScErrorCode, ScErrorType},
    BytesObject, Compare, Env, EnvBase, ErrorHandler, Host, HostError, U256Val, U32Val,
    {ConversionError, TryFromVal, U256},
};
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G2Affine};
use ark_ec::{AffineRepr, CurveGroup};
use ark_ff::{BigInteger, PrimeField, UniformRand};
use ark_serialize::CanonicalSerialize;
use core::panic;
use rand::{rngs::StdRng, SeedableRng};
use std::{cmp::Ordering, ops::Add};

const MODULUS: &str = "0x2523648240000001BA344D80000000086121000000000013A700000000000013";

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

fn negative_g1(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let g1 = G1Affine::rand(rng);
        if g1.y().is_none() {
            // infinity
            continue;
        }
        if g1.y > -g1.y {
            // check that the Y-sign bit is indeed set in arkworks by retrieving
            // the MSB (the last byte in little-endian serialized buffer) and
            // asserting on the flag bit
            let mut buf = [0u8; BN254_G1_SERIALIZED_SIZE];
            g1.serialize_uncompressed(&mut buf[..]).unwrap(); // this is little-endian
            assert!(buf[BN254_G1_SERIALIZED_SIZE - 1] & 0b1000_0000 != 0u8);
            // we get our point
            return host.bn254_g1_affine_serialize_uncompressed(&g1);
        }
    }
}

fn sample_g1_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g1 = sample_g1(host, rng)?;
    host.bytes_copy_from_slice(g1, U32Val::from(0), MODULUS.as_bytes())
}

fn g1_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::zero())
}

fn minus_g1(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g1 = host.bn254_g1_affine_deserialize(bo)?;
    host.bn254_g1_affine_serialize_uncompressed(&-g1)
}

fn sample_g2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    host.bn254_g2_affine_serialize_uncompressed(&G2Affine::rand(rng))
}

fn sample_g2_not_on_curve(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        let y = Fq2::rand(rng);
        let p = G2Affine::new_unchecked(x, y);
        if !p.is_on_curve() {
            return host.bn254_g2_affine_serialize_uncompressed(&p);
        }
    }
}

fn sample_g2_not_in_subgroup(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let x = Fq2::rand(rng);
        if let Some(p) = G2Affine::get_point_from_x_unchecked(x, true) {
            assert!(p.is_on_curve());
            if !p.is_in_correct_subgroup_assuming_on_curve() {
                return host.bn254_g2_affine_serialize_uncompressed(&p);
            }
        }
    }
}

fn sample_g2_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g2 = sample_g2(host, rng)?;
    host.bytes_copy_from_slice(g2, U32Val::from(0), MODULUS.as_bytes())
}

fn minus_g2(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g2 = host.bn254_g2_affine_deserialize(bo)?;
    host.bn254_g2_affine_serialize_uncompressed(&-g2)
}

fn g2_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g2_affine_serialize_uncompressed(&G2Affine::zero())
}

fn invalid_g2(
    host: &Host,
    ty: InvalidPointTypes,
    rng: &mut StdRng,
) -> Result<BytesObject, HostError> {
    let affine = G2Affine::rand(rng);
    assert!(!affine.is_zero());
    let bo = host.bn254_g2_affine_serialize_uncompressed(&affine)?;
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

fn negative_g2(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    loop {
        let g2 = G2Affine::rand(rng);
        if g2.y().is_none() {
            // infinity
            continue;
        }
        if g2.y > -g2.y {
            // check that the Y-sign bit is indeed set in arkworks by retrieving
            // the MSB (the last byte in little-endian serialized buffer) and
            // asserting on the flag bit
            let mut buf = [0u8; BN254_G2_SERIALIZED_SIZE];
            g2.serialize_uncompressed(&mut buf[..]).unwrap(); // this is little-endian
            assert!(buf[BN254_G2_SERIALIZED_SIZE - 1] & 0b1000_0000 != 0u8);
            // we get our point
            return host.bn254_g2_affine_serialize_uncompressed(&g2);
        }
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
fn test_bn254_g1_add() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0x5b; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // Add a compressed point - should fail
    {
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
        let neg_a = minus_g1(a.clone(), &host)?;
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

    // 3. any G1 point is invalid
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

    // 4. any G2 point is invalid
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

    // 5. e(P, Q+R) = e(P, Q)*e(P, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = minus_g1(p, &host)?;
        let q = G2Affine::rand(&mut rng);
        let r = G2Affine::rand(&mut rng);
        let q_plus_r = host.bn254_g2_affine_serialize_uncompressed(&q.add(&r).into_affine())?;

        let q_bytes = host.bn254_g2_affine_serialize_uncompressed(&q)?;
        let r_bytes = host.bn254_g2_affine_serialize_uncompressed(&r)?;

        //check e(-P, Q+R)*e(P, Q)*e(P, R) == 1
        let g1_vec = host.vec_new_from_slice(&[neg_p.to_val(), p.to_val(), p.to_val()])?;
        let g2_vec =
            host.vec_new_from_slice(&[q_plus_r.to_val(), q_bytes.to_val(), r_bytes.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 6. e(P+S, R) = e(P, R)*e(S, R)
    {
        host.budget_ref().reset_default()?;
        let p = sample_g1(&host, &mut rng)?;
        let s = sample_g1(&host, &mut rng)?;
        let r = sample_g2(&host, &mut rng)?;
        let neg_r = minus_g2(r, &host)?;
        let p_plus_s = host.bn254_g1_add(p, s)?;
        // check e(P+S, -R) * e(P, R)*e(S, R) == 1
        let g1_vec = host.vec_new_from_slice(&[p_plus_s.to_val(), p.to_val(), s.to_val()])?;
        let g2_vec = host.vec_new_from_slice(&[neg_r.to_val(), r.to_val(), r.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 7. e([a]P, [b]Q) = e([b]P, [a]Q) = e([ab]P, Q)= e(P, [ab]Q)
    {
        host.budget_ref().reset_default()?;
        let a = sample_fr(&host, &mut rng)?;
        let b = sample_fr(&host, &mut rng)?;
        let p = sample_g1(&host, &mut rng)?;
        let neg_p = minus_g1(p, &host)?;
        let q_affine = G2Affine::rand(&mut rng);
        let neg_q = host.bn254_g2_affine_serialize_uncompressed(&-q_affine)?;

        // Use bn254_g1_mul for G1 scalar multiplications
        let a_p = host.bn254_g1_mul(p, a)?;
        let b_p = host.bn254_g1_mul(p, b)?;

        // Compute G2 scalar multiplications and ab using ark
        let a_q = host.bn254_g2_affine_serialize_uncompressed(
            &(q_affine * host.bn254_fr_from_u256val(a)?).into(),
        )?;
        let b_q = host.bn254_g2_affine_serialize_uncompressed(
            &(q_affine * host.bn254_fr_from_u256val(b)?).into(),
        )?;
        let ab_fr = host.bn254_fr_from_u256val(a)? * host.bn254_fr_from_u256val(b)?;
        let ab_p = host.bn254_g1_mul(p, fr_to_u256val(&host, ab_fr)?)?;
        let ab_q = host.bn254_g2_affine_serialize_uncompressed(&(q_affine * ab_fr).into())?;

        // check e([a]P, [b]Q) * e([b]P, [a]Q) * e([ab]P, -Q) * e(-P, [ab]Q) == 1
        let g1_vec =
            host.vec_new_from_slice(&[a_p.to_val(), b_p.to_val(), ab_p.to_val(), neg_p.to_val()])?;
        let g2_vec =
            host.vec_new_from_slice(&[b_q.to_val(), a_q.to_val(), neg_q.to_val(), ab_q.to_val()])?;
        let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;
        assert!(res.as_val().is_true())
    }

    // 8. any of g1 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            g1_zero(&host)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
        ])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // 9. any of g2 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
            sample_g1(&host, &mut rng)?.to_val(),
        ])?;
        let vp2 = host.vec_new_from_slice(&[
            sample_g2(&host, &mut rng)?.to_val(),
            sample_g2(&host, &mut rng)?.to_val(),
            g2_zero(&host)?.to_val(),
        ])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // 10. entire vector is zero
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[g1_zero(&host)?.to_val(), g1_zero(&host)?.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[g2_zero(&host)?.to_val(), g2_zero(&host)?.to_val()])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    Ok(())
}

#[test]
fn test_serialization_roundtrip() -> Result<(), HostError> {
    let mut rng = StdRng::from_seed([0xff; 32]);
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    // g1
    {
        let g1_roundtrip_check = |g1: &G1Affine| -> Result<bool, HostError> {
            let bo = host.bn254_g1_affine_serialize_uncompressed(&g1)?;
            let g1_back = host.bn254_g1_affine_deserialize(bo)?;
            Ok(g1.eq(&g1_back))
        };
        assert!(g1_roundtrip_check(&G1Affine::zero())?);
        assert!(g1_roundtrip_check(&G1Affine::generator())?);
        for _ in 0..20 {
            // on curve (and always in subgroup for BN254 G1)
            let g1 = G1Affine::rand(&mut rng);
            assert!(g1_roundtrip_check(&g1)?)
        }
        for _ in 0..10 {
            // not on curve
            let g1 = G1Affine::new_unchecked(Fq::rand(&mut rng), Fq::rand(&mut rng));
            if g1.is_on_curve() {
                continue;
            }
            assert!(HostError::result_matches_err(
                g1_roundtrip_check(&g1),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }
    // g2
    {
        let g2_roundtrip_check = |g2: &G2Affine| -> Result<bool, HostError> {
            let bo = host.bn254_g2_affine_serialize_uncompressed(&g2)?;
            let g2_back = host.bn254_g2_affine_deserialize(bo)?;
            Ok(g2.eq(&g2_back))
        };
        assert!(g2_roundtrip_check(&G2Affine::zero())?);
        assert!(g2_roundtrip_check(&G2Affine::generator())?);
        for _ in 0..20 {
            // on curve and in subgroup
            let g2 = G2Affine::rand(&mut rng);
            assert!(g2_roundtrip_check(&g2)?)
        }
        for i in 0..10 {
            // on curve and not in subgroup
            let g2 = G2Affine::get_point_from_x_unchecked(Fq2::rand(&mut rng), (i % 2) != 0)
                .unwrap_or(G2Affine::zero());
            if !g2.is_in_correct_subgroup_assuming_on_curve() {
                assert!(HostError::result_matches_err(
                    g2_roundtrip_check(&g2),
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
                g2_roundtrip_check(&g2),
                (ScErrorType::Crypto, ScErrorCode::InvalidInput)
            ));
        }
    }

    // fr
    {
        let fr_roundtrip_check = |fr: Fr| -> Result<bool, HostError> {
            let uv = fr_to_u256val(&host, fr.clone())?;
            let fr_back = host.bn254_fr_from_u256val(uv)?;
            Ok(fr == fr_back)
        };
        for _ in 0..20 {
            assert!(fr_roundtrip_check(Fr::rand(&mut rng))?)
        }
    }
    Ok(())
}

#[test]
fn hardcoded_serialization() -> Result<(), HostError> {
    /*
    This is a test vector from Matteo Lisotto with 260 bytes containing:
        4 bytes as selector that can be ignored
        64 bytes is a G1 point
        128 bytes is a G2 point
        64 bytes G1 point
        They are in big endian.
     */
    let test_data = "73c457ba01698c70b2f3dd6b19a450205e703fe33be07c41caec66a6f03384788aac1c0121a63c107b0e9d223e3ccb9343ac4244d077b481d94ebf1b6e6fb6365e5818c729b12498230d06e179d6858d9ce69c1ad1cd892bcbd9a89d2df0ba329bde93971f05a1b0e7babcfb772f3f4c2088b16f45feb2aa0c58f18f78a5861fdc5cc02007e1f86e21d516b42a6ad7bad5bbcc4b8b3fbaefd56804748828cd3dece00f531cc3ea0c1d713852e2916bf95e5e883e4ebdc2085a9652967ac6c1d164761bdb1f181e7635aad462677a0d5d24d1d7757fc074d17ba32d2aa2637b71d62c426505f71b3f87954f7b27ba60325520b87701e95d457cf3bd7d663e9f1af453fb4d";

    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // Decode hex string to bytes
    let bytes = hex::decode(test_data).unwrap();

    // Skip the first 4 bytes (selector)
    let offset = 4;

    // Extract first G1 point (64 bytes)
    let g1_1_bytes = &bytes[offset..offset + BN254_G1_SERIALIZED_SIZE];
    let g1_1 = host.add_host_object(host.scbytes_from_slice(g1_1_bytes)?)?;

    // Extract G2 point (128 bytes)
    let g2_offset = offset + BN254_G1_SERIALIZED_SIZE;
    let g2_bytes = &bytes[g2_offset..g2_offset + BN254_G2_SERIALIZED_SIZE];
    let g2 = host.add_host_object(host.scbytes_from_slice(g2_bytes)?)?;

    // Extract second G1 point (64 bytes)
    let g1_2_offset = g2_offset + BN254_G2_SERIALIZED_SIZE;
    let g1_2_bytes = &bytes[g1_2_offset..g1_2_offset + BN254_G1_SERIALIZED_SIZE];
    let g1_2 = host.add_host_object(host.scbytes_from_slice(g1_2_bytes)?)?;

    {
        let double = U256Val::from_u32(2);
        let res1 = host.bn254_g1_mul(g1_1, double)?;
        let res2 = host.bn254_g1_add(g1_1, g1_1)?;

        assert_eq!(
            host.obj_cmp(res1.into(), res2.into())?,
            Ordering::Equal as i64
        );
    }

    {
        let a_plus_b = host.bn254_g1_add(g1_1, g1_2)?;
        let b_plus_a = host.bn254_g1_add(g1_2, g1_1)?;
        assert_eq!(
            host.obj_cmp(a_plus_b.into(), b_plus_a.into())?,
            Ordering::Equal as i64
        );
    }

    // any of g2 point is infinity
    {
        host.budget_ref().reset_default()?;
        let vp1 = host.vec_new_from_slice(&[g1_1.to_val(), g1_2.to_val()])?;
        let vp2 = host.vec_new_from_slice(&[g2.to_val(), g2_zero(&host)?.to_val()])?;
        assert!(host.bn254_multi_pairing_check(vp1, vp2).is_ok());
    }

    // Pass in raw infinity point
    {
        let zero = hex::decode("0".repeat(128)).unwrap();
        let g1_zero = host.add_host_object(host.scbytes_from_slice(zero.as_slice())?)?;
        let g1_plus_zero = host.bn254_g1_add(g1_1, g1_zero)?;

        assert_eq!(
            host.obj_cmp(g1_plus_zero.into(), g1_1.into())?,
            Ordering::Equal as i64
        );
    }

    Ok(())
}

// From https://www.evm.codes/precompiled
#[test]
fn g1_add_mul_hardcoded() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let expected_sum = host.test_bin_obj(&hex::decode("030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd315ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4").unwrap())?;

    let g_1_2 = "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002";

    let bytes = hex::decode(g_1_2).unwrap();
    let g1_bytes = host.test_bin_obj(&bytes)?;

    // Test g1_add: g1_bytes + g1_bytes
    let res_add = host.bn254_g1_add(g1_bytes, g1_bytes)?;

    assert_eq!(
        (*host).compare(&res_add.to_val(), &expected_sum.to_val())?,
        core::cmp::Ordering::Equal
    );

    // Test g1_mul: g1_bytes * 2
    let scalar_2 = U256Val::from_u32(2);
    let res_mul = host.bn254_g1_mul(g1_bytes, scalar_2)?;

    assert_eq!(
        (*host).compare(&res_mul.to_val(), &expected_sum.to_val())?,
        core::cmp::Ordering::Equal
    );

    Ok(())
}

// From https://www.evm.codes/precompiled
#[test]
fn g1_pairing_hardcoded() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // From Solidity pairing example
    // First pairing: e(G1_1, G2_1)
    let g1_1_x = "2cf44499d5d27bb186308b7af7af02ac5bc9eeb6a3d147c186b21fb1b76e18da";
    let g1_1_y = "2c0f001f52110ccfe69108924926e45f0b0c868df0e7bde1fe16d3242dc715f6";
    let g1_1_bytes = hex::decode(format!("{}{}", g1_1_x, g1_1_y)).unwrap();
    let g1_1 = host.test_bin_obj(&g1_1_bytes)?;

    let g2_1_x1 = "1fb19bb476f6b9e44e2a32234da8212f61cd63919354bc06aef31e3cfaff3ebc";
    let g2_1_x0 = "22606845ff186793914e03e21df544c34ffe2f2f3504de8a79d9159eca2d98d9";
    let g2_1_y1 = "2bd368e28381e8eccb5fa81fc26cf3f048eea9abfdd85d7ed3ab3698d63e4f90";
    let g2_1_y0 = "2fe02e47887507adf0ff1743cbac6ba291e66f59be6bd763950bb16041a0a85e";
    let g2_1_bytes = hex::decode(format!("{}{}{}{}", g2_1_x1, g2_1_x0, g2_1_y1, g2_1_y0)).unwrap();
    let g2_1 = host.test_bin_obj(&g2_1_bytes)?;

    // Second pairing: e(G1_2, G2_2)
    let g1_2_x = "0000000000000000000000000000000000000000000000000000000000000001";
    let g1_2_y = "30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd45";
    let g1_2_bytes = hex::decode(format!("{}{}", g1_2_x, g1_2_y)).unwrap();
    let g1_2 = host.test_bin_obj(&g1_2_bytes)?;

    let g2_2_x1 = "1971ff0471b09fa93caaf13cbf443c1aede09cc4328f5a62aad45f40ec133eb4";
    let g2_2_x0 = "091058a3141822985733cbdddfed0fd8d6c104e9e9eff40bf5abfef9ab163bc7";
    let g2_2_y1 = "2a23af9a5ce2ba2796c1f4e453a370eb0af8c212d9dc9acd8fc02c2e907baea2";
    let g2_2_y0 = "23a8eb0b0996252cb548a4487da97b02422ebc0e834613f954de6c7e0afdc1fc";
    let g2_2_bytes = hex::decode(format!("{}{}{}{}", g2_2_x1, g2_2_x0, g2_2_y1, g2_2_y0)).unwrap();
    let g2_2 = host.test_bin_obj(&g2_2_bytes)?;

    // Test multi pairing check: e(G1_1, G2_1) * e(G1_2, G2_2) == 1
    host.budget_ref().reset_default()?;
    let g1_vec = host.vec_new_from_slice(&[g1_1.to_val(), g1_2.to_val()])?;
    let g2_vec = host.vec_new_from_slice(&[g2_1.to_val(), g2_2.to_val()])?;
    let res = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;

    assert!(res.as_val().is_true());

    Ok(())
}

// ============================================================================
// Tests for flag bit handling in serialization/deserialization
// ============================================================================

#[test]
fn test_bn254_g1_deserialize_rejects_y_sign_bit() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    let mut rng = StdRng::from_seed([0u8; 32]);

    for _ in 0..10 {
        // This is a negative g1 point serialized by us, it should not set the y-sign bit
        let g1_bytes_obj = negative_g1(&host, &mut rng)?;

        // Get the bytes and verify the y-sign bit is NOT set
        let mut g1_bytes = vec![0u8; BN254_G1_SERIALIZED_SIZE];
        host.bytes_copy_to_slice(g1_bytes_obj, U32Val::from(0), &mut g1_bytes)?;
        assert_eq!(
            g1_bytes[0] & 0b1000_0000,
            0,
            "Y-sign bit should not be set even for negative y"
        );

        // Now manually set the y-sign bit (bit 0, which is 0x80 in the MSB)
        g1_bytes[0] |= 0b1000_0000;

        // Try to deserialize - should fail
        let g1_bytes_obj_modified = host.test_bin_obj(&g1_bytes)?;
        let result = host.bn254_g1_affine_deserialize(g1_bytes_obj_modified);

        assert!(HostError::result_matches_err(
            result,
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    Ok(())
}

#[test]
fn test_bn254_g1_deserialize_rejects_infinity_bit() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // This is an infinity (zero) g1 point serialized by us, it should not set the infinity bit
    let g1_bytes_obj = g1_zero(&host)?;

    // Get the bytes and verify the infinity bit is NOT set
    let mut g1_bytes = vec![0u8; BN254_G1_SERIALIZED_SIZE];
    host.bytes_copy_to_slice(g1_bytes_obj, U32Val::from(0), &mut g1_bytes)?;
    assert_eq!(
        g1_bytes[0] & 0b0100_0000,
        0,
        "Infinity bit should not be set even for zero/infinity points"
    );

    // Now manually set the infinity bit (bit 1, which is 0x40 in the MSB)
    g1_bytes[0] |= 0b0100_0000;

    // Try to deserialize - should fail
    let g1_bytes_obj_modified = host.test_bin_obj(&g1_bytes)?;
    let result = host.bn254_g1_affine_deserialize(g1_bytes_obj_modified);

    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    Ok(())
}

#[test]
fn test_bn254_g2_deserialize_rejects_y_sign_bit() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;
    let mut rng = StdRng::from_seed([0u8; 32]);

    // This is a negative G1 point serialized by us, it should not set the y-sign bit
    let g2_bytes_obj = negative_g2(&host, &mut rng)?;

    for _ in 0..10 {
        // Get the bytes and verify the y-sign bit is NOT set
        let mut g2_bytes = vec![0u8; BN254_G2_SERIALIZED_SIZE];
        host.bytes_copy_to_slice(g2_bytes_obj, U32Val::from(0), &mut g2_bytes)?;
        assert_eq!(
            g2_bytes[0] & 0b1000_0000,
            0,
            "Y-sign bit should not be set even for negative y"
        );

        // Now manually set the y-sign bit (bit 0, which is 0x80 in the MSB)
        g2_bytes[0] |= 0b1000_0000;

        // Try to deserialize - should fail
        let g2_bytes_obj_modified = host.test_bin_obj(&g2_bytes)?;
        let result = host.bn254_g2_affine_deserialize(g2_bytes_obj_modified);

        assert!(HostError::result_matches_err(
            result,
            (ScErrorType::Crypto, ScErrorCode::InvalidInput)
        ));
    }
    Ok(())
}

#[test]
fn test_bn254_g2_deserialize_rejects_infinity_bit() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    // This is an infinity (zero) g2 point serialized by us, it should not set the infinity bit
    let g2_bytes_obj = g2_zero(&host)?;

    // Get the bytes and verify the infinity bit is NOT set
    let mut g2_bytes = vec![0u8; BN254_G2_SERIALIZED_SIZE];
    host.bytes_copy_to_slice(g2_bytes_obj, U32Val::from(0), &mut g2_bytes)?;
    assert_eq!(
        g2_bytes[0] & 0b0100_0000,
        0,
        "Infinity bit should not be set even for zero/infinity points"
    );

    // Now manually set the infinity bit (bit 1, which is 0x40 in the MSB)
    g2_bytes[0] |= 0b0100_0000;

    // Try to deserialize - should fail
    let g2_bytes_obj_modified = host.test_bin_obj(&g2_bytes)?;
    let result = host.bn254_g2_affine_deserialize(g2_bytes_obj_modified);

    assert!(HostError::result_matches_err(
        result,
        (ScErrorType::Crypto, ScErrorCode::InvalidInput)
    ));

    Ok(())
}

// Helper functions for Ethereum test vectors
fn parse_ethereum_g1_add_input(input: &str) -> (Vec<u8>, Vec<u8>) {
    let bytes = hex::decode(input).unwrap();
    assert_eq!(bytes.len(), 128); // Two G1 points (64 bytes each)

    let g1_1 = bytes[0..64].to_vec();
    let g1_2 = bytes[64..128].to_vec();
    (g1_1, g1_2)
}

fn parse_ethereum_g1_mul_input(input: &str) -> (Vec<u8>, Vec<u8>) {
    let bytes = hex::decode(input).unwrap();
    assert_eq!(bytes.len(), 96); // G1 point (64 bytes) + scalar (32 bytes)

    let g1 = bytes[0..64].to_vec();
    let scalar = bytes[64..96].to_vec();
    (g1, scalar)
}

fn parse_ethereum_pairing_input(input: &str) -> (Vec<Vec<u8>>, Vec<Vec<u8>>) {
    let bytes = hex::decode(input).unwrap();
    assert_eq!(bytes.len() % 192, 0); // Each pair is 192 bytes

    let num_pairs = bytes.len() / 192;
    let mut g1_points = Vec::new();
    let mut g2_points = Vec::new();

    for i in 0..num_pairs {
        let offset = i * 192;
        g1_points.push(bytes[offset..offset + 64].to_vec());
        g2_points.push(bytes[offset + 64..offset + 192].to_vec());
    }
    (g1_points, g2_points)
}

fn scalar_from_bytes(host: &Host, scalar_bytes: &[u8]) -> Result<U256Val, HostError> {
    assert_eq!(scalar_bytes.len(), 32);
    let bo = host.test_bin_obj(scalar_bytes)?;
    host.u256_val_from_be_bytes(bo)
}

// Ethereum test vectors from https://github.com/ethereum/go-ethereum/blob/master/core/vm/contracts_test.go
#[test]
fn ethereum_bn254_add_tests() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let test_cases = vec![
        ("chfast1", "18b18acfb4c2c30276db5411368e7185b311dd124691610c5d3b74034e093dc9063c909c4720840cb5134cb9f59fa749755796819658d32efc0d288198f3726607c2b7f58a84bd6145f00c9c2bc0bb1a187f20ff2c92963a88019e7c6a014eed06614e20c147e940f2d70da3f74c9a17df361706a4485c742bd6788478fa17d7", "2243525c5efd4b9c3d3c45ac0ca3fe4dd85e830a4ce6b65fa1eeaee202839703301d1d33be6da8e509df21cc35964723180eed7532537db9ae5e7d48f195c915"),
        ("chfast2", "2243525c5efd4b9c3d3c45ac0ca3fe4dd85e830a4ce6b65fa1eeaee202839703301d1d33be6da8e509df21cc35964723180eed7532537db9ae5e7d48f195c91518b18acfb4c2c30276db5411368e7185b311dd124691610c5d3b74034e093dc9063c909c4720840cb5134cb9f59fa749755796819658d32efc0d288198f37266", "2bd3e6d0f3b142924f5ca7b49ce5b9d54c4703d7ae5648e61d02268b1a0a9fb721611ce0a6af85915e2f1d70300909ce2e49dfad4a4619c8390cae66cefdb204"),
        ("cdetrio1", "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
        ("cdetrio6", "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002", "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002"),
        ("cdetrio9", "0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000", "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002"),
        ("cdetrio11", "0000000000000000000000000000000000000000000000000000000000000001000000000000000000000000000000000000000000000000000000000000000200000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002", "030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd315ed738c0e0a7c92e7845f96b2ae9c0a68a6a449e3538fc7ff3ebf7a5a18a2c4"),
        ("cdetrio13", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d98", "15bf2bb17880144b5d1cd2b1f46eff9d617bffd1ca57c37fb5a49bd84e53cf66049c797f9ce0d17083deb32b5e36f2ea2a212ee036598dd7624c168993d1355f"),
    ];

    for (name, input, expected) in test_cases {
        let (g1_1_bytes, g1_2_bytes) = parse_ethereum_g1_add_input(input);
        let g1_1 = host.test_bin_obj(&g1_1_bytes)?;
        let g1_2 = host.test_bin_obj(&g1_2_bytes)?;

        let result = host.bn254_g1_add(g1_1, g1_2)?;

        let expected_bytes = hex::decode(expected).unwrap();
        let expected_bo = host.test_bin_obj(&expected_bytes)?;

        assert_eq!(
            host.obj_cmp(result.into(), expected_bo.into())?,
            Ordering::Equal as i64,
            "Test {} failed",
            name
        );
    }

    Ok(())
}

#[test]
fn ethereum_bn254_mul_tests() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let test_cases = vec![
        ("chfast1", "2bd3e6d0f3b142924f5ca7b49ce5b9d54c4703d7ae5648e61d02268b1a0a9fb721611ce0a6af85915e2f1d70300909ce2e49dfad4a4619c8390cae66cefdb20400000000000000000000000000000000000000000000000011138ce750fa15c2", "070a8d6a982153cae4be29d434e8faef8a47b274a053f5a4ee2a6c9c13c31e5c031b8ce914eba3a9ffb989f9cdd5b0f01943074bf4f0f315690ec3cec6981afc"),
        ("chfast2", "070a8d6a982153cae4be29d434e8faef8a47b274a053f5a4ee2a6c9c13c31e5c031b8ce914eba3a9ffb989f9cdd5b0f01943074bf4f0f315690ec3cec6981afc30644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd46", "025a6f4181d2b4ea8b724290ffb40156eb0adb514c688556eb79cdea0752c2bb2eff3f31dea215f1eb86023a133a996eb6300b44da664d64251d05381bb8a02e"),
        ("chfast3", "025a6f4181d2b4ea8b724290ffb40156eb0adb514c688556eb79cdea0752c2bb2eff3f31dea215f1eb86023a133a996eb6300b44da664d64251d05381bb8a02e183227397098d014dc2822db40c0ac2ecbc0b548b438e5469e10460b6c3e7ea3", "14789d0d4a730b354403b5fac948113739e276c23e0258d8596ee72f9cd9d3230af18a63153e0ec25ff9f2951dd3fa90ed0197bfef6e2a1a62b5095b9d2b4a27"),
        ("cdetrio1", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f6ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "2cde5879ba6f13c0b5aa4ef627f159a3347df9722efce88a9afbb20b763b4c411aa7e43076f6aee272755a7f9b84832e71559ba0d2e0b17d5f9f01755e5b0d11"),
        ("cdetrio2", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f630644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000000", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe3163511ddc1c3f25d396745388200081287b3fd1472d8339d5fecb2eae0830451"),
        ("cdetrio3", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f60000000000000000000000000000000100000000000000000000000000000000", "1051acb0700ec6d42a88215852d582efbaef31529b6fcbc3277b5c1b300f5cf0135b2394bb45ab04b8bd7611bd2dfe1de6a4e6e2ccea1ea1955f577cd66af85b"),
        ("cdetrio4", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f60000000000000000000000000000000000000000000000000000000000000009", "1dbad7d39dbc56379f78fac1bca147dc8e66de1b9d183c7b167351bfe0aeab742cd757d51289cd8dbd0acf9e673ad67d0f0a89f912af47ed1be53664f5692575"),
        ("cdetrio5", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f60000000000000000000000000000000000000000000000000000000000000001", "1a87b0584ce92f4593d161480614f2989035225609f08058ccfa3d0f940febe31a2f3c951f6dadcc7ee9007dff81504b0fcd6d7cf59996efdc33d92bf7f9f8f6"),
        ("cdetrio6", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7cffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "29e587aadd7c06722aabba753017c093f70ba7eb1f1c0104ec0564e7e3e21f6022b1143f6a41008e7755c71c3d00b6b915d386de21783ef590486d8afa8453b1"),
        ("cdetrio7", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c30644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000000", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa92e83f8d734803fc370eba25ed1f6b8768bd6d83887b87165fc2434fe11a830cb"),
        ("cdetrio8", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c0000000000000000000000000000000100000000000000000000000000000000", "221a3577763877920d0d14a91cd59b9479f83b87a653bb41f82a3f6f120cea7c2752c7f64cdd7f0e494bff7b60419f242210f2026ed2ec70f89f78a4c56a1f15"),
        ("cdetrio9", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c0000000000000000000000000000000000000000000000000000000000000009", "228e687a379ba154554040f8821f4e41ee2be287c201aa9c3bc02c9dd12f1e691e0fd6ee672d04cfd924ed8fdc7ba5f2d06c53c1edc30f65f2af5a5b97f0a76a"),
        ("cdetrio10", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c0000000000000000000000000000000000000000000000000000000000000001", "17c139df0efee0f766bc0204762b774362e4ded88953a39ce849a8a7fa163fa901e0559bacb160664764a357af8a9fe70baa9258e0b959273ffc5718c6d4cc7c"),
        ("cdetrio11", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d98ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff", "00a1a234d08efaa2616607e31eca1980128b00b415c845ff25bba3afcb81dc00242077290ed33906aeb8e42fd98c41bcb9057ba03421af3f2d08cfc441186024"),
        ("cdetrio12", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d9830644e72e131a029b85045b68181585d2833e84879b9709143e1f593f0000000", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b8692929ee761a352600f54921df9bf472e66217e7bb0cee9032e00acc86b3c8bfaf"),
        ("cdetrio13", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d980000000000000000000000000000000100000000000000000000000000000000", "1071b63011e8c222c5a771dfa03c2e11aac9666dd097f2c620852c3951a4376a2f46fe2f73e1cf310a168d56baa5575a8319389d7bfa6b29ee2d908305791434"),
        ("cdetrio14", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d980000000000000000000000000000000000000000000000000000000000000009", "19f75b9dd68c080a688774a6213f131e3052bd353a304a189d7a2ee367e3c2582612f545fb9fc89fde80fd81c68fc7dcb27fea5fc124eeda69433cf5c46d2d7f"),
        ("cdetrio15", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d980000000000000000000000000000000000000000000000000000000000000001", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d98"),
        ("zeroScalar", "039730ea8dff1254c0fee9c0ea777d29a9c710b7e616683f194f18c43b43b869073a5ffcc6fc7a28c30723d6e58ce577356982d65b833a5a5c15bf9024b43d980000000000000000000000000000000000000000000000000000000000000000", "00000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"),
    ];

    for (name, input, expected) in test_cases {
        let (g1_bytes, scalar_bytes) = parse_ethereum_g1_mul_input(input);
        let g1 = host.test_bin_obj(&g1_bytes)?;
        let scalar = scalar_from_bytes(&host, &scalar_bytes)?;

        let result = host.bn254_g1_mul(g1, scalar)?;

        let expected_bytes = hex::decode(expected).unwrap();
        let expected_bo = host.test_bin_obj(&expected_bytes)?;

        assert_eq!(
            host.obj_cmp(result.into(), expected_bo.into())?,
            Ordering::Equal as i64,
            "Test {} failed",
            name
        );
    }

    Ok(())
}

#[test]
fn ethereum_bn254_pairing_tests() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    host.enable_debug()?;

    let test_cases = vec![
        ("jeff1", "1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f593034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf704bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a416782bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c2032c61a830e3c17286de9462bf242fca2883585b93870a73853face6a6bf411198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("jeff2", "2eca0c7238bf16e83e7a1e6c5d49540685ff51380f309842a98561558019fc0203d3260361bb8451de5ff5ecd17f010ff22f5c31cdf184e9020b06fa5997db841213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f06967a1237ebfeca9aaae0d6d0bab8e28c198c5a339ef8a2407e31cdac516db922160fa257a5fd5b280642ff47b65eca77e626cb685c84fa6d3b6882a283ddd1198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("jeff3", "0f25929bcb43d5a57391564615c9e70a992b10eafa4db109709649cf48c50dd216da2f5cb6be7a0aa72c440c53c9bbdfec6c36c7d515536431b3a865468acbba2e89718ad33c8bed92e210e81d1853435399a271913a6520736a4729cf0d51eb01a9e2ffa2e92599b68e44de5bcf354fa2642bd4f26b259daa6f7ce3ed57aeb314a9a87b789a58af499b314e13c3d65bede56c07ea2d418d6874857b70763713178fb49a2d6cd347dc58973ff49613a20757d0fcc22079f9abd10c3baee245901b9e027bd5cfc2cb5db82d4dc9677ac795ec500ecd47deee3b5da006d6d049b811d7511c78158de484232fc68daf8a45cf217d1c2fae693ff5871e8752d73b21198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("jeff4", "2f2ea0b3da1e8ef11914acf8b2e1b32d99df51f5f4f206fc6b947eae860eddb6068134ddb33dc888ef446b648d72338684d678d2eb2371c61a50734d78da4b7225f83c8b6ab9de74e7da488ef02645c5a16a6652c3c71a15dc37fe3a5dcb7cb122acdedd6308e3bb230d226d16a105295f523a8a02bfc5e8bd2da135ac4c245d065bbad92e7c4e31bf3757f1fe7362a63fbfee50e7dc68da116e67d600d9bf6806d302580dc0661002994e7cd3a7f224e7ddc27802777486bf80f40e4ca3cfdb186bac5188a98c45e6016873d107f5cd131f3a3e339d0375e58bd6219347b008122ae2b09e539e152ec5364e7e2204b03d11d3caa038bfc7cd499f8176aacbee1f39e4e4afc4bc74790a4a028aff2c3d2538731fb755edefd8cb48d6ea589b5e283f150794b6736f670d6a1033f9b46c6f5204f50813eb85c8dc4b59db1c5d39140d97ee4d2b36d99bc49974d18ecca3e7ad51011956051b464d9e27d46cc25e0764bb98575bd466d32db7b15f582b2d5c452b36aa394b789366e5e3ca5aabd415794ab061441e51d01e94640b7e3084a07e02c78cf3103c542bc5b298669f211b88da1679b0b64a63b7e0e7bfe52aae524f73a55be7fe70c7e9bfc94b4cf0da1213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("jeff5", "20a754d2071d4d53903e3b31a7e98ad6882d58aec240ef981fdf0a9d22c5926a29c853fcea789887315916bbeb89ca37edb355b4f980c9a12a94f30deeed30211213d2149b006137fcfb23036606f848d638d576a120ca981b5b1a5f9300b3ee2276cf730cf493cd95d64677bbb75fc42db72513a4c1e387b476d056f80aa75f21ee6226d31426322afcda621464d0611d226783262e21bb3bc86b537e986237096df1f82dff337dd5972e32a8ad43e28a78a96a823ef1cd4debe12b6552ea5f1abb4a25eb9379ae96c84fff9f0540abcfc0a0d11aeda02d4f37e4baf74cb0c11073b3ff2cdbb38755f8691ea59e9606696b3ff278acfc098fa8226470d03869217cee0a9ad79a4493b5253e2e4e3a39fc2df38419f230d341f60cb064a0ac290a3d76f140db8418ba512272381446eb73958670f00cf46f1d9e64cba057b53c26f64a8ec70387a13e41430ed3ee4a7db2059cc5fc13c067194bcc0cb49a98552fd72bd9edb657346127da132e5b82ab908f5816c826acb499e22f2412d1a2d70f25929bcb43d5a57391564615c9e70a992b10eafa4db109709649cf48c50dd2198a1f162a73261f112401aa2db79c7dab1533c9935c77290a6ce3b191f2318d198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("jeff6", "1c76476f4def4bb94541d57ebba1193381ffa7aa76ada664dd31c16024c43f593034dd2920f673e204fee2811c678745fc819b55d3e9d294e45c9b03a76aef41209dd15ebff5d46c4bd888e51a93cf99a7329636c63514396b4a452003a35bf704bf11ca01483bfa8b34b43561848d28905960114c8ac04049af4b6315a416782bb8324af6cfc93537a2ad1a445cfd0ca2a71acd7ac41fadbf933c2a51be344d120a2a4cf30c1bf9845f20c6fe39e07ea2cce61f0c9bb048165fe5e4de877550111e129f1cf1097710d41c4ac70fcdfa5ba2023c6ff1cbeac322de49d1b6df7c103188585e2364128fe25c70558f1560f4f9350baf3959e603cc91486e110936198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000000"),
        ("two_point_match_2", "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("two_point_match_3", "105456a333e6d636854f987ea7bb713dfd0ae8371a72aea313ae0c32c0bf10160cf031d41b41557f3e7e3ba0c51bebe5da8e6ecd855ec50fc87efcdeac168bcc0476be093a6d2b4bbf907172049874af11e1b6267606e00804d3ff0037ec57fd3010c68cb50161b7d1d96bb71edfec9880171954e56871abf3d93cc94d745fa114c059d74e5b6c4ec14ae5864ebe23a71781d86c29fb8fb6cce94f70d3de7a2101b33461f39d9e887dbb100f170a2345dde3c07e256d1dfa2b657ba5cd030427000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000021a2c3013d2ea92e13c800cde68ef56a294b883f6ac35d25f587c09b1b3c635f7290158a80cd3d66530f74dc94c94adb88f5cdb481acca997b6e60071f08a115f2f997f3dbd66a7afe07fe7862ce239edba9e05c5afff7f8a1259c9733b2dfbb929d1691530ca701b4a106054688728c9972c8512e9789e9567aae23e302ccd75", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("two_point_match_4", "105456a333e6d636854f987ea7bb713dfd0ae8371a72aea313ae0c32c0bf10160cf031d41b41557f3e7e3ba0c51bebe5da8e6ecd855ec50fc87efcdeac168bcc0476be093a6d2b4bbf907172049874af11e1b6267606e00804d3ff0037ec57fd3010c68cb50161b7d1d96bb71edfec9880171954e56871abf3d93cc94d745fa114c059d74e5b6c4ec14ae5864ebe23a71781d86c29fb8fb6cce94f70d3de7a2101b33461f39d9e887dbb100f170a2345dde3c07e256d1dfa2b657ba5cd030427000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000021a2c3013d2ea92e13c800cde68ef56a294b883f6ac35d25f587c09b1b3c635f7290158a80cd3d66530f74dc94c94adb88f5cdb481acca997b6e60071f08a115f2f997f3dbd66a7afe07fe7862ce239edba9e05c5afff7f8a1259c9733b2dfbb929d1691530ca701b4a106054688728c9972c8512e9789e9567aae23e302ccd75", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("ten_point_match_1", "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed275dc4a288d1afb3cbb1ac09187524c7db36395df7be3b99e673b13a075a65ec1d9befcd05a5323e6da4d435f3b617cdb3af83285c2df711ef39c01571827f9d", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("ten_point_match_2", "00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa00000000000000000000000000000000000000000000000000000000000000010000000000000000000000000000000000000000000000000000000000000002203e205db4f19b37b60121b83a7333706db86431c6d835849957ed8c3928ad7927dc7234fd11d3e8c36c59277c3e6f149d5cd3cfa9a62aee49f8130962b4b3b9195e8aa5b7827463722b8c153931579d3505566b4edf48d498e185f0509de15204bb53b8977e5f92a0bc372742c4830944a59b4fe6b1c0466e2a6dad122b5d2e030644e72e131a029b85045b68181585d97816a916871ca8d3c208c16d87cfd31a76dae6d3272396d0cbe61fced2bc532edac647851e3ac53ce1cc9c7e645a83198e9393920d483a7260bfb731fb5d25f1aa493335a9e71297e485b7aef312c21800deef121f1e76426a00665e5c4479674322d4f75edadd46debd5cd992f6ed090689d0585ff075ec9e99ad690c3395bc4b313370b38ef355acdadcd122975b12c85ea5db8c6deb4aab71808dcb408fe3d1e7690c43d37b4ce6cc0166fa7daa", "0000000000000000000000000000000000000000000000000000000000000001"),
        ("ten_point_match_3", "105456a333e6d636854f987ea7bb713dfd0ae8371a72aea313ae0c32c0bf10160cf031d41b41557f3e7e3ba0c51bebe5da8e6ecd855ec50fc87efcdeac168bcc0476be093a6d2b4bbf907172049874af11e1b6267606e00804d3ff0037ec57fd3010c68cb50161b7d1d96bb71edfec9880171954e56871abf3d93cc94d745fa114c059d74e5b6c4ec14ae5864ebe23a71781d86c29fb8fb6cce94f70d3de7a2101b33461f39d9e887dbb100f170a2345dde3c07e256d1dfa2b657ba5cd030427000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000000000000000000000000000000000000000021a2c3013d2ea92e13c800cde68ef56a294b883f6ac35d25f587c09b1b3c635f7290158a80cd3d66530f74dc94c94adb88f5cdb481acca997b6e60071f08a115f2f997f3dbd66a7afe07fe7862ce239edba9e05c5afff7f8a1259c9733b2dfbb929d1691530ca701b4a106054688728c9972c8512e9789e9567aae23e302ccd75", "0000000000000000000000000000000000000000000000000000000000000001"),
    ];

    for (name, input, expected) in test_cases {
        let (g1_points, g2_points) = parse_ethereum_pairing_input(input);

        host.budget_ref().reset_default()?;

        let g1_vals: Result<Vec<_>, _> = g1_points
            .iter()
            .map(|bytes| host.test_bin_obj(bytes).map(|bo| bo.to_val()))
            .collect();
        let g1_vec = host.vec_new_from_slice(&g1_vals?)?;

        let g2_vals: Result<Vec<_>, _> = g2_points
            .iter()
            .map(|bytes| host.test_bin_obj(bytes).map(|bo| bo.to_val()))
            .collect();
        let g2_vec = host.vec_new_from_slice(&g2_vals?)?;

        let result = host.bn254_multi_pairing_check(g1_vec, g2_vec)?;

        // Ethereum returns 0x01 for true, 0x00 for false
        let expected_bytes = hex::decode(expected).unwrap();
        let expected_result = expected_bytes[31] == 1;

        assert_eq!(
            result.as_val().is_true(),
            expected_result,
            "Test {} failed",
            name
        );
    }

    Ok(())
}
