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
        if g1.y <= -g1.y {
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
        if g2.y <= -g2.y {
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

    // This is a negative g1 point serialized by us, it should not set the y-sign bit
    let g2_bytes_obj = negative_g2(&host, &mut rng)?;

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
