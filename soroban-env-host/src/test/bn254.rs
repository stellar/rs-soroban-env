use crate::{
    crypto::bn254::{BN254_G1_SERIALIZED_SIZE, BN254_G2_SERIALIZED_SIZE},
    xdr::{ContractCostType, ScErrorCode, ScErrorType},
    BytesObject, Env, EnvBase, ErrorHandler, Host, HostError, U256Val, U32Val,
};
use ark_bn254::{Fq, Fq2, Fr, G1Affine, G2Affine};
use ark_ec::{AffineRepr, CurveGroup};

use std::ops::Add;

use ark_ff::{BigInteger, PrimeField, UniformRand};
use ark_serialize::CanonicalSerialize;
use core::panic;
use rand::{rngs::StdRng, SeedableRng};
use soroban_env_common::{ConversionError, TryFromVal, U256};
use std::cmp::Ordering;

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

fn sample_g1_out_of_range(host: &Host, rng: &mut StdRng) -> Result<BytesObject, HostError> {
    let g1 = sample_g1(host, rng)?;
    host.bytes_copy_from_slice(g1, U32Val::from(0), MODULUS.as_bytes())
}

fn g1_zero(host: &Host) -> Result<BytesObject, HostError> {
    host.bn254_g1_affine_serialize_uncompressed(&G1Affine::zero())
}

fn neg_g1(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g1 = host.bn254_g1_affine_deserialize_from_bytesobj(bo)?;
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

fn neg_g2(bo: BytesObject, host: &Host) -> Result<BytesObject, HostError> {
    let g2 = host.bn254_affine_deserialize::<BN254_G2_SERIALIZED_SIZE, _>(
        bo,
        ContractCostType::Bn254G2CheckPointOnCurve,
        true,
        "G2",
    )?;
    bn254_g2_affine_serialize_uncompressed(host, &-g2)
}

fn g2_zero(host: &Host) -> Result<BytesObject, HostError> {
    bn254_g2_affine_serialize_uncompressed(host, &G2Affine::zero())
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
        let neg_p = neg_g1(p, &host)?;
        let q = G2Affine::rand(&mut rng);
        let r = G2Affine::rand(&mut rng);
        let q_plus_r = bn254_g2_affine_serialize_uncompressed(&host, &q.add(&r).into_affine())?;

        let q_bytes = bn254_g2_affine_serialize_uncompressed(&host, &q)?;
        let r_bytes = bn254_g2_affine_serialize_uncompressed(&host, &r)?;

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
        let neg_r = neg_g2(r, &host)?;
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
        let neg_p = neg_g1(p, &host)?;
        let q_affine = G2Affine::rand(&mut rng);
        let neg_q = bn254_g2_affine_serialize_uncompressed(&host, &-q_affine)?;

        // Use bn254_g1_mul for G1 scalar multiplications
        let a_p = host.bn254_g1_mul(p, a)?;
        let b_p = host.bn254_g1_mul(p, b)?;

        // Compute G2 scalar multiplications and ab using ark
        let a_q = bn254_g2_affine_serialize_uncompressed(
            &host,
            &(q_affine * host.bn254_fr_from_u256val(a)?).into(),
        )?;
        let b_q = bn254_g2_affine_serialize_uncompressed(
            &host,
            &(q_affine * host.bn254_fr_from_u256val(b)?).into(),
        )?;
        let ab_fr = host.bn254_fr_from_u256val(a)? * host.bn254_fr_from_u256val(b)?;
        let ab_p = host.bn254_g1_mul(p, fr_to_u256val(&host, ab_fr)?)?;
        let ab_q = bn254_g2_affine_serialize_uncompressed(&host, &(q_affine * ab_fr).into())?;

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
            let g1_back = host.bn254_g1_affine_deserialize_from_bytesobj(bo)?;
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
        let g2_roundtrip_check = |g2: &G2Affine, subgroup_check: bool| -> Result<bool, HostError> {
            let bo = bn254_g2_affine_serialize_uncompressed(&host, &g2)?;
            let g2_back = host.bn254_affine_deserialize::<BN254_G2_SERIALIZED_SIZE, _>(
                bo,
                ContractCostType::Bn254G2CheckPointOnCurve,
                subgroup_check,
                "G2",
            )?;
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
