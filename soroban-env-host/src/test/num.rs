use soroban_env_common::{
    num::*,
    xdr::{ScErrorCode, ScErrorType, ScVal},
    Compare, Env, EnvBase, Object, TryFromVal, TryIntoVal, I256,
};

use crate::{budget::AsBudget, host_object::HostObjectType, Host, HostError, RawVal};
use core::fmt::Debug;
use std::cmp::Ordering;

fn check_roundtrip_ok<
    T: Eq + Debug + Copy + TryIntoVal<Host, RawVal> + TryFromVal<Host, RawVal>,
>(
    h: &Host,
    input: T,
    expect_object: bool,
) {
    let raw1: RawVal = input.try_into_val(h).unwrap();
    let sc1: ScVal = raw1.try_into_val(h).unwrap();
    let raw2: RawVal = sc1.try_into_val(h).unwrap();
    let output: T = raw2.try_into_val(h).unwrap();
    assert_eq!(input, output);
    assert_eq!(h.compare(&raw1, &raw2).unwrap(), Ordering::Equal);
    if expect_object {
        assert!(raw1.is_object());
        assert!(raw2.is_object());
    } else {
        assert!(!raw1.is_object());
        assert!(!raw2.is_object());
        assert_eq!(raw1.get_payload(), raw2.get_payload());
    }
}

fn check_roundtrip_compare_ok<
    T: Eq + Debug + Copy + Ord + TryIntoVal<Host, RawVal> + TryFromVal<Host, RawVal>,
>(
    h: &Host,
    input_vec: Vec<T>,
) {
    let pairs: Vec<((RawVal, ScVal), &T)> = input_vec
        .iter()
        .map(|v| {
            let raw: RawVal = v.try_into_val(h).unwrap();
            let scv: ScVal = raw.try_into_val(h).unwrap();
            (raw, scv)
        })
        .zip(input_vec.iter())
        .collect();

    for first in &pairs {
        for second in &pairs {
            let ord = first.1.cmp(second.1);
            assert_eq!(h.compare(&first.0 .0, &second.0 .0).unwrap(), ord);
            assert_eq!(
                h.as_budget().compare(&first.0 .1, &second.0 .1).unwrap(),
                ord
            );
        }
    }
}

#[test]
fn test_num_scval_roundtrips() {
    let host = Host::default();

    check_roundtrip_ok::<i64>(&host, 0_i64, false);
    check_roundtrip_ok::<i64>(&host, 1_i64, false);
    check_roundtrip_ok::<i64>(&host, -1_i64, false);
    check_roundtrip_ok::<i64>(&host, 12345_i64, false);
    check_roundtrip_ok::<i64>(&host, -12345_i64, false);
    check_roundtrip_ok::<i64>(&host, MIN_SMALL_I64, false);
    check_roundtrip_ok::<i64>(&host, MAX_SMALL_I64, false);
    check_roundtrip_ok::<i64>(&host, MIN_SMALL_I64 - 1, true);
    check_roundtrip_ok::<i64>(&host, MAX_SMALL_I64 + 1, true);
    check_roundtrip_ok::<i64>(&host, i64::MIN, true);
    check_roundtrip_ok::<i64>(&host, i64::MAX, true);

    check_roundtrip_ok::<u64>(&host, 0_u64, false);
    check_roundtrip_ok::<u64>(&host, 1_u64, false);
    check_roundtrip_ok::<u64>(&host, 12345_u64, false);
    check_roundtrip_ok::<u64>(&host, MAX_SMALL_U64, false);
    check_roundtrip_ok::<u64>(&host, MAX_SMALL_U64 + 1, true);
    check_roundtrip_ok::<u64>(&host, u64::MAX, true);

    check_roundtrip_ok::<i128>(&host, 0_i128, false);
    check_roundtrip_ok::<i128>(&host, 1_i128, false);
    check_roundtrip_ok::<i128>(&host, -1_i128, false);
    check_roundtrip_ok::<i128>(&host, 12345_i128, false);
    check_roundtrip_ok::<i128>(&host, -12345_i128, false);
    check_roundtrip_ok::<i128>(&host, MIN_SMALL_I128, false);
    check_roundtrip_ok::<i128>(&host, MAX_SMALL_I128, false);
    check_roundtrip_ok::<i128>(&host, MIN_SMALL_I128 - 1, true);
    check_roundtrip_ok::<i128>(&host, MAX_SMALL_I128 + 1, true);
    check_roundtrip_ok::<i128>(&host, i128::MIN, true);
    check_roundtrip_ok::<i128>(&host, i128::MAX, true);

    check_roundtrip_ok::<u128>(&host, 0_u128, false);
    check_roundtrip_ok::<u128>(&host, 1_u128, false);
    check_roundtrip_ok::<u128>(&host, 12345_u128, false);
    check_roundtrip_ok::<u128>(&host, MAX_SMALL_U128, false);
    check_roundtrip_ok::<u128>(&host, MAX_SMALL_U128 + 1, true);
    check_roundtrip_ok::<u128>(&host, u128::MAX, true);

    check_roundtrip_ok::<U256>(&host, U256::from(0_u128), false);
    check_roundtrip_ok::<U256>(&host, U256::from(1_u128), false);
    check_roundtrip_ok::<U256>(&host, U256::from(12345_u128), false);
    check_roundtrip_ok::<U256>(&host, MAX_SMALL_U256, false);
    check_roundtrip_ok::<U256>(&host, MAX_SMALL_U256 + 1, true);
    check_roundtrip_ok::<U256>(&host, U256::MAX, true);

    check_roundtrip_ok::<I256>(&host, I256::from(0_i128), false);
    check_roundtrip_ok::<I256>(&host, I256::from(1_i128), false);
    check_roundtrip_ok::<I256>(&host, I256::from(-1_i128), false);
    check_roundtrip_ok::<I256>(&host, I256::from(12345_i128), false);
    check_roundtrip_ok::<I256>(&host, I256::from(-12345_i128), false);
    check_roundtrip_ok::<I256>(&host, MIN_SMALL_I256, false);
    check_roundtrip_ok::<I256>(&host, MAX_SMALL_I256, false);
    check_roundtrip_ok::<I256>(&host, MIN_SMALL_I256 - 1, true);
    check_roundtrip_ok::<I256>(&host, MAX_SMALL_I256 + 1, true);
    check_roundtrip_ok::<I256>(&host, I256::MIN, true);
    check_roundtrip_ok::<I256>(&host, I256::MAX, true);

    let pos = 0x00ab_cdef_9876_5432u64; // some positive int64
    let neg = 0xfedc_ba98_7654_3210u64; // some negative int64
    let v = vec![pos, neg];
    for hi_hi in &v {
        for hi_lo in &v {
            for lo_hi in &v {
                for lo_lo in &v {
                    let i = i256_from_pieces(*hi_hi as i64, *hi_lo, *lo_hi, *lo_lo);
                    check_roundtrip_ok::<I256>(&host, i, true)
                }
            }
        }
    }
}

#[test]
fn test_num_rawval_scval_roundtrip_ordering() {
    let host = Host::default();
    let input_vec = vec![
        0_i128,
        1_i128,
        -1_i128,
        12345_i128,
        -12345_i128,
        MIN_SMALL_I128,
        MAX_SMALL_I128,
        MIN_SMALL_I128 - 1,
        MAX_SMALL_I128 + 1,
        i128::MIN,
        i128::MAX,
    ];
    check_roundtrip_compare_ok::<i128>(&host, input_vec);

    let input_vec = vec![
        0_u128,
        1_u128,
        12345_u128,
        MAX_SMALL_U128,
        MAX_SMALL_U128 + 1,
        u128::MAX,
    ];
    check_roundtrip_compare_ok::<u128>(&host, input_vec);

    let input_vec = vec![
        U256::from(0_u128),
        U256::from(1_u128),
        U256::from(12345_u128),
        MAX_SMALL_U256,
        MAX_SMALL_U256 + 1,
        U256::MAX,
    ];
    check_roundtrip_compare_ok::<U256>(&host, input_vec);

    let mut input_vec = vec![
        I256::from(0_i128),
        I256::from(1_i128),
        I256::from(-1_i128),
        I256::from(12345_i128),
        I256::from(-12345_i128),
        MIN_SMALL_I256,
        MAX_SMALL_I256,
        MIN_SMALL_I256 - 1,
        MAX_SMALL_I256 + 1,
        I256::MIN,
        I256::MAX,
    ];

    let pos = 0x00ab_cdef_9876_5432u64; // some positive int64
    let neg = 0xfedc_ba98_7654_3210u64; // some negative int64
    let v = vec![pos, neg];
    for hi_hi in &v {
        for hi_lo in &v {
            for lo_hi in &v {
                for lo_lo in &v {
                    input_vec.push(i256_from_pieces(*hi_hi as i64, *hi_lo, *lo_hi, *lo_lo));
                }
            }
        }
    }
    check_roundtrip_compare_ok::<I256>(&host, input_vec);
}

fn check_num_arith_ok<T, F>(host: &Host, lhs: T, rhs: T, f: F, expected: T) -> Result<(), HostError>
where
    T: HostObjectType,
    F: FnOnce(
        &Host,
        <T as HostObjectType>::Wrapper,
        <T as HostObjectType>::Wrapper,
    ) -> Result<<T as HostObjectType>::Wrapper, HostError>,
{
    let res: Object = host.add_host_object(expected)?.into();
    let res_back: Object = f(host, host.add_host_object(lhs)?, host.add_host_object(rhs)?)?.into();
    assert_eq!(
        host.compare(res.as_raw(), res_back.as_raw()).unwrap(),
        Ordering::Equal
    );
    Ok(())
}

fn check_num_arith_rhs_u32_ok<T, F>(
    host: &Host,
    lhs: T,
    rhs: u32,
    f: F,
    expected: T,
) -> Result<(), HostError>
where
    T: HostObjectType,
    F: FnOnce(
        &Host,
        <T as HostObjectType>::Wrapper,
        U32Val,
    ) -> Result<<T as HostObjectType>::Wrapper, HostError>,
{
    let res: Object = host.add_host_object(expected)?.into();
    let res_back: Object = f(host, host.add_host_object(lhs)?, U32Val::from(rhs))?.into();
    assert_eq!(
        host.compare(res.as_raw(), res_back.as_raw()).unwrap(),
        Ordering::Equal
    );
    Ok(())
}

fn check_num_arith_expect_err<T, F>(host: &Host, lhs: T, rhs: T, f: F) -> Result<(), HostError>
where
    T: HostObjectType,
    F: FnOnce(
        &Host,
        <T as HostObjectType>::Wrapper,
        <T as HostObjectType>::Wrapper,
    ) -> Result<<T as HostObjectType>::Wrapper, HostError>,
{
    let res_back = f(host, host.add_host_object(lhs)?, host.add_host_object(rhs)?);
    let code = (ScErrorType::Object, ScErrorCode::ArithDomain);
    assert!(HostError::result_matches_err(res_back, code));
    Ok(())
}

fn check_num_arith_rhs_u32_expect_err<T, F>(
    host: &Host,
    lhs: T,
    rhs: u32,
    f: F,
) -> Result<(), HostError>
where
    T: HostObjectType,
    F: FnOnce(
        &Host,
        <T as HostObjectType>::Wrapper,
        U32Val,
    ) -> Result<<T as HostObjectType>::Wrapper, HostError>,
{
    let res_back = f(host, host.add_host_object(lhs)?, U32Val::from(rhs));
    let code = (ScErrorType::Object, ScErrorCode::ArithDomain);
    assert!(HostError::result_matches_err(res_back, code));
    Ok(())
}

#[test]
fn test_u256_arith() -> Result<(), HostError> {
    let host = Host::default();
    // add
    check_num_arith_ok(
        &host,
        U256::MAX - 2,
        U256::new(1),
        Host::u256_add,
        U256::MAX - 1,
    )?;
    check_num_arith_expect_err(&host, U256::MAX - 2, U256::new(3), Host::u256_add)?;

    // sub
    check_num_arith_ok(
        &host,
        U256::new(1),
        U256::new(1),
        Host::u256_sub,
        U256::ZERO,
    )?;
    check_num_arith_expect_err(&host, U256::ZERO, U256::new(1), Host::u256_sub)?;

    // mul
    check_num_arith_ok(
        &host,
        U256::new(5),
        U256::new(1),
        Host::u256_mul,
        U256::new(5),
    )?;
    check_num_arith_expect_err(&host, U256::MAX, U256::new(2), Host::u256_mul)?;

    // div
    check_num_arith_ok(
        &host,
        U256::new(128),
        U256::new(2),
        Host::u256_div,
        U256::new(64),
    )?;
    check_num_arith_expect_err(&host, U256::new(1), U256::ZERO, Host::u256_div)?;

    // pow
    check_num_arith_rhs_u32_ok(&host, U256::new(2), 5, Host::u256_pow, U256::new(32))?;
    check_num_arith_rhs_u32_expect_err(&host, U256::MAX, 2, Host::u256_pow)?;

    // shl
    check_num_arith_rhs_u32_ok(&host, U256::new(0x1), 4, Host::u256_shl, U256::new(0x10))?;
    check_num_arith_rhs_u32_expect_err(&host, U256::new(0x10), 257, Host::u256_shl)?;

    // shr
    check_num_arith_rhs_u32_ok(&host, U256::new(0x10), 4, Host::u256_shr, U256::new(0x1))?;
    check_num_arith_rhs_u32_expect_err(&host, U256::new(0x10), 257, Host::u256_shr)?;

    Ok(())
}

#[test]
fn test_i256_arith() -> Result<(), HostError> {
    let host = Host::default();
    // add
    check_num_arith_ok(
        &host,
        I256::MAX - 2,
        I256::new(1),
        Host::i256_add,
        I256::MAX - 1,
    )?;
    check_num_arith_expect_err(&host, I256::MAX - 2, I256::new(3), Host::i256_add)?;

    // sub
    check_num_arith_ok(
        &host,
        I256::MIN + 2,
        I256::new(1),
        Host::i256_sub,
        I256::MIN + 1,
    )?;
    check_num_arith_expect_err(&host, I256::MIN + 2, I256::new(3), Host::i256_sub)?;

    // mul
    check_num_arith_ok(&host, I256::MAX, I256::new(1), Host::i256_mul, I256::MAX)?;
    check_num_arith_expect_err(&host, I256::MAX, I256::new(2), Host::i256_mul)?;

    // div
    check_num_arith_ok(
        &host,
        I256::MIN + 1,
        I256::new(-1),
        Host::i256_div,
        I256::MAX,
    )?;
    check_num_arith_expect_err(&host, I256::MIN, I256::new(-1), Host::i256_div)?;
    check_num_arith_expect_err(&host, I256::new(1), I256::new(0), Host::i256_div)?;

    // pow
    check_num_arith_rhs_u32_ok(&host, I256::new(8), 2, Host::i256_pow, I256::new(64))?;
    check_num_arith_rhs_u32_expect_err(&host, I256::MAX, 2, Host::i256_pow)?;

    // shl
    check_num_arith_rhs_u32_ok(&host, I256::new(0x1), 4, Host::i256_shl, I256::new(0x10))?;
    check_num_arith_rhs_u32_expect_err(&host, I256::new(0x1), 257, Host::i256_shl)?;

    // shr
    check_num_arith_rhs_u32_ok(&host, I256::new(0x10), 4, Host::i256_shr, I256::new(0x1))?;
    check_num_arith_rhs_u32_expect_err(&host, I256::new(0x10), 256, Host::i256_shr)?;

    Ok(())
}

#[test]
fn test_i256_bytes_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let num = I256::from_words(-4353239472894, 6576786237846);
    let bo = host.bytes_new_from_slice(num.to_be_bytes().as_slice())?;
    let obj = host.i256_obj_from_be_bytes(bo)?;
    let bo_back = host.i256_obj_to_be_bytes(obj)?;

    let mut buf = [0; 32];
    host.bytes_copy_to_slice(bo_back, U32Val::from(0), buf.as_mut_slice())?;
    let num_back = I256::from_be_bytes(buf);
    assert_eq!(num, num_back);
    Ok(())
}

#[test]
fn test_u256_bytes_roundtrip() -> Result<(), HostError> {
    let host = Host::default();
    let num = U256::from_words(4353239472894, 6576786237846);
    let bo = host.bytes_new_from_slice(num.to_be_bytes().as_slice())?;
    let obj = host.u256_obj_from_be_bytes(bo)?;
    let bo_back = host.u256_obj_to_be_bytes(obj)?;

    let mut buf = [0; 32];
    host.bytes_copy_to_slice(bo_back, U32Val::from(0), buf.as_mut_slice())?;
    let num_back = U256::from_be_bytes(buf);
    assert_eq!(num, num_back);
    Ok(())
}
