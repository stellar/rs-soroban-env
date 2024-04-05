use soroban_env_common::{
    num::*,
    xdr::{ScErrorCode, ScErrorType, ScVal},
    Compare, Env, EnvBase, TryFromVal, TryIntoVal, I256,
};

use crate::{budget::AsBudget, Host, HostError, Val};
use core::fmt::Debug;
use std::cmp::Ordering;

fn check_roundtrip_ok<T: Eq + Debug + Copy + TryIntoVal<Host, Val> + TryFromVal<Host, Val>>(
    h: &Host,
    input: T,
    expect_object: bool,
) {
    let raw1: Val = input.try_into_val(h).unwrap();
    let sc1: ScVal = raw1.try_into_val(h).unwrap();
    let raw2: Val = sc1.try_into_val(h).unwrap();
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
    T: Eq + Debug + Copy + Ord + TryIntoVal<Host, Val> + TryFromVal<Host, Val>,
>(
    h: &Host,
    input_vec: Vec<T>,
) {
    let pairs: Vec<((Val, ScVal), &T)> = input_vec
        .iter()
        .map(|v| {
            let val: Val = v.try_into_val(h).unwrap();
            let scv: ScVal = val.try_into_val(h).unwrap();
            (val, scv)
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
    let host = observe_host!(Host::test_host());

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
fn test_num_val_scval_roundtrip_ordering() {
    let host = observe_host!(Host::test_host());
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

fn check_num_arith_ok<T, V, F>(
    host: &Host,
    lhs: T,
    rhs: T,
    f: F,
    expected: T,
) -> Result<(), HostError>
where
    V: TryFromVal<Host, T> + Into<Val>,
    HostError: From<<V as TryFromVal<Host, T>>::Error>,
    F: FnOnce(&Host, V, V) -> Result<V, HostError>,
{
    let res: V = V::try_from_val(host, &expected)?;
    let lhs: V = V::try_from_val(host, &lhs)?;
    let rhs: V = V::try_from_val(host, &rhs)?;
    let res_back: V = f(host, lhs, rhs)?;
    assert_eq!(
        host.compare(&res.into(), &res_back.into()).unwrap(),
        Ordering::Equal
    );
    Ok(())
}

fn check_num_arith_rhs_u32_ok<T, V, F>(
    host: &Host,
    lhs: T,
    rhs: u32,
    f: F,
    expected: T,
) -> Result<(), HostError>
where
    V: TryFromVal<Host, T> + Into<Val>,
    HostError: From<<V as TryFromVal<Host, T>>::Error>,
    F: FnOnce(&Host, V, U32Val) -> Result<V, HostError>,
{
    let res: V = V::try_from_val(host, &expected)?;
    let lhs: V = V::try_from_val(host, &lhs)?;
    let res_back: V = f(host, lhs, U32Val::from(rhs))?;
    assert_eq!(
        host.compare(&res.into(), &res_back.into()).unwrap(),
        Ordering::Equal
    );
    Ok(())
}

fn check_num_arith_expect_err<T, V, F>(host: &Host, lhs: T, rhs: T, f: F) -> Result<(), HostError>
where
    V: TryFromVal<Host, T> + Into<Val>,
    HostError: From<<V as TryFromVal<Host, T>>::Error>,
    F: FnOnce(&Host, V, V) -> Result<V, HostError>,
{
    let lhs: V = V::try_from_val(host, &lhs)?;
    let rhs: V = V::try_from_val(host, &rhs)?;
    let res_back: Result<V, HostError> = f(host, lhs, rhs);
    let code = (ScErrorType::Object, ScErrorCode::ArithDomain);
    assert!(HostError::result_matches_err(res_back, code));
    Ok(())
}

fn check_num_arith_rhs_u32_expect_err<T, V, F>(
    host: &Host,
    lhs: T,
    rhs: u32,
    f: F,
) -> Result<(), HostError>
where
    V: TryFromVal<Host, T> + Into<Val>,
    HostError: From<<V as TryFromVal<Host, T>>::Error>,
    F: FnOnce(&Host, V, U32Val) -> Result<V, HostError>,
{
    let lhs: V = V::try_from_val(host, &lhs)?;
    let res_back: Result<V, HostError> = f(host, lhs, U32Val::from(rhs));
    let code = (ScErrorType::Object, ScErrorCode::ArithDomain);
    assert!(HostError::result_matches_err(res_back, code));
    Ok(())
}

#[test]
fn test_u256_arith() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
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

    // rem_euclid
    check_num_arith_ok(
        &host,
        U256::new(7),
        U256::new(4),
        Host::u256_rem_euclid,
        U256::new(3),
    )?;
    check_num_arith_expect_err(&host, U256::new(1), U256::ZERO, Host::u256_rem_euclid)?;

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
    let host = observe_host!(Host::test_host());
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

    // rem_euclid
    check_num_arith_ok(
        &host,
        I256::new(-7),
        I256::new(4),
        Host::i256_rem_euclid,
        I256::new(1),
    )?;
    check_num_arith_expect_err(&host, I256::new(1), I256::ZERO, Host::i256_rem_euclid)?;

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
    let host = observe_host!(Host::test_host());
    let num = I256::from_words(-4353239472894, 6576786237846);
    let bo = host.bytes_new_from_slice(num.to_be_bytes().as_slice())?;
    let val = host.i256_val_from_be_bytes(bo)?;
    let bo_back = host.i256_val_to_be_bytes(val)?;

    let mut buf = [0; 32];
    host.bytes_copy_to_slice(bo_back, U32Val::from(0), buf.as_mut_slice())?;
    let num_back = I256::from_be_bytes(buf);
    assert_eq!(num, num_back);
    Ok(())
}

#[test]
fn test_u256_bytes_roundtrip() -> Result<(), HostError> {
    let host = observe_host!(Host::test_host());
    let num = U256::from_words(4353239472894, 6576786237846);
    let bo = host.bytes_new_from_slice(num.to_be_bytes().as_slice())?;
    let val = host.u256_val_from_be_bytes(bo)?;
    let bo_back = host.u256_val_to_be_bytes(val)?;

    let mut buf = [0; 32];
    host.bytes_copy_to_slice(bo_back, U32Val::from(0), buf.as_mut_slice())?;
    let num_back = U256::from_be_bytes(buf);
    assert_eq!(num, num_back);
    Ok(())
}
