use soroban_env_common::{num::*, xdr::ScVal, Compare, TryFromVal, TryIntoVal, I256};

use crate::{budget::AsBudget, Host, RawVal};
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
