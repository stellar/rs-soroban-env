use std::cmp::Ordering;

use crate::FuzzResult;
use arbitrary::{Arbitrary, Unstructured};
use soroban_env_host::{Compare as _, Env, Host, HostError, TryIntoVal as _, Val, I256, U256};

#[derive(Arbitrary, Clone, Copy)]
enum Target {
    Unsigned(Op),
    Signed(Op),
}

#[derive(Arbitrary, Clone, Copy)]
enum Op {
    Add(Num256, Num256),
    Sub(Num256, Num256),
    Mul(Num256, Num256),
    Pow(Num256, u32),
}

#[derive(Arbitrary, Clone, Copy)]
struct Num256(u128, u128);

impl Num256 {
    fn as_u256(&self) -> U256 {
        U256::from_words(self.0, self.1)
    }

    fn as_i256(&self) -> I256 {
        I256::from_words(self.0 as _, self.1 as _)
    }
}

macro_rules! opmaster {
    ($num1:expr, $num2:expr, $as:ident, $op:ident, $opchecked:ident, $env:expr, $pow:tt) => {{
        let v1 = $num1.$as();
        let v2 = pow_select!($num2, $num2.$as(), $pow);

        let v1_val = v1.try_into_val($env).unwrap();
        let v2_val = pow_select!(v2.into(), v2.try_into_val($env).unwrap(), $pow);

        let res = $env.$op(v1_val, v2_val);
        let res_checked = $env.$opchecked(v1_val, v2_val);
        (res, res_checked)
    }};
}

macro_rules! pow_select {
    ($exp1:expr, $exp2:expr, true) => {{
        $exp1
    }};
    ($exp1:expr, $exp2:expr, false) => {{
        $exp2
    }};
}

macro_rules! op {
    ($num1:expr, $num2:expr, $as:ident, $op:ident, $opchecked:ident, $env:expr) => {{
        opmaster!($num1, $num2, $as, $op, $opchecked, $env, false)
    }};
}

macro_rules! op_pow {
    ($num1:expr, $num2:expr, $as:ident, $op:ident, $opchecked:ident, $env:expr) => {{
        opmaster!($num1, $num2, $as, $op, $opchecked, $env, true)
    }};
}

// ============================================================================
// Checked Arithmetic Fuzz Target
// ============================================================================

pub fn run_fuzz_target(data: &[u8]) -> FuzzResult {
    let mut u = Unstructured::new(data);
    let host = Host::test_host();

    let choice: Target = u.arbitrary().unwrap();
    let should_overflow = will_overflow(choice);

    match choice {
        Target::Unsigned(op) => {
            let (res, res_checked) = match op {
                Op::Add(num1, num2) => op!(num1, num2, as_u256, u256_add, u256_checked_add, &host),
                Op::Sub(num1, num2) => op!(num1, num2, as_u256, u256_sub, u256_checked_sub, &host),
                Op::Mul(num1, num2) => op!(num1, num2, as_u256, u256_mul, u256_checked_mul, &host),
                Op::Pow(num1, num2) => {
                    op_pow!(num1, num2, as_u256, u256_pow, u256_checked_pow, &host)
                }
            };
            check(&host, res, res_checked, should_overflow)
        }
        Target::Signed(op) => {
            let (res, res_checked) = match op {
                Op::Add(num1, num2) => op!(num1, num2, as_i256, i256_add, i256_checked_add, &host),
                Op::Sub(num1, num2) => op!(num1, num2, as_i256, i256_sub, i256_checked_sub, &host),
                Op::Mul(num1, num2) => op!(num1, num2, as_i256, i256_mul, i256_checked_mul, &host),
                Op::Pow(num1, num2) => {
                    op_pow!(num1, num2, as_i256, i256_pow, i256_checked_pow, &host)
                }
            };
            check(&host, res, res_checked, should_overflow)
        }
    }
}

fn check<T>(
    env: &Host,
    res: Result<T, HostError>,
    res_checked: Result<Val, HostError>,
    should_overflow: bool,
) -> FuzzResult
where
    Val: From<T>,
{
    match (should_overflow, res, res_checked) {
        (_, _, Err(err)) if !err.is_recoverable() => FuzzResult::InternalError,
        (_, Err(err), _) if !err.is_recoverable() => FuzzResult::InternalError,
        (false, Ok(val), Ok(val_checked)) if !val_checked.is_void() => {
            if env.compare(&val.into(), &val_checked).unwrap() == Ordering::Equal {
                FuzzResult::Ok
            } else {
                FuzzResult::InternalError
            }
        }
        (true, Err(_), Ok(val_checked)) if val_checked.is_void() => FuzzResult::Ok,
        _ => FuzzResult::InternalError,
    }
}

// ============================================================================
// Overflow Oracle
// ============================================================================

fn will_overflow(target: Target) -> bool {
    match target {
        Target::Unsigned(op) => match op {
            Op::Add(lhs, rhs) => u256_add_will_overflow(lhs.as_u256(), rhs.as_u256()),
            Op::Sub(lhs, rhs) => u256_sub_will_overflow(lhs.as_u256(), rhs.as_u256()),
            Op::Mul(lhs, rhs) => u256_mul_will_overflow(lhs.as_u256(), rhs.as_u256()),
            Op::Pow(base, exponent) => u256_pow_will_overflow(base.as_u256(), exponent),
        },
        Target::Signed(op) => match op {
            Op::Add(lhs, rhs) => i256_add_will_overflow(lhs.as_i256(), rhs.as_i256()),
            Op::Sub(lhs, rhs) => i256_sub_will_overflow(lhs.as_i256(), rhs.as_i256()),
            Op::Mul(lhs, rhs) => i256_mul_will_overflow(lhs.as_i256(), rhs.as_i256()),
            Op::Pow(base, exponent) => i256_pow_will_overflow(base.as_i256(), exponent),
        },
    }
}

// ============================================================================
// U256
// ============================================================================

pub fn u256_add_will_overflow(lhs: U256, rhs: U256) -> bool {
    lhs > U256::MAX - rhs
}

pub fn u256_sub_will_overflow(lhs: U256, rhs: U256) -> bool {
    lhs < rhs
}

pub fn u256_mul_will_overflow(lhs: U256, rhs: U256) -> bool {
    rhs != U256::ZERO && lhs > U256::MAX / rhs
}

pub fn u256_pow_will_overflow(mut base: U256, mut exponent: u32) -> bool {
    let mut result = U256::ONE;

    while exponent != 0 {
        if exponent & 1 == 1 {
            if u256_mul_will_overflow(result, base) {
                return true;
            }
            result *= base;
        }

        exponent >>= 1;
        if exponent != 0 {
            if u256_mul_will_overflow(base, base) {
                return true;
            }
            base *= base;
        }
    }

    false
}

// ============================================================================
// I256
// ============================================================================

pub fn i256_add_will_overflow(lhs: I256, rhs: I256) -> bool {
    (rhs > I256::ZERO && lhs > I256::MAX - rhs) || (rhs < I256::ZERO && lhs < I256::MIN - rhs)
}

pub fn i256_sub_will_overflow(lhs: I256, rhs: I256) -> bool {
    (rhs > I256::ZERO && lhs < I256::MIN + rhs) || (rhs < I256::ZERO && lhs > I256::MAX + rhs)
}

pub fn i256_mul_will_overflow(lhs: I256, rhs: I256) -> bool {
    if lhs == I256::ZERO || rhs == I256::ZERO {
        false
    } else if lhs == -I256::ONE {
        rhs == I256::MIN
    } else if rhs == -I256::ONE {
        lhs == I256::MIN
    } else if lhs > I256::ZERO {
        if rhs > I256::ZERO {
            lhs > I256::MAX / rhs
        } else {
            rhs < I256::MIN / lhs
        }
    } else if rhs > I256::ZERO {
        lhs < I256::MIN / rhs
    } else {
        lhs < I256::MAX / rhs
    }
}

pub fn i256_pow_will_overflow(mut base: I256, mut exponent: u32) -> bool {
    let mut result = I256::ONE;

    while exponent != 0 {
        if exponent & 1 == 1 {
            if i256_mul_will_overflow(result, base) {
                return true;
            }
            result *= base;
        }

        exponent >>= 1;
        if exponent != 0 {
            if i256_mul_will_overflow(base, base) {
                return true;
            }
            base *= base;
        }
    }

    false
}

#[cfg(test)]
mod overflow_oracle_tests {
    use super::*;

    fn num(value: u128) -> Num256 {
        Num256(0, value)
    }

    #[test]
    fn target_overflow_dispatches_by_signedness_and_operation() {
        assert!(will_overflow(Target::Unsigned(Op::Add(
            Num256(u128::MAX, u128::MAX),
            num(1),
        ))));
        assert!(will_overflow(Target::Unsigned(Op::Sub(num(0), num(1)))));
        assert!(will_overflow(Target::Unsigned(Op::Mul(
            Num256(u128::MAX, u128::MAX),
            num(2),
        ))));
        assert!(will_overflow(Target::Unsigned(Op::Pow(num(2), 256))));

        assert!(will_overflow(Target::Signed(Op::Add(
            Num256(i128::MAX as u128, u128::MAX),
            num(1),
        ))));
        assert!(will_overflow(Target::Signed(Op::Sub(
            Num256(i128::MIN as u128, 0),
            num(1),
        ))));
        assert!(will_overflow(Target::Signed(Op::Mul(
            Num256(i128::MIN as u128, 0),
            Num256(u128::MAX, u128::MAX),
        ))));
        assert!(will_overflow(Target::Signed(Op::Pow(num(2), 255))));
    }

    #[test]
    fn u256_add_overflow_boundaries() {
        assert!(!u256_add_will_overflow(U256::ZERO, U256::ZERO));
        assert!(!u256_add_will_overflow(U256::MAX, U256::ZERO));
        assert!(!u256_add_will_overflow(
            U256::MAX - U256::new(7),
            U256::new(7)
        ));

        assert!(u256_add_will_overflow(U256::MAX, U256::ONE));
        assert!(u256_add_will_overflow(
            U256::MAX - U256::new(7),
            U256::new(8)
        ));
    }

    #[test]
    fn u256_sub_overflow_boundaries() {
        assert!(!u256_sub_will_overflow(U256::ZERO, U256::ZERO));
        assert!(!u256_sub_will_overflow(U256::MAX, U256::MAX));
        assert!(!u256_sub_will_overflow(U256::ONE, U256::ZERO));
        assert!(u256_sub_will_overflow(U256::ZERO, U256::ONE));
    }

    #[test]
    fn u256_mul_overflow_boundaries() {
        assert!(!u256_mul_will_overflow(U256::MAX, U256::ZERO));
        assert!(!u256_mul_will_overflow(U256::MAX, U256::ONE));
        assert!(!u256_mul_will_overflow(
            U256::MAX / U256::new(2),
            U256::new(2)
        ));
        assert!(u256_mul_will_overflow(U256::MAX, U256::new(2)));
        assert!(u256_mul_will_overflow(
            U256::MAX / U256::new(2) + U256::ONE,
            U256::new(2)
        ));
    }

    #[test]
    fn u256_pow_overflow_boundaries() {
        assert!(!u256_pow_will_overflow(U256::MAX, 0));
        assert!(!u256_pow_will_overflow(U256::MAX, 1));
        assert!(u256_pow_will_overflow(U256::MAX, 2));
        assert!(!u256_pow_will_overflow(U256::ZERO, u32::MAX));
        assert!(!u256_pow_will_overflow(U256::ONE, u32::MAX));
        assert!(!u256_pow_will_overflow(U256::new(2), 255));
        assert!(u256_pow_will_overflow(U256::new(2), 256));
    }

    #[test]
    fn i256_add_overflow_boundaries() {
        assert!(!i256_add_will_overflow(I256::ZERO, I256::ZERO));
        assert!(!i256_add_will_overflow(I256::MAX, -I256::ONE));
        assert!(!i256_add_will_overflow(I256::MIN, I256::ONE));
        assert!(i256_add_will_overflow(I256::MAX, I256::ONE));
        assert!(i256_add_will_overflow(I256::MIN, -I256::ONE));
    }

    #[test]
    fn i256_sub_overflow_boundaries() {
        assert!(!i256_sub_will_overflow(I256::ZERO, I256::ZERO));
        assert!(!i256_sub_will_overflow(I256::MIN, I256::MIN));
        assert!(!i256_sub_will_overflow(I256::MAX, I256::MAX));
        assert!(i256_sub_will_overflow(I256::MIN, I256::ONE));
        assert!(i256_sub_will_overflow(I256::MAX, -I256::ONE));
    }

    #[test]
    fn i256_mul_overflow_boundaries() {
        assert!(!i256_mul_will_overflow(I256::MIN, I256::ZERO));
        assert!(!i256_mul_will_overflow(I256::MIN, I256::ONE));
        assert!(!i256_mul_will_overflow(I256::MAX, I256::ONE));
        assert!(i256_mul_will_overflow(I256::MIN, -I256::ONE));
        assert!(i256_mul_will_overflow(I256::MAX, I256::new(2)));
        assert!(i256_mul_will_overflow(I256::MIN, I256::new(2)));
    }

    #[test]
    fn i256_pow_overflow_boundaries() {
        assert!(!i256_pow_will_overflow(I256::MIN, 0));
        assert!(!i256_pow_will_overflow(I256::MIN, 1));
        assert!(i256_pow_will_overflow(I256::MIN, 2));
        assert!(!i256_pow_will_overflow(I256::ZERO, u32::MAX));
        assert!(!i256_pow_will_overflow(I256::ONE, u32::MAX));
        assert!(!i256_pow_will_overflow(-I256::ONE, u32::MAX));
        assert!(!i256_pow_will_overflow(I256::new(2), 254));
        assert!(i256_pow_will_overflow(I256::new(2), 255));
        assert!(i256_pow_will_overflow(-I256::new(2), 256));
    }

    #[test]
    fn i256_predicates_match_checked_arithmetic() {
        let values = [
            I256::MIN,
            I256::MIN + I256::ONE,
            -I256::new(2),
            -I256::ONE,
            I256::ZERO,
            I256::ONE,
            I256::new(2),
            I256::MAX - I256::ONE,
            I256::MAX,
        ];

        for lhs in values {
            for rhs in values {
                assert_eq!(
                    i256_add_will_overflow(lhs, rhs),
                    lhs.checked_add(rhs).is_none()
                );
                assert_eq!(
                    i256_sub_will_overflow(lhs, rhs),
                    lhs.checked_sub(rhs).is_none()
                );
                assert_eq!(
                    i256_mul_will_overflow(lhs, rhs),
                    lhs.checked_mul(rhs).is_none()
                );
            }

            for exponent in [0, 1, 2, 3, 127, 128, 254, 255, 256, u32::MAX] {
                assert_eq!(
                    i256_pow_will_overflow(lhs, exponent),
                    lhs.checked_pow(exponent).is_none()
                );
            }
        }
    }
}
