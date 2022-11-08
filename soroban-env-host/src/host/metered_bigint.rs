use super::{MeteredClone, MeteredCmp};
use crate::{
    budget::{Budget, CostType},
    xdr::ScUnknownErrorCode,
    HostError,
};
use num_bigint::{BigInt, Sign};
use num_integer::Integer;
use num_traits::{Pow, Zero};
use num_traits::{Signed, ToPrimitive};
use std::ops::{Add, BitAnd, BitOr, BitXor, Div, Mul, Neg, Not, Rem, Shl, Shr, Sub};
use std::{
    cmp::{max, min, Ordering},
    rc::Rc,
};

pub struct MeteredBigInt {
    budget: Budget,
    num: BigInt,
}

// implement all the "charge" functions in here
// TODO: can we do it with macro?
impl MeteredBigInt {
    // BigInt creation involves heap allocation
    fn charge_new(&self) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntNew, 1)
    }

    fn charge_add_sub(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntAddSub, x)
    }

    fn charge_mul(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntMul, x)
    }

    fn charge_div_rem(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntDivRem, x)
    }

    fn charge_bitwise_op(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntBitwiseOp, x)
    }

    fn charge_shift(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntShift, x)
    }

    fn charge_cmp(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntCmp, x)
    }

    fn charge_gcd_lcm(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntGcdLcm, x)
    }

    fn charge_pow(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntPow, x)
    }

    fn charge_pow_mod(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntPowMod, x)
    }

    fn charge_sqrt(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntSqrt, x)
    }

    fn charge_from_bytes(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntFromBytes, x)
    }

    fn charge_to_bytes(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntToBytes, x)
    }

    fn charge_to_radix(&self, x: u64) -> Result<(), HostError> {
        self.budget.charge(CostType::BigIntToRadix, x)
    }
}

impl MeteredBigInt {
    pub fn new(budget: Budget) -> Result<Self, HostError> {
        budget.charge(CostType::BigIntNew, 1)?;
        Ok(Self {
            budget,
            num: BigInt::default(),
        })
    }

    // Notes on mtering: free.
    pub fn bits(&self) -> u64 {
        self.num.bits()
    }

    pub fn from_u64(budget: Budget, x: u64) -> Result<Self, HostError> {
        budget.charge(CostType::BigIntNew, 1)?;
        Ok(Self {
            budget,
            num: Into::<BigInt>::into(x),
        })
    }

    pub fn to_u64(&self) -> Option<u64> {
        self.num.to_u64()
    }

    pub fn from_i64(budget: Budget, x: i64) -> Result<Self, HostError> {
        budget.charge(CostType::BigIntNew, 1)?;
        Ok(Self {
            budget,
            num: Into::<BigInt>::into(x),
        })
    }

    pub fn to_i64(&self) -> Option<i64> {
        self.num.to_i64()
    }

    pub fn from_bigint(budget: Budget, num: BigInt) -> Self {
        Self { budget, num }
    }

    pub fn add(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_add_sub(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).add(&other.num),
        })
    }

    pub fn sub(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_add_sub(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).sub(&other.num),
        })
    }

    pub fn mul(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_mul(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).mul(&other.num),
        })
    }

    pub fn div(&self, other: &Self) -> Result<Self, HostError> {
        debug_assert!(!other.is_zero());
        self.charge_div_rem(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).div(&other.num),
        })
    }

    pub fn rem(&self, other: &Self) -> Result<Self, HostError> {
        debug_assert!(!other.is_zero());
        self.charge_div_rem(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).rem(&other.num),
        })
    }

    pub fn bitand(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_bitwise_op(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).bitand(&other.num),
        })
    }

    pub fn bitor(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_bitwise_op(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).bitor(&other.num),
        })
    }

    pub fn bitxor(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_bitwise_op(max(self.bits(), other.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).bitxor(&other.num),
        })
    }

    pub fn shl(&self, rhs: &Self) -> Result<Self, HostError> {
        self.charge_shift(self.bits())?;
        if rhs.is_negative() {
            // TODO: Replace with proper err code "attempt to shift left with negative"
            return Err(ScUnknownErrorCode::General.into());
        }
        if let Some(u) = rhs.num.to_usize() {
            Ok(Self {
                budget: self.budget.clone(),
                num: (&self.num).shl(u),
            })
        } else {
            // TODO: Replace with proper err code "left-shift overflow"
            return Err(ScUnknownErrorCode::General.into());
        }
    }

    pub fn shr(&self, rhs: &Self) -> Result<Self, HostError> {
        self.charge_shift(self.bits())?;
        if rhs.is_negative() {
            // TODO: Replace with proper err code "attempt to shift right with negative"
            return Err(ScUnknownErrorCode::General.into());
        }
        if let Some(u) = rhs.num.to_usize() {
            Ok(Self {
                budget: self.budget.clone(),
                num: (&self.num).shr(u),
            })
        } else {
            // TODO: Replace with proper err code "left-shift overflow"
            return Err(ScUnknownErrorCode::General.into());
        }
    }

    pub fn is_zero(&self) -> bool {
        self.num.is_zero()
    }

    pub fn is_negative(&self) -> bool {
        self.num.is_negative()
    }

    pub fn neg(&self) -> Self {
        Self {
            budget: self.budget.clone(),
            num: (&self.num).neg(),
        }
    }

    pub fn not(&self) -> Self {
        Self {
            budget: self.budget.clone(),
            num: (&self.num).not(),
        }
    }

    pub fn gcd(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_gcd_lcm(max((&self.num).bits(), other.num.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).gcd(&other.num),
        })
    }

    pub fn lcm(&self, other: &Self) -> Result<Self, HostError> {
        self.charge_gcd_lcm(max((&self.num).bits(), other.num.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).lcm(&other.num),
        })
    }

    pub fn pow(&self, rhs: &Self) -> Result<Self, HostError> {
        if rhs.is_negative() {
            // TODO: Replace with proper err code "negative exponentiation not supported"
            return Err(ScUnknownErrorCode::General.into());
        }
        if let Some(u) = rhs.num.to_usize() {
            // Notes on metering: the worst case complexity O(m*2^n)
            self.charge_pow(self.bits().saturating_mul(2 << rhs.bits()))?;
            Ok(Self {
                budget: self.budget.clone(),
                num: Pow::pow(&self.num, u),
            })
        } else {
            // TODO: Replace with proper err code "pow overflow"
            return Err(ScUnknownErrorCode::General.into());
        }
    }

    pub fn modpow(&self, exponent: &Self, modulus: &Self) -> Result<Self, HostError> {
        // TODO: Replace with proper err code "negative exponentiation not supported"
        if exponent.is_negative() {
            return Err(ScUnknownErrorCode::General.into());
        }
        // TODO: Replace with proper err code "zero modulus not supported"
        if modulus.is_zero() {
            return Err(ScUnknownErrorCode::General.into());
        }
        // TODO: I did not fully look into how the algorithm works, instead assumed the cost profile is simliar to pow
        self.charge_pow_mod(self.bits().saturating_mul(2 << exponent.bits()))?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).modpow(&exponent.num, &modulus.num),
        })
    }

    pub fn sqrt(&self) -> Result<Self, HostError> {
        // TODO: Replace with proper err code "sqrt is imaginary"
        if self.is_negative() {
            return Err(ScUnknownErrorCode::General.into());
        }
        self.charge_sqrt(self.bits())?;
        Ok(Self {
            budget: self.budget.clone(),
            num: (&self.num).sqrt(),
        })
    }

    pub fn from_bytes_be(budget: Budget, sign: Sign, bytes: &[u8]) -> Result<Self, HostError> {
        budget.charge(CostType::BigIntFromBytes, bytes.len() as u64)?;
        Ok(Self {
            budget,
            num: BigInt::from_bytes_be(sign, bytes),
        })
    }

    pub fn to_bytes_be(&self) -> Result<(Sign, Vec<u8>), HostError> {
        self.charge_to_bytes(self.bits())?;
        Ok((&self.num).to_bytes_be())
    }

    pub fn from_radix_be(
        budget: Budget,
        sign: Sign,
        buf: &[u8],
        radix: u32,
    ) -> Result<Self, HostError> {
        debug_assert!(2 <= radix && radix <= 256);
        budget.charge(CostType::BigIntFromBytes, buf.len() as u64)?;
        // TODO: proper error code "digit cannot be >= radix"
        let num = BigInt::from_radix_be(sign, buf, radix)
            .ok_or(Into::<HostError>::into(ScUnknownErrorCode::General))?;
        Ok(Self { budget, num })
    }

    pub fn to_radix_be(&self, r: u32) -> Result<(Sign, Vec<u8>), HostError> {
        self.charge_to_radix(self.bits())?;
        Ok((&self.num).to_radix_be(r))
    }
}

impl Clone for MeteredBigInt {
    fn clone(&self) -> Self {
        Self {
            budget: self.budget.clone(),
            num: (&self.num).clone(),
        }
    }
}

impl MeteredClone for MeteredBigInt {
    fn metered_clone(&self, budget: &Budget) -> Result<Self, HostError> {
        debug_assert!(Rc::ptr_eq(&self.budget.0, &budget.0));
        self.charge_new()?;
        Ok(self.clone())
    }
}

impl PartialEq for MeteredBigInt {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.budget.0, &other.budget.0) && self.num == other.num
    }
}

impl Eq for MeteredBigInt {}

impl PartialOrd for MeteredBigInt {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        debug_assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.num.partial_cmp(&other.num)
    }
}

impl Ord for MeteredBigInt {
    fn cmp(&self, other: &Self) -> Ordering {
        debug_assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.num.cmp(&other.num)
    }
}

impl MeteredCmp for MeteredBigInt {
    fn metered_cmp(&self, other: &Self, budget: &Budget) -> Result<Ordering, HostError> {
        assert!(Rc::ptr_eq(&self.budget.0, &other.budget.0));
        self.charge_cmp(min(self.bits(), other.bits()))?;
        Ok(self.num.cmp(&other.num))
    }
}
