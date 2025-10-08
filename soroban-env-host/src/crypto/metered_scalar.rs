use crate::{
    host::metered_clone::MeteredClone, Host, HostError, U256Val, xdr::ContractCostType, U256Small, U256Object, U256, ConversionError, TryFromVal, ErrorHandler
};
use ark_bls12_381::Fr as BlsScalar;
use ark_bn254::Fr as BnScalar;
use ark_ff::{PrimeField, BigInteger};

/// A trait for scalar field elements that can perform metered arithmetic operations.
/// These operations charge the budget for computational costs and are fallible when
/// the budget is exceeded.
pub trait MeteredScalar: PrimeField + Sized + Clone + PartialEq + MeteredClone {
    /// Performs metered addition assignment, charging the budget for the operation.
    /// Modifies `self` by adding `other` to it.
    fn metered_add_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError>;
    
    /// Performs metered subtraction assignment, charging the budget for the operation.
    /// Modifies `self` by subtracting `other` from it.
    fn metered_sub_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError>;
    
    /// Performs metered multiplication assignment, charging the budget for the operation.
    /// Modifies `self` by multiplying it with `other`.
    fn metered_mul_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError>;
    
    /// Performs metered squaring in place, charging the budget for the operation.
    /// Modifies `self` by squaring it.
    fn metered_square_in_place(&mut self, host: &Host) -> Result<(), HostError>;
    
    /// Performs metered doubling in place, charging the budget for the operation.
    /// Modifies `self` by doubling it.
    fn metered_double_in_place(&mut self, host: &Host) -> Result<(), HostError>;
    
    /// Performs metered exponentiation, charging the budget for the operation.
    /// Returns a new element that is `self` raised to the power of `exp`.
    fn metered_pow(&self, exp: &u64, host: &Host) -> Result<Self, HostError>;

    /// Performs metered addition, charging the budget for the operation.
    /// Returns a new element that is the sum of `self` and `other`.
    fn metered_add(&self, other: &Self, host: &Host) -> Result<Self, HostError> {
        let mut result = self.clone();
        result.metered_add_assign(other, host)?;
        Ok(result)
    }
    
    /// Performs metered multiplication, charging the budget for the operation.
    /// Returns a new element that is the product of `self` and `other`.
    fn metered_mul(&self, other: &Self, host: &Host) -> Result<Self, HostError> {
        let mut result = self.clone();
        result.metered_mul_assign(other, host)?;
        Ok(result)
    }

    fn from_u256val(host: &Host, sv: U256Val) -> Result<Self, HostError>;

    fn to_u256val(self, host: &Host) -> Result<U256Val, HostError>;
}

impl MeteredScalar for BlsScalar {
    fn metered_add_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        host.fr_add_internal(self, other)
    }
    
    fn metered_sub_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        host.fr_sub_internal(self, other)
    }

    fn metered_mul_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        host.fr_mul_internal(self, other)
    }
    
    fn metered_square_in_place(&mut self, host: &Host) -> Result<(), HostError> {
        // Square is essentially multiply by self
        let temp = *self;
        host.fr_mul_internal(self, &temp)
    }
    
    fn metered_double_in_place(&mut self, host: &Host) -> Result<(), HostError> {
        // Double is essentially add self to self
        let temp = *self;
        host.fr_add_internal(self, &temp)
    }
    
    fn metered_pow(&self, exp: &u64, host: &Host) -> Result<Self, HostError> {
        host.fr_pow_internal(self, exp)
    }
    
    fn from_u256val(host: &Host, sv: U256Val) -> Result<Self, HostError> {
        host.charge_budget(ContractCostType::Bls12381FrFromU256, None)?;
        let fr = if let Ok(small) = U256Small::try_from(sv) {
            Self::from_le_bytes_mod_order(&u64::from(small).to_le_bytes())
        } else {
            let obj: U256Object = sv.try_into()?;
            host.visit_obj(obj, |u: &U256| {
                Ok(Self::from_le_bytes_mod_order(&u.to_le_bytes()))
            })?
        };
        Ok(fr)    
    }

    fn to_u256val(self, host: &Host) -> Result<U256Val, HostError> {
        host.charge_budget(ContractCostType::Bls12381FrToU256, None)?;
        // The `into_bigint` carries the majority of the cost. It performs the
        // Montgomery reduction on the internal representation, which is doing a
        // number of wrapping arithmetics on each u64 word (`Fr` contains 4
        // words). The core routine is in `ark_ff::MontConfig::into_bigint`,
        // this cannot panic.
        let bytes: [u8; 32] = self
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| HostError::from(ConversionError))?;
        let u = U256::from_be_bytes(bytes);
        host.map_err(U256Val::try_from_val(host, &u))
    }
}

impl MeteredScalar for BnScalar {
    fn metered_add_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        host.bn254_fr_add_internal(self, other)
    }
    
    fn metered_sub_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        // For now, use the same cost type as BLS12-381 until BN254 specific costs are added
        host.charge_budget(crate::xdr::ContractCostType::Bls12381FrAddSub, None)?;
        *self -= *other;
        Ok(())
    }

    fn metered_mul_assign(&mut self, other: &Self, host: &Host) -> Result<(), HostError> {
        host.bn254_fr_mul_internal(self, other)
    }
    
    fn metered_square_in_place(&mut self, host: &Host) -> Result<(), HostError> {
        // Square is essentially multiply by self
        let temp = *self;
        host.bn254_fr_mul_internal(self, &temp)
    }
    
    fn metered_double_in_place(&mut self, host: &Host) -> Result<(), HostError> {
        // Double is essentially add self to self
        let temp = *self;
        host.bn254_fr_add_internal(self, &temp)
    }
    
    fn metered_pow(&self, exp: &u64, host: &Host) -> Result<Self, HostError> {
        host.bn254_fr_pow_internal(self, exp)
    }
    
    fn from_u256val(host: &Host, sv: U256Val) -> Result<Self, HostError> {
        host.charge_budget(ContractCostType::Bn254FrFromU256, None)?;
        let fr = if let Ok(small) = U256Small::try_from(sv) {
            Self::from_le_bytes_mod_order(&u64::from(small).to_le_bytes())
        } else {
            let obj: U256Object = sv.try_into()?;
            host.visit_obj(obj, |u: &U256| {
                Ok(Self::from_le_bytes_mod_order(&u.to_le_bytes()))
            })?
        };
        Ok(fr)
    }

    fn to_u256val(self, host: &Host) -> Result<U256Val, HostError> {
        // TODO: use proper Bn type
        host.charge_budget(ContractCostType::Bls12381FrToU256, None)?;
        // The `into_bigint` carries the majority of the cost. It performs the
        // Montgomery reduction on the internal representation, which is doing a
        // number of wrapping arithmetics on each u64 word (`Fr` contains 4
        // words). The core routine is in `ark_ff::MontConfig::into_bigint`,
        // this cannot panic.
        let bytes: [u8; 32] = self
            .into_bigint()
            .to_bytes_be()
            .try_into()
            .map_err(|_| HostError::from(ConversionError))?;
        let u = U256::from_be_bytes(bytes);
        host.map_err(U256Val::try_from_val(host, &u))
    }    
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Host;
    use crate::xdr::ContractCostType;

    #[test]
    fn test_metered_scalar_bls12_381() {
        let host = Host::test_host();
        
        // Test addition assignment
        let mut a = BlsScalar::from(3u64);
        let b = BlsScalar::from(5u64);
        a.metered_add_assign(&b, &host).unwrap();
        assert_eq!(a, BlsScalar::from(8u64));
        
        // Test multiplication assignment
        let mut c = BlsScalar::from(3u64);
        let d = BlsScalar::from(5u64);
        c.metered_mul_assign(&d, &host).unwrap();
        assert_eq!(c, BlsScalar::from(15u64));
    }

    #[test]
    fn test_metered_scalar_bn254() {
        let host = Host::test_host();
        
        // Test addition assignment
        let mut a = BnScalar::from(7u64);
        let b = BnScalar::from(11u64);
        a.metered_add_assign(&b, &host).unwrap();
        assert_eq!(a, BnScalar::from(18u64));
        
        // Test multiplication assignment
        let mut c = BnScalar::from(7u64);
        let d = BnScalar::from(11u64);
        c.metered_mul_assign(&d, &host).unwrap();
        assert_eq!(c, BnScalar::from(77u64));
    }

    #[test]
    fn test_metered_scalar_bls12_381_budget_exceeded() {
        use crate::xdr::{ScErrorCode, ScErrorType};
        
        // Create a host with very limited budget
        let host = Host::test_host()
            .test_budget(10, 10)  // Very small CPU and memory budget
            .enable_model(ContractCostType::Bls12381FrAddSub, 100, 0, 1, 0); // High cost per operation
        
        let mut a = BlsScalar::from(3u64);
        let b = BlsScalar::from(5u64);
        
        // This should fail due to budget exceeded
        let result = a.metered_add_assign(&b, &host);
        let expected_error = (ScErrorType::Budget, ScErrorCode::ExceededLimit);
        assert!(HostError::result_matches_err(result, expected_error));
    }

    #[test]
    fn test_metered_scalar_bls12_381_mul_budget_exceeded() {
        use crate::xdr::{ScErrorCode, ScErrorType};
        
        // Create a host with very limited budget
        let host = Host::test_host()
            .test_budget(10, 10)  // Very small CPU and memory budget
            .enable_model(ContractCostType::Bls12381FrMul, 100, 0, 1, 0); // High cost per operation
        
        let mut a = BlsScalar::from(3u64);
        let b = BlsScalar::from(5u64);
        
        // This should fail due to budget exceeded
        let result = a.metered_mul_assign(&b, &host);
        let expected_error = (ScErrorType::Budget, ScErrorCode::ExceededLimit);
        assert!(HostError::result_matches_err(result, expected_error));
    }
}

