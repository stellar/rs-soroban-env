use super::poseidon_params::PoseidonParams;
use super::super::metered_scalar::MeteredScalar;
use super::utils;
use crate::{host::metered_clone::{MeteredClone, MeteredContainer}, xdr::{ScErrorCode, ScErrorType}, Host, HostError, Val, Error};

const VEC_OOB: Error = Error::from_type_and_code(ScErrorType::Object, ScErrorCode::IndexBounds);

#[derive(Clone, Debug)]
pub struct Poseidon<S: MeteredScalar> {
    pub(crate) params: PoseidonParams<S>,
}

impl<S: MeteredScalar> Poseidon<S> {
    pub fn new(params: PoseidonParams<S>) -> Self {
        Poseidon {
            params,
        }
    }

    pub fn permutation(&self, host: &Host, input: &Vec<S>) -> Result<Vec<S>, HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon::permutation input length does not match `t`", &[Val::from_u32(input.len() as u32).into(), Val::from_u32(t as u32).into()]));
        }

        let mut current_state = input.metered_clone(host)?;

        for r in 0..self.params.rounds_f_beginning {
            current_state = utils::add_rc(host, &current_state, self.params.round_constants.get(r).ok_or_else(|| HostError::from(VEC_OOB))?)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }
        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            current_state = utils::add_rc(host, &current_state, self.params.round_constants.get(r).ok_or_else(|| HostError::from(VEC_OOB))?)?;
            let first = current_state.first_mut().ok_or_else(|| HostError::from(VEC_OOB))?;
            *first = utils::sbox_p(host, first, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }
        for r in p_end..self.params.rounds {
            current_state = utils::add_rc(host, &current_state, self.params.round_constants.get(r).ok_or_else(|| HostError::from(VEC_OOB))?)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }
        Ok(current_state)
    }

    fn matmul(&self, host: &Host, input: &[S], mat: &[Vec<S>]) -> Result<Vec<S>, HostError> { 
        let t = input.len();

        // Validate input length matches matrix dimension
        if mat.len() != t {
            return Err(host.err(
                ScErrorType::Crypto, 
                ScErrorCode::InvalidInput, 
                "poseidon::matmul: matrix dimension does not match the input length", 
                &[Val::from_u32(mat.len() as u32).into(), Val::from_u32(t as u32).into()]
            ));
        }
        
        // Allocate output vector
        Vec::<S>::charge_bulk_init_cpy(t as u64, host)?;
        let mut out = vec![S::zero(); t];
        
        // Perform matrix-vector multiplication: out[i] = sum(mat[i][j] * input[j])
        for (out_element, matrix_row) in out.iter_mut().zip(mat.iter()) {
            if matrix_row.len() != t {
                return Err(host.err(
                    ScErrorType::Crypto, 
                    ScErrorCode::InvalidInput, 
                    "poseidon::matmul: matrix row length does not match input length)", 
                    &[Val::from_u32(matrix_row.len() as u32).into(), Val::from_u32(t as u32).into()]
                ));
            }
            for (mat_element, input_element) in matrix_row.iter().zip(input.iter()) {
                let mut tmp = *mat_element;
                tmp.metered_mul_assign(input_element, host)?;
                out_element.metered_add_assign(&tmp, host)?;
            }
        }
        
        Ok(out)
    }
}