use super::poseidon_params::PoseidonParams;
use super::super::metered_scalar::MeteredScalar;
use super::utils;
use crate::{host::metered_clone::{MeteredClone, MeteredContainer}, xdr::{ScErrorCode, ScErrorType}, Host, HostError};

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

    pub fn get_t(&self) -> usize {
        self.params.t
    }

    pub fn permutation(&self, input: &Vec<S>, host: &Host) -> Result<Vec<S>, HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon::permutation length mismatch", &[]));
        }

        let mut current_state = input.metered_clone(host)?;

        for r in 0..self.params.rounds_f_beginning {
            current_state = utils::add_rc(&current_state, &self.params.round_constants[r], host)?;
            current_state = utils::sbox(&current_state, self.params.d, host)?;
            current_state = self.matmul(&current_state, &self.params.mds, host)?;
        }
        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            current_state = utils::add_rc(&current_state, &self.params.round_constants[r], host)?;
            current_state[0] = utils::sbox_p(&current_state[0], self.params.d, host)?;
            current_state = self.matmul(&current_state, &self.params.mds, host)?;
        }
        for r in p_end..self.params.rounds {
            current_state = utils::add_rc(&current_state, &self.params.round_constants[r], host)?;
            current_state = utils::sbox(&current_state, self.params.d, host)?;
            current_state = self.matmul(&current_state, &self.params.mds, host)?;
        }
        Ok(current_state)
    }

    fn matmul(&self, input: &[S], mat: &[Vec<S>], host: &Host) -> Result<Vec<S>, HostError> { 
        let t = mat.len();
        if t != input.len() {
            return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon::matmul length mismatch", &[]));
        }
        Vec::<S>::charge_bulk_init_cpy(t as u64, host)?;
        let mut out = vec![S::zero(); t];
        for row in 0..t {
            for (col, inp) in input.iter().enumerate().take(t) {
                let mut tmp = mat[row][col];
                tmp.metered_mul_assign(inp, host)?;
                out[row].metered_add_assign(&tmp, host)?;
            }
        }
        Ok(out)
    }
}