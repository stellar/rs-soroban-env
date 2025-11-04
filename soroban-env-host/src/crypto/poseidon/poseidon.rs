use crate::{
    crypto::{
        metered_scalar::MeteredScalar,
        poseidon::{poseidon_params::PoseidonParams, utils, INVALID_INPUT, VEC_OOB},
    },
    host::metered_clone::{MeteredClone, MeteredContainer},
    ErrorHandler, Host, HostError, Val,
};

#[derive(Clone, Debug)]
pub struct Poseidon<S: MeteredScalar> {
    pub(crate) params: PoseidonParams<S>,
}

impl<S: MeteredScalar> Poseidon<S> {
    pub fn new(params: PoseidonParams<S>) -> Self {
        Poseidon { params }
    }

    pub fn permutation(&self, host: &Host, input: &Vec<S>) -> Result<Vec<S>, HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon: permutation input length does not match `t`",
                &[
                    Val::from_u32(input.len() as u32).into(),
                    Val::from_u32(t as u32).into(),
                ],
            ));
        }

        let mut current_state = input.metered_clone(host)?;

        for r in 0..self.params.rounds_f_beginning {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(VEC_OOB, "Poseidon: round constant index out of bounds", &[])
            })?;
            current_state = utils::add_rc(host, &current_state, rc)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }

        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(VEC_OOB, "Poseidon: round constant index out of bounds", &[])
            })?;
            current_state = utils::add_rc(host, &current_state, rc)?;
            let first = current_state
                .first_mut()
                .ok_or_else(|| host.error(VEC_OOB, "Poseidon: current_state is empty", &[]))?;
            *first = utils::sbox_p(host, first, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }

        for r in p_end..self.params.rounds {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(VEC_OOB, "Poseidon: round constant index out of bounds", &[])
            })?;
            current_state = utils::add_rc(host, &current_state, rc)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            current_state = self.matmul(host, &current_state, &self.params.mds)?;
        }
        Ok(current_state)
    }

    /// Applies MDS matrix multiplication for Poseidon.
    ///
    /// Reference: Section 3 of the original Poseidon paper
    /// (https://eprint.iacr.org/2019/458)
    fn matmul(&self, host: &Host, input: &[S], mat: &[Vec<S>]) -> Result<Vec<S>, HostError> {
        let t = input.len();

        // Validate input length matches matrix dimension
        if mat.len() != t {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon: matmul matrix dimension does not match the input length",
                &[
                    Val::from_u32(mat.len() as u32).into(),
                    Val::from_u32(t as u32).into(),
                ],
            ));
        }

        // Allocate output vector
        Vec::<S>::charge_bulk_init_cpy(t as u64, host)?;
        let mut out = vec![S::zero(); t];

        // Perform matrix-vector multiplication: out[i] = sum(mat[i][j] * input[j])
        for (out_element, matrix_row) in out.iter_mut().zip(mat.iter()) {
            if matrix_row.len() != t {
                return Err(host.error(
                    INVALID_INPUT,
                    "Poseidon: matmul matrix row length does not match input length",
                    &[
                        Val::from_u32(matrix_row.len() as u32).into(),
                        Val::from_u32(t as u32).into(),
                    ],
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
