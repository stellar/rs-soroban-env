use super::poseidon2_params::Poseidon2Params;
use super::super::metered_scalar::MeteredScalar;
use super::utils;
use crate::{host::metered_clone::MeteredClone, xdr::{ScErrorCode, ScErrorType}, Host, HostError};

#[derive(Clone, Debug)]
pub struct Poseidon2<F: MeteredScalar> {
    pub(crate) params: Poseidon2Params<F>,
}

impl<F: MeteredScalar> Poseidon2<F> {
    pub fn new(params: Poseidon2Params<F>) -> Self {
        Poseidon2 {
            params,
        }
    }

    pub fn permutation(&self, input: &Vec<F>, host: &Host) -> Result<Vec<F>, HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon2::permutation input length mismatch", &[]));
        }

        let mut current_state = input.metered_clone(host)?;

        // Linear layer at beginning
        self.matmul_external(&mut current_state, host)?;

        for r in 0..self.params.rounds_f_beginning {
            current_state = utils::add_rc(host, &current_state, &self.params.round_constants[r])?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            self.matmul_external(&mut current_state, host)?;
        }

        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            current_state[0].metered_add_assign(&self.params.round_constants[r][0], host)?;
            current_state[0] = utils::sbox_p(host, &current_state[0], self.params.d)?;
            self.matmul_internal(&mut current_state, &self.params.mat_internal_diag_m_1, host)?;
        }
        
        for r in p_end..self.params.rounds {
            current_state = utils::add_rc(host, &current_state, &self.params.round_constants[r])?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            self.matmul_external(&mut current_state, host)?;
        }
        Ok(current_state)
    }

    fn matmul_m4(&self, input: &mut[F], host: &Host) -> Result<(), HostError> {
        let t = self.params.t;
        let t4 = t / 4;
        for i in 0..t4 {
            let start_index = i * 4;
            let mut t_0 = input[start_index];
            t_0.metered_add_assign(&input[start_index + 1], host)?;
            let mut t_1 = input[start_index + 2];
            t_1.metered_add_assign(&input[start_index + 3], host)?;
            let mut t_2 = input[start_index + 1];
            t_2.metered_double_in_place(host)?;
            t_2.metered_add_assign(&t_1, host)?;
            let mut t_3 = input[start_index + 3];
            t_3.metered_double_in_place(host)?;
            t_3.metered_add_assign(&t_0, host)?;
            let mut t_4 = t_1;
            t_4.metered_double_in_place(host)?;
            t_4.metered_double_in_place(host)?;
            t_4.metered_add_assign(&t_3, host)?;
            let mut t_5 = t_0;
            t_5.metered_double_in_place(host)?;
            t_5.metered_double_in_place(host)?;
            t_5.metered_add_assign(&t_2, host)?;
            let mut t_6 = t_3;
            t_6.metered_add_assign(&t_5, host)?;
            let mut t_7 = t_2;
            t_7.metered_add_assign(&t_4, host)?;
            input[start_index] = t_6;
            input[start_index + 1] = t_5;
            input[start_index + 2] = t_7;
            input[start_index + 3] = t_4;
        }
        Ok(())
    }

    fn matmul_external(&self, input: &mut[F], host: &Host) -> Result<(), HostError> {
        let t = self.params.t;
        match t {
            2 => {
                // Matrix circ(2, 1)
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_add_assign(&sum, host)?;
            }
            3 => {
                // Matrix circ(2, 1, 1)
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                sum.metered_add_assign(&input[2], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_add_assign(&sum, host)?;
                input[2].metered_add_assign(&sum, host)?;
            }
            4 => {
                // Applying cheap 4x4 MDS matrix to each 4-element part of the state
                self.matmul_m4(input, host)?;
            }
            8 | 12 | 16 | 20 | 24 => {
                // Applying cheap 4x4 MDS matrix to each 4-element part of the state
                self.matmul_m4(input, host)?;

                // Applying second cheap matrix for t > 4
                let t4 = t / 4;
                let mut stored = [F::zero(); 4];
                for l in 0..4 {
                    stored[l] = input[l];
                    for j in 1..t4 {
                        stored[l].metered_add_assign(&input[4 * j + l], host)?;
                    }
                }
                for i in 0..input.len() {
                    input[i].metered_add_assign(&stored[i % 4], host)?;
                }
            }
            _ => {
                return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon2::matmul_external unsupported state size", &[]));
            }
        }
        Ok(())
    }

    fn matmul_internal(&self, input: &mut[F], mat_internal_diag_m_1: &[F], host: &Host) -> Result<(), HostError> {
        let t = self.params.t;

        match t {
            2 => {
                // [2, 1]
                // [1, 3]
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_double_in_place(host)?;
                input[1].metered_add_assign(&sum, host)?;
            }
            3 => {
                // [2, 1, 1]
                // [1, 2, 1]
                // [1, 1, 3]
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                sum.metered_add_assign(&input[2], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_add_assign(&sum, host)?;
                input[2].metered_double_in_place(host)?;
                input[2].metered_add_assign(&sum, host)?;
            }
            4 | 8 | 12 | 16 | 20 | 24 => {
                // Compute input sum
                let mut sum = input[0];
                for el in input.iter().skip(1).take(t-1) {
                    sum.metered_add_assign(el, host)?;
                }
                // Add sum + diag entry * element to each element
                for i in 0..input.len() {
                    input[i].metered_mul_assign(&mat_internal_diag_m_1[i], host)?;
                    input[i].metered_add_assign(&sum, host)?;
                }
            }
            _ => {
                return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon2::matmul_internal unsupported state size", &[]));
            }
        }
        Ok(())
    }
}