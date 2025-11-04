use crate::{
    crypto::{
        metered_scalar::MeteredScalar,
        poseidon::{poseidon2_params::Poseidon2Params, utils, INVALID_INPUT, VEC_OOB},
    },
    host::metered_clone::{MeteredClone, MeteredContainer},
    ErrorHandler, Host, HostError, Val,
};

#[derive(Clone, Debug)]
pub struct Poseidon2<F: MeteredScalar> {
    pub(crate) params: Poseidon2Params<F>,
}

impl<F: MeteredScalar> Poseidon2<F> {
    pub fn new(params: Poseidon2Params<F>) -> Self {
        Poseidon2 { params }
    }

    pub fn permutation(&self, host: &Host, input: &Vec<F>) -> Result<Vec<F>, HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: permutation input length does not match `t`",
                &[
                    Val::from_u32(input.len() as u32).into(),
                    Val::from_u32(t as u32).into(),
                ],
            ));
        }

        let mut current_state = input.metered_clone(host)?;

        // Linear layer at beginning
        self.matmul_external(host, &mut current_state)?;

        for r in 0..self.params.rounds_f_beginning {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(
                    VEC_OOB,
                    "Poseidon2: round constant index out of bounds",
                    &[],
                )
            })?;
            current_state = utils::add_rc(host, &current_state, rc)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            self.matmul_external(host, &mut current_state)?;
        }

        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(
                    VEC_OOB,
                    "Poseidon2: round constant index out of bounds",
                    &[],
                )
            })?;
            let rc_first = rc.first().ok_or_else(|| {
                host.error(VEC_OOB, "Poseidon2: round constant vector is empty", &[])
            })?;
            let first = current_state
                .first_mut()
                .ok_or_else(|| host.error(VEC_OOB, "Poseidon2: current_state is empty", &[]))?;
            first.metered_add_assign(rc_first, host)?;
            *first = utils::sbox_p(host, first, self.params.d)?;
            self.matmul_internal(host, &mut current_state, &self.params.mat_internal_diag_m_1)?;
        }

        for r in p_end..self.params.rounds {
            let rc = self.params.round_constants.get(r).ok_or_else(|| {
                host.error(
                    VEC_OOB,
                    "Poseidon2: round constant index out of bounds",
                    &[],
                )
            })?;
            current_state = utils::add_rc(host, &current_state, rc)?;
            current_state = utils::sbox(host, &current_state, self.params.d)?;
            self.matmul_external(host, &mut current_state)?;
        }
        Ok(current_state)
    }

    /// Applies the 4x4 MDS matrix to each 4-element chunk.
    ///
    /// Reference: Section 5.1 of the Poseidon2 paper
    /// (https://eprint.iacr.org/2023/323)
    fn matmul_m4(&self, host: &Host, input: &mut [F]) -> Result<(), HostError> {
        let t = self.params.t;
        if t % 4 != 0 {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: matmul_m4 state size must be divisible by 4",
                &[],
            ));
        }
        if input.len() != t {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: matmul_m4 input length does not match `t`",
                &[
                    Val::from_u32(input.len() as u32).into(),
                    Val::from_u32(t as u32).into(),
                ],
            ));
        }

        let t4 = t / 4;
        for i in 0..t4 {
            // we've already asserted above t is a multiple of 4, so indexing below should be safe
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

    /// Applies the external round matrix multiplication.
    ///
    /// The external matrix is used in the full S-box rounds (beginning and end of the permutation).
    /// Different state sizes use different optimized matrix constructions
    ///
    /// Reference: Section 5.1 of the Poseidon2 paper
    /// (https://eprint.iacr.org/2023/323)
    fn matmul_external(&self, host: &Host, input: &mut [F]) -> Result<(), HostError> {
        let t = self.params.t;
        if input.len() != t {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: matmul_external input length does not match `t`",
                &[
                    Val::from_u32(input.len() as u32).into(),
                    Val::from_u32(t as u32).into(),
                ],
            ));
        }
        match t {
            2 => {
                // Matrix circ(2, 1)
                // t == 2, so indexing below is safe
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_add_assign(&sum, host)?;
            }
            3 => {
                // Matrix circ(2, 1, 1)
                // t == 3, so indexing below is safe
                let mut sum = input[0];
                sum.metered_add_assign(&input[1], host)?;
                sum.metered_add_assign(&input[2], host)?;
                input[0].metered_add_assign(&sum, host)?;
                input[1].metered_add_assign(&sum, host)?;
                input[2].metered_add_assign(&sum, host)?;
            }
            4 => {
                // Applying cheap 4x4 MDS matrix to each 4-element part of the state
                self.matmul_m4(host, input)?;
            }
            8 | 12 | 16 | 20 | 24 => {
                // Applying cheap 4x4 MDS matrix to each 4-element part of the state
                self.matmul_m4(host, input)?;

                // Applying second cheap matrix for t > 4
                let t4 = t / 4;
                Vec::<F>::charge_bulk_init_cpy(4, host)?;
                let mut stored = [F::zero(); 4];
                for l in 0..4 {
                    // input length is greater than 4 (min. 8), indexing is safe
                    stored[l] = input[l];
                    for j in 1..t4 {
                        // input length is a multiple of 4 (min. 8), so the indexing here is safe
                        stored[l].metered_add_assign(&input[4 * j + l], host)?;
                    }
                }
                for i in 0..input.len() {
                    // stored is a array of size 4, indexing is safe
                    input[i].metered_add_assign(&stored[i % 4], host)?;
                }
            }
            _ => {
                return Err(host.error(
                    INVALID_INPUT,
                    "Poseidon2: matmul_external unsupported state size",
                    &[Val::from(t as u32)],
                ));
            }
        }
        Ok(())
    }

    /// Applies the internal round matrix multiplication.
    ///
    /// The internal matrix is used during the partial S-box rounds
    /// where only the first state element goes through the S-box.
    ///
    /// Reference: Section 5.2 of the Poseidon2 paper
    /// (https://eprint.iacr.org/2023/323)
    fn matmul_internal(
        &self,
        host: &Host,
        input: &mut [F],
        mat_internal_diag_m_1: &[F],
    ) -> Result<(), HostError> {
        let t = self.params.t;
        if t != input.len() || t != mat_internal_diag_m_1.len() {
            return Err(host.error(INVALID_INPUT, "Poseidon2: matmul_internal length mismatch between `t`, input length and/or matrix length", 
                &[Val::from_u32(t as u32).into(), Val::from_u32(input.len() as u32).into(), Val::from_u32(mat_internal_diag_m_1.len() as u32).into(), ]
            ));
        }

        match t {
            2 => {
                // [2, 1]
                // [1, 3]
                // t == 2, so indexing below is safe
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
                // t == 3, so indexing below is safe
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
                for el in input.iter().skip(1).take(t - 1) {
                    sum.metered_add_assign(el, host)?;
                }
                // Add sum + diag entry * element to each element
                for i in 0..input.len() {
                    // input and mat_internal_diag_m_1 have the same length, indexing is safe
                    input[i].metered_mul_assign(&mat_internal_diag_m_1[i], host)?;
                    input[i].metered_add_assign(&sum, host)?;
                }
            }
            _ => {
                return Err(host.error(
                    INVALID_INPUT,
                    "Poseidon2: matmul_internal unsupported state size",
                    &[],
                ));
            }
        }
        Ok(())
    }
}
