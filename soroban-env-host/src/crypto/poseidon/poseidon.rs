use super::poseidon_params::PoseidonParams;
use ark_ff::PrimeField;
use std::sync::Arc;

#[derive(Clone, Debug)]
pub struct Poseidon<S: PrimeField> {
    pub(crate) params: Arc<PoseidonParams<S>>,
}

impl<S: PrimeField> Poseidon<S> {
    pub fn new(params: &Arc<PoseidonParams<S>>) -> Self {
        Poseidon {
            params: Arc::clone(params),
        }
    }

    pub fn get_t(&self) -> usize {
        self.params.t
    }

    pub fn permutation(&self, input: &[S]) -> Vec<S> {
        let t = self.params.t;
        assert_eq!(input.len(), t);

        let mut current_state = input.to_owned();
        for r in 0..self.params.rounds_f_beginning {
            current_state = self.add_rc(&current_state, &self.params.round_constants[r]);
            current_state = self.sbox(&current_state);
            current_state = self.matmul(&current_state, &self.params.mds);
        }
        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        current_state = self.add_rc(&current_state, &self.params.opt_round_constants[0]);
        current_state = self.matmul(&current_state, &self.params.m_i);

        for r in self.params.rounds_f_beginning..p_end {
            current_state[0] = self.sbox_p(&current_state[0]);
            if r < p_end - 1 {
                current_state[0].add_assign(
                    &self.params.opt_round_constants[r + 1 - self.params.rounds_f_beginning][0],
                );
            }
            current_state = self.cheap_matmul(&current_state, p_end - r - 1);
        }
        for r in p_end..self.params.rounds {
            current_state = self.add_rc(&current_state, &self.params.round_constants[r]);
            current_state = self.sbox(&current_state);
            current_state = self.matmul(&current_state, &self.params.mds);
        }
        current_state
    }

    pub fn permutation_not_opt(&self, input: &[S]) -> Vec<S> {
        let t = self.params.t;
        assert_eq!(input.len(), t);

        let mut current_state = input.to_owned();

        for r in 0..self.params.rounds_f_beginning {
            current_state = self.add_rc(&current_state, &self.params.round_constants[r]);
            current_state = self.sbox(&current_state);
            current_state = self.matmul(&current_state, &self.params.mds);
        }
        let p_end = self.params.rounds_f_beginning + self.params.rounds_p;
        for r in self.params.rounds_f_beginning..p_end {
            current_state = self.add_rc(&current_state, &self.params.round_constants[r]);
            current_state[0] = self.sbox_p(&current_state[0]);
            current_state = self.matmul(&current_state, &self.params.mds);
        }
        for r in p_end..self.params.rounds {
            current_state = self.add_rc(&current_state, &self.params.round_constants[r]);
            current_state = self.sbox(&current_state);
            current_state = self.matmul(&current_state, &self.params.mds);
        }
        current_state
    }

    fn sbox(&self, input: &[S]) -> Vec<S> {
        input.iter().map(|el| self.sbox_p(el)).collect()
    }

    fn sbox_p(&self, input: &S) -> S {
        let mut input2 = *input;
        input2.square_in_place();

        match self.params.d {
            3 => {
                let mut out = input2;
                out.mul_assign(input);
                out
            }
            5 => {
                let mut out = input2;
                out.square_in_place();
                out.mul_assign(input);
                out
            }
            7 => {
                let mut out = input2;
                out.square_in_place();
                out.mul_assign(&input2);
                out.mul_assign(input);
                out
            }
            _ => {
                panic!()
            }
        }
    }

    fn cheap_matmul(&self, input: &[S], r: usize) -> Vec<S> {
        let v = &self.params.v[r];
        let w_hat = &self.params.w_hat[r];
        let t = self.params.t;

        let mut new_state = vec![S::zero(); t];
        new_state[0] = self.params.mds[0][0];
        new_state[0].mul_assign(&input[0]);
        for i in 1..t {
            let mut tmp = w_hat[i - 1];
            tmp.mul_assign(&input[i]);
            new_state[0].add_assign(&tmp);
        }
        for i in 1..t {
            new_state[i] = input[0];
            new_state[i].mul_assign(&v[i - 1]);
            new_state[i].add_assign(&input[i]);
        }

        new_state
    }

    fn matmul(&self, input: &[S], mat: &[Vec<S>]) -> Vec<S> {
        let t = mat.len();
        debug_assert!(t == input.len());
        let mut out = vec![S::zero(); t];
        for row in 0..t {
            for (col, inp) in input.iter().enumerate().take(t) {
                let mut tmp = mat[row][col];
                tmp.mul_assign(inp);
                out[row].add_assign(&tmp);
            }
        }
        out
    }

    fn add_rc(&self, input: &[S], rc: &[S]) -> Vec<S> {
        input
            .iter()
            .zip(rc.iter())
            .map(|(a, b)| {
                let mut r = *a;
                r.add_assign(b);
                r
            })
            .collect()
    }
}