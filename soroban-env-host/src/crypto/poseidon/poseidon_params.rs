use ark_ff::PrimeField;
use super::mat_utils;

#[derive(Clone, Debug)]
pub struct PoseidonParams<S: PrimeField> {
    pub(crate) t: usize, // statesize
    pub(crate) d: usize, // sbox degree
    pub(crate) rounds_f_beginning: usize,
    pub(crate) rounds_p: usize,
    #[allow(dead_code)]
    pub(crate) rounds_f_end: usize,
    pub(crate) rounds: usize,
    pub(crate) mds: Vec<Vec<S>>,
    pub(crate) round_constants: Vec<Vec<S>>,
}

impl<S: PrimeField> PoseidonParams<S> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        t: usize,
        d: usize,
        rounds_f: usize,
        rounds_p: usize,
        mds: &[Vec<S>],
        round_constants: &[Vec<S>],
    ) -> Self {
        assert!(d == 3 || d == 5 || d == 7);
        assert_eq!(mds.len(), t);
        assert_eq!(rounds_f % 2, 0);
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;

        PoseidonParams {
            t,
            d,
            rounds_f_beginning: r,
            rounds_p,
            rounds_f_end: r,
            rounds,
            mds: mds.to_owned(),
            round_constants: round_constants.to_owned(),
        }
    }
}
