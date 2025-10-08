use crate::{Error, xdr::{ScErrorType, ScErrorCode}};
use super::super::metered_scalar::MeteredScalar;

#[derive(Clone, Debug)]
pub struct Poseidon2Params<F: MeteredScalar> {
    pub(crate) t: usize, // statesize
    pub(crate) d: usize, // sbox degree
    pub(crate) rounds_f_beginning: usize,
    pub(crate) rounds_p: usize,
    pub(crate) rounds: usize,
    pub(crate) mat_internal_diag_m_1: Vec<F>,
    pub(crate) round_constants: Vec<Vec<F>>,
}

impl<F: MeteredScalar> Poseidon2Params<F> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        t: usize,
        d: usize,
        rounds_f: usize,
        rounds_p: usize,
        mat_internal_diag_m_1: Vec<F>,
        round_constants: Vec<Vec<F>>,
    ) -> Result<Self, Error> {
        if !(d == 3 || d == 5 || d == 7 || d == 11) {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }
        if mat_internal_diag_m_1.len() != t {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }
        if rounds_f % 2 != 0 {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }        
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;

        Ok(Poseidon2Params {
            t,
            d,
            rounds_f_beginning: r,
            rounds_p,
            rounds,
            mat_internal_diag_m_1,
            round_constants,
        })
    }
}
