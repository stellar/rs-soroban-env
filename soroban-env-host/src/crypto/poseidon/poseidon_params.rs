use crate::{Error, xdr::{ScErrorType, ScErrorCode}};
use super::super::metered_scalar::MeteredScalar;

#[derive(Clone, Debug)]
pub struct PoseidonParams<S: MeteredScalar> {
    pub(crate) t: usize, // statesize
    pub(crate) d: usize, // sbox degree
    pub(crate) rounds_f_beginning: usize,
    pub(crate) rounds_p: usize,
    pub(crate) rounds: usize,
    pub(crate) mds: Vec<Vec<S>>,
    pub(crate) round_constants: Vec<Vec<S>>,
}

impl<S: MeteredScalar> PoseidonParams<S> {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        t: usize,
        d: usize,
        rounds_f: usize,
        rounds_p: usize,
        mds: Vec<Vec<S>>,
        round_constants: Vec<Vec<S>>,
    ) -> Result<Self, Error> {
        if !(d == 3 || d == 5 || d == 7) {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }
        if mds.len() != t {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }
        if rounds_f % 2 != 0 {
            return Err(Error::from_type_and_code(ScErrorType::Crypto, ScErrorCode::InvalidInput));
        }
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;

        Ok(PoseidonParams {
            t,
            d,
            rounds_f_beginning: r,
            rounds_p,
            rounds,
            mds,
            round_constants,
        })
    }
}
