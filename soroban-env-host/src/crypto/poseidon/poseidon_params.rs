use crate::{crypto::poseidon::SUPPORTED_SBOX_DEGREES, Host, HostError, Val, ErrorHandler};
use super::super::metered_scalar::MeteredScalar;
use super::INVALID_INPUT;

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
    /// Creates new PoseidonParams with full validation using Host for error reporting.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        host: &Host,
        t: u32,
        d: u32,
        rounds_f: u32,
        rounds_p: u32,
        mds: Vec<Vec<S>>,
        round_constants: Vec<Vec<S>>,
    ) -> Result<Self, HostError> {
        if !SUPPORTED_SBOX_DEGREES.contains(&d) {
            return Err(host.error(INVALID_INPUT, "Unsupported s-box degree", &[Val::from_u32(d).into()]));
        }
        if mds.len() != t as usize {
            return Err(host.error(INVALID_INPUT, "mds matrix length does not match `t`", &[Val::from_u32(mds.len() as u32).into(), Val::from_u32(t).into()]));
        }
        if rounds_f % 2 != 0 {
            return Err(host.error(INVALID_INPUT, "`round_f` must be even", &[Val::from_u32(rounds_f).into()]));
        }
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;
        if round_constants.len() != rounds as usize {
            return Err(host.error(INVALID_INPUT, "round constants length does not match No. of rounds (rounds_f + rounds_p)", &[Val::from_u32(round_constants.len() as u32).into(), Val::from_u32(rounds).into()]));
        }

        Ok(PoseidonParams {
            t: t as usize,
            d: d as usize,
            rounds_f_beginning: r as usize,
            rounds_p: rounds_p as usize,
            rounds: rounds as usize,
            mds,
            round_constants,
        })
    }

    /// Creates new PoseidonParams with basic validation, suitable for LazyStatic initialization.
    /// Panics if parameters are invalid (do *not* use in production).
    #[cfg(test)]
    #[allow(clippy::too_many_arguments)]
    pub fn new_unchecked(
        t: u32,
        d: u32,
        rounds_f: u32,
        rounds_p: u32,
        mds: Vec<Vec<S>>,
        round_constants: Vec<Vec<S>>,
    ) -> Self {
        // Basic compile-time validations (will panic if violated)
        assert!(SUPPORTED_SBOX_DEGREES.contains(&d), "Unsupported s-box degree: {}", d);
        assert_eq!(mds.len(), t as usize, "mds matrix length {} does not match t={}", mds.len(), t);
        assert_eq!(rounds_f % 2, 0, "rounds_f must be even, got {}", rounds_f);
        
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;
        assert_eq!(round_constants.len(), rounds as usize, 
            "round constants length {} does not match rounds={}", round_constants.len(), rounds);

        PoseidonParams {
            t: t as usize,
            d: d as usize,
            rounds_f_beginning: r as usize,
            rounds_p: rounds_p as usize,
            rounds: rounds as usize,
            mds,
            round_constants,
        }
    }
}
