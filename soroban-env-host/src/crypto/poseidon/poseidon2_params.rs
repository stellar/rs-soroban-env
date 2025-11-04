use crate::{
    crypto::{
        metered_scalar::MeteredScalar,
        poseidon::{INVALID_INPUT, SUPPORTED_SBOX_DEGREES},
    },
    ErrorHandler, Host, HostError, Val,
};

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
    /// Creates new Poseidon2Params with full validation using Host for error reporting.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        host: &Host,
        t: u32,
        d: u32,
        rounds_f: u32,
        rounds_p: u32,
        mat_internal_diag_m_1: Vec<F>,
        round_constants: Vec<Vec<F>>,
    ) -> Result<Self, HostError> {
        if !SUPPORTED_SBOX_DEGREES.contains(&d) {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: unsupported s-box degree",
                &[Val::from_u32(d).into()],
            ));
        }
        if mat_internal_diag_m_1.len() != t as usize {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: mat_internal_diag_m_1 length does not match `t`",
                &[
                    Val::from_u32(mat_internal_diag_m_1.len() as u32).into(),
                    Val::from_u32(t).into(),
                ],
            ));
        }
        if rounds_f % 2 != 0 {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: `rounds_f` must be even",
                &[Val::from_u32(rounds_f).into()],
            ));
        }
        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;
        if round_constants.len() != rounds as usize {
            return Err(host.error(
                INVALID_INPUT,
                "Poseidon2: round constants length does not match No. of rounds (rounds_f + rounds_p)",
                &[
                    Val::from_u32(round_constants.len() as u32).into(),
                    Val::from_u32(rounds).into(),
                ],
            ));
        }

        Ok(Poseidon2Params {
            t: t as usize,
            d: d as usize,
            rounds_f_beginning: r as usize,
            rounds_p: rounds_p as usize,
            rounds: rounds as usize,
            mat_internal_diag_m_1,
            round_constants,
        })
    }

    /// Creates new Poseidon2Params with basic validation, suitable for LazyStatic initialization.
    /// Panics if parameters are invalid (do *not* use in production).
    #[cfg(test)]
    #[allow(clippy::too_many_arguments)]
    pub fn new_unchecked(
        t: u32,
        d: u32,
        rounds_f: u32,
        rounds_p: u32,
        mat_internal_diag_m_1: Vec<F>,
        round_constants: Vec<Vec<F>>,
    ) -> Self {
        // Basic compile-time validations (will panic if violated)
        assert!(
            SUPPORTED_SBOX_DEGREES.contains(&d),
            "Unsupported s-box degree: {}",
            d
        );
        assert_eq!(
            mat_internal_diag_m_1.len(),
            t as usize,
            "mat_internal_diag_m_1 length {} does not match t={}",
            mat_internal_diag_m_1.len(),
            t
        );
        assert_eq!(rounds_f % 2, 0, "rounds_f must be even, got {}", rounds_f);

        let r = rounds_f / 2;
        let rounds = rounds_f + rounds_p;
        assert_eq!(
            round_constants.len(),
            rounds as usize,
            "round constants length {} does not match rounds={}",
            round_constants.len(),
            rounds
        );

        Poseidon2Params {
            t: t as usize,
            d: d as usize,
            rounds_f_beginning: r as usize,
            rounds_p: rounds_p as usize,
            rounds: rounds as usize,
            mat_internal_diag_m_1,
            round_constants,
        }
    }
}
