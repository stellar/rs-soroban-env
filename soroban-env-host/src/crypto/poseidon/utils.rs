
use super::super::metered_scalar::MeteredScalar;
use crate::{host::metered_clone::MeteredIterator, xdr::{ScErrorCode, ScErrorType}, Host, HostError};

pub fn sbox<S: MeteredScalar>(input: &Vec<S>, degree: usize, host: &Host) -> Result<Vec<S>, HostError> {
    input.iter().map(|el| sbox_p(el, degree, host)).metered_collect(host)?
}

pub fn sbox_p<S: MeteredScalar>(input: &S, degree: usize, host: &Host) -> Result<S, HostError> {
    let degree = degree as u64;
    if degree != 3 && degree != 5 && degree != 7 {
        return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "poseidon::sbox_p invalid degree", &[]))
    }
    input.metered_pow(&degree, host)
}

pub fn add_rc<S: MeteredScalar>(input: &Vec<S>, rc: &Vec<S>, host: &Host) -> Result<Vec<S>, HostError> {
    if input.len() != rc.len() {
        return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "length mismatch between input array and round constants", &[]))
    }
    input
        .iter()
        .zip(rc.iter())
        .map(|(a, b)| {
            a.metered_add(b, host)
        })
        .metered_collect(host)?
}