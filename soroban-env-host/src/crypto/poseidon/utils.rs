
use super::super::metered_scalar::MeteredScalar;
use crate::{host::metered_clone::MeteredIterator, xdr::{ScErrorCode, ScErrorType}, Host, HostError, Val};

pub(crate) fn sbox<S: MeteredScalar>(host: &Host, input: &Vec<S>, degree: usize) -> Result<Vec<S>, HostError> {
    input.iter().map(|el| sbox_p(host, el, degree)).metered_collect(host)?
}

pub(crate) fn sbox_p<S: MeteredScalar>(host: &Host, input: &S, degree: usize) -> Result<S, HostError> {
    input.metered_pow(&(degree as u64), host)
}

pub(crate) fn add_rc<S: MeteredScalar>(host: &Host, input: &Vec<S>, rc: &Vec<S>) -> Result<Vec<S>, HostError> {
    if input.len() != rc.len() {
        return Err(host.err(ScErrorType::Crypto, ScErrorCode::InvalidInput, "length mismatch between input array and round constants", &[Val::from_u32(input.len() as u32).into(), Val::from_u32(rc.len() as u32).into()]))
    }
    input
        .iter()
        .zip(rc.iter())
        .map(|(a, b)| {
            a.metered_add(b, host)
        })
        .metered_collect(host)?
}