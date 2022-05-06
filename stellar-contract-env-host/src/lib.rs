mod host;
pub(crate) mod host_object;
pub(crate) mod weak_host;

#[cfg(test)]
mod test;

pub use host::Host;
pub use stellar_contract_env_common::*;
