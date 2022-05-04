#[cfg(feature = "guest")]
pub use stellar_contract_env_guest::*;

#[cfg(feature = "host")]
pub use stellar_contract_env_host::*;

#[cfg(all(feature = "guest", feature = "host"))]
compile_error!("guest and host features are mutually exclusive");
