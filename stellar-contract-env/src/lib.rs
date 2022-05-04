#[cfg(feature = "guest")]
pub use stellar_contract_env_guest::*;

#[cfg(feature = "host")]
pub use stellar_contract_env_host::*;

#[cfg(all(feature = "guest", feature = "host"))]
compile_error!("guest and host features are mutually exclusive");

#[cfg(not(any(feature = "guest", feature = "host")))]
compile_error!("guest or host feature must be selected");
