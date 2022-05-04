mod guest;
pub use stellar_contract_env_common::*;

pub use guest::Guest;
pub type Object = EnvObj<Guest>;
