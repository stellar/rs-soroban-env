mod guest;
pub use stellar_contract_env_common::*;

use guest::Guest;
pub type GuestEnv = Guest;
pub type Object = EnvObj<Guest>;
