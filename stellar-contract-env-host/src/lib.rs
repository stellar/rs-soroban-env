mod host;
mod host_object;

pub use host::{Host, WeakHost};
pub use host_object::{HostMap, HostObject, HostObjectType, HostVal, HostVec};
pub use stellar_contract_env_common::*;

pub type Object = EnvObj<WeakHost>;
