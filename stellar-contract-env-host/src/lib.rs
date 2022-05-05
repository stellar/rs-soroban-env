mod host;
mod host_object;

pub use host::{Host, RcHost, WeakHost};
pub use host_object::{HostMap, HostObject, HostObjectType, HostVal, HostVec};
pub use stellar_contract_env_common::*;

pub type HostEnv = RcHost;
pub type Object = EnvObj<WeakHost>;
