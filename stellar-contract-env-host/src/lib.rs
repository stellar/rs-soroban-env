mod host;
mod host_object;
use std::rc::Rc;

pub use host::{Host, WeakHost};
pub use host_object::{HostMap, HostObject, HostObjectType, HostVal, HostVec};
pub use stellar_contract_env_common::*;

pub type HostEnv = Rc<Host>;
pub type Object = EnvObj<WeakHost>;
