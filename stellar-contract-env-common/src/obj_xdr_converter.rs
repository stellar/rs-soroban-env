use stellar_xdr::ScObject;

use super::Object;

// Trait XdrConverter is implemented by types that can convert XDR ScObject
// types to and from Objects.
pub trait ObjectXdrConverter: Sized + Clone {
    fn from_xdr_obj(&self, ob: Object) -> Result<ScObject, ()>;
    fn to_xdr_obj(&self, ob: &ScObject) -> Result<Object, ()>;
}
