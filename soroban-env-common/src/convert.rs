/// General trait representing a the ability of some object to perform a
/// (possibly unsuccessful) conversion between two other types.
pub trait TryConvert<F, T> {
    type Error;
    fn convert(&self, f: F) -> Result<T, Self::Error>;
}
