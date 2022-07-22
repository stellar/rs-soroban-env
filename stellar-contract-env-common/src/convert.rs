pub trait TryConvert<F, T> {
    type Error;
    fn convert(&self, f: F) -> Result<T, Self::Error>;
}
