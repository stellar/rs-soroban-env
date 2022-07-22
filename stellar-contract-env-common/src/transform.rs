pub trait TryTransform<F, T> {
    type Error;
    fn transform(&self, f: F) -> Result<T, Self::Error>;
}
