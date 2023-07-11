use stellar_xdr::DepthLimiter;

/// `DepthGuard` is a RAII guard for managing the depth of a `DepthLimiter`.
/// It automatically calls the `leave` method of the `DepthLimiter` when it goes out of scope.
pub struct DepthGuard<'a, D: DepthLimiter>(&'a D);

impl<'a, D: DepthLimiter> DepthGuard<'a, D> {
    /// Creates a new `DepthGuard`. The `enter` method of the `DepthLimiter` will be called.
    /// If the `enter` method returns an error due to the depth limit being exceeded, that error
    /// will be returned.
    ///
    /// - `d`: The `DepthLimiter` whose depth will be managed by the `DepthGuard`.
    #[allow(unused)]
    pub fn new(d: &'a D) -> Result<Self, D::DepthError> {
        d.enter()?;
        Ok(Self(d))
    }
}

impl<'a, D: DepthLimiter> Drop for DepthGuard<'a, D> {
    /// Calls the `leave` method of the `DepthLimiter` when the `DepthGuard` is dropped.
    fn drop(&mut self) {
        self.0.leave()
    }
}
