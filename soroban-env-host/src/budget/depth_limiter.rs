use crate::{
    budget::{Budget, BudgetImpl},
    host::error::TryBorrowOrErr,
    xdr::{ScErrorCode, ScErrorType},
    Error, HostError,
};

/// `DepthLimiter` is a trait designed for managing the depth of recursive operations.
/// It provides a mechanism to limit recursion depth, and defines the behavior upon
/// entering and leaving a recursion level.
pub trait DepthLimiter {
    /// Defines the behavior for entering a new recursion level.
    /// An `ExceededLimit` is returned if the new level exceeds the depth limit.
    fn enter(&mut self) -> Result<(), HostError>;

    /// Defines the behavior for leaving a recursion level.
    fn leave(&mut self) -> Result<(), HostError>;

    /// Wraps a given function `f` with depth limiting guards.
    /// It triggers an `enter` before, and a `leave` after the execution of `f`.
    ///
    /// # Parameters
    ///
    /// - `f`: The function to be executed under depth limit constraints.
    ///
    /// # Returns
    ///
    /// - `Err` if 1. the depth limit has been exceeded upon `enter` 2. `f` executes
    ///         with an error 3. if error occurs on `leave`.
    ///   `Ok` otherwise.
    fn with_limited_depth<T, F>(&mut self, f: F) -> Result<T, HostError>
    where
        F: FnOnce(&mut Self) -> Result<T, HostError>,
    {
        self.enter()?;
        let res = f(self);
        self.leave()?;
        res
    }
}

impl DepthLimiter for BudgetImpl {
    fn enter(&mut self) -> Result<(), HostError> {
        if let Some(depth) = self.depth_limit.checked_sub(1) {
            self.depth_limit = depth;
        } else {
            return Err(Error::from_type_and_code(
                ScErrorType::Context,
                ScErrorCode::ExceededLimit,
            )
            .into());
        }
        Ok(())
    }

    // `leave` should be called in tandem with `enter` such that the depth
    // doesn't exceed the initial depth limit.
    fn leave(&mut self) -> Result<(), HostError> {
        self.depth_limit = self.depth_limit.saturating_add(1);
        Ok(())
    }
}

impl DepthLimiter for Budget {
    fn enter(&mut self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.enter()
    }

    fn leave(&mut self) -> Result<(), HostError> {
        self.0.try_borrow_mut_or_err()?.leave()
    }
}
