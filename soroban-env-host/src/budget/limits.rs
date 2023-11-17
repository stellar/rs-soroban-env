use crate::{
    budget::{Budget, BudgetImpl},
    host::error::TryBorrowOrErr,
    xdr::{Limits, ScErrorCode, ScErrorType},
    Error, HostError,
};

/// These constants are used to set limits on recursion and data length in the
/// context of XDR (de)serialization. They serve as safeguards agaist both
/// exccessive stack allocation, which could cause an unrecoverable `SIGABRT`,
/// and exccessive heap memory allocation.
pub const DEFAULT_XDR_RW_LIMITS: Limits = Limits {
    // recursion limit for reading and writing XDR structures.
    depth: 500,
    // maximum byte length for a data structure during serialization and
    // deserialization to and from the XDR format. The limit of 16MiB
    // corresponds to the overlay maximum message size.
    len: 0x1000000,
};

/// - `DEFAULT_HOST_DEPTH_LIMIT`: This limit applies to the host environment. It
///   guards recursion paths involving the `Env` and `Budget`, particularly
///   during operations like conversion, comparison, and deep cloning. The limit
///   is strategically checked at critical recursion points, such as when
///   encountering a `Val`. As the actual stack usage can be higher,
///   `DEFAULT_HOST_DEPTH_LIMIT` is conservatively set to a lower value than the
///   XDR limit.
pub const DEFAULT_HOST_DEPTH_LIMIT: u32 = 100;

// These are some sane values, however the embedder should typically customize
// these to match the network config.
pub(crate) const DEFAULT_CPU_INSN_LIMIT: u64 = 100_000_000;
pub(crate) const DEFAULT_MEM_BYTES_LIMIT: u64 = 40 * 1024 * 1024; // 40MB

/// `DepthLimiter` is a trait designed for managing the depth of recursive operations.
/// It provides a mechanism to limit recursion depth, and defines the behavior upon
/// entering and leaving a recursion level.
pub(crate) trait DepthLimiter {
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
