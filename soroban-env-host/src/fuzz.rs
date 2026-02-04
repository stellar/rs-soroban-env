//! Fuzz target entry points for external harnesses.
//!
//! This module provides functions that implement the core logic of fuzz targets,
//! callable from external harnesses like stellar-core or cargo-fuzz.
//!
//! The functions take raw bytes as input (as provided by libfuzzer/AFL/etc)
//! and return a result indicating whether the input was valid and what happened.

pub mod expr;
pub mod wasmi;

/// Result of running a fuzz target.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum FuzzResult {
    /// Input was successfully processed (contract may have returned error, but no internal errors)
    Ok,
    /// Input was rejected (too short, malformed, etc) - not a bug
    Reject,
    /// An internal error occurred - this is a bug we want to find
    InternalError,
}
