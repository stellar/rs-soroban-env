#![no_std]

//! This crate provides the [Guest] type, the "stub" implementation of the [Env]
//! interface used for communicating between a contract guest and its host.
//!
//! It also re-exports all of the content of the [soroban_env_common] crate for
//! use by guest code. Most of the type and module definitions visible here are
//! actually defined in the common crate.

mod guest;

pub use guest::Guest;
pub use soroban_env_common::*;
