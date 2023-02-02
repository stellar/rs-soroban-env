//! This module contains version constants (and other metadata) that are
//! embedded in binaries (especially WASM binaries) compiled against a
//! particular version of this crate. Versioning at this level provides an early
//! diagnostic check for compatibility between a loaded WASM binary and the
//! [Env](crate::Env) interface provided by the host, rather than a cryptic failure
//! due to a runtime host function signature mismatch.

// There are two different notions of versioning in this system:
//
//   1. Interface versioning
//   2. Implementation versioning
//
// Interface versioning controls the willingness of a host to _try_ to run a
// _new_ transaction against some contract. For this, a contract embeds an
// interface version number _inside itself_ and the host inspects it before
// starting the contract. If the contract's interface version is outside the
// supported range for a host, the host will fail the transaction with a useful
// error rather than an inscrutable misbehaviour (link error, data corruption,
// etc.) during execution.
//
// At present the supported interface-version range is always just a single
// point, and it is hard-wired into the host (in
// `soroban_env_host::vm::check_meta_section`) as a comparison against the
// current [`meta::INTERFACE_VERSION`] declared by the macro below. Every time
// this declaration changes, the host compiled with it implicitly drops support
// for old contracts compiled against the old version number; this is due to us
// existing in a "pre-API-stability" development regime, where every change
// is a compatibility break.
//
//  In the future when we commit to API stability two things will change:
//
//   1. The value will stop being hard-wired; it will change based on the
//      current ledger, as a config value that varies over time based on
//      consensus.
//
//   2. It will (mostly) have a fixed lower bound and only ever have its upper
//      bound expand, since that is what "API stability" means: old code still
//      runs on new hosts. The "mostly" qualifier here covers the case where we
//      have to reset the lower bound to expire old APIs (used by old contracts)
//      if they prove to be a security risk; this will only happen in extreme
//      cases, hopefully never.
//
// Implementation versioning is different, and has more to do with ensuring that
// all validators that _do_ decide to execute a transaction -- whether new _or
// old_ -- execute it on an observably-identical software version, such that an
// arbitrarily subtle dependency on implementation quirks does not cause any
// bit-level divergence in the transaction results. Implementation versioning is
// done in stellar-core itself, by maintaining multiple versions of the host
// crate: strictly identical versions for new transactions entering the network,
// and observably-identical ones for replay of historical transactions (with a
// policy to allow expiring old versions that differ only in ways no
// transactions in the public network history actually observe). Implementation
// versioning will also be done by storing a version on-chain that changes by
// consensus, but the host implementation version number is _not_ compiled into
// a contract, since the same contract will be expected to run against multliple
// implementations over a long period of time.

pub const ENV_META_V0_SECTION_NAME: &'static str = "contractenvmetav0";

soroban_env_macros::generate_env_meta_consts!(
    interface_version: 28,
);
