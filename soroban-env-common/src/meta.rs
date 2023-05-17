//! This module contains version constants (and other metadata) that are
//! embedded in binaries (especially WASM binaries) compiled against a
//! particular version of this crate. Versioning at this level provides an early
//! diagnostic check for compatibility between a loaded WASM binary and the
//! [Env](crate::Env) interface provided by the host, rather than a cryptic failure
//! due to a runtime host function signature mismatch.

// Currently the only constant is `INTERFACE_VERSION` which is a u64 with a low
// and high component. The low component is a pre-release version which should
// be zero any time you make a real release, and the high component is the
// ledger version / protocol version (the two terms are used interchangably in
// the stellar codebase), which should both match the major release version of
// soroban and the major release version of stellar-core that it's embedded
// within.
//
// Protocol numbers will be checked for ordered compatibility (a host will only
// run protocols it's at least as new as) whereas pre-release numbers will be
// checked for _exact_ identity. Any pre-release number is considered
// incompatible with every other pre-release number, requires recompiling
// contracts.
//
// Any change to the logical interface of a released version of soroban (with a
// nonzero major version):
//
//   - Must be accompanied by a protocol-number increment, so the network
//     switches behaviour in consensus, and can gate backward-compatibility code
//     on protocol transitions during replay.
//
//   - Should in most cases made in a way that's backward compatible with old
//     versions, so that an old contract can continue to run on a new host.
//     Exceptions can be made for intentional breakage such as deprecating bad
//     functionality or insecure functions, but keep in mind any code that was
//     exercised in a recorded transaction _must_ stay around at least gated by
//     protocol so it can replay correctly.
//
// When a release goes out, it should have pre-release version zero. During
// development of a new release, or before the initial release of soroban, a
// nonzero pre-release number can be used to force recompiles on interface
// changes.

pub const ENV_META_V0_SECTION_NAME: &str = "contractenvmetav0";

soroban_env_macros::generate_env_meta_consts!(
    ledger_protocol_version: 20,
    pre_release_version: 39,
);

pub fn get_ledger_protocol_version(interface_version: u64) -> u32 {
    // The ledger protocol version is the high 32 bits of INTERFACE_VERSION
    (interface_version >> 32) as u32
}

pub fn get_pre_release_version(interface_version: u64) -> u32 {
    // The pre-release version is the low 32 bits of INTERFACE_VERSION
    interface_version as u32
}
