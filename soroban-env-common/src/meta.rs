//! This module contains version constants (and other metadata) that are
//! embedded in binaries (especially WASM binaries) compiled against a
//! particular version of this crate. Versioning at this level provides an early
//! diagnostic check for compatibility between a loaded WASM binary and the
//! [Env](crate::Env) interface provided by the host, rather than a cryptic failure
//! due to a runtime host function signature mismatch.

// Currently the only constant is `INTERFACE_VERSION` which is a struct
// containing a 32-bit protocol version and 32-bit pre-release version. The
// pre-release version should be zero any time you make a real release, and the
// protocol version is the ledger version / protocol version (the two terms are
// used interchangably in the stellar codebase), which should both match the
// major release version of soroban and the major release version of
// stellar-core that it's embedded within.
//
// Protocol numbers will be checked for ordered compatibility (a host will only
// run contracts built for same-or-older protocol versions than its own) whereas
// pre-release numbers will be checked for _exact_ identity. Any pre-release
// number is considered incompatible with every other pre-release number,
// requires recompiling contracts.
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

use crate::xdr::ScEnvMetaEntryInterfaceVersion;

pub const ENV_META_V0_SECTION_NAME: &str = "contractenvmetav0";

// If the "next" feature is enabled, we're building from the "next" xdr
// definitions branch and rust module, which contains experimental, unstable,
// in-development definitions we aren't even close to ready to release to the
// network. This is typically associated with a one-higher-than-released
// protocol number for testing purposes.
#[cfg(feature = "next")]
soroban_env_macros::generate_env_meta_consts!(
    ledger_protocol_version: 23,
    pre_release_version: 1,
);

// If the "next" feature is _not_ enabled, it means we're building for a
// nearly-current release to the network and are using the "curr" xdr branch and
// module. This will therefore be associated with a current or nearly-current
// network protocol number.
#[cfg(not(feature = "next"))]
soroban_env_macros::generate_env_meta_consts!(
    ledger_protocol_version: 22,
    pre_release_version: 0,
);
