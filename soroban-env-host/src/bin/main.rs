// This is a test application that embeds and runs the host. It exists at the
// moment just to provide a target for the `cackle` API-checker to observe the
// linking of soroban-env-host as a dependency, and thus check API uses inside
// soroban-env-host (this is probably a limitation of the `cackle` tool but at
// the moment I haven't figured out a workaround).
//
// In the future this might also provide some other top-level host functionality
// that users or developers might wish to run on the command-line.

use soroban_env_host::{budget::Budget, e2e_invoke::invoke_host_function, LedgerInfo};

fn main() {
    let budget = Budget::default();
    let enable_diagnostics = true;
    let encoded_host_fn = &[0u8];
    let encoded_resources = &[0u8];
    let encoded_source_account = &[0u8];
    let encoded_auth_entries = [[0u8]].iter();
    let ledger_info = LedgerInfo::default();
    let encoded_ledger_entries = [[0u8]].iter();
    let encoded_ttl_entries = [[0u8]].iter();
    let base_prng_seed = &[0u8];
    let mut diagnostic_events = Vec::new();
    let _ = invoke_host_function(
        &budget,
        enable_diagnostics,
        encoded_host_fn,
        encoded_resources,
        encoded_source_account,
        encoded_auth_entries,
        ledger_info,
        encoded_ledger_entries,
        encoded_ttl_entries,
        base_prng_seed,
        &mut diagnostic_events,
    );
}
