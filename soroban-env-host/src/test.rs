pub(crate) mod observe;

mod address;
mod auth;
mod basic;
mod budget_metering;
mod bytes;
mod complex;
mod crypto;
mod depth_limit;
mod dispatch;
mod e2e_tests;
mod event;
mod finish;
mod frame;
mod host;
mod hostile;
#[cfg(opt_build)]
mod hostile_opt;
mod invocation;
mod ledger;
mod lifecycle;
mod lifetime_extension;
mod linear_memory;
mod map;
#[cfg(feature = "testutils")]
mod metering_benchmark;
mod num;
mod post_mvp;
mod prng;
// FIXME: Re-enable this module after fixing the issue with the new `wasmi` version.
// mod protocol_gate;
mod stellar_asset_contract;
mod storage;
mod str;
mod symbol;
mod tuple;
mod vec;
