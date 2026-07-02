//! Tests for reclaiming (refunding) the memory budget held by a contract VM
//! when its frame exits. VM instantiation and the contract's linear memory are
//! deallocated when the frame's `Vm` is dropped, so the memory budget charged
//! for them is returned to the running `mem_bytes` count. The reported
//! (peak/high-water-mark) consumption is unaffected.

use crate::{
    budget::AsBudget,
    testutils::wasm::{wasm_module_with_mem_grow, wasm_module_with_mem_grow_then_trap},
    xdr::ContractCostType,
    Env, Host, HostError, Symbol,
};

const WASM_PAGE_SIZE: u64 = 0x10000; // 64 KiB

// Registers `wasm`, invokes its `test` function once, and returns
// `(peak_mem_bytes, live_mem_bytes)` after the top-level call completes.
//
// The `MemAlloc` memory cost model is set to `const=0, lin=1` so that growing
// linear memory by N bytes charges exactly N to `mem_bytes`; every other cost
// model is left zeroed (by `test_budget`), so the only refundable memory
// charged during the call is the VM's linear memory. `live` is the running
// `total_count` after the call (post-refund); `peak` is the high-water mark.
fn run_and_measure(wasm: &[u8], mem_limit: u64) -> Result<(u64, u64), HostError> {
    let host = Host::test_host_with_recording_footprint();
    let id = host.register_test_contract_wasm(wasm);
    let host = host
        .test_budget(u64::MAX, mem_limit)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let sym = Symbol::try_from_small_str("test").unwrap();
    let args = host.test_vec_obj::<u32>(&[])?;
    host.call(id, sym, args)?;
    let peak = host.as_budget().get_mem_bytes_consumed()?;
    let live = mem_limit.saturating_sub(host.as_budget().get_mem_bytes_remaining()?);
    Ok((peak, live))
}

/// A VM's linear memory is refunded when its frame exits: the surviving
/// (post-call) memory is independent of how much linear memory the contract
/// grew, while the peak reflects the momentarily-held memory.
#[test]
fn vm_linear_memory_is_refunded_on_frame_exit() -> Result<(), HostError> {
    let mem_limit = 1u64 << 30; // 1 GiB, plenty of headroom
    let pages_a = 10usize;
    let pages_b = 40usize;
    let (peak_a, live_a) = run_and_measure(&wasm_module_with_mem_grow(pages_a), mem_limit)?;
    let (peak_b, live_b) = run_and_measure(&wasm_module_with_mem_grow(pages_b), mem_limit)?;

    // The extra linear memory grown by run B (over run A) is fully refunded, so
    // the surviving live memory is identical regardless of how much linear
    // memory the contract grew.
    assert_eq!(
        live_a, live_b,
        "post-call live memory should be independent of linear memory grown (fully refunded)"
    );

    // The peak reflects the extra linear memory momentarily held during
    // execution: exactly (pages_b - pages_a) pages more.
    let extra = (pages_b - pages_a) as u64 * WASM_PAGE_SIZE;
    assert_eq!(
        peak_b - peak_a,
        extra,
        "peak should grow by exactly the extra linear memory grown"
    );

    // A refund actually happened: the peak exceeded the surviving live memory.
    assert!(
        peak_a > live_a,
        "peak {peak_a} should exceed live {live_a} (memory was refunded)"
    );
    Ok(())
}

/// Many sequential calls, each growing and freeing a large chunk of linear
/// memory, all succeed under a memory limit far below their cumulative total —
/// which is only possible because each call's memory is refunded before the
/// next one runs.
#[test]
fn sequential_vm_calls_reuse_refunded_memory() -> Result<(), HostError> {
    let grow_pages = 40usize;
    // ~2.68 MiB held during a single call (grown pages + the initial page).
    let per_call = (grow_pages as u64 + 1) * WASM_PAGE_SIZE;
    // Room for ~2 concurrent calls' worth, but far less than 10 calls' total.
    let mem_limit = per_call * 2;

    let host = Host::test_host_with_recording_footprint();
    let id = host.register_test_contract_wasm(&wasm_module_with_mem_grow(grow_pages));
    let host = host
        .test_budget(u64::MAX, mem_limit)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let sym = Symbol::try_from_small_str("test").unwrap();

    let n_calls = 10;
    for i in 0..n_calls {
        let args = host.test_vec_obj::<u32>(&[])?;
        host.call(id, sym, args)
            .unwrap_or_else(|e| panic!("call {i} failed (memory not reclaimed between calls?): {e:?}"));
    }

    // Despite 10 calls totalling ~26 MiB of grow-and-free, the peak never
    // needed more than roughly a single call's footprint.
    let peak = host.as_budget().get_mem_bytes_consumed()?;
    assert!(
        peak < mem_limit,
        "peak {peak} should stay under the {mem_limit}-byte limit thanks to refunds"
    );
    Ok(())
}

/// A VM's linear memory is refunded even when the frame exits via an error: the
/// `Store` (and its linear memory) is dropped on the error path too.
#[test]
fn vm_memory_refunded_even_on_trap() -> Result<(), HostError> {
    let mem_limit = 1u64 << 30;
    let grow_pages = 40usize;
    let linear = grow_pages as u64 * WASM_PAGE_SIZE;

    let host = Host::test_host_with_recording_footprint();
    let id = host.register_test_contract_wasm(&wasm_module_with_mem_grow_then_trap(grow_pages));
    let host = host
        .test_budget(u64::MAX, mem_limit)
        .enable_model(ContractCostType::MemAlloc, 0, 0, 0, 1);
    let sym = Symbol::try_from_small_str("test").unwrap();
    let args = host.test_vec_obj::<u32>(&[])?;

    let res = host.call(id, sym, args);
    assert!(res.is_err(), "the contract should have trapped");

    // The grown linear memory (~2.6 MiB) must not survive the trapped call.
    let live = mem_limit.saturating_sub(host.as_budget().get_mem_bytes_remaining()?);
    assert!(
        live < linear,
        "trapped call retained {live} bytes; the {linear} bytes of linear memory were not refunded"
    );
    Ok(())
}
