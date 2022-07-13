#![allow(dead_code)]

use arrayvec::ArrayVec;
use stellar_contract_env_common::RawVal;

// TODO: update this when ContractEvent shows up in the XDR defns.
// TODO: optimize storage on this to use pools / bumpalo / etc.
#[derive(Clone, Debug)]
pub(crate) enum HostEvent {
    Contract(/*ContractEvent*/),
    // Same size as LargeDebug but doesn't chew up a heap allocation.
    SmallDebug {
        msg: &'static str,
        // ArrayVec<RawVal, 2> is 3 words: RawVal, RawVal, len.
        args: ArrayVec<RawVal, 2>,
    },
    // Arbitrary set of k/v arguments.
    LargeDebug {
        msg: &'static str,
        // Vec is 3 words: ptr, len, capacity.
        args: Vec<RawVal>,
    },
}

#[derive(Clone, Debug, Default)]
pub struct Events(Vec<HostEvent>);

impl Events {
    pub(crate) fn add_debug_event(&mut self, msg: &'static str, in_args: &[RawVal]) {
        let debug = if in_args.len() <= 2 {
            let args: ArrayVec<RawVal, 2> = ArrayVec::from_iter(in_args.iter().cloned());
            HostEvent::SmallDebug { msg, args }
        } else {
            let args = in_args.into();
            HostEvent::LargeDebug { msg, args }
        };
        self.0.push(debug);
    }
}
