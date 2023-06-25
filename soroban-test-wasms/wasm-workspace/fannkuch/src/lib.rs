#![no_std]
use soroban_sdk::{contract, contractimpl, Env, Val};

#[contract]
pub struct Contract;

fn fannkuchredux<const N: usize>(env: Env) -> usize {
    let mut perm: [usize; N] = [0; N];
    let mut perm1: [usize; N] = [0; N];
    let mut count: [usize; N] = [0; N];
    let mut max_flips_count: usize = 0;
    let mut perm_count: usize = 0;
    let mut checksum: usize = 0;

    for i in 0..N {
        perm1[i] = i;
    }

    let mut r: usize = N;
    loop {
        while r != 1 {
            count[r - 1] = r;
            r -= 1;
        }

        for i in 0..N {
            perm[i] = perm1[i];
        }

        let mut k: usize;
        let mut flips_count: usize = 0;
        loop {
            k = perm[0];
            if k == 0 {
                break;
            }
            let k2 = (k + 1) >> 1;
            for i in 0..k2 {
                let temp = perm[i];
                perm[i] = perm[k - i];
                perm[k - i] = temp;
            }
            flips_count += 1;
        }

        max_flips_count = flips_count.max(max_flips_count);
        // NB: this is a slightly different checksum than the
        // one in the problem spec, to avoid arithmetic over/under
        // flow.
        if perm_count % 2 == 0 {
            checksum = checksum + flips_count;
        } else {
            checksum = 0xffff & (checksum * flips_count);
        }

        loop {
            if r == N {
                env.logs()
                    .add("checksum", &[Val::from_u32(checksum as u32).to_val()]);
                env.logs().add(
                    "max_flips",
                    &[Val::from_u32(max_flips_count as u32).to_val()],
                );
                return max_flips_count;
            }

            let perm0 = perm1[0];
            let mut i = 0;
            while i < r {
                let j = i + 1;
                perm1[i] = perm1[j];
                i = j;
            }
            perm1[r] = perm0;
            count[r] = count[r] - 1;
            if count[r] > 0 {
                break;
            }
            r += 1;
        }
        perm_count += 1;
    }
}

#[contractimpl]
impl Contract {
    pub fn main(env: Env) {
        fannkuchredux::<8>(env);
    }
}
