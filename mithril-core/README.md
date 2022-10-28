Mithril-core ![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg) ![crates.io](https://img.shields.io/crates/v/mithril_core.svg)
=======
This crate is ongoing work, has not been audited, and it's API is by no means final. Do not use in production.

### A rust implementation of Stake-based Threshold Multisignatures (STMs)
`mithril-core` implements Stake-based Threshold Multisignatures as described in the paper
[Mithril: Stake-based Threshold Multisignatures](https://eprint.iacr.org/2021/916.pdf), by
Pyrros Chaidos and Aggelos Kiayias.

This library uses zkcrypto's implementation of curve [BLS12-381](https://github.com/zkcrypto/bls12_381)
by default for implementing the multisignature scheme. One can optionally choose the
[blst](https://github.com/supranational/blst) backend (by using the feature `blast`),
but this is not recommended due to some [flaky tests](https://github.com/input-output-hk/mithril/issues/207)
That are still being resolved. We
currently only support the trivial concatenation proof system (Section 4.3) and do not support
other proof systems such as Bulletproofs or Halo2.

This library provides implementations of:

* Stake-based Threshold Multisignatures
* Key registration procedure for STM signatures

The user-facing documentation for the above modules can be found [here]().

# Example
```rust
use mithril::key_reg::KeyReg;
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSigner};
use rayon::prelude::*;

use mithril::error::AggregationFailure;
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = blake2::Blake2b;

fn main() {
    let nparties = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    //////////////////////////
    // initialization phase //
    //////////////////////////

    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    let parties = (0..nparties)
        .into_iter()
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::init();

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties as usize);
    for stake in parties {
        let p = StmInitializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
        ps.push(p);
    }

    let closed_reg = key_reg.close();

    let ps = ps
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()))
        .collect::<Vec<StmSigner<H>>>();

    /////////////////////
    // operation phase //
    /////////////////////

    let sigs = ps
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<StmSig<H>>>();

    let clerk = StmClerk::from_signer(&ps[0]);
    let avk = clerk.compute_avk();

    // Check all parties can verify every sig
    for s in sigs.iter() {
        assert!(s.verify(&params, &avk, &msg).is_ok(), "Verification failed");
    }

    // Aggregate with random parties
    let msig = clerk.aggregate(&sigs, &msg);

    assert!(msig.is_ok(), "aggregation failed");
    assert!(msig.unwrap().verify(&msg, &clerk.compute_avk(), &params).is_ok());
}
```

# Test and Benchmarks
You can run tests of the library using `cargo test` (we recommend to use the `--release` flag, otherwise
the tests might take a while) and run benchmarks using `cargo bench`. This crate uses `criterion` to run
benchmarks.

We have run the benchmarks on an Apple M1 Pro machine with 16 GB of RAM, on macOS 12.6.

Note that single signatures in batch compat version does not depend on any variable and size of an individual signature is `176` bytes.

```shell
+----------------------+
| Size of benchmarks   |
+----------------------+
| Results obtained by using the parameters suggested in paper.
+----------------------+
+----------------------+
| Aggregate signatures |
+----------------------+
+----------------------+
| Hash: Blake2b 512    |
+----------------------+
k = 445 | m = 2728 | nr parties = 3000; 118760 bytes (old version = 356632 bytes)
+----------------------+
| Hash: Blake2b 256    |
+----------------------+
k = 445 | m = 2728 | nr parties = 3000; 99384 bytes (old version = 222536 bytes)
+----------------------+
+----------------------+
| Aggregate signatures |
+----------------------+
| Hash: Blake2b 512    |
+----------------------+
k = 554 | m = 3597 | nr parties = 3000; 133936 bytes (old version = 419808 bytes)
+----------------------+
| Hash: Blake2b 256    |
+----------------------+
k = 554 | m = 3597 | nr parties = 3000; 113728 bytes (old version = 261488 bytes)
make build && ./mithrildemo --nparties 16 -k 5 -m 5 --phi-f 0.9

```

```shell
STM/Blake2b/Key registration/k: 25, m: 150, nr_parties: 300
                        time:   [409.70 ms 426.81 ms 446.30 ms]
                        change: [+2.3183% +7.5525% +13.315%] (p = 0.02 < 0.05)
                        Performance has regressed.

STM/Blake2b/Play all lotteries/k: 25, m: 150, nr_parties: 300
                        time:   [696.58 µs 697.62 µs 698.75 µs]
                        change: [-1.1128% -0.8545% -0.5490%] (p = 0.00 < 0.05)
                        Change within noise threshold.

STM/Blake2b/Aggregation/k: 25, m: 150, nr_parties: 300
                        time:   [18.765 ms 18.775 ms 18.785 ms]
                        change: [-1.5665% -1.4456% -1.3236%] (p = 0.00 < 0.05)
                        Performance has improved.

STM/Blake2b/Verification/k: 25, m: 150, nr_parties: 300
                        time:   [2.1577 ms 2.1715 ms 2.1915 ms]
                        change: [-0.0379% +0.5723% +1.6451%] (p = 0.14 > 0.05)
                        No change in performance detected.


STM/Blake2b/Key registration/k: 250, m: 1523, nr_parties: 2000
                        time:   [2.5807 s 2.5880 s 2.5961 s]
                        change: [-1.7298% -0.2763% +0.7870%] (p = 0.78 > 0.05)
                        No change in performance detected.

STM/Blake2b/Play all lotteries/k: 250, m: 1523, nr_parties: 2000
                        time:   [5.9318 ms 5.9447 ms 5.9582 ms]
                        change: [+1.1467% +1.4105% +1.6686%] (p = 0.00 < 0.05)
                        Performance has regressed.

STM/Blake2b/Aggregation/k: 250, m: 1523, nr_parties: 2000
                        time:   [190.81 ms 191.15 ms 191.54 ms]
                        change: [-0.2176% +0.0444% +0.3235%] (p = 0.82 > 0.05)
                        No change in performance detected.

STM/Blake2b/Verification/k: 250, m: 1523, nr_parties: 2000
                        time:   [13.944 ms 14.010 ms 14.077 ms]
                        change: [-1.0844% -0.6175% -0.0397%] (p = 0.03 < 0.05)
                        Change within noise threshold.


```

# ToDo list once we go public
- [ ] Upload mithril-core to crates.io
- [ ] Ensure that the badges are working
- [ ] Update links for crates and docs
- [ ] Error handling
