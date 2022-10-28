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

We have run the benchmarks on a 2,7 GHz Quad-Core Intel Core i7 machine with 16 GB of RAM, on macOS 12.1. 

```shell
+-------------------+
|   Size of proofs  |
+-------------------+
|-------------------|
|   Trivial proofs  |
+-------------------+
| Results obtained by using the parameters suggested in paper.
+-------------------+
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 118760 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 445 | m = 2728 | nr parties = 3000; 99384 bytes
+-------------------+
| Hash: Blake2b 512 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 133936 bytes
+-------------------+
| Hash: Blake2b 256 |
+-------------------+
k = 554 | m = 3597 | nr parties = 3000; 113728 bytes
```

```shell
STM/Blake2b/Key registration/k: 25, m: 150, nr_parties: 300
                        time:   [388.95 ms 389.43 ms 389.90 ms]
STM/Blake2b/Play all lotteries/k: 25, m: 150, nr_parties: 300
                        time:   [699.76 µs 701.42 µs 703.63 µs]
STM/Blake2b/Aggregation/k: 25, m: 150, nr_parties: 300
                        time:   [18.888 ms 18.903 ms 18.920 ms]
STM/Blake2b/Verification/k: 25, m: 150, nr_parties: 300
                        time:   [2.1547 ms 2.1609 ms 2.1686 ms]

STM/Blake2b/Key registration/k: 250, m: 1523, nr_parties: 2000
                        time:   [2.5986 s 2.6042 s 2.6101 s]
STM/Blake2b/Play all lotteries/k: 250, m: 1523, nr_parties: 2000
                        time:   [5.9141 ms 5.9346 ms 5.9641 ms]
STM/Blake2b/Aggregation/k: 250, m: 1523, nr_parties: 2000
                        time:   [189.63 ms 190.10 ms 190.69 ms]
STM/Blake2b/Verification/k: 250, m: 1523, nr_parties: 2000
                        time:   [14.040 ms 14.143 ms 14.271 ms]
```

# ToDo list once we go public
- [ ] Upload mithril-core to crates.io
- [ ] Ensure that the badges are working
- [ ] Update links for crates and docs
- [ ] Error handling
