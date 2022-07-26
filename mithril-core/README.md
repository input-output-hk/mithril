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
+----------------+
| Size of proofs |
+----------------+
|----------------|
| Trivial proofs |
|Curve: Bls12_381|
+----------------+
k = 8; 6968 bytes
k = 16; 13496 bytes
k = 32; 26552 bytes
k = 64; 52664 bytes
k = 128; 104888 bytes
k = 256; 209336 bytes
k = 512; 418232 bytes
```

```shell
STM/"Bls12_381"/Key registration/32                                                                            
                        time:   [295.76 ms 367.82 ms 295.76 ms]
STM/"Bls12_381"/Key registration/64                                                                            
                        time:   [565.63 ms 573.72 ms 589.60 ms]
STM/"Bls12_381"/Key registration/128                                                                            
                        time:   [1.1800 s 1.1922 s 1.2124 s]
STM/"Bls12_381"/Key registration/256                                                                            
                        time:   [2.4396 s 2.5564 s 2.7018 s]
STM/"Bls12_381"/Key registration/512                                                                            
                        time:   [4.2276 s 4.3262 s 4.4616 s]
STM/"Bls12_381"/Key registration/1024                                                                            
                        time:   [8.0138 s 8.2035 s 8.3750 s]
STM/"Bls12_381"/Key registration/2048                                                                            
                        time:   [17.938 s 18.145 s 18.448 s]
STM/"Bls12_381"/Key registration/4096                                                                            
                        time:   [32.553 s 32.997 s 33.457 s]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 50                                                                            
                        time:   [40.488 ms 41.386 ms 42.959 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 100                                                                            
                        time:   [70.278 ms 72.137 ms 75.313 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 150                                                                            
                        time:   [95.440 ms 101.44 ms 102.21 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 200                                                                            
                        time:   [130.12 ms 134.24 ms 137.99 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 250                                                                            
                        time:   [175.21 ms 175.15 ms 176.48 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 300                                                                            
                        time:   [173.27 ms 176.25 ms 178.82 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 350                                                                            
                        time:   [211.78 ms 213.77 ms 215.16 ms]
STM/"Bls12_381"/Play all lotteries/k: 8, m: 400                                                                            
                        time:   [231.01 ms 239.26 ms 244.34 ms]
STM/"Bls12_381"/Aggregation/k: 8, m: 50                                                                            
                        time:   [28.582 ms 28.715 ms 28.778 ms]
STM/"Bls12_381"/Aggregation/k: 16, m: 50                                                                            
                        time:   [56.970 ms 57.116 ms 57.718 ms]
STM/"Bls12_381"/Aggregation/k: 32, m: 50                                                                            
                        time:   [111.46 ms 111.74 ms 111.92 ms]
```

# ToDo list once we go public
- [ ] Upload mithril-core to crates.io
- [ ] Ensure that the badges are working
- [ ] Update links for crates and docs
- [ ] Re-run benchmarks
- [ ] Error handling
