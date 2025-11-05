# Mithril-stm [![CI workflow](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml/badge.svg)](https://github.com/input-output-hk/mithril/actions/workflows/ci.yml) [![crates.io](https://img.shields.io/crates/v/mithril-stm.svg)](https://crates.io/crates/mithril-stm) [![License](https://img.shields.io/badge/license-Apache%202.0-blue?style=flat-square)](https://github.com/input-output-hk/mithril/blob/main/LICENSE) [![Discord](https://img.shields.io/discord/500028886025895936.svg?logo=discord&style=flat-square)](https://discord.gg/5kaErDKDRq)

**This is a work in progress** 🛠

- `mithril-stm` is a Rust implementation of the scheme described in the paper [Mithril: Stake-based Threshold Multisignatures](https://eprint.iacr.org/2021/916.pdf) by Pyrros Chaidos and Aggelos Kiayias.
- The BLS12-381 signature library [blst](https://github.com/supranational/blst) is used as the backend for the implementation of STM.
- This implementation supports the _trivial concatenation proof system_ (Section 4.3). Other proof systems such as _Bulletproofs_ or _Halo2_ are not supported in this version.
- We implemented the concatenation proof system as batch proofs:
  - Individual signatures do not contain the Merkle path to prove membership of the avk. Instead, it is the role of the aggregator to generate such proofs. This allows for a more efficient implementation of batched membership proofs (or batched Merkle paths).
- Protocol documentation is given in [Mithril Protocol in depth](https://mithril.network/doc/mithril/mithril-protocol/protocol/).
- The API also includes _core verification_. This functionality allows a full node verifier (`CoreVerifier`) that is
  able to verify the signatures that are generated without the registration information, i.e., `avk`. A
  `CoreVerifier` is assumed to know identities of the signers, so, it does not need to check the registration.

- This library provides:
  - The implementation of the Stake-based Threshold Multisignatures
  - The implementation of `CoreVerifier`
  - Key registration procedure for STM signatures
  - The tests for the library functions, STM scheme, and `CoreVerifier`
  - Benchmark tests

## Pre-requisites

**Install Rust**

- Install a [correctly configured](https://www.rust-lang.org/learn/get-started) Rust toolchain (latest stable version).

- Install Build Tools `build-essential` and `m4`. For example, on Ubuntu/Debian/Mint, run `sudo apt install build-essential m4`.

## Download source code

```bash
# Download sources from github
git clone https://github.com/input-output-hk/mithril

# Go to sources directory
cd mithril-stm
```

## Compiling the library

```shell
cargo build --release
```

## Running the tests

For running rust tests, simply run (to run the tests faster, the use of `--release` flag is recommended):

```shell
cargo test --release
```

## Running the benches

```shell
cargo bench
```

## Example

The following is a simple example of the STM implementation:

```rust
use blake2::{digest::consts::U32, Blake2b};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;

use mithril_stm::{
    AggregateSignatureType, AggregationError, Clerk, Initializer, KeyRegistration, Parameters,
    Signer, SingleSignature,
};

type H = Blake2b<U32>;

let nparties = 32;
let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
let mut msg = [0u8; 16];
rng.fill_bytes(&mut msg);

//////////////////////////
// initialization phase //
//////////////////////////

let params = Parameters {
    k: 357,
    m: 2642,
    phi_f: 0.2,
};

let parties = (0..nparties)
    .into_iter()
    .map(|_| 1 + (rng.next_u64() % 9999))
    .collect::<Vec<_>>();

let mut key_reg = KeyRegistration::init();

let mut ps: Vec<Initializer> = Vec::with_capacity(nparties as usize);
for stake in parties {
    let p = Initializer::new(params, stake, &mut rng);
    key_reg.register(stake, p.get_verification_key_proof_of_possession()).unwrap();
    ps.push(p);
}

let closed_reg = key_reg.close();

let ps = ps
    .into_par_iter()
    .map(|p| p.create_signer(closed_reg.clone()).unwrap())
    .collect::<Vec<Signer<H>>>();

/////////////////////
// operation phase //
/////////////////////

let sigs = ps
    .par_iter()
    .filter_map(|p| p.sign(&msg))
    .collect::<Vec<SingleSignature>>();

let clerk = Clerk::new_clerk_from_signer(&ps[0]);
let avk = clerk.compute_aggregate_verification_key();

// Check all parties can verify every sig
for (s, p) in sigs.iter().zip(ps.iter()) {
    assert!(s.verify(&params, &p.get_verification_key(), &p.get_stake(), &avk, &msg).is_ok(), "Verification
    failed");
}

// Aggregate a concatenation proof with random parties
let msig = clerk.aggregate_signatures_with_type(&sigs, &msg, AggregateSignatureType::Concatenation);

match msig {
    Ok(aggr) => {
        println!("Aggregate ok");
        assert!(aggr.verify(&msg, &clerk.compute_aggregate_verification_key(), &params).is_ok());
    }
    Err(error) => match error.downcast_ref::<AggregationError>() {
        Some(AggregationError::NotEnoughSignatures(n, k)) => {
            println!("Not enough signatures");
            assert!(n < &params.k && k == &params.k)
        },
        Some(AggregationError::UsizeConversionInvalid) => {
            println!("Invalid usize conversion");
        },
        Some(AggregationError::UnsupportedProofSystem(aggregate_signature_type)) => {
            println!("Unsupported proof system: {:?}", aggregate_signature_type);
        },
        None => {
            println!("Unexpected error during aggregation: {:?}", error);
        }
    },
}
```

## Benchmarks

Here we give the benchmark results of STM for size and time. We run the benchmarks on macOS 12.6 on an Apple M1 Pro machine with 16 GB of RAM.

Note that the size of an individual signature with one valid index is **72 bytes** (48 bytes from `sigma`, 8 bytes from `party_index`, 8 bytes for the `length` of winning indices and at least 8 bytes for a single winning `index`) and increases linearly in the length of valid indices (where an index is 8 bytes).

```shell
+----------------------+
| Size of benchmarks   |
+----------------------+
| Results obtained by using the parameters suggested by the paper.
+----------------------+
+----------------------+
| Aggregate signatures |
+----------------------+
+----------------------+
| Hash: Blake2b 512    |
+----------------------+
k = 445 | m = 2728 | nr parties = 3000; 118760 bytes
+----------------------+
| Hash: Blake2b 256    |
+----------------------+
k = 445 | m = 2728 | nr parties = 3000; 99384 bytes
+----------------------+
+----------------------+
| Aggregate signatures |
+----------------------+
| Hash: Blake2b 512    |
+----------------------+
k = 554 | m = 3597 | nr parties = 3000; 133936 bytes
+----------------------+
| Hash: Blake2b 256    |
+----------------------+
k = 554 | m = 3597 | nr parties = 3000; 113728 bytes
```

```shell
STM/Blake2b/Key registration/k: 25, m: 150, nr_parties: 300
                        time:   [409.70 ms 426.81 ms 446.30 ms]
STM/Blake2b/Play all lotteries/k: 25, m: 150, nr_parties: 300
                        time:   [696.58 µs 697.62 µs 698.75 µs]
STM/Blake2b/Aggregation/k: 25, m: 150, nr_parties: 300
                        time:   [18.765 ms 18.775 ms 18.785 ms]
STM/Blake2b/Verification/k: 25, m: 150, nr_parties: 300
                        time:   [2.1577 ms 2.1715 ms 2.1915 ms]

STM/Blake2b/Key registration/k: 250, m: 1523, nr_parties: 2000
                        time:   [2.5807 s 2.5880 s 2.5961 s]
STM/Blake2b/Play all lotteries/k: 250, m: 1523, nr_parties: 2000
                        time:   [5.9318 ms 5.9447 ms 5.9582 ms]
STM/Blake2b/Aggregation/k: 250, m: 1523, nr_parties: 2000
                        time:   [190.81 ms 191.15 ms 191.54 ms]
STM/Blake2b/Verification/k: 250, m: 1523, nr_parties: 2000
                        time:   [13.944 ms 14.010 ms 14.077 ms]
```
