use blake2::digest::{Digest, FixedOutput};
use blake2::{digest::consts::U32, Blake2b};
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril_stm::{
    CoreVerifier, KeyReg, Stake, StmClerk, StmInitializer, StmParameters, StmSigner,
    StmVerificationKey,
};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;
use std::fmt::Debug;

/// This benchmark framework is not ideal. We really have to think what is the best mechanism for
/// benchmarking these signatures, over which parameters, how many times to run them, etc:
/// * Registration depends on the number of parties (should be constant, as it is a lookup table)
/// * Signing depends on the parameter `m`, as it defines the number of lotteries a user can play
/// * Aggregation depends on `k`.
fn stm_benches<H>(c: &mut Criterion, nr_parties: usize, params: StmParameters, hashing_alg: &str)
where
    H: Clone + Debug + Digest + Send + Sync + FixedOutput + Default,
{
    let mut group = c.benchmark_group(format!("STM/{hashing_alg}"));
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let param_string = format!(
        "k: {}, m: {}, nr_parties: {}",
        params.k, params.m, nr_parties
    );

    let stakes = (0..nr_parties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nr_parties);
    for stake in stakes {
        initializers.push(StmInitializer::setup(params, stake, &mut rng));
    }
    let mut key_reg = KeyReg::init();

    group.bench_function(BenchmarkId::new("Key registration", &param_string), |b| {
        b.iter(|| {
            // We need to initialise the key_reg at each iteration
            key_reg = KeyReg::init();
            for p in initializers.iter() {
                key_reg.register(p.stake, p.verification_key()).unwrap();
            }
        })
    });

    let closed_reg = key_reg.close();

    let signers = initializers
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<StmSigner<H>>>();

    group.bench_function(BenchmarkId::new("Play all lotteries", &param_string), |b| {
        b.iter(|| {
            signers[0].sign(&msg);
        })
    });

    let sigs = signers
        .par_iter()
        .filter_map(|p| p.sign(&msg))
        .collect::<Vec<_>>();

    let clerk = StmClerk::from_signer(&signers[0]);

    group.bench_function(BenchmarkId::new("Aggregation", &param_string), |b| {
        b.iter(|| clerk.aggregate(&sigs, &msg))
    });
}

fn core_verifier_benches<H>(c: &mut Criterion, nr_parties: usize, params: StmParameters)
where
    H: Clone + Debug + Digest + Send + Sync + FixedOutput + Default,
{
    let mut group = c.benchmark_group("Core verifier");
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let mut public_signers: Vec<(StmVerificationKey, Stake)> = Vec::with_capacity(nr_parties);
    let mut initializers: Vec<StmInitializer> = Vec::with_capacity(nr_parties);

    let param_string = format!(
        "k: {}, m: {}, nr_parties: {}",
        params.k, params.m, nr_parties
    );

    let stakes = (0..nr_parties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    for stake in stakes {
        let initializer = StmInitializer::setup(params, stake, &mut rng);
        initializers.push(initializer.clone());
        public_signers.push((initializer.verification_key().vk, initializer.stake));
    }

    let core_verifier = CoreVerifier::setup(&public_signers);

    let signers: Vec<StmSigner<H>> = initializers
        .into_iter()
        .filter_map(|s| s.new_core_signer(&core_verifier.eligible_parties))
        .collect();

    group.bench_function(BenchmarkId::new("Play all lotteries", &param_string), |b| {
        b.iter(|| {
            signers[0].core_sign(&msg, core_verifier.total_stake);
        })
    });

    let signatures = signers
        .par_iter()
        .filter_map(|p| p.core_sign(&msg, core_verifier.total_stake))
        .collect::<Vec<_>>();

    group.bench_function(BenchmarkId::new("Core verification", &param_string), |b| {
        b.iter(|| core_verifier.verify(&signatures, &params, &msg))
    });
}

fn stm_benches_blake_300(c: &mut Criterion) {
    stm_benches::<Blake2b<U32>>(
        c,
        300,
        StmParameters {
            m: 150,
            k: 25,
            phi_f: 0.2,
        },
        "Blake2b",
    );
}

fn core_verifier_benches_blake_300(c: &mut Criterion) {
    core_verifier_benches::<Blake2b<U32>>(
        c,
        300,
        StmParameters {
            m: 150,
            k: 25,
            phi_f: 0.2,
        },
    );
}

fn stm_benches_blake_2000(c: &mut Criterion) {
    stm_benches::<Blake2b<U32>>(
        c,
        2000,
        StmParameters {
            m: 1523,
            k: 250,
            phi_f: 0.2,
        },
        "Blake2b",
    );
}

fn core_verifier_benches_blake_2000(c: &mut Criterion) {
    core_verifier_benches::<Blake2b<U32>>(
        c,
        2000,
        StmParameters {
            m: 1523,
            k: 250,
            phi_f: 0.2,
        },
    );
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    core_verifier_benches_blake_300,
    core_verifier_benches_blake_2000,
    stm_benches_blake_300,
    stm_benches_blake_2000,
);
criterion_main!(benches);
