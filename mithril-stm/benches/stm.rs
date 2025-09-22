use std::fmt::Debug;

use blake2::digest::{Digest, FixedOutput};
use blake2::{Blake2b, digest::consts::U32};
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;

use mithril_stm::{
    AggregateSignature, AggregateSignatureType, BasicVerifier, Clerk, Initializer, KeyRegistration,
    Parameters, Signer, Stake, VerificationKey,
};

/// This benchmark framework is not ideal. We really have to think what is the best mechanism for
/// benchmarking these signatures, over which parameters, how many times to run them, etc:
/// * Registration depends on the number of parties (should be constant, as it is a lookup table)
/// * Signing depends on the parameter `m`, as it defines the number of lotteries a user can play
/// * Aggregation depends on `k`.
fn stm_benches<H>(c: &mut Criterion, nr_parties: usize, params: Parameters, hashing_alg: &str)
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

    let mut initializers: Vec<Initializer> = Vec::with_capacity(nr_parties);
    for stake in stakes {
        initializers.push(Initializer::new(params, stake, &mut rng));
    }
    let mut key_reg = KeyRegistration::init();

    group.bench_function(BenchmarkId::new("Key registration", &param_string), |b| {
        b.iter(|| {
            // We need to initialise the key_reg at each iteration
            key_reg = KeyRegistration::init();
            for p in initializers.iter() {
                key_reg
                    .register(p.stake, p.get_verification_key_proof_of_possession())
                    .unwrap();
            }
        })
    });

    let closed_reg = key_reg.close();

    let signers = initializers
        .into_par_iter()
        .map(|p| p.create_signer(closed_reg.clone()).unwrap())
        .collect::<Vec<Signer<H>>>();

    group.bench_function(BenchmarkId::new("Play all lotteries", &param_string), |b| {
        b.iter(|| {
            signers[0].sign(&msg);
        })
    });

    let sigs = signers.par_iter().filter_map(|p| p.sign(&msg)).collect::<Vec<_>>();

    let clerk = Clerk::new_clerk_from_signer(&signers[0]);
    let aggregate_signature_type = AggregateSignatureType::Concatenation;

    group.bench_function(BenchmarkId::new("Aggregation", &param_string), |b| {
        b.iter(|| clerk.aggregate_signatures_with_type(&sigs, &msg, aggregate_signature_type))
    });
}

fn batch_benches<H>(
    c: &mut Criterion,
    array_batches: &[usize],
    nr_parties: usize,
    params: Parameters,
    hashing_alg: &str,
) where
    H: Clone + Debug + Digest + FixedOutput + Send + Sync,
{
    let mut group = c.benchmark_group(format!("STM/{hashing_alg}"));
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);

    let param_string = format!(
        "k: {}, m: {}, nr_parties: {}",
        params.k, params.m, nr_parties
    );

    for &nr_batches in array_batches {
        let batch_string = format!("{param_string}/batch size: {nr_batches}");

        let mut batch_msgs = Vec::with_capacity(nr_batches);
        let mut batch_params = Vec::with_capacity(nr_batches);
        let mut batch_stms = Vec::with_capacity(nr_batches);
        let mut batch_avks = Vec::with_capacity(nr_batches);

        for _ in 0..nr_batches {
            let mut msg = [0u8; 32];
            rng.fill_bytes(&mut msg);
            batch_msgs.push(msg.to_vec());
            batch_params.push(params);

            let stakes = (0..nr_parties)
                .map(|_| 1 + (rng.next_u64() % 9999))
                .collect::<Vec<_>>();

            let mut initializers: Vec<Initializer> = Vec::with_capacity(nr_parties);
            for stake in stakes {
                initializers.push(Initializer::new(params, stake, &mut rng));
            }
            let mut key_reg = KeyRegistration::init();
            for p in initializers.iter() {
                key_reg
                    .register(p.stake, p.get_verification_key_proof_of_possession())
                    .unwrap();
            }

            let closed_reg = key_reg.close();

            let signers = initializers
                .into_par_iter()
                .map(|p| p.create_signer(closed_reg.clone()).unwrap())
                .collect::<Vec<Signer<H>>>();

            let sigs = signers.par_iter().filter_map(|p| p.sign(&msg)).collect::<Vec<_>>();

            let clerk = Clerk::new_clerk_from_signer(&signers[0]);
            let aggregate_signature_type = AggregateSignatureType::Concatenation;
            let msig = clerk
                .aggregate_signatures_with_type(&sigs, &msg, aggregate_signature_type)
                .unwrap();

            batch_avks.push(clerk.compute_aggregate_verification_key());
            batch_stms.push(msig);
        }

        group.bench_function(BenchmarkId::new("Batch Verification", batch_string), |b| {
            b.iter(|| {
                AggregateSignature::batch_verify(
                    &batch_stms,
                    &batch_msgs,
                    &batch_avks,
                    &batch_params,
                )
                .is_ok()
            })
        });
    }
}

fn basic_verifier_benches<H>(c: &mut Criterion, nr_parties: usize, params: Parameters)
where
    H: Clone + Debug + Digest + Send + Sync + FixedOutput + Default,
{
    let mut group = c.benchmark_group("Core verifier");
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let mut public_signers: Vec<(VerificationKey, Stake)> = Vec::with_capacity(nr_parties);
    let mut initializers: Vec<Initializer> = Vec::with_capacity(nr_parties);

    let param_string = format!(
        "k: {}, m: {}, nr_parties: {}",
        params.k, params.m, nr_parties
    );

    let stakes = (0..nr_parties)
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    for stake in stakes {
        let initializer = Initializer::new(params, stake, &mut rng);
        initializers.push(initializer.clone());
        public_signers.push((
            initializer.get_verification_key_proof_of_possession().vk,
            initializer.stake,
        ));
    }

    let core_verifier = BasicVerifier::new(&public_signers);

    let signers: Vec<Signer<H>> = initializers
        .into_iter()
        .filter_map(|s| s.create_basic_signer(&core_verifier.eligible_parties))
        .collect();

    group.bench_function(BenchmarkId::new("Play all lotteries", &param_string), |b| {
        b.iter(|| {
            signers[0].basic_sign(&msg, core_verifier.total_stake);
        })
    });

    let signatures = signers
        .par_iter()
        .filter_map(|p| p.basic_sign(&msg, core_verifier.total_stake))
        .collect::<Vec<_>>();

    group.bench_function(BenchmarkId::new("Core verification", &param_string), |b| {
        b.iter(|| core_verifier.verify(&signatures, &params, &msg))
    });
}

fn batch_stm_benches_blake_300(c: &mut Criterion) {
    batch_benches::<Blake2b<U32>>(
        c,
        &[1, 10, 20, 100],
        300,
        Parameters {
            m: 150,
            k: 25,
            phi_f: 0.4,
        },
        "Blake2b",
    );
}

fn stm_benches_blake_300(c: &mut Criterion) {
    stm_benches::<Blake2b<U32>>(
        c,
        300,
        Parameters {
            m: 150,
            k: 25,
            phi_f: 0.2,
        },
        "Blake2b",
    );
}

fn core_verifier_benches_blake_300(c: &mut Criterion) {
    basic_verifier_benches::<Blake2b<U32>>(
        c,
        300,
        Parameters {
            m: 150,
            k: 25,
            phi_f: 0.2,
        },
    );
}

fn batch_stm_benches_blake_2000(c: &mut Criterion) {
    batch_benches::<Blake2b<U32>>(
        c,
        &[1, 10, 20, 100],
        2000,
        Parameters {
            m: 1523,
            k: 250,
            phi_f: 0.4,
        },
        "Blake2b",
    );
}

fn stm_benches_blake_2000(c: &mut Criterion) {
    stm_benches::<Blake2b<U32>>(
        c,
        2000,
        Parameters {
            m: 1523,
            k: 250,
            phi_f: 0.2,
        },
        "Blake2b",
    );
}

fn core_verifier_benches_blake_2000(c: &mut Criterion) {
    basic_verifier_benches::<Blake2b<U32>>(
        c,
        2000,
        Parameters {
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
    batch_stm_benches_blake_300,
    batch_stm_benches_blake_2000,
);
criterion_main!(benches);
