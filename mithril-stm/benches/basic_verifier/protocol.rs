use std::fmt::Debug;

use blake2::digest::{Digest, FixedOutput};
use blake2::{Blake2b, digest::consts::U32};
use criterion::{BenchmarkId, Criterion, criterion_group, criterion_main};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;

use mithril_stm::{BasicVerifier, Initializer, Parameters, Signer, Stake, VerificationKey};

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
);
criterion_main!(benches);
