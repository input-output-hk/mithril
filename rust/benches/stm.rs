use ark_bls12_377::Bls12_377;
use ark_bls12_381::Bls12_381;
use ark_ec::PairingEngine;
use ark_ff::{FromBytes, ToBytes, ToConstraintField};
use blake2::Blake2b;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril::key_reg::KeyReg;
use mithril::merkle_tree::MTHashLeaf;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::models::digest::DigestHash;
use mithril::stm::{MTValue, StmClerk, StmInitializer, StmParameters, StmSigner};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;
use std::hash::Hash;

///
/// This benchmark framework is not ideal. We really have to think what is the best mechanism for
/// benchmarking these signatures, over which parameters, how many times to run them, etc:
/// * Registration depends on the number of parties (should be constant, as it is a lookup table)
/// * Signing depends on the parameter `m`, as it defines the number of lotteries a user can play
/// * Aggregation depends on `k`.
/// * Verification is independent from the parameters.
///

const SIZE: usize = 8;
static NR_PARTIES: [usize; SIZE] = [32, 64, 128, 256, 512, 1024, 2048, 4096];
static NR_M: [u64; SIZE] = [50, 100, 150, 200, 250, 300, 350, 400];
static NR_K: [u64; SIZE] = [8, 16, 32, 64, 128, 256, 512, 1024];

fn stm_benches<C, H>(c: &mut Criterion, curve: &str)
where
    C: PairingEngine + Hash,
    C::G1Projective: ToConstraintField<C::Fq>,
    H: MTHashLeaf<MTValue<C>, F = DigestHash> + Clone,
    <H as MTHashLeaf<MTValue<C>>>::F: Send + Sync + FromBytes + ToBytes,
{
    let mut group = c.benchmark_group(format!("STM/{:?}", curve));
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let parties = (0..NR_PARTIES[SIZE - 1])
        .into_iter()
        .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
        .collect::<Vec<_>>();

    let mut k = 8;
    let mut m = 50;

    let params = StmParameters {
        k,
        m,
        // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
        phi_f: 1.0,
    };

    let mut ps: Vec<StmInitializer<C>> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
    for (pid, stake) in parties.clone() {
        ps.push(StmInitializer::setup(params, pid, stake, &mut rng));
    }
    let mut key_reg = KeyReg::new(&parties);
    for &nr in NR_PARTIES.iter() {
        group.bench_with_input(BenchmarkId::new("Key registration", &nr), &nr, |b, &nr| {
            b.iter(|| {
                // We need to initialise the key_reg at each iteration
                key_reg = KeyReg::new(&parties[..nr]);
                for p in ps[..nr].iter() {
                    key_reg
                        .register(p.party_id(), p.verification_key())
                        .unwrap();
                }
            })
        });
    }

    let closed_reg = key_reg.close();

    let ps = ps
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()))
        .collect::<Vec<StmSigner<H, C>>>();

    let mut party_dummy = ps[0].clone();

    for &m in NR_M.iter() {
        k = 8;

        let param_string = format!("k: {}, m: {}", k, m);

        let params = StmParameters {
            k,
            m,
            // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
            phi_f: 1.0,
        };

        let mut key_reg = KeyReg::new(&parties);
        let mut ps: Vec<StmInitializer<C>> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
        for (pid, stake) in parties.clone() {
            let p = StmInitializer::setup(params, pid, stake, &mut rng);
            key_reg
                .register(p.party_id(), p.verification_key())
                .unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();

        let ps = ps
            .into_par_iter()
            .map(|p| p.new_signer(closed_reg.clone()))
            .collect::<Vec<StmSigner<H, C>>>();

        group.bench_with_input(
            BenchmarkId::new("Play all lotteries", &param_string),
            &m,
            |b, &m| {
                b.iter(|| {
                    for ix in 1..m {
                        ps[0].sign(&msg, ix);
                    }
                })
            },
        );
    }

    let mut sigs = Vec::new();
    let mut ixs = Vec::new();

    for &k in NR_K.iter() {
        m = 50;
        let param_string = format!("k: {}, m: {}", k, m);

        let params = StmParameters {
            k,
            m,
            // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
            phi_f: 1.0,
        };

        let mut key_reg = KeyReg::new(&parties);
        let mut ps: Vec<StmInitializer<C>> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
        for (pid, stake) in parties.clone() {
            let p = StmInitializer::setup(params, pid, stake, &mut rng);
            key_reg
                .register(p.party_id(), p.verification_key())
                .unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();

        let ps = ps
            .into_par_iter()
            .map(|p| p.new_signer(closed_reg.clone()))
            .collect::<Vec<StmSigner<H, C>>>();

        let p_results = ps
            .par_iter()
            .map(|p| {
                let mut sigs = Vec::new();
                let mut ixs = Vec::new();
                for ix in 1..params.m {
                    if let Some(sig) = p.sign(&msg, ix) {
                        sigs.push(sig);
                        ixs.push(ix);
                    }
                }

                (ixs, sigs)
            })
            .collect::<Vec<_>>();

        sigs = Vec::new();
        ixs = Vec::new();

        for res in p_results {
            ixs.extend(res.0);
            sigs.extend(res.1);
        }

        party_dummy = ps[0].clone();
        let clerk = StmClerk::from_signer(&party_dummy, TrivialEnv);

        group.bench_function(BenchmarkId::new("Aggregation", &param_string), |b| {
            b.iter(|| clerk.aggregate::<ConcatProof<C, H, H::F>>(&sigs, &ixs, &msg))
        });
    }

    let clerk = StmClerk::from_signer(&party_dummy, TrivialEnv);
    let msig = clerk
        .aggregate::<ConcatProof<C, H, H::F>>(&sigs, &ixs, &msg)
        .unwrap();
    group.bench_function("Verification", |b| {
        b.iter(|| {
            clerk
                .verify_msig::<ConcatProof<C, H, H::F>>(&msig, &msg)
                .is_ok()
        })
    });
}

fn stm_benches_bls12_377_blake(c: &mut Criterion) {
    stm_benches::<Bls12_377, Blake2b>(c, "Bls12_381");
}

fn stm_benches_bls12_381_blake(c: &mut Criterion) {
    stm_benches::<Bls12_381, Blake2b>(c, "Bls12_381");
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(5);
                 targets = stm_benches_bls12_377_blake, stm_benches_bls12_381_blake);
criterion_main!(benches);
