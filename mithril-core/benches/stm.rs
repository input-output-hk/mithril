use blake2::Blake2b;
use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use digest::{Digest, FixedOutput};
use mithril::key_reg::KeyReg;
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSigner};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;
use std::fmt::Debug;

///
/// This benchmark framework is not ideal. We really have to think what is the best mechanism for
/// benchmarking these signatures, over which parameters, how many times to run them, etc:
/// * Registration depends on the number of parties (should be constant, as it is a lookup table)
/// * Signing depends on the parameter `m`, as it defines the number of lotteries a user can play
/// * Aggregation depends on `k`.
/// * Verification is independent from the parameters.
///

const SIZE: usize = 3;
static NR_PARTIES: [usize; SIZE] = [32, 64, 128];
static NR_M: [u64; SIZE] = [50, 100, 150];
static NR_K: [u64; SIZE] = [8, 16, 32];

fn stm_benches<H>(c: &mut Criterion, curve: &str)
where
    H: Clone + Debug + Digest + FixedOutput + Send + Sync,
{
    let mut group = c.benchmark_group(format!("STM/{:?}", curve));
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let stakes = (0..NR_PARTIES[SIZE - 1])
        .into_iter()
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut k = 8;
    let mut m = 50;

    let params = StmParameters {
        k,
        m,
        // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
        phi_f: 1.0,
    };

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
    for stake in stakes.clone() {
        ps.push(StmInitializer::setup(params, stake, &mut rng));
    }
    let mut key_reg = KeyReg::init();
    for &nr in NR_PARTIES.iter() {
        group.bench_with_input(BenchmarkId::new("Key registration", &nr), &nr, |b, &nr| {
            b.iter(|| {
                // We need to initialise the key_reg at each iteration
                key_reg = KeyReg::init();
                for p in ps[..nr].iter() {
                    key_reg.register(p.stake(), p.verification_key()).unwrap();
                }
            })
        });
    }

    let closed_reg = key_reg.close();

    let ps = ps
        .into_par_iter()
        .map(|p| p.new_signer(closed_reg.clone()))
        .collect::<Vec<StmSigner<H>>>();

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

        let mut key_reg = KeyReg::init();
        let mut ps: Vec<StmInitializer> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
        for stake in stakes.clone() {
            let p = StmInitializer::setup(params, stake, &mut rng);
            key_reg.register(stake, p.verification_key()).unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();

        let ps = ps
            .into_par_iter()
            .map(|p| p.new_signer(closed_reg.clone()))
            .collect::<Vec<StmSigner<H>>>();

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

    for &k in NR_K.iter() {
        m = 50;
        let param_string = format!("k: {}, m: {}", k, m);

        let params = StmParameters {
            k,
            m,
            // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
            phi_f: 1.0,
        };

        let mut key_reg = KeyReg::init();
        let mut ps: Vec<StmInitializer> = Vec::with_capacity(NR_PARTIES[SIZE - 1]);
        for stake in stakes.clone() {
            let p = StmInitializer::setup(params, stake, &mut rng);
            key_reg.register(stake, p.verification_key()).unwrap();
            ps.push(p);
        }

        let closed_reg = key_reg.close();

        let ps = ps
            .into_par_iter()
            .map(|p| p.new_signer(closed_reg.clone()))
            .collect::<Vec<StmSigner<H>>>();

        let p_results = ps
            .par_iter()
            .map(|p| {
                let mut sigs = Vec::new();
                for ix in 1..params.m {
                    if let Some(sig) = p.sign(&msg, ix) {
                        sigs.push(sig);
                    }
                }

                sigs
            })
            .collect::<Vec<_>>();

        sigs = Vec::new();

        // todo: probably can be optimized
        for res in p_results {
            sigs.extend(res);
        }

        party_dummy = ps[0].clone();
        let clerk = StmClerk::from_signer(&party_dummy);

        group.bench_function(BenchmarkId::new("Aggregation", &param_string), |b| {
            b.iter(|| clerk.aggregate(&sigs, &msg))
        });
    }

    let clerk = StmClerk::from_signer(&party_dummy);
    let msig = clerk.aggregate(&sigs, &msg).unwrap();
    group.bench_function("Verification", |b| {
        b.iter(|| msig.verify(&msg, &clerk.compute_avk(), &params).is_ok())
    });
}

fn stm_benches_bls12_377_blake(c: &mut Criterion) {
    stm_benches::<Blake2b>(c, "Bls12_381");
}

fn stm_benches_bls12_381_blake(c: &mut Criterion) {
    stm_benches::<Blake2b>(c, "Bls12_381");
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(5);
                 targets = stm_benches_bls12_377_blake, stm_benches_bls12_381_blake);
criterion_main!(benches);
