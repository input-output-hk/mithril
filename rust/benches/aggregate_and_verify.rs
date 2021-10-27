use ark_bls12_377::Bls12_377;
use criterion::{criterion_group, criterion_main, Criterion};
use mithril::key_reg::KeyReg;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSigner};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;
use std::time::Duration;

type C = Bls12_377;
type H = blake2::Blake2b;

fn aggregate_and_verify(c: &mut Criterion) {
    let nparties = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let params = StmParameters {
        k: 100,
        m: 1000,
        phi_f: 0.9,
    };

    let parties = (0..nparties)
        .into_iter()
        .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::new(&parties);

    let mut ps: Vec<StmInitializer<C>> = Vec::with_capacity(nparties);
    for (pid, stake) in parties {
        let p = StmInitializer::setup(params, pid, stake, &mut rng);
        key_reg
            .register(p.party_id(), p.stake(), p.verification_key())
            .unwrap();
        ps.push(p);
    }

    let reg = key_reg.retrieve_all();

    let ps = ps
        .into_par_iter()
        .map(|p| p.new_signer(&reg))
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
    let mut sigs = Vec::new();
    let mut ixs = Vec::new();
    for res in p_results {
        ixs.extend(res.0);
        sigs.extend(res.1);
    }

    let clerk = StmClerk::from_signer(&ps[0], TrivialEnv);

    let mut group = c.benchmark_group("Signature aggregation and verification");
    group.bench_function("Aggregation", |b| {
        b.iter(|| clerk.aggregate::<ConcatProof<C, H>>(&sigs, &ixs, &msg))
    });
    let msig = clerk.aggregate::<ConcatProof<C, H>>(&sigs, &ixs, &msg);
    group.bench_with_input("Verification", &msig, |b, msig| {
        b.iter(|| {
            if let Ok(aggr) = msig {
                clerk.verify_msig::<ConcatProof<C, H>>(aggr, &msg);
            }
        })
    });
}

criterion_group!(name = benches;
                 config = Criterion::default().measurement_time(Duration::new(150, 0));
                 targets = aggregate_and_verify);
criterion_main!(benches);
