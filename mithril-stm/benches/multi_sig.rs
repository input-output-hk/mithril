use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion};
use mithril::multi_sig::{Signature, SigningKey, VerificationKey};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

fn batch_benches(c: &mut Criterion, array_batches: &[usize], nr_sigs: usize) {
    let mut group = c.benchmark_group("MultiSig".to_string());
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut batch_msgs = Vec::new();
    let mut batch_vk = Vec::new();
    let mut batch_sig = Vec::new();

    for &nr_batches in array_batches {
        let batch_string = format!("Batch size: {}", nr_batches);

        for _ in 0..nr_batches {
            let mut msg = [0u8; 32];
            rng.fill_bytes(&mut msg);
            let mut mvks = Vec::new();
            let mut sigs = Vec::new();
            for _ in 0..nr_sigs {
                let sk = SigningKey::gen(&mut rng);
                let vk = VerificationKey::from(&sk);
                let sig = sk.sign(&msg);
                sigs.push(sig);
                mvks.push(vk);
            }
            let (agg_vk, agg_sig) = Signature::aggregate(&mvks, &sigs).unwrap();
            batch_msgs.push(msg.to_vec());
            batch_vk.push(agg_vk);
            batch_sig.push(agg_sig);
        }

        group.bench_function(BenchmarkId::new("Batch Verification", &batch_string), |b| {
            b.iter(|| {
                Signature::batch_verify_aggregates(&batch_msgs, &batch_vk, &batch_sig).is_ok()
            })
        });
    }
}

fn batch_multi_sig_benches(c: &mut Criterion) {
    batch_benches(c, &[1, 10, 20, 50, 100], 300);
}

criterion_group!(name = benches;
                 config = Criterion::default().nresamples(1000);
                 targets =
    batch_multi_sig_benches
);
criterion_main!(benches);
