use ark_bls12_377::Bls12_377;
use ark_ff::ToBytes;
use mithril::key_reg::KeyReg;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSigner};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};
use rayon::prelude::*;

type C = Bls12_377;
type H = blake2::Blake2b;

fn main() {
    // The only parameter over which the proof size changes is `k`, the number of required
    // signatures.
    println!("+----------------+");
    println!("| Size of proofs |");
    println!("+----------------+");
    println!("|----------------|");
    println!("| Trivial proofs |");
    println!("+----------------+");

    static NR_K: [u64; 8] = [8, 16, 32, 64, 128, 256, 512, 1024];
    let nparties = 1;

    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    let parties = (0..nparties)
        .into_iter()
        .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
        .collect::<Vec<_>>();

    for k in NR_K {
        let params = StmParameters {
            k,
            m: 1024,
            // equal to 1, to win all loteries. This will give us an upper bound on how long it takes to play `m` lotteries
            phi_f: 1.0,
        };

        let parties = (0..nparties)
            .into_iter()
            .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
            .collect::<Vec<_>>();
        let mut ps: Vec<StmInitializer<C>> = Vec::with_capacity(nparties);

        let mut key_reg = KeyReg::new(&parties);
        for (pid, stake) in parties.clone() {
            let p = StmInitializer::setup(params, pid, stake, &mut rng);
            key_reg
                .register(p.party_id(), p.verification_key())
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
        let msig = clerk
            .aggregate::<ConcatProof<C, H>>(&sigs, &ixs, &msg)
            .unwrap();

        let mut writer = Vec::new();
        msig.write(&mut writer);

        println!("k = {}; {} bytes", k, writer.len());
    }
}
