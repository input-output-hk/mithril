use ark_bls12_377::Bls12_377;
use mithril::key_reg::KeyReg;
use mithril::mithril_proof::concat_proofs::{ConcatProof, TrivialEnv};
use mithril::stm::{AggregationFailure, StmClerk, StmInitializer, StmParameters, StmSigner};
use rayon::prelude::*;

use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

type H = blake2::Blake2b;

#[test]
fn test_full_protocol() {
    let nparties = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 16];
    rng.fill_bytes(&mut msg);

    //////////////////////////
    // initialization phase //
    //////////////////////////
    println!("* Initialization phase");

    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    let parties = (0..nparties)
        .into_iter()
        .map(|pid| (pid, 1 + (rng.next_u64() % 9999)))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::new(&parties);

    let mut ps: Vec<StmInitializer<Bls12_377>> = Vec::with_capacity(nparties);
    for (pid, stake) in parties {
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
        .collect::<Vec<StmSigner<H, Bls12_377>>>();

    /////////////////////
    // operation phase //
    /////////////////////
    println!("* Operation phase");

    println!("** Finding signatures");
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

    // Check all parties can verify every sig
    println!("** Verifying signatures");
    for (s, ix) in sigs.iter().zip(&ixs) {
        assert!(clerk.verify_sig(s, *ix, &msg), "Verification failed");
    }

    // Aggregate and verify with random parties
    println!("** Aggregating signatures");
    let msig = clerk.aggregate::<ConcatProof<Bls12_377, H>>(&sigs, &ixs, &msg);
    match msig {
        Ok(aggr) => {
            println!("Aggregate ok");
            assert!(clerk.verify_msig::<ConcatProof<Bls12_377, H>>(&aggr, &msg));
        }
        Err(AggregationFailure::NotEnoughSignatures(n)) => {
            println!("Not enough signatures");
            assert!(n < params.k as usize)
        }
        Err(_) => unreachable!(),
    }
}
