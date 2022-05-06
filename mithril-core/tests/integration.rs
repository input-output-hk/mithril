use mithril::key_reg::KeyReg;
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSigner, StmVerifier};
use rayon::prelude::*;

use mithril::error::AggregationFailure;
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

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties as usize);
    for (pid, stake) in parties {
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
        .collect::<Vec<StmSigner<H>>>();

    /////////////////////
    // operation phase //
    /////////////////////

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

    let clerk = StmClerk::from_signer(&ps[0]);

    // Check all parties can verify every sig
    for s in sigs.iter() {
        assert!(clerk.verify_sig(s, &msg).is_ok(), "Verification failed");
    }

    // Aggregate with random parties
    let msig = clerk.aggregate(&sigs, &msg);

    // Verify aggregated signature with a fresh verifier
    let verifier = StmVerifier::new(
        closed_reg.avk.to_commitment(),
        params,
        closed_reg.total_stake,
    );
    match msig {
        Ok(aggr) => {
            println!("Aggregate ok");
            assert!(verifier.verify_msig(&msg, &aggr).is_ok());
        }
        Err(AggregationFailure::NotEnoughSignatures(n, k)) => {
            println!("Not enough signatures");
            assert!(n < params.k && k == params.k)
        }
    }
}
