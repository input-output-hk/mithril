use mithril::key_reg::KeyReg;
use mithril::stm::{StmClerk, StmInitializer, StmParameters, StmSig, StmSigner};
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
        .map(|_| 1 + (rng.next_u64() % 9999))
        .collect::<Vec<_>>();

    let mut key_reg = KeyReg::init();

    let mut ps: Vec<StmInitializer> = Vec::with_capacity(nparties as usize);
    for stake in parties {
        let p = StmInitializer::setup(params, stake, &mut rng);
        key_reg.register(stake, p.verification_key()).unwrap();
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

    let sigs = ps
        .par_iter()
        .filter_map(|p| {
            p.sign(&msg)
        })
        .collect::<Vec<StmSig<H>>>();


    let clerk = StmClerk::from_signer(&ps[0]);
    let avk = clerk.compute_avk();

    // Check all parties can verify every sig
    for s in sigs.iter() {
        assert!(s.verify(&params, &avk, &msg).is_ok(), "Verification failed");
    }

    // Aggregate with random parties
    let msig = clerk.aggregate(&sigs, &msg);

    match msig {
        Ok(aggr) => {
            println!("Aggregate ok");
            assert!(aggr.verify(&msg, &clerk.compute_avk(), &params).is_ok());
        }
        Err(AggregationFailure::NotEnoughSignatures(n, k)) => {
            println!("Not enough signatures");
            assert!(n < params.k && k == params.k)
        }
        Err(AggregationFailure::InvalidUsizeConversion) => {
            println!("Invalid usize conversion");
        }
    }
}
