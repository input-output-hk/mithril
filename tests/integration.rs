use ark_bls12_377::Bls12_377;
use ark_bls12_377;
use mithril::key_reg::KeyReg;
use mithril::proof::ConcatProof;
use mithril::stm::{AggregationFailure, StmClerk, StmInitializer, StmParameters, StmSigner};
use rand;
use rayon::prelude::*;

#[test]
fn test_full_protocol() {
    let nparties = 32;
    let msg = rand::random::<[u8; 16]>();

    //////////////////////////
    // initialization phase //
    //////////////////////////
    println!("* Initialization phase");

    let mut key_reg = KeyReg::new();

    let mut ps: Vec<StmInitializer<Bls12_377>> = Vec::with_capacity(nparties);
    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    for pid in 0..nparties {
        let stake = 1 + (rand::random::<u64>() % 9999);
        let mut p = StmInitializer::setup(params, pid, stake);
        p.register(&mut key_reg);
        ps.push(p);
    }

    let ps = ps
        .into_par_iter()
        .map(|mut p| {
            p.retrieve_all(&key_reg);
            p.finish()
        })
        .collect::<Vec<StmSigner<Bls12_377>>>();

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

    let clerk = StmClerk::from_signer(&ps[0]);

    // Check all parties can verify every sig
    println!("** Verifying signatures");
    for (s, ix) in sigs.iter().zip(&ixs) {
        assert!(clerk.verify_sig(s, *ix, &msg), "Verification failed");
    }

    // Aggregate and verify with random parties
    println!("** Aggregating signatures");
    let msig = clerk.aggregate::<ConcatProof<Bls12_377>>(&sigs, &ixs, &msg);
    match msig {
        Ok(aggr) => assert!(clerk.verify_msig(&aggr, &msg)),
        Err(AggregationFailure::NotEnoughSignatures(n)) => assert!(n < params.k as usize),
        Err(_) => assert!(false),
    }
}
