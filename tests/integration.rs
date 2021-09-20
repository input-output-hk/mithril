use mithril::key_reg::KeyReg;
use mithril::proof::ConcatProof;
use mithril::stm::{StmParameters, StmParty};
use rand;
use rayon::prelude::*;

#[test]
fn test_full_protocol() {
    let nparties = 64;
    let msg = rand::random::<[u8; 16]>();

    //////////////////////////
    // initialization phase //
    //////////////////////////
    println!("* Initialization phase");

    let mut key_reg = KeyReg::new();

    let mut ps = Vec::with_capacity(nparties);
    let params = StmParameters {
        k: 357,
        m: 2642,
        phi_f: 0.2,
    };

    for pid in 0..nparties {
        let stake = 1 + (rand::random::<u64>() % 9999);
        let mut p = StmParty::setup(params, pid, stake);
        p.register(&mut key_reg);
        ps.push(p);
    }

    ps.par_iter_mut().for_each(|p| {
        p.retrieve_all(&key_reg);
        p.create_avk();
    });

    /////////////////////
    // operation phase //
    /////////////////////
    println!("* Operation phase");

    println!("** Finding signatures");
    let mut sigs = Vec::new();
    let mut ixs = Vec::new();
    for ix in 1..params.m {
        for p in &ps {
            if let Some(sig) = p.create_sig(&msg, ix) {
                sigs.push(sig);
                ixs.push(ix);
                break;
            }
        }

        if params.k as usize == sigs.len() {
            break;
        }
    }
    // Check that we can find a quorum
    assert!(params.k as usize == sigs.len());

    // Check all parties can verify every sig
    println!("** Verifying signatures");
    for (s, ix) in sigs.iter().zip(&ixs) {
        for p in &ps {
            assert!(p.verify(s.clone(), *ix, &msg), "Verification failed");
        }
    }

    // Aggregate and verify with random parties
    println!("** Aggregating signatures");
    let aggregator = rand::random::<usize>() % ps.len();
    let verifier = rand::random::<usize>() % ps.len();
    let msig = ps[aggregator]
        .aggregate::<ConcatProof>(&sigs, &ixs, &msg)
        .expect("Aggregation failed");
    assert!(
        ps[verifier].verify_aggregate(&msig, &msg),
        "Aggregate verification failed"
    );
}
