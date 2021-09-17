use mithril::key_reg::KeyReg;
use mithril::stm::{StmParty, StmParameters};
use rand;
use rayon::prelude::*;
use mithril::proof::ConcatProof;

#[test]
fn test_full_protocol() {
    let nparties = 128;
    let msg = rand::random::<[u8;16]>();

    //////////////////////////
    // initialization phase //
    //////////////////////////
    println!("* Initialization phase");

    let mut key_reg = KeyReg::new();

    let mut ps = Vec::with_capacity(nparties);
    let params = StmParameters { m: 100, k: 20, phi_f: 0.2 };

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

    let mut sigs = Vec::new();
    let mut ixs  = Vec::new();
    let mut ix = 0;
    for p in &ps {
        if let Some(sig) = p.create_sig(&msg, ix) {
            sigs.push(sig);
            ixs.push(ix);
            ix += 1;
        }
    }

    // Check all parties can verify every sig
    for (s,ix) in sigs.iter().zip(&ixs) {
        for p in &ps {
            assert!(p.verify(s.clone(), *ix, &msg), "Verification failed");
        }
    }

    // Aggregate sig
    let msig = ps[0].aggregate::<ConcatProof>(&sigs, &ixs, &msg).expect("Aggregation failed");
    assert!(ps[0].verify_aggregate(&msig, &msg), "Aggregate verification failed");
}
