use mithril::merkle_tree::new_constants;
use mithril::key_reg::KeyReg;
use mithril::party::Party;
use mithril::Index;
use rand;
use rayon::prelude::*;

#[test]
fn test_full_protocol() {
    let nparties = 100;
    let msg = rand::random::<[u8;16]>();
    let constants = new_constants();

    //////////////////////////
    // initialization phase //
    //////////////////////////
    println!("* Initialization phase");

    let mut key_reg = KeyReg::new();

    let mut ps = Vec::with_capacity(nparties);

    for pid in 0..nparties {
        let stake = 1 + (rand::random::<u64>() % 9999);
        let mut p = Party::setup(pid, stake, &constants);
        p.register(&mut key_reg);
        ps.push(p);
    }

    ps.par_iter_mut().for_each(|p| {
        p.retrieve_all(&key_reg);
    });

    /////////////////////
    // operation phase //
    /////////////////////
    println!("* Operation phase");

    let mut sigs = Vec::new();
    let mut ixs  = Vec::new();
    for p in &ps {
        let ix = Index::random();
        if let Some(sig) = p.create_sig(&msg, ix) {
            sigs.push(sig);
            ixs.push(ix);
        }
    }

    // Check all parties can verify every sig
    for (s,ix) in sigs.iter().zip(&ixs) {
        for p in &ps {
            assert!(p.verify(s.clone(), *ix, &msg), "Verification failed");
        }
    }

    // Aggregate sig
    let msig = ps[0].aggregate(&sigs, &ixs, &msg).expect("Aggregation failed");
    assert!(ps[0].verify_aggregate(&msig, &msg), "Aggregate verification failed");
}
