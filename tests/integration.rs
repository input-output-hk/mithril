use mithril::key_reg::KeyReg;
use mithril::party::Party;
use mithril::{Stake, Index};

#[test]
fn sign() {
    let nparties = 1000;
    let msg = rand::random::<[u8;16]>();

    //////////////////////////
    // initialization phase //
    //////////////////////////

    let mut key_reg = KeyReg::new();

    let mut ps = Vec::with_capacity(nparties);

    for pid in 0..nparties {
        let stake = Stake(0.1); // XXX: How to set stake?
        let mut p = Party::setup(pid, stake);
        p.register(&mut key_reg);
        ps.push(p);
    }

    for p in ps.iter_mut() {
        p.retrieve_all(&key_reg);
    }

    /////////////////////
    // operation phase //
    /////////////////////

    let index = Index(0); // XXX: not clear right now what index is for

    let mut sigs = Vec::new();
    for p in &ps {
        if let Some(sig) = p.create_sig(&msg, index) {
            sigs.push(sig);
        }
    }

    // Check all parties can verify every sig
    for s in &sigs {
        for p in &ps {
            assert!(p.verify(*s, index, &msg), "Verification failed");
        }
    }

    // Aggregate sig
    let msig = ps[0].aggregate(&sigs, &[index], &msg).expect("Aggregation failed");
    assert!(ps[0].verify_aggregate(&msig, &msg), "Aggregate verification failed");
}
