use crate::ev_lt_phi;
use crate::msp;
use crate::merkle_tree::MerkleTree;
use crate::party::Sig;
use super::{Index, Phi, Path};

use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Clone)]
pub struct Witness {
    pub sigs: Vec<Sig>,
    pub indices: Vec<Index>,
    pub evals: Vec<u64>,
}

// Proof system that simply concatenates the witness
#[derive(Clone)]
pub struct ConcatProof(Witness);

impl ConcatProof {
    pub fn prove(_avk: &MerkleTree, _ivk: &msp::MVK, _msg: &[u8], w: &Witness) -> Self {
        Self(w.clone())
    }

    // Parameterized by:
    //   N (leaves in avk?)
    //   m ???
    //   k (length of witness?)
    //   phi ???
    pub fn verify(&self, phi: Phi, total_stake: u64, m: u64, avk: &MerkleTree, ivk: &msp::MVK, msg: &[u8]) -> bool
    {
        // ivk = Prod(1..k, mvk[i])
        let ivk_check = ivk.0 == self.0
                                     .sigs
                                     .iter()
                                     .map(|s| s.pk.mvk.0)
                                     .sum();

        // \forall i. index[i] <= m (what the hell is m)
        let index_bound_check = self.0.indices.iter().fold(true, |r, Index(i)| r && i <= &m);

        // \forall i. \forall j. (i == j || index[i] != index[j])
        let index_uniq_check =
               HashSet::<Index>::from_iter(self.0.indices.iter().cloned()).len()
            == self.0.indices.len();

        // \forall i : [1..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
        let path_check =
            self.0.sigs.iter().fold(true, |r, sig| {
                r && avk.check(&(sig.pk.mvk, sig.stake), sig.party, &sig.path)
            });

        // \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
        let msp_evals =
            Iterator::zip(self.0.indices.iter(), self.0.sigs.iter())
            .map(|(idx, sig)| msp::MSP::eval(msg, *idx, &sig.sigma) );
        let eval_check =
            Iterator::zip(self.0.evals.iter(), msp_evals)
            .fold(true, |r, (ev, msp_e)| r && *ev == msp_e );
        // \forall i : [1..k]. ev[i] <= phi(stake_i)
        let eval_stake_check =
            Iterator::zip(self.0.evals.iter(), self.0.sigs.iter())
            .fold(true, |r, (ev, sig)|
                  ev_lt_phi(phi, *ev, sig.stake, total_stake));

        ivk_check &&
        index_bound_check &&
        index_uniq_check &&
        path_check &&
        eval_check &&
        eval_stake_check
    }
}
