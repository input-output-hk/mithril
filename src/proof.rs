use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk};
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;

#[derive(Clone)]
pub struct Witness {
    pub sigs: Vec<StmSig>,
    pub indices: Vec<Index>,
    pub evals: Vec<u64>,
}

// Proof system that simply concatenates the witness
#[derive(Clone)]
pub struct ConcatProof(Witness);

impl ConcatProof {
    pub fn prove(_avk: &MerkleTree, _ivk: &MspMvk, _msg: &[u8], w: &Witness) -> Self {
        Self(w.clone())
    }

    pub fn verify(
        &self,
        params: &StmParameters,
        total_stake: u64,
        avk: &MerkleTree,
        ivk: &MspMvk,
        msg: &[u8],
    ) -> bool {
        // ivk = Prod(1..k, mvk[i])
        let ivk_check = ivk.0 == self.0.sigs.iter().map(|s| s.pk.mvk.0).sum();

        // \forall i. index[i] <= m
        let index_bound_check = self.0.indices.iter().all(|i| i <= &params.m);

        // \forall i. \forall j. (i == j || index[i] != index[j])
        let index_uniq_check = HashSet::<Index>::from_iter(self.0.indices.iter().cloned()).len()
            == self.0.indices.len();

        // k-sized quorum
        let quorum_check = params.k as usize <= self.0.sigs.len()
            && params.k as usize <= self.0.evals.len()
            && params.k as usize <= self.0.indices.len();

        if !quorum_check {
            return false;
        }

        // \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
        let path_check = self.0.sigs[0..params.k as usize]
            .iter()
            .all(|sig| avk.check(&(sig.pk, sig.stake), sig.party, &sig.path));

        // \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
        let msp_evals = self.0.indices[0..params.k as usize]
            .iter()
            .zip(self.0.sigs[0..params.k as usize].iter())
            .map(|(idx, sig)| {
                let msgp = avk.concat_with_msg(msg);
                Msp::eval(&msgp, *idx, &sig.sigma)
            });
        let eval_check = self.0.evals[0..params.k as usize]
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e);

        // \forall i : [1..k]. ev[i] <= phi(stake_i)
        let eval_stake_check = self.0.evals[0..params.k as usize]
            .iter()
            .zip(&self.0.sigs[0..params.k as usize])
            .all(|(ev, sig)| ev_lt_phi(params.phi_f, *ev, sig.stake, total_stake));

        ivk_check
            && index_bound_check
            && index_uniq_check
            && path_check
            && eval_check
            && eval_stake_check
    }
}
