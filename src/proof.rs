//! Prove the validity of aggregated signatures.

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspSig, MspMvk};
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;

pub struct Statement<'l> {
    pub(crate) avk: &'l MerkleTree,
    pub(crate) ivk: &'l MspMvk,
    pub(crate) mu: &'l MspSig,
    pub(crate) msg: &'l [u8],
}

pub struct Witness<'l> {
    pub(crate) sigs: &'l [StmSig],
    pub(crate) indices: &'l [Index],
    pub(crate) evals: &'l [u64],
}

pub trait Proof {
    fn prove(stmt: Statement, witness: Witness) -> Self;

    fn verify(&self, params: &StmParameters, total_stake: u64, stmt: Statement) -> bool;
}

/// Proof system that simply concatenates the signatures.
#[derive(Clone)]
pub struct ConcatProof {
    sigs: Vec<StmSig>,
    indices: Vec<Index>,
    evals: Vec<u64>,
}

impl Proof for ConcatProof {
    fn prove(stmt: Statement, witness: Witness) -> Self {
        Self {
            sigs: witness.sigs.to_vec(),
            indices: witness.indices.to_vec(),
            evals: witness.evals.to_vec(),
        }
    }

    fn verify(&self, params: &StmParameters, total_stake: u64, stmt: Statement) -> bool {
        self.check_ivk(stmt.ivk)
            && self.check_sum(stmt.mu)
            && self.check_index_bound(params)
            && self.check_index_unique()
            && self.check_quorum(params)
            && self.check_path(params, stmt.avk)
            && self.check_eval(params, stmt.avk, stmt.msg)
            && self.check_stake(params, total_stake)
    }
}

impl ConcatProof {
    /// ivk = Prod(1..k, mvk[i])
    fn check_ivk(&self, ivk: &MspMvk) -> bool {
        ivk.0 == self.sigs.iter().map(|s| s.pk.mvk.0).sum()
    }

    fn check_sum(&self, mu: &MspSig) -> bool {
        let mu1 = self.sigs.iter().map(|s| s.sigma.0).sum();
        mu.0 == mu1
    }

    /// \forall i. index[i] <= m
    fn check_index_bound(&self, params: &StmParameters) -> bool {
        self.indices.iter().all(|i| i <= &params.m)
    }

    /// \forall i. \forall j. (i == j || index[i] != index[j])
    fn check_index_unique(&self) -> bool {
        HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() == self.indices.len()
    }

    /// k-sized quorum
    fn check_quorum(&self, params: &StmParameters) -> bool {
        params.k as usize <= self.sigs.len()
            && params.k as usize <= self.evals.len()
            && params.k as usize <= self.indices.len()
    }

    /// \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
    fn check_path(&self, params: &StmParameters, avk: &MerkleTree) -> bool {
        self.sigs[0..params.k as usize]
            .iter()
            .all(|sig| avk.check(&(sig.pk, sig.stake), sig.party, &sig.path))
    }

    /// \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
    fn check_eval(&self, params: &StmParameters, avk: &MerkleTree, msg: &[u8]) -> bool {
        let msp_evals = self.indices[0..params.k as usize]
            .iter()
            .zip(self.sigs[0..params.k as usize].iter())
            .map(|(idx, sig)| {
                let msgp = avk.concat_with_msg(msg);
                Msp::eval(&msgp, *idx, &sig.sigma)
            });

        self.evals[0..params.k as usize]
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e)
    }

    /// \forall i : [1..k]. ev[i] <= phi(stake_i)
    fn check_stake(&self, params: &StmParameters, total_stake: u64) -> bool {
        self.evals[0..params.k as usize]
            .iter()
            .zip(&self.sigs[0..params.k as usize])
            .all(|(ev, sig)| ev_lt_phi(params.phi_f, *ev, sig.stake, total_stake))
    }
}
