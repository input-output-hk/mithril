//! Prove the validity of aggregated signatures.

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk};
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;

pub trait Proof {
    fn prove(
        avk: &MerkleTree,
        ivk: &MspMvk,
        msg: &[u8],
        sigs: &[StmSig],
        indices: &[Index],
        evals: &[u64],
    ) -> Self;
    fn verify(
        &self,
        params: &StmParameters,
        total_stake: u64,
        avk: &MerkleTree,
        ivk: &MspMvk,
        msg: &[u8],
    ) -> bool;
}

/// Proof system that simply concatenates the signatures.
#[derive(Clone)]
pub struct ConcatProof {
    sigs: Vec<StmSig>,
    indices: Vec<Index>,
    evals: Vec<u64>,
}

impl Proof for ConcatProof {
    fn prove(
        _avk: &MerkleTree,
        _ivk: &MspMvk,
        _msg: &[u8],
        sigs: &[StmSig],
        indices: &[Index],
        evals: &[u64],
    ) -> Self {
        Self {
            sigs: sigs.to_vec(),
            indices: indices.to_vec(),
            evals: evals.to_vec(),
        }
    }

    fn verify(
        &self,
        params: &StmParameters,
        total_stake: u64,
        avk: &MerkleTree,
        ivk: &MspMvk,
        msg: &[u8],
    ) -> bool {
        self.check_ivk(ivk)
            && self.check_index_bound(params)
            && self.check_index_unique()
            && self.check_quorum(params)
            && self.check_path(params, avk)
            && self.check_eval(params, avk, msg)
            && self.check_stake(params, total_stake)
    }
}

impl ConcatProof {
    /// ivk = Prod(1..k, mvk[i])
    fn check_ivk(&self, ivk: &MspMvk) -> bool {
        ivk.0 == self.sigs.iter().map(|s| s.pk.mvk.0).sum()
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
