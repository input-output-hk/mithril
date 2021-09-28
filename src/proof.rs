//! Prove the validity of aggregated signatures.

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk, MspSig};
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;

pub trait Proof {
    type Witness;
    type ProvingKey;
    type VerificationKey;

    fn prove(pk: &Self::ProvingKey, stmt: Box<dyn Fn(&Self::Witness) -> bool>, wit: Self::Witness) -> Self;

    fn verify(&self, vk: &Self::VerificationKey, stmt: Box<dyn Fn(&Self::Witness) -> bool>) -> bool;
}

#[derive(Clone)]
pub struct ConcatWitness {
    pub(crate) avk: MerkleTree,
    pub(crate) ivk: MspMvk,
    pub(crate) mu: MspSig,
    pub(crate) msg: Vec<u8>,
    pub(crate) sigs: Vec<StmSig>,
    pub(crate) indices: Vec<Index>,
    pub(crate) evals: Vec<u64>,
    pub(crate) params: StmParameters,
    pub(crate) total_stake: u64,
}

/// Proof system that simply concatenates the signatures.
#[derive(Clone)]
pub struct ConcatProof(ConcatWitness);

impl Proof for ConcatProof {
    type Witness = ConcatWitness;
    type ProvingKey = ();
    type VerificationKey = ();

    fn prove(_pk: &(), _stmt: Box<dyn Fn(&Self::Witness) -> bool>, wit: Self::Witness) -> Self {
        ConcatProof(wit)
    }

    fn verify(&self, vk: &(), stmt: Box<dyn Fn(&Self::Witness) -> bool>) -> bool {
        stmt(&self.0)
    }
}

impl ConcatWitness {
    pub fn check(&self) -> bool {
        self.check_quorum()
            && self.check_ivk()
            && self.check_sum()
            && self.check_index_bound()
            && self.check_index_unique()
            && self.check_path()
            && self.check_eval()
            && self.check_stake()
    }

    /// ivk = Prod(1..k, mvk[i])
    /// requires that this proof has exactly k signatures
    fn check_ivk(&self) -> bool {
        let mvks = self.sigs.iter().map(|s| s.pk.mvk).collect::<Vec<_>>();

        let sum = Msp::aggregate_keys(&mvks).0;
        self.ivk.0 == sum
    }

    /// mu = Prod(1..k, sigma[i])
    /// requires that this proof has exactly k signatures
    fn check_sum(&self) -> bool {
        let mu1 = self.sigs.iter().map(|s| s.sigma.0).sum();
        self.mu.0 == mu1
    }

    /// \forall i. index[i] <= m
    /// requires that this proof has exactly k signatures
    fn check_index_bound(&self) -> bool {
        self.indices.iter().all(|i| i <= &self.params.m)
    }

    /// \forall i. \forall j. (i == j || index[i] != index[j])
    /// requires that this proof has exactly k signatures
    fn check_index_unique(&self) -> bool {
        HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() == self.indices.len()
    }

    /// k-sized quorum
    /// if this returns `true`, then then there are exactly k signatures
    fn check_quorum(&self) -> bool {
        self.params.k as usize == self.sigs.len()
            && self.params.k as usize == self.evals.len()
            && self.params.k as usize == self.indices.len()
    }

    /// \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
    /// requires that this proof has exactly k signatures
    fn check_path(&self) -> bool {
        self.sigs
            .iter()
            .all(|sig| self.avk.check(&(sig.pk, sig.stake), sig.party, &sig.path))
    }

    /// \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
    /// requires that this proof has exactly k signatures
    fn check_eval(&self) -> bool {
        let msp_evals = self.indices.iter().zip(self.sigs.iter()).map(|(idx, sig)| {
            let msgp = self.avk.concat_with_msg(&self.msg);
            Msp::eval(&msgp, *idx, &sig.sigma)
        });

        self.evals
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e)
    }

    /// \forall i : [1..k]. ev[i] <= phi(stake_i)
    /// requires that this proof has exactly k signatures
    fn check_stake(&self) -> bool {
        self.evals
            .iter()
            .zip(&self.sigs)
            .all(|(ev, sig)| ev_lt_phi(self.params.phi_f, *ev, sig.stake, self.total_stake))
    }
}
