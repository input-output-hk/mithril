use crate::proof::ProofSystem;

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk, MspSig};
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;

/// The statement we want to prove, namely that
/// the signature aggregated by our scheme with
/// the given MerkleTree, aggregated verification keys,
/// and aggregated signatures is valid for the
/// given message.
pub struct Statement<'l> {
    pub(crate) avk: &'l MerkleTree,
    pub(crate) ivk: &'l MspMvk,
    pub(crate) mu: &'l MspSig,
    pub(crate) msg: &'l [u8],
    pub(crate) params: &'l StmParameters,
    pub(crate) total_stake: u64,
}

#[derive(Clone)]
pub struct Witness {
    pub(crate) sigs: Vec<StmSig>,
    pub(crate) indices: Vec<Index>,
    pub(crate) evals: Vec<u64>,
}

// /// Proof system that simply concatenates the signatures.
// #[derive(Clone)]
// pub struct ConcatProof(Witness);
pub struct ConcatProof<'l> {
    p: std::marker::PhantomData<&'l ()>,
}

impl<'l> ConcatProof<'l> {
    pub fn new() -> Self {
        Self {
            p: std::marker::PhantomData,
        }
    }
}

impl Witness {
    fn verify<'l>(&self, stmt: &Statement<'l>) -> bool {
        self.check_quorum(stmt.params.k as usize)
            && self.check_ivk(stmt.ivk)
            && self.check_sum(stmt.mu)
            && self.check_index_bound(stmt.params.m)
            && self.check_index_unique()
            && self.check_path(stmt.avk)
            && self.check_eval(stmt.avk, stmt.msg)
            && self.check_stake(stmt.params.phi_f, stmt.total_stake)
    }

    /// ivk = Prod(1..k, mvk[i])
    /// requires that this proof has exactly k signatures
    fn check_ivk(&self, ivk: &MspMvk) -> bool {
        let mvks = self.sigs.iter().map(|s| s.pk.mvk).collect::<Vec<_>>();

        let sum = Msp::aggregate_keys(&mvks).0;
        ivk.0 == sum
    }

    /// mu = Prod(1..k, sigma[i])
    /// requires that this proof has exactly k signatures
    fn check_sum(&self, mu: &MspSig) -> bool {
        let mu1 = self.sigs.iter().map(|s| s.sigma.0).sum();
        mu.0 == mu1
    }

    /// \forall i. index[i] <= m
    /// requires that this proof has exactly k signatures
    fn check_index_bound(&self, m: u64) -> bool {
        self.indices.iter().all(|i| *i <= m)
    }

    /// \forall i. \forall j. (i == j || index[i] != index[j])
    /// requires that this proof has exactly k signatures
    fn check_index_unique(&self) -> bool {
        HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() == self.indices.len()
    }

    /// k-sized quorum
    /// if this returns `true`, then then there are exactly k signatures
    fn check_quorum(&self, k: usize) -> bool {
        k == self.sigs.len() && k == self.evals.len() && k == self.indices.len()
    }

    /// \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
    /// requires that this proof has exactly k signatures
    fn check_path(&self, avk: &MerkleTree) -> bool {
        self.sigs
            .iter()
            .all(|sig| avk.check(&(sig.pk, sig.stake), sig.party, &sig.path))
    }

    /// \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
    /// requires that this proof has exactly k signatures
    fn check_eval(&self, avk: &MerkleTree, msg: &[u8]) -> bool {
        let msp_evals = self.indices.iter().zip(self.sigs.iter()).map(|(idx, sig)| {
            let msgp = avk.concat_with_msg(msg);
            Msp::eval(&msgp, *idx, &sig.sigma)
        });

        self.evals
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e)
    }

    /// \forall i : [1..k]. ev[i] <= phi(stake_i)
    /// requires that this proof has exactly k signatures
    fn check_stake(&self, phi_f: f64, total_stake: u64) -> bool {
        self.evals
            .iter()
            .zip(&self.sigs)
            .all(|(ev, sig)| ev_lt_phi(phi_f, *ev, sig.stake, total_stake))
    }
}

impl<'l> ProofSystem for ConcatProof<'l> {
    type Statement = Statement<'l>;
    type Proof = Witness;
    type Witness = Witness;
    type ProvingKey = ();
    type VerificationKey = ();

    fn prove(&self, _pk: (), _stmt: Self::Statement, wit: Self::Witness) -> Self::Proof {
        wit
    }

    fn verify(&self, vk: (), proof: &Self::Proof, stmt: &Self::Statement) -> bool {
        proof.verify(&stmt)
    }
}
