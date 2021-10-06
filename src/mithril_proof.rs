//! Prove the validity of aggregated signatures.

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::MerkleTree;
use crate::msp::{Msp, MspMvk, MspSig};
use crate::proof::Proof;
use crate::proof::ProverEnv;
use crate::stm::{StmParameters, StmSig};

use std::collections::HashSet;
use std::iter::FromIterator;
use std::rc::Rc;

/// The statement we want to prove, namely that
/// the signature aggregated by our scheme with
/// the given MerkleTree, aggregated verification keys,
/// and aggregated signatures is valid for the
/// given message.
pub struct Statement {
    pub(crate) avk: Rc<MerkleTree>,
    pub(crate) ivk: Rc<MspMvk>,
    pub(crate) mu: Rc<MspSig>,
    pub(crate) msg: Rc<[u8]>,
    pub(crate) params: Rc<StmParameters>,
    pub(crate) total_stake: u64,
}

/// A Witness is an aggregation of signatures
#[derive(Clone)]
pub struct Witness {
    pub(crate) sigs: Vec<StmSig>,
    pub(crate) indices: Vec<Index>,
    pub(crate) evals: Vec<u64>,
}

impl Witness {
    fn verify<'l>(&self, stmt: &Statement) -> bool {
        self.check_quorum(stmt.params.k as usize)
            && self.check_ivk(&stmt.ivk)
            && self.check_sum(&stmt.mu)
            && self.check_index_bound(stmt.params.m)
            && self.check_index_unique()
            && self.check_path(&stmt.avk)
            && self.check_eval(&stmt.avk, &stmt.msg)
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

/// Here we fix the type of statements and witnesses to those defined by the Mithril protocol.
/// Implementations of `MithrilProof` fix the relation as well.
pub trait MithrilProof<Env>: Proof<Env, Self::S, Self::Relation, Self::W>
where
    Env: ProverEnv,
{
    type S: From<Statement>;
    type W: From<Witness>;
    type Relation;
    const RELATION: Self::Relation;
}

pub mod concat_proofs {
    //! This is the trivial proof system instantiated to Mithril: witnesses are
    //! just the aggregated signatures themselves.
    use super::{MithrilProof, Statement, Witness};
    pub use crate::proof::trivial::TrivialEnv;
    use crate::proof::trivial::TrivialProof;

    pub type ConcatProof = TrivialProof<Witness>;

    impl MithrilProof<TrivialEnv> for TrivialProof<Witness>
    {
        type S = Statement;
        type W = Witness;
        type Relation = fn(&Statement, &Witness) -> bool;
        const RELATION: fn(&Statement, &Witness) -> bool = trivial_relation;
    }

    fn trivial_relation<'l>(s: &Statement, w: &Witness) -> bool {
        w.verify(s)
    }
}
