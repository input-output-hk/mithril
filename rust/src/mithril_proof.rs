//! Prove the validity of aggregated signatures.

use super::Index;
use crate::concat_avk_with_msg;
use crate::ev_lt_phi;
use crate::merkle_tree::{MTHashLeaf, MerkleTree};
use crate::msp::{Msp, MspMvk, MspSig};
use crate::proof::Proof;
use crate::stm::{MTValue, StmParameters, StmSig};
use ark_ec::PairingEngine;
use std::collections::HashSet;
use std::iter::FromIterator;
use std::rc::Rc;

/// The statement we want to prove, namely that
/// the signature aggregated by our scheme with
/// the given MerkleTree, aggregated verification keys,
/// and aggregated signatures is valid for the
/// given message.
pub struct Statement<PE: PairingEngine, H: MTHashLeaf<MTValue<PE>>> {
    // We use Rc here to avoid exposing a lifetime parameter. Parameterizing
    // Statement by a lifetime ends up bubbling the lifetime to whoever is
    // producing proofs, effectively tying the lifetime of the proof to that of
    // the prover, which is undesirable.
    pub(crate) avk: Rc<MerkleTree<MTValue<PE>, H>>,
    pub(crate) ivk: MspMvk<PE>,
    pub(crate) mu: MspSig<PE>,
    pub(crate) msg: Vec<u8>,
    pub(crate) params: StmParameters,
    pub(crate) total_stake: u64,
}

/// A Witness is an aggregation of signatures
#[derive(Debug, Clone)]
pub struct Witness<PE: PairingEngine, H: MTHashLeaf<MTValue<PE>>> {
    pub(crate) sigs: Vec<StmSig<PE, H::F>>,
    pub(crate) indices: Vec<Index>,
    pub(crate) evals: Vec<u64>,
}

impl<PE: PairingEngine, H: MTHashLeaf<MTValue<PE>>> Witness<PE, H> {
    fn verify(&self, stmt: &Statement<PE, H>) -> bool {
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
    fn check_ivk(&self, ivk: &MspMvk<PE>) -> bool {
        let mvks = self.sigs.iter().map(|s| s.pk.mvk).collect::<Vec<_>>();

        let sum = Msp::aggregate_keys(&mvks).0;
        ivk.0 == sum
    }

    /// mu = Prod(1..k, sigma[i])
    fn check_sum(&self, mu: &MspSig<PE>) -> bool {
        let mu1 = self.sigs.iter().map(|s| s.sigma.0).sum();
        mu.0 == mu1
    }

    /// \forall i. index[i] <= m
    fn check_index_bound(&self, m: u64) -> bool {
        self.indices.iter().all(|i| *i <= m)
    }

    /// \forall i. \forall j. (i == j || index[i] != index[j])
    fn check_index_unique(&self) -> bool {
        HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() == self.indices.len()
    }

    /// k-sized quorum
    /// if this returns `true`, then there are exactly k signatures
    fn check_quorum(&self, k: usize) -> bool {
        k == self.sigs.len() && k == self.evals.len() && k == self.indices.len()
    }

    /// \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
    fn check_path(&self, avk: &MerkleTree<MTValue<PE>, H>) -> bool {
        self.sigs
            .iter()
            .all(|sig| avk.check(&MTValue(sig.pk.mvk, sig.stake), sig.party, &sig.path))
    }

    /// \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
    fn check_eval(&self, avk: &MerkleTree<MTValue<PE>, H>, msg: &[u8]) -> bool {
        let msp_evals = self.indices.iter().zip(self.sigs.iter()).map(|(idx, sig)| {
            let msgp = concat_avk_with_msg(avk, msg);
            Msp::eval(&msgp, *idx, &sig.sigma)
        });

        self.evals
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e)
    }

    /// \forall i : [1..k]. ev[i] <= phi(stake_i)
    fn check_stake(&self, phi_f: f64, total_stake: u64) -> bool {
        self.evals
            .iter()
            .zip(&self.sigs)
            .all(|(ev, sig)| ev_lt_phi(phi_f, *ev, sig.stake, total_stake))
    }
}

/// A MithrilProof just fixes the relation to a constant.
pub trait MithrilProof: Proof {
    const RELATION: Self::Relation;
}

pub mod concat_proofs {
    //! This is the trivial proof system instantiated to Mithril: witnesses are
    //! just the aggregated signatures themselves.
    use super::{MithrilProof, Statement, Witness};
    use crate::merkle_tree::MTHashLeaf;
    pub use crate::proof::trivial::TrivialEnv;
    use crate::proof::trivial::TrivialProof;
    use crate::stm::MTValue;
    use ark_ec::PairingEngine;

    pub type ConcatEnv = TrivialEnv;
    pub type ConcatRel<PE, H> = fn(&Statement<PE, H>, &Witness<PE, H>) -> bool;
    pub type ConcatProof<PE, H> = TrivialProof<Statement<PE, H>, ConcatRel<PE, H>, Witness<PE, H>>;

    impl<PE, H> MithrilProof for ConcatProof<PE, H>
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
    {
        const RELATION: ConcatRel<PE, H> = trivial_relation;
    }

    fn trivial_relation<PE, H>(s: &Statement<PE, H>, w: &Witness<PE, H>) -> bool
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
    {
        w.verify(s)
    }
}
