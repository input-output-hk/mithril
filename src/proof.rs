//! Prove the validity of aggregated signatures.

use super::Index;
use crate::ev_lt_phi;
use crate::merkle_tree::{HashLeaf, MerkleTree};
use crate::msp::{Msp, MspMvk};
use crate::stm::{StmParameters, StmSig, MTValue};

use ark_ec::PairingEngine;
use std::collections::HashSet;
use std::iter::FromIterator;

pub trait Proof<PE: PairingEngine, H: HashLeaf<MTValue<PE>>> {
    fn prove(
        avk: &MerkleTree<MTValue<PE>,H>,
        ivk: &MspMvk<PE>,
        msg: &[u8],
        sigs: &[StmSig<PE, H::F>],
        indices: &[Index],
        evals: &[u64],
    ) -> Self;
    fn verify(
        &self,
        params: &StmParameters,
        total_stake: u64,
        avk: &MerkleTree<MTValue<PE>,H>,
        ivk: &MspMvk<PE>,
        msg: &[u8],
    ) -> bool;
}

/// Proof system that simply concatenates the signatures.
#[derive(Clone)]
pub struct ConcatProof<P, F>
where
    P: PairingEngine,
{
    sigs: Vec<StmSig<P, F>>,
    indices: Vec<Index>,
    evals: Vec<u64>,
}

impl<P, H> Proof<P, H> for ConcatProof<P, H::F>
where
    P: PairingEngine,
    H: HashLeaf<MTValue<P>>,
{
    fn prove(
        _avk: &MerkleTree<MTValue<P>,H>,
        _ivk: &MspMvk<P>,
        _msg: &[u8],
        sigs: &[StmSig<P, H::F>],
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
        avk: &MerkleTree<MTValue<P>,H>,
        ivk: &MspMvk<P>,
        msg: &[u8],
    ) -> bool {
        // ivk = Prod(1..k, mvk[i])
        let ivk_check = ivk.0 == self.sigs.iter().map(|s| s.pk.mvk.0).sum();

        // \forall i. index[i] <= m
        let index_bound_check = self.indices.iter().all(|i| i <= &params.m);

        // \forall i. \forall j. (i == j || index[i] != index[j])
        let index_uniq_check =
            HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() == self.indices.len();

        // k-sized quorum
        let quorum_check = params.k as usize <= self.sigs.len()
            && params.k as usize <= self.evals.len()
            && params.k as usize <= self.indices.len();

        if !quorum_check {
            return false;
        }

        // \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
        let path_check = self.sigs[0..params.k as usize].iter().all(|sig| {
            avk.check(&MTValue(sig.pk.mvk, sig.stake),
                sig.party,
                &sig.path,
            )
        });

        // \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
        let msp_evals = self.indices[0..params.k as usize]
            .iter()
            .zip(self.sigs[0..params.k as usize].iter())
            .map(|(idx, sig)| {
                let msgp = avk.concat_with_msg(msg);
                Msp::eval(&msgp, *idx, &sig.sigma)
            });
        let eval_check = self.evals[0..params.k as usize]
            .iter()
            .zip(msp_evals)
            .all(|(ev, msp_e)| *ev == msp_e);

        // \forall i : [1..k]. ev[i] <= phi(stake_i)
        let eval_stake_check = self.evals[0..params.k as usize]
            .iter()
            .zip(&self.sigs[0..params.k as usize])
            .all(|(ev, sig)| ev_lt_phi(params.phi_f, *ev, sig.stake, total_stake));

        ivk_check
            && index_bound_check
            && index_uniq_check
            && path_check
            && eval_check
            && eval_stake_check
    }
}
