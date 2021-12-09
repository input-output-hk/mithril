//! Prove the validity of aggregated signatures.

use crate::merkle_tree::{concat_avk_with_msg, MTHashLeaf, MerkleTree, Path};
use crate::msp::{Msp, MspMvk, MspSig};
use crate::proof::Proof;
use crate::stm::{ev_lt_phi, Index, MTValue, StmParameters, StmSig};
use ark_ec::PairingEngine;
use ark_ff::{FromBytes, ToBytes};
use std::collections::HashSet;
use std::convert::TryInto;
use std::io::{Read, Write};
use std::iter::FromIterator;
use std::rc::Rc;

/// The statement we want to prove, namely that
/// the signature aggregated by our scheme with
/// the given MerkleTree, aggregated verification keys,
/// and aggregated signatures is valid for the
/// given message.
pub struct MithrilStatement<PE: PairingEngine, H: MTHashLeaf<MTValue<PE>>> {
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

/// A MithrilWitness is an aggregation of signatures
#[derive(Debug, Clone)]
pub struct MithrilWitness<PE: PairingEngine, H: MTHashLeaf<MTValue<PE>>> {
    pub(crate) sigs: Vec<StmSig<PE, H::F>>,
    pub(crate) indices: Vec<Index>,
    pub(crate) evals: Vec<[u8; 64]>,
}

/// Errors which can be output by Mithril verification.
#[derive(Debug, Clone, thiserror::Error)]
pub enum MithrilWitnessError<PE: PairingEngine, F> {
    /// No quorum was found
    #[error("No Quorum was found.")]
    NoQuorum,
    /// The IVK is invalid after aggregating the keys
    #[error("Aggregated key does not correspond to the expected key.")]
    IvkInvalid(MspMvk<PE>),
    /// Mu is not the sum of the signatures
    #[error("Aggregated signature does not correspond to the expected signature.")]
    SumInvalid(MspSig<PE>),
    /// There is an index out of bounds
    #[error("Received index, {0}, is higher than what the security parameter allows, {1}.")]
    IndexBoundFailed(u64, u64),
    /// There is a duplicate index
    #[error("Indeces are not unique.")]
    IndexNotUnique,
    /// The path is not valid for the Merkle Tree
    #[error("The path of the Merkle Tree is invalid.")]
    PathInvalid(Path<F>),
    /// MSP.Eval was computed incorrectly
    #[error("The claimed evaluation of function phi is incorrect.")]
    EvalInvalid([u8; 64]),
    /// A party did not actually win the lottery
    #[error("The current party did not win the lottery.")]
    StakeInvalid,
}

#[allow(clippy::from_over_into)]
impl<PE: PairingEngine, F> Into<i64> for MithrilWitnessError<PE, F> {
    fn into(self) -> i64 {
        // -1 is reserved to the function failing.
        match self {
            MithrilWitnessError::NoQuorum => -2,
            MithrilWitnessError::IvkInvalid(_) => -3,
            MithrilWitnessError::SumInvalid(_) => -4,
            MithrilWitnessError::IndexBoundFailed(_, _) => -5,
            MithrilWitnessError::IndexNotUnique => -6,
            MithrilWitnessError::PathInvalid(_) => -7,
            MithrilWitnessError::EvalInvalid(_) => -8,
            MithrilWitnessError::StakeInvalid => -9,
        }
    }
}

impl<PE, H> MithrilWitness<PE, H>
where
    PE: PairingEngine,
    H: MTHashLeaf<MTValue<PE>>,
{
    fn verify(&self, stmt: &MithrilStatement<PE, H>) -> Result<(), MithrilWitnessError<PE, H::F>> {
        self.check_quorum(stmt.params.k as usize)?;
        self.check_ivk(&stmt.ivk)?;
        self.check_sum(&stmt.mu)?;
        self.check_index_bound(stmt.params.m)?;
        self.check_index_unique()?;
        self.check_path(&stmt.avk)?;
        self.check_eval(&stmt.avk, &stmt.msg)?;
        self.check_stake(stmt.params.phi_f, stmt.total_stake)?;
        Ok(())
    }

    /// ivk = Prod(1..k, mvk[i])
    fn check_ivk(&self, ivk: &MspMvk<PE>) -> Result<(), MithrilWitnessError<PE, H::F>> {
        let mvks = self.sigs.iter().map(|s| s.pk.mvk).collect::<Vec<_>>();
        let sum = Msp::aggregate_keys(&mvks).0;
        if ivk.0 != sum {
            return Err(MithrilWitnessError::IvkInvalid(MspMvk(sum)));
        }
        Ok(())
    }

    /// mu = Prod(1..k, sigma[i])
    fn check_sum(&self, mu: &MspSig<PE>) -> Result<(), MithrilWitnessError<PE, H::F>> {
        let mu1 = self.sigs.iter().map(|s| s.sigma.0).sum();
        if mu.0 != mu1 {
            return Err(MithrilWitnessError::SumInvalid(MspSig(mu1)));
        }
        Ok(())
    }

    /// \forall i. index[i] <= m
    fn check_index_bound(&self, m: u64) -> Result<(), MithrilWitnessError<PE, H::F>> {
        for &i in self.indices.iter() {
            if i > m {
                return Err(MithrilWitnessError::IndexBoundFailed(i, m));
            }
        }
        Ok(())
    }

    /// \forall i. \forall j. (i == j || index[i] != index[j])
    fn check_index_unique(&self) -> Result<(), MithrilWitnessError<PE, H::F>> {
        if HashSet::<Index>::from_iter(self.indices.iter().cloned()).len() != self.indices.len() {
            return Err(MithrilWitnessError::IndexNotUnique);
        }
        Ok(())
    }

    /// k-sized quorum
    /// if this returns `true`, then there are exactly k signatures
    fn check_quorum(&self, k: usize) -> Result<(), MithrilWitnessError<PE, H::F>> {
        if !(k == self.sigs.len() && k == self.evals.len() && k == self.indices.len()) {
            return Err(MithrilWitnessError::NoQuorum);
        }
        Ok(())
    }

    /// \forall i : [0..k]. path[i] is a witness for (mvk[i]), stake[i] in avk
    fn check_path(
        &self,
        avk: &MerkleTree<MTValue<PE>, H>,
    ) -> Result<(), MithrilWitnessError<PE, H::F>> {
        for sig in self.sigs.iter() {
            if !avk.check(&MTValue(sig.pk.mvk, sig.stake), sig.party, &sig.path) {
                return Err(MithrilWitnessError::PathInvalid(sig.path.clone()));
            }
        }
        Ok(())
    }

    /// \forall i : [1..k]. ev[i] = MSP.Eval(msg, index[i], sig[i])
    fn check_eval(
        &self,
        avk: &MerkleTree<MTValue<PE>, H>,
        msg: &[u8],
    ) -> Result<(), MithrilWitnessError<PE, H::F>> {
        let msp_evals = self.indices.iter().zip(self.sigs.iter()).map(|(idx, sig)| {
            let msgp = concat_avk_with_msg(avk, msg);
            Msp::eval(&msgp, *idx, &sig.sigma)
        });

        for (&ev, msp_e) in self.evals.iter().zip(msp_evals) {
            if ev != msp_e {
                return Err(MithrilWitnessError::EvalInvalid(ev));
            }
        }
        Ok(())
    }

    /// \forall i : [1..k]. ev[i] <= phi(stake_i)
    fn check_stake(
        &self,
        phi_f: f64,
        total_stake: u64,
    ) -> Result<(), MithrilWitnessError<PE, H::F>> {
        if !self
            .evals
            .iter()
            .zip(&self.sigs)
            .all(|(ev, sig)| ev_lt_phi(phi_f, *ev, sig.stake, total_stake))
        {
            return Err(MithrilWitnessError::StakeInvalid);
        }
        Ok(())
    }
}

impl<PE, H> ToBytes for MithrilWitness<PE, H>
where
    PE: PairingEngine,
    H: MTHashLeaf<MTValue<PE>>,
    H::F: ToBytes,
{
    fn write<W: Write>(&self, mut writer: W) -> std::io::Result<()> {
        let n: u64 = self.sigs.len().try_into().unwrap();
        n.write(&mut writer)?;
        self.sigs.write(&mut writer)?;
        self.indices.write(&mut writer)?;
        self.evals.write(&mut writer)
    }
}
impl<PE, H> FromBytes for MithrilWitness<PE, H>
where
    PE: PairingEngine,
    H: MTHashLeaf<MTValue<PE>>,
    H::F: FromBytes,
{
    fn read<R: Read>(mut reader: R) -> std::io::Result<Self> {
        let n = u64::read(&mut reader)?;
        let mut sigs: Vec<StmSig<PE, H::F>> = Vec::with_capacity(n as usize);
        let mut indices: Vec<Index> = Vec::with_capacity(n as usize);
        let mut evals: Vec<[u8; 64]> = Vec::with_capacity(n as usize);
        for _ in 0..n {
            let s = StmSig::<PE, H::F>::read(&mut reader)?;
            sigs.push(s);
        }
        for _ in 0..n {
            let idx = Index::read(&mut reader)?;
            indices.push(idx);
        }
        let mut ev = [0u8; 64];
        for _ in 0..n {
            reader.read_exact(&mut ev)?;
            evals.push(ev);
        }

        Ok(MithrilWitness {
            sigs,
            indices,
            evals,
        })
    }
}

/// A MithrilProof just fixes the relation to a constant.
pub trait MithrilProof: Proof + ToBytes + FromBytes {
    /// The relation is a constant.
    const RELATION: Self::Relation;
}

pub mod concat_proofs {
    //! This is the trivial proof system instantiated to Mithril: witnesses are
    //! just the aggregated signatures themselves.
    use super::{MithrilProof, MithrilStatement, MithrilWitness, MithrilWitnessError};
    use crate::merkle_tree::MTHashLeaf;
    use crate::proof::trivial::TrivialProof;
    pub use crate::proof::trivial::{TrivialEnv, TrivialError};
    use crate::stm::MTValue;
    use ark_ec::PairingEngine;
    use ark_ff::{FromBytes, ToBytes};
    use std::io::{Read, Write};

    /// Set the env to the TrivialEnv.
    pub type ConcatEnv = TrivialEnv;
    /// The relation is a function outputting an error or not.
    pub type ConcatRel<PE, H, F> = fn(
        &MithrilStatement<PE, H>,
        &MithrilWitness<PE, H>,
    ) -> Result<(), MithrilWitnessError<PE, F>>;
    /// The proof is a TrivialProof.
    pub type ConcatProof<PE, H, F> =
        TrivialProof<MithrilStatement<PE, H>, ConcatRel<PE, H, F>, MithrilWitness<PE, H>>;

    impl<PE, H> MithrilProof for ConcatProof<PE, H, H::F>
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
        H::F: ToBytes + FromBytes,
    {
        const RELATION: ConcatRel<PE, H, H::F> = trivial_relation;
    }

    impl<PE, H> ToBytes for ConcatProof<PE, H, H::F>
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
        H::F: ToBytes,
    {
        fn write<W: Write>(&self, writer: W) -> std::io::Result<()> {
            self.witness.write(writer)
        }
    }

    impl<PE, H> FromBytes for ConcatProof<PE, H, H::F>
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
        H::F: FromBytes,
    {
        fn read<R: Read>(reader: R) -> std::io::Result<Self> {
            let witness = MithrilWitness::<PE, H>::read(reader)?;

            Ok(TrivialProof::new(witness))
        }
    }

    fn trivial_relation<PE, H>(
        s: &MithrilStatement<PE, H>,
        w: &MithrilWitness<PE, H>,
    ) -> Result<(), MithrilWitnessError<PE, H::F>>
    where
        PE: PairingEngine,
        H: MTHashLeaf<MTValue<PE>>,
    {
        w.verify(s)
    }
}
