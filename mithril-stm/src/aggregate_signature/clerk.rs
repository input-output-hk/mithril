use blake2::digest::{Digest, FixedOutput};

use crate::{
    AggregationError, ClosedKeyRegistration, CoreVerifier, Index, Parameters, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, StmAggrSig, StmAggrVerificationKey, StmSigner,
    StmVerificationKey,
};

/// `StmClerk` can verify and aggregate `StmSig`s and verify `StmMultiSig`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct StmClerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: Digest + Clone + FixedOutput> StmClerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn from_registration(params: &Parameters, closed_reg: &ClosedKeyRegistration<D>) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a Clerk from a signer.
    pub fn from_signer(signer: &StmSigner<D>) -> Self {
        let closed_reg = signer
            .get_closed_reg()
            .clone()
            .expect("Core signer does not include closed registration. StmClerk, and so, the Stm certificate cannot be built without closed registration!")
            ;

        Self {
            params: signer.get_params(),
            closed_reg,
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `StmAggrSig`.
    pub fn aggregate(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> Result<StmAggrSig<D>, AggregationError> {
        let sig_reg_list = sigs
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: self.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

        let avk = StmAggrVerificationKey::from(&self.closed_reg);
        let msgp = avk.get_mt_commitment().concat_with_msg(msg);
        let mut unique_sigs = CoreVerifier::dedup_sigs_for_indices(
            &self.closed_reg.total_stake,
            &self.params,
            &msgp,
            &sig_reg_list,
        )?;

        unique_sigs.sort_unstable();

        let mt_index_list = unique_sigs
            .iter()
            .map(|sig_reg| sig_reg.sig.signer_index as usize)
            .collect::<Vec<usize>>();

        let batch_proof = self.closed_reg.merkle_tree.get_batched_path(mt_index_list);

        Ok(StmAggrSig {
            signatures: unique_sigs,
            batch_proof,
        })
    }

    /// Compute the `StmAggrVerificationKey` related to the used registration.
    pub fn compute_avk(&self) -> StmAggrVerificationKey<D> {
        StmAggrVerificationKey::from(&self.closed_reg)
    }

    /// Get the (VK, stake) of a party given its index.
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(StmVerificationKey, Stake)> {
        self.closed_reg
            .reg_parties
            .get(*party_index as usize)
            .map(|&r| r.into())
    }
}
