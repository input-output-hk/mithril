use blake2::digest::{Digest, FixedOutput};

use crate::{
    AggregateSignature, AggregateVerificationKey, AggregationError, BasicVerifier,
    ClosedKeyRegistration, Index, Parameters, Signer, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, VerificationKey,
};

/// `Clerk` can verify and aggregate `SingleSignature`s and verify `AggregateSignature`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct Clerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: Digest + Clone + FixedOutput> Clerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        params: &Parameters,
        closed_reg: &ClosedKeyRegistration<D>,
    ) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a new `Clerk` from a closed registration instance.
    #[deprecated(
        since = "0.4.9",
        note = "Use `new_clerk_from_closed_key_registration` instead"
    )]
    pub fn from_registration(params: &Parameters, closed_reg: &ClosedKeyRegistration<D>) -> Self {
        Self::new_clerk_from_closed_key_registration(params, closed_reg)
    }

    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer(signer: &Signer<D>) -> Self {
        let closed_reg = signer
            .get_closed_key_registration()
            .clone()
            .expect("Core signer does not include closed registration. Clerk, and so, the Stm certificate cannot be built without closed registration!")
            ;

        Self {
            params: signer.get_parameters(),
            closed_reg,
        }
    }

    /// Create a Clerk from a signer.
    #[deprecated(since = "0.4.9", note = "Use `new_clerk_from_signer` instead")]
    pub fn from_signer(signer: &Signer<D>) -> Self {
        Self::new_clerk_from_signer(signer)
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `AggregateSignature`.
    pub fn aggregate_signatures(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> Result<AggregateSignature<D>, AggregationError> {
        let sig_reg_list = sigs
            .iter()
            .map(|sig| SingleSignatureWithRegisteredParty {
                sig: sig.clone(),
                reg_party: self.closed_reg.reg_parties[sig.signer_index as usize],
            })
            .collect::<Vec<SingleSignatureWithRegisteredParty>>();

        let avk = AggregateVerificationKey::from(&self.closed_reg);
        let msgp = avk.get_merkle_tree_batch_commitment().concatenate_with_message(msg);
        let mut unique_sigs = BasicVerifier::select_valid_signatures_for_k_indices(
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

        let batch_proof = self
            .closed_reg
            .merkle_tree
            .compute_merkle_tree_batch_path(mt_index_list);

        Ok(AggregateSignature {
            signatures: unique_sigs,
            batch_proof,
        })
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `AggregateSignature`.
    #[deprecated(since = "0.4.9", note = "Use `aggregate_signatures` instead")]
    pub fn aggregate(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> Result<AggregateSignature<D>, AggregationError> {
        Self::aggregate_signatures(self, sigs, msg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    pub fn compute_aggregate_verification_key(&self) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::from(&self.closed_reg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    #[deprecated(
        since = "0.4.9",
        note = "Use `compute_aggregate_verification_key` instead"
    )]
    pub fn compute_avk(&self) -> AggregateVerificationKey<D> {
        Self::compute_aggregate_verification_key(self)
    }

    /// Get the (VK, stake) of a party given its index.
    pub fn get_registered_party_for_index(
        &self,
        party_index: &Index,
    ) -> Option<(VerificationKey, Stake)> {
        self.closed_reg
            .reg_parties
            .get(*party_index as usize)
            .map(|&r| r.into())
    }

    /// Get the (VK, stake) of a party given its index.
    #[deprecated(since = "0.4.9", note = "Use `get_registered_party_for_index` instead")]
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(VerificationKey, Stake)> {
        Self::get_registered_party_for_index(self, party_index)
    }
}
