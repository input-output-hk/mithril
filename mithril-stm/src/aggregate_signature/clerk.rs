use blake2::digest::{Digest, FixedOutput};

use crate::{
    AggregateSignature, AggregateSignatureType, AggregateVerificationKey, AggregationError,
    ClosedKeyRegistration, Index, Parameters, Signer, SingleSignature, Stake, VerificationKey,
    aggregate_signature::ConcatenationProof,
};

/// `Clerk` can verify and aggregate `SingleSignature`s and verify `AggregateSignature`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct Clerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: Digest + Clone + FixedOutput + Send + Sync> Clerk<D> {
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
        since = "0.5.0",
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
    #[deprecated(since = "0.5.0", note = "Use `new_clerk_from_signer` instead")]
    pub fn from_signer(signer: &Signer<D>) -> Self {
        Self::new_clerk_from_signer(signer)
    }

    /// Aggregate a set of signatures.
    #[deprecated(since = "0.5.3", note = "Use `aggregate_signatures_with_type` instead")]
    pub fn aggregate_signatures(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> Result<AggregateSignature<D>, AggregationError> {
        self.aggregate_signatures_with_type(sigs, msg, AggregateSignatureType::default())
    }

    /// Aggregate a set of signatures with a given proof type.
    pub fn aggregate_signatures_with_type(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
        aggregate_signature_type: AggregateSignatureType,
    ) -> Result<AggregateSignature<D>, AggregationError> {
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::aggregate_signatures(self, sigs, msg)?,
            )),
            #[cfg(feature = "future_proof_system")]
            AggregateSignatureType::Future => Err(AggregationError::UnsupportedProofSystem(
                aggregate_signature_type,
            )),
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `AggregateSignature`.
    #[deprecated(since = "0.5.0", note = "Use `aggregate_signatures` instead")]
    #[allow(deprecated)]
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
        since = "0.5.0",
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
    #[deprecated(since = "0.5.0", note = "Use `get_registered_party_for_index` instead")]
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(VerificationKey, Stake)> {
        Self::get_registered_party_for_index(self, party_index)
    }
}
