use anyhow::Context;

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::{
    AggregateVerificationKey, ClosedKeyRegistration, Index, MembershipDigest, Parameters, Signer,
    SingleSignature, Stake, StmResult, VerificationKeyForConcatenation,
    proof_system::{ConcatenationClerk, ConcatenationProof},
};

use super::{AggregateSignature, AggregateSignatureType};

#[cfg(feature = "future_snark")]
use super::AggregationError;

/// Clerk for aggregate signatures.
#[derive(Debug, Clone)]
pub struct Clerk {
    concatenation_proof_clerk: ConcatenationClerk,
}

impl Clerk {
    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer<D: MembershipDigest>(signer: &Signer<D>) -> Self {
        Self {
            concatenation_proof_clerk: ConcatenationClerk::new_clerk_from_signer(signer),
        }
    }

    /// Create a Clerk from a closed key registration.
    pub fn new_clerk_from_closed_key_registration(
        parameters: &Parameters,
        closed_reg: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            concatenation_proof_clerk: ConcatenationClerk::new_clerk_from_closed_key_registration(
                parameters, closed_reg,
            ),
        }
    }
    /// Aggregate a set of signatures with a given proof type.
    pub fn aggregate_signatures_with_type<D: MembershipDigest>(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
        aggregate_signature_type: AggregateSignatureType,
    ) -> StmResult<AggregateSignature<D>> {
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::aggregate_signatures(self.to_concatenation_clerk(), sigs, msg)
                    .with_context(|| {
                        format!(
                            "Signatures failed to aggregate for type {}",
                            AggregateSignatureType::Concatenation
                        )
                    })?,
            )),
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Future => Err(anyhow!(
                AggregationError::UnsupportedProofSystem(aggregate_signature_type)
            )),
        }
    }

    /// Get the concatenation clerk.
    pub fn to_concatenation_clerk(&self) -> &ConcatenationClerk {
        &self.concatenation_proof_clerk
    }

    /// Compute the aggregate verification key.
    /// It computes only the concatenation aggregate verification key for now.
    pub fn compute_aggregate_verification_key<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::Concatenation(
            self.concatenation_proof_clerk.compute_concatenation_proof_key(),
        )
    }

    /// Get the concatenation registered party for a given index.
    pub fn get_concatenation_registered_party_for_index(
        &self,
        party_index: &Index,
    ) -> StmResult<(VerificationKeyForConcatenation, Stake)> {
        let entry = self
            .to_concatenation_clerk()
            .closed_key_registration
            .key_registration
            .clone()
            .get_registration_entry_for_index(party_index)?;
        Ok((entry.get_bls_verification_key(), entry.get_stake()))
    }

    #[cfg(test)]
    pub fn update_k(&mut self, k: u64) {
        self.concatenation_proof_clerk.update_k(k);
    }

    #[cfg(test)]
    pub fn update_m(&mut self, m: u64) {
        self.concatenation_proof_clerk.update_m(m);
    }
}
