use anyhow::Context;

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::{
    AggregateVerificationKey, ClosedKeyRegistration, MembershipDigest, Parameters, Signer,
    SingleSignature, StmResult,
    proof_system::{ConcatenationClerk, ConcatenationProof},
};

use super::{AggregateSignature, AggregateSignatureType};

#[cfg(feature = "future_snark")]
use super::AggregationError;

/// `Clerk` can verify and aggregate `SingleSignature`s and verify `AggregateSignature`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
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
    pub fn to_concatenation_clerk(&self) -> &ConcatenationClerk {
        &self.concatenation_proof_clerk
    }

    pub fn compute_aggregate_verification_key<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::Concatenation(
            self.concatenation_proof_clerk.compute_aggregate_verification_key(),
        )
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
