use anyhow::Context;
use std::marker::PhantomData;

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::{
    AggregateVerificationKey, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    Signer, SingleSignature, Stake, StmResult, VerificationKeyForConcatenation,
    proof_system::{ConcatenationClerk, ConcatenationProof},
};

#[cfg(feature = "future_snark")]
use crate::proof_system::SnarkClerk;

use super::{AggregateSignature, AggregateSignatureType};

#[cfg(feature = "future_snark")]
use super::AggregationError;

/// Clerk for aggregate signatures.
///
/// Manages both the concatenation proof clerk and, when the `future_snark`
/// feature is enabled, the SNARK proof clerk. Provides methods for signature
/// aggregation and aggregate verification key computation.
#[derive(Debug, Clone)]
pub struct Clerk<D: MembershipDigest> {
    concatenation_proof_clerk: ConcatenationClerk,
    #[cfg(feature = "future_snark")]
    snark_proof_clerk: Option<SnarkClerk>,
    phantom_data: PhantomData<D>,
}

impl<D: MembershipDigest> Clerk<D> {
    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer(signer: &Signer<D>) -> Self {
        Self {
            concatenation_proof_clerk: ConcatenationClerk::new_clerk_from_signer(signer),
            #[cfg(feature = "future_snark")]
            snark_proof_clerk: signer
                .closed_key_registration
                .has_snark_verification_keys()
                .then(|| SnarkClerk::new_clerk_from_signer(signer)),
            phantom_data: PhantomData,
        }
    }

    /// Create a Clerk from a closed key registration.
    pub fn new_clerk_from_closed_key_registration(
        parameters: &Parameters,
        closed_registration: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            concatenation_proof_clerk: ConcatenationClerk::new_clerk_from_closed_key_registration(
                parameters,
                closed_registration,
            ),
            #[cfg(feature = "future_snark")]
            snark_proof_clerk: closed_registration.has_snark_verification_keys().then(|| {
                SnarkClerk::new_clerk_from_closed_key_registration(parameters, closed_registration)
            }),
            phantom_data: PhantomData,
        }
    }

    /// Aggregate a set of signatures with a given proof type.
    pub fn aggregate_signatures_with_type(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
        aggregate_signature_type: AggregateSignatureType,
    ) -> StmResult<AggregateSignature<D>> {
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::aggregate_signatures(self.get_concatenation_clerk(), sigs, msg)
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
    pub fn get_concatenation_clerk(&self) -> &ConcatenationClerk {
        &self.concatenation_proof_clerk
    }

    /// Compute the aggregate verification key covering both proof systems.
    pub fn compute_aggregate_verification_key(&self) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::new(
            self.concatenation_proof_clerk
                .compute_aggregate_verification_key_for_concatenation(),
            #[cfg(feature = "future_snark")]
            self.snark_proof_clerk
                .as_ref()
                .map(|clerk| clerk.compute_aggregate_verification_key_for_snark()),
        )
    }

    /// Get the concatenation registered party for a given index.
    pub fn get_concatenation_registered_party_for_index(
        &self,
        party_index: &LotteryIndex,
    ) -> StmResult<(VerificationKeyForConcatenation, Stake)> {
        let entry = self
            .get_concatenation_clerk()
            .closed_key_registration
            .get_registration_entry_for_index(party_index)?;
        Ok((
            entry.get_verification_key_for_concatenation(),
            entry.get_stake(),
        ))
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
