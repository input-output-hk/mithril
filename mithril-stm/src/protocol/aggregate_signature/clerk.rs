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
use crate::{LotteryTargetValue, VerificationKeyForSnark, proof_system::SnarkClerk};

use super::{AggregateSignature, AggregateSignatureType};

#[cfg(feature = "future_snark")]
use super::AggregationError;

/// Clerk for aggregate signatures.
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
                .closed_registration_entries
                .iter()
                .any(|e| e.get_verification_key_for_snark().is_some())
                .then(|| SnarkClerk::new_clerk_from_signer(signer)),
            phantom_data: PhantomData,
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
            #[cfg(feature = "future_snark")]
            snark_proof_clerk: closed_reg
                .closed_registration_entries
                .iter()
                .any(|e| e.get_verification_key_for_snark().is_some())
                .then(|| {
                    SnarkClerk::new_clerk_from_closed_key_registration(parameters, closed_reg)
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

    /// Get the SNARK clerk.
    #[cfg(feature = "future_snark")]
    pub fn get_snark_clerk(&self) -> Option<&SnarkClerk> {
        self.snark_proof_clerk.as_ref()
    }

    /// Compute the aggregate verification key.
    /// It computes only the concatenation aggregate verification key for now.
    // TODO: Replace None with the actual SNARK verification key when implementing
    // SNARK aggregation primitives.
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

    /// Get the SNARK registered party for a given index.
    #[cfg(feature = "future_snark")]
    pub fn get_snark_registered_party_for_index(
        &self,
        party_index: &LotteryIndex,
    ) -> StmResult<Option<(VerificationKeyForSnark, LotteryTargetValue)>> {
        if let Some(snark_clerk) = self.get_snark_clerk() {
            let entry = snark_clerk
                .closed_key_registration
                .get_registration_entry_for_index(party_index)?;
            Ok(entry
                .get_verification_key_for_snark()
                .zip(entry.get_lottery_target_value()))
        } else {
            Ok(None)
        }
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
