use anyhow::Context;
use rand_core::OsRng;
use std::marker::PhantomData;

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::{
    AggregateVerificationKey, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    Signer, SingleSignature, SnarkProof, Stake, StmResult, VerificationKeyForConcatenation,
    proof_system::{
        ConcatenationClerk, ConcatenationProof, IvcRollingState, ivc_halo2_snark::proof::IvcProof,
    },
};

use crate::{
    AncillaryProverData, MithrilMembershipDigest,
    circuits::{halo2_ivc::state::Global, trusted_setup::TrustedSetupProvider},
    proof_system::ivc_halo2_snark::{
        IvcProverSetup, TempCertificateKeyProvider, TempIvcKeyProvider,
        proof::IvcGenesisBootstrapInput,
    },
};

#[cfg(feature = "future_snark")]
use crate::AggregateSignatureError;
#[cfg(feature = "future_snark")]
use crate::proof_system::ivc_halo2_snark::proof::IvcProver;
#[cfg(feature = "future_snark")]
use crate::proof_system::{MERKLE_TREE_DEPTH_FOR_SNARK, SnarkClerk, SnarkProver};

use super::{
    AggregateSignature, AggregateSignatureType, AncillaryProofInput, AncillaryProofOutput,
};

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
        ancillary_input: AncillaryProofInput,
    ) -> StmResult<(AggregateSignature<D>, AncillaryProofOutput)> {
        let _ = ancillary_input;
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => {
                let proof = ConcatenationProof::aggregate_signatures(
                    self.get_concatenation_clerk(),
                    sigs,
                    msg,
                )
                .with_context(|| {
                    format!(
                        "Signatures failed to aggregate for type {}",
                        AggregateSignatureType::Concatenation
                    )
                })?;
                Ok((
                    AggregateSignature::Concatenation(Box::new(proof)),
                    AncillaryProofOutput::new(None, None),
                ))
            }
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Snark => {
                let clerk = self
                    .get_snark_clerk()
                    .ok_or_else(|| anyhow!(AggregateSignatureError::MissingSnarkClerk))?;
                SnarkProver::try_new_non_deterministic(
                    &clerk.parameters,
                    MERKLE_TREE_DEPTH_FOR_SNARK,
                )?
                .aggregate_signatures(clerk, sigs, msg)
                .map(|p| {
                    (
                        AggregateSignature::Snark(Box::new(p)),
                        AncillaryProofOutput::new(None, None),
                    )
                })
                .with_context(|| {
                    format!(
                        "Signatures failed to aggregate for type {}",
                        AggregateSignatureType::Snark
                    )
                })
            }
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::IvcSnark => {
                let clerk = self
                    .get_snark_clerk()
                    .ok_or_else(|| anyhow!(AggregateSignatureError::MissingSnarkClerk))?;
                let snark_proof = SnarkProver::try_new_non_deterministic(
                    &clerk.parameters,
                    MERKLE_TREE_DEPTH_FOR_SNARK,
                )?
                .aggregate_signatures::<MithrilMembershipDigest>(clerk, sigs, msg)
                .with_context(|| {
                    format!(
                        "Signatures failed to aggregate for type {}",
                        AggregateSignatureType::Snark
                    )
                })?;

                // For now the function will fail as we give two Some values
                // which should not happen but this might be changed
                let proof_output = ivc_prover_input_preparation_and_prove(
                    snark_proof,
                    msg,
                    clerk,
                    ancillary_input,
                )?;

                let ancillary_prover_data = AncillaryProverData::IvcSnark(
                    proof_output.1.ok_or_else(|| anyhow!("missing rolling state"))?,
                );

                // TODO: add the AncillaryProverData and AncillaryVerifierData to the output
                Ok((AggregateSignature::IvcSnark(Box::new(proof_output.0)), None))
            }
        }
    }

    /// Get the concatenation clerk.
    pub fn get_concatenation_clerk(&self) -> &ConcatenationClerk {
        &self.concatenation_proof_clerk
    }

    /// Get the SNARK clerk, if available.
    #[cfg(feature = "future_snark")]
    pub fn get_snark_clerk(&self) -> Option<&SnarkClerk> {
        self.snark_proof_clerk.as_ref()
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

fn ivc_prover_input_preparation_and_prove<D: MembershipDigest>(
    snark_proof: SnarkProof<D>,
    msg: &[u8],
    clerk: &SnarkClerk,
    ancillary_input: AncillaryProofInput,
) -> StmResult<(IvcProof<blake2b_simd::State>, Option<IvcRollingState>)> {
    // TODO: get the protocol message preimage
    let protocol_message_preimage = todo!();
    // TODO: get the protocol message preimage
    let genesis_verification_key = todo!();
    // TODO: get the genesis message
    let genesis_message = todo!();

    let trusted_setup_provider = TrustedSetupProvider::default();
    let srs = trusted_setup_provider.get_trusted_setup_parameters()?;
    let certificate_key_provider =
        TempCertificateKeyProvider::new(srs.into(), clerk.parameters, MERKLE_TREE_DEPTH_FOR_SNARK);
    let ivc_key_provider =
        TempIvcKeyProvider::new(srs.into(), certificate_key_provider.get_verifying_key()?);

    let ivc_setup = IvcProverSetup::load(
        &trusted_setup_provider,
        &certificate_key_provider,
        &ivc_key_provider,
    )?;

    let prover = IvcProver {
        ivc_setup: ivc_setup.into(),
        rng: OsRng,
    };

    // This is only used to get the root of the tree so maybe we can replace
    // the avk inputs by the root directly
    let avk = clerk.compute_aggregate_verification_key_for_snark();

    let global = Global::new(
        genesis_message,
        genesis_verification_key,
        &certificate_key_provider.get_verifying_key()?,
        &ivc_key_provider.get_verifying_key()?,
    );

    let rolling_state = ancillary_input
        .prover_data()
        .ok_or_else(|| anyhow!("Missing prover data"))?
        .as_ivc_rolling_state();

    // IvcGenesisBootstrapInput contains the same values as AncillaryGenesisData
    // We might consider dropping one or implementing a conversion between the two
    let genesis_bootstrap = ancillary_input.genesis_data().try_into()?;

    // For now the function will fail as we give two Some values
    // which should not happen but this might be changed
    prover.prove(
        snark_proof,
        msg,
        &avk,
        &global,
        protocol_message_preimage,
        Some(&rolling_state),
        Some(genesis_bootstrap),
    )
}
