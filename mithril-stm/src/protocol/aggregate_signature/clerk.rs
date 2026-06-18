use anyhow::Context;
use rand_core::OsRng;
use std::{marker::PhantomData, sync::Arc};

#[cfg(feature = "future_snark")]
use anyhow::anyhow;

use crate::{
    AggregateVerificationKey, BaseFieldElement, ClosedKeyRegistration, LotteryIndex,
    MembershipDigest, Parameters, Signer, SingleSignature, SnarkProof, Stake, StmResult,
    VerificationKeyForConcatenation,
    circuits::halo2_ivc::{ProtocolMessagePreimage, types::MessageHash},
    proof_system::{
        ConcatenationClerk, ConcatenationProof, IvcRollingState, ivc_halo2_snark::proof::IvcProof,
    },
};

use crate::{
    AncillaryProverData, AncillaryVerifierData, MithrilMembershipDigest,
    circuits::{halo2_ivc::state::Global, trusted_setup::TrustedSetupProvider},
    proof_system::ivc_halo2_snark::{
        IvcProverSetup, TempCertificateKeyProvider, TempIvcKeyProvider,
        verifier_setup::IvcVerifierData,
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

                println!("Clerk ready to prove the first certificate");

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

                println!("First certificate proof generated, starting the IVC proof");

                // A same-epoch step does not advance the rolling state, so the prover returns
                // none: carry the input rolling state forward so the next certificate keeps
                // building on it.
                let previous_prover_data = ancillary_input.prover_data().cloned();

                let (ivc_proof, next_rolling_state, verifier_data) =
                    ivc_prover_input_preparation_and_prove(
                        snark_proof,
                        msg,
                        clerk,
                        ancillary_input,
                    )?;

                let ancillary_prover_data =
                    resolve_ivc_ancillary_prover_data(next_rolling_state, previous_prover_data)?;
                let ancillary_verifier_data = AncillaryVerifierData::IvcSnark(verifier_data);

                Ok((
                    AggregateSignature::IvcSnark(Box::new(ivc_proof)),
                    AncillaryProofOutput::new(
                        Some(ancillary_prover_data),
                        Some(ancillary_verifier_data),
                    ),
                ))
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
) -> StmResult<(
    IvcProof<blake2b_simd::State>,
    Option<IvcRollingState>,
    IvcVerifierData,
)> {
    let protocol_message_preimage_bytes: [u8; 190] =
        ancillary_input.message_preimage().try_into()?;

    println!("Getting the genesis_verification_key");

    let genesis_verification_key = ancillary_input
        .genesis_data()
        .genesis_schnorr_verification_key()
        .cloned()
        .ok_or_else(|| anyhow!("Missing genesis verification key from ancillary data"))?;

    println!("Getting the genesis_preimage");

    let genesis_preimage = ancillary_input.genesis_data().genesis_message_preimage();

    println!("Converting the genesis_preimage to field element");
    // TODO: clean this to have proper call to SHA256
    // the try_from function applies the Sha256 hash
    let genesis_message_field_elem = BaseFieldElement::try_from(genesis_preimage)?.0;

    println!("Starting the trusted_setup generation via the provider");

    let trusted_setup_provider = TrustedSetupProvider::default();
    println!("Getting the srs");

    let srs = Arc::new(trusted_setup_provider.get_trusted_setup_parameters()?);
    println!("Creating the certificate_key_provider from srs");

    let certificate_key_provider =
        TempCertificateKeyProvider::new(srs.clone(), clerk.parameters, MERKLE_TREE_DEPTH_FOR_SNARK);
    println!("Creating the ivc_key_provider from srs");

    let ivc_key_provider =
        TempIvcKeyProvider::new(srs.clone(), certificate_key_provider.get_verifying_key()?);

    println!("Creating the ivc_setup from providers");

    let ivc_setup = IvcProverSetup::load(
        &trusted_setup_provider,
        &certificate_key_provider,
        &ivc_key_provider,
    )?;

    println!("Creating the prover from ivc_setup");

    let mut prover = IvcProver {
        ivc_setup: ivc_setup.into(),
        rng: OsRng,
    };

    println!("Computing the avk");

    // This is only used to get the root of the tree so maybe we can replace
    // the avk inputs by the root directly
    let avk = clerk.compute_aggregate_verification_key_for_snark();

    let genesis_message = MessageHash::from_field(genesis_message_field_elem);
    let certificate_midnight_verifying_key =
        certificate_key_provider.get_midnight_verifying_key()?;
    let certificate_circuit_verification_key = certificate_midnight_verifying_key.vk().clone();
    let ivc_circuit_verification_key = ivc_key_provider.get_verifying_key()?;
    println!("Creating the global value");

    let global = Global::new(
        genesis_message,
        genesis_verification_key,
        &certificate_circuit_verification_key,
        &ivc_circuit_verification_key,
    );

    let verifier_data = IvcVerifierData::new(
        genesis_message,
        genesis_verification_key,
        certificate_midnight_verifying_key,
        ivc_circuit_verification_key,
    );

    println!("Extracting the rolling state if present");

    let rolling_state = match ancillary_input.prover_data() {
        Some(input) => Some(input.as_ivc_rolling_state()),
        None => None,
    };

    println!("Converting the genesis bootstrap data");

    // IvcGenesisBootstrapInput contains the same values as AncillaryGenesisData
    // We might consider dropping one or implementing a conversion between the two
    let genesis_bootstrap = &ancillary_input.genesis_data().try_into()?;

    println!("Starting the IVC proof");

    // For now the function will fail as we give two Some values
    // which should not happen but this might be changed
    let (ivc_proof, next_rolling_state) = prover.prove(
        snark_proof,
        msg,
        &avk,
        &global,
        &ProtocolMessagePreimage(protocol_message_preimage_bytes),
        genesis_bootstrap,
        rolling_state.as_ref(),
    )?;

    Ok((ivc_proof, next_rolling_state, verifier_data))
}

/// Resolve the prover data to store on a new IVC certificate.
///
/// A next-epoch step advances the rolling state, which is stored as is. A same-epoch step does not
/// advance it (the prover returns none), so the input rolling state carried from the parent
/// certificate is reused for the next certificate.
fn resolve_ivc_ancillary_prover_data(
    next_rolling_state: Option<IvcRollingState>,
    previous_prover_data: Option<AncillaryProverData>,
) -> StmResult<AncillaryProverData> {
    match next_rolling_state {
        Some(rolling_state) => Ok(AncillaryProverData::IvcSnark(rolling_state)),
        None => previous_prover_data.ok_or_else(|| anyhow!("missing rolling state")),
    }
}

#[cfg(all(test, feature = "future_snark"))]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::AncillaryProverData;
    use crate::proof_system::IvcRollingState;
    use crate::signature_scheme::{BaseFieldElement, SchnorrSigningKey};

    use super::resolve_ivc_ancillary_prover_data;

    fn build_rolling_state() -> IvcRollingState {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let signing_key = SchnorrSigningKey::generate(&mut rng);
        let message = vec![BaseFieldElement::from(1u64)];
        let genesis_signature = signing_key.sign_standard(&message, &mut rng).unwrap();
        IvcRollingState::genesis(genesis_signature, &[])
    }

    #[test]
    fn next_epoch_step_stores_the_advanced_rolling_state() {
        let prover_data =
            resolve_ivc_ancillary_prover_data(Some(build_rolling_state()), None).unwrap();

        assert!(matches!(prover_data, AncillaryProverData::IvcSnark(_)));
    }

    #[test]
    fn same_epoch_step_carries_the_previous_rolling_state_forward() {
        let previous = AncillaryProverData::IvcSnark(build_rolling_state());

        let prover_data = resolve_ivc_ancillary_prover_data(None, Some(previous)).unwrap();

        assert!(matches!(prover_data, AncillaryProverData::IvcSnark(_)));
    }

    #[test]
    fn fails_when_neither_an_advanced_nor_a_previous_rolling_state_is_available() {
        let error = resolve_ivc_ancillary_prover_data(None, None).unwrap_err();

        assert!(error.to_string().contains("missing rolling state"));
    }
}
