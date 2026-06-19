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

                // For now the function will fail as we give two Some values
                // which should not happen but this might be changed
                let (ivc_proof, rolling_state, verifier_data) =
                    ivc_prover_input_preparation_and_prove(
                        snark_proof,
                        msg,
                        clerk,
                        ancillary_input,
                    )?;

                let ancillary_prover_data = AncillaryProverData::IvcSnark(
                    rolling_state.ok_or_else(|| anyhow!("missing rolling state"))?,
                );
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
    let certificate_circuit_verification_key = certificate_key_provider.get_verifying_key()?;
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
        certificate_circuit_verification_key,
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

#[cfg(test)]
mod tests {
    #[cfg(feature = "future_snark")]
    mod slow {
        use std::sync::Arc;
        use std::time::Instant;

        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;
        use sha2::{Digest, Sha256};
        use tempfile::tempdir;

        use crate::AggregateSignature::IvcSnark;
        use crate::circuits::halo2_ivc::K;
        use crate::circuits::halo2_ivc::state::Global;
        use crate::circuits::halo2_ivc::tests::common::asset_readers::load_embedded_verification_context_asset;
        use crate::circuits::halo2_ivc::tests::common::generators::build_recursive_global;
        use crate::circuits::halo2_ivc::tests::common::generators::setup::{
            QUORUM_SIZE, SIGNER_COUNT,
        };
        use crate::circuits::halo2_ivc::types::MessageHash;
        use crate::circuits::trusted_setup::{
            TrustedSetupProvider, build_provider_with_unsafe_srs,
        };
        use crate::proof_system::ivc_halo2_snark::verifier_setup::{self, IvcVerifierSetup};
        use crate::proof_system::ivc_halo2_snark::{
            IvcProverSetup, TempCertificateKeyProvider, TempIvcKeyProvider,
        };
        use crate::protocol::aggregate_signature::{
            AggregateSignatureType, AncillaryGenesisData, AncillaryProofInput,
        };
        use crate::{
            AggregateVerificationKey, AggregateVerificationKeyForSnark, AncillaryProverData,
            BaseFieldElement, SchnorrSigningKey, SchnorrVerificationKey,
        };
        use crate::{
            Initializer, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
            Signer, SingleSignature,
            circuits::halo2_ivc::tests::common::{
                asset_readers::load_embedded_first_certificate_in_epoch_asset,
                generators::{
                    build_asset_generation_setup, build_genesis_protocol_message_preimage,
                },
            },
        };

        use super::super::Clerk;

        type D = MithrilMembershipDigest;

        fn setup_parties_with_snark_keys(params: Parameters, nparties: usize) -> Vec<Signer<D>> {
            let mut rng = ChaCha20Rng::from_seed([42u8; 32]);
            let mut key_reg = KeyRegistration::initialize();
            let mut initializers = Vec::with_capacity(nparties);

            for i in 0..nparties {
                let stake = (i as u64 + 1) * 10;
                let init = Initializer::new(params, stake, &mut rng);
                let entry = RegistrationEntry::new(
                    init.get_verification_key_proof_of_possession_for_concatenation(),
                    init.stake,
                    init.schnorr_verification_key.clone(),
                )
                .unwrap();
                key_reg.register_by_entry(&entry).unwrap();
                initializers.push(init);
            }

            let closed_reg = key_reg.close_registration(&params).unwrap();
            initializers
                .into_iter()
                .map(|init| init.try_create_signer::<D>(&closed_reg).unwrap())
                .collect()
        }

        fn get_signatures(msg: &[u8], signers: &[Signer<D>]) -> Vec<SingleSignature> {
            signers
                .iter()
                .filter_map(|s| s.create_single_signature(msg).ok())
                .collect()
        }

        #[test]
        fn aggregate_signatures_with_type_ivc_snark_succeeds_on_genesis_step() {
            /*
                What I need to test:
                    The call to agg sig should work as follow: generate a snark proof of the current signatures then call the ivc prove.
                    If the current signatures are the first after genesis then the prover runs the genesis step and then runs the first step.
                    If not then it just runs the step.
                    If the epoch is the same, it bases itself on the first ivc proof of the epoch and runs the step.
                    If not it runs the step to change epoch and create the first ivc proof of the new epoch

                What is needed to run the tests
                    Genesis information
                    prover input information
                        protocol message
                        ...
            */

            let temp_dir = tempdir().expect("temp dir creation should succeed");

            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 1.0,
            };

            // Generate the Clerk
            let nparties = 5;
            let message_preimage = [
                100, 105, 103, 101, 115, 116, 16, 176, 3, 177, 117, 37, 103, 12, 53, 79, 204, 177,
                5, 29, 166, 175, 219, 238, 208, 244, 237, 50, 243, 235, 78, 116, 175, 115, 115,
                216, 50, 89, 110, 101, 120, 116, 95, 97, 103, 103, 114, 101, 103, 97, 116, 101, 95,
                118, 101, 114, 105, 102, 105, 99, 97, 116, 105, 111, 110, 95, 107, 101, 121, 171,
                31, 233, 204, 25, 208, 111, 243, 26, 55, 65, 207, 108, 15, 61, 48, 94, 190, 72,
                235, 26, 159, 69, 15, 113, 189, 8, 159, 172, 209, 98, 17, 0, 0, 0, 0, 64, 66, 15,
                0, 0, 0, 0, 0, 110, 101, 120, 116, 95, 112, 114, 111, 116, 111, 99, 111, 108, 95,
                112, 97, 114, 97, 109, 101, 116, 101, 114, 115, 7, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 99, 117, 114, 114, 101,
                110, 116, 95, 101, 112, 111, 99, 104, 6, 0, 0, 0, 0, 0, 0, 0,
            ];
            let msg: [u8; 32] = Sha256::digest(message_preimage).into();
            let ps = setup_parties_with_snark_keys(parameters, nparties);
            let clerk = Clerk::new_clerk_from_signer(&ps[0]);
            let avk: AggregateVerificationKey<MithrilMembershipDigest> =
                clerk.compute_aggregate_verification_key();

            let sigs = get_signatures(&msg, &ps);

            // First verify that the regular SNARK works
            // let (agg_sig_snark, _) = clerk
            //     .aggregate_signatures_with_type(
            //         &sigs,
            //         &msg,
            //         AggregateSignatureType::Snark,
            //         AncillaryProofInput::dummy(),
            //     )
            //     .unwrap();

            // agg_sig_snark.verify(&msg, &avk, &parameters, None).unwrap();

            // Generating Ancillary data

            // Genesis data
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let genesis_message_preimage = [
                100, 105, 103, 101, 115, 116, 188, 195, 32, 175, 25, 79, 116, 67, 229, 98, 109,
                154, 196, 202, 248, 54, 145, 200, 225, 104, 140, 210, 217, 43, 122, 186, 71, 208,
                221, 55, 131, 213, 110, 101, 120, 116, 95, 97, 103, 103, 114, 101, 103, 97, 116,
                101, 95, 118, 101, 114, 105, 102, 105, 99, 97, 116, 105, 111, 110, 95, 107, 101,
                121, 171, 31, 233, 204, 25, 208, 111, 243, 26, 55, 65, 207, 108, 15, 61, 48, 94,
                190, 72, 235, 26, 159, 69, 15, 113, 189, 8, 159, 172, 209, 98, 17, 0, 0, 0, 0, 64,
                66, 15, 0, 0, 0, 0, 0, 110, 101, 120, 116, 95, 112, 114, 111, 116, 111, 99, 111,
                108, 95, 112, 97, 114, 97, 109, 101, 116, 101, 114, 115, 7, 0, 0, 0, 0, 0, 0, 0, 0,
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 99, 117, 114,
                114, 101, 110, 116, 95, 101, 112, 111, 99, 104, 5, 0, 0, 0, 0, 0, 0, 0,
            ];
            let genesis_schnorr_signing_key = SchnorrSigningKey::generate(&mut rng);
            let genesis_message =
                BaseFieldElement::try_from(genesis_message_preimage.as_slice()).unwrap();
            let genesis_message_signature = genesis_schnorr_signing_key
                .sign_standard(&[genesis_message], &mut rng)
                .unwrap();
            let genesis_schnorr_verification_key =
                SchnorrVerificationKey::new_from_signing_key(genesis_schnorr_signing_key);

            let genesis_data = AncillaryGenesisData::new(
                genesis_message_preimage.to_vec(),
                Some(genesis_message_signature),
                Some(genesis_schnorr_verification_key),
            );

            // Prover data
            // let prover_data = AncillaryProverData::IvcSnark(())

            let ancillary_proof_input =
                AncillaryProofInput::new(None, genesis_data.clone(), message_preimage.to_vec());

            println!("Proving first step");
            let (proof, ancillary_output_data) = clerk
                .aggregate_signatures_with_type(
                    &sigs,
                    &msg,
                    AggregateSignatureType::IvcSnark,
                    ancillary_proof_input,
                )
                .unwrap();

            println!("Verifying first step");

            let ivc_proof = match proof.clone() {
                IvcSnark(ivc_proof) => ivc_proof,
                _ => todo!(),
            };

            let certificate_circuit_verification_key = ancillary_output_data
                .verifier_data()
                .unwrap()
                .as_ivc_verifier_data()
                .certificate_circuit_verification_key();

            let ivc_circuit_verification_key = ancillary_output_data
                .verifier_data()
                .unwrap()
                .as_ivc_verifier_data()
                .ivc_circuit_verification_key();

            let global = Global::new(
                MessageHash::from_field(genesis_message.0),
                genesis_schnorr_verification_key,
                &certificate_circuit_verification_key,
                &ivc_circuit_verification_key,
            );
            let verifier_setup = IvcVerifierSetup::try_new(
                &certificate_circuit_verification_key,
                ivc_circuit_verification_key,
            )
            .unwrap();

            let verify_proof = ivc_proof.verify(&global, &verifier_setup);

            println!("result of trying to verify the proof: {:?}", verify_proof);

            let verify_status = proof.verify(
                &msg,
                &avk,
                &parameters,
                ancillary_output_data.verifier_data().cloned(),
            );

            println!("verifying status for first step: {:?}", verify_status);

            // let ancillary_proof_input = AncillaryProofInput::new(
            //     ancillary_output_data.prover_data().cloned(),
            //     genesis_data,
            //     message_preimage.to_vec(),
            // );
            // println!("Proving second step");
            // let (proof, ancillary_output_data) = clerk
            //     .aggregate_signatures_with_type(
            //         &sigs,
            //         &msg,
            //         AggregateSignatureType::IvcSnark,
            //         ancillary_proof_input,
            //     )
            //     .unwrap();

            // println!("Verifying second step");
            // proof
            //     .verify(
            //         &msg,
            //         &avk,
            //         &parameters,
            //         ancillary_output_data.verifier_data().cloned(),
            //     )
            //     .unwrap()
        }
    }
}
