//! Pre-circuit inputs produced by the IVC prover's preparation step.
//!
//! Holds the witness, the advanced chain state, and the folded accumulator that
//! the in-circuit construction and proof-generation steps consume next.

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        state::{Global, State, Witness},
        types::{
            MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage, ProtocolParametersHash,
        },
    },
    proof_system::ivc_halo2_snark::{
        prover_input_helpers::{
            TransitionType, build_next_accumulator, build_next_state,
            build_snark_message_and_decode_fields, determine_transition, verify_certificate_proof,
        },
        rolling_state::IvcRollingState,
        setup::IvcSetup,
    },
};

/// Pre-circuit inputs consumed by the IVC prover's circuit-construction and proof-generation steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this input
#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct IvcProverInput {
    /// In-circuit witness for the new step.
    pub(crate) witness: Witness,
    /// Chain state advanced by one step.
    pub(crate) next_state: State,
    /// Folded accumulator the new step's IVC proof commits to.
    pub(crate) next_accumulator: Accumulator<BlstrsEmulation>,
}

// TODO: remove this allow dead_code directive when the IVC prover consumes this input
#[allow(dead_code)]
impl IvcProverInput {
    /// Advances the chain state by one step and bundles the in-circuit witness, the
    /// next state, and the next folded accumulator.
    ///
    /// First classifies the requested step via `determine_transition`, which also
    /// validates the epoch advance against the rolling chain state. At genesis (step
    /// counter zero) dispatches to `Self::prepare_genesis`: the chain's genesis
    /// signature is verified, no certificate is processed, and the rolling state's
    /// accumulator passes through. At non-genesis steps the certificate proof is
    /// verifier-prepared, then the certificate and previous IVC accumulators are
    /// folded into the chain's accumulator.
    pub(crate) fn prepare<D: MembershipDigest>(
        certificate_proof: SnarkProof<D>,
        certificate_message_bytes: &[u8],
        aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        protocol_message_preimage: &ProtocolMessagePreimage,
        rolling_state: &IvcRollingState,
        setup: &IvcSetup,
    ) -> StmResult<Self> {
        let transition_type = determine_transition(rolling_state, protocol_message_preimage)?;

        if matches!(transition_type, TransitionType::Genesis) {
            return Self::prepare_genesis(rolling_state, protocol_message_preimage, global);
        }

        let certificate_dual_msm = verify_certificate_proof(
            &certificate_proof,
            certificate_message_bytes,
            aggregate_verification_key_for_snark,
            setup,
        )?;

        let (certificate_message_hash, certificate_merkle_tree_commitment) =
            build_snark_message_and_decode_fields(
                aggregate_verification_key_for_snark,
                certificate_message_bytes,
            )?;

        let next_state = build_next_state(
            transition_type,
            rolling_state,
            certificate_message_hash,
            certificate_merkle_tree_commitment,
            protocol_message_preimage,
        )?;

        let next_accumulator =
            build_next_accumulator(certificate_dual_msm, rolling_state, setup, global)?;

        let witness = Witness::new(
            rolling_state.genesis_signature(),
            certificate_message_hash,
            certificate_merkle_tree_commitment,
            protocol_message_preimage.clone(),
        );

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator,
        })
    }

    /// Builds the genesis-step `IvcProverInput`. Verifies the chain's genesis signature,
    /// constructs the base-case state and witness with all certificate-derived fields set
    /// to ZERO and the chain message set to `global.genesis_message`, and passes the
    /// rolling state's trivial accumulator through unchanged.
    fn prepare_genesis(
        rolling_state: &IvcRollingState,
        protocol_message_preimage: &ProtocolMessagePreimage,
        global: &Global,
    ) -> StmResult<Self> {
        rolling_state.verify_genesis_signature(global)?;

        let new_step_counter = rolling_state.new_step_counter()?;
        let next_state = State::new(
            new_step_counter,
            global.genesis_message,
            MerkleTreeCommitment::ZERO,
            protocol_message_preimage.next_merkle_tree_commitment(),
            ProtocolParametersHash::ZERO,
            protocol_message_preimage.next_protocol_parameters(),
            protocol_message_preimage.current_epoch(),
        );

        let witness = Witness::new(
            rolling_state.genesis_signature(),
            MessageHash::ZERO,
            MerkleTreeCommitment::ZERO,
            protocol_message_preimage.clone(),
        );

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator: rolling_state.accumulator().clone(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod slow {
        use std::sync::Arc;

        use midnight_proofs::utils::SerdeFormat;
        use tempfile::tempdir;

        use crate::{
            MithrilMembershipDigest, Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    errors::IvcCircuitError,
                    io::Write as IvcWrite,
                    tests::common::{
                        asset_readers::{
                            VerificationContextAsset,
                            load_embedded_first_certificate_in_epoch_asset,
                            load_embedded_following_certificate_in_epoch_asset,
                            load_embedded_next_epoch_step_output_asset,
                            load_embedded_recursive_chain_state_asset,
                            load_embedded_verification_context_asset,
                        },
                        generators::{
                            build_asset_generation_setup, build_genesis_protocol_message_preimage,
                            build_recursive_global,
                            setup::{AssetGenerationSetup, QUORUM_SIZE, SIGNER_COUNT, TOTAL_STAKE},
                        },
                    },
                },
                trusted_setup::build_provider_with_unsafe_srs,
            },
            proof_system::{
                halo2_snark::CircuitVerificationKey,
                ivc_halo2_snark::unsafe_setup_helpers::{
                    TempCertificateKeyProvider, TempIvcKeyProvider,
                },
            },
            signature_scheme::StandardSchnorrSignature,
        };

        use super::*;

        fn build_ivc_setup() -> IvcSetup {
            let temp_dir = tempdir().expect("temp dir creation should succeed");
            let trusted_setup_provider = build_provider_with_unsafe_srs(temp_dir.path(), K);
            let srs = Arc::new(
                trusted_setup_provider
                    .get_trusted_setup_parameters()
                    .expect("unsafe SRS should load"),
            );
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            let cert_provider =
                TempCertificateKeyProvider::new(Arc::clone(&srs), parameters, merkle_tree_depth);
            let cert_vk = cert_provider
                .get_verifying_key()
                .expect("certificate verifying key keygen should succeed");
            let ivc_provider = TempIvcKeyProvider::new(srs, cert_vk);
            IvcSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                .expect("IvcSetup::load should succeed under the unsafe SRS")
        }

        fn build_global(
            asset_setup: &AssetGenerationSetup,
            verification_context: &VerificationContextAsset,
        ) -> Global {
            build_recursive_global(
                asset_setup,
                &verification_context.certificate_verifying_key,
                &verification_context.recursive_verifying_key,
            )
        }

        fn wrap_snark_proof(
            verification_context: &VerificationContextAsset,
            certificate_proof_bytes: Vec<u8>,
        ) -> SnarkProof<MithrilMembershipDigest> {
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            let circuit_verification_key =
                CircuitVerificationKey::new(verification_context.certificate_verifying_key.clone());
            SnarkProof::from_parts(
                certificate_proof_bytes,
                parameters,
                merkle_tree_depth,
                circuit_verification_key,
            )
        }

        fn wrap_avk(
            aggregate_verification_key_merkle_root: &[u8; 32],
        ) -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            let mut avk_bytes = [0u8; 40];
            avk_bytes[0..32].copy_from_slice(aggregate_verification_key_merkle_root);
            avk_bytes[32..40].copy_from_slice(&TOTAL_STAKE.to_be_bytes());
            AggregateVerificationKeyForSnark::<MithrilMembershipDigest>::from_bytes(&avk_bytes)
                .expect("AVK should decode from asset bytes")
        }

        fn wrap_protocol_message_preimage(preimage: &[u8]) -> ProtocolMessagePreimage {
            use crate::circuits::halo2_ivc::PREIMAGE_SIZE;
            let preimage_array: [u8; PREIMAGE_SIZE] = preimage
                .try_into()
                .expect("preimage should be exactly PREIMAGE_SIZE bytes");
            ProtocolMessagePreimage::new(preimage_array)
        }

        fn accumulator_bytes(accumulator: &Accumulator<BlstrsEmulation>) -> Vec<u8> {
            let mut bytes = Vec::new();
            accumulator
                .write(&mut bytes, SerdeFormat::RawBytesUnchecked)
                .expect("accumulator serialization should succeed");
            bytes
        }

        fn assert_prepare_happy_path_output(
            input: &IvcProverInput,
            step_next_state: &State,
            step_next_accumulator: &Accumulator<BlstrsEmulation>,
            chain_genesis_signature: StandardSchnorrSignature,
            protocol_message_preimage: &ProtocolMessagePreimage,
        ) {
            assert_eq!(input.next_state, *step_next_state);
            assert_eq!(
                accumulator_bytes(&input.next_accumulator),
                accumulator_bytes(step_next_accumulator),
            );
            assert_eq!(input.witness.genesis_signature, chain_genesis_signature);
            assert_eq!(&input.witness.message_preimage, protocol_message_preimage);
            assert_eq!(
                input.witness.certificate_merkle_tree_commitment,
                step_next_state.merkle_tree_commitment,
            );
            assert_eq!(input.witness.certificate_message, step_next_state.message);
        }

        #[test]
        #[ignore = "slow: runs real keygen; opt-in only"]
        fn prepare_integration_scenarios() {
            let setup = build_ivc_setup();
            let asset_setup = build_asset_generation_setup();
            let verification_context = load_embedded_verification_context_asset()
                .expect("verification context asset should load");
            let global = build_global(&asset_setup, &verification_context);
            {
                let first_step = load_embedded_first_certificate_in_epoch_asset()
                    .expect("first step cert asset should load");
                let combined_names: Vec<String> =
                    setup.combined_fixed_bases.keys().cloned().collect();
                let rolling_state =
                    IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    first_step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&first_step.aggregate_verification_key_merkle_root);
                let genesis_preimage_bytes = build_genesis_protocol_message_preimage(&asset_setup);
                let protocol_message_preimage =
                    wrap_protocol_message_preimage(&genesis_preimage_bytes);

                IvcProverInput::prepare(
                    certificate_proof,
                    &first_step.message,
                    &avk,
                    &global,
                    &protocol_message_preimage,
                    &rolling_state,
                    &setup,
                )
                .expect("prepare should succeed at genesis (dispatching to prepare_genesis)");
            }

            {
                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let protocol_message_preimage =
                    wrap_protocol_message_preimage(&step.message_preimage);
                let chain_genesis_signature = chain_state.genesis_signature;
                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let input = IvcProverInput::prepare(
                    certificate_proof,
                    &step.message,
                    &avk,
                    &global,
                    &protocol_message_preimage,
                    &rolling_state,
                    &setup,
                )
                .expect("scenario 2: prepare should succeed at same-epoch step");

                assert_prepare_happy_path_output(
                    &input,
                    &step.next_state,
                    &step.next_accumulator,
                    chain_genesis_signature,
                    &protocol_message_preimage,
                );
            }

            {
                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_next_epoch_step_output_asset()
                    .expect("recursive step output asset should load");

                let certificate_proof = wrap_snark_proof(
                    &verification_context,
                    step.certificate_proof.clone().into_vec(),
                );
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let protocol_message_preimage =
                    wrap_protocol_message_preimage(&step.message_preimage);
                let chain_genesis_signature = chain_state.genesis_signature;
                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let input = IvcProverInput::prepare(
                    certificate_proof,
                    &step.message,
                    &avk,
                    &global,
                    &protocol_message_preimage,
                    &rolling_state,
                    &setup,
                )
                .expect("prepare should succeed at next-epoch step");

                assert_prepare_happy_path_output(
                    &input,
                    &step.next_state,
                    &step.next_accumulator,
                    chain_genesis_signature,
                    &protocol_message_preimage,
                );
            }

            {
                let chain_state = load_embedded_recursive_chain_state_asset()
                    .expect("recursive chain state asset should load");
                let step = load_embedded_following_certificate_in_epoch_asset()
                    .expect("same-epoch step output asset should load");

                let mut corrupted = step.certificate_proof.clone().into_vec();
                corrupted[0] ^= 0xFF;

                let certificate_proof = wrap_snark_proof(&verification_context, corrupted);
                let avk = wrap_avk(&step.aggregate_verification_key_merkle_root);
                let protocol_message_preimage =
                    wrap_protocol_message_preimage(&step.message_preimage);
                let rolling_state = IvcRollingState::new(
                    chain_state.state,
                    chain_state.ivc_proof,
                    chain_state.accumulator,
                    chain_state.genesis_signature,
                );

                let result = IvcProverInput::prepare(
                    certificate_proof,
                    &step.message,
                    &avk,
                    &global,
                    &protocol_message_preimage,
                    &rolling_state,
                    &setup,
                );

                let err = result
                    .expect_err("prepare should reject a corrupted certificate proof")
                    .downcast::<IvcCircuitError>()
                    .expect("error should downcast to IvcCircuitError");
                assert_eq!(err, IvcCircuitError::CertificateProofRejected);
            }
        }
    }

    mod fast {
        use midnight_proofs::utils::SerdeFormat;
        use rand_chacha::ChaCha20Rng;
        use rand_core::SeedableRng;

        use super::*;
        use crate::{
            circuits::halo2_ivc::{
                PREIMAGE_CURRENT_EPOCH_BYTES, PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES,
                PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES, PREIMAGE_SIZE,
                io::Write as IvcWrite,
                types::{
                    CertificateCircuitVerificationKeyRepresentation, EpochNumber,
                    IvcCircuitVerificationKeyRepresentation, StepCounter,
                },
            },
            signature_scheme::{
                BaseFieldElement, SchnorrSignatureError, SchnorrSigningKey, SchnorrVerificationKey,
            },
        };

        #[test]
        fn prepare_genesis_rejects_invalid_signature() {
            let mut chain_rng = ChaCha20Rng::from_seed([0u8; 32]);
            let chain_signing_key = SchnorrSigningKey::generate(&mut chain_rng);
            let chain_verification_key =
                SchnorrVerificationKey::new_from_signing_key(chain_signing_key);

            let global = Global {
                genesis_message: MessageHash::ZERO,
                genesis_verification_key: chain_verification_key,
                certificate_circuit_verification_key_representation:
                    CertificateCircuitVerificationKeyRepresentation::from_field(
                        BaseFieldElement::from(0u64).0,
                    ),
                ivc_circuit_verification_key_representation:
                    IvcCircuitVerificationKeyRepresentation::from_field(
                        BaseFieldElement::from(0u64).0,
                    ),
            };

            let mut wrong_rng = ChaCha20Rng::from_seed([1u8; 32]);
            let wrong_signing_key = SchnorrSigningKey::generate(&mut wrong_rng);
            let invalid_signature = wrong_signing_key
                .sign_standard(
                    &[BaseFieldElement::from(global.genesis_message.as_field())],
                    &mut wrong_rng,
                )
                .expect("sign_standard should succeed for a synthetic message");

            let rolling_state = IvcRollingState::genesis(invalid_signature, &[]);
            let protocol_message_preimage = ProtocolMessagePreimage::new([0u8; PREIMAGE_SIZE]);

            let err = IvcProverInput::prepare_genesis(
                &rolling_state,
                &protocol_message_preimage,
                &global,
            )
            .expect_err("prepare_genesis should reject a signature that does not verify");
            let schnorr_error = err
                .downcast::<SchnorrSignatureError>()
                .expect("error chain should carry SchnorrSignatureError");
            assert!(matches!(
                schnorr_error,
                SchnorrSignatureError::StandardSignatureInvalid(_)
            ));
        }

        #[test]
        fn prepare_genesis_produces_expected_state_and_witness() {
            let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
            let signing_key = SchnorrSigningKey::generate(&mut rng);
            let verification_key =
                SchnorrVerificationKey::new_from_signing_key(signing_key.clone());

            let genesis_message = MessageHash::from_field(
                BaseFieldElement::from_raw(&[0x42; 32])
                    .expect("from_raw applies modulus reduction")
                    .0,
            );
            let global = Global {
                genesis_message,
                genesis_verification_key: verification_key,
                certificate_circuit_verification_key_representation:
                    CertificateCircuitVerificationKeyRepresentation::from_field(
                        BaseFieldElement::from(0u64).0,
                    ),
                ivc_circuit_verification_key_representation:
                    IvcCircuitVerificationKeyRepresentation::from_field(
                        BaseFieldElement::from(0u64).0,
                    ),
            };

            let genesis_signature = signing_key
                .sign_standard(
                    &[BaseFieldElement::from(global.genesis_message.as_field())],
                    &mut rng,
                )
                .expect("sign_standard should succeed for the genesis message");
            let rolling_state = IvcRollingState::genesis(genesis_signature, &[]);

            let cert_epoch = EpochNumber::ZERO;
            let mut preimage_bytes = [0u8; PREIMAGE_SIZE];
            preimage_bytes[PREIMAGE_CURRENT_EPOCH_BYTES]
                .copy_from_slice(&cert_epoch.as_u64().to_le_bytes());
            preimage_bytes[PREIMAGE_NEXT_MERKLE_TREE_COMMITMENT_BYTES].copy_from_slice(&[0x11; 32]);
            preimage_bytes[PREIMAGE_NEXT_PROTOCOL_PARAMETERS_BYTES].copy_from_slice(&[0x22; 32]);
            let protocol_message_preimage = ProtocolMessagePreimage::new(preimage_bytes);

            let input = IvcProverInput::prepare_genesis(
                &rolling_state,
                &protocol_message_preimage,
                &global,
            )
            .expect("prepare_genesis should succeed for a valid genesis signature");

            let expected_next_state = State::new(
                StepCounter::new(1),
                global.genesis_message,
                MerkleTreeCommitment::ZERO,
                protocol_message_preimage.next_merkle_tree_commitment(),
                ProtocolParametersHash::ZERO,
                protocol_message_preimage.next_protocol_parameters(),
                protocol_message_preimage.current_epoch(),
            );
            assert_eq!(input.next_state, expected_next_state);

            let expected_witness = Witness::new(
                genesis_signature,
                MessageHash::ZERO,
                MerkleTreeCommitment::ZERO,
                protocol_message_preimage.clone(),
            );
            assert_eq!(input.witness, expected_witness);

            let mut next_acc_bytes = Vec::new();
            input
                .next_accumulator
                .write(&mut next_acc_bytes, SerdeFormat::RawBytesUnchecked)
                .expect("accumulator serialization should succeed");
            let mut rolling_acc_bytes = Vec::new();
            rolling_state
                .accumulator()
                .write(&mut rolling_acc_bytes, SerdeFormat::RawBytesUnchecked)
                .expect("accumulator serialization should succeed");
            assert_eq!(next_acc_bytes, rolling_acc_bytes);
        }
    }
}
