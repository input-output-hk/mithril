//! Pre-circuit inputs produced by the IVC prover's preparation step.
//!
//! Holds the witness, the advanced chain state, and the folded accumulator that
//! the in-circuit construction and proof-generation steps consume next.

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::{
    AggregateVerificationKeyForSnark, MembershipDigest, SnarkProof, StmResult,
    circuits::halo2_ivc::{
        errors::IvcCircuitError,
        state::{Global, State, Witness},
        types::{
            EpochNumber, MerkleTreeCommitment, MessageHash, ProtocolMessagePreimage,
            ProtocolParametersHash, StepCounter,
        },
    },
    proof_system::{
        halo2_snark::build_snark_message,
        ivc_halo2_snark::{
            decoded_protocol_message::DecodedProtocolMessage, rolling_state::IvcRollingState,
            setup::IvcSetup,
        },
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
    pub(crate) fn prepare<D: MembershipDigest>(
        snark_proof: SnarkProof<D>,
        message: &[u8],
        aggregate_verification_key_for_snark: &AggregateVerificationKeyForSnark<D>,
        global: &Global,
        decoded_protocol_message: &DecodedProtocolMessage,
        rolling_state: &IvcRollingState,
        setup: &IvcSetup,
    ) -> StmResult<Self> {
        // Verify the SNARK proof and prepare the dual MSM for the certificate accumulator.
        let dual_msm = snark_proof.prepare_and_check(
            message,
            aggregate_verification_key_for_snark,
            &setup.srs_verifier_params,
        )?;

        let chain_epoch = rolling_state.state().current_epoch.as_field();
        let certificate_epoch = decoded_protocol_message.current_epoch().0;

        let is_same_epoch = certificate_epoch == chain_epoch;
        let is_next_epoch = certificate_epoch == chain_epoch + EpochNumber::new(1).as_field();
        let is_genesis = rolling_state.state().step_counter == StepCounter::ZERO;

        // Verify the genesis signature if at genesis.
        if is_genesis {
            rolling_state.verify_genesis_signature(global)?;
        }

        // Non-genesis steps must advance by zero or one epoch. Genesis bypasses this check.
        if !is_genesis && !is_same_epoch && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpochTransition {
                certificate_epoch: EpochNumber::from_field(certificate_epoch).as_u64(),
                chain_epoch: rolling_state.state().current_epoch.as_u64(),
            }
            .into());
        }

        // Within an epoch, every cert's preimage must announce the same next-epoch
        // lookahead as the chain's previous state. The cert circuit doesn't enforce this
        // across certs (it's a chain-level invariant), so we check it here.
        if is_same_epoch && !is_genesis {
            let chain_next_merkle_tree_commitment =
                rolling_state.state().next_merkle_tree_commitment.as_field();
            let chain_next_protocol_parameters =
                rolling_state.state().next_protocol_parameters.as_field();
            if decoded_protocol_message.next_merkle_tree_commitment().0
                != chain_next_merkle_tree_commitment
                || decoded_protocol_message.next_protocol_parameters().0
                    != chain_next_protocol_parameters
            {
                return Err(IvcCircuitError::SameEpochLookaheadMismatch {
                    chain_epoch: rolling_state.state().current_epoch.as_u64(),
                }
                .into());
            }
        }

        // Build the SNARK message and extract the certificate message and Merkle tree commitment.
        let snark_message = build_snark_message(
            &aggregate_verification_key_for_snark.get_merkle_tree_commitment().root,
            message,
        )?;
        let certificate_message = MessageHash::from_field(snark_message[1].0);
        let certificate_merkle_tree_commitment =
            MerkleTreeCommitment::from_field(snark_message[0].0);

        // Compute the new chain state the IVC proof will commit to, based on the epoch transition type.
        let (new_message, new_merkle_tree_commitment, new_protocol_parameters) = if is_genesis {
            (
                global.genesis_message,
                MerkleTreeCommitment::ZERO,
                ProtocolParametersHash::ZERO,
            )
        } else {
            let new_protocol_parameters = if is_same_epoch {
                rolling_state.state().protocol_parameters
            } else {
                rolling_state.state().next_protocol_parameters
            };
            (
                certificate_message,
                certificate_merkle_tree_commitment,
                new_protocol_parameters,
            )
        };
        let new_step_counter = rolling_state.new_step_counter()?;
        let next_state = State::new(
            new_step_counter,
            new_message,
            new_merkle_tree_commitment,
            MerkleTreeCommitment::from_field(
                decoded_protocol_message.next_merkle_tree_commitment().0,
            ),
            new_protocol_parameters,
            ProtocolParametersHash::from_field(
                decoded_protocol_message.next_protocol_parameters().0,
            ),
            EpochNumber::from_field(certificate_epoch),
        );

        // Compute the new folded accumulator the IVC proof will commit to, by accumulating the
        // previous folded accumulator with the certificate proof and the previous IVC proof
        // (unless at genesis, where we skip the previous IVC proof and use a trivial accumulator instead).
        let certificate_accumulator = setup.certificate_accumulator(dual_msm);
        let previous_ivc_proof_accumulator = if is_genesis {
            setup.trivial_previous_ivc_proof_accumulator()
        } else {
            setup.previous_ivc_proof_accumulator(
                rolling_state.ivc_proof().as_bytes(),
                &rolling_state.previous_ivc_proof_public_inputs(global),
            )?
        };
        let mut next_accumulator = Accumulator::accumulate(&[
            rolling_state.accumulator().clone(),
            certificate_accumulator,
            previous_ivc_proof_accumulator,
        ]);
        next_accumulator.collapse();

        // Construct the in-circuit witness for the new step.
        let witness = Witness::new(
            rolling_state.genesis_signature(),
            certificate_message,
            certificate_merkle_tree_commitment,
            ProtocolMessagePreimage::from(*decoded_protocol_message.message_preimage()),
        );

        Ok(IvcProverInput {
            witness,
            next_state,
            next_accumulator,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Asset-driven tests for `IvcProverInput::prepare`. Slow tier because the shared
    // `IvcSetup` runs real `keygen` once per test binary (real cache providers are not
    // yet wired in).
    mod slow {
        use std::sync::{Arc, OnceLock};

        use tempfile::tempdir;

        use crate::{
            MithrilMembershipDigest, Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    errors::IvcCircuitError,
                    tests::common::{
                        asset_readers::{
                            VerificationContextAsset, load_embedded_first_step_cert_asset,
                            load_embedded_recursive_chain_state_asset,
                            load_embedded_recursive_step_output_asset,
                            load_embedded_same_epoch_step_output_asset,
                            load_embedded_verification_context_asset,
                        },
                        generators::{
                            build_asset_generation_setup, build_recursive_global,
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

        /// `IvcSetup` is expensive to build (one keygen pass). Share it across all tests in
        /// this binary via a `OnceLock`. The deterministic `unsafe_setup_helpers` pipeline
        /// uses the same parameters as the asset generator, so the cert and IVC verifying
        /// keys derived here match those committed in the asset files.
        fn shared_ivc_setup() -> &'static IvcSetup {
            static CELL: OnceLock<IvcSetup> = OnceLock::new();
            CELL.get_or_init(|| {
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
                let cert_provider = TempCertificateKeyProvider::new(
                    Arc::clone(&srs),
                    parameters,
                    merkle_tree_depth,
                );
                let cert_vk = cert_provider
                    .get_verifying_key()
                    .expect("certificate verifying key keygen should succeed");
                let ivc_provider = TempIvcKeyProvider::new(srs, cert_vk);
                IvcSetup::load(&trusted_setup_provider, &cert_provider, &ivc_provider)
                    .expect("IvcSetup::load should succeed under the unsafe SRS")
            })
        }

        /// Deterministic asset-side setup (signing keys, merkle tree, genesis sig). No
        /// keygen; share lazily as a courtesy.
        fn shared_asset_setup() -> &'static AssetGenerationSetup {
            static CELL: OnceLock<AssetGenerationSetup> = OnceLock::new();
            CELL.get_or_init(build_asset_generation_setup)
        }

        /// Shared verification-context asset (cert VK as `MidnightVK`, IVC VK, verifier
        /// params, combined fixed bases). Loaded once.
        fn shared_verification_context() -> &'static VerificationContextAsset {
            static CELL: OnceLock<VerificationContextAsset> = OnceLock::new();
            CELL.get_or_init(|| {
                load_embedded_verification_context_asset()
                    .expect("verification context asset should load")
            })
        }

        /// Builds the typed `Global` that matches the AVK and VKs the asset proofs were
        /// generated against.
        fn build_global() -> Global {
            let asset_setup = shared_asset_setup();
            let ctx = shared_verification_context();
            build_recursive_global(
                asset_setup,
                &ctx.certificate_verifying_key,
                &ctx.recursive_verifying_key,
            )
        }

        /// Wraps stored certificate proof bytes into a typed `SnarkProof` using the
        /// certificate VK from the verification-context asset. Skips keygen.
        fn wrap_snark_proof(
            certificate_proof_bytes: Vec<u8>,
        ) -> SnarkProof<MithrilMembershipDigest> {
            let ctx = shared_verification_context();
            let parameters = Parameters {
                k: QUORUM_SIZE as u64,
                m: (QUORUM_SIZE * 10) as u64,
                phi_f: 0.2,
            };
            let merkle_tree_depth = SIGNER_COUNT.next_power_of_two().trailing_zeros();
            let circuit_verification_key =
                CircuitVerificationKey::new(ctx.certificate_verifying_key.clone());
            SnarkProof::from_parts(
                certificate_proof_bytes,
                parameters,
                merkle_tree_depth,
                circuit_verification_key,
            )
        }

        /// Wraps a stored AVK merkle root into a typed
        /// `AggregateVerificationKeyForSnark`. Uses `TOTAL_STAKE` so the encoded AVK
        /// matches the one committed to by the cert proofs.
        fn wrap_avk(
            avk_merkle_root: &[u8; 32],
        ) -> AggregateVerificationKeyForSnark<MithrilMembershipDigest> {
            let mut avk_bytes = [0u8; 40];
            avk_bytes[0..32].copy_from_slice(avk_merkle_root);
            avk_bytes[32..40].copy_from_slice(&TOTAL_STAKE.to_be_bytes());
            AggregateVerificationKeyForSnark::<MithrilMembershipDigest>::from_bytes(&avk_bytes)
                .expect("AVK should decode from asset bytes")
        }

        /// Builds an `DecodedProtocolMessage` from a stored protocol-message preimage.
        fn wrap_decoded_protocol_message(preimage: &[u8]) -> DecodedProtocolMessage {
            use crate::circuits::halo2_ivc::PREIMAGE_SIZE;
            let preimage_array: [u8; PREIMAGE_SIZE] = preimage
                .try_into()
                .expect("preimage should be exactly PREIMAGE_SIZE bytes");
            DecodedProtocolMessage::new(preimage_array)
                .expect("epoch data should decode from preimage")
        }

        /// Builds the `IvcRollingState` carrying the previous-step pieces a non-genesis
        /// `prepare` call consumes.
        fn build_rolling_state(
            state: State,
            ivc_proof: crate::circuits::halo2_ivc::types::IvcProofBytes,
            accumulator: midnight_circuits::verifier::Accumulator<BlstrsEmulation>,
            genesis_signature: StandardSchnorrSignature,
        ) -> IvcRollingState {
            IvcRollingState::new(state, ivc_proof, accumulator, genesis_signature)
        }

        // ----- Scenario 1: genesis step -----

        #[test]
        fn prepare_at_genesis_produces_advanced_state_and_witness() {
            let setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let first_step =
                load_embedded_first_step_cert_asset().expect("first step cert asset should load");

            let global = build_global();
            let combined_names: Vec<String> = setup.combined_fixed_bases.keys().cloned().collect();
            let rolling_state =
                IvcRollingState::genesis(asset_setup.genesis_signature, &combined_names);

            let snark_proof = wrap_snark_proof(first_step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&first_step.avk_merkle_root);
            let decoded_protocol_message =
                wrap_decoded_protocol_message(&first_step.message_preimage);

            let input = IvcProverInput::prepare(
                snark_proof,
                &first_step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at genesis");

            // Step counter advances by one.
            assert_eq!(input.next_state.step_counter, StepCounter::new(1));
            // At genesis, the new chain state's value fields are forced to genesis values.
            assert_eq!(input.next_state.message, global.genesis_message);
            assert_eq!(
                input.next_state.merkle_tree_commitment,
                MerkleTreeCommitment::ZERO
            );
            assert_eq!(
                input.next_state.protocol_parameters,
                ProtocolParametersHash::ZERO
            );
            // Lookahead fields come from decoded_protocol_message.
            assert_eq!(
                input.next_state.next_merkle_tree_commitment.as_field(),
                decoded_protocol_message.next_merkle_tree_commitment().0
            );
            assert_eq!(
                input.next_state.next_protocol_parameters.as_field(),
                decoded_protocol_message.next_protocol_parameters().0
            );
            assert_eq!(
                input.next_state.current_epoch.as_field(),
                decoded_protocol_message.current_epoch().0
            );
            // Witness carries the chain's genesis signature.
            assert_eq!(
                input.witness.genesis_signature,
                asset_setup.genesis_signature
            );
        }

        // ----- Scenario 2: same-epoch step -----

        #[test]
        fn prepare_at_same_epoch_advances_state_correctly() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_same_epoch_step_output_asset()
                .expect("same-epoch step output asset should load");

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.avk_merkle_root);
            let decoded_protocol_message = wrap_decoded_protocol_message(&step.message_preimage);

            let previous_step_counter = chain_state.state.step_counter.as_u64();
            let previous_protocol_parameters = chain_state.state.protocol_parameters;
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let input = IvcProverInput::prepare(
                snark_proof,
                &step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at same-epoch step");

            assert_eq!(
                input.next_state.step_counter,
                StepCounter::new(previous_step_counter + 1)
            );
            // Same-epoch: protocol parameters carry forward from the previous current.
            assert_eq!(
                input.next_state.protocol_parameters,
                previous_protocol_parameters
            );
            assert_eq!(
                input.next_state.current_epoch.as_field(),
                decoded_protocol_message.current_epoch().0
            );
        }

        // ----- Scenario 3: next-epoch step -----

        #[test]
        fn prepare_at_next_epoch_carries_lookahead_protocol_parameters() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_recursive_step_output_asset()
                .expect("recursive step output asset should load");

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.avk_merkle_root);
            let decoded_protocol_message = wrap_decoded_protocol_message(&step.message_preimage);

            let previous_step_counter = chain_state.state.step_counter.as_u64();
            let previous_next_protocol_parameters = chain_state.state.next_protocol_parameters;
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let input = IvcProverInput::prepare(
                snark_proof,
                &step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            )
            .expect("prepare should succeed at next-epoch step");

            assert_eq!(
                input.next_state.step_counter,
                StepCounter::new(previous_step_counter + 1)
            );
            // Next-epoch: the previous-state lookahead is promoted to current.
            assert_eq!(
                input.next_state.protocol_parameters,
                previous_next_protocol_parameters
            );
            assert_eq!(
                input.next_state.current_epoch.as_field(),
                decoded_protocol_message.current_epoch().0
            );
        }

        // ----- Scenario 4: invalid SNARK proof -----

        #[test]
        fn prepare_rejects_invalid_snark_proof() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_same_epoch_step_output_asset()
                .expect("same-epoch step output asset should load");

            let global = build_global();
            // Flip one byte in the certificate proof so the verifier rejects it.
            let mut corrupted = step.certificate_proof.clone().into_vec();
            corrupted[0] ^= 0xFF;

            let snark_proof = wrap_snark_proof(corrupted);
            let avk = wrap_avk(&step.avk_merkle_root);
            let decoded_protocol_message = wrap_decoded_protocol_message(&step.message_preimage);
            let rolling_state = build_rolling_state(
                chain_state.state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let result = IvcProverInput::prepare(
                snark_proof,
                &step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject a corrupted certificate proof")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert_eq!(err, IvcCircuitError::CertificateProofRejected);
        }

        // ----- Scenario 5: invalid genesis signature -----

        #[test]
        fn prepare_rejects_invalid_genesis_signature() {
            let setup = shared_ivc_setup();
            let asset_setup = shared_asset_setup();
            let first_step =
                load_embedded_first_step_cert_asset().expect("first step cert asset should load");

            let global = build_global();
            // Flip a low-order bit in the signature's challenge bytes to make it fail
            // verification while still being a structurally valid sig.
            let mut sig_bytes = asset_setup.genesis_signature.to_bytes();
            sig_bytes[32] ^= 0x01;
            let bad_signature = StandardSchnorrSignature::from_bytes(&sig_bytes)
                .expect("mutated signature should still deserialize");

            let combined_names: Vec<String> = setup.combined_fixed_bases.keys().cloned().collect();
            let rolling_state = IvcRollingState::genesis(bad_signature, &combined_names);

            let snark_proof = wrap_snark_proof(first_step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&first_step.avk_merkle_root);
            let decoded_protocol_message =
                wrap_decoded_protocol_message(&first_step.message_preimage);

            let result = IvcProverInput::prepare(
                snark_proof,
                &first_step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            );

            assert!(
                result.is_err(),
                "prepare should reject an invalid genesis signature"
            );
        }

        // ----- Scenario 6: invalid epoch transition -----

        #[test]
        fn prepare_rejects_invalid_epoch_transition() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_same_epoch_step_output_asset()
                .expect("same-epoch step output asset should load");

            // Force the chain's current epoch to be far away from the cert's epoch.
            // Same-epoch cert commits to some epoch E; we set the chain at u64::MAX - 100,
            // which is neither equal to nor one greater than E.
            let modified_state = State::new(
                chain_state.state.step_counter,
                chain_state.state.message,
                chain_state.state.merkle_tree_commitment,
                chain_state.state.next_merkle_tree_commitment,
                chain_state.state.protocol_parameters,
                chain_state.state.next_protocol_parameters,
                EpochNumber::new(u64::MAX - 100),
            );
            let rolling_state = build_rolling_state(
                modified_state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.avk_merkle_root);
            let decoded_protocol_message = wrap_decoded_protocol_message(&step.message_preimage);

            let result = IvcProverInput::prepare(
                snark_proof,
                &step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject a bad epoch transition")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert!(
                matches!(err, IvcCircuitError::InvalidEpochTransition { .. }),
                "expected InvalidEpochTransition, got {err:?}"
            );
        }

        // ----- Scenario 7: step counter overflow -----

        #[test]
        fn prepare_rejects_step_counter_overflow() {
            let setup = shared_ivc_setup();
            let chain_state = load_embedded_recursive_chain_state_asset()
                .expect("recursive chain state asset should load");
            let step = load_embedded_same_epoch_step_output_asset()
                .expect("same-epoch step output asset should load");

            // Force the chain's step counter to u64::MAX so the +1 advance overflows.
            let modified_state = State::new(
                StepCounter::new(u64::MAX),
                chain_state.state.message,
                chain_state.state.merkle_tree_commitment,
                chain_state.state.next_merkle_tree_commitment,
                chain_state.state.protocol_parameters,
                chain_state.state.next_protocol_parameters,
                chain_state.state.current_epoch,
            );
            let rolling_state = build_rolling_state(
                modified_state,
                chain_state.ivc_proof,
                chain_state.accumulator,
                chain_state.genesis_signature,
            );

            let global = build_global();
            let snark_proof = wrap_snark_proof(step.certificate_proof.clone().into_vec());
            let avk = wrap_avk(&step.avk_merkle_root);
            let decoded_protocol_message = wrap_decoded_protocol_message(&step.message_preimage);

            let result = IvcProverInput::prepare(
                snark_proof,
                &step.message,
                &avk,
                &global,
                &decoded_protocol_message,
                &rolling_state,
                setup,
            );

            let err = result
                .expect_err("prepare should reject step counter overflow")
                .downcast::<IvcCircuitError>()
                .expect("error should downcast to IvcCircuitError");
            assert!(
                matches!(err, IvcCircuitError::StepCounterOverflow { .. }),
                "expected StepCounterOverflow, got {err:?}"
            );
        }
    }
}
