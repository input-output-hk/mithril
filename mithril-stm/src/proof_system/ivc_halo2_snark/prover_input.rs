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
        ivc_halo2_snark::{epoch_data::EpochData, rolling_state::IvcRollingState, setup::IvcSetup},
    },
};

/// Pre-circuit inputs consumed by the IVC prover's circuit-construction and proof-generation steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this input
#[allow(dead_code)]
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
        epoch_data: &EpochData,
        rolling_state: &IvcRollingState,
        setup: &IvcSetup,
    ) -> StmResult<Self> {
        // Verify the SNARK proof and prepare the dual MSM for the certificate accumulator.
        let dual_msm = snark_proof.prepare_and_check(
            message,
            aggregate_verification_key_for_snark,
            &setup.srs.verifier_params(),
        )?;

        let chain_epoch = rolling_state.state().current_epoch.as_field();
        let certificate_epoch = epoch_data.current_epoch().0;

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
            MerkleTreeCommitment::from_field(epoch_data.next_merkle_tree_commitment().0),
            new_protocol_parameters,
            ProtocolParametersHash::from_field(epoch_data.next_protocol_parameters().0),
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
            ProtocolMessagePreimage::from(*epoch_data.message_preimage()),
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
    // TODO: drop `allow(dead_code)` once step 7 wires up the actual test functions.
    #[allow(dead_code)]
    mod slow {
        use std::sync::{Arc, OnceLock};

        use tempfile::tempdir;

        use super::*;

        use crate::{
            MithrilMembershipDigest, Parameters,
            circuits::{
                halo2_ivc::{
                    K,
                    tests::common::{
                        asset_readers::{
                            VerificationContextAsset, load_embedded_verification_context_asset,
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

        /// Builds an `EpochData` from a stored protocol-message preimage.
        fn wrap_epoch_data(preimage: &[u8]) -> EpochData {
            use crate::circuits::halo2_ivc::PREIMAGE_SIZE;
            let preimage_array: [u8; PREIMAGE_SIZE] = preimage
                .try_into()
                .expect("preimage should be exactly PREIMAGE_SIZE bytes");
            EpochData::new(preimage_array).expect("epoch data should decode from preimage")
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
    }
}
