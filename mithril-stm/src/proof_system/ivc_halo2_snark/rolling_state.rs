//! `IvcRollingState`: caller-owned bridge between consecutive IVC proving steps.

use sha2::{Digest, Sha256};

use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, AssignedAccumulator, BlstrsEmulation},
};

use crate::{
    StmResult,
    circuits::halo2::types::CircuitBase,
    circuits::halo2_ivc::{
        certificate_proof::verify_and_prepare_poseidon,
        errors::IvcCircuitError,
        state::{Global, State, Witness, trivial_acc},
        types::{
            CertificateProofBytes, IvcProofBytes, MerkleTreeCommitment, MessageHash,
            ProtocolMessagePreimage, ProtocolParametersHash, StepCounter,
        },
    },
    signature_scheme::StandardSchnorrSignature,
};

use super::{
    epoch_data::EpochData, proof::IvcProof, prover_input::IvcProverInput, setup::IvcSetup,
};

/// Caller-owned bridge between consecutive IVC proving steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this rolling state
#[allow(dead_code)]
#[derive(Debug)]
pub(crate) struct IvcRollingState {
    /// Last committed chain state.
    state: State,
    /// Bytes of the last IVC proof under the Poseidon transcript
    ivc_proof: IvcProofBytes,
    /// Folded accumulator the new step will build on top of
    accumulator: Accumulator<BlstrsEmulation>,
    /// Chain-specific Schnorr signature over the genesis state
    genesis_signature: StandardSchnorrSignature,
}

#[allow(dead_code)]
impl IvcRollingState {
    /// Builds a rolling state from the four fields produced by an IVC proving step.
    pub(crate) fn new(
        state: State,
        ivc_proof: IvcProofBytes,
        accumulator: Accumulator<BlstrsEmulation>,
        genesis_signature: StandardSchnorrSignature,
    ) -> Self {
        Self {
            state,
            ivc_proof,
            accumulator,
            genesis_signature,
        }
    }

    /// Builds the genesis rolling state: zeroed chain state, empty IVC proof,
    /// trivial accumulator over the supplied fixed-base names, and the
    /// caller-supplied genesis signature.
    ///
    /// `fixed_base_names` must be the keys of `IvcSetup::combined_fixed_bases`.
    /// Passing any other list produces a genesis accumulator that does not match
    /// what the in-circuit verifier expects, and the first proving step will fail.
    pub(crate) fn genesis(
        genesis_signature: StandardSchnorrSignature,
        fixed_base_names: &[String],
    ) -> Self {
        Self {
            state: State::genesis(),
            ivc_proof: IvcProofBytes::empty(),
            accumulator: trivial_acc(fixed_base_names),
            genesis_signature,
        }
    }

    /// Builds the rolling state for the next step from the previous IVC proof.
    /// The chain state, proof bytes, and folded accumulator are pulled from the
    /// previous proof; the genesis signature is the chain-specific constant
    /// supplied once at session start.
    pub(crate) fn from_previous_proof(
        previous_proof: &IvcProof,
        genesis_signature: StandardSchnorrSignature,
    ) -> Self {
        Self {
            state: previous_proof.state.clone(),
            ivc_proof: previous_proof.proof_bytes.clone(),
            accumulator: previous_proof.accumulator.clone(),
            genesis_signature,
        }
    }

    /// Returns the last committed chain state.
    pub(crate) fn state(&self) -> &State {
        &self.state
    }

    /// Returns the last IVC proof bytes wrapper.
    pub(crate) fn ivc_proof(&self) -> &IvcProofBytes {
        &self.ivc_proof
    }

    /// Returns the folded accumulator the new step will build on top of.
    pub(crate) fn accumulator(&self) -> &Accumulator<BlstrsEmulation> {
        &self.accumulator
    }

    /// Returns the chain-specific Schnorr signature over the genesis state.
    pub(crate) fn genesis_signature(&self) -> StandardSchnorrSignature {
        self.genesis_signature
    }

    /// Executes all off-circuit IVC prover preparation steps and returns the
    /// pre-circuit inputs for the next IVC proving step.
    ///
    /// # Steps
    ///
    /// 1. **Cert message**: SHA2-256 of the protocol-message preimage, interpreted
    ///    as a little-endian base-field element (mirrors the circuit's `combine_bytes`
    ///    gadget on the SHA256 output).
    /// 2. **Epoch classification**: compares the certificate's epoch against the
    ///    current chain state to determine genesis / same-epoch / next-epoch.
    ///    Returns `IvcCircuitError::InvalidEpoch` if epoch is neither.
    /// 3. **Certificate accumulator**: genesis → trivial accumulator (in-circuit the
    ///    result is zeroed via `scale_by_bit`; off-circuit we skip verification entirely);
    ///    non-genesis → verify the certificate SNARK proof under the Poseidon transcript,
    ///    extract the verifier's `DualMSM`, apply `extract_fixed_bases` with the
    ///    certificate fixed-base map, and collapse.
    /// 4. **Genesis signature**: included in the witness; in-circuit validation of
    ///    the Schnorr signature is enforced by the IVC circuit on the genesis step.
    /// 5. **Witness construction**: packages genesis signature, cert merkle root, cert
    ///    message, and raw preimage bytes for the circuit.
    /// 6. **Chain state advancement**: applies the circuit's transition rules to
    ///    produce `next_state`.
    /// 7. **Self-proof accumulator**: genesis → trivial accumulator; non-genesis →
    ///    verify previous IVC proof, extract, collapse.
    /// 8. **Accumulator folding**: genesis → trivial accumulator (matches `scale_by_bit`
    ///    behaviour in the circuit); non-genesis → `accumulate([chain_acc, cert_acc,
    ///    self_acc])` then `collapse()`.
    pub(crate) fn prepare_prover_input(
        &self,
        global: &Global,
        epoch_data: &EpochData,
        cert_proof: &CertificateProofBytes,
        setup: &IvcSetup,
    ) -> StmResult<IvcProverInput> {
        let is_genesis = self.state.counter == StepCounter::ZERO;
        let preimage = epoch_data.message_preimage();

        // Shared across genesis and non-genesis steps.
        let combined_fixed_base_names: Vec<String> =
            setup.combined_fixed_bases.keys().cloned().collect();

        // Step 1: cert_msg = SHA256(preimage) as a little-endian base-field element.
        // Mirrors the circuit's `sha2_256_chip.hash` + `combine_bytes` on the digest.
        // SHA256 always produces 32 bytes so the slice conversions below are infallible.
        let hash_bytes: [u8; 32] = Sha256::digest(preimage.as_slice()).into();
        let cert_msg = MessageHash::from_field(CircuitBase::from_raw([
            u64::from_le_bytes(hash_bytes[0..8].try_into().unwrap()),
            u64::from_le_bytes(hash_bytes[8..16].try_into().unwrap()),
            u64::from_le_bytes(hash_bytes[16..24].try_into().unwrap()),
            u64::from_le_bytes(hash_bytes[24..32].try_into().unwrap()),
        ]));

        // Step 2: classify the step.
        // is_same_epoch: cert epoch == state epoch.
        // is_next_epoch: cert epoch == state epoch + 1.
        // Genesis: epoch constraints do not apply.
        let epoch_field: CircuitBase = epoch_data.current_epoch().0;
        let is_same_epoch = epoch_field == self.state.current_epoch.as_field();
        let is_next_epoch =
            epoch_field == self.state.current_epoch.as_field() + CircuitBase::from(1u64);

        if !is_genesis && !is_same_epoch && !is_next_epoch {
            return Err(IvcCircuitError::InvalidEpoch.into());
        }

        // cert_merkle_root: the Merkle root the certificate was signed under.
        // Genesis → ZERO; same-epoch → state.merkle_root; next-epoch → state.next_merkle_root.
        let cert_merkle_root = if is_genesis {
            MerkleTreeCommitment::ZERO
        } else if is_same_epoch {
            self.state.merkle_root
        } else {
            self.state.next_merkle_root
        };

        // Decode next_merkle_root, next_protocol_params, and current_epoch via EpochData
        // typed accessors (decoded once in EpochData::new using the same LE combine_bytes
        // encoding the circuit applies).
        let next_merkle_root = epoch_data.next_merkle_tree_commitment();
        let next_protocol_params = epoch_data.next_protocol_parameters_hash();
        let current_epoch = epoch_data.epoch_number();

        // Steps 4 & 5: build the witness.
        // genesis_sig is carried from the rolling state unchanged.
        // cert_msg and cert_merkle_root are used as the certificate public inputs.
        let witness = Witness::new(
            self.genesis_signature,
            cert_merkle_root,
            cert_msg,
            ProtocolMessagePreimage::from(*preimage),
        );

        // Step 6: advance chain state following the circuit's transition function.
        //
        // msg:             genesis → genesis_msg; else → SHA256(preimage)
        // merkle_root:     genesis → ZERO; else → cert_merkle_root
        // protocol_params: genesis → ZERO; same-epoch → state.protocol_params;
        //                  next-epoch → state.next_protocol_params
        let msg = if is_genesis {
            global.genesis_msg
        } else {
            cert_msg
        };
        let protocol_params = if is_genesis {
            ProtocolParametersHash::ZERO
        } else if is_same_epoch {
            self.state.protocol_params
        } else {
            self.state.next_protocol_params
        };
        let next_counter = self
            .state
            .counter
            .as_u64()
            .checked_add(1)
            .ok_or(IvcCircuitError::StepCounterOverflow)?;
        let next_state = State::new(
            StepCounter::new(next_counter),
            msg,
            cert_merkle_root, // = ZERO for genesis by construction
            next_merkle_root,
            protocol_params,
            next_protocol_params,
            current_epoch,
        );

        let verifier_params = setup.srs.verifier_params();

        // Step 3: verify certificate SNARK proof and extract accumulator.
        // Genesis: the circuit zeros the cert accumulator contribution via
        // `scale_by_bit(is_not_genesis)` so we use trivial_acc off-circuit.
        // Non-genesis: public inputs match the circuit's `prepare` call: [cert_merkle_root, cert_msg].
        let cert_public_inputs: Vec<CircuitBase> =
            vec![cert_merkle_root.as_field(), cert_msg.as_field()];
        let cert_accumulator = if is_genesis {
            trivial_acc(&combined_fixed_base_names)
        } else {
            let dual_msm = verify_and_prepare_poseidon(
                cert_proof.as_bytes(),
                &cert_public_inputs,
                &setup.certificate_verifying_key,
                &verifier_params,
            )?;
            let mut acc: Accumulator<BlstrsEmulation> = dual_msm.into();
            acc.extract_fixed_bases(&setup.certificate_fixed_bases);
            acc.collapse();
            acc
        };

        // Step 7: prepare the IVC self-proof accumulator.
        // Genesis: no previous IVC proof → trivial accumulator over combined fixed bases.
        // Non-genesis: verify previous IVC proof under Poseidon transcript.
        let self_proof_accumulator = if is_genesis {
            trivial_acc(&combined_fixed_base_names)
        } else {
            // Public inputs of the previous IVC proof: [global | state | accumulator].
            let self_public_inputs: Vec<CircuitBase> = [
                global.as_public_input(),
                self.state.as_public_input(),
                AssignedAccumulator::as_public_input(&self.accumulator),
            ]
            .concat();
            let self_dual_msm = verify_and_prepare_poseidon(
                self.ivc_proof.as_bytes(),
                &self_public_inputs,
                &setup.ivc_verifying_key,
                &verifier_params,
            )
            .map_err(|_| IvcCircuitError::IvcProofRejected)?;
            let mut self_acc: Accumulator<BlstrsEmulation> = self_dual_msm.into();
            self_acc.extract_fixed_bases(&setup.ivc_fixed_bases);
            self_acc.collapse();
            self_acc
        };

        // Step 8: fold the three accumulators and collapse.
        // Genesis: the circuit zeros cert and self-proof contributions via `scale_by_bit`;
        // return trivial_acc directly to match the in-circuit output exactly.
        // Non-genesis: order matches the in-circuit `accumulate` call:
        // [chain_acc, cert_acc, self_acc].
        let next_accumulator = if is_genesis {
            trivial_acc(&combined_fixed_base_names)
        } else {
            let mut acc = Accumulator::accumulate(&[
                self.accumulator.clone(),
                cert_accumulator,
                self_proof_accumulator,
            ]);
            acc.collapse();
            acc
        };

        Ok(IvcProverInput {
            witness,
            cert_proof: cert_proof.clone(),
            next_state,
            next_accumulator,
        })
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::signature_scheme::{BaseFieldElement, SchnorrSigningKey};

    use super::*;

    fn build_genesis_signature() -> StandardSchnorrSignature {
        let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
        let signing_key = SchnorrSigningKey::generate(&mut rng);
        let message = vec![BaseFieldElement::from(1u64)];
        signing_key.sign_standard(&message, &mut rng).unwrap()
    }

    #[test]
    fn genesis_initializes_zero_state_empty_proof_and_supplied_signature() {
        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string(), "base_two".to_string()];

        let rolling_state = IvcRollingState::genesis(genesis_signature, &fixed_base_names);

        assert_eq!(
            rolling_state.state().as_public_input(),
            State::genesis().as_public_input(),
            "genesis rolling state must carry the genesis chain state"
        );
        assert!(
            rolling_state.ivc_proof().is_empty(),
            "genesis rolling state must have an empty IVC proof"
        );
        assert_eq!(rolling_state.genesis_signature(), genesis_signature);
    }

    #[test]
    fn genesis_accumulator_carries_supplied_fixed_base_names() {
        let genesis_signature = build_genesis_signature();
        let fixed_base_names = vec!["base_one".to_string(), "base_two".to_string()];

        let rolling_state = IvcRollingState::genesis(genesis_signature, &fixed_base_names);

        let mut accumulator_fixed_base_keys: Vec<String> = rolling_state
            .accumulator()
            .rhs()
            .fixed_base_scalars()
            .keys()
            .cloned()
            .collect();
        accumulator_fixed_base_keys.sort();
        let mut expected = fixed_base_names.clone();
        expected.sort();
        assert_eq!(accumulator_fixed_base_keys, expected);
    }

    #[test]
    fn from_previous_proof_carries_proof_bytes_and_supplied_signature() {
        let genesis_signature = build_genesis_signature();
        let proof_bytes = IvcProofBytes::new(vec![0xDE, 0xAD, 0xBE, 0xEF, 0x01, 0x02, 0x03]);
        let previous_proof = IvcProof {
            proof_bytes: proof_bytes.clone(),
            state: State::genesis(),
            accumulator: trivial_acc(&["base_one".to_string()]),
        };

        let rolling_state =
            IvcRollingState::from_previous_proof(&previous_proof, genesis_signature);

        assert_eq!(rolling_state.ivc_proof(), &proof_bytes);
        assert_eq!(rolling_state.genesis_signature(), genesis_signature);
    }

    #[test]
    fn from_previous_proof_chain_state_matches_previous_proof_chain_state() {
        let genesis_signature = build_genesis_signature();
        let previous_state = State::genesis();
        let previous_proof = IvcProof {
            proof_bytes: IvcProofBytes::empty(),
            state: previous_state,
            accumulator: trivial_acc(&["base_one".to_string()]),
        };

        let rolling_state =
            IvcRollingState::from_previous_proof(&previous_proof, genesis_signature);

        assert_eq!(
            rolling_state.state().as_public_input(),
            State::genesis().as_public_input(),
            "rolling state's chain state must mirror the previous proof's chain state"
        );
    }
}
