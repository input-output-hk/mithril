//! `IvcRollingState`: caller-owned bridge between consecutive IVC proving steps.

use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, BlstrsEmulation},
};

use crate::{
    StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            AssignedAccumulator,
            errors::IvcCircuitError,
            state::{Global, State, trivial_acc},
            types::{IvcProofBytes, StepCounter},
        },
    },
    signature_scheme::{BaseFieldElement, StandardSchnorrSignature},
};

use super::proof::IvcProof;

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

// TODO: remove this allow dead_code directive when the IVC prover uses this rolling state
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

    /// Verifies the chain's genesis Schnorr signature against the genesis message and
    /// verification key carried in `global`. Called at the first proving step only.
    pub(crate) fn verify_genesis_signature(&self, global: &Global) -> StmResult<()> {
        self.genesis_signature.verify(
            &[BaseFieldElement::from(global.genesis_message.as_field())],
            &global.genesis_verification_key,
        )
    }

    /// Returns the public inputs expected by the IVC verifier gadget for the previous
    /// step's IVC proof: `[global | previous state | previous folded accumulator]`.
    pub(crate) fn previous_ivc_proof_public_inputs(&self, global: &Global) -> Vec<CircuitBase> {
        [
            global.as_public_input(),
            self.state.as_public_input(),
            AssignedAccumulator::as_public_input(&self.accumulator),
        ]
        .concat()
    }

    /// Returns the step counter for the next step (current + 1). Errors if the counter
    /// would overflow `u64`.
    pub(crate) fn new_step_counter(&self) -> StmResult<StepCounter> {
        let current = self.state.step_counter.as_u64();
        let next = current
            .checked_add(1)
            .ok_or(IvcCircuitError::StepCounterOverflow { current })?;
        Ok(StepCounter::new(next))
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
