//! `IvcRollingState`: caller-owned bridge between consecutive IVC proving steps.

use midnight_circuits::{
    types::Instantiable,
    verifier::{Accumulator, BlstrsEmulation},
};
use serde::{Deserialize, Serialize};

use crate::{
    StmResult,
    circuits::{
        halo2::types::CircuitBase,
        halo2_ivc::{
            AssignedAccumulator,
            errors::IvcCircuitError,
            state::{Global, State, trivial_acc},
            types::{EpochNumber, IvcProofBytes, StepCounter},
        },
    },
    signature_scheme::{BaseFieldElement, StandardSchnorrSignature},
};

/// Caller-owned bridge between consecutive IVC proving steps.
#[derive(Clone, Debug, Serialize, Deserialize)]
pub struct IvcRollingState {
    /// Last committed chain state.
    state: State,
    /// Bytes of the last IVC proof under the Poseidon transcript
    ivc_proof: IvcProofBytes,
    /// Folded accumulator the new step will build on top of
    #[serde(with = "midnight_accumulator_serde")]
    accumulator: Accumulator<BlstrsEmulation>,
    /// Chain-specific Schnorr signature over the genesis message
    genesis_signature: StandardSchnorrSignature,
}

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
    /// `fixed_base_names` must be the keys of `IvcProverSetup::combined_fixed_bases`.
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

    /// Returns `true` if this rolling state is at the genesis step (`step_counter == 0`).
    pub(crate) fn is_genesis(&self) -> bool {
        self.state.step_counter == StepCounter::ZERO
    }

    /// Returns `true` if the certificate belongs to the epoch immediately following
    /// the chain's current epoch (`certificate_epoch == current_epoch + 1`).
    pub(crate) fn is_next_epoch(&self, certificate_epoch: EpochNumber) -> bool {
        certificate_epoch.as_field()
            == self.state.current_epoch.as_field() + EpochNumber::new(1).as_field()
    }
}

pub(crate) mod midnight_accumulator_serde {
    use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};
    use midnight_proofs::utils::SerdeFormat;
    use serde::{Deserializer, Serializer};

    use crate::circuits::halo2_ivc::io::{Read, Write};

    /// Serialization function based on the write function of Midnight's Accumulator
    pub fn serialize<S: Serializer>(
        accumulator: &Accumulator<BlstrsEmulation>,
        serializer: S,
    ) -> Result<S::Ok, S::Error> {
        let mut buf = Vec::new();
        accumulator
            .write(&mut buf, SerdeFormat::RawBytesUnchecked)
            .map_err(serde::ser::Error::custom)?;
        serializer.serialize_bytes(&buf)
    }

    /// Deserialization function based on the read function of Midnight's Accumulator
    pub fn deserialize<'de, D: Deserializer<'de>>(
        deserializer: D,
    ) -> Result<Accumulator<BlstrsEmulation>, D::Error> {
        let bytes: Vec<u8> = serde::Deserialize::deserialize(deserializer)?;
        Accumulator::<BlstrsEmulation>::read(&mut bytes.as_slice(), SerdeFormat::RawBytesUnchecked)
            .map_err(serde::de::Error::custom)
    }
}

#[cfg(test)]
mod tests {
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        circuits::halo2_ivc::types::EpochNumber,
        signature_scheme::{BaseFieldElement, SchnorrSigningKey},
    };

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
    fn is_genesis_returns_true_for_genesis_state() {
        let genesis_signature = build_genesis_signature();
        let rolling_state = IvcRollingState::genesis(genesis_signature, &[]);
        assert!(rolling_state.is_genesis());
    }

    #[test]
    fn is_next_epoch_identifies_direct_successor_of_chain_epoch() {
        let genesis_signature = build_genesis_signature();
        let rolling_state = IvcRollingState::genesis(genesis_signature, &[]);
        // Genesis state has current_epoch == 0. The immediate next epoch is 1.
        assert!(rolling_state.is_next_epoch(EpochNumber::new(1)));
        assert!(!rolling_state.is_next_epoch(EpochNumber::new(0)));
        assert!(!rolling_state.is_next_epoch(EpochNumber::new(2)));
    }
}
