use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::circuits::halo2_ivc::state::{State, Witness};

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
