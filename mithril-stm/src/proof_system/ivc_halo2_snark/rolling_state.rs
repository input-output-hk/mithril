//! `IvcRollingState`: caller-owned bridge between consecutive IVC proving steps.

use midnight_circuits::verifier::{Accumulator, BlstrsEmulation};

use crate::{circuits::halo2_ivc::state::State, signature_scheme::StandardSchnorrSignature};

/// Caller-owned bridge between consecutive IVC proving steps.
// TODO: remove this allow dead_code directive when the IVC prover consumes this rolling state
#[allow(dead_code)]
pub(crate) struct IvcRollingState {
    /// Last committed chain state.
    pub(crate) state: State,
    /// Bytes of the last IVC proof under the Poseidon transcript
    pub(crate) ivc_proof: Vec<u8>,
    /// Folded accumulator the new step will build on top of
    pub(crate) accumulator: Accumulator<BlstrsEmulation>,
    /// Chain-specific Schnorr signature over the genesis state
    pub(crate) genesis_signature: StandardSchnorrSignature,
}
