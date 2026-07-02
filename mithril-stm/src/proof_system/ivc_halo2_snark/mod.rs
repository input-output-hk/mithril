pub(crate) mod errors;
pub(crate) mod proof;
mod prover_input;
mod prover_input_helpers;
mod prover_setup;
pub(crate) mod rolling_state;
pub(crate) mod verifier_setup;

pub(crate) use prover_setup::IvcSnarkProverSetup;
