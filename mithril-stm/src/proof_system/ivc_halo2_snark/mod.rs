pub(crate) mod errors;
pub(crate) mod proof;
mod prover_input;
mod prover_setup;
pub(crate) mod rolling_state;
pub(crate) mod temp_ivc_prover_cache;
mod unsafe_setup_helpers;
pub(crate) mod verifier_setup;

pub(crate) use prover_setup::IvcProverSetup;
pub(crate) use temp_ivc_prover_cache::load_ivc_prover_setup;
pub(crate) use unsafe_setup_helpers::{TempCertificateKeyProvider, TempIvcKeyProvider};

// The recursive circuit's PLONK key aliases are defined in `circuits::halo2_ivc` (the circuit
// layer) and re-exported here so the proof-system modules keep their existing import paths.
pub(crate) use crate::circuits::halo2_ivc::{PlonkProvingKey, PlonkVerifyingKey};
