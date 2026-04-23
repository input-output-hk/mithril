//! Golden tests lock in recursive Halo2 IVC verification behavior.
//! This subtree also owns the committed assets, asset readers, generators, and
//! supporting test infrastructure for the golden workflow.

pub(crate) const ASSET_SEED: u64 = 42;
pub(crate) const CERTIFICATE_CIRCUIT_DEGREE: u32 = 13;
pub(crate) const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;

mod asset_readers;
mod cases;
mod generators;
mod helpers;

pub(crate) use asset_readers::{
    load_embedded_recursive_chain_state_asset, load_embedded_recursive_step_output_asset,
    load_embedded_verification_context_asset,
};
pub(crate) use generators::{
    build_asset_generation_setup, build_genesis_base_case_next_state,
    build_genesis_base_case_witness, build_genesis_protocol_message_preimage,
};
pub(crate) use helpers::{
    assert_recursive_mock_prover_rejects, build_recursive_mock_prover_setup,
    verify_and_prepare_blake2b_recursive_proof,
};
