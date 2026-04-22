//! Generator helpers and entrypoints for recursive Halo2 IVC golden assets.

mod asset_generation;
mod proofs;
mod setup;
mod tests;
mod transitions;

pub(crate) use asset_generation::{
    generate_recursive_chain_state_asset, generate_recursive_step_output_asset,
    generate_verification_context_asset,
};
pub(crate) use proofs::{verify_and_prepare_blake2b_ivc, verify_and_prepare_poseidon_ivc};
pub(crate) use setup::{
    AssetGenerationSetup, build_asset_generation_setup, build_recursive_fixed_bases,
    build_recursive_global, build_shared_recursive_context,
};
pub(crate) use transitions::{
    build_genesis_base_case_next_state, build_genesis_base_case_witness,
    build_genesis_protocol_message_preimage, build_next_certificate_asset_data,
    build_same_epoch_certificate_asset_data, certificate_public_inputs_for_step,
    next_message_and_preimage_for_step, next_state_for_step,
};
