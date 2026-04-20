use super::{
    build_asset_generation_setup, default_asset_paths, generate_recursive_chain_state_asset,
    generate_recursive_step_output_asset, generate_verification_context_asset,
};

// These ignored tests are manual asset-generation entrypoints for committed
// golden assets. They are intentionally excluded from normal test runs because
// they rewrite fixture files rather than asserting behavior.
#[test]
#[ignore]
fn generate_verification_context_only() {
    let setup = build_asset_generation_setup();
    let paths = default_asset_paths();

    generate_verification_context_asset(&setup, &paths);
}

#[test]
#[ignore]
fn generate_recursive_chain_state_only() {
    let setup = build_asset_generation_setup();
    let paths = default_asset_paths();

    generate_recursive_chain_state_asset(&setup, &paths);
}

#[test]
#[ignore]
fn generate_recursive_step_output_only() {
    let setup = build_asset_generation_setup();
    let paths = default_asset_paths();

    generate_recursive_step_output_asset(&setup, &paths);
}
