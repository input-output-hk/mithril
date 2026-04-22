use crate::circuits::halo2_ivc::{
    PREIMAGE_SIZE,
    tests::golden::{build_asset_generation_setup, build_genesis_protocol_message_preimage},
};

#[test]
fn preimage_length_is_190_bytes() {
    // Off-circuit check that the genesis protocol message serializer produces
    // exactly PREIMAGE_SIZE bytes, matching the fixed byte ranges the circuit
    // reads for next_merkle_root, next_protocol_params, and current_epoch.
    let setup = build_asset_generation_setup();
    let preimage = build_genesis_protocol_message_preimage(&setup);
    assert_eq!(preimage.len(), PREIMAGE_SIZE);
}
