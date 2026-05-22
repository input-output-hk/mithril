//! Tests that IvcCircuit constructor validations return the expected typed errors.

use crate::circuits::halo2_ivc::{
    K, circuit::IvcCircuit, errors::IvcCircuitError,
    tests::common::asset_readers::load_embedded_verification_context_asset,
};

#[test]
fn validate_self_vk_degree_rejects_wrong_degree_vk() {
    // The certificate VK stored in the verification context has degree
    // CERTIFICATE_CIRCUIT_DEGREE (13) != K (19), so it is a cheap wrong-degree
    // input for validate_self_vk_degree without requiring any SRS generation.
    let ctx =
        load_embedded_verification_context_asset().expect("verification context asset should load");
    let wrong_degree_vk = ctx.certificate_verifying_key.vk();
    let actual_degree = wrong_degree_vk.get_domain().k();

    let result = IvcCircuit::validate_self_vk_degree(wrong_degree_vk);

    let err = result
        .unwrap_err()
        .downcast::<IvcCircuitError>()
        .expect("error should downcast to IvcCircuitError");
    assert_eq!(
        err,
        IvcCircuitError::SelfVkDegreeMismatch {
            expected: K,
            actual: actual_degree,
        }
    );
}
