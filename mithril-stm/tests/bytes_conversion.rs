mod test_extensions;

use blake2::{Blake2b, digest::consts::U32};
use rand_chacha::ChaCha20Rng;
use rand_core::{RngCore, SeedableRng};

use mithril_stm::{StmAggrSig, StmInitializer, StmParameters, StmSig, StmVerificationKey};

use test_extensions::protocol_phase::{
    InitializationPhaseResult, OperationPhaseResult, initialization_phase, operation_phase,
};

#[test]
fn test_stm_parameters_serialization() {
    let params = StmParameters {
        m: 100,
        k: 2,
        phi_f: 0.2,
    };
    let bytes = params.to_bytes();
    let deserialized_params = StmParameters::from_bytes(&bytes).unwrap();

    assert_eq!(params, deserialized_params);
}

#[test]
fn test_binary_conversions() {
    let nparties: usize = 32;
    let mut rng = ChaCha20Rng::from_seed([0u8; 32]);
    let mut msg = [0u8; 32];
    rng.fill_bytes(&mut msg);

    let params = StmParameters {
        k: 20,
        m: 200,
        phi_f: 0.9,
    };
    let InitializationPhaseResult {
        signers,
        reg_parties,
        initializers,
    } = initialization_phase(nparties, rng.clone(), params);

    let encoded = params.to_bytes();
    StmParameters::from_bytes(&encoded[1..]).expect_err("Decoding should fail with invalid bytes");
    let decoded = StmParameters::from_bytes(&encoded).unwrap();
    assert_eq!(params, decoded);

    let verification_key = reg_parties[0].0;
    let encoded = verification_key.to_bytes();
    StmVerificationKey::from_bytes(&encoded[1..])
        .expect_err("StmVerificationKey decoding should fail with invalid bytes");
    let decoded = StmVerificationKey::from_bytes(&encoded).unwrap();
    assert_eq!(verification_key, decoded);

    let initializer = &initializers[0];
    let encoded = initializer.to_bytes();
    StmInitializer::from_bytes(&encoded[1..])
        .expect_err("StmInitializer decoding should fail with invalid bytes");
    let decoded = StmInitializer::from_bytes(&encoded).unwrap();
    assert_eq!(initializer.to_bytes(), decoded.to_bytes());

    let OperationPhaseResult { msig, avk: _, sigs } =
        operation_phase(params, signers, reg_parties, msg);

    let sig = &sigs[0];
    let encoded = sig.to_bytes();
    StmSig::from_bytes::<Blake2b<U32>>(&encoded[1..])
        .expect_err("StmSig decoding should fail with invalid bytes");
    let decoded = StmSig::from_bytes::<Blake2b<U32>>(&encoded).unwrap();
    assert_eq!(sig, &decoded);

    let msig = msig.unwrap();
    let encoded = msig.to_bytes();
    StmAggrSig::<Blake2b<U32>>::from_bytes(&encoded[1..])
        .expect_err("StmAggrSig decoding should fail with invalid bytes");
    let decoded = StmAggrSig::<Blake2b<U32>>::from_bytes(&encoded).unwrap();
    assert_eq!(msig.to_bytes(), decoded.to_bytes());
}
