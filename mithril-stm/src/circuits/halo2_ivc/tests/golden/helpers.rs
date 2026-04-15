use group::Group;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_proofs::{
    plonk::prepare,
    poly::kzg::{KZGCommitmentScheme, msm::DualMSM},
    transcript::{CircuitTranscript, Transcript},
};

use crate::circuits::halo2_ivc::{C, E, F, VerifyingKey};

/// Verifies a stored recursive proof that uses the Poseidon transcript.
pub(crate) fn verify_and_prepare_poseidon_recursive_proof(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    let mut transcript = CircuitTranscript::<PoseidonState<F>>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<PoseidonState<F>>>(
        verifying_key,
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .expect("recursive proof verification should succeed");
    transcript
        .assert_empty()
        .expect("recursive proof transcript should be empty");
    dual_msm
}

/// Verifies a stored recursive proof that uses the Blake2b transcript.
pub(crate) fn verify_and_prepare_blake2b_recursive_proof(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    let mut transcript = CircuitTranscript::<blake2b_simd::State>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<blake2b_simd::State>>(
        verifying_key,
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .expect("final recursive proof verification should succeed");
    transcript
        .assert_empty()
        .expect("final recursive proof transcript should be empty");
    dual_msm
}
