use std::hash::Hash;

use ff::FromUniformBytes;
use group::Group;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{create_proof, prepare},
    poly::commitment::PolynomialCommitmentScheme,
    poly::kzg::{KZGCommitmentScheme, msm::DualMSM, params::ParamsKZG},
    transcript::{CircuitTranscript, Hashable, Sampleable, Transcript, TranscriptHash},
};
use rand_core::{CryptoRng, RngCore};

use crate::circuits::AsPlonkVerifyingKey;
use crate::circuits::halo2_ivc::keys::RecursiveCircuitProvingKey;
use crate::circuits::halo2_ivc::{C, E, F, circuit::IvcCircuitData};

/// Generates a recursive proof using the chosen transcript hash.
fn prove_ivc_with_transcript<H: TranscriptHash>(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &RecursiveCircuitProvingKey,
    ivc_circuit_data: &IvcCircuitData,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
    proof_generation_error: &str,
) -> Vec<u8>
where
    F: Sampleable<H> + Hashable<H> + Hash + Ord + FromUniformBytes<64>,
    <KZGCommitmentScheme<E> as PolynomialCommitmentScheme<F>>::Commitment: Hashable<H>,
{
    let mut transcript = CircuitTranscript::<H>::init();
    create_proof::<F, KZGCommitmentScheme<E>, CircuitTranscript<H>, IvcCircuitData>(
        commitment_parameters,
        proving_key.proving_key(),
        std::slice::from_ref(ivc_circuit_data),
        1,
        &[&[&[], public_inputs]],
        random_generator,
        &mut transcript,
    )
    .expect(proof_generation_error);
    transcript.finalize()
}

/// Verifies a recursive proof and returns the prepared MSM.
fn verify_prepare_ivc_with_transcript<H: TranscriptHash>(
    verifying_key: &impl AsPlonkVerifyingKey,
    proof: &[u8],
    public_inputs: &[F],
    verification_error: &str,
    transcript_error: &str,
) -> DualMSM<E>
where
    F: Sampleable<H> + Hashable<H> + Hash + Ord + FromUniformBytes<64>,
    <KZGCommitmentScheme<E> as PolynomialCommitmentScheme<F>>::Commitment: Hashable<H>,
{
    let mut transcript = CircuitTranscript::<H>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<H>>(
        verifying_key.plonk_verifying_key(),
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .expect(verification_error);
    transcript.assert_empty().expect(transcript_error);
    dual_msm
}

/// Generates a recursive proof using the Poseidon transcript.
pub(crate) fn prove_poseidon_ivc(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &RecursiveCircuitProvingKey,
    ivc_circuit_data: &IvcCircuitData,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
) -> Vec<u8> {
    prove_ivc_with_transcript::<PoseidonState<F>>(
        commitment_parameters,
        proving_key,
        ivc_circuit_data,
        public_inputs,
        random_generator,
        "IVC proof generation should not fail",
    )
}

/// Verifies a recursive proof using the Poseidon transcript and returns the prepared MSM.
pub(crate) fn verify_prepare_poseidon_ivc(
    verifying_key: &impl AsPlonkVerifyingKey,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    verify_prepare_ivc_with_transcript::<PoseidonState<F>>(
        verifying_key,
        proof,
        public_inputs,
        "Poseidon recursive proof verification should succeed",
        "Poseidon transcript should be empty after verification",
    )
}

/// Generates the final recursive proof using the Blake2b transcript.
pub(crate) fn prove_blake2b_ivc(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &RecursiveCircuitProvingKey,
    ivc_circuit_data: &IvcCircuitData,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
) -> Vec<u8> {
    prove_ivc_with_transcript::<blake2b_simd::State>(
        commitment_parameters,
        proving_key,
        ivc_circuit_data,
        public_inputs,
        random_generator,
        "IVC Blake2b proof generation should not fail",
    )
}

/// Attempts to verify a proof using the Poseidon transcript without panicking.
///
/// Returns `None` if the bytes are too malformed to parse (deserialization error
/// or leftover bytes after verification). Returns `Some(dual_msm)` if the bytes
/// parse successfully — the caller must still call `dual_msm.check()` to determine
/// whether the KZG opening equations hold.
///
/// Use this instead of `verify_prepare_poseidon_ivc` when testing with tampered
/// bytes where a panic-on-error would be inappropriate.
pub(crate) fn try_verify_prepare_poseidon_ivc(
    verifying_key: &impl AsPlonkVerifyingKey,
    proof: &[u8],
    public_inputs: &[F],
) -> Option<DualMSM<E>> {
    let mut transcript = CircuitTranscript::<PoseidonState<F>>::init_from_bytes(proof);
    let dual_msm = prepare::<F, KZGCommitmentScheme<E>, CircuitTranscript<PoseidonState<F>>>(
        verifying_key.plonk_verifying_key(),
        &[&[C::identity()]],
        &[&[public_inputs]],
        &mut transcript,
    )
    .ok()?;
    transcript.assert_empty().ok()?;
    Some(dual_msm)
}

/// Verifies the final recursive proof using the Blake2b transcript.
pub(crate) fn verify_prepare_blake2b_ivc(
    verifying_key: &impl AsPlonkVerifyingKey,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    verify_prepare_ivc_with_transcript::<blake2b_simd::State>(
        verifying_key,
        proof,
        public_inputs,
        "Blake2b recursive proof verification should succeed",
        "Blake2b transcript should be empty after verification",
    )
}
