use std::hash::Hash;

use ff::FromUniformBytes;
use group::Group;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_curves::Bls12;
use midnight_proofs::{
    plonk::{ProvingKey, VerifyingKey, create_proof, prepare},
    poly::commitment::PolynomialCommitmentScheme,
    poly::kzg::{KZGCommitmentScheme, msm::DualMSM, params::ParamsKZG},
    transcript::{CircuitTranscript, Hashable, Sampleable, Transcript, TranscriptHash},
};
use rand_core::{CryptoRng, RngCore};

use crate::circuits::halo2_ivc::{C, E, F, circuit::IvcCircuit};

/// Generates a recursive proof using the chosen transcript hash.
fn prove_ivc_with_transcript<H: TranscriptHash>(
    commitment_parameters: &ParamsKZG<Bls12>,
    proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    circuit: &IvcCircuit,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
    proof_generation_error: &str,
) -> Vec<u8>
where
    F: Sampleable<H> + Hashable<H> + Hash + Ord + FromUniformBytes<64>,
    <KZGCommitmentScheme<E> as PolynomialCommitmentScheme<F>>::Commitment: Hashable<H>,
{
    let mut transcript = CircuitTranscript::<H>::init();
    create_proof::<F, KZGCommitmentScheme<E>, CircuitTranscript<H>, IvcCircuit>(
        commitment_parameters,
        proving_key,
        std::slice::from_ref(circuit),
        1,
        &[&[&[], public_inputs]],
        random_generator,
        &mut transcript,
    )
    .expect(proof_generation_error);
    transcript.finalize()
}

/// Verifies a recursive proof and returns the prepared MSM.
fn verify_and_prepare_ivc_with_transcript<H: TranscriptHash>(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
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
        verifying_key,
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
    proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    circuit: &IvcCircuit,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
) -> Vec<u8> {
    prove_ivc_with_transcript::<PoseidonState<F>>(
        commitment_parameters,
        proving_key,
        circuit,
        public_inputs,
        random_generator,
        "IVC proof generation should not fail",
    )
}

/// Verifies a recursive proof using the Poseidon transcript and returns the prepared MSM.
pub(crate) fn verify_prepare_poseidon_ivc(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    verify_and_prepare_ivc_with_transcript::<PoseidonState<F>>(
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
    proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    circuit: &IvcCircuit,
    public_inputs: &[F],
    random_generator: &mut (impl RngCore + CryptoRng),
) -> Vec<u8> {
    prove_ivc_with_transcript::<blake2b_simd::State>(
        commitment_parameters,
        proving_key,
        circuit,
        public_inputs,
        random_generator,
        "IVC Blake2b proof generation should not fail",
    )
}

/// Verifies the final recursive proof using the Blake2b transcript.
pub(crate) fn verify_prepare_blake2b_ivc(
    verifying_key: &VerifyingKey<F, KZGCommitmentScheme<E>>,
    proof: &[u8],
    public_inputs: &[F],
) -> DualMSM<E> {
    verify_and_prepare_ivc_with_transcript::<blake2b_simd::State>(
        verifying_key,
        proof,
        public_inputs,
        "Blake2b recursive proof verification should succeed",
        "Blake2b transcript should be empty after verification",
    )
}
