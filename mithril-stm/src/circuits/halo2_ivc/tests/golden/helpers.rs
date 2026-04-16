use std::collections::BTreeMap;

use group::Group;
use midnight_circuits::hash::poseidon::PoseidonState;
use midnight_circuits::types::Instantiable;
use midnight_curves::Bls12;
use midnight_curves::pairing::Engine;
use midnight_proofs::{
    dev::MockProver,
    plonk::{keygen_vk_with_k, prepare},
    poly::kzg::{
        KZGCommitmentScheme,
        msm::DualMSM,
        params::{ParamsKZG, ParamsVerifierKZG},
    },
    transcript::{CircuitTranscript, Transcript},
};
use midnight_zk_stdlib as zk_lib;
use rand_chacha::ChaCha20Rng;
use rand_core::SeedableRng;

use crate::circuits::halo2_ivc::tests::{
    asset_readers::RecursiveChainStateAsset, generators::AssetGenerationSetup,
};
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, CERT_VK_NAME, E, F, IVC_ONE_NAME, K, VerifyingKey,
    circuit::IvcCircuit,
    state::{Global, fixed_bases_and_names, trivial_acc},
};

const ASSET_SEED: u64 = 42;
const CERTIFICATE_CIRCUIT_DEGREE: u32 = 13;
const RECURSIVE_CIRCUIT_DEGREE: u32 = 19;

/// Shared recursive context reused by MockProver-based golden cases.
pub(crate) struct RecursiveMockProverSetup {
    pub(crate) certificate_commitment_parameters: ParamsKZG<Bls12>,
    pub(crate) certificate_verifying_key: midnight_zk_stdlib::MidnightVK,
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
    pub(crate) global: Global,
    pub(crate) recursive_fixed_bases: BTreeMap<String, C>,
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
    pub(crate) universal_verifier_params: ParamsVerifierKZG<E>,
    pub(crate) verifier_tau_in_g2: <E as Engine>::G2Affine,
    pub(crate) trivial_accumulator:
        crate::circuits::halo2_ivc::Accumulator<crate::circuits::halo2_ivc::S>,
}

fn build_deterministic_params(circuit_degree: u32) -> ParamsKZG<Bls12> {
    ParamsKZG::<Bls12>::unsafe_setup(circuit_degree, ChaCha20Rng::seed_from_u64(ASSET_SEED))
}

/// Builds the shared recursive circuit context needed by MockProver-based golden tests.
///
/// This mirrors the verifier-side setup used by the asset generators, but keeps
/// the logic local to the golden helper layer so the base-case test can be
/// reviewed independently from the generator code.
pub(crate) fn build_recursive_mock_prover_setup(
    setup: &AssetGenerationSetup,
) -> RecursiveMockProverSetup {
    let shared_srs_degree = RECURSIVE_CIRCUIT_DEGREE.max(CERTIFICATE_CIRCUIT_DEGREE);
    let universal_kzg_parameters = build_deterministic_params(shared_srs_degree);
    let universal_verifier_params = universal_kzg_parameters.verifier_params();
    let verifier_tau_in_g2 = universal_kzg_parameters.s_g2().into();

    let certificate_commitment_parameters = {
        let mut certificate_commitment_parameters = universal_kzg_parameters.clone();
        if CERTIFICATE_CIRCUIT_DEGREE < shared_srs_degree {
            certificate_commitment_parameters.downsize(CERTIFICATE_CIRCUIT_DEGREE);
        }
        certificate_commitment_parameters
    };

    let recursive_commitment_parameters = {
        let mut recursive_commitment_parameters = universal_kzg_parameters.clone();
        if RECURSIVE_CIRCUIT_DEGREE < shared_srs_degree {
            recursive_commitment_parameters.downsize(RECURSIVE_CIRCUIT_DEGREE);
        }
        recursive_commitment_parameters
    };

    let certificate_verifying_key = zk_lib::setup_vk(
        &certificate_commitment_parameters,
        &setup.certificate_relation,
    );
    let default_ivc_circuit = IvcCircuit::unknown(certificate_verifying_key.vk());
    let recursive_verifying_key = keygen_vk_with_k(
        &recursive_commitment_parameters,
        &default_ivc_circuit,
        RECURSIVE_CIRCUIT_DEGREE,
    )
    .expect("recursive verifying key generation should not fail");

    let (certificate_fixed_bases, certificate_fixed_base_names) =
        fixed_bases_and_names(CERT_VK_NAME, certificate_verifying_key.vk());
    let (recursive_fixed_bases, recursive_fixed_base_names) =
        fixed_bases_and_names(IVC_ONE_NAME, &recursive_verifying_key);
    let mut combined_fixed_bases = certificate_fixed_bases.clone();
    combined_fixed_bases.extend(recursive_fixed_bases.clone());
    let fixed_base_names = certificate_fixed_base_names
        .into_iter()
        .chain(recursive_fixed_base_names)
        .collect::<Vec<_>>();

    let global = Global::new(
        setup.genesis_message,
        setup.genesis_verification_key.clone(),
        certificate_verifying_key.vk(),
        &recursive_verifying_key,
    );

    RecursiveMockProverSetup {
        certificate_commitment_parameters,
        certificate_verifying_key,
        recursive_verifying_key,
        global,
        recursive_fixed_bases,
        combined_fixed_bases,
        universal_verifier_params,
        verifier_tau_in_g2,
        trivial_accumulator: trivial_acc(&fixed_base_names),
    }
}

/// Runs `MockProver` on the recursive circuit and asserts that all constraints hold.
pub(crate) fn assert_recursive_mock_prover_accepts(circuit: IvcCircuit, public_inputs: Vec<F>) {
    let prover = MockProver::run(K, &circuit, vec![vec![], public_inputs])
        .expect("recursive MockProver setup should succeed");
    assert!(
        prover.verify().is_ok(),
        "recursive MockProver should accept the provided circuit and public inputs"
    );
}

/// Prepares the stored previous recursive proof and returns its accumulator contribution.
///
/// This mirrors the first half of the normal recursive-step asset generation:
/// the previous recursive proof is verified off-circuit against the stored
/// state/accumulator public inputs, then reduced to the accumulator term that
/// the next step folds in-circuit.
pub(crate) fn prepare_previous_recursive_proof_accumulator(
    setup: &RecursiveMockProverSetup,
    recursive_chain_state: &RecursiveChainStateAsset,
) -> Accumulator<crate::circuits::halo2_ivc::S> {
    let previous_public_inputs = [
        setup.global.as_public_input(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();

    let previous_dual_msm = verify_and_prepare_poseidon_recursive_proof(
        &setup.recursive_verifying_key,
        &recursive_chain_state.proof,
        &previous_public_inputs,
    );
    assert!(
        previous_dual_msm.clone().check(&setup.universal_verifier_params),
        "stored previous recursive proof should verify before the normal step check"
    );

    let mut previous_proof_accumulator: Accumulator<crate::circuits::halo2_ivc::S> =
        previous_dual_msm.into();
    previous_proof_accumulator.extract_fixed_bases(&setup.recursive_fixed_bases);
    previous_proof_accumulator.collapse();
    previous_proof_accumulator
}

/// Computes the expected next accumulator for one normal non-genesis recursive step.
///
/// This folds the stored previous accumulator, the fresh certificate
/// contribution, and the prepared previous recursive-proof contribution exactly
/// as the generator does for `recursive_step_output`.
pub(crate) fn compute_expected_next_accumulator(
    setup: &RecursiveMockProverSetup,
    recursive_chain_state: &RecursiveChainStateAsset,
    certificate_accumulator: Accumulator<crate::circuits::halo2_ivc::S>,
) -> Accumulator<crate::circuits::halo2_ivc::S> {
    let previous_proof_accumulator =
        prepare_previous_recursive_proof_accumulator(setup, recursive_chain_state);

    let mut next_accumulator = Accumulator::accumulate(&[
        recursive_chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();
    assert!(
        next_accumulator.check(&setup.verifier_tau_in_g2, &setup.combined_fixed_bases),
        "expected next accumulator should verify before the normal step check"
    );
    next_accumulator
}

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
