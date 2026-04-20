use std::collections::BTreeMap;

use midnight_circuits::types::Instantiable;
use midnight_curves::Bls12;
use midnight_curves::pairing::Engine;
use midnight_proofs::{
    dev::MockProver,
    poly::kzg::{
        KZGCommitmentScheme,
        params::{ParamsKZG, ParamsVerifierKZG},
    },
};
use midnight_zk_stdlib::MidnightVK;

use crate::circuits::halo2_ivc::tests::golden::{
    asset_readers::RecursiveChainStateAsset,
    generators::{
        AssetGenerationSetup, build_recursive_fixed_bases, build_recursive_global,
        build_shared_recursive_context, certificate_public_inputs_for_step,
    },
};
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, E, F, K, S, VerifyingKey,
    circuit::IvcCircuit,
    state::{Global, State, trivial_acc},
};

pub(crate) use crate::circuits::halo2_ivc::tests::golden::generators::{
    verify_and_prepare_blake2b_ivc as verify_and_prepare_blake2b_recursive_proof,
    verify_and_prepare_poseidon_ivc as verify_and_prepare_poseidon_recursive_proof,
};

/// Shared recursive context reused by MockProver-based golden cases.
pub(crate) struct RecursiveMockProverSetup {
    /// Certificate-sized commitment parameters used by the golden checks.
    pub(crate) certificate_commitment_parameters: ParamsKZG<Bls12>,
    /// Certificate verifying key reused by the golden checks.
    pub(crate) certificate_verifying_key: MidnightVK,
    /// Recursive verifying key reused by the golden checks.
    pub(crate) recursive_verifying_key: VerifyingKey<F, KZGCommitmentScheme<E>>,
    /// Shared recursive global inputs.
    pub(crate) global: Global,
    /// Fixed bases extracted from the certificate verifying key.
    pub(crate) certificate_fixed_bases: BTreeMap<String, C>,
    /// Fixed bases extracted from the recursive verifying key.
    pub(crate) recursive_fixed_bases: BTreeMap<String, C>,
    /// Union of certificate and recursive fixed bases.
    pub(crate) combined_fixed_bases: BTreeMap<String, C>,
    /// Verifier-side view of the shared universal KZG parameters.
    pub(crate) universal_verifier_params: ParamsVerifierKZG<E>,
    /// The verifier-side `s_g2` element used for accumulator checks.
    pub(crate) verifier_tau_in_g2: <E as Engine>::G2Affine,
    /// Trivial accumulator used by the recursive base case.
    pub(crate) trivial_accumulator: Accumulator<S>,
}

/// Builds the shared recursive circuit context needed by MockProver-based golden tests.
///
/// This mirrors the verifier-side setup used by the asset generators, but keeps
/// the logic local to the golden helper layer so the base-case test can be
/// reviewed independently from the generator code.
pub(crate) fn build_recursive_mock_prover_setup(
    setup: &AssetGenerationSetup,
) -> RecursiveMockProverSetup {
    let context = build_shared_recursive_context(setup);
    let verifier_tau_in_g2 = context.universal_kzg_parameters.s_g2().into();
    let (certificate_fixed_bases, recursive_fixed_bases, combined_fixed_bases) =
        build_recursive_fixed_bases(
            &context.certificate_verifying_key,
            &context.recursive_verifying_key,
        );
    let fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();

    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    RecursiveMockProverSetup {
        certificate_commitment_parameters: context.certificate_commitment_parameters,
        certificate_verifying_key: context.certificate_verifying_key,
        recursive_verifying_key: context.recursive_verifying_key,
        global,
        certificate_fixed_bases,
        recursive_fixed_bases,
        combined_fixed_bases,
        universal_verifier_params: context.universal_verifier_params,
        verifier_tau_in_g2,
        trivial_accumulator: trivial_acc(&fixed_base_names),
    }
}

/// Runs `MockProver` on the recursive circuit and asserts that all constraints hold.
pub(crate) fn assert_recursive_mock_prover_accepts(circuit: IvcCircuit, public_inputs: Vec<F>) {
    let prover = MockProver::run(K, &circuit, vec![vec![], public_inputs])
        .expect("recursive MockProver setup should succeed");
    prover
        .verify()
        .expect("recursive MockProver should accept the provided circuit and public inputs");
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
) -> Accumulator<S> {
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

    let mut previous_proof_accumulator: Accumulator<S> = previous_dual_msm.into();
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
    certificate_accumulator: Accumulator<S>,
) -> Accumulator<S> {
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

/// Prepares the stored certificate proof used by the committed recursive step.
///
/// The certificate proof is verified against the public inputs implied by the
/// stored chain transition:
/// - `merkle_root` comes from the previous state's `next_merkle_root`
/// - `message` is the committed `next_state.msg`
pub(crate) fn prepare_stored_step_certificate_accumulator(
    setup: &RecursiveMockProverSetup,
    recursive_chain_state: &RecursiveChainStateAsset,
    expected_next_state: &State,
    certificate_proof: &[u8],
) -> Accumulator<S> {
    let certificate_public_inputs =
        certificate_public_inputs_for_step(&recursive_chain_state.state, expected_next_state);

    let certificate_dual_msm = verify_and_prepare_poseidon_recursive_proof(
        setup.certificate_verifying_key.vk(),
        certificate_proof,
        &certificate_public_inputs,
    );
    assert!(
        certificate_dual_msm
            .clone()
            .check(&setup.certificate_commitment_parameters.verifier_params()),
        "stored step certificate proof should verify before the chained-flow check"
    );

    let mut certificate_accumulator: Accumulator<S> = certificate_dual_msm.into();
    certificate_accumulator.extract_fixed_bases(&setup.certificate_fixed_bases);
    certificate_accumulator.collapse();
    certificate_accumulator
}

/// Recomputes the exact next accumulator from stored step artifacts.
pub(crate) fn compute_exact_next_accumulator_from_assets(
    setup: &RecursiveMockProverSetup,
    recursive_chain_state: &RecursiveChainStateAsset,
    expected_next_state: &State,
    certificate_proof: &[u8],
) -> Accumulator<S> {
    let certificate_accumulator = prepare_stored_step_certificate_accumulator(
        setup,
        recursive_chain_state,
        expected_next_state,
        certificate_proof,
    );
    compute_expected_next_accumulator(setup, recursive_chain_state, certificate_accumulator)
}
