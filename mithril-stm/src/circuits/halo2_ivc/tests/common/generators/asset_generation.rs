use std::time::Instant;

use midnight_circuits::types::Instantiable;
use midnight_proofs::{plonk::ProvingKey, poly::kzg::KZGCommitmentScheme};
use rand_core::OsRng;

use super::proofs::{
    prove_blake2b_ivc, prove_poseidon_ivc, verify_and_prepare_blake2b_ivc,
    verify_and_prepare_poseidon_ivc,
};
use super::setup::{
    AssetGenerationSetup, AssetPaths, GENESIS_EPOCH, INITIAL_CHAIN_LENGTH,
    build_recursive_fixed_bases, build_recursive_global, build_recursive_proving_key,
    build_shared_recursive_context,
};
use super::transitions::{
    build_genesis_base_case_next_state, build_genesis_base_case_witness,
    build_next_certificate_asset_data, build_same_epoch_certificate_asset_data,
};
use crate::circuits::halo2_ivc::tests::common::asset_readers::{
    RecursiveChainStateAsset, RecursiveStepOutputAsset, VerificationContextAsset,
    load_recursive_chain_state_asset, store_recursive_chain_state_asset,
    store_recursive_step_output_asset, store_verification_context_asset,
};
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, C, E, F, S,
    circuit::IvcCircuit,
    state::{Global, State, trivial_acc},
};

struct CertificateChainArtifacts {
    certificate_proofs: Vec<Vec<u8>>,
    certificate_accumulators: Vec<Accumulator<S>>,
    recursive_next_states: Vec<State>,
    recursive_witnesses: Vec<crate::circuits::halo2_ivc::state::Witness>,
}

struct RecursiveChainSnapshot {
    state: State,
    proof: Vec<u8>,
    accumulator: Accumulator<S>,
}

struct NextRecursiveStepInputs {
    certificate_proof: Vec<u8>,
    next_state: State,
    recursive_witness: crate::circuits::halo2_ivc::state::Witness,
    next_accumulator: Accumulator<S>,
}

fn build_certificate_chain_artifacts(
    setup: &AssetGenerationSetup,
    context: &super::setup::SharedRecursiveContext,
    recursive_fixed_base_names: &[String],
) -> CertificateChainArtifacts {
    let mut certificate_proofs = vec![vec![]];
    let mut certificate_accumulators = vec![trivial_acc(recursive_fixed_base_names)];
    let mut recursive_next_states = vec![build_genesis_base_case_next_state(setup, GENESIS_EPOCH)];
    let mut recursive_witnesses = vec![build_genesis_base_case_witness(setup)];
    let mut certificate_random_generator = OsRng;

    for step in 1..=INITIAL_CHAIN_LENGTH {
        println!(
            "generate_recursive_chain_state: certificate proof {step}/{INITIAL_CHAIN_LENGTH} starting"
        );
        let certificate_step_start = Instant::now();
        let previous_state = recursive_next_states
            .last()
            .expect("recursive chain should always have a previous state")
            .clone();
        let (certificate_proof, certificate_accumulator, next_state, recursive_witness) =
            build_next_certificate_asset_data(
                setup,
                &context.certificate_commitment_parameters,
                &setup.certificate_relation,
                &context.certificate_verifying_key,
                &previous_state,
                &mut certificate_random_generator,
            );

        recursive_next_states.push(next_state);
        recursive_witnesses.push(recursive_witness);

        println!(
            "generate_recursive_chain_state: certificate proof {step}/{INITIAL_CHAIN_LENGTH} done in {:?}",
            certificate_step_start.elapsed()
        );

        certificate_proofs.push(certificate_proof);
        certificate_accumulators.push(certificate_accumulator);
    }

    CertificateChainArtifacts {
        certificate_proofs,
        certificate_accumulators,
        recursive_next_states,
        recursive_witnesses,
    }
}

fn build_recursive_chain_snapshot(
    setup: &AssetGenerationSetup,
    context: &super::setup::SharedRecursiveContext,
    global: &Global,
    recursive_proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    recursive_fixed_bases: &std::collections::BTreeMap<String, C>,
    combined_fixed_bases: &std::collections::BTreeMap<String, C>,
    artifacts: CertificateChainArtifacts,
) -> RecursiveChainSnapshot {
    let combined_fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();
    let mut current_state = State::genesis();
    let mut recursive_proof = vec![];
    let mut current_accumulator = trivial_acc(&combined_fixed_base_names);
    let mut next_accumulator = current_accumulator.clone();
    let mut recursive_random_generator = OsRng;

    for i in 0..=INITIAL_CHAIN_LENGTH {
        println!(
            "generate_recursive_chain_state: recursive proof {}/{} starting",
            i + 1,
            INITIAL_CHAIN_LENGTH + 1
        );
        let recursive_step_start = Instant::now();
        let circuit = IvcCircuit::new(
            global.clone(),
            current_state.clone(),
            artifacts.recursive_witnesses[i].clone(),
            artifacts.certificate_proofs[i].clone(),
            recursive_proof.clone(),
            current_accumulator.clone(),
            context.certificate_verifying_key.vk(),
            &context.recursive_verifying_key,
        );

        let public_inputs = [
            global.as_public_input(),
            artifacts.recursive_next_states[i].as_public_input(),
            AssignedAccumulator::as_public_input(&next_accumulator),
        ]
        .concat();

        let proof = prove_poseidon_ivc(
            &context.recursive_commitment_parameters,
            recursive_proving_key,
            &circuit,
            &public_inputs,
            &mut recursive_random_generator,
        );
        let dual_msm = verify_and_prepare_poseidon_ivc(
            &context.recursive_verifying_key,
            &proof,
            &public_inputs,
        );
        assert!(dual_msm.clone().check(&context.universal_verifier_params));

        let mut proof_accumulator: Accumulator<S> = dual_msm.into();
        proof_accumulator.extract_fixed_bases(recursive_fixed_bases);
        proof_accumulator.collapse();

        println!(
            "generate_recursive_chain_state: recursive proof {}/{} done in {:?}",
            i + 1,
            INITIAL_CHAIN_LENGTH + 1,
            recursive_step_start.elapsed()
        );

        current_state = artifacts.recursive_next_states[i].clone();
        current_accumulator = next_accumulator.clone();
        recursive_proof = proof;

        if i < INITIAL_CHAIN_LENGTH {
            let mut accumulated_accumulator = Accumulator::accumulate(&[
                next_accumulator.clone(),
                artifacts.certificate_accumulators[i + 1].clone(),
                proof_accumulator,
            ]);
            accumulated_accumulator.collapse();
            assert!(
                accumulated_accumulator.check(
                    &context.universal_kzg_parameters.s_g2().into(),
                    combined_fixed_bases,
                ),
                "recursive accumulator verification failed at step {i}"
            );
            next_accumulator = accumulated_accumulator;
        }
    }

    assert_eq!(
        current_state.next_merkle_root, setup.genesis_next_merkle_root,
        "recursive_chain_state writer is about to persist a next_merkle_root that does not match setup"
    );

    RecursiveChainSnapshot {
        state: current_state,
        proof: recursive_proof,
        accumulator: current_accumulator,
    }
}

fn store_recursive_chain_snapshot(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
    global: &Global,
    snapshot: &RecursiveChainSnapshot,
) {
    println!(
        "generate_recursive_chain_state: writing asset -> {}",
        paths.recursive_chain_state.display()
    );
    let asset = RecursiveChainStateAsset {
        global_field_elements: global.as_public_input(),
        state: snapshot.state.clone(),
        proof: snapshot.proof.clone(),
        accumulator: snapshot.accumulator.clone(),
    };
    store_recursive_chain_state_asset(&paths.recursive_chain_state, &asset)
        .expect("failed to write recursive_chain_state asset");

    let reloaded = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to reload recursive_chain_state asset after writing");
    assert_eq!(
        reloaded.state.next_merkle_root, setup.genesis_next_merkle_root,
        "reloaded recursive_chain_state next_merkle_root does not match setup"
    );
}

fn build_next_recursive_step_inputs(
    setup: &AssetGenerationSetup,
    context: &super::setup::SharedRecursiveContext,
    global: &Global,
    recursive_chain_state: &RecursiveChainStateAsset,
    recursive_fixed_bases: &std::collections::BTreeMap<String, C>,
    combined_fixed_bases: &std::collections::BTreeMap<String, C>,
) -> NextRecursiveStepInputs {
    let mut recursive_step_output_random_generator = OsRng;
    println!("generate_recursive_step_output: building next certificate");
    let certificate_start = Instant::now();
    let (certificate_proof, certificate_accumulator, next_state, recursive_witness) =
        build_next_certificate_asset_data(
            setup,
            &context.certificate_commitment_parameters,
            &setup.certificate_relation,
            &context.certificate_verifying_key,
            &recursive_chain_state.state,
            &mut recursive_step_output_random_generator,
        );
    println!(
        "generate_recursive_step_output: next certificate done in {:?}",
        certificate_start.elapsed()
    );

    let previous_public_inputs = [
        global.as_public_input(),
        recursive_chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&recursive_chain_state.accumulator),
    ]
    .concat();
    let previous_dual_msm = verify_and_prepare_poseidon_ivc(
        &context.recursive_verifying_key,
        &recursive_chain_state.proof,
        &previous_public_inputs,
    );
    assert!(previous_dual_msm.clone().check(&context.universal_verifier_params));
    let mut previous_proof_accumulator: Accumulator<S> = previous_dual_msm.into();
    previous_proof_accumulator.extract_fixed_bases(recursive_fixed_bases);
    previous_proof_accumulator.collapse();

    let mut next_accumulator = Accumulator::accumulate(&[
        recursive_chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();
    assert!(
        next_accumulator.check(
            &context.universal_kzg_parameters.s_g2().into(),
            combined_fixed_bases,
        ),
        "next accumulator check failed"
    );
    println!("generate_recursive_step_output: next accumulator computed");

    NextRecursiveStepInputs {
        certificate_proof,
        next_state,
        recursive_witness,
        next_accumulator,
    }
}

fn build_recursive_step_output_proof(
    context: &super::setup::SharedRecursiveContext,
    global: &Global,
    recursive_proving_key: &ProvingKey<F, KZGCommitmentScheme<E>>,
    recursive_chain_state: &RecursiveChainStateAsset,
    next_step_inputs: &NextRecursiveStepInputs,
) -> Vec<u8> {
    let mut recursive_step_output_random_generator = OsRng;
    let circuit = IvcCircuit::new(
        global.clone(),
        recursive_chain_state.state.clone(),
        next_step_inputs.recursive_witness.clone(),
        next_step_inputs.certificate_proof.clone(),
        recursive_chain_state.proof.clone(),
        recursive_chain_state.accumulator.clone(),
        context.certificate_verifying_key.vk(),
        &context.recursive_verifying_key,
    );
    let public_inputs = [
        global.as_public_input(),
        next_step_inputs.next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_step_inputs.next_accumulator),
    ]
    .concat();

    println!("generate_recursive_step_output: final blake2b recursive proof starting");
    let final_proof_start = Instant::now();
    let final_proof = prove_blake2b_ivc(
        &context.recursive_commitment_parameters,
        recursive_proving_key,
        &circuit,
        &public_inputs,
        &mut recursive_step_output_random_generator,
    );
    let final_dual_msm = verify_and_prepare_blake2b_ivc(
        &context.recursive_verifying_key,
        &final_proof,
        &public_inputs,
    );
    assert!(final_dual_msm.check(&context.universal_verifier_params));
    println!(
        "generate_recursive_step_output: final blake2b recursive proof done in {:?}",
        final_proof_start.elapsed()
    );
    final_proof
}

fn store_recursive_step_output(
    paths: &AssetPaths,
    next_step_inputs: NextRecursiveStepInputs,
    proof: Vec<u8>,
) {
    println!(
        "generate_recursive_step_output: writing asset -> {}",
        paths.recursive_step_output.display()
    );
    let asset = RecursiveStepOutputAsset {
        proof,
        next_accumulator: next_step_inputs.next_accumulator,
        next_state: next_step_inputs.next_state,
        certificate_proof: next_step_inputs.certificate_proof,
    };
    store_recursive_step_output_asset(&paths.recursive_step_output, &asset)
        .expect("failed to write recursive_step_output asset");
}

/// Generates and writes the stored recursive chain snapshot asset.
pub(crate) fn generate_recursive_chain_state_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_recursive_chain_state: start -> {}",
        paths.recursive_chain_state.display()
    );
    let total_start = Instant::now();

    let context = build_shared_recursive_context(setup);
    let (_, recursive_fixed_bases, combined_fixed_bases) = build_recursive_fixed_bases(
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );
    let recursive_fixed_base_names = recursive_fixed_bases.keys().cloned().collect::<Vec<_>>();
    let recursive_proving_key = build_recursive_proving_key(&context);
    println!("generate_recursive_chain_state: shared recursive context ready");
    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );
    let artifacts = build_certificate_chain_artifacts(setup, &context, &recursive_fixed_base_names);
    let snapshot = build_recursive_chain_snapshot(
        setup,
        &context,
        &global,
        &recursive_proving_key,
        &recursive_fixed_bases,
        &combined_fixed_bases,
        artifacts,
    );
    store_recursive_chain_snapshot(setup, paths, &global, &snapshot);
    println!(
        "generate_recursive_chain_state: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the static verifier-side asset bundle.
pub(crate) fn generate_verification_context_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_verification_context: start -> {}",
        paths.verification_context.display()
    );
    let total_start = Instant::now();
    let context = build_shared_recursive_context(setup);
    println!("generate_verification_context: certificate and recursive verifying keys ready");

    let (_, _, combined_fixed_bases) = build_recursive_fixed_bases(
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    let asset = VerificationContextAsset {
        global_field_elements: global.as_public_input(),
        recursive_verifying_key: context.recursive_verifying_key.clone(),
        combined_fixed_bases,
        verifier_params: context.universal_verifier_params,
        verifier_tau_in_g2: context.universal_kzg_parameters.s_g2().into(),
    };
    store_verification_context_asset(&paths.verification_context, &asset)
        .expect("failed to write verification_context asset");
    println!(
        "generate_verification_context: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the asset produced by one more recursive step.
pub(crate) fn generate_recursive_step_output_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_recursive_step_output: start -> {}",
        paths.recursive_step_output.display()
    );
    let total_start = Instant::now();
    println!(
        "generate_recursive_step_output: loading recursive chain state <- {}",
        paths.recursive_chain_state.display()
    );
    let recursive_chain_state = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to load recursive_chain_state asset");

    let context = build_shared_recursive_context(setup);
    let recursive_proving_key = build_recursive_proving_key(&context);
    println!("generate_recursive_step_output: certificate and recursive keys ready");

    let (_, recursive_fixed_bases, combined_fixed_bases) = build_recursive_fixed_bases(
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );
    let next_step_inputs = build_next_recursive_step_inputs(
        setup,
        &context,
        &global,
        &recursive_chain_state,
        &recursive_fixed_bases,
        &combined_fixed_bases,
    );
    let final_proof = build_recursive_step_output_proof(
        &context,
        &global,
        &recursive_proving_key,
        &recursive_chain_state,
        &next_step_inputs,
    );
    store_recursive_step_output(paths, next_step_inputs, final_proof);
    println!(
        "generate_recursive_step_output: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the genesis step output asset (one Blake2b IVC proof
/// from the genesis base case, with no prior certificate or IVC proof).
pub(crate) fn generate_genesis_step_output_asset(setup: &AssetGenerationSetup, paths: &AssetPaths) {
    println!(
        "generate_genesis_step_output: start -> {}",
        paths.genesis_step_output.display()
    );
    let total_start = Instant::now();

    let context = build_shared_recursive_context(setup);
    let (_, _, combined_fixed_bases) = build_recursive_fixed_bases(
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );
    let combined_fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();
    let recursive_proving_key = build_recursive_proving_key(&context);
    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    let genesis_witness = build_genesis_base_case_witness(setup);
    let genesis_next_state = build_genesis_base_case_next_state(setup, GENESIS_EPOCH);
    let current_accumulator = trivial_acc(&combined_fixed_base_names);
    let next_accumulator = current_accumulator.clone();

    let circuit = IvcCircuit::new(
        global.clone(),
        State::genesis(),
        genesis_witness,
        vec![],
        vec![],
        current_accumulator,
        context.certificate_verifying_key.vk(),
        &context.recursive_verifying_key,
    );

    let public_inputs = [
        global.as_public_input(),
        genesis_next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_accumulator),
    ]
    .concat();

    let mut rng = OsRng;
    println!("generate_genesis_step_output: proving Blake2b");
    let proof_start = Instant::now();
    let proof = prove_blake2b_ivc(
        &context.recursive_commitment_parameters,
        &recursive_proving_key,
        &circuit,
        &public_inputs,
        &mut rng,
    );
    let dual_msm =
        verify_and_prepare_blake2b_ivc(&context.recursive_verifying_key, &proof, &public_inputs);
    assert!(
        dual_msm.check(&context.universal_verifier_params),
        "genesis step proof verification failed"
    );
    println!(
        "generate_genesis_step_output: proven in {:?}",
        proof_start.elapsed()
    );

    println!(
        "generate_genesis_step_output: writing asset -> {}",
        paths.genesis_step_output.display()
    );
    let asset = RecursiveStepOutputAsset {
        proof,
        next_accumulator,
        next_state: genesis_next_state,
        certificate_proof: vec![],
    };
    store_recursive_step_output_asset(&paths.genesis_step_output, &asset)
        .expect("failed to write genesis_step_output asset");
    println!(
        "generate_genesis_step_output: done in {:?}",
        total_start.elapsed()
    );
}

/// Generates and writes the same-epoch step output asset (one Blake2b IVC
/// proof extending the stored chain state with a same-epoch certificate).
pub(crate) fn generate_same_epoch_step_output_asset(
    setup: &AssetGenerationSetup,
    paths: &AssetPaths,
) {
    println!(
        "generate_same_epoch_step_output: start -> {}",
        paths.same_epoch_step_output.display()
    );
    let total_start = Instant::now();

    let context = build_shared_recursive_context(setup);
    let (_, recursive_fixed_bases, combined_fixed_bases) = build_recursive_fixed_bases(
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );
    let recursive_proving_key = build_recursive_proving_key(&context);
    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    println!(
        "generate_same_epoch_step_output: loading chain state <- {}",
        paths.recursive_chain_state.display()
    );
    let chain_state = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to load recursive_chain_state asset");

    let mut rng = OsRng;
    println!("generate_same_epoch_step_output: building same-epoch certificate");
    let cert_start = Instant::now();
    let (certificate_proof, certificate_accumulator, next_state, ivc_witness) =
        build_same_epoch_certificate_asset_data(
            setup,
            &context.certificate_commitment_parameters,
            &setup.certificate_relation,
            &context.certificate_verifying_key,
            &chain_state.state,
            &mut rng,
        );
    println!(
        "generate_same_epoch_step_output: certificate done in {:?}",
        cert_start.elapsed()
    );

    let previous_public_inputs = [
        global.as_public_input(),
        chain_state.state.as_public_input(),
        AssignedAccumulator::as_public_input(&chain_state.accumulator),
    ]
    .concat();
    let previous_dual_msm = verify_and_prepare_poseidon_ivc(
        &context.recursive_verifying_key,
        &chain_state.proof,
        &previous_public_inputs,
    );
    assert!(
        previous_dual_msm.clone().check(&context.universal_verifier_params),
        "previous chain state proof verification failed"
    );
    let mut previous_proof_accumulator: Accumulator<S> = previous_dual_msm.into();
    previous_proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
    previous_proof_accumulator.collapse();

    let mut next_accumulator = Accumulator::accumulate(&[
        chain_state.accumulator.clone(),
        certificate_accumulator,
        previous_proof_accumulator,
    ]);
    next_accumulator.collapse();
    assert!(
        next_accumulator.check(
            &context.universal_kzg_parameters.s_g2().into(),
            &combined_fixed_bases,
        ),
        "same-epoch next accumulator check failed"
    );

    let circuit = IvcCircuit::new(
        global.clone(),
        chain_state.state.clone(),
        ivc_witness,
        certificate_proof.clone(),
        chain_state.proof.clone(),
        chain_state.accumulator.clone(),
        context.certificate_verifying_key.vk(),
        &context.recursive_verifying_key,
    );

    let public_inputs = [
        global.as_public_input(),
        next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_accumulator),
    ]
    .concat();

    println!("generate_same_epoch_step_output: proving Blake2b");
    let proof_start = Instant::now();
    let proof = prove_blake2b_ivc(
        &context.recursive_commitment_parameters,
        &recursive_proving_key,
        &circuit,
        &public_inputs,
        &mut rng,
    );
    let dual_msm =
        verify_and_prepare_blake2b_ivc(&context.recursive_verifying_key, &proof, &public_inputs);
    assert!(
        dual_msm.check(&context.universal_verifier_params),
        "same-epoch step proof verification failed"
    );
    println!(
        "generate_same_epoch_step_output: proven in {:?}",
        proof_start.elapsed()
    );

    println!(
        "generate_same_epoch_step_output: writing asset -> {}",
        paths.same_epoch_step_output.display()
    );
    let asset = RecursiveStepOutputAsset {
        proof,
        next_accumulator,
        next_state,
        certificate_proof,
    };
    store_recursive_step_output_asset(&paths.same_epoch_step_output, &asset)
        .expect("failed to write same_epoch_step_output asset");
    println!(
        "generate_same_epoch_step_output: done in {:?}",
        total_start.elapsed()
    );
}

// These ignored tests are manual asset-generation entrypoints for the committed
// golden assets. They are intentionally excluded from normal test runs because
// they rewrite binary files rather than asserting behavior.
#[test]
#[ignore]
fn generate_verification_context_only() {
    use super::setup::{AssetPaths, build_asset_generation_setup};
    generate_verification_context_asset(&build_asset_generation_setup(), &AssetPaths::default());
}

#[test]
#[ignore]
fn generate_recursive_chain_state_only() {
    use super::setup::{AssetPaths, build_asset_generation_setup};
    generate_recursive_chain_state_asset(&build_asset_generation_setup(), &AssetPaths::default());
}

#[test]
#[ignore]
fn generate_recursive_step_output_only() {
    use super::setup::{AssetPaths, build_asset_generation_setup};
    generate_recursive_step_output_asset(&build_asset_generation_setup(), &AssetPaths::default());
}

#[test]
#[ignore]
fn generate_genesis_step_output_only() {
    use super::setup::{AssetPaths, build_asset_generation_setup};
    generate_genesis_step_output_asset(&build_asset_generation_setup(), &AssetPaths::default());
}

#[test]
#[ignore]
fn generate_same_epoch_step_output_only() {
    use super::setup::{AssetPaths, build_asset_generation_setup};
    generate_same_epoch_step_output_asset(&build_asset_generation_setup(), &AssetPaths::default());
}
