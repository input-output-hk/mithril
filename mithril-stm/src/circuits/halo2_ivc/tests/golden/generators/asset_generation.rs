use std::time::Instant;

use midnight_circuits::types::Instantiable;
use rand_core::OsRng;

use crate::circuits::halo2_ivc::tests::golden::asset_readers::{
    RecursiveChainStateAsset, RecursiveStepOutputAsset, VerificationContextAsset,
    load_recursive_chain_state_asset, store_recursive_chain_state_asset,
    store_recursive_step_output_asset, store_verification_context_asset,
};
use crate::circuits::halo2_ivc::{
    Accumulator, AssignedAccumulator, S, circuit::IvcCircuit, state::State,
};

use super::proofs::{
    prove_blake2b_ivc, prove_poseidon_ivc, verify_and_prepare_blake2b_ivc,
    verify_and_prepare_poseidon_ivc,
};
use super::setup::{
    AssetGenerationSetup, AssetPaths, INITIAL_CHAIN_LENGTH, build_recursive_fixed_bases,
    build_recursive_global, build_recursive_proving_key, build_shared_recursive_context,
};
use super::transitions::{
    build_genesis_base_case_next_state, build_genesis_base_case_witness,
    build_next_certificate_asset_data,
};
use crate::circuits::halo2_ivc::state::trivial_acc;

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
    let mut certificate_proofs = vec![vec![]];
    let mut certificate_accumulators = vec![trivial_acc(&recursive_fixed_base_names)];

    let recursive_proving_key = build_recursive_proving_key(&context);
    println!("generate_recursive_chain_state: shared recursive context ready");

    let combined_fixed_base_names = combined_fixed_bases.keys().cloned().collect::<Vec<_>>();

    let global = build_recursive_global(
        setup,
        &context.certificate_verifying_key,
        &context.recursive_verifying_key,
    );

    let mut certificate_random_generator = OsRng;
    let mut recursive_next_states = vec![build_genesis_base_case_next_state(setup, 5u64)];
    let mut recursive_witnesses = vec![build_genesis_base_case_witness(setup)];

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
            recursive_witnesses[i].clone(),
            certificate_proofs[i].clone(),
            recursive_proof.clone(),
            current_accumulator.clone(),
            context.certificate_verifying_key.vk(),
            &context.recursive_verifying_key,
        );

        let public_inputs = [
            global.as_public_input(),
            recursive_next_states[i].as_public_input(),
            AssignedAccumulator::as_public_input(&next_accumulator),
        ]
        .concat();

        let proof = prove_poseidon_ivc(
            &context.recursive_commitment_parameters,
            &recursive_proving_key,
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
        proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
        proof_accumulator.collapse();

        println!(
            "generate_recursive_chain_state: recursive proof {}/{} done in {:?}",
            i + 1,
            INITIAL_CHAIN_LENGTH + 1,
            recursive_step_start.elapsed()
        );

        current_state = recursive_next_states[i].clone();
        current_accumulator = next_accumulator.clone();
        recursive_proof = proof;

        if i < INITIAL_CHAIN_LENGTH {
            let mut accumulated_accumulator = Accumulator::accumulate(&[
                next_accumulator.clone(),
                certificate_accumulators[i + 1].clone(),
                proof_accumulator,
            ]);
            accumulated_accumulator.collapse();
            assert!(
                accumulated_accumulator.check(
                    &context.universal_kzg_parameters.s_g2().into(),
                    &combined_fixed_bases,
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
    println!(
        "generate_recursive_chain_state: writing asset -> {}",
        paths.recursive_chain_state.display()
    );
    let asset = RecursiveChainStateAsset {
        global_field_elements: global.as_public_input(),
        state: current_state.clone(),
        proof: recursive_proof.clone(),
        accumulator: current_accumulator.clone(),
    };
    store_recursive_chain_state_asset(&paths.recursive_chain_state, &asset)
        .expect("failed to write recursive_chain_state asset");

    let reloaded = load_recursive_chain_state_asset(&paths.recursive_chain_state)
        .expect("failed to reload recursive_chain_state asset after writing");
    assert_eq!(
        reloaded.state.next_merkle_root, setup.genesis_next_merkle_root,
        "reloaded recursive_chain_state next_merkle_root does not match setup"
    );
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
    previous_proof_accumulator.extract_fixed_bases(&recursive_fixed_bases);
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
            &combined_fixed_bases,
        ),
        "next accumulator check failed"
    );
    println!("generate_recursive_step_output: next accumulator computed");

    let circuit = IvcCircuit::new(
        global.clone(),
        recursive_chain_state.state.clone(),
        recursive_witness,
        certificate_proof.clone(),
        recursive_chain_state.proof.clone(),
        recursive_chain_state.accumulator.clone(),
        context.certificate_verifying_key.vk(),
        &context.recursive_verifying_key,
    );
    let public_inputs = [
        global.as_public_input(),
        next_state.as_public_input(),
        AssignedAccumulator::as_public_input(&next_accumulator),
    ]
    .concat();

    println!("generate_recursive_step_output: final blake2b recursive proof starting");
    let final_proof_start = Instant::now();
    let final_proof = prove_blake2b_ivc(
        &context.recursive_commitment_parameters,
        &recursive_proving_key,
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

    println!(
        "generate_recursive_step_output: writing asset -> {}",
        paths.recursive_step_output.display()
    );
    let asset = RecursiveStepOutputAsset {
        proof: final_proof,
        next_accumulator,
        next_state,
        certificate_proof,
    };
    store_recursive_step_output_asset(&paths.recursive_step_output, &asset)
        .expect("failed to write recursive_step_output asset");
    println!(
        "generate_recursive_step_output: done in {:?}",
        total_start.elapsed()
    );
}
