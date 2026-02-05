use ff::Field;

use crate::circuits::halo2::golden::helpers::{
    LeafSelector, StmCircuitScenario, build_witness_with_fixed_signer, build_witness_with_indices,
    create_default_merkle_tree, create_merkle_tree_with_leaf_selector, prove_and_verify_result,
    run_stm_circuit_case, run_stm_circuit_case_default, setup_stm_circuit_env,
};
use crate::circuits::halo2::types::JubjubBase;

#[test]
fn baseline_valid() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case_default(current_function!(), K, QUORUM);
}

// Expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn medium_valid() {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_stm_circuit_case_default(current_function!(), K, QUORUM);
}

// Extremely expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn large_valid() {
    const K: u32 = 21;
    const QUORUM: u32 = 1024;
    run_stm_circuit_case_default(current_function!(), K, QUORUM);
}

#[test]
fn msg_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case(current_function!(), K, QUORUM, JubjubBase::ZERO);
}

#[test]
fn msg_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -JubjubBase::ONE; // p - 1
    run_stm_circuit_case(current_function!(), K, QUORUM, msg_max);
}

#[test]
fn indices_from_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let indices = vec![0, 1, 2];
    let witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

#[test]
fn indices_to_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    assert!(m >= QUORUM, "num_lotteries must be >= quorum");
    let start = m - QUORUM;
    let indices = (start..m).collect::<Vec<u32>>();
    let witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

#[test]
fn mp_all_right() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, rightmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::RightMost, target);

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![4, 12, 25];
    assert!(indices.iter().all(|i| *i < m));
    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        rightmost_index,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn mp_all_left() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, leftmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::LeftMost, target);

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![5, 13, 21];
    assert!(indices.iter().all(|i| *i < m));
    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        leftmost_index,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}
