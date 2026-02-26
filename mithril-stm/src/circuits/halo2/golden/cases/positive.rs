use ff::Field;

use crate::circuits::halo2::golden::helpers::{
    LeafSelector, StmCircuitScenario, build_witness_with_fixed_signer, build_witness_with_indices,
    create_default_merkle_tree, create_merkle_tree_with_leaf_selector, prove_and_verify_result,
    run_stm_circuit_case, run_stm_circuit_case_default, setup_stm_circuit_env,
    LOTTERIES_PER_QUORUM,
};
use crate::circuits::halo2::types::SignedMessageWithoutPrefix;

#[test]
fn baseline_valid() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)
        .expect("baseline_valid setup should succeed");
}

// Expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn medium_valid() {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)
        .expect("medium_valid setup should succeed");
}

// Extremely expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn large_valid() {
    const K: u32 = 21;
    const QUORUM: u32 = 1024;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)
        .expect("large_valid setup should succeed");
}

#[test]
fn message_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case(
        current_function!(),
        K,
        QUORUM,
        SignedMessageWithoutPrefix::ZERO,
    )
    .expect("message_zero setup should succeed");
}

#[test]
fn message_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -SignedMessageWithoutPrefix::ONE; // p - 1
    run_stm_circuit_case(current_function!(), K, QUORUM, msg_max)
        .expect("message_max setup should succeed");
}

#[test]
fn indices_from_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM, QUORUM * LOTTERIES_PER_QUORUM)
        .expect("indices_from_zero env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("indices_from_zero tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let indices = vec![0, 1, 2];
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("indices_from_zero witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("indices_from_zero prove/verify should succeed");
}

#[test]
fn indices_to_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM, QUORUM * LOTTERIES_PER_QUORUM)
        .expect("indices_to_max env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("indices_to_max tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    assert!(m >= QUORUM, "num_lotteries must be >= quorum");
    let start = m - QUORUM;
    let indices = (start..m).collect::<Vec<u32>>();
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("indices_to_max witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("indices_to_max prove/verify should succeed");
}

#[test]
fn merkle_path_all_right() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM, QUORUM * LOTTERIES_PER_QUORUM)
        .expect("merkle_path_all_right env setup should succeed");
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, rightmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::RightMost, target)
            .expect("merkle_path_all_right tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![4, 12, 25];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_fixed_signer(&merkle_tree, rightmost_index, merkle_root, msg, &indices)
            .expect("merkle_path_all_right witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)
        .expect("merkle_path_all_right prove/verify should succeed");
}

// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn merkle_path_all_left() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM, QUORUM * LOTTERIES_PER_QUORUM)
        .expect("merkle_path_all_left env setup should succeed");
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, leftmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::LeftMost, target)
            .expect("merkle_path_all_left tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![5, 13, 21];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_fixed_signer(&merkle_tree, leftmost_index, merkle_root, msg, &indices)
            .expect("merkle_path_all_left witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)
        .expect("merkle_path_all_left prove/verify should succeed");
}
