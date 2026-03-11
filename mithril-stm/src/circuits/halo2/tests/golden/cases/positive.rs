use crate::LotteryIndex;
use crate::circuits::halo2::tests::golden::helpers::{
    LOTTERIES_PER_K, LeafSelector, StmCircuitScenario, build_witness_with_fixed_signer,
    build_witness_with_indices, create_default_merkle_tree, create_merkle_tree_with_leaf_selector,
    prove_and_verify_result, run_stm_circuit_case, run_stm_circuit_case_default,
    setup_stm_circuit_env,
};
use crate::circuits::halo2::types::SignedMessageWithoutPrefix;

#[test]
fn baseline_valid() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    run_stm_circuit_case_default(current_function!(), CIRCUIT_DEGREE, K)
        .expect("baseline_valid setup should succeed");
}

// Expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn medium_valid() {
    const CIRCUIT_DEGREE: u32 = 16;
    const K: u32 = 32;
    run_stm_circuit_case_default(current_function!(), CIRCUIT_DEGREE, K)
        .expect("medium_valid setup should succeed");
}

// Extremely expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn large_valid() {
    const CIRCUIT_DEGREE: u32 = 21;
    const K: u32 = 1024;
    run_stm_circuit_case_default(current_function!(), CIRCUIT_DEGREE, K)
        .expect("large_valid setup should succeed");
}

#[test]
fn message_zero() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    run_stm_circuit_case(
        current_function!(),
        CIRCUIT_DEGREE,
        K,
        SignedMessageWithoutPrefix::ZERO,
    )
    .expect("message_zero setup should succeed");
}

#[test]
fn message_max() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    let msg_max = -SignedMessageWithoutPrefix::ONE; // p - 1
    run_stm_circuit_case(current_function!(), CIRCUIT_DEGREE, K, msg_max)
        .expect("message_max setup should succeed");
}

#[test]
fn indices_from_zero() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    let message = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), CIRCUIT_DEGREE, K, K * LOTTERIES_PER_K)
        .expect("indices_from_zero env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("indices_from_zero tree creation should succeed");
    let merkle_tree_commitment = merkle_tree.root();
    let indices: Vec<LotteryIndex> = vec![0, 1, 2];
    let witness =
        build_witness_with_indices(&merkle_tree, merkle_tree_commitment, message, &indices)
            .expect("indices_from_zero witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_tree_commitment, message, witness);
    prove_and_verify_result(&env, scenario).expect("indices_from_zero prove/verify should succeed");
}

#[test]
fn indices_to_max() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    let message = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), CIRCUIT_DEGREE, K, K * LOTTERIES_PER_K)
        .expect("indices_to_max env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("indices_to_max tree creation should succeed");
    let merkle_tree_commitment = merkle_tree.root();
    let m = env.m();
    assert!(m >= K, "m must be >= k");
    let start = m - K;
    let indices = (start as LotteryIndex..m as LotteryIndex).collect::<Vec<LotteryIndex>>();
    let witness =
        build_witness_with_indices(&merkle_tree, merkle_tree_commitment, message, &indices)
            .expect("indices_to_max witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_tree_commitment, message, witness);
    prove_and_verify_result(&env, scenario).expect("indices_to_max prove/verify should succeed");
}

#[test]
fn merkle_path_all_right() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    let message = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), CIRCUIT_DEGREE, K, K * LOTTERIES_PER_K)
        .expect("merkle_path_all_right env setup should succeed");
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, rightmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::RightMost, target)
            .expect("merkle_path_all_right tree creation should succeed");

    let merkle_tree_commitment = merkle_tree.root();
    let m = env.m();
    let indices: Vec<LotteryIndex> = vec![4, 12, 25];
    assert!(indices.iter().all(|i| *i < m as LotteryIndex));
    let witness = build_witness_with_fixed_signer(
        &merkle_tree,
        rightmost_index,
        merkle_tree_commitment,
        message,
        &indices,
    )
    .expect("merkle_path_all_right witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_tree_commitment, message, witness);
    prove_and_verify_result(&env, scenario)
        .expect("merkle_path_all_right prove/verify should succeed");
}

// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn merkle_path_all_left() {
    const CIRCUIT_DEGREE: u32 = 13;
    const K: u32 = 3;
    let message = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), CIRCUIT_DEGREE, K, K * LOTTERIES_PER_K)
        .expect("merkle_path_all_left env setup should succeed");
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, leftmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::LeftMost, target)
            .expect("merkle_path_all_left tree creation should succeed");

    let merkle_tree_commitment = merkle_tree.root();
    let m = env.m();
    let indices: Vec<LotteryIndex> = vec![5, 13, 21];
    assert!(indices.iter().all(|i| *i < m as LotteryIndex));
    let witness = build_witness_with_fixed_signer(
        &merkle_tree,
        leftmost_index,
        merkle_tree_commitment,
        message,
        &indices,
    )
    .expect("merkle_path_all_left witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_tree_commitment, message, witness);
    prove_and_verify_result(&env, scenario)
        .expect("merkle_path_all_left prove/verify should succeed");
}
