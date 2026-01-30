use crate::circuits::halo2::golden::support::stm_case::{
    build_witness, build_witness_with_fixed_signer, build_witness_with_indices,
    create_default_merkle_tree, create_merkle_tree_with_leftmost_leaf,
    create_merkle_tree_with_rightmost_leaf, prove_and_verify_result, run_stm_case,
    run_stm_case_default, setup_stm_env, STMProofError, STMScenario,
};
use crate::circuits::halo2::types::JubjubBase;
use ff::Field;

#[test]
fn test_stm_baseline() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_case_default("small", K, QUORUM);
}

#[test]
fn test_stm_medium() {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_stm_case_default("medium", K, QUORUM);
}

#[test]
fn test_stm_message_zero() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_case("message_zero", K, QUORUM, JubjubBase::ZERO);
}

#[test]
fn test_stm_message_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -JubjubBase::ONE; // p - 1
    run_stm_case("message_max", K, QUORUM, msg_max);
}

#[test]
fn test_stm_min_strict_indices() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("min_strict_indices", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let indices = vec![0, 1, 2];
    let witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

#[test]
fn test_stm_max_strict_indices() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("max_strict_indices", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    assert!(m >= QUORUM, "num_lotteries must be >= quorum");
    let start = m - QUORUM;
    let indices = (start..m).collect::<Vec<u32>>();
    let witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

#[test]
fn test_stm_all_right() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("all_right", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, rightmost_index) =
        create_merkle_tree_with_rightmost_leaf(depth, target);
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![4, 12, 25];
    assert!(indices.iter().all(|i| *i < m));

    // Structural edge-case: all-right Merkle path shape with a fixed signer.
    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        rightmost_index,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

/// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn test_stm_all_left() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("all_left", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, leftmost_index) =
        create_merkle_tree_with_leftmost_leaf(depth, target);
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![5, 13, 21];
    assert!(indices.iter().all(|i| *i < m));

    // Structural edge-case: all-left Merkle path shape with a fixed signer.
    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        leftmost_index,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

#[test]
fn test_stm_wrong_msg() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = JubjubBase::from(42);
    let msg1 = JubjubBase::from(43);

    let env = setup_stm_env("wrong_msg", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg0, QUORUM);
    let scenario = STMScenario::new(merkle_root, msg1, witness);

    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_wrong_root() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("wrong_root", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root_0 = merkle_tree.root();
    let merkle_root_1 = merkle_root_0 + JubjubBase::ONE;
    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root_0, msg, QUORUM);
    let scenario = STMScenario::new(merkle_root_1, msg, witness);

    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

// The following "large" test case is intentionally commented out.
// This test is extremely expensive (large K and quorum) and can take
// a very long time to run, which would make CI impractically heavy.
// In the future, we may introduce a dedicated benchmarking or ignored-test
// mechanism to re-enable it in a controlled way.
//
// #[test]
// fn test_stm_large() {
//     const K: u32 = 21;
//     const QUORUM: u32 = 1024;
//     run_stm_case_default("large", K, QUORUM);
// }
