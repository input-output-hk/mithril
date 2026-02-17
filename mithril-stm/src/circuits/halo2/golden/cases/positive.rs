use ff::Field;

use crate::circuits::halo2::golden::helpers::{
    LeafSelector, StmCircuitProofError, StmCircuitScenario, build_witness_with_fixed_signer,
    build_witness_with_indices, create_default_merkle_tree, create_merkle_tree_with_leaf_selector,
    prove_and_verify_result, run_stm_circuit_case, run_stm_circuit_case_default,
    setup_stm_circuit_env,
};
use crate::circuits::halo2::types::SignedMessageWithoutPrefix;

#[test]
fn baseline_valid() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)?;
    Ok(())
}

// Expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn medium_valid() -> Result<(), StmCircuitProofError> {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)?;
    Ok(())
}

// Extremely expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn large_valid() -> Result<(), StmCircuitProofError> {
    const K: u32 = 21;
    const QUORUM: u32 = 1024;
    run_stm_circuit_case_default(current_function!(), K, QUORUM)?;
    Ok(())
}

#[test]
fn message_zero() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case(
        current_function!(),
        K,
        QUORUM,
        SignedMessageWithoutPrefix::ZERO,
    )?;
    Ok(())
}

#[test]
fn message_max() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -SignedMessageWithoutPrefix::ONE; // p - 1
    run_stm_circuit_case(current_function!(), K, QUORUM, msg_max)?;
    Ok(())
}

#[test]
fn indices_from_zero() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM)?;
    let merkle_tree = create_default_merkle_tree(env.num_signers())?;
    let merkle_root = merkle_tree.root();
    let indices = vec![0, 1, 2];
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)?;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)?;
    Ok(())
}

#[test]
fn indices_to_max() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM)?;
    let merkle_tree = create_default_merkle_tree(env.num_signers())?;
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    assert!(m >= QUORUM, "num_lotteries must be >= quorum");
    let start = m - QUORUM;
    let indices = (start..m).collect::<Vec<u32>>();
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)?;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)?;
    Ok(())
}

#[test]
fn merkle_path_all_right() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM)?;
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, rightmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::RightMost, target)?;

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![4, 12, 25];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_fixed_signer(&merkle_tree, rightmost_index, merkle_root, msg, &indices)?;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)?;
    Ok(())
}

// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn merkle_path_all_left() -> Result<(), StmCircuitProofError> {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM)?;
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -SignedMessageWithoutPrefix::ONE;
    let (merkle_tree, leftmost_index) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::LeftMost, target)?;

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![5, 13, 21];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_fixed_signer(&merkle_tree, leftmost_index, merkle_root, msg, &indices)?;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario)?;
    Ok(())
}
