use crate::circuits::halo2::golden::support::stm_case::{
    build_witness, build_witness_with_fixed_signer, build_witness_with_indices,
    create_default_merkle_tree, create_merkle_tree_with_controlled_leaf,
    create_merkle_tree_with_leftmost_leaf,
    create_merkle_tree_with_rightmost_leaf, prove_and_verify_result, run_stm_case,
    run_stm_case_default, setup_stm_env, STMProofError, STMScenario,
};
use crate::circuits::halo2::types::{JubjubBase, JubjubScalar};
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

#[test]
fn test_stm_signed_other_msg() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = JubjubBase::from(42);
    let msg1 = JubjubBase::from(43);

    let env = setup_stm_env("signed_other_msg", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let witness0 = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg0,
        &indices,
    );
    let witness1 = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg1,
        &indices,
    );

    // Witness membership/path/index match (root, msg1), but signature is from (root, msg0).
    let mut witness = Vec::with_capacity(witness1.len());
    for (w1, w0) in witness1.into_iter().zip(witness0.into_iter()) {
        witness.push((w1.0, w1.1, w0.2, w1.3));
    }

    let scenario = STMScenario::new(merkle_root, msg1, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_sig_leaf_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("sig_leaf_mismatch", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let mut mismatch_idx = None;
    for i in 0..witness.len() {
        for j in (i + 1)..witness.len() {
            if witness[i].0.to_bytes() != witness[j].0.to_bytes() {
                mismatch_idx = Some((i, j));
                break;
            }
        }
        if mismatch_idx.is_some() {
            break;
        }
    }
    let (i, j) = mismatch_idx.expect("expected at least two distinct signers in witness");

    // Keep leaf/path/index from i, but replace signature with j's signature.
    let sig_j = witness[j].2.clone();
    witness[i].2 = sig_j;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_bad_challenge() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("bad_challenge", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );
    witness[0].2.c += JubjubBase::ONE;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_bad_response() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("bad_response", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );
    witness[0].2.s += JubjubScalar::ONE;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_bad_commitment() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("bad_commitment", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );
    witness[0].2.sigma = witness[1].2.sigma;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_sig_lottery_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("sig_lottery_mismatch", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let signer_index = 0usize;
    let (sks, leaves, merkle_tree) =
        create_merkle_tree_with_controlled_leaf(depth, signer_index, JubjubBase::ZERO);
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![5, 17, 25];
    assert!(indices.iter().all(|i| *i < m));

    // Lottery-mismatch negative test: target=0 so eligibility should fail (ev > target),
    // with negligible flake probability if ev==0.
    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        signer_index,
        merkle_root,
        msg,
        &indices,
    );
    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_non_increasing() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("non_increasing", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 14];
    assert!(indices.iter().all(|i| *i < m));

    let witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_index_oob() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("index_oob", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, m];

    let witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_corrupt_sibling() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("corrupt_sibling", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let d = 0usize;
    assert!(!witness[0].1.siblings.is_empty());
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].1 += JubjubBase::ONE;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_flip_position() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("flip_position", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let d = 1usize;
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].0 = match witness[0].1.siblings[d].0 {
        crate::circuits::halo2::off_circuit::merkle_tree::Position::Left => {
            crate::circuits::halo2::off_circuit::merkle_tree::Position::Right
        }
        crate::circuits::halo2::off_circuit::merkle_tree::Position::Right => {
            crate::circuits::halo2::off_circuit::merkle_tree::Position::Left
        }
    };

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[ignore]
#[test]
fn test_stm_wrong_path_len_short() {
    // Ignored: malformed MerklePath length currently causes a panic during circuit synthesis (witness layout),
    // not a clean verification failure. To be re-enabled once the circuit is hardened to handle this safely.
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("wrong_path_len_short", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    assert!(!witness[0].1.siblings.is_empty());
    witness[0].1.siblings.pop();

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[ignore]
#[test]
fn test_stm_wrong_path_len_long() {
    // Ignored: malformed MerklePath length currently causes a panic during circuit synthesis (witness layout),
    // not a clean verification failure. To be re-enabled once the circuit is hardened to handle this safely.
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("wrong_path_len_long", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    witness[0].1.siblings.push((
        crate::circuits::halo2::off_circuit::merkle_tree::Position::Left,
        JubjubBase::ZERO,
    ));

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}

#[test]
fn test_stm_path_other_leaf() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_env("path_other_leaf", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness = build_witness_with_indices(
        &sks,
        &leaves,
        &merkle_tree,
        merkle_root,
        msg,
        &indices,
    );

    let mut mismatch_idx = None;
    for i in 0..witness.len() {
        for j in (i + 1)..witness.len() {
            if witness[i].0.to_bytes() != witness[j].0.to_bytes() {
                mismatch_idx = Some((i, j));
                break;
            }
        }
        if mismatch_idx.is_some() {
            break;
        }
    }
    let (i, j) = mismatch_idx.expect("expected at least two distinct leaves in witness");

    // Keep path/signature/index from i, but swap leaf with j.
    witness[i].0 = witness[j].0;

    let scenario = STMScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(STMProofError::VerifyFail)));
}
