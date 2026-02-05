use ff::Field;

use crate::circuits::halo2::golden::support::{
    StmCircuitProofError, StmCircuitScenario, build_witness, build_witness_with_fixed_signer,
    build_witness_with_indices, create_default_merkle_tree,
    create_merkle_tree_with_controlled_leaf, create_merkle_tree_with_leftmost_leaf,
    create_merkle_tree_with_rightmost_leaf, prove_and_verify_result, run_stm_circuit_case,
    run_stm_circuit_case_default, setup_stm_circuit_env,
};
use crate::circuits::halo2::types::{JubjubBase, JubjubScalar};

// Baseline: valid witness; expected to succeed.
#[test]
fn baseline() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case_default("small", K, QUORUM);
}

// Larger valid witness; expected to succeed.
// Expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn medium() {
    const K: u32 = 16;
    const QUORUM: u32 = 32;
    run_stm_circuit_case_default("medium", K, QUORUM);
}

// Very large valid witness; expected to succeed.
// Extremely expensive; excluded from CI and intended for manual runs.
#[ignore]
#[test]
fn large() {
    const K: u32 = 21;
    const QUORUM: u32 = 1024;
    run_stm_circuit_case_default("large", K, QUORUM);
}

// Public message is zero; expected to succeed.
#[test]
fn msg_0() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    run_stm_circuit_case("msg_0", K, QUORUM, JubjubBase::ZERO);
}

// Public message is max field element; expected to succeed.
#[test]
fn msg_max() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg_max = -JubjubBase::ONE; // p - 1
    run_stm_circuit_case("msg_max", K, QUORUM, msg_max);
}

// Indices start at 0 and strictly increase; expected to succeed.
#[test]
fn min_strict_indices() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("min_strict_indices", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let indices = vec![0, 1, 2];
    let witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    prove_and_verify_result(&env, scenario).expect("Proof generation/verification failed");
}

// Indices end at m-1 and strictly increase; expected to succeed.
#[test]
fn max_strict_indices() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("max_strict_indices", K, QUORUM);
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

// All-right Merkle path with a fixed signer; expected to succeed.
#[test]
fn all_right() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("all_right", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, rightmost_index) =
        create_merkle_tree_with_rightmost_leaf(depth, target);
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

// All-left Merkle path with a fixed signer; expected to succeed.
// Requires power-of-two signers; otherwise padding prevents an all-left path.
#[test]
fn all_left() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("all_left", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let target = -JubjubBase::ONE;
    let (sks, leaves, merkle_tree, leftmost_index) =
        create_merkle_tree_with_leftmost_leaf(depth, target);
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

// Mutation: public msg mismatched to witness; expected to fail verification.
#[test]
fn wrong_msg() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = JubjubBase::from(42);
    let msg1 = JubjubBase::from(43);

    let env = setup_stm_circuit_env("wrong_msg", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg0, QUORUM);
    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);

    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: public Merkle root mismatched; expected to fail verification.
#[test]
fn wrong_root() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_root", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root_0 = merkle_tree.root();
    let merkle_root_1 = merkle_root_0 + JubjubBase::ONE;
    let witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root_0, msg, QUORUM);
    let scenario = StmCircuitScenario::new(merkle_root_1, msg, witness);

    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: signature produced for a different msg; expected to fail verification.
#[test]
fn signed_other_msg() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = JubjubBase::from(42);
    let msg1 = JubjubBase::from(43);

    let env = setup_stm_circuit_env("signed_other_msg", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let witness0 =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg0, &indices);
    let witness1 =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg1, &indices);

    // Witness membership/path/index match (root, msg1), but signature is from (root, msg0).
    let mut witness = Vec::with_capacity(witness1.len());
    for (w1, w0) in witness1.into_iter().zip(witness0.into_iter()) {
        witness.push((w1.0, w1.1, w0.2, w1.3));
    }

    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: leaf VK and signature from different signers; expected to fail verification.
#[test]
fn sig_leaf_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("sig_leaf_mismatch", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

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

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: signature challenge altered; expected to fail verification.
#[test]
fn bad_challenge() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("bad_challenge", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);
    witness[0].2.c += JubjubBase::ONE;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: signature response altered; expected to fail verification.
#[test]
fn bad_response() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("bad_response", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);
    witness[0].2.s += JubjubScalar::ONE;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: signature commitment altered; expected to fail verification.
#[test]
fn bad_commitment() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("bad_commitment", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);
    witness[0].2.sigma = witness[1].2.sigma;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: lottery target too small; expected to fail verification.
#[test]
fn sig_lottery_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("sig_lottery_mismatch", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let signer_index = 0usize;
    let (sks, leaves, merkle_tree) =
        create_merkle_tree_with_controlled_leaf(depth, signer_index, JubjubBase::ZERO);
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![5, 17, 25];
    assert!(indices.iter().all(|i| *i < m));

    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        signer_index,
        merkle_root,
        msg,
        &indices,
    );
    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: indices are not strictly increasing; expected to fail verification.
#[test]
fn non_increasing() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("non_increasing", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 14];
    assert!(indices.iter().all(|i| *i < m));

    let witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: index is out of bounds (>= m); expected to fail verification.
#[test]
fn index_oob() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("index_oob", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, m];

    let witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: Merkle sibling hash corrupted; expected to fail verification.
#[test]
fn corrupt_sibling() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("corrupt_sibling", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    let d = 0usize;
    assert!(!witness[0].1.siblings.is_empty());
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].1 += JubjubBase::ONE;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: Merkle position bit flipped; expected to fail verification.
#[test]
fn flip_position() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("flip_position", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

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

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Malformed path length panics during synthesis; expected to fail verification once hardened.
#[should_panic]
#[test]
fn wrong_path_len_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_path_len_short", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    assert!(!witness[0].1.siblings.is_empty());
    witness[0].1.siblings.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Malformed path length panics during synthesis; expected to fail verification once hardened.
#[should_panic]
#[test]
fn wrong_path_len_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_path_len_long", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

    witness[0].1.siblings.push((
        crate::circuits::halo2::off_circuit::merkle_tree::Position::Left,
        JubjubBase::ZERO,
    ));

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: leaf swapped while keeping its path; expected to fail verification.
#[test]
fn path_other_leaf() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("path_other_leaf", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

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

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: leaf and Merkle path come from different signers; expected to fail verification.
#[test]
fn leaf_path_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("leaf_path_mismatch", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

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

    // Keep leaf/signature/index from i, but swap in j's Merkle path.
    witness[i].1 = witness[j].1.clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: leaf verification key is swapped while keeping target/path/signature; expected to fail verification.
#[test]
fn wrong_vk() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_vk", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);

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

    // Keep target/path/signature/index from i, but swap in j's verification key.
    let target = witness[i].0.1;
    let vk = witness[j].0.0;
    witness[i].0 = crate::circuits::halo2::off_circuit::merkle_tree::MTLeaf(vk, target);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: leaf target is too small (target < ev); expected to fail verification.
#[test]
fn target_lt_ev() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("target_lt_ev", K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let signer_index = 0usize;
    let (sks, leaves, merkle_tree) =
        create_merkle_tree_with_controlled_leaf(depth, signer_index, JubjubBase::ZERO);
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![5, 17, 25];
    assert!(indices.iter().all(|i| *i < m));

    let witness = build_witness_with_fixed_signer(
        &sks,
        &leaves,
        &merkle_tree,
        signer_index,
        merkle_root,
        msg,
        &indices,
    );

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Malformed witness length panics during synthesis; expected to fail verification once hardened.
#[should_panic]
#[test]
fn wrong_witness_len_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_witness_len_short", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let mut witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg, QUORUM);
    witness.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Malformed witness length panics during synthesis; expected to fail verification once hardened.
#[should_panic]
#[test]
fn wrong_witness_len_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("wrong_witness_len_long", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let mut witness = build_witness(&sks, &leaves, &merkle_tree, merkle_root, msg, QUORUM);
    let extra = witness.last().cloned().expect("expected non-empty witness");
    witness.push(extra);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

// Mutation: duplicate a valid witness entry (index/signature/leaf/path); expected to fail verification.
#[test]
fn duplicate_entries() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = JubjubBase::from(42);

    let env = setup_stm_circuit_env("duplicate_entries", K, QUORUM);
    let (sks, leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();

    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));

    let mut witness =
        build_witness_with_indices(&sks, &leaves, &merkle_tree, merkle_root, msg, &indices);
    witness[1] = witness[0].clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}
