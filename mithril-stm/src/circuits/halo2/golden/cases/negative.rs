use ff::Field;

use crate::circuits::halo2::golden::helpers::{
    LeafSelector, StmCircuitProofError, StmCircuitScenario, build_witness,
    build_witness_with_fixed_signer, build_witness_with_indices, create_default_merkle_tree,
    create_merkle_tree_with_leaf_selector, find_two_distinct_witness_entries,
    prove_and_verify_result, setup_stm_circuit_env,
};
use crate::circuits::halo2::types::{Msg, Position};
use crate::signature_scheme::{BaseFieldElement, ScalarFieldElement};

#[test]
fn message_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = Msg::from(42);
    let msg1 = Msg::from(43);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let witness = build_witness(&signer_leaves, &merkle_tree, merkle_root, msg0, QUORUM);

    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn merkle_root_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root_0 = merkle_tree.root();
    let merkle_root_1 = merkle_root_0 + Msg::ONE;
    let witness = build_witness(&signer_leaves, &merkle_tree, merkle_root_0, msg, QUORUM);

    let scenario = StmCircuitScenario::new(merkle_root_1, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn signature_other_message() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = Msg::from(42);
    let msg1 = Msg::from(43);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let witness0 =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg0, &indices);
    let witness1 =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg1, &indices);

    // Witness membership/path/index match (root, msg1), but signature is from (root, msg0).
    let mut witness = Vec::with_capacity(witness1.len());
    for (w1, w0) in witness1.into_iter().zip(witness0.into_iter()) {
        witness.push((w1.0, w1.1, w0.2, w1.3));
    }

    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn signature_verification_key_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    let (i, j) = find_two_distinct_witness_entries(&witness);
    // Keep leaf/path/index from i, but replace signature with j's signature.
    let sig_j = witness[j].2;
    witness[i].2 = sig_j;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn signature_bad_challenge() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    witness[0].2.challenge = witness[0].2.challenge + BaseFieldElement::get_one();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn signature_bad_response() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    witness[0].2.response = witness[0].2.response
        - ScalarFieldElement::from_base_field(&BaseFieldElement::get_one())
            .expect("Failed to convert base field element to scalar");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn signature_bad_commitment() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    witness[0].2.commitment_point = witness[1].2.commitment_point;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn indices_not_increasing() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 14];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn index_out_of_bounds() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, m];
    let witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn merkle_path_corrupt_sibling() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    let d = 0usize;
    assert!(!witness[0].1.siblings.is_empty());
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].1 += Msg::ONE;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn merkle_path_flip_position() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    let d = 1usize;
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].0 = match witness[0].1.siblings[d].0 {
        Position::Left => Position::Right,
        Position::Right => Position::Left,
    };

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[should_panic]
#[test]
fn merkle_path_length_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    assert!(!witness[0].1.siblings.is_empty());
    witness[0].1.siblings.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[should_panic]
#[test]
fn merkle_path_length_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    witness[0].1.siblings.push((Position::Left, Msg::ZERO));

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn leaf_swap_keep_merkle_path() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    let (i, j) = find_two_distinct_witness_entries(&witness);
    // Keep path/signature/index from i, but swap leaf with j.
    witness[i].0 = witness[j].0;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn leaf_merkle_path_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    let (i, j) = find_two_distinct_witness_entries(&witness);
    // Keep leaf/signature/index from i, but swap in j's Merkle path.
    witness[i].1 = witness[j].1.clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn leaf_wrong_verification_key() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);
    let (i, j) = find_two_distinct_witness_entries(&witness);
    // Keep target/path/signature/index from i, but swap in j's verification key.
    let target = witness[i].0.1;
    let vk_bytes = witness[j].0.0;
    witness[i].0 = crate::circuits::halo2::types::MTLeaf(vk_bytes, target);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn target_less_than_evaluation() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let signer_index = 0usize;
    let (signer_leaves, merkle_tree, _) =
        create_merkle_tree_with_leaf_selector(depth, LeafSelector::Index(signer_index), Msg::ZERO);

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![5, 17, 25];
    assert!(indices.iter().all(|i| *i < m));
    let witness = build_witness_with_fixed_signer(
        &signer_leaves,
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

#[should_panic]
#[test]
fn witness_length_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let mut witness = build_witness(&signer_leaves, &merkle_tree, merkle_root, msg, QUORUM);

    witness.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[should_panic]
#[test]
fn witness_length_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let mut witness = build_witness(&signer_leaves, &merkle_tree, merkle_root, msg, QUORUM);

    let extra = witness.last().cloned().expect("expected non-empty witness");
    witness.push(extra);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}

#[test]
fn witness_duplicate_entry() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = Msg::from(42);
    let env = setup_stm_circuit_env(current_function!(), K, QUORUM);
    let (signer_leaves, merkle_tree) = create_default_merkle_tree(env.num_signers());
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness =
        build_witness_with_indices(&signer_leaves, &merkle_tree, merkle_root, msg, &indices);

    witness[1] = witness[0].clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let result = prove_and_verify_result(&env, scenario);
    assert!(matches!(result, Err(StmCircuitProofError::VerifyFail)));
}
