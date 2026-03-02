use ff::Field;

use crate::circuits::halo2::errors::CircuitError;
use crate::circuits::halo2::golden::helpers::{
    LOTTERIES_PER_QUORUM, LeafSelector, StmCircuitScenario, assert_proof_rejected_by_verifier,
    assert_proving_circuit_error, build_witness, build_witness_with_fixed_signer,
    build_witness_with_indices, create_default_merkle_tree, create_merkle_tree_with_leaf_selector,
    find_two_distinct_witness_entries, prove_and_verify_result, setup_stm_circuit_env,
};
use crate::circuits::halo2::types::{Position, SignedMessageWithoutPrefix};
use crate::signature_scheme::{BaseFieldElement, ScalarFieldElement};

#[test]
fn quorum_not_less_than_num_lotteries() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    const NUM_LOTTERIES: u32 = 3;
    let result = setup_stm_circuit_env(current_function!(), K, QUORUM, NUM_LOTTERIES);
    let error = assert_proving_circuit_error(result);
    assert!(matches!(
        error,
        CircuitError::InvalidCircuitParameters {
            quorum: QUORUM,
            num_lotteries: NUM_LOTTERIES,
        }
    ));
}

#[test]
fn message_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = SignedMessageWithoutPrefix::from(42);
    let msg1 = SignedMessageWithoutPrefix::from(43);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("message_mismatch env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("message_mismatch tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let witness = build_witness(&merkle_tree, merkle_root, msg0, QUORUM)
        .expect("message_mismatch witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn merkle_root_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("merkle_root_mismatch env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("merkle_root_mismatch tree creation should succeed");
    let merkle_root_0 = merkle_tree.root();
    let merkle_root_1 = merkle_root_0 + SignedMessageWithoutPrefix::ONE;
    let witness = build_witness(&merkle_tree, merkle_root_0, msg, QUORUM)
        .expect("merkle_root_mismatch witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root_1, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn signature_other_message() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg0 = SignedMessageWithoutPrefix::from(42);
    let msg1 = SignedMessageWithoutPrefix::from(43);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("signature_other_message env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("signature_other_message tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let witness0 = build_witness_with_indices(&merkle_tree, merkle_root, msg0, &indices)
        .expect("signature_other_message witness0 build should succeed");
    let witness1 = build_witness_with_indices(&merkle_tree, merkle_root, msg1, &indices)
        .expect("signature_other_message witness1 build should succeed");

    // Witness membership/path/index match (root, msg1), but signature is from (root, msg0).
    let mut witness = Vec::with_capacity(witness1.len());
    for (w1, w0) in witness1.into_iter().zip(witness0.into_iter()) {
        witness.push((w1.0, w1.1, w0.2, w1.3));
    }

    let scenario = StmCircuitScenario::new(merkle_root, msg1, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn signature_verification_key_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("signature_verification_key_mismatch env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("signature_verification_key_mismatch tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("signature_verification_key_mismatch witness build should succeed");

    let (i, j) = find_two_distinct_witness_entries(&witness)
        .expect("signature_verification_key_mismatch distinct entries should exist");
    // Keep leaf/path/index from i, but replace signature with j's signature.
    let sig_j = witness[j].2;
    witness[i].2 = sig_j;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn signature_bad_challenge() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("signature_bad_challenge env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("signature_bad_challenge tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("signature_bad_challenge witness build should succeed");

    witness[0].2.challenge = witness[0].2.challenge + BaseFieldElement::get_one();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn signature_bad_response() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("signature_bad_response env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("signature_bad_response tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("signature_bad_response witness build should succeed");

    witness[0].2.response = witness[0].2.response
        - ScalarFieldElement::from_base_field(&BaseFieldElement::get_one())
            .expect("signature_bad_response response scalar conversion should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn signature_bad_commitment() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("signature_bad_commitment env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("signature_bad_commitment tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("signature_bad_commitment witness build should succeed");

    witness[0].2.commitment_point = witness[1].2.commitment_point;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn indices_not_increasing() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("indices_not_increasing env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("indices_not_increasing tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 14];
    assert!(indices.iter().all(|i| *i < m));
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("indices_not_increasing witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn index_out_of_bounds() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("index_out_of_bounds env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("index_out_of_bounds tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, m];
    let witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("index_out_of_bounds witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn merkle_path_corrupt_sibling() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("merkle_path_corrupt_sibling env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("merkle_path_corrupt_sibling tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("merkle_path_corrupt_sibling witness build should succeed");
    let d = 0usize;
    assert!(!witness[0].1.siblings.is_empty());
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].1 += SignedMessageWithoutPrefix::ONE;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn merkle_path_flip_position() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("merkle_path_flip_position env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("merkle_path_flip_position tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("merkle_path_flip_position witness build should succeed");
    let d = 1usize;
    assert!(d < witness[0].1.siblings.len());
    witness[0].1.siblings[d].0 = match witness[0].1.siblings[d].0 {
        Position::Left => Position::Right,
        Position::Right => Position::Left,
    };

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn merkle_path_length_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("merkle_path_length_short env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("merkle_path_length_short tree creation should succeed");
    let expected_depth = env.num_signers().next_power_of_two().trailing_zeros();

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("merkle_path_length_short witness build should succeed");
    assert!(!witness[0].1.siblings.is_empty());
    witness[0].1.siblings.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let error = assert_proving_circuit_error(prove_and_verify_result(&env, scenario));
    assert!(matches!(
        error,
        CircuitError::MerkleSiblingLengthMismatch {
            expected_depth: expected,
            actual: found,
        } if expected == expected_depth && found == expected_depth - 1
    ));
}

#[test]
fn merkle_path_length_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("merkle_path_length_long env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("merkle_path_length_long tree creation should succeed");
    let expected_depth = env.num_signers().next_power_of_two().trailing_zeros();

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("merkle_path_length_long witness build should succeed");
    witness[0]
        .1
        .siblings
        .push((Position::Left, SignedMessageWithoutPrefix::ZERO));

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let error = assert_proving_circuit_error(prove_and_verify_result(&env, scenario));
    assert!(matches!(
        error,
        CircuitError::MerkleSiblingLengthMismatch {
            expected_depth: expected,
            actual: found,
        } if expected == expected_depth && found == expected_depth + 1
    ));
}

#[test]
fn leaf_swap_keep_merkle_path() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("leaf_swap_keep_merkle_path env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("leaf_swap_keep_merkle_path tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("leaf_swap_keep_merkle_path witness build should succeed");
    let (i, j) = find_two_distinct_witness_entries(&witness)
        .expect("leaf_swap_keep_merkle_path distinct entries should exist");
    // Keep path/signature/index from i, but swap leaf with j.
    witness[i].0 = witness[j].0;

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn leaf_merkle_path_mismatch() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("leaf_merkle_path_mismatch env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("leaf_merkle_path_mismatch tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("leaf_merkle_path_mismatch witness build should succeed");
    let (i, j) = find_two_distinct_witness_entries(&witness)
        .expect("leaf_merkle_path_mismatch distinct entries should exist");
    // Keep leaf/signature/index from i, but swap in j's Merkle path.
    witness[i].1 = witness[j].1.clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn leaf_wrong_verification_key() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("leaf_wrong_verification_key env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("leaf_wrong_verification_key tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("leaf_wrong_verification_key witness build should succeed");
    let (i, j) = find_two_distinct_witness_entries(&witness)
        .expect("leaf_wrong_verification_key distinct entries should exist");
    // Keep target/path/signature/index from i, but swap in j's verification key.
    let target = witness[i].0.1;
    let verification_key = witness[j].0.0;
    witness[i].0 = crate::circuits::halo2::types::MTLeaf(verification_key, target);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn target_less_than_evaluation() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("target_less_than_evaluation env setup should succeed");
    let depth = env.num_signers().next_power_of_two().trailing_zeros();
    let signer_index = 0usize;
    let (merkle_tree, _) = create_merkle_tree_with_leaf_selector(
        depth,
        LeafSelector::Index(signer_index),
        SignedMessageWithoutPrefix::ZERO,
    )
    .expect("target_less_than_evaluation tree creation should succeed");

    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![5, 17, 25];
    assert!(indices.iter().all(|i| *i < m));
    let witness =
        build_witness_with_fixed_signer(&merkle_tree, signer_index, merkle_root, msg, &indices)
            .expect("target_less_than_evaluation witness build should succeed");

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}

#[test]
fn witness_length_short() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("witness_length_short env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("witness_length_short tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let mut witness = build_witness(&merkle_tree, merkle_root, msg, QUORUM)
        .expect("witness_length_short witness build should succeed");

    witness.pop();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let error = assert_proving_circuit_error(prove_and_verify_result(&env, scenario));
    assert!(matches!(
        error,
        CircuitError::WitnessLengthMismatch {
            expected_quorum,
            actual: 2,
        } if expected_quorum == QUORUM
    ));
}

#[test]
fn witness_length_long() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("witness_length_long env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("witness_length_long tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let mut witness = build_witness(&merkle_tree, merkle_root, msg, QUORUM)
        .expect("witness_length_long witness build should succeed");

    let extra = witness
        .last()
        .cloned()
        .expect("witness should not be empty in witness_length_long");
    witness.push(extra);

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    let error = assert_proving_circuit_error(prove_and_verify_result(&env, scenario));
    assert!(matches!(
        error,
        CircuitError::WitnessLengthMismatch {
            expected_quorum,
            actual: 4,
        } if expected_quorum == QUORUM
    ));
}

#[test]
fn witness_duplicate_entry() {
    const K: u32 = 13;
    const QUORUM: u32 = 3;
    let msg = SignedMessageWithoutPrefix::from(42);
    let env = setup_stm_circuit_env(
        current_function!(),
        K,
        QUORUM,
        QUORUM * LOTTERIES_PER_QUORUM,
    )
    .expect("witness_duplicate_entry env setup should succeed");
    let merkle_tree = create_default_merkle_tree(env.num_signers())
        .expect("witness_duplicate_entry tree creation should succeed");
    let merkle_root = merkle_tree.root();
    let m = env.num_lotteries();
    let indices = vec![6, 14, 22];
    assert!(indices.iter().all(|i| *i < m));
    let mut witness = build_witness_with_indices(&merkle_tree, merkle_root, msg, &indices)
        .expect("witness_duplicate_entry witness build should succeed");

    witness[1] = witness[0].clone();

    let scenario = StmCircuitScenario::new(merkle_root, msg, witness);
    assert_proof_rejected_by_verifier(prove_and_verify_result(&env, scenario));
}
