use anyhow::Context;

use crate::{
    BaseFieldElement, MembershipDigest, SingleSignature, StmResult,
    membership_commitment::{MerkleTree, MerkleTreeSnarkLeaf},
    proof_system::{
        AggregateVerificationKeyForSnark, SnarkClerk,
        halo2_snark::{build_snark_message, compute_winning_lottery_indices},
    },
};

use super::{SignatureRegistrationEntry, WitnessEntry};

/// Public instance for the SNARK circuit: `(merkle_tree_root, message)`.
type Instance = (BaseFieldElement, BaseFieldElement);

/// SNARK proof consisting of the public instance and a list of witness entries.
///
/// The instance holds the Merkle tree root and message (public inputs to the circuit).
/// The witness holds one [`WitnessEntry`] per winning lottery index, each containing
/// the signature, Merkle leaf, and Merkle path needed by the circuit.
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
#[derive(Debug, Clone)]
pub struct SnarkProof {
    /// Public inputs to the SNARK circuit.
    instance: Instance,
    /// Per-winning-lottery-index witness data.
    witness: Vec<WitnessEntry>,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SnarkProof {
    /// Aggregate single signatures into a `SnarkProof`.
    ///
    /// This function:
    /// 1. Computes the aggregate verification key and SNARK message.
    /// 2. Pairs each signature with its registration entry, filtering out entries without SNARK data.
    /// 3. Verifies each SNARK signature and computes its winning lottery indices.
    /// 4. Deduplicates indices across signers to select at least `k` unique winning indices.
    /// 5. Expands deduped signatures into witness entries (one per winning lottery index).
    /// 6. Sorts witness entries by lottery index for deterministic circuit ordering.
    /// 7. Constructs the public instance from the Merkle tree root and message.
    pub fn aggregate_signatures<D: MembershipDigest>(
        clerk: &SnarkClerk,
        signatures: &[SingleSignature],
        message: &[u8],
    ) -> StmResult<SnarkProof> {
        let avk: AggregateVerificationKeyForSnark<D> =
            clerk.compute_aggregate_verification_key_for_snark();
        let message_to_sign = build_snark_message(&avk.get_merkle_tree_commitment().root, message)?;

        // Pair each signature with its registration entry, filtering out entries without SNARK data.
        // Errors from `get_snark_registration_entry` (e.g., invalid signer index) are propagated;
        // only `None` results (missing SNARK key/signature) are silently filtered out.
        let mut sig_reg_list: Vec<SignatureRegistrationEntry> = signatures
            .iter()
            .map(|sig| -> StmResult<Option<SignatureRegistrationEntry>> {
                let Some(snark_sig) = sig.snark_signature.clone() else {
                    return Ok(None);
                };
                let Some(reg_entry) = clerk.get_snark_registration_entry(sig.signer_index)? else {
                    return Ok(None);
                };
                Ok(Some(SignatureRegistrationEntry::new(snark_sig, reg_entry)))
            })
            .collect::<StmResult<Vec<_>>>()?
            .into_iter()
            .flatten()
            .collect();

        // Verify each signature, compute its winning lottery indices, and set them on the entry.
        // Discard entries that fail verification or win no indices.
        sig_reg_list.retain_mut(|entry| {
            let reg = entry.get_registration_entry();
            if entry.get_signature().verify(&reg.0, message, &avk).is_ok()
                && let Ok(indices) = compute_winning_lottery_indices(
                    clerk.parameters.m,
                    &message_to_sign,
                    &entry.get_signature().get_schnorr_signature(),
                    reg.1,
                )
            {
                entry.set_indices(&indices);
                return true;
            }
            false
        });

        // Deduplicate indices across all entries, selecting at least `k` unique winning indices.
        let deduped_signatures =
            SnarkClerk::select_valid_signatures_for_k_indices(&clerk.parameters, &sig_reg_list)
                .with_context(
                    || "Failed to aggregate unique signatures during selection for the k indices.",
                )?;

        let merkle_tree: MerkleTree<D::SnarkHash, MerkleTreeSnarkLeaf> =
            clerk.closed_key_registration.to_merkle_tree();

        // Build a leaf-index lookup map once for O(1) lookups instead of O(n) per signature.
        let leaf_index_map = merkle_tree.build_leaf_index_map();

        // Expand each deduped signature into one `WitnessEntry` per winning lottery index,
        // then sort all entries by lottery index for deterministic circuit ordering.
        let mut witness: Vec<WitnessEntry> = deduped_signatures
            .into_iter()
            .map(|entry| {
                let leaf_index = merkle_tree
                    .find_leaf_index_from_map(entry.get_registration_entry(), &leaf_index_map)?;
                WitnessEntry::convert_snark_single_signature_to_witness_entries::<D>(
                    entry,
                    &merkle_tree,
                    leaf_index,
                )
            })
            .collect::<StmResult<Vec<_>>>()
            .with_context(|| "Failed to convert deduped signatures into witness entries.")?
            .into_iter()
            .flatten()
            .collect();
        witness.sort_unstable();

        // Public instance: (merkle_tree_root, message) as base field elements.
        let instance = (message_to_sign[0], message_to_sign[1]);

        Ok(SnarkProof { instance, witness })
    }

    /// Return the public instance `(merkle_tree_root, message)`.
    pub fn get_instance(&self) -> Instance {
        self.instance
    }

    /// Return a reference to the witness entries.
    pub fn get_witness(&self) -> &[WitnessEntry] {
        &self.witness
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        AggregationError, ClosedKeyRegistration, Initializer, KeyRegistration,
        MithrilMembershipDigest, Parameters, Signer, SingleSignature,
        proof_system::{
            SnarkClerk,
            halo2_snark::{AggregateVerificationKeyForSnark, build_snark_message},
        },
    };

    use super::SnarkProof;

    type D = MithrilMembershipDigest;

    fn find_signatures(msg: &[u8], signers: &[Signer<D>]) -> Vec<SingleSignature> {
        signers
            .iter()
            .filter_map(|signer| signer.create_single_signature(msg).ok())
            .collect()
    }

    fn setup_signers_and_clerk(
        params: Parameters,
        nparties: usize,
        seed: [u8; 32],
    ) -> (Vec<Signer<D>>, SnarkClerk) {
        let mut rng = ChaCha20Rng::from_seed(seed);
        let mut key_registration = KeyRegistration::initialize();
        let mut initializers = Vec::new();

        for i in 0..nparties {
            let stake = (i as u64 + 1) * 10;
            let initializer = Initializer::new(params, stake, &mut rng);
            key_registration
                .register_by_entry(&initializer.clone().try_into().unwrap())
                .unwrap();
            initializers.push(initializer);
        }

        let closed_registration: ClosedKeyRegistration =
            key_registration.close_registration(&params).unwrap();

        let signers: Vec<Signer<D>> = initializers
            .into_iter()
            .map(|init| init.try_create_signer(&closed_registration).unwrap())
            .collect();

        let clerk = SnarkClerk::new_clerk_from_signer::<D>(&signers[0]);
        (signers, clerk)
    }

    // NOTE: `phi_f` is hardcoded to `0.5` in these tests but has no real effect on the SNARK
    // witness because `LotteryTargetValue` is currently set to `p-1` in
    // `ClosedRegistrationEntry::from(RegistrationEntry, Stake)`, making every signer win every
    // lottery index regardless of `phi_f`. These tests will become more meaningful once ticket
    // 3068 is merged, which replaces the `p-1` default with the actual
    // `compute_lottery_target_value(stake, total_stake)` computation (currently commented out in
    // `closed_registration_entry.rs`).
    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn aggregate_signatures(
            seed in any::<[u8; 32]>(),
            msg in any::<[u8; 32]>(),
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
        ) {
            let params = Parameters { m, k, phi_f: 0.5 };
            let (signers, clerk) = setup_signers_and_clerk(params, nparties, seed);
            let sigs = find_signatures(&msg, &signers);

            let result = SnarkProof::aggregate_signatures::<D>(&clerk, &sigs, &msg);

            match result {
                Ok(proof) => {
                    let witness = proof.get_witness();
                    assert!(!witness.is_empty(), "Witness should not be empty");

                    // Witness entries are sorted by lottery index
                    for window in witness.windows(2) {
                        assert!(
                            window[0].get_lottery_index() <= window[1].get_lottery_index(),
                            "Witness entries should be sorted by lottery index"
                        );
                    }

                    // All lottery indices are unique and count >= k
                    let mut unique_indices = HashSet::new();
                    for entry in witness {
                        assert!(
                            unique_indices.insert(entry.get_lottery_index()),
                            "Lottery indices should be unique"
                        );
                    }
                    assert!(
                        unique_indices.len() as u64 >= k,
                        "Should have at least k unique indices"
                    );

                    // Instance matches expected values
                    let avk: AggregateVerificationKeyForSnark<D> =
                        clerk.compute_aggregate_verification_key_for_snark();
                    let message_to_sign =
                        build_snark_message(&avk.get_merkle_tree_commitment().root, &msg)
                            .unwrap();
                    let (root, message) = proof.get_instance();
                    assert_eq!(root, message_to_sign[0], "Instance root should match");
                    assert_eq!(message, message_to_sign[1], "Instance message should match");
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.downcast_ref::<AggregationError>(),
                            Some(AggregationError::NotEnoughSignatures(..))
                        ),
                        "Expected NotEnoughSignatures, got: {:?}",
                        error
                    );
                }
            }
        }

        #[test]
        fn witness_leaf_matches_registration(
            seed in any::<[u8; 32]>(),
            msg in any::<[u8; 32]>(),
            nparties in 2_usize..10,
            m in 10_u64..20,
            k in 1_u64..5,
        ) {
            let params = Parameters { m, k, phi_f: 0.5 };
            let (signers, clerk) = setup_signers_and_clerk(params, nparties, seed);
            let sigs = find_signatures(&msg, &signers);

            let result = SnarkProof::aggregate_signatures::<D>(&clerk, &sigs, &msg);

            match result {
                Ok(proof) => {
                    // Collect all registered SNARK leaves
                    let all_leaves: HashSet<_> = signers
                        .iter()
                        .enumerate()
                        .filter_map(|(i, _)| {
                            clerk
                                .get_snark_registration_entry(i as u64)
                                .ok()
                                .flatten()
                        })
                        .collect();

                    for entry in proof.get_witness() {
                        assert!(
                            all_leaves.contains(&entry.get_merkle_tree_leaf()),
                            "Witness leaf should be from a registered signer"
                        );
                    }
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.downcast_ref::<AggregationError>(),
                            Some(AggregationError::NotEnoughSignatures(..))
                        ),
                        "Expected NotEnoughSignatures, got: {:?}",
                        error
                    );
                }
            }
        }
    }
}
