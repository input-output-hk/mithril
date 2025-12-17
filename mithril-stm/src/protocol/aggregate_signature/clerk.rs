use anyhow::{Context, anyhow};
use blake2::digest::{Digest, FixedOutput};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    ClosedKeyRegistration, Index, Parameters, Signer, SingleSignature,
    SingleSignatureWithRegisteredParty, Stake, StmResult, VerificationKey,
    proof_system::ConcatenationProof,
};

use super::{
    AggregateSignature, AggregateSignatureType, AggregateVerificationKey, AggregationError,
};

/// `Clerk` can verify and aggregate `SingleSignature`s and verify `AggregateSignature`s.
/// Clerks can only be generated with the registration closed.
/// This avoids that a Merkle Tree is computed before all parties have registered.
#[derive(Debug, Clone)]
pub struct Clerk<D: Clone + Digest> {
    pub(crate) closed_reg: ClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: Digest + Clone + FixedOutput + Send + Sync> Clerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        params: &Parameters,
        closed_reg: &ClosedKeyRegistration<D>,
    ) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a new `Clerk` from a closed registration instance.
    #[deprecated(
        since = "0.5.0",
        note = "Use `new_clerk_from_closed_key_registration` instead"
    )]
    pub fn from_registration(params: &Parameters, closed_reg: &ClosedKeyRegistration<D>) -> Self {
        Self::new_clerk_from_closed_key_registration(params, closed_reg)
    }

    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer(signer: &Signer<D>) -> Self {
        let closed_reg = signer
            .get_closed_key_registration()
            .clone()
            .expect("Core signer does not include closed registration. Clerk, and so, the Stm certificate cannot be built without closed registration!")
            ;

        Self {
            params: signer.get_parameters(),
            closed_reg,
        }
    }

    /// Create a Clerk from a signer.
    #[deprecated(since = "0.5.0", note = "Use `new_clerk_from_signer` instead")]
    pub fn from_signer(signer: &Signer<D>) -> Self {
        Self::new_clerk_from_signer(signer)
    }

    /// Aggregate a set of signatures.
    #[deprecated(since = "0.5.3", note = "Use `aggregate_signatures_with_type` instead")]
    pub fn aggregate_signatures(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> StmResult<AggregateSignature<D>> {
        self.aggregate_signatures_with_type(sigs, msg, AggregateSignatureType::default())
    }

    /// Aggregate a set of signatures with a given proof type.
    pub fn aggregate_signatures_with_type(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
        aggregate_signature_type: AggregateSignatureType,
    ) -> StmResult<AggregateSignature<D>> {
        match aggregate_signature_type {
            AggregateSignatureType::Concatenation => Ok(AggregateSignature::Concatenation(
                ConcatenationProof::aggregate_signatures(self, sigs, msg).with_context(|| {
                    format!(
                        "Signatures failed to aggregate for type {}",
                        AggregateSignatureType::Concatenation
                    )
                })?,
            )),
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Future => Err(anyhow!(
                AggregationError::UnsupportedProofSystem(aggregate_signature_type)
            )),
        }
    }

    /// Aggregate a set of signatures for their corresponding indices.
    ///
    /// This function first deduplicates the repeated signatures, and if there are enough signatures, it collects the merkle tree indexes of unique signatures.
    /// The list of merkle tree indexes is used to create a batch proof, to prove that all signatures are from eligible signers.
    ///
    /// It returns an instance of `AggregateSignature`.
    #[deprecated(since = "0.5.0", note = "Use `aggregate_signatures` instead")]
    #[allow(deprecated)]
    pub fn aggregate(
        &self,
        sigs: &[SingleSignature],
        msg: &[u8],
    ) -> StmResult<AggregateSignature<D>> {
        Self::aggregate_signatures(self, sigs, msg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    pub fn compute_aggregate_verification_key(&self) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::from(&self.closed_reg)
    }

    /// Compute the `AggregateVerificationKey` related to the used registration.
    #[deprecated(
        since = "0.5.0",
        note = "Use `compute_aggregate_verification_key` instead"
    )]
    pub fn compute_avk(&self) -> AggregateVerificationKey<D> {
        Self::compute_aggregate_verification_key(self)
    }

    /// Get the (VK, stake) of a party given its index.
    pub fn get_registered_party_for_index(
        &self,
        party_index: &Index,
    ) -> Option<(VerificationKey, Stake)> {
        self.closed_reg
            .reg_parties
            .get(*party_index as usize)
            .map(|&r| r.into())
    }

    /// Get the (VK, stake) of a party given its index.
    #[deprecated(since = "0.5.0", note = "Use `get_registered_party_for_index` instead")]
    pub fn get_reg_party(&self, party_index: &Index) -> Option<(VerificationKey, Stake)> {
        Self::get_registered_party_for_index(self, party_index)
    }

    /// Given a slice of `sig_reg_list`, this function returns a new list of `sig_reg_list` with only valid indices.
    /// In case of conflict (having several signatures for the same index)
    /// it selects the smallest signature (i.e. takes the signature with the smallest scalar).
    /// The function selects at least `self.k` indexes.
    ///  # Error
    /// If there is no sufficient signatures, then the function fails.
    // todo: We need to agree on a criteria to dedup (by default we use a BTreeMap that guarantees keys order)
    // todo: not good, because it only removes index if there is a conflict (see benches)
    pub fn select_valid_signatures_for_k_indices(
        params: &Parameters,
        msg: &[u8],
        signatures: &[SingleSignatureWithRegisteredParty],
        avk: &AggregateVerificationKey<D>,
    ) -> StmResult<Vec<SingleSignatureWithRegisteredParty>> {
        let (signatures_by_index, signatures_by_indices_to_remove) =
            Clerk::select_canonical_signatures_by_index_with_conflict_resolution(
                params, msg, signatures, avk,
            );

        let mut deduplicated_signatures: HashSet<SingleSignatureWithRegisteredParty> =
            HashSet::new();
        let mut total_number_of_indices: u64 = 0;

        for (_, current_signature) in signatures_by_index.iter() {
            if !deduplicated_signatures.contains(current_signature) {
                let mut signature_to_deduplicate = current_signature.clone();
                if let Some(indices) = signatures_by_indices_to_remove.get(current_signature) {
                    signature_to_deduplicate.sig.indexes = signature_to_deduplicate
                        .sig
                        .indexes
                        .clone()
                        .into_iter()
                        .filter(|i| !indices.contains(i))
                        .collect();
                }
                let result: Result<u64, _> = signature_to_deduplicate.sig.indexes.len().try_into();
                if let Ok(number_of_indices) = result {
                    if deduplicated_signatures.contains(&signature_to_deduplicate) {
                        panic!("Should not reach!");
                    }
                    deduplicated_signatures.insert(signature_to_deduplicate);
                    total_number_of_indices += number_of_indices;

                    if total_number_of_indices >= params.k {
                        return Ok(deduplicated_signatures.into_iter().collect());
                    }
                }
            }
        }
        Err(anyhow!(AggregationError::NotEnoughSignatures(
            total_number_of_indices,
            params.k
        )))
    }

    /// Helper function to process a collection of signatures and construct a canonical mapping from
    /// each index to a single valid signature.
    ///
    /// - Signatures that fail verification are ignored.
    /// - For each index, at most one signature is retained.
    /// - If multiple valid signatures claim the same index, the conflict is resolved by selecting
    ///   the signature with the minimal `sigma` value as the canonical representative.
    /// - All non-canonical signatures are recorded together with the indices for which they were superseded.
    fn select_canonical_signatures_by_index_with_conflict_resolution(
        params: &Parameters,
        msg: &[u8],
        signatures: &[SingleSignatureWithRegisteredParty],
        avk: &AggregateVerificationKey<D>,
    ) -> (
        BTreeMap<Index, SingleSignatureWithRegisteredParty>,
        HashMap<SingleSignatureWithRegisteredParty, Vec<Index>>,
    ) {
        let mut signatures_by_index: BTreeMap<Index, SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut signatures_by_indices_to_remove: HashMap<
            SingleSignatureWithRegisteredParty,
            Vec<Index>,
        > = HashMap::new();

        for current_signature in signatures.iter() {
            // Skip signatures that fail verification
            if current_signature
                .sig
                .verify(
                    &params,
                    &current_signature.reg_party.0,
                    &current_signature.reg_party.1,
                    &avk,
                    msg,
                )
                .is_ok()
            {
                // Process each index claimed by the current signature
                for index in current_signature.sig.indexes.iter() {
                    let mut insert_this_signature = false;

                    // Check whether an existing signature already claims this index
                    if let Some(previous_signature) = signatures_by_index.get(index) {
                        // Resolve conflict by selecting the signature with smaller `sigma`
                        let signature_to_remove_index =
                            if current_signature.sig.sigma < previous_signature.sig.sigma {
                                insert_this_signature = true;
                                previous_signature
                            } else {
                                current_signature
                            };
                        // Record the index for which the losing signature was superseded
                        if let Some(indexes) =
                            signatures_by_indices_to_remove.get_mut(signature_to_remove_index)
                        {
                            indexes.push(*index);
                        } else {
                            signatures_by_indices_to_remove
                                .insert(signature_to_remove_index.clone(), vec![*index]);
                        }
                    } else {
                        insert_this_signature = true;
                    }
                    if insert_this_signature {
                        signatures_by_index.insert(*index, current_signature.clone());
                    }
                }
            }
        }
        (signatures_by_index, signatures_by_indices_to_remove)
    }
}

#[cfg(test)]
mod tests {
    use blake2::{Blake2b, digest::consts::U32};
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    use crate::{
        Clerk, ClosedKeyRegistration, Initializer, KeyRegistration, Parameters,
        SingleSignatureWithRegisteredParty,
    };

    use super::AggregationError;

    type D = Blake2b<U32>;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn test_dedup(
            seed in any::<[u8; 32]>(),
            msg in any::<[u8;16]>(),
            nparties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            phi_f in 0.1_f64..1.0,
            num_invalid_sigs_per_party in 0_usize..3,
        ) {
            let params = Parameters { m, k, phi_f };
            let mut rng = ChaCha20Rng::from_seed(seed);

            // False messages
            let mut false_msgs = Vec::new();
            for _ in 0..num_invalid_sigs_per_party {
                let mut false_msg = vec![0u8; 32];
                rng.fill_bytes(&mut false_msg);
                if false_msg == msg {
                    false_msg[0] = msg[0].wrapping_add(1);
                }
                false_msgs.push(false_msg);
            }

            let mut key_registration = KeyRegistration::init();
            let mut initializers = Vec::new();

            for i in 0..nparties {
                let stake = (i as u64 + 1) * 10;
                let initializer = Initializer::new(params, stake, &mut rng);
                key_registration.register(initializer.stake, initializer.pk).unwrap();
                initializers.push(initializer);
            }

            let closed_registration: ClosedKeyRegistration<D> = key_registration.close();

            let signers: Vec<_> = initializers
                .into_iter()
                .map(|init| init.create_signer(closed_registration.clone()).unwrap())
                .collect();

            let clerk = Clerk::new_clerk_from_signer(&signers[0]);
            let avk = clerk.compute_aggregate_verification_key();

            let mut all_sigs = Vec::new();
            for signer in &signers {
                // Add invalid signatures
                for false_msg in &false_msgs {
                    if let Some(sig) = signer.sign(false_msg) {
                        all_sigs.push(sig);
                    }
                }
                // Add valid signatures
                if let Some(sig) = signer.sign(&msg) {
                    all_sigs.push(sig);
                }
            }

            let sig_reg_list = all_sigs
                .iter()
                .map(|sig| SingleSignatureWithRegisteredParty {
                    sig: sig.clone(),
                    reg_party: clerk.closed_reg.reg_parties[sig.signer_index as usize],
                })
                .collect::<Vec<SingleSignatureWithRegisteredParty>>();

            let dedup_result =
                Clerk::select_valid_signatures_for_k_indices(&params, &msg, &sig_reg_list, &avk);

            match dedup_result {
                Ok(valid_sigs) => {
                    assert!(!valid_sigs.is_empty(), "Should have at least one valid signature");

                    for passed_sigs in valid_sigs {
                        let signer = &signers[passed_sigs.sig.signer_index as usize];
                        let verify_result = passed_sigs.sig.verify(
                            &params,
                            &signer.get_verification_key(),
                            &signer.get_stake(),
                            &avk,
                            &msg,
                        );
                        assert!(verify_result.is_ok(), "All returned signatures should verify: {:?}", verify_result);
                    }
                }
                Err(error) => {
                    assert!(
                        matches!(
                            error.downcast_ref::<AggregationError>(),
                            Some(AggregationError::NotEnoughSignatures(..))
                        ),
                        "Expected NotEnoughSignatures, got: {:?}", error
                    );
                }
            }
        }
    }
}
