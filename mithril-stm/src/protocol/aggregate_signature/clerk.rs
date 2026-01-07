use anyhow::{Context, anyhow};
use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    OutdatedClosedKeyRegistration, Index, MembershipDigest, OutdatedSigner, Parameters, SingleSignature,
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
pub struct Clerk<D: MembershipDigest> {
    pub(crate) closed_reg: OutdatedClosedKeyRegistration<D>,
    pub(crate) params: Parameters,
}

impl<D: MembershipDigest> Clerk<D> {
    /// Create a new `Clerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        params: &Parameters,
        closed_reg: &OutdatedClosedKeyRegistration<D>,
    ) -> Self {
        Self {
            params: *params,
            closed_reg: closed_reg.clone(),
        }
    }

    /// Create a Clerk from a signer.
    pub fn new_clerk_from_signer(signer: &OutdatedSigner<D>) -> Self {
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

    /// Compute the `AggregateVerificationKey` related to the used registration.
    pub fn compute_aggregate_verification_key(&self) -> AggregateVerificationKey<D> {
        AggregateVerificationKey::from(&self.closed_reg)
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
        sigs: &[SingleSignatureWithRegisteredParty],
        avk: &AggregateVerificationKey<D>,
    ) -> StmResult<Vec<SingleSignatureWithRegisteredParty>> {
        let mut sig_by_index: BTreeMap<Index, &SingleSignatureWithRegisteredParty> =
            BTreeMap::new();
        let mut removal_idx_by_vk: HashMap<&SingleSignatureWithRegisteredParty, Vec<Index>> =
            HashMap::new();

        for sig_reg in sigs.iter() {
            if sig_reg
                .sig
                .verify(params, &sig_reg.reg_party.0, &sig_reg.reg_party.1, avk, msg)
                .is_err()
            {
                continue;
            }
            for index in sig_reg.sig.get_concatenation_signature_indices().iter() {
                let mut insert_this_sig = false;
                if let Some(&previous_sig) = sig_by_index.get(index) {
                    let sig_to_remove_index = if sig_reg.sig.get_concatenation_signature_sigma()
                        < previous_sig.sig.get_concatenation_signature_sigma()
                    {
                        insert_this_sig = true;
                        previous_sig
                    } else {
                        sig_reg
                    };

                    if let Some(indexes) = removal_idx_by_vk.get_mut(sig_to_remove_index) {
                        indexes.push(*index);
                    } else {
                        removal_idx_by_vk.insert(sig_to_remove_index, vec![*index]);
                    }
                } else {
                    insert_this_sig = true;
                }

                if insert_this_sig {
                    sig_by_index.insert(*index, sig_reg);
                }
            }
        }

        let mut dedup_sigs: HashSet<SingleSignatureWithRegisteredParty> = HashSet::new();
        let mut count: u64 = 0;

        for (_, &sig_reg) in sig_by_index.iter() {
            if dedup_sigs.contains(sig_reg) {
                continue;
            }
            let mut deduped_sig = sig_reg.clone();
            if let Some(indexes) = removal_idx_by_vk.get(sig_reg) {
                let indices = deduped_sig
                    .sig
                    .get_concatenation_signature_indices()
                    .into_iter()
                    .filter(|i| !indexes.contains(i))
                    .collect::<Vec<Index>>();
                deduped_sig.sig.set_concatenation_signature_indices(&indices);
            }

            let size: Result<u64, _> =
                deduped_sig.sig.get_concatenation_signature_indices().len().try_into();
            if let Ok(size) = size {
                if dedup_sigs.contains(&deduped_sig) {
                    panic!("Should not reach!");
                }
                dedup_sigs.insert(deduped_sig);
                count += size;

                if count >= params.k {
                    return Ok(dedup_sigs.into_iter().collect());
                }
            }
        }
        Err(anyhow!(AggregationError::NotEnoughSignatures(
            count, params.k
        )))
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::{RngCore, SeedableRng};

    use crate::{
        Clerk, OutdatedClosedKeyRegistration, MithrilMembershipDigest, OutdatedInitializer,
        OutdatedKeyRegistration, Parameters, SingleSignatureWithRegisteredParty,
    };

    use super::AggregationError;

    type D = MithrilMembershipDigest;

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

            let mut key_registration = OutdatedKeyRegistration::init();
            let mut initializers = Vec::new();

            for i in 0..nparties {
                let stake = (i as u64 + 1) * 10;
                let initializer = OutdatedInitializer::new(params, stake, &mut rng);
                key_registration.register(initializer.stake, initializer.pk).unwrap();
                initializers.push(initializer);
            }

            let closed_registration: OutdatedClosedKeyRegistration<D> = key_registration.close();

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
