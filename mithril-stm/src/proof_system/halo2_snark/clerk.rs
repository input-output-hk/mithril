use std::collections::{BTreeMap, btree_map::Entry};

use crate::{
    AggregationError, ClosedKeyRegistration, LotteryIndex, MembershipDigest, Parameters,
    RegistrationEntryForSnark, Signer, SingleSignature, StmResult,
};

use super::AggregateVerificationKeyForSnark;

/// Clerk for managing the SNARK proof system.
///
/// Responsible for computing the SNARK aggregate verification key from
/// a closed key registration. This is the SNARK counterpart of the
/// `ConcatenationClerk`.
#[derive(Debug, Clone)]
pub struct SnarkClerk {
    /// The closed key registration associated with this clerk.
    pub(crate) closed_key_registration: ClosedKeyRegistration,
    /// Protocol parameters
    pub(crate) parameters: Parameters,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SnarkClerk {
    /// Create a new `SnarkClerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        parameters: &Parameters,
        closed_key_registration: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            parameters: *parameters,
            closed_key_registration: closed_key_registration.clone(),
        }
    }

    /// Create a `SnarkClerk` from a signer.
    pub fn new_clerk_from_signer<D: MembershipDigest>(signer: &Signer<D>) -> Self {
        Self {
            parameters: signer.parameters,
            closed_key_registration: signer.closed_key_registration.clone(),
        }
    }

    /// Compute the `AggregateVerificationKeyForSnark` related to the used registration.
    pub fn compute_aggregate_verification_key_for_snark<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKeyForSnark<D> {
        AggregateVerificationKeyForSnark::from(&self.closed_key_registration)
    }

    /// Get the SNARK registration entry for a given signer index.
    pub fn get_snark_registration_entry(
        &self,
        signer_index: LotteryIndex,
    ) -> StmResult<Option<RegistrationEntryForSnark>> {
        let closed_registration_entry = self
            .closed_key_registration
            .get_registration_entry_for_index(&signer_index)?;
        Ok(closed_registration_entry.into())
    }

    /// Deduplicates signatures by lottery index, keeping the one with the smallest schnorr
    /// signature for each index. Returns exactly `k` entries (sorted by lottery index).
    /// # Error
    /// Returns `AggregationError::NotEnoughSignatures` if fewer than `k` unique indices exist.
    pub(crate) fn select_valid_signatures_for_k_indices(
        parameters: &Parameters,
        signatures: &[SingleSignature],
    ) -> StmResult<BTreeMap<LotteryIndex, SingleSignature>> {
        let mut unique_index_signature_map: BTreeMap<LotteryIndex, SingleSignature> =
            BTreeMap::new();

        for signature in signatures {
            let (Some(indices), Some(snark_sig)) = (
                signature.get_snark_signature_indices(),
                signature.snark_signature.as_ref(),
            ) else {
                continue;
            };

            for index in indices {
                match unique_index_signature_map.entry(index) {
                    Entry::Occupied(mut existing) => {
                        if existing.get().snark_signature.as_ref().is_some_and(|s| {
                            s.get_schnorr_signature() > snark_sig.get_schnorr_signature()
                        }) {
                            existing.insert(signature.clone());
                        }
                    }
                    Entry::Vacant(vacant) => {
                        vacant.insert(signature.clone());
                    }
                }
            }
        }

        let count = unique_index_signature_map.len() as u64;
        if count < parameters.k {
            return Err(AggregationError::NotEnoughSignatures(count, parameters.k).into());
        }

        while unique_index_signature_map.len() as u64 > parameters.k {
            unique_index_signature_map.pop_last();
        }

        Ok(unique_index_signature_map)
    }
}

#[cfg(test)]
mod tests {

    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Initializer, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
        proof_system::{SnarkClerk, halo2_snark::AggregateVerificationKeyForSnark},
    };

    type D = MithrilMembershipDigest;

    proptest! {
        #![proptest_config(ProptestConfig::with_cases(50))]

        #[test]
        fn compute_snark_avk(
            seed in any::<[u8; 32]>(),
            number_of_parties in 1_usize..10,
            m in 1_u64..20,
            k in 1_u64..10,
            phi_f in 0.1_f64..1.0,
        ) {
            let parameters = Parameters { m, k, phi_f };
            let mut rng = ChaCha20Rng::from_seed(seed);

            let mut key_registration = KeyRegistration::initialize();
            let mut initializers = Vec::new();

            for i in 0..number_of_parties {
                let stake = (i as u64 + 1) * 10;
                let initializer = Initializer::new(parameters, stake, &mut rng);
                let entry = RegistrationEntry::new(
                    initializer.get_verification_key_proof_of_possession_for_concatenation(),
                    initializer.stake,
                    #[cfg(feature = "future_snark")]
                    initializer.schnorr_verification_key,
                )
                .unwrap();
                key_registration.register_by_entry(&entry).unwrap();
                initializers.push(initializer);
            }

            let closed_registration = key_registration.close_registration(&parameters).unwrap();

            let signers: Vec<_> = initializers
                .into_iter()
                .map(|init| init.try_create_signer::<D>(&closed_registration).unwrap())
                .collect();

            let clerk_from_registration =
                SnarkClerk::new_clerk_from_closed_key_registration(&parameters, &closed_registration);
            let clerk_from_signer = SnarkClerk::new_clerk_from_signer::<D>(&signers[0]);

            let avk_from_registration: AggregateVerificationKeyForSnark<D> =
                clerk_from_registration.compute_aggregate_verification_key_for_snark();
            let avk_from_signer: AggregateVerificationKeyForSnark<D> =
                clerk_from_signer.compute_aggregate_verification_key_for_snark();

            let expected_total_stake: u64 = (1..=number_of_parties as u64).map(|i| i * 10).sum();
            prop_assert_eq!(avk_from_registration.get_total_stake(), expected_total_stake);
            prop_assert_eq!(&avk_from_registration, &avk_from_signer);

            let bytes = avk_from_registration.to_bytes();
            let deserialized = AggregateVerificationKeyForSnark::<D>::from_bytes(&bytes)
                .expect("deserialization should succeed");
            prop_assert_eq!(&avk_from_registration, &deserialized);

            let avk_second: AggregateVerificationKeyForSnark<D> =
                clerk_from_registration.compute_aggregate_verification_key_for_snark();
            prop_assert_eq!(&avk_from_registration, &avk_second);
        }
    }
}
