use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, Signer,
    proof_system::AggregateVerificationKeyForSnark,
};

/// Clerk for managing the SNARK proof system.
///
/// Responsible for computing the SNARK aggregate verification key from
/// a closed key registration. This is the SNARK counterpart of the
/// `ConcatenationClerk`.
#[derive(Debug, Clone)]
pub struct SnarkClerk {
    /// The closed key registration associated with this clerk.
    pub(crate) closed_key_registration: ClosedKeyRegistration,
}

impl SnarkClerk {
    /// Create a new `SnarkClerk` from a closed registration instance.
    pub fn new_clerk_from_closed_key_registration(
        _parameters: &Parameters,
        closed_key_registration: &ClosedKeyRegistration,
    ) -> Self {
        Self {
            closed_key_registration: closed_key_registration.clone(),
        }
    }

    /// Create a `SnarkClerk` from a signer.
    pub fn new_clerk_from_signer<D: MembershipDigest>(signer: &Signer<D>) -> Self {
        Self {
            closed_key_registration: signer.closed_key_registration.clone(),
        }
    }

    /// Compute the SNARK aggregate verification key from the closed registration.
    pub fn compute_aggregate_verification_key_for_snark<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKeyForSnark<D> {
        AggregateVerificationKeyForSnark::from(&self.closed_key_registration)
    }
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;
    use rand_chacha::ChaCha20Rng;
    use rand_core::SeedableRng;

    use crate::{
        Initializer, KeyRegistration, MithrilMembershipDigest, Parameters, RegistrationEntry,
    };

    use super::*;

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

            let closed_registration = key_registration.close_registration();

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
            prop_assert_eq!(avk_from_registration.get_target_value(), expected_total_stake);
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
