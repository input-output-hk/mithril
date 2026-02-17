use crate::{
    ClosedKeyRegistration, MembershipDigest, proof_system::AggregateVerificationKeyForConcatenation,
};

#[cfg(feature = "future_snark")]
use crate::proof_system::AggregateVerificationKeyForSnark;

/// Aggregate verification key
#[derive(Debug, Clone)]
pub struct AggregateVerificationKey<D: MembershipDigest> {
    /// Concatenation aggregate verification key.
    concatenation_aggregate_verification_key: AggregateVerificationKeyForConcatenation<D>,
    #[cfg(feature = "future_snark")]
    snark_aggregate_verification_key: Option<AggregateVerificationKeyForSnark<D>>,
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    pub fn new(
        concatenation_aggregate_verification_key: AggregateVerificationKeyForConcatenation<D>,
        #[cfg(feature = "future_snark")] snark_aggregate_verification_key: Option<
            AggregateVerificationKeyForSnark<D>,
        >,
    ) -> Self {
        Self {
            concatenation_aggregate_verification_key,
            #[cfg(feature = "future_snark")]
            snark_aggregate_verification_key,
        }
    }

    /// Returns the concatenation aggregate verification key
    pub fn to_concatenation_aggregate_verification_key(
        &self,
    ) -> &AggregateVerificationKeyForConcatenation<D> {
        &self.concatenation_aggregate_verification_key
    }

    /// Returns the snark aggregate verification key
    #[cfg(feature = "future_snark")]
    pub fn to_snark_aggregate_verification_key(
        &self,
    ) -> Option<&AggregateVerificationKeyForSnark<D>> {
        self.snark_aggregate_verification_key.as_ref()
    }
}

impl<D: MembershipDigest> PartialEq for AggregateVerificationKey<D> {
    fn eq(&self, other: &Self) -> bool {
        self.to_concatenation_aggregate_verification_key()
            == other.to_concatenation_aggregate_verification_key()
    }
}

impl<D: MembershipDigest> Eq for AggregateVerificationKey<D> {}

impl<D: MembershipDigest> From<&ClosedKeyRegistration> for AggregateVerificationKey<D> {
    fn from(reg: &ClosedKeyRegistration) -> Self {
        AggregateVerificationKey {
            concatenation_aggregate_verification_key:
                AggregateVerificationKeyForConcatenation::from(reg),
            #[cfg(feature = "future_snark")]
            snark_aggregate_verification_key: Some(AggregateVerificationKeyForSnark::from(reg)),
        }
    }
}
