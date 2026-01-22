use crate::{
    ClosedKeyRegistration, MembershipDigest, proof_system::AggregateVerificationKeyForConcatenation,
};

/// Aggregate verification key
#[derive(Debug, Clone)]
pub struct AggregateVerificationKey<D: MembershipDigest> {
    /// Concatenation aggregate verification key.
    concatenation_aggregate_verification_key: AggregateVerificationKeyForConcatenation<D>,
}

impl<D: MembershipDigest> AggregateVerificationKey<D> {
    pub fn new(
        concatenation_aggregate_verification_key: AggregateVerificationKeyForConcatenation<D>,
    ) -> Self {
        Self {
            concatenation_aggregate_verification_key,
        }
    }

    /// Returns the concatenation aggregate verification key
    pub fn to_concatenation_aggregate_verification_key(
        &self,
    ) -> &AggregateVerificationKeyForConcatenation<D> {
        &self.concatenation_aggregate_verification_key
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
        }
    }
}
