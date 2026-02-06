use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, Signer,
    proof_system::AggregateVerificationKeyForSnark,
};

/// The `SnarkClerk` is responsible for managing the proof system related to
/// snark signatures.
#[derive(Debug, Clone)]
pub struct SnarkClerk {
    /// The closed key registration associated with this clerk.
    pub(crate) closed_key_registration: ClosedKeyRegistration,
    /// Protocol parameters
    pub(crate) parameters: Parameters,
}

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

    /// Compute the SNARK aggregate verification key related to the used registration.
    pub fn compute_aggregate_verification_key_for_snark<D: MembershipDigest>(
        &self,
    ) -> AggregateVerificationKeyForSnark<D> {
        AggregateVerificationKeyForSnark::from(&self.closed_key_registration)
    }

    #[cfg(test)]
    pub(crate) fn update_k(&mut self, k: u64) {
        self.parameters.k = k;
    }

    #[cfg(test)]
    pub(crate) fn update_m(&mut self, m: u64) {
        self.parameters.m = m;
    }
}

// TODO: tests and golden tests
