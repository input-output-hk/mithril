#[cfg(feature = "future_snark")]
use crate::protocol::RegistrationEntryForSnark;
use crate::{
    MembershipDigest, Parameters, SchnorrSigningKey, VerificationKeyForSnark,
    membership_commitment::MerkleTree,
};

#[derive(Debug, Clone)]
pub(crate) struct SnarkProofSigner<D: MembershipDigest> {
    parameters: Parameters,
    signing_key: SchnorrSigningKey,
    verification_key: VerificationKeyForSnark,
    key_registration_commitment: MerkleTree<D::SnarkHash, RegistrationEntryForSnark>,
}

impl<D: MembershipDigest> SnarkProofSigner<D> {
    pub fn new(
        parameters: Parameters,
        signing_key: SchnorrSigningKey,
        verification_key: VerificationKeyForSnark,
        key_registration_commitment: MerkleTree<D::SnarkHash, RegistrationEntryForSnark>,
    ) -> Self {
        Self {
            parameters,
            signing_key,
            verification_key,
            key_registration_commitment,
        }
    }
}
