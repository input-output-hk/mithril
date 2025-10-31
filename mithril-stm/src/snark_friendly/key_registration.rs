use super::*;

#[derive(Clone)]
pub struct KeyRegistration {
    signer_registrations: Vec<SignerRegistration>,
}

impl KeyRegistration {
    pub fn new(signer_registrations: Vec<SignerRegistration>) -> Self {
        Self {
            signer_registrations,
        }
    }

    pub fn get_signer_index_for_registration(
        &self,
        signer_registration: &SignerRegistration,
    ) -> Option<SignerIndex> {
        self.signer_registrations
            .iter()
            .position(|r| r == signer_registration)
            .map(|s| s.into())
    }

    pub fn into_merkle_tree<D: Digest>(self) -> MerkleTree<D> {
        todo!("Implement conversion of KeyRegistration into MerkleTree")
    }

    #[cfg(feature = "future_snark")]
    // In case we need it for reccursive snarks
    pub fn into_pedersen_commitment(self) -> PedersenCommitment {
        todo!("Implement conversion of KeyRegistration into PedersenCommitment")
    }
}
