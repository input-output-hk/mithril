use crate::*;

#[derive(PartialEq, Eq, Clone)]
pub struct SignerRegistration {
    pub stake: Stake,
    pub bls_public_key: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key: Option<JubjubVerificationKeyProofOfPossession>,
}

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

    pub fn into_merkle_tree_for_concatenation<D: Digest>(self) -> MerkleTree<D> {
        // Uses (bls_verification_key,stake) as leaves
        todo!("Implement conversion of KeyRegistration into MerkleTree for Concatenation proof")
    }

    #[cfg(feature = "future_snark")]
    pub fn into_merkle_tree_for_snark<D: Digest>(self) -> MerkleTree<D> {
        // Uses (jubjub_verification_key,ev) as leaves
        todo!("Implement conversion of KeyRegistration into MerkleTree for SNARK proof")
    }

    #[cfg(feature = "future_snark")]
    // In case we need it for recursive snarks
    pub fn into_pedersen_commitment(self) -> PedersenCommitment {
        todo!("Implement conversion of KeyRegistration into PedersenCommitment")
    }
}
