use crate::{
    commitment_scheme::merkle_tree::{MerkleTree, MerkleTreeLeaf},
    core::{Digest, SignerIndex, Stake, eligibility::EligibilityValue, signer},
    signature_scheme::{
        bls_signature::{BlsVerificationKey, BlsVerificationKeyProofOfPossession},
        schnorr_signature::JubjubVerificationKey,
    },
    *,
};
#[cfg(feature = "future_snark")]
use crate::{
    commitment_scheme::pedersen_commitment::PedersenCommitment,
    signature_scheme::schnorr_signature::JubjubVerificationKeyProofOfPossession,
};

/// Represents a signer registration entry
#[derive(PartialEq, Eq, Clone)]
pub struct SignerRegistration {
    pub stake: Stake,
    pub bls_public_key_proof_of_possession: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key_proof_of_possession: Option<JubjubVerificationKeyProofOfPossession>,
}

impl From<SignerRegistration> for SignerRegistrationEntryConcatenation {
    fn from(signer_registration: SignerRegistration) -> Self {
        (
            signer_registration
                .bls_public_key_proof_of_possession
                .into_verification_key(),
            signer_registration.stake,
        )
    }
}

#[cfg(feature = "future_snark")]
impl From<SignerRegistration> for SignerRegistrationEntrySnark {
    fn from(signer_registration: SignerRegistration) -> Self {
        (
            signer_registration
                .schnorr_public_key_proof_of_possession
                .clone()
                .map(|pk| pk.into_verification_key())
                .unwrap(),
            EligibilityValue::compute_eligibility_value(signer_registration.stake),
        )
    }
}

/// The type used for committing signer registrations for the Concatenation proof system.
pub type SignerRegistrationEntryConcatenation = (BlsVerificationKey, Stake);

impl MerkleTreeLeaf for SignerRegistrationEntryConcatenation {
    fn to_bytes(&self) -> Vec<u8> {
        Vec::new()
    }
}

/// The type used for committing signer registrations for the SNARK proof system.
pub type SignerRegistrationEntrySnark = (JubjubVerificationKey, EligibilityValue);

impl MerkleTreeLeaf for SignerRegistrationEntrySnark {
    fn to_bytes(&self) -> Vec<u8> {
        Vec::new()
    }
}

/// The type used for registering signer keys
#[derive(Clone)]
pub struct KeyRegistration {
    signer_registrations: Vec<SignerRegistration>,
}

impl KeyRegistration {
    /// Creates a new KeyRegistration
    pub fn new(signer_registrations: Vec<SignerRegistration>) -> Self {
        Self {
            signer_registrations,
        }
    }

    /// Gets the index of a signer registration
    pub fn get_signer_index_for_registration(
        &self,
        signer_registration: &SignerRegistration,
    ) -> Option<SignerIndex> {
        self.signer_registrations
            .iter()
            .position(|r| r == signer_registration)
            .map(|s| s.into())
    }

    /// Converts the KeyRegistration into a Merkle tree
    pub fn into_merkle_tree<D: Digest, L: From<SignerRegistration> + MerkleTreeLeaf>(
        self,
    ) -> StdResult<MerkleTree<D, L>> {
        Ok(MerkleTree::new(
            &self
                .signer_registrations
                .iter()
                .map(|signer_registration| signer_registration.clone().into())
                .collect::<Vec<L>>(),
        ))
    }

    /// Converts the KeyRegistration into a Pedersen commitment for the SNARK proof system (as an example if we need it later for recursive snarks)
    #[cfg(feature = "future_snark")]
    pub fn into_pedersen_commitment(self) -> PedersenCommitment {
        todo!("Implement conversion of KeyRegistration into PedersenCommitment")
    }
}
