use crate::*;

/// Represents a signer registration entry
#[derive(PartialEq, Eq, Clone)]
pub struct SignerRegistration {
    pub stake: Stake,
    pub bls_public_key_proof_of_possession: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key_proof_of_possession: Option<JubjubVerificationKeyProofOfPossession>,
}

/// The type used for committing signer registrations for the Concatenation proof system.
type SignerRegistrationEntryConcatenation = (BlsVerificationKey, Stake);

impl MerkleTreeLeaf for SignerRegistrationEntryConcatenation {
    fn to_bytes(&self) -> Vec<u8> {
        Vec::new()
    }
}

/// The type used for committing signer registrations for the SNARK proof system.
type SignerRegistrationEntrySnark = (JubjubVerificationKey, EligibilityValue);

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

    pub fn into_merkle_tree_for_concatenation<D: Digest>(self) -> StdResult<MerkleTree<D>> {
        Ok(MerkleTree::new(
            &self
                .signer_registrations
                .iter()
                .map(|signer_registration| {
                    (
                        signer_registration
                            .bls_public_key_proof_of_possession
                            .clone()
                            .into_verification_key(),
                        signer_registration.stake.clone(),
                    )
                })
                .collect::<Vec<SignerRegistrationEntryConcatenation>>(),
        ))
    }

    #[cfg(feature = "future_snark")]
    pub fn into_merkle_tree_for_snark<D: Digest>(self) -> StdResult<MerkleTree<D>> {
        Ok(MerkleTree::new(
            &self
                .signer_registrations
                .iter()
                .map(|signer_registration| {
                    (
                        signer_registration
                            .schnorr_public_key_proof_of_possession
                            .clone()
                            .map(|pk| pk.into_verification_key())
                            .unwrap(),
                        EligibilityValue::compute_eligibility_value(
                            signer_registration.stake.clone(),
                        ),
                    )
                })
                .collect::<Vec<SignerRegistrationEntrySnark>>(),
        ))
    }

    #[cfg(feature = "future_snark")]
    // In case we need it for recursive snarks
    pub fn into_pedersen_commitment(self) -> PedersenCommitment {
        todo!("Implement conversion of KeyRegistration into PedersenCommitment")
    }
}
