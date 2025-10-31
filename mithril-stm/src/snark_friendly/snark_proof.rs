use super::*;

pub struct PoseidonDigest;

impl Digest for PoseidonDigest {
    fn digest(data: &[u8]) -> Vec<u8> {
        todo!("Implement Poseidon hash function")
    }
}

pub struct SnarkSingleSignature {
    pub signature: SchnorrSignature,
    pub lottery_indices: Vec<u64>, // TODO: this field could be removed or left empty and indices would be recomputed at aggregation time
}

pub struct SnarkProofSingleSignatureGenerator {
    pub signer_index: SignerIndex,
    pub stake: Stake,
    pub parameters: Parameters,
    pub schnorr_crypto_signer: SchnorrCryptoSigner,
    pub key_registration_commitment: MerkleTree<PoseidonDigest>,
}

impl SnarkProofSingleSignatureGenerator {
    pub fn new(
        signer_index: SignerIndex,
        stake: Stake,
        parameters: Parameters,
        schnorr_crypto_signer: SchnorrCryptoSigner,
        key_registration: MerkleTree<PoseidonDigest>,
    ) -> Self {
        Self {
            signer_index,
            stake,
            parameters,
            schnorr_crypto_signer,
            key_registration_commitment: key_registration,
        }
    }

    fn compute_message_prefix(&self) -> Vec<u8> {
        todo!("Implement message prefix computation for SNARK proof")
    }
}

impl ProofSystemSingleSignatureGenerator for SnarkProofSingleSignatureGenerator {
    type ProofSystemSingleSignature = SnarkSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature> {
        let mut message_to_sign = self.compute_message_prefix();
        message_to_sign.extend_from_slice(message);
        // Signer registration => Reveal of signer registration with Merkle proof
        todo!("Implement snark proof individual signature generation");
    }
}

pub struct SnarkProof {}

pub struct SnarkProofGenerator {
    pub parameters: Parameters,
    pub snark_proof_individual_signature_generator: SnarkProofSingleSignatureGenerator,
    pub key_registration: MerkleTree<PoseidonDigest>,
}

impl SnarkProofGenerator {
    pub fn new(parameters: &Parameters, key_registrations: &KeyRegistration) -> Self {
        todo!("Implement new for SnarkProofGenerator")
    }

    pub fn create_snark_proof(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<SnarkProof> {
        // Implement snark proof creation logic here
        todo!("Implement snark proof creation")
    }

    pub fn verify(&self) -> StdResult<bool> {
        // Implement snark proof verification logic here
        todo!("Implement snark proof verification")
    }
}
