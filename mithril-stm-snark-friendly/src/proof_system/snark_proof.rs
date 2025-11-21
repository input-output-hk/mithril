use crate::{
    commitment_scheme::{MembershipCommitmentConstraints, merkle_tree::MerkleTree},
    core::{
        Digest, Parameters, SignerIndex, Stake,
        key_registration::{KeyRegistration, SignerRegistrationEntrySnark},
        single_signature::SingleSignature,
    },
    proof_system::{
        ProofSystemAggregateSignatureProver, ProofSystemAggregateSignatureVerifier,
        ProofSystemSingleSignatureGenerator,
    },
    signature_scheme::schnorr_signature::{JubjubSignature, SchnorrCryptoSigner},
    *,
};

/// Poseidon hash function digest
pub struct PoseidonDigest;

impl Digest for PoseidonDigest {
    fn digest(data: &[u8]) -> Vec<u8> {
        todo!("Implement Poseidon hash function")
    }
}

/// SNARK proof individual signature structure
pub struct SnarkSingleSignature {
    pub signature: JubjubSignature,
    pub lottery_indices: Vec<u64>, // TODO: this field could be removed or left empty and indices would be recomputed at aggregation time
}

/// SNARK proof individual signature generator
pub struct SnarkProofSingleSignatureGenerator<C: MembershipCommitmentConstraints> {
    pub signer_index: SignerIndex,
    pub stake: Stake,
    pub parameters: Parameters,
    pub schnorr_crypto_signer: SchnorrCryptoSigner,
    pub key_registration_commitment: MerkleTree<C::SnarkHash, C::SnarkCommittedData>,
}

impl<C: MembershipCommitmentConstraints> SnarkProofSingleSignatureGenerator<C> {
    /// Creates a new SnarkProofSingleSignatureGenerator
    pub fn new(
        signer_index: SignerIndex,
        stake: Stake,
        parameters: Parameters,
        schnorr_crypto_signer: SchnorrCryptoSigner,
        key_registration: MerkleTree<C::SnarkHash, C::SnarkCommittedData>,
    ) -> Self {
        Self {
            signer_index,
            stake,
            parameters,
            schnorr_crypto_signer,
            key_registration_commitment: key_registration,
        }
    }

    /// Computes the message prefix for SNARK proof
    fn compute_message_prefix(&self) -> Vec<u8> {
        todo!("Implement message prefix computation for SNARK proof")
    }
}

impl<C: MembershipCommitmentConstraints> ProofSystemSingleSignatureGenerator
    for SnarkProofSingleSignatureGenerator<C>
{
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

/// Aggregate verification key for SNARK proof
pub type SnarkAggregateVerificationKey = (); // TODO: to be defined

/// SNARK proof structure
pub struct SnarkProof {}

impl SnarkProof {
    /// Verifies the SNARK proof
    pub fn verify(
        &self,
        message: &[u8],
        verification_key: &SnarkAggregateVerificationKey,
    ) -> StdResult<()> {
        // Implement verification logic here
        todo!("Implement SNARK proof verification")
    }
}

/// SNARK proof generator
pub struct SnarkProofGenerator<C: MembershipCommitmentConstraints> {
    pub parameters: Parameters,
    pub snark_proof_individual_signature_generator: SnarkProofSingleSignatureGenerator<C>,
    pub key_registration: MerkleTree<C::SnarkHash, C::SnarkCommittedData>,
}

impl<C: MembershipCommitmentConstraints> SnarkProofGenerator<C> {
    /// Creates a new SnarkProofGenerator
    pub fn new(parameters: &Parameters, key_registrations: &KeyRegistration) -> Self {
        todo!("Implement new for SnarkProofGenerator")
    }

    /// Creates a SNARK proof from the given signatures
    pub fn create_snark_proof(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<SnarkProof> {
        // Implement snark proof creation logic here
        todo!("Implement SNARK proof creation")
    }
}

impl<C: MembershipCommitmentConstraints> ProofSystemAggregateSignatureProver
    for SnarkProofGenerator<C>
{
    type ProofSystemAggregateSignature = SnarkProof;

    fn create_aggregate_signature(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<Self::ProofSystemAggregateSignature> {
        self.create_snark_proof(message, signatures)
    }
}

impl<C: MembershipCommitmentConstraints> ProofSystemAggregateSignatureVerifier
    for SnarkProofGenerator<C>
{
    type ProofSystemAggregateSignature = SnarkProof;
    type ProofSystemAggregateVerificationKey = SnarkAggregateVerificationKey;

    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &Self::ProofSystemAggregateSignature,
        aggregate_verification_key: &Self::ProofSystemAggregateVerificationKey,
    ) -> StdResult<()> {
        multi_signature.verify(message, aggregate_verification_key)
    }
}
