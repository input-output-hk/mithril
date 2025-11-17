use crate::{
    commitment_scheme::merkle_tree::MerkleTree,
    core::{
        Digest, Parameters, SignerIndex, Stake,
        key_registration::{KeyRegistration, SignerRegistrationEntryConcatenation},
        single_signature::SingleSignature,
    },
    proof_system::{
        ProofSystemAggregateSignatureProver, ProofSystemAggregateSignatureVerifier,
        ProofSystemSingleSignatureGenerator,
        concatenation_proof::{
            BlakeDigest, ConcatenationAggregateVerificationKey, ConcatenationProof,
            ConcatenationSingleSignature,
        },
    },
    signature_scheme::bls_signature::{BlsCryptoSigner, BlsSignature},
    *,
};

/// Concatenation proof individual basic signature generator
pub struct ConcatenationProofBasicSingleSignatureGenerator {
    pub signer_index: SignerIndex,
    pub stake: Stake,
    pub parameters: Parameters,
    pub bls_crypto_signer: BlsCryptoSigner,
    pub key_registration_commitment: MerkleTree<BlakeDigest, SignerRegistrationEntryConcatenation>,
}

impl ConcatenationProofBasicSingleSignatureGenerator {
    /// Creates a new ConcatenationProofBasicSingleSignatureGenerator
    pub fn new(
        signer_index: SignerIndex,
        stake: Stake,
        parameters: Parameters,
        bls_crypto_signer: BlsCryptoSigner,
        key_registration: MerkleTree<BlakeDigest, SignerRegistrationEntryConcatenation>,
    ) -> Self {
        Self {
            signer_index,
            stake,
            parameters,
            bls_crypto_signer,
            key_registration_commitment: key_registration,
        }
    }

    /// Computes the message prefix for concatenation proof
    fn compute_message_prefix(&self) -> Vec<u8> {
        todo!("Implement message prefix computation for concatenation proof")
    }
}

impl ProofSystemSingleSignatureGenerator for ConcatenationProofBasicSingleSignatureGenerator {
    type ProofSystemSingleSignature = ConcatenationSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature> {
        let message = {
            let mut msg = self.compute_message_prefix();
            msg.extend_from_slice(message);
            msg
        };
        todo!("Implement concatenation proof individual signature generation")
    }
}

/// Concatenation basic proof generator
pub struct ConcatenationProofBasicGenerator {
    pub parameters: Parameters,
    pub concatenation_proof_individual_signature_generator:
        ConcatenationProofBasicSingleSignatureGenerator,
    pub key_registration: MerkleTree<BlakeDigest, SignerRegistrationEntryConcatenation>,
}

impl ConcatenationProofBasicGenerator {
    /// Creates a new ConcatenationProofBasicGenerator
    pub fn new(parameters: &Parameters, key_registrations: &KeyRegistration) -> Self {
        todo!("Implement new for ConcatenationProofGenerator")
    }

    /// Creates a concatenation proof from the given signatures
    pub fn create_concatenation_proof(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<ConcatenationProof> {
        // Implement concatenation proof creation logic here
        todo!("Implement concatenation proof creation")
    }
}

impl ProofSystemAggregateSignatureProver for ConcatenationProofBasicGenerator {
    type ProofSystemAggregateSignature = ConcatenationProof;

    fn create_aggregate_signature(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<Self::ProofSystemAggregateSignature> {
        self.create_concatenation_proof(message, signatures)
    }
}

impl ProofSystemAggregateSignatureVerifier for ConcatenationProofBasicGenerator {
    type ProofSystemAggregateSignature = ConcatenationProof;
    type ProofSystemAggregateVerificationKey = ConcatenationAggregateVerificationKey;

    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &Self::ProofSystemAggregateSignature,
        aggregate_verification_key: &Self::ProofSystemAggregateVerificationKey,
    ) -> StdResult<()> {
        multi_signature.verify_basic(message, aggregate_verification_key)
    }
}
