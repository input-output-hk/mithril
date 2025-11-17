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
            basic::{
                ConcatenationProofBasicGenerator, ConcatenationProofBasicSingleSignatureGenerator,
            },
        },
    },
    signature_scheme::bls_signature::{BlsCryptoSigner, BlsSignature},
    *,
};

/// Concatenation proof individual full signature generator
pub struct ConcatenationProofFullSingleSignatureGenerator {
    pub basic_generator: ConcatenationProofBasicSingleSignatureGenerator,
}

impl ConcatenationProofFullSingleSignatureGenerator {
    /// Creates a new ConcatenationProofFullSingleSignatureGenerator
    pub fn new(
        signer_index: SignerIndex,
        stake: Stake,
        parameters: Parameters,
        bls_crypto_signer: BlsCryptoSigner,
        key_registration: MerkleTree<BlakeDigest, SignerRegistrationEntryConcatenation>,
    ) -> Self {
        Self {
            basic_generator: ConcatenationProofBasicSingleSignatureGenerator::new(
                signer_index,
                stake,
                parameters,
                bls_crypto_signer,
                key_registration,
            ),
        }
    }
}

impl ProofSystemSingleSignatureGenerator for ConcatenationProofFullSingleSignatureGenerator {
    type ProofSystemSingleSignature = ConcatenationSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature> {
        self.basic_generator.create_individual_signature(message)
    }
}

/// Concatenation full proof generator
pub struct ConcatenationProofFullGenerator {
    pub basic_generator: ConcatenationProofBasicGenerator,
}

impl ConcatenationProofFullGenerator {
    /// Creates a new ConcatenationProofFullGenerator
    pub fn new(parameters: &Parameters, key_registrations: &KeyRegistration) -> Self {
        todo!("Implement new for ConcatenationProofFullGenerator")
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

impl ProofSystemAggregateSignatureProver for ConcatenationProofFullGenerator {
    type ProofSystemAggregateSignature = ConcatenationProof;

    fn create_aggregate_signature(
        &self,
        message: &[u8],
        signatures: &[SingleSignature],
    ) -> StdResult<Self::ProofSystemAggregateSignature> {
        self.basic_generator.create_concatenation_proof(message, signatures)
    }
}

impl ProofSystemAggregateSignatureVerifier for ConcatenationProofFullGenerator {
    type ProofSystemAggregateSignature = ConcatenationProof;
    type ProofSystemAggregateVerificationKey = ConcatenationAggregateVerificationKey;

    fn verify_multi_signature(
        &self,
        message: &[u8],
        multi_signature: &Self::ProofSystemAggregateSignature,
        aggregate_verification_key: &Self::ProofSystemAggregateVerificationKey,
    ) -> StdResult<()> {
        multi_signature.verify(message, aggregate_verification_key)
    }
}
