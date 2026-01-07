use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, SignerIndex, SingleSignature, StmResult,
    proof_system::ConcatenationProofSigner,
};

/// Single signature generator. Contains the signer's registration index and the signature
/// generators of each proof system. For now, it only includes the signer of concatenation proof.
#[derive(Debug, Clone)]
pub struct Signer<D: MembershipDigest> {
    /// Index of the signer in registration
    pub signer_index: SignerIndex,
    /// Single signature generation of concatenation proof system
    pub(crate) concatenation_proof_signer: ConcatenationProofSigner<D>,
    pub closed_key_registration: ClosedKeyRegistration,
    pub parameters: Parameters,
}

impl<D: MembershipDigest> Signer<D> {
    /// Creates a new single signature generator
    pub(crate) fn new(
        signer_index: SignerIndex,
        concatenation_proof_signer: ConcatenationProofSigner<D>,
        closed_key_registration: ClosedKeyRegistration,
        parameters: Parameters,
    ) -> Self {
        Self {
            signer_index,
            concatenation_proof_signer,
            closed_key_registration,
            parameters,
        }
    }

    /// Creates and returns a single signature for the given message.
    pub fn create_signle_signature(&self, message: &[u8]) -> StmResult<SingleSignature> {
        let concatenation_signature =
            self.concatenation_proof_signer.create_signle_signature(message)?;
        Ok(SingleSignature {
            concatenation_signature,
            signer_index: self.signer_index,
        })
    }
}
