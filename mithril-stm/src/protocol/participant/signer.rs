use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, SignerIndex, SingleSignature, Stake,
    StmResult, VerificationKeyForConcatenation, proof_system::ConcatenationProofSigner,
};

#[cfg(feature = "future_snark")]
use crate::{LotteryTargetValue, proof_system::SnarkProofSigner};

/// Single signature generator. Contains the signer's registration index and the signature
/// generators of each proof system. For now, it only includes the signer of concatenation proof.
#[derive(Debug, Clone)]
pub struct Signer<D: MembershipDigest> {
    /// Index of the signer in registration
    pub signer_index: SignerIndex,
    /// Single signature generation of concatenation proof system
    pub(crate) concatenation_proof_signer: ConcatenationProofSigner<D>,
    stake: Stake,
    pub closed_key_registration: ClosedKeyRegistration,
    pub parameters: Parameters,
    // TODO: remove this allow dead_code directive when snark_proof_signer is used
    #[allow(dead_code)]
    #[cfg(feature = "future_snark")]
    pub(crate) snark_proof_signer: Option<SnarkProofSigner<D>>,
}

impl<D: MembershipDigest> Signer<D> {
    /// Creates a new single signature generator
    pub(crate) fn new(
        signer_index: SignerIndex,
        concatenation_proof_signer: ConcatenationProofSigner<D>,
        closed_key_registration: ClosedKeyRegistration,
        parameters: Parameters,
        stake: Stake,
        #[cfg(feature = "future_snark")] snark_proof_signer: Option<SnarkProofSigner<D>>,
    ) -> Self {
        Self {
            signer_index,
            concatenation_proof_signer,
            stake,
            closed_key_registration,
            parameters,
            #[cfg(feature = "future_snark")]
            snark_proof_signer,
        }
    }

    /// Creates and returns a single signature for the given message.
    pub fn create_single_signature(&self, message: &[u8]) -> StmResult<SingleSignature> {
        let concatenation_signature =
            self.concatenation_proof_signer.create_single_signature(message)?;

        // TODO: Update this rng
        #[cfg(feature = "future_snark")]
        let snark_signature = if let Some(snark_signer) = &self.snark_proof_signer {
            let mut rng = rand_core::OsRng;
            Some(snark_signer.create_single_signature(message, &mut rng)?)
        } else {
            None
        };

        Ok(SingleSignature {
            concatenation_signature,
            signer_index: self.signer_index,
            #[cfg(feature = "future_snark")]
            snark_signature,
        })
    }

    /// Signing function that returns an Option type.
    pub fn sign(&self, message: &[u8]) -> Option<SingleSignature> {
        let result = self.create_single_signature(message);
        result.ok()
    }

    /// Gets the BLS verification key.
    pub fn get_bls_verification_key(&self) -> VerificationKeyForConcatenation {
        self.concatenation_proof_signer.get_verification_key()
    }

    /// Gets the stake associated with the signer.
    pub fn get_stake(&self) -> Stake {
        self.stake
    }

    #[cfg(feature = "future_snark")]
    /// Gets the lottery target value.
    pub fn get_lottery_target_value(&self) -> Option<LotteryTargetValue> {
        self.snark_proof_signer
            .as_ref()
            .map(|signer| signer.get_lottery_target_value())
    }
}
