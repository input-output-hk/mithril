use crate::{
    ClosedKeyRegistration, MembershipDigest, Parameters, SignerIndex, SingleSignature, Stake,
    StmResult, VerificationKeyForConcatenation, proof_system::ConcatenationProofSigner,
};

#[cfg(feature = "future_snark")]
use crate::{LotteryTargetValue, VerificationKeyForSnark, proof_system::SnarkProofSigner};

/// Single signature generator. Contains the signer's registration index and the signature
/// generators of each proof system.
#[derive(Debug, Clone)]
pub struct Signer<D: MembershipDigest> {
    /// Index of the signer in registration
    pub signer_index: SignerIndex,
    /// Single signature generator of concatenation proof system
    pub(crate) concatenation_proof_signer: ConcatenationProofSigner<D>,
    /// Stake associated with the signer
    stake: Stake,
    /// Closed key registration containing closed registration entries and total stake
    pub closed_key_registration: ClosedKeyRegistration,
    /// Parameters of the protocol
    pub parameters: Parameters,
    /// Single signature generator of the future snark proof system
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

    /// Creates and returns a single signature for the given message. First tries to create a
    /// concatenation proof signature. Tries to create a snark proof signature as well if
    /// * Concatenation proof signature creation is successful,
    /// * the future snark proof system is enabled,
    /// * a snark proof signer is available.
    pub fn create_single_signature(&self, message: &[u8]) -> StmResult<SingleSignature> {
        let concatenation_signature =
            self.concatenation_proof_signer.create_single_signature(message)?;

        // TODO: Update this rng. We might want to use a different RNG for the snark signature generation, or pass it as an argument to the function.
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
    /// Gets the lottery target value.
    #[cfg(feature = "future_snark")]
    pub fn get_lottery_target_value(&self) -> Option<LotteryTargetValue> {
        self.snark_proof_signer
            .as_ref()
            .map(|signer| signer.get_lottery_target_value())
    }

    /// Gets the Schnorr verification key.    
    #[cfg(feature = "future_snark")]
    pub fn get_schnorr_verification_key(&self) -> Option<VerificationKeyForSnark> {
        self.snark_proof_signer
            .as_ref()
            .map(|signer| signer.get_verification_key())
    }
}
