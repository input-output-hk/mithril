use anyhow::anyhow;

use crate::{
    core::{
        Parameters, Stake,
        key_registration::{KeyRegistration, SignerRegistration},
        signer::Signer,
    },
    proof_system::concatenation_proof::full::ConcatenationProofFullSingleSignatureGenerator,
    signature_scheme::bls_signature::{
        BlsCryptoSigner, BlsSigningKey, BlsVerificationKeyProofOfPossession,
    },
    *,
};
#[cfg(feature = "future_snark")]
use crate::{
    proof_system::snark_proof::SnarkProofSingleSignatureGenerator,
    signature_scheme::schnorr_signature::SchnorrCryptoSigner,
    signature_scheme::schnorr_signature::{
        JubjubSigningKey, JubjubVerificationKeyProofOfPossession,
    },
};

/// The initializer for a signer
pub struct Initializer {
    pub stake: Stake,
    pub parameters: Parameters,
    pub bls_signing_key: BlsSigningKey,
    pub bls_public_key: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_signing_key: Option<JubjubSigningKey>,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key: Option<JubjubVerificationKeyProofOfPossession>,
}

impl Initializer {
    /// Creates a new signer initializer
    pub fn new(
        stake: Stake,
        parameters: Parameters,
        bls_signing_key: BlsSigningKey,
        bls_public_key: BlsVerificationKeyProofOfPossession,
        #[cfg(feature = "future_snark")] schnorr_signing_key: Option<JubjubSigningKey>,
        #[cfg(feature = "future_snark")] schnorr_public_key: Option<
            JubjubVerificationKeyProofOfPossession,
        >,
    ) -> Self {
        Self {
            stake,
            parameters,
            bls_signing_key,
            bls_public_key,
            #[cfg(feature = "future_snark")]
            schnorr_signing_key,
            #[cfg(feature = "future_snark")]
            schnorr_public_key,
        }
    }

    /// Tries to create a signer from the given key registration
    pub fn try_create_signer(&self, key_registration: KeyRegistration) -> StdResult<Signer> {
        let signer_index = key_registration
            .get_signer_index_for_registration(&SignerRegistration {
                stake: self.stake.clone(),
                bls_public_key_proof_of_possession: self.bls_public_key.clone(),
                #[cfg(feature = "future_snark")]
                schnorr_public_key_proof_of_possession: self.schnorr_public_key.clone(),
            })
            .ok_or(anyhow!("Signer registration not found in key registration"))?;
        let concatenation_proof_individual_signature_generator =
            ConcatenationProofFullSingleSignatureGenerator::new(
                signer_index.clone(),
                self.stake.clone(),
                self.parameters.clone(),
                BlsCryptoSigner::new(self.bls_signing_key.clone()),
                key_registration.clone().into_merkle_tree()?,
            );
        #[cfg(feature = "future_snark")]
        let snark_proof_individual_signature_generator =
            if let (Some(schnorr_signing_key), Some(_schnorr_public_key)) =
                (&self.schnorr_signing_key, &self.schnorr_public_key)
            {
                Some(SnarkProofSingleSignatureGenerator::new(
                    signer_index.clone(),
                    self.stake.clone(),
                    self.parameters.clone(),
                    SchnorrCryptoSigner::new(schnorr_signing_key.clone()),
                    key_registration.clone().into_merkle_tree()?,
                ))
            } else {
                None
            };

        Ok(Signer::new(
            signer_index.clone(),
            self.stake.clone(),
            self.parameters.clone(),
            concatenation_proof_individual_signature_generator,
            #[cfg(feature = "future_snark")]
            snark_proof_individual_signature_generator,
        ))
    }
}
