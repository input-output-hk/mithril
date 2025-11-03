use anyhow::anyhow;

use crate::snark_friendly::*;

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

    pub fn try_create_signer(&self, key_registration: KeyRegistration) -> StdResult<Signer> {
        let signer_index = key_registration
            .get_signer_index_for_registration(&SignerRegistration {
                stake: self.stake.clone(),
                bls_public_key: self.bls_public_key.clone(),
                #[cfg(feature = "future_snark")]
                schnorr_public_key: self.schnorr_public_key.clone(),
            })
            .ok_or(anyhow!("Signer registration not found in key registration"))?;
        let concatenation_proof_individual_signature_generator =
            ConcatenationProofSingleSignatureGenerator::new(
                signer_index.clone(),
                self.stake.clone(),
                self.parameters.clone(),
                BlsCryptoSigner::new(self.bls_signing_key.clone()),
                key_registration.clone().into_merkle_tree_for_concatenation(),
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
                    key_registration.clone().into_merkle_tree_for_snark(),
                ))
            } else {
                None
            };

        Ok(Signer::new(
            signer_index.clone(),
            self.stake.clone(),
            self.parameters.clone(),
            BlsCryptoSigner::new(self.bls_signing_key.clone()),
            concatenation_proof_individual_signature_generator,
            #[cfg(feature = "future_snark")]
            snark_proof_individual_signature_generator,
        ))
    }
}
