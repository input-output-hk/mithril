use anyhow::anyhow;

use super::*;

pub struct Initializer {
    pub stake: Stake,
    pub parameters: Parameters,
    pub bls_signing_key: BlsSigningKey,
    pub bls_public_key: BlsVerificationKeyProofOfPossession,
    #[cfg(feature = "future_snark")]
    pub schnorr_signing_key: Option<SchnorrSigningKey>,
    #[cfg(feature = "future_snark")]
    pub schnorr_public_key: Option<SchnorrVerificationKeyProofOfPossession>,
}

impl Initializer {
    pub fn new(
        stake: Stake,
        parameters: Parameters,
        bls_signing_key: BlsSigningKey,
        bls_public_key: BlsVerificationKeyProofOfPossession,
        #[cfg(feature = "future_snark")] schnorr_signing_key: Option<SchnorrSigningKey>,
        #[cfg(feature = "future_snark")] schnorr_public_key: Option<
            SchnorrVerificationKeyProofOfPossession,
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
                todo!(),
                self.stake.clone(),
                self.parameters.clone(),
                BlsCryptoSigner::new(self.bls_signing_key.clone()),
                key_registration.clone().into_merkle_tree(),
            );
        #[cfg(feature = "future_snark")]
        let snark_proof_individual_signature_generator =
            if let (Some(schnorr_signing_key), Some(_schnorr_public_key)) =
                (&self.schnorr_signing_key, &self.schnorr_public_key)
            {
                Some(SnarkProofSingleSignatureGenerator::new(
                    todo!(),
                    self.stake.clone(),
                    self.parameters.clone(),
                    SchnorrCryptoSigner::new(schnorr_signing_key.clone()),
                    key_registration.clone().into_merkle_tree(),
                ))
            } else {
                None
            };

        Ok(Signer::new(
            todo!(),
            self.stake.clone(),
            self.parameters.clone(),
            BlsCryptoSigner::new(self.bls_signing_key.clone()),
            concatenation_proof_individual_signature_generator,
            #[cfg(feature = "future_snark")]
            snark_proof_individual_signature_generator,
        ))
    }
}
