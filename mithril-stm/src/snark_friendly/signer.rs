use crate::snark_friendly::*;

pub struct Signer {
    pub signer_index: SignerIndex,
    pub stake: Stake,
    pub parameters: Parameters,
    pub concatenation_proof_individual_signature_generator:
        ConcatenationProofSingleSignatureGenerator,
    #[cfg(feature = "future_snark")]
    pub snark_proof_individual_signature_generator: Option<SnarkProofSingleSignatureGenerator>,
}

impl Signer {
    pub fn new(
        signer_index: SignerIndex,
        stake: Stake,
        parameters: Parameters,
        bls_crypto_signer: BlsCryptoSigner,
        concatenation_proof_individual_signature_generator:
            ConcatenationProofSingleSignatureGenerator,
        #[cfg(feature = "future_snark")] snark_proof_individual_signature_generator: Option<
            SnarkProofSingleSignatureGenerator,
        >,
    ) -> Self {
        Self {
            signer_index,
            stake,
            parameters,
            concatenation_proof_individual_signature_generator,
            #[cfg(feature = "future_snark")]
            snark_proof_individual_signature_generator,
        }
    }

    pub fn sign(&self, message: &[u8]) -> StdResult<SingleSignature> {
        let concatenation_individual_signature = self
            .concatenation_proof_individual_signature_generator
            .create_individual_signature(message)?;
        #[cfg(feature = "future_snark")]
        let snark_individual_signature =
            if let Some(snark_generator) = &self.snark_proof_individual_signature_generator {
                Some(snark_generator.create_individual_signature(message)?)
            } else {
                None
            };
        #[cfg(feature = "future_snark")]
        let (schnorr_signature, snark_indices) = match snark_individual_signature {
            Some(snark_sig) => (
                Some(snark_sig.signature),
                Some(snark_sig.lottery_indices.clone()),
            ),
            None => (None, None),
        };

        Ok(SingleSignature {
            signer_index: self.signer_index.clone(),
            bls_signature: concatenation_individual_signature.signature,
            concatenation_indices: concatenation_individual_signature.lottery_indices,
            #[cfg(feature = "future_snark")]
            schnorr_signature,
            #[cfg(feature = "future_snark")]
            snark_indices,
        })
    }
}
