use crate::*;

/// The type of aggregate signature to produce
pub enum AggregateSignatureType {
    Concatenation,
    #[cfg(feature = "future_snark")]
    Snark,
}

/// The aggregate signature produced by the aggregator
pub enum AggregateSignature {
    Concatenation(ConcatenationProof),
    #[cfg(feature = "future_snark")]
    Snark(SnarkProof),
}

/// The signature aggregator structure
pub struct SignatureAggregator {
    aggregate_signature_type: AggregateSignatureType,
    parameters: Parameters,
    key_registrations: KeyRegistration,
}

impl SignatureAggregator {
    /// Creates a new SignatureAggregator
    pub fn new(
        aggregate_signature_type: AggregateSignatureType,
        parameters: Parameters,
        key_registrations: KeyRegistration,
    ) -> Self {
        Self {
            aggregate_signature_type,
            parameters,
            key_registrations,
        }
    }

    /// Aggregates the given signatures into an aggregate signature
    pub fn aggregate_signatures(
        &self,
        message: &[u8],
        signatures: Vec<SingleSignature>,
    ) -> StdResult<AggregateSignature> {
        match self.aggregate_signature_type {
            AggregateSignatureType::Concatenation => {
                let concatenation_proof_generator =
                    ConcatenationProofGenerator::new(&self.parameters, &self.key_registrations);
                let concatenation_proof = concatenation_proof_generator
                    .create_concatenation_proof(message, &signatures)?;

                Ok(AggregateSignature::Concatenation(concatenation_proof))
            }
            #[cfg(feature = "future_snark")]
            AggregateSignatureType::Snark => {
                let snark_proof_generator =
                    SnarkProofGenerator::new(&self.parameters, &self.key_registrations);
                let snark_proof = snark_proof_generator.create_snark_proof(message, &signatures)?;

                Ok(AggregateSignature::Snark(snark_proof))
            }
        }
    }
}
