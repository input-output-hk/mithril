use crate::*;

pub enum AggregateSignatureType {
    Concatenation,
    #[cfg(feature = "future_snark")]
    Snark,
}

pub enum AggregateSignature {
    Concatenation(ConcatenationProof),
    #[cfg(feature = "future_snark")]
    Snark(SnarkProof),
}

pub struct Aggregator {
    aggregate_signature_type: AggregateSignatureType,
    parameters: Parameters,
    key_registrations: KeyRegistration, //TODO: Maybe we want a generic representing the commitment for the specific proof system we are aggregating
}

impl Aggregator {
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
