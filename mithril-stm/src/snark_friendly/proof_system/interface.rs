use crate::snark_friendly::*;

pub trait ProofSystemSingleSignatureGenerator {
    type ProofSystemSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature>;
}
