use crate::*;

pub trait ProofSystemSingleSignatureGenerator {
    type ProofSystemSingleSignature;

    fn create_individual_signature(
        &self,
        message: &[u8],
    ) -> StdResult<Self::ProofSystemSingleSignature>;
}
