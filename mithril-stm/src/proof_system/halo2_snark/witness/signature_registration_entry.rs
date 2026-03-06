use crate::{LotteryIndex, RegistrationEntryForSnark, proof_system::SingleSignatureForSnark};

#[allow(dead_code)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub(crate) struct SignatureRegistrationEntry {
    signature: SingleSignatureForSnark,
    registration_entry: RegistrationEntryForSnark,
}

#[allow(dead_code)]
impl SignatureRegistrationEntry {
    pub(crate) fn new(
        signature: SingleSignatureForSnark,
        registration_entry: RegistrationEntryForSnark,
    ) -> Self {
        Self {
            signature,
            registration_entry,
        }
    }

    pub(crate) fn get_signature(&self) -> &SingleSignatureForSnark {
        &self.signature
    }

    pub(crate) fn set_indices(&mut self, indices: &[LotteryIndex]) {
        self.signature.set_indices(indices);
    }

    pub(crate) fn get_registration_entry(&self) -> &RegistrationEntryForSnark {
        &self.registration_entry
    }
}
