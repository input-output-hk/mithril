use crate::{LotteryIndex, RegistrationEntryForSnark, proof_system::SingleSignatureForSnark};

/// Pairs a SNARK single signature with its corresponding registration entry.
///
/// Used during witness preparation to associate each verified signature
/// with the signer's registration data (verification key and lottery target value).
// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
#[derive(Clone, Hash, PartialEq, Eq)]
pub(crate) struct SignatureRegistrationEntry {
    /// The SNARK single signature
    signature: SingleSignatureForSnark,
    /// The signer's registration entry for SNARK
    registration_entry: RegistrationEntryForSnark,
}

// TODO: remove this allow dead_code directive when function is called or future_snark is activated
#[allow(dead_code)]
impl SignatureRegistrationEntry {
    /// Create a new `SignatureRegistrationEntry` from a signature and its registration entry.
    pub(crate) fn new(
        signature: SingleSignatureForSnark,
        registration_entry: RegistrationEntryForSnark,
    ) -> Self {
        Self {
            signature,
            registration_entry,
        }
    }

    /// Return a reference to the SNARK single signature.
    pub(crate) fn get_signature(&self) -> &SingleSignatureForSnark {
        &self.signature
    }

    /// Set the winning lottery indices on the inner signature.
    pub(crate) fn set_indices(&mut self, indices: &[LotteryIndex]) {
        self.signature.set_indices(indices);
    }

    /// Return a reference to the signer's registration entry.
    pub(crate) fn get_registration_entry(&self) -> &RegistrationEntryForSnark {
        &self.registration_entry
    }
}
