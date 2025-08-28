use std::collections::VecDeque;

use kes_summed_ed25519::kes::Sum6KesSig;
use std::sync::Mutex;

use crate::{
    StdResult,
    crypto_helper::{KesPeriod, KesSigner, KesSignerStandard, OpCert},
    test::crypto_helper::{
        KesCryptographicMaterialForTest, KesPartyIndexForTest, create_kes_cryptographic_material,
    },
};

type KesSignatureResult = StdResult<(Sum6KesSig, OpCert)>;

/// Fake KES Signer implementation.
pub struct KesSignerFake {
    results: Mutex<VecDeque<KesSignatureResult>>,
}

impl KesSignerFake {
    /// Creates a new `KesSignerFake` instance.
    pub fn new(results: Vec<KesSignatureResult>) -> Self {
        Self {
            results: Mutex::new(results.into()),
        }
    }

    /// Returns a dummy signature result that is always successful.
    pub fn dummy_signature(test_directory: &str) -> (Sum6KesSig, OpCert) {
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            0 as KesPeriod,
            &format!("{}-kes", test_directory),
        );
        let message = b"Test message for KES signing";
        let kes_signer =
            KesSignerStandard::new(kes_secret_key_file.clone(), operational_certificate_file);
        let kes_signing_period = 1;
        let (kes_signature, op_cert) = kes_signer
            .sign(message, kes_signing_period)
            .expect("Signing should not fail");

        (kes_signature, op_cert)
    }

    /// Returns a dummy signature result that always fails.
    pub fn dummy_signature_result_err() -> KesSignatureResult {
        Err(anyhow::anyhow!("Dummy error"))
    }
}

impl KesSigner for KesSignerFake {
    fn sign(&self, _message: &[u8], _kes_period: KesPeriod) -> KesSignatureResult {
        let mut results = self.results.lock().unwrap();

        results.pop_front().unwrap()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fake_kes_signer_returns_signature_batches_in_expected_order() {
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            0 as KesPeriod,
            "fake_kes_signer_returns_signature_batches_in_expected_order",
        );
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let kes_signing_period = 1;
        let (kes_signature, op_cert) = kes_signer
            .sign(message, kes_signing_period)
            .expect("Signing should not fail");
        let fake_kes_signer = KesSignerFake::new(vec![
            Ok((kes_signature, op_cert.clone())),
            Err(anyhow::anyhow!("Fake error")),
        ]);

        let (kes_signature_1, op_cert_1) = fake_kes_signer
            .sign(message, kes_signing_period)
            .expect("Signing should not fail");
        assert_eq!(kes_signature, kes_signature_1);
        assert_eq!(op_cert, op_cert_1);

        fake_kes_signer
            .sign(message, kes_signing_period)
            .expect_err("Signing should fail");
    }
}
