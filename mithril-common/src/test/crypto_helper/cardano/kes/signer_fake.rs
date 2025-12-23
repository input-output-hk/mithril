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
    signed_messages: Mutex<VecDeque<Vec<u8>>>,
}

impl KesSignerFake {
    /// Creates a new `KesSignerFake` instance.
    pub fn new(results: Vec<KesSignatureResult>) -> Self {
        Self {
            results: Mutex::new(results.into()),
            signed_messages: Mutex::new(VecDeque::new()),
        }
    }

    /// Returns the messages that were requested to be signed
    pub fn get_signed_messages(&self) -> Vec<Vec<u8>> {
        let messages = self.signed_messages.lock().unwrap();
        messages.iter().cloned().collect()
    }

    /// Returns a dummy signature result that is always successful.
    pub fn dummy_signature(test_directory: &str) -> (Sum6KesSig, OpCert) {
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            KesPeriod(0),
            &format!("{}-kes", test_directory),
        );
        let message = b"Test message for KES signing";
        let kes_signer =
            KesSignerStandard::new(kes_secret_key_file.clone(), operational_certificate_file);
        let kes_signing_period = KesPeriod(1);
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
    fn sign(&self, message: &[u8], _current_kes_period: KesPeriod) -> KesSignatureResult {
        let mut messages = self.signed_messages.lock().unwrap();
        messages.push_back(message.to_vec());

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
            KesPeriod(0),
            "fake_kes_signer_returns_signature_batches_in_expected_order",
        );
        let message1 = b"Test message 1 for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let kes_signing_period = KesPeriod(1);
        let (kes_signature, op_cert) = kes_signer
            .sign(message1, kes_signing_period)
            .expect("Signing should not fail");
        let fake_kes_signer = KesSignerFake::new(vec![
            Ok((kes_signature, op_cert.clone())),
            Err(anyhow::anyhow!("Fake error")),
        ]);

        let (kes_signature_1, op_cert_1) = fake_kes_signer
            .sign(message1, kes_signing_period)
            .expect("Signing should not fail");
        assert_eq!(kes_signature, kes_signature_1);
        assert_eq!(op_cert, op_cert_1);

        let message2 = b"Test message 2 for KES signing";
        fake_kes_signer
            .sign(message2, kes_signing_period)
            .expect_err("Signing should fail");

        let signed_messages = fake_kes_signer.get_signed_messages();
        assert_eq!(vec![message1.to_vec(), message2.to_vec()], signed_messages);
    }
}
