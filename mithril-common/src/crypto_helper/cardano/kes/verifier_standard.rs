use kes_summed_ed25519::{kes::Sum6KesSig, traits::KesSig};

use crate::{
    crypto_helper::{
        cardano::{KesError, KesVerifier},
        KESPeriod, OpCert,
    },
    StdResult,
};

/// A standard KES signature verifier.
#[derive(Debug)]
pub struct KesVerifierStandard;

impl KesVerifier for KesVerifierStandard {
    /// Verify the signed message and return the original message.
    fn verify(
        &self,
        message: &[u8],
        signature: &Sum6KesSig,
        operational_certificate: &OpCert,
        kes_period: KESPeriod,
    ) -> StdResult<()> {
        operational_certificate
            .validate()
            .map_err(|_| KesError::OpCertInvalid)?;

        // Check if the KES period in the operational certificate matches the provided KES period +/- 1
        let kes_period_try_min = std::cmp::max(0, kes_period.saturating_sub(1));
        let kes_period_try_max = std::cmp::min(64, kes_period.saturating_add(1));
        for kes_period_try in kes_period_try_min..kes_period_try_max {
            if signature
                .verify(kes_period_try, &operational_certificate.kes_vk, message)
                .is_ok()
            {
                return Ok(());
            }
        }

        Err(
            KesError::SignatureInvalid(kes_period, operational_certificate.start_kes_period as u32)
                .into(),
        )
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::cardano::{
        kes::tests_setup::create_kes_cryptographic_material, KesSigner, KesSignerStandard,
    };

    use super::*;

    #[test]
    fn verify_valid_signature_succeeds() {
        let (_party_id, operational_certificate_file, kes_secret_key_file) =
            create_kes_cryptographic_material(1, 0 as KESPeriod, "verify_valid_signature_succeeds");
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let kes_signing_period = 1;
        let (signature, op_cert) = kes_signer
            .sign(message, kes_signing_period)
            .expect("Signing should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_signing_period)
            .expect("Signature verification should not fail");
    }

    #[test]
    fn verify_invalid_signature_fails() {
        let (_party_id, operational_certificate_file, kes_secret_key_file) =
            create_kes_cryptographic_material(1, 0 as KESPeriod, "verify_invalid_signature_fails");
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let kes_signing_period = 1;
        let (signature, op_cert) = kes_signer
            .sign(message, kes_signing_period)
            .expect("Signing should not fail");

        KesVerifierStandard
            .verify(
                b"Different message",
                &signature,
                &op_cert,
                kes_signing_period,
            )
            .expect_err("Signature verification should fail");
    }
}
