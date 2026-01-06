use kes_summed_ed25519::{kes::Sum6KesSig, traits::KesSig};

use crate::{
    StdResult,
    crypto_helper::{
        KesEvolutions, OpCert,
        cardano::{KesVerifier, KesVerifyError},
    },
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
        kes_evolutions: KesEvolutions,
    ) -> StdResult<()> {
        operational_certificate
            .validate()
            .map_err(|_| KesVerifyError::OpCertInvalid)?;

        // Check if the signature verifies for the provided KES evolutions value +/- 1 KES period.
        // This is needed to account for clock skew between signer and verifier.
        let kes_evolutions_try_min = std::cmp::max(0, kes_evolutions.saturating_sub(1));
        let kes_evolutions_try_max = std::cmp::min(64, kes_evolutions.saturating_add(1));
        for kes_evolutions_try in kes_evolutions_try_min..=kes_evolutions_try_max {
            if signature
                .verify(
                    u32::try_from(kes_evolutions_try)
                        .map_err(|_| KesVerifyError::InvalidKesEvolutions(kes_evolutions))?,
                    &operational_certificate.get_kes_verification_key(),
                    message,
                )
                .is_ok()
            {
                return Ok(());
            }
        }

        Err(KesVerifyError::SignatureInvalid(
            kes_evolutions,
            operational_certificate.get_start_kes_period(),
        )
        .into())
    }
}

#[cfg(test)]
mod tests {
    use crate::crypto_helper::KesPeriod;
    use crate::crypto_helper::cardano::kes::{KesSigner, KesSignerStandard};
    use crate::current_function;
    use crate::test::crypto_helper::{
        KesCryptographicMaterialForTest, KesPartyIndexForTest, create_kes_cryptographic_material,
    };

    use super::*;

    #[test]
    fn verify_valid_signature_succeeds() {
        let start_kes_period = KesPeriod(10);
        let kes_evolutions = KesEvolutions(1);
        let signing_kes_period = start_kes_period + kes_evolutions;
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            start_kes_period,
            current_function!(),
        );
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);

        let (signature, op_cert) = kes_signer
            .sign(message, signing_kes_period)
            .expect("Signing should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions)
            .expect("Signature verification should not fail");
    }

    #[test]
    fn verify_invalid_signature_fails() {
        let start_kes_period = KesPeriod(10);
        let kes_evolutions = KesEvolutions(1);
        let signing_kes_period = start_kes_period + kes_evolutions;
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            start_kes_period,
            current_function!(),
        );
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let (signature, op_cert) = kes_signer
            .sign(message, signing_kes_period)
            .expect("Signing should not fail");

        KesVerifierStandard
            .verify(b"Different message", &signature, &op_cert, kes_evolutions)
            .expect_err("Signature verification should fail");
    }

    #[test]
    fn verify_valid_signature_invalid_kes_evolutions_fails() {
        let start_kes_period = KesPeriod(10);
        let kes_evolutions = KesEvolutions(5);
        let signing_kes_period = start_kes_period + kes_evolutions;
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            start_kes_period,
            current_function!(),
        );
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);

        let (signature, op_cert) = kes_signer
            .sign(message, signing_kes_period)
            .expect("Signing should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions - 2)
            .expect_err("Signature verification should fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions - 1)
            .expect("Signature verification should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions)
            .expect("Signature verification should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions + 1)
            .expect("Signature verification should not fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, kes_evolutions + 2)
            .expect_err("Signature verification should fail");

        KesVerifierStandard
            .verify(message, &signature, &op_cert, KesEvolutions(u64::MAX))
            .expect_err("Signature verification should fail");
    }
}
