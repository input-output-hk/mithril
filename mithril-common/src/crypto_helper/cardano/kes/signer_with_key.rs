use std::path::PathBuf;

use anyhow::{Context, anyhow};
use kes_summed_ed25519::{
    kes::{Sum6Kes, Sum6KesSig},
    traits::KesSk,
};

use crate::{
    StdResult,
    crypto_helper::{
        KesPeriod, OpCert, SerDeShelleyFileFormat, Sum6KesBytes,
        cardano::{KesSignError, KesSigner},
    },
};

/// Standard KES Signer implementation which uses a KES secret key.
pub struct KesSignerStandard {
    kes_sk_path: PathBuf,
    operational_certificate_path: PathBuf,
}

impl KesSignerStandard {
    /// Create a new instance of `StandardKesSigner`.
    pub fn new(kes_sk_path: PathBuf, operational_certificate_path: PathBuf) -> Self {
        Self {
            kes_sk_path,
            operational_certificate_path,
        }
    }
}

impl KesSigner for KesSignerStandard {
    fn sign(
        &self,
        message: &[u8],
        current_kes_period: KesPeriod,
    ) -> StdResult<(Sum6KesSig, OpCert)> {
        let mut kes_sk_bytes = Sum6KesBytes::from_file(&self.kes_sk_path)
            .with_context(|| "StandardKesSigner can not read KES secret key from file")?;
        let mut kes_sk = Sum6Kes::try_from(&mut kes_sk_bytes)
            .with_context(|| "StandardKesSigner can not use KES secret key")?;
        let operational_certificate = OpCert::from_file(&self.operational_certificate_path)
            .with_context(|| "StandardKesSigner can not read operational certificate from file")?;
        let kes_period_start = operational_certificate.get_start_kes_period() as u32;
        if kes_period_start > current_kes_period {
            return Err(anyhow!(KesSignError::PeriodMismatch(
                kes_period_start,
                current_kes_period
            )));
        }
        let kes_evolutions = current_kes_period.saturating_sub(kes_period_start);

        // We need to perform the evolutions
        for evolution in 0..kes_evolutions {
            kes_sk.update().map_err(|_| KesSignError::UpdateKey(evolution))?;
        }

        Ok((kes_sk.sign(message), operational_certificate))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::crypto_helper::cardano::kes::{KesVerifier, KesVerifierStandard};
    use crate::current_function;
    use crate::test::crypto_helper::{
        KesCryptographicMaterialForTest, KesPartyIndexForTest, create_kes_cryptographic_material,
    };

    #[test]
    fn create_valid_signature_for_message() {
        let start_kes_period = 10 as KesPeriod;
        let kes_evolutions = 32;
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
    fn create_invalid_signature_for_different_message() {
        let start_kes_period = 10 as KesPeriod;
        let kes_evolutions = 32;
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
    fn create_invalid_signature_for_invalid_current_kes_period() {
        let start_kes_period = 10 as KesPeriod;
        let signing_kes_period = 5;
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

        let res = kes_signer
            .sign(message, signing_kes_period)
            .expect_err("Signing should fail");
        assert_eq!(
            res.downcast_ref::<KesSignError>(),
            Some(&KesSignError::PeriodMismatch(
                start_kes_period,
                signing_kes_period
            ))
        );
    }

    #[test]
    fn create_invalid_signature_for_invalid_kes_evolutions() {
        const MAX_KES_EVOLUTIONS: KesPeriod = 63;
        let start_kes_period = 10 as KesPeriod;
        let signing_kes_period = start_kes_period + MAX_KES_EVOLUTIONS + 1;
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

        let res = kes_signer
            .sign(message, signing_kes_period)
            .expect_err("Signing should fail");
        assert_eq!(
            res.downcast_ref::<KesSignError>(),
            Some(&KesSignError::UpdateKey(MAX_KES_EVOLUTIONS))
        );
    }
}
