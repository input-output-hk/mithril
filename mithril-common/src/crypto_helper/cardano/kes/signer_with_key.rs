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
    fn sign(&self, message: &[u8], kes_period: KesPeriod) -> StdResult<(Sum6KesSig, OpCert)> {
        let mut kes_sk_bytes =
            Sum6KesBytes::from_file(&self.kes_sk_path)
                .map_err(|e| anyhow!(e))
                .with_context(|| "StandardKesSigner can not read KES secret key from file")?;
        let mut kes_sk = Sum6Kes::try_from(&mut kes_sk_bytes)
            .map_err(|e| anyhow!(e))
            .with_context(|| "StandardKesSigner can not use KES secret key")?;
        let kes_sk_period = kes_sk.get_period();
        if kes_sk_period > kes_period {
            return Err(anyhow!(KesSignError::PeriodMismatch(
                kes_sk_period,
                kes_period
            )));
        }

        // We need to perform the evolutions
        for period in kes_sk_period..kes_period {
            kes_sk.update().map_err(|_| KesSignError::UpdateKey(period))?;
        }

        let operational_certificate = OpCert::from_file(&self.operational_certificate_path)
            .map_err(|e| anyhow!(e))
            .with_context(|| "StandardKesSigner can not read operational certificate from file")?;

        Ok((kes_sk.sign(message), operational_certificate))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::crypto_helper::cardano::kes::{KesVerifier, KesVerifierStandard};
    use crate::test::crypto_helper::{
        KesCryptographicMaterialForTest, KesPartyIndexForTest, create_kes_cryptographic_material,
    };

    #[test]
    fn create_valid_signature_for_message() {
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            0 as KesPeriod,
            "create_valid_signature_for_message",
        );
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
    fn create_invalid_signature_for_different_message() {
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            0 as KesPeriod,
            "create_invalid_signature_for_different_message",
        );
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

    #[test]
    fn create_invalid_signature_for_invalid_kes_period() {
        let kes_period_start = 5 as KesPeriod;
        let KesCryptographicMaterialForTest {
            party_id: _,
            operational_certificate_file,
            kes_secret_key_file,
        } = create_kes_cryptographic_material(
            1 as KesPartyIndexForTest,
            kes_period_start,
            "create_invalid_signature_for_invalid_kes_period",
        );
        let message = b"Test message for KES signing";
        let kes_signer = KesSignerStandard::new(kes_secret_key_file, operational_certificate_file);
        let kes_signing_period = 2;
        assert!(
            kes_signing_period < kes_period_start,
            "KES signing period should be less than the KES period of the key"
        );

        kes_signer
            .sign(message, kes_signing_period)
            .expect_err("Signing should fail");
    }
}
