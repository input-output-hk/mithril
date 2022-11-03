use crate::entities::{ProtocolParameters, ProtocolVersion, SignerWithStake, StakeDistribution};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// CertificateMetadata represents the metadata associated to a Certificate
#[derive(Clone, Debug, PartialEq, Default, Serialize, Deserialize)]
pub struct CertificateMetadata {
    /// Protocol Version (semver)
    /// Useful to achieve backward compatibility of the certificates (including of the multi signature)
    /// part of METADATA(p,n)
    #[serde(rename = "version")]
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters
    /// part of METADATA(p,n)
    #[serde(rename = "parameters")]
    pub protocol_parameters: ProtocolParameters,

    /// Date and time when the certificate was initiated
    /// Represents the time at which the single signatures registration is opened
    /// part of METADATA(p,n)
    pub initiated_at: String,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    pub sealed_at: String,

    /// The list of the active signers with their stakes and verification keys
    /// part of METADATA(p,n)
    pub signers: Vec<SignerWithStake>,
}

impl CertificateMetadata {
    /// CertificateMetadata factory
    pub fn new(
        protocol_version: ProtocolVersion,
        protocol_parameters: ProtocolParameters,
        initiated_at: String,
        sealed_at: String,
        signers: Vec<SignerWithStake>,
    ) -> CertificateMetadata {
        CertificateMetadata {
            protocol_version,
            protocol_parameters,
            initiated_at,
            sealed_at,
            signers,
        }
    }

    /// Deduce the stake distribution from the metadata [signers][CertificateMetadata::signers]
    pub fn get_stake_distribution(&self) -> StakeDistribution {
        self.signers.clone().iter().map(|s| s.into()).collect()
    }

    /// Computes the hash of the certificate metadata
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.protocol_version.as_bytes());
        hasher.update(self.protocol_parameters.compute_hash().as_bytes());
        hasher.update(self.initiated_at.as_bytes());
        hasher.update(self.sealed_at.as_bytes());
        self.signers
            .iter()
            .for_each(|signer| hasher.update(signer.compute_hash().as_bytes()));
        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_certificate_metadata_compute_hash() {
        let hash_expected = "512632ae13b4527486303a78b500fb328b9f353b8a635a32f74fe1d899439ae8";

        assert_eq!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0-modified".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(2000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at-modified".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at-modified".to_string(),
                vec![
                    SignerWithStake::new(
                        "1".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![
                    SignerWithStake::new(
                        "1-modified".to_string(),
                        "verification-key-123".to_string(),
                        None,
                        None,
                        None,
                        10
                    ),
                    SignerWithStake::new(
                        "2".to_string(),
                        "verification-key-456".to_string(),
                        None,
                        None,
                        None,
                        20
                    )
                ],
            )
            .compute_hash()
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata::new(
                "0.1.0".to_string(),
                ProtocolParameters::new(1000, 100, 0.123),
                "initiated_at".to_string(),
                "sealed_at".to_string(),
                vec![SignerWithStake::new(
                    "1".to_string(),
                    "verification-key-123".to_string(),
                    None,
                    None,
                    None,
                    10
                ),],
            )
            .compute_hash()
        );
    }
}
