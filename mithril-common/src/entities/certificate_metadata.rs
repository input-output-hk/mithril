use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

use crate::entities::{ProtocolParameters, ProtocolVersion, SignerWithStake, StakeDistribution};

use super::{PartyId, Stake};

/// This represents a stakeholder.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct StakeDistributionParty {
    /// Party identifier as in the stake distribution
    pub party_id: PartyId,

    /// Amount of stake owned by the party.
    pub stake: Stake,
}

impl From<SignerWithStake> for StakeDistributionParty {
    fn from(value: SignerWithStake) -> Self {
        Self {
            party_id: value.party_id,
            stake: value.stake,
        }
    }
}

impl StakeDistributionParty {
    /// As a sub structure of certificate, Party must be hashable.
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.party_id.as_bytes());
        hasher.update(self.stake.to_be_bytes());

        hex::encode(hasher.finalize())
    }

    /// Transform a list of signers into a list of `StakeDistributionParty``
    pub fn from_signers(signers: Vec<SignerWithStake>) -> Vec<Self> {
        signers.into_iter().map(|s| s.into()).collect()
    }
}

/// CertificateMetadata represents the metadata associated to a Certificate
#[derive(Clone, Debug, PartialEq)]
pub struct CertificateMetadata {
    /// Cardano network
    pub network: String,

    /// Protocol Version (semver)
    /// Useful to achieve backward compatibility of the certificates (including of the multi signature)
    /// part of METADATA(p,n)
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters
    /// part of METADATA(p,n)
    pub protocol_parameters: ProtocolParameters,

    /// Date and time when the certificate was initiated
    /// Represents the time at which the single signatures registration is opened
    /// part of METADATA(p,n)
    pub initiated_at: DateTime<Utc>,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    pub sealed_at: DateTime<Utc>,

    /// The list of the active signers with their stakes and verification keys
    /// part of METADATA(p,n)
    pub signers: Vec<StakeDistributionParty>,
}

impl CertificateMetadata {
    /// CertificateMetadata factory
    pub fn new<T: Into<String>, U: Into<ProtocolVersion>>(
        network: T,
        protocol_version: U,
        protocol_parameters: ProtocolParameters,
        initiated_at: DateTime<Utc>,
        sealed_at: DateTime<Utc>,
        signers: Vec<StakeDistributionParty>,
    ) -> CertificateMetadata {
        CertificateMetadata {
            network: network.into(),
            protocol_version: protocol_version.into(),
            protocol_parameters,
            initiated_at,
            sealed_at,
            signers,
        }
    }

    /// Deduce the stake distribution from the metadata [signers][CertificateMetadata::signers]
    pub fn get_stake_distribution(&self) -> StakeDistribution {
        self.signers
            .clone()
            .iter()
            .map(|s| (s.party_id.clone(), s.stake))
            .collect()
    }

    /// Computes the hash of the certificate metadata
    pub fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();
        hasher.update(self.network.as_bytes());
        hasher.update(self.protocol_version.as_bytes());
        hasher.update(self.protocol_parameters.compute_hash().as_bytes());
        hasher.update(
            self.initiated_at
                .timestamp_nanos_opt()
                .unwrap_or_default()
                .to_be_bytes(),
        );
        hasher.update(self.sealed_at.timestamp_nanos_opt().unwrap_or_default().to_be_bytes());

        for party in &self.signers {
            hasher.update(party.compute_hash().as_bytes());
        }

        hex::encode(hasher.finalize())
    }
}

#[cfg(test)]
mod tests {
    use chrono::{Duration, TimeZone, Timelike};

    use super::*;

    fn get_parties() -> Vec<StakeDistributionParty> {
        vec![
            StakeDistributionParty {
                party_id: "1".to_string(),
                stake: 10,
            },
            StakeDistributionParty {
                party_id: "2".to_string(),
                stake: 20,
            },
        ]
    }

    #[test]
    fn test_certificate_metadata_compute_hash() {
        let hash_expected = "f16631f048b33746aa0141cf607ee53ddb76308725e6912530cc41cc54834206";

        let initiated_at = Utc
            .with_ymd_and_hms(2024, 2, 12, 13, 11, 47)
            .unwrap()
            .with_nanosecond(123043)
            .unwrap();
        let sealed_at = initiated_at + Duration::try_seconds(100).unwrap();
        let metadata = CertificateMetadata::new(
            "devnet",
            "0.1.0",
            ProtocolParameters::new(1000, 100, 0.123),
            initiated_at,
            sealed_at,
            get_parties(),
        );

        assert_eq!(hash_expected, metadata.compute_hash());

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                network: "modified".into(),
                ..metadata.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                protocol_version: "0.1.0-modified".to_string(),
                ..metadata.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                protocol_parameters: ProtocolParameters::new(2000, 100, 0.123),
                ..metadata.clone()
            }
            .compute_hash(),
        );

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                initiated_at: metadata.initiated_at - Duration::try_seconds(78).unwrap(),
                ..metadata.clone()
            }
            .compute_hash()
        );

        let mut signers_with_different_party_id = get_parties();
        signers_with_different_party_id[0].party_id = "1-modified".to_string();

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                sealed_at: metadata.sealed_at - Duration::try_seconds(78).unwrap(),
                ..metadata.clone()
            }
            .compute_hash(),
        );

        let mut signers = get_parties();
        signers.truncate(1);

        assert_ne!(
            hash_expected,
            CertificateMetadata {
                signers,
                ..metadata.clone()
            }
            .compute_hash(),
        );
    }
}
