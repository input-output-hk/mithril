use serde::{Deserialize, Serialize};

use crate::entities::Beacon;

use crate::entities::Epoch;

/// Message structure of a certificate list
pub type CertificateListMessage = Vec<CertificateListItemMessage>;

/// Message structure of a certificate list item
// TODO: select final fields for the list
#[derive(Clone, Debug, PartialEq, Eq, Default, Serialize, Deserialize)]
pub struct CertificateListItemMessage {
    /// Hash of the current certificate
    /// Computed from the other fields of the certificate
    /// aka H(Cp,n))
    pub hash: String,

    /// Hash of the previous certificate in the chain
    /// This is either the hash of the first certificate of the epoch in the chain
    /// Or the first certificate of the previous epoch in the chain (if the certificate is the first of its epoch)
    /// aka H(FC(n))
    pub previous_hash: String,

    /// Mithril beacon on the Cardano chain
    /// aka BEACON(p,n)
    pub beacon: Beacon,

    /// Message that is signed by the signers
    /// aka H(MSG(p,n) || AVK(n-1))
    pub signed_message: String,

    /// Date and time when the certificate was initiated
    /// Represents the time at which the single signatures registration is opened
    /// part of METADATA(p,n)
    pub initiated_at: String,

    /// Date and time when the certificate was sealed
    /// Represents the time at which the quorum of single signatures was reached so that they were aggregated into a multi signature
    /// part of METADATA(p,n)
    pub sealed_at: String,
}

impl CertificateListItemMessage {
    /// Return a dummy test entity (test-only).
    pub fn dummy() -> Self {
        Self {
            hash: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            previous_hash: "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb"
                .to_string(),
            beacon: Beacon {
                network: "preview".to_string(),
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            signed_message: "eca9866c06a9fb98a34686449ff0c03f75f8ddd9a126840e5cdd77cda2ffc4de"
                .to_string(),
            initiated_at: "2023-01-19T13:43:05.618857482Z".to_string(),
            sealed_at: "2023-01-19T13:45:17.628757381Z".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn golden_message() -> CertificateListMessage {
        vec![CertificateListItemMessage {
            hash: "0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6".to_string(),
            previous_hash: "d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb"
                .to_string(),
            beacon: Beacon {
                network: "preview".to_string(),
                epoch: Epoch(86),
                immutable_file_number: 1728,
            },
            signed_message: "eca9866c06a9fb98a34686449ff0c03f75f8ddd9a126840e5cdd77cda2ffc4de"
                .to_string(),
            initiated_at: "2023-01-19T13:43:05.618857482Z".to_string(),
            sealed_at: "2023-01-19T13:45:17.628757381Z".to_string(),
        }]
    }

    // Test the retro compatibility with possible future upgrades.
    #[test]
    fn test_v1() {
        let json = r#"[{
"hash":"0b9f5ad7f33cc523775c82249294eb8a1541d54f08eb3107cafc5638403ec7c6",
"previous_hash":"d5daf6c03ace4a9c074e951844075b9b373bafc4e039160e3e2af01823e9abfb",
"beacon":{"network":"preview","epoch":86,"immutable_file_number":1728},
"signed_message":"eca9866c06a9fb98a34686449ff0c03f75f8ddd9a126840e5cdd77cda2ffc4de",
"initiated_at":"2023-01-19T13:43:05.618857482Z",
"sealed_at":"2023-01-19T13:45:17.628757381Z"
}]"#;
        let message: CertificateListMessage = serde_json::from_str(json).expect(
            "This JSON is expected to be succesfully parsed into a CertificateListMessage instance.",
        );

        assert_eq!(golden_message(), message);
    }
}
