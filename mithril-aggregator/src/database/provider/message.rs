use std::sync::Arc;

use mithril_common::{
    messages::{
        CertificateListItemMessage, CertificateListItemMessageMetadata, CertificateMessage,
        CertificateMetadataMessagePart,
    },
    StdResult,
};
use sqlite::ConnectionWithFullMutex;

use super::{CertificateRecord, CertificateRecordProvider};

/// Repository that turn inner Record entities into Message entities.
pub struct CertificateMessageRepository {
    connection: Arc<ConnectionWithFullMutex>,
}

impl CertificateMessageRepository {
    /// Constructor
    pub fn new(connection: Arc<ConnectionWithFullMutex>) -> Self {
        Self { connection }
    }
}

impl From<CertificateRecord> for CertificateMessage {
    fn from(value: CertificateRecord) -> Self {
        let metadata = CertificateMetadataMessagePart {
            protocol_version: value.protocol_version,
            protocol_parameters: value.protocol_parameters,
            initiated_at: value.initiated_at,
            sealed_at: value.sealed_at,
            signers: value.signers,
        };
        let (multi_signature, genesis_signature) = if value.parent_certificate_id.is_none() {
            (String::new(), value.signature)
        } else {
            (value.signature, String::new())
        };

        CertificateMessage {
            hash: value.certificate_id,
            previous_hash: value.parent_certificate_id.unwrap_or_default(),
            beacon: value.beacon,
            metadata,
            protocol_message: value.protocol_message,
            signed_message: value.message,
            aggregate_verification_key: value.aggregate_verification_key,
            multi_signature,
            genesis_signature,
        }
    }
}

impl From<CertificateRecord> for CertificateListItemMessage {
    fn from(value: CertificateRecord) -> Self {
        let metadata = CertificateListItemMessageMetadata {
            protocol_version: value.protocol_version,
            protocol_parameters: value.protocol_parameters,
            initiated_at: value.initiated_at,
            sealed_at: value.sealed_at,
            total_signers: value.signers.len(),
        };

        CertificateListItemMessage {
            hash: value.certificate_id,
            previous_hash: value.parent_certificate_id.unwrap_or_default(),
            beacon: value.beacon,
            metadata,
            protocol_message: value.protocol_message,
            signed_message: value.message,
            aggregate_verification_key: value.aggregate_verification_key,
        }
    }
}

impl CertificateMessageRepository {
    /// Return the certificate matching the given hash.
    pub async fn get_certificate(&self, hash: &str) -> StdResult<Option<CertificateMessage>> {
        let provider = CertificateRecordProvider::new(&self.connection);
        let mut cursor = provider.get_by_certificate_id(hash)?;

        Ok(cursor.next().map(|v| v.into()))
    }

    /// Return the last N certificates
    pub async fn get_last(&self, limit: usize) -> StdResult<Vec<CertificateListItemMessage>> {
        let provider = CertificateRecordProvider::new(&self.connection);
        let cursor = provider.get_all()?;

        Ok(cursor.into_iter().take(limit).map(|v| v.into()).collect())
    }
}
