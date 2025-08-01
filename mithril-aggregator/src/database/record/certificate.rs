use chrono::{DateTime, Utc};

use mithril_common::StdError;
use mithril_common::entities::{
    Certificate, CertificateMetadata, CertificateSignature, Epoch,
    HexEncodedAggregateVerificationKey, HexEncodedKey, ProtocolMessage, ProtocolParameters,
    ProtocolVersion, SignedEntityType, StakeDistributionParty,
};
use mithril_common::messages::{
    CertificateListItemMessage, CertificateListItemMessageMetadata, CertificateMessage,
    CertificateMetadataMessagePart,
};
#[cfg(test)]
use mithril_common::{
    entities::{CardanoDbBeacon, ImmutableFileNumber},
    test::double::{fake_data, fake_keys},
};
use mithril_persistence::{
    database::Hydrator,
    sqlite::{HydrationError, Projection, SqLiteEntity},
};

/// Certificate record is the representation of a stored certificate.
#[derive(Debug, PartialEq, Clone)]
pub struct CertificateRecord {
    /// Certificate id.
    pub certificate_id: String,

    /// Parent Certificate id.
    pub parent_certificate_id: Option<String>,

    /// Message that is signed.
    pub message: String,

    /// Signature of the certificate.
    /// Note: multi-signature if parent certificate id is set, genesis signature otherwise.
    pub signature: HexEncodedKey,

    /// Aggregate verification key
    /// Note: used only if signature is a multi-signature
    pub aggregate_verification_key: HexEncodedAggregateVerificationKey,

    /// Epoch of creation of the certificate.
    pub epoch: Epoch,

    /// Cardano network of the certificate.
    pub network: String,

    /// Signed entity type of the message
    pub signed_entity_type: SignedEntityType,

    /// Protocol Version (semver)
    pub protocol_version: ProtocolVersion,

    /// Protocol parameters.
    pub protocol_parameters: ProtocolParameters,

    /// Structured message that is used to create the signed message
    pub protocol_message: ProtocolMessage,

    /// The list of the active signers with their stakes
    pub signers: Vec<StakeDistributionParty>,

    /// Date and time when the certificate was initiated
    pub initiated_at: DateTime<Utc>,

    /// Date and time when the certificate was sealed
    pub sealed_at: DateTime<Utc>,
}

#[cfg(test)]
impl CertificateRecord {
    pub(crate) fn dummy_genesis(id: &str, epoch: Epoch) -> Self {
        Self {
            parent_certificate_id: None,
            signature: fake_keys::genesis_signature()[0].to_owned(),
            ..Self::dummy(id, "", epoch, SignedEntityType::genesis(epoch))
        }
    }

    pub(crate) fn dummy_db_snapshot(
        id: &str,
        parent_id: &str,
        epoch: Epoch,
        immutable_file_number: ImmutableFileNumber,
    ) -> Self {
        Self::dummy(
            id,
            parent_id,
            epoch,
            SignedEntityType::CardanoImmutableFilesFull(CardanoDbBeacon::new(
                *epoch,
                immutable_file_number,
            )),
        )
    }

    pub(crate) fn dummy(
        id: &str,
        parent_id: &str,
        epoch: Epoch,
        signed_entity_type: SignedEntityType,
    ) -> Self {
        Self {
            certificate_id: id.to_string(),
            parent_certificate_id: Some(parent_id.to_string()),
            message: "message".to_string(),
            signature: fake_keys::multi_signature()[0].to_owned(),
            aggregate_verification_key: fake_keys::aggregate_verification_key()[0].to_owned(),
            epoch,
            network: fake_data::network().to_string(),
            signed_entity_type,
            protocol_version: "protocol_version".to_string(),
            protocol_parameters: ProtocolParameters {
                k: 0,
                m: 0,
                phi_f: 0.0,
            },
            protocol_message: Default::default(),
            signers: vec![],
            initiated_at: DateTime::parse_from_rfc3339("2024-02-12T13:11:47Z")
                .unwrap()
                .with_timezone(&Utc),
            sealed_at: DateTime::parse_from_rfc3339("2024-02-12T13:12:57Z")
                .unwrap()
                .with_timezone(&Utc),
        }
    }
}

impl TryFrom<Certificate> for CertificateRecord {
    type Error = StdError;

    fn try_from(other: Certificate) -> Result<Self, Self::Error> {
        let signed_entity_type = other.signed_entity_type();
        let (signature, parent_certificate_id) = match other.signature {
            CertificateSignature::GenesisSignature(signature) => (signature.to_bytes_hex()?, None),
            CertificateSignature::MultiSignature(_, signature) => {
                (signature.to_json_hex()?, Some(other.previous_hash))
            }
        };

        let certificate_record = CertificateRecord {
            certificate_id: other.hash,
            parent_certificate_id,
            message: other.signed_message,
            signature,
            aggregate_verification_key: other.aggregate_verification_key.to_json_hex()?,
            epoch: other.epoch,
            network: other.metadata.network,
            signed_entity_type,
            protocol_version: other.metadata.protocol_version,
            protocol_parameters: other.metadata.protocol_parameters,
            protocol_message: other.protocol_message,
            signers: other.metadata.signers,
            initiated_at: other.metadata.initiated_at,
            sealed_at: other.metadata.sealed_at,
        };

        Ok(certificate_record)
    }
}

impl TryFrom<CertificateRecord> for Certificate {
    type Error = StdError;

    fn try_from(other: CertificateRecord) -> Result<Self, Self::Error> {
        let certificate_metadata = CertificateMetadata::new(
            other.network,
            other.protocol_version,
            other.protocol_parameters,
            other.initiated_at,
            other.sealed_at,
            other.signers,
        );
        let (previous_hash, signature) = match other.parent_certificate_id {
            None => (
                String::new(),
                CertificateSignature::GenesisSignature(other.signature.try_into()?),
            ),
            Some(parent_certificate_id) => (
                parent_certificate_id,
                CertificateSignature::MultiSignature(
                    other.signed_entity_type,
                    other.signature.try_into()?,
                ),
            ),
        };

        let certificate = Certificate {
            hash: other.certificate_id,
            previous_hash,
            epoch: other.epoch,
            metadata: certificate_metadata,
            signed_message: other.protocol_message.compute_hash(),
            protocol_message: other.protocol_message,
            aggregate_verification_key: other.aggregate_verification_key.try_into()?,
            signature,
        };

        Ok(certificate)
    }
}

impl From<CertificateRecord> for CertificateMessage {
    fn from(value: CertificateRecord) -> Self {
        let metadata = CertificateMetadataMessagePart {
            network: value.network,
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
            epoch: value.epoch,
            signed_entity_type: value.signed_entity_type,
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
            network: value.network,
            protocol_version: value.protocol_version,
            protocol_parameters: value.protocol_parameters,
            initiated_at: value.initiated_at,
            sealed_at: value.sealed_at,
            total_signers: value.signers.len(),
        };

        CertificateListItemMessage {
            hash: value.certificate_id,
            previous_hash: value.parent_certificate_id.unwrap_or_default(),
            epoch: value.epoch,
            signed_entity_type: value.signed_entity_type,
            metadata,
            protocol_message: value.protocol_message,
            signed_message: value.message,
            aggregate_verification_key: value.aggregate_verification_key,
        }
    }
}

impl SqLiteEntity for CertificateRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let certificate_id = row.read::<&str, _>(0).to_string();
        let parent_certificate_id = row.read::<Option<&str>, _>(1).map(|s| s.to_owned());
        let message = row.read::<&str, _>(2).to_string();
        let signature = row.read::<&str, _>(3).to_string();
        let aggregate_verification_key = row.read::<&str, _>(4).to_string();
        let epoch_int = row.read::<i64, _>(5);
        let network = row.read::<&str, _>(6).to_string();
        let signed_entity_type_id = row.read::<i64, _>(7);
        let signed_entity_beacon_string = Hydrator::read_signed_entity_beacon_column(&row, 8);
        let protocol_version = row.read::<&str, _>(9).to_string();
        let protocol_parameters_string = row.read::<&str, _>(10);
        let protocol_message_string = row.read::<&str, _>(11);
        let signers_string = row.read::<&str, _>(12);
        let initiated_at = row.read::<&str, _>(13);
        let sealed_at = row.read::<&str, _>(14);

        let certificate_record = Self {
            certificate_id,
            parent_certificate_id,
            message,
            signature,
            aggregate_verification_key,
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_int}) to u64. Error: '{e}'"
                ))
            })?),
            network,
            signed_entity_type: Hydrator::hydrate_signed_entity_type(
                signed_entity_type_id.try_into().map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not cast i64 ({signed_entity_type_id}) to u64. Error: '{e}'"
                    ))
                })?,
                &signed_entity_beacon_string,
            )?,
            protocol_version,
            protocol_parameters: serde_json::from_str(protocol_parameters_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_parameters_string}' to ProtocolParameters. Error: {e}"
                    ))
                },
            )?,
            protocol_message: serde_json::from_str(protocol_message_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{protocol_message_string}' to ProtocolMessage. Error: {e}"
                    ))
                },
            )?,
            signers: serde_json::from_str(signers_string).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{signers_string}' to Vec<StakeDistributionParty>. Error: {e}"
                    ))
                },
            )?,
            initiated_at: DateTime::parse_from_rfc3339(initiated_at).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{initiated_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                },
            )?.with_timezone(&Utc),
            sealed_at: DateTime::parse_from_rfc3339(sealed_at).map_err(
                |e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{sealed_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                },
            )?.with_timezone(&Utc),
        };

        Ok(certificate_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("certificate_id", "{:certificate:}.certificate_id", "text");
        projection.add_field(
            "parent_certificate_id",
            "{:certificate:}.parent_certificate_id",
            "text",
        );
        projection.add_field("message", "{:certificate:}.message", "text");
        projection.add_field("signature", "{:certificate:}.signature", "text");
        projection.add_field(
            "aggregate_verification_key",
            "{:certificate:}.aggregate_verification_key",
            "text",
        );
        projection.add_field("epoch", "{:certificate:}.epoch", "integer");
        projection.add_field("network", "{:certificate:}.network", "text");
        projection.add_field(
            "signed_entity_type_id",
            "{:certificate:}.signed_entity_type_id",
            "integer",
        );
        projection.add_field(
            "signed_entity_beacon",
            "{:certificate:}.signed_entity_beacon",
            "text",
        );
        projection.add_field(
            "protocol_version",
            "{:certificate:}.protocol_version",
            "text",
        );
        projection.add_field(
            "protocol_parameters",
            "{:certificate:}.protocol_parameters",
            "text",
        );
        projection.add_field(
            "protocol_message",
            "{:certificate:}.protocol_message",
            "text",
        );
        projection.add_field("signers", "{:certificate:}.signers", "text");
        projection.add_field("initiated_at", "{:certificate:}.initiated_at", "text");
        projection.add_field("sealed_at", "{:certificate:}.sealed_at", "text");

        projection
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test::crypto_helper::setup_certificate_chain;

    use super::*;

    #[test]
    fn test_convert_certificates() {
        let certificates = setup_certificate_chain(20, 3);
        let mut certificate_records: Vec<CertificateRecord> = Vec::new();
        for certificate in certificates.certificates_chained.clone() {
            certificate_records.push(certificate.try_into().unwrap());
        }
        let mut certificates_new: Vec<Certificate> = Vec::new();
        for certificate_record in certificate_records {
            certificates_new.push(certificate_record.try_into().unwrap());
        }
        assert_eq!(certificates.certificates_chained, certificates_new);
    }

    #[test]
    fn converting_certificate_record_to_certificate_should_not_recompute_hash() {
        let expected_hash = "my_hash";
        let record = CertificateRecord::dummy_genesis(expected_hash, Epoch(1));
        let certificate: Certificate = record.try_into().unwrap();

        assert_eq!(expected_hash, &certificate.hash);
    }
}
