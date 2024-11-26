use chrono::{DateTime, Utc};
use mithril_common::entities::{CertificatePending, Epoch};
use mithril_persistence::sqlite::{HydrationError, Projection, SourceAlias, SqLiteEntity};

/// CertificatePending record is the representation of a stored pending certificate.
pub struct CertificatePendingRecord {
    /// Current Epoch
    pub epoch: Epoch,

    /// Pending certificate serialization as json
    pub certificate: String,

    /// Date and time when the pending certificate was created
    pub created_at: DateTime<Utc>,
}

impl CertificatePendingRecord {
    /// Construct a [Projection] that will allow to hydrate this `CertificatePendingRecord` and expend table alias.
    pub fn expand_projection(table: &str) -> String {
        let aliases = SourceAlias::new(&[("{:pending_certificate:}", table)]);
        Self::get_projection().expand(aliases)
    }
}

impl SqLiteEntity for CertificatePendingRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_int = row.read::<i64, _>(0);
        let pending_certificate_json = row.read::<&str, _>(1);
        let created_at = &row.read::<&str, _>(2);

        let record = Self {
            certificate: pending_certificate_json.to_string(),
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_int}) to u64. Error: '{e}'"
                ))
            })?),
        };

        Ok(record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();

        projection.add_field("epoch", "{:pending_certificate:}.epoch", "integer");
        projection.add_field("certificate", "{:pending_certificate:}.certificate", "text");
        projection.add_field("created_at", "{:pending_certificate:}.created_at", "text");

        projection
    }
}

impl From<CertificatePending> for CertificatePendingRecord {
    fn from(value: CertificatePending) -> Self {
        Self {
            epoch: value.epoch,
            certificate: serde_json::to_string(&value).unwrap(),
            created_at: Utc::now(),
        }
    }
}

impl From<CertificatePendingRecord> for CertificatePending {
    fn from(record: CertificatePendingRecord) -> Self {
        let c: CertificatePending = serde_json::from_str(&record.certificate).unwrap();
        Self {
            epoch: record.epoch,
            signed_entity_type: c.signed_entity_type,
            protocol_parameters: c.protocol_parameters,
            next_protocol_parameters: c.next_protocol_parameters,
            signers: c.signers,
            next_signers: c.next_signers,
        }
    }
}
