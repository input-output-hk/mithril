use chrono::{DateTime, Utc};

use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Signer record is the representation of a stored signer.
#[derive(Debug, PartialEq, Clone)]
pub struct SignerRecord {
    /// Signer id.
    pub signer_id: String,

    /// Pool ticker of the signer.
    pub pool_ticker: Option<String>,

    /// Date and time when the signer was created.
    pub created_at: DateTime<Utc>,

    /// Date and time when the signer was updated.
    pub updated_at: DateTime<Utc>,

    /// Date and time when the signer registered for the last time.
    pub last_registered_at: Option<DateTime<Utc>>,
}

impl SqLiteEntity for SignerRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let signer_id = row.read::<&str, _>(0).to_string();
        let pool_ticker = row.read::<Option<&str>, _>(1).map(|s| s.to_owned());
        let created_at = row.read::<&str, _>(2);
        let updated_at = row.read::<&str, _>(3);
        let registered_at = row.read::<Option<&str>, _>(4);

        let signer_record = Self {
            signer_id,
            pool_ticker,
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            updated_at: DateTime::parse_from_rfc3339(updated_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{updated_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            last_registered_at: registered_at
                .map(|d| match DateTime::parse_from_rfc3339(d) {
                    Ok(date) => Ok(date.with_timezone(&Utc)),
                    Err(e) => Err(HydrationError::InvalidData(format!(
                        "Could not turn string '{d}' to rfc3339 Datetime. Error: {e}"
                    ))),
                })
                .transpose()?,
        };

        Ok(signer_record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("signer_id", "{:signer:}.signer_id", "text");
        projection.add_field("pool_ticker", "{:signer:}.pool_ticker", "text");
        projection.add_field("created_at", "{:signer:}.created_at", "text");
        projection.add_field("updated_at", "{:signer:}.updated_at", "text");
        projection.add_field(
            "last_registered_at",
            "{:signer:}.last_registered_at",
            "text",
        );

        projection
    }
}
