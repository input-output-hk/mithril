use chrono::{DateTime, Utc};

use mithril_common::{crypto_helper::ProtocolInitializer, entities::Epoch};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// Protocol initializer.
#[derive(Debug)]
pub struct ProtocolInitializerRecord {
    /// Epoch
    pub epoch: Epoch,

    /// Protocol Initializer
    pub protocol_initializer: ProtocolInitializer,

    /// DateTime of the record creation.
    pub created_at: DateTime<Utc>,
}

impl SqLiteEntity for ProtocolInitializerRecord {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let epoch_int = row.read::<i64, _>(0);
        let protocol = row.read::<&str, _>(1);
        let datetime = &row.read::<&str, _>(2);

        let record = Self {
            protocol_initializer: serde_json::from_str(protocol).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast string ({}) to ProtocolInitializer. Error: '{e}'",
                    protocol
                ))
            })?,
            epoch: Epoch(epoch_int.try_into().map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not cast i64 ({epoch_int}) to u64. Error: '{e}'"
                ))
            })?),
            created_at: DateTime::parse_from_rfc3339(datetime)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{datetime}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
        };

        Ok(record)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("epoch", "{:protocol_initializer:}.epoch", "integer");
        projection.add_field("protocol", "{:protocol_initializer:}.protocol", "integer");
        projection.add_field("created_at", "{:protocol_initializer:}.created_at", "text");

        projection
    }
}
