use chrono::{DateTime, Utc};
use sqlite::Row;
use uuid::Uuid;

use mithril_common::entities::{Epoch, ProtocolMessage, SignedEntityType};
use mithril_persistence::database::Hydrator;
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

/// ## OpenMessage
///
/// An open message is a message open for signatures. Every signer may send a
/// single signature for this message from which a multi signature will be
/// generated if possible.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenMessageRecord {
    /// OpenMessage unique identifier
    pub open_message_id: Uuid,

    /// Epoch
    pub epoch: Epoch,

    /// Type of message
    pub signed_entity_type: SignedEntityType,

    /// Message used by the Mithril Protocol
    pub protocol_message: ProtocolMessage,

    /// Has this open message been converted into a certificate?
    pub is_certified: bool,

    /// Has this open message expired
    pub is_expired: bool,

    /// Message creation datetime, it is set by the database.
    pub created_at: DateTime<Utc>,

    /// Message expiration datetime, if it exists.
    pub expires_at: Option<DateTime<Utc>>,
}

impl OpenMessageRecord {
    #[cfg(test)]
    /// Create a dumb OpenMessage instance mainly for test purposes
    pub fn dummy() -> Self {
        let beacon = mithril_common::test_utils::fake_data::beacon();
        let epoch = beacon.epoch;
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon);

        Self {
            open_message_id: Uuid::parse_str("193d1442-e89b-43cf-9519-04d8db9a12ff").unwrap(),
            epoch,
            signed_entity_type,
            protocol_message: ProtocolMessage::new(),
            is_certified: false,
            is_expired: false,
            created_at: Utc::now(),
            expires_at: None,
        }
    }
}

impl SqLiteEntity for OpenMessageRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let open_message_id = row.read::<&str, _>(0);
        let open_message_id = Uuid::parse_str(open_message_id).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid UUID in open_message.open_message_id: '{open_message_id}'. Error: {e}"
            ))
        })?;
        let protocol_message = row.read::<&str, _>(4);
        let protocol_message = serde_json::from_str(protocol_message).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Invalid protocol message JSON representation '{protocol_message}'. Error: {e}"
            ))
        })?;
        let epoch_setting_id = row.read::<i64, _>(1);
        let epoch_val = u64::try_from(epoch_setting_id)
            .map_err(|e| panic!("Integer field open_message.epoch_setting_id (value={epoch_setting_id}) is incompatible with u64 Epoch representation. Error = {e}"))?;
        let beacon_str = Hydrator::read_signed_entity_beacon_column(&row, 2);
        let signed_entity_type_id = usize::try_from(row.read::<i64, _>(3)).map_err(|e| {
            panic!(
                "Integer field open_message.signed_entity_type_id cannot be turned into usize: {e}"
            )
        })?;
        let signed_entity_type =
            Hydrator::hydrate_signed_entity_type(signed_entity_type_id, &beacon_str)?;
        let is_certified = row.read::<i64, _>(5) != 0;
        let datetime = &row.read::<&str, _>(7);
        let created_at =
            DateTime::parse_from_rfc3339(datetime).map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not turn open_message.created_at field value '{datetime}' to rfc3339 Datetime. Error: {e}"
                ))
            })?.with_timezone(&Utc);
        let is_expired = row.read::<i64, _>(6) != 0;
        let datetime = &row.read::<Option<&str>, _>(8);
        let expires_at = datetime.map(|datetime| DateTime::parse_from_rfc3339(datetime).map_err(|e| {
            HydrationError::InvalidData(format!(
                "Could not turn open_message.expires_at field value '{datetime}' to rfc3339 Datetime. Error: {e}"
            ))
        })).transpose()?.map(|datetime| datetime.with_timezone(&Utc));
        let open_message = Self {
            open_message_id,
            epoch: Epoch(epoch_val),
            signed_entity_type,
            protocol_message,
            is_certified,
            is_expired,
            created_at,
            expires_at,
        };

        Ok(open_message)
    }

    fn get_projection() -> Projection {
        Projection::from(&[
            (
                "open_message_id",
                "{:open_message:}.open_message_id",
                "text",
            ),
            (
                "epoch_setting_id",
                "{:open_message:}.epoch_setting_id",
                "int",
            ),
            ("beacon", "{:open_message:}.beacon", "text"),
            (
                "signed_entity_type_id",
                "{:open_message:}.signed_entity_type_id",
                "int",
            ),
            (
                "protocol_message",
                "{:open_message:}.protocol_message",
                "text",
            ),
            ("is_certified", "{:open_message:}.is_certified", "bool"),
            ("is_expired", "{:open_message:}.is_expired", "bool"),
            ("created_at", "{:open_message:}.created_at", "text"),
            ("expires_at", "{:open_message:}.expires_at", "text"),
        ])
    }
}
