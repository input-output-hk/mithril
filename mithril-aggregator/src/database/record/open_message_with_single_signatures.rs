use chrono::{DateTime, Utc};
use sqlite::Row;
use uuid::Uuid;

use mithril_common::entities::{Epoch, ProtocolMessage, SignedEntityType, SingleSignatures};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

use crate::database::record::OpenMessageRecord;

/// Open Message with associated single signatures if any.
#[derive(Debug, Clone)]
pub struct OpenMessageWithSingleSignaturesRecord {
    /// OpenMessage unique identifier
    pub open_message_id: Uuid,

    /// Epoch
    pub epoch: Epoch,

    /// Type of message
    pub signed_entity_type: SignedEntityType,

    /// Message used by the Mithril Protocol
    pub protocol_message: ProtocolMessage,

    /// Has this message been converted into a Certificate?
    pub is_certified: bool,

    /// Has this open message expired
    pub is_expired: bool,

    /// associated single signatures
    pub single_signatures: Vec<SingleSignatures>,

    /// Message creation datetime, it is set by the database.
    pub created_at: DateTime<Utc>,

    /// Message expiration datetime, if it exists.
    pub expires_at: Option<DateTime<Utc>>,
}

impl From<OpenMessageWithSingleSignaturesRecord> for OpenMessageRecord {
    fn from(value: OpenMessageWithSingleSignaturesRecord) -> Self {
        Self {
            open_message_id: value.open_message_id,
            epoch: value.epoch,
            signed_entity_type: value.signed_entity_type,
            protocol_message: value.protocol_message,
            is_certified: value.is_certified,
            is_expired: value.is_expired,
            created_at: value.created_at,
            expires_at: value.expires_at,
        }
    }
}

impl SqLiteEntity for OpenMessageWithSingleSignaturesRecord {
    fn hydrate(row: Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let single_signatures = &row.read::<&str, _>(9);
        let single_signatures: Vec<SingleSignatures> = serde_json::from_str(single_signatures)
            .map_err(|e| {
                HydrationError::InvalidData(format!(
                    "Could not parse single signatures JSON: '{single_signatures}'. Error: {e}"
                ))
            })?;

        let open_message = OpenMessageRecord::hydrate(row)?;

        let open_message = Self {
            open_message_id: open_message.open_message_id,
            epoch: open_message.epoch,
            signed_entity_type: open_message.signed_entity_type,
            protocol_message: open_message.protocol_message,
            is_certified: open_message.is_certified,
            is_expired: open_message.is_expired,
            single_signatures,
            created_at: open_message.created_at,
            expires_at: open_message.expires_at,
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
            (
                "single_signatures",
                "case when {:single_signature:}.signer_id is null then json('[]') \
else json_group_array( \
    json_object( \
        'party_id', {:single_signature:}.signer_id, \
        'signature', {:single_signature:}.signature, \
        'indexes', json({:single_signature:}.lottery_indexes) \
    ) \
) end",
                "text",
            ),
        ])
    }
}
