use chrono::{DateTime, Utc};

use mithril_common::entities::{
    Epoch, PartyId, ProtocolMessage, SignedEntityType, SingleSignature,
};

use crate::database::record::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};

/// ## OpenMessage
///
/// An open message is a message open for signatures. Every signer may send a
/// single signature for this message from which a multi signature will be
/// generated if possible.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct OpenMessage {
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
    pub single_signatures: Vec<SingleSignature>,

    /// Message creation datetime
    pub created_at: DateTime<Utc>,

    /// Message expiration datetime, if it exists.
    pub expires_at: Option<DateTime<Utc>>,
}

impl OpenMessage {
    /// Gather all signers party_id for this open message
    pub fn get_signers_id(&self) -> Vec<PartyId> {
        self.single_signatures
            .iter()
            .map(|sig| sig.party_id.to_owned())
            .collect()
    }
}

impl From<OpenMessageRecord> for OpenMessage {
    fn from(record: OpenMessageRecord) -> Self {
        Self {
            epoch: record.epoch,
            signed_entity_type: record.signed_entity_type,
            protocol_message: record.protocol_message,
            is_certified: record.is_certified,
            is_expired: record.is_expired,
            single_signatures: vec![],
            created_at: record.created_at,
            expires_at: record.expires_at,
        }
    }
}

impl From<OpenMessageWithSingleSignaturesRecord> for OpenMessage {
    fn from(record: OpenMessageWithSingleSignaturesRecord) -> Self {
        Self {
            epoch: record.epoch,
            signed_entity_type: record.signed_entity_type,
            protocol_message: record.protocol_message,
            is_certified: record.is_certified,
            is_expired: record.is_expired,
            single_signatures: record.single_signatures,
            created_at: record.created_at,
            expires_at: record.expires_at,
        }
    }
}

#[cfg(test)]
mod test {
    use chrono::Utc;
    use uuid::Uuid;

    use mithril_common::{
        entities::{Epoch, ProtocolMessage, SignedEntityType},
        test::double::{Dummy, fake_data},
    };

    use crate::database::record::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};

    use super::OpenMessage;

    #[test]
    fn test_from_record() {
        let created_at = Utc::now();
        let record = OpenMessageRecord {
            open_message_id: OpenMessageRecord::new_id(),
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            is_expired: false,
            created_at,
            expires_at: None,
        };
        let expected = OpenMessage {
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            is_expired: false,
            single_signatures: vec![],
            created_at,
            expires_at: None,
        };
        let result: OpenMessage = record.into();

        assert_eq!(expected, result);
    }

    #[test]
    fn test_from_record_with_single_signatures() {
        let created_at = Utc::now();
        let record = OpenMessageWithSingleSignaturesRecord {
            open_message_id: Uuid::new_v4(),
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            is_expired: false,
            created_at,
            expires_at: None,
            single_signatures: vec![fake_data::single_signature(vec![1, 4, 5])],
        };
        let expected = OpenMessage {
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            is_expired: false,
            single_signatures: vec![fake_data::single_signature(vec![1, 4, 5])],
            created_at,
            expires_at: None,
        };
        let result: OpenMessage = record.into();

        assert_eq!(expected, result);
    }
}
