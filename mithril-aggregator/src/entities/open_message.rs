use mithril_common::entities::{Epoch, ProtocolMessage, SignedEntityType, SingleSignatures};

use crate::database::provider::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};

/// ## OpenMessage
///
/// An open message is a message open for signatures. Every signer may send a
/// single signature for this message from which a multi signature will be
/// generated if possible.
#[derive(Debug, Clone, PartialEq)]
pub struct OpenMessage {
    /// OpenMessage unique identifier
    // pub open_message_id: Uuid, // do we need it in the entity ?

    /// Epoch
    pub epoch: Epoch,

    /// Type of message
    pub signed_entity_type: SignedEntityType,

    /// Message used by the Mithril Protocol
    pub protocol_message: ProtocolMessage,

    /// Has this message been converted into a Certificate?
    pub is_certified: bool,

    /// associated single signatures
    pub single_signatures: Vec<SingleSignatures>,
}

impl OpenMessage {
    #[cfg(test)]
    /// Create a dumb OpenMessage instance mainly for test purposes
    pub fn dummy() -> Self {
        use mithril_common::test_utils::fake_data;

        let beacon = mithril_common::test_utils::fake_data::beacon();
        let epoch = beacon.epoch;
        let signed_entity_type = SignedEntityType::CardanoImmutableFilesFull(beacon);

        Self {
            epoch,
            signed_entity_type,
            protocol_message: ProtocolMessage::new(),
            is_certified: false,
            single_signatures: vec![
                fake_data::single_signatures(vec![1, 4, 5]),
                fake_data::single_signatures(vec![2, 3, 8]),
            ],
        }
    }
}

impl From<OpenMessageRecord> for OpenMessage {
    fn from(record: OpenMessageRecord) -> Self {
        Self {
            epoch: record.epoch,
            signed_entity_type: record.signed_entity_type,
            protocol_message: record.protocol_message,
            is_certified: record.is_certified,
            single_signatures: vec![],
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
            single_signatures: record.single_signatures,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::database::provider::{OpenMessageRecord, OpenMessageWithSingleSignaturesRecord};
    use mithril_common::{
        entities::{Epoch, ProtocolMessage, SignedEntityType},
        test_utils::fake_data,
    };
    use std::vec;
    use uuid::Uuid;

    use super::OpenMessage;

    #[test]
    fn test_from_record() {
        let record = OpenMessageRecord {
            open_message_id: Uuid::new_v4(),
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            created_at: chrono::Local::now().naive_local(),
        };
        let expected = OpenMessage {
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            single_signatures: vec![],
        };
        let result: OpenMessage = record.into();

        assert_eq!(expected, result);
    }

    #[test]
    fn test_from_record_with_single_signatures() {
        let record = OpenMessageWithSingleSignaturesRecord {
            open_message_id: Uuid::new_v4(),
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            created_at: chrono::Local::now().naive_local(),
            single_signatures: vec![fake_data::single_signatures(vec![1, 4, 5])],
        };
        let expected = OpenMessage {
            epoch: Epoch(1),
            signed_entity_type: SignedEntityType::dummy(),
            protocol_message: ProtocolMessage::default(),
            is_certified: false,
            single_signatures: vec![fake_data::single_signatures(vec![1, 4, 5])],
        };
        let result: OpenMessage = record.into();

        assert_eq!(expected, result);
    }
}
