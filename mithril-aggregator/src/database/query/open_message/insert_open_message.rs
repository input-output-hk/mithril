use chrono::Utc;
use mithril_common::StdResult;
use mithril_common::entities::{Epoch, ProtocolMessage, SignedEntityType};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::query::open_message::conditions;
use crate::database::record::OpenMessageRecord;

/// Query to insert [OpenMessageRecord] in the sqlite database
pub struct InsertOpenMessageQuery {
    condition: WhereCondition,
}

impl InsertOpenMessageQuery {
    pub fn one(
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
        protocol_message: &ProtocolMessage,
    ) -> StdResult<Self> {
        let now = Utc::now();
        let record = OpenMessageRecord {
            open_message_id: OpenMessageRecord::new_id(),
            epoch,
            signed_entity_type: signed_entity_type.clone(),
            protocol_message: protocol_message.clone(),
            is_certified: false,
            is_expired: false,
            created_at: now,
            expires_at: signed_entity_type.get_open_message_timeout().map(|t| now + t),
        };

        Ok(Self {
            condition: conditions::insert_one(record)?,
        })
    }
}

impl Query for InsertOpenMessageQuery {
    type Entity = OpenMessageRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("insert into open_message {condition} returning {projection}")
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::ProtocolMessagePartKey;
    use mithril_persistence::sqlite::ConnectionExtensions;

    use super::*;
    use crate::database::query::GetOpenMessageQuery;
    use crate::database::test_helper::main_db_connection;

    #[test]
    fn test_insert_one() {
        let connection = main_db_connection().unwrap();
        let epoch = Epoch(5);
        let signed_entity_type = SignedEntityType::CardanoStakeDistribution(Epoch(10));
        let mut protocol_message = ProtocolMessage::new();
        protocol_message.set_message_part(
            ProtocolMessagePartKey::CardanoStakeDistributionEpoch,
            "value".to_string(),
        );

        connection
            .fetch_first(
                InsertOpenMessageQuery::one(epoch, &signed_entity_type, &protocol_message).unwrap(),
            )
            .unwrap();
        let records: Vec<OpenMessageRecord> =
            connection.fetch_collect(GetOpenMessageQuery::all()).unwrap();

        assert_eq!(1, records.len());
        assert_eq!(
            OpenMessageRecord {
                open_message_id: records[0].open_message_id,
                epoch,
                signed_entity_type,
                protocol_message,
                is_certified: false,
                is_expired: false,
                created_at: records[0].created_at,
                expires_at: records[0].expires_at,
            },
            records[0]
        );
    }

    #[should_panic]
    #[test]
    fn test_insert_two_entries_with_same_signed_entity_violate_unique_constraint() {
        let connection = main_db_connection().unwrap();
        let epoch = Epoch(5);
        let signed_entity_type = SignedEntityType::MithrilStakeDistribution(Epoch(10));

        connection
            .fetch_first(
                InsertOpenMessageQuery::one(epoch, &signed_entity_type, &ProtocolMessage::new())
                    .unwrap(),
            )
            .unwrap();

        let _ = connection.fetch_first(
            InsertOpenMessageQuery::one(epoch + 10, &signed_entity_type, &ProtocolMessage::new())
                .unwrap(),
        );
    }
}
