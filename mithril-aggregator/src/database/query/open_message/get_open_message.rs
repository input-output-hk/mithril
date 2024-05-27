use chrono::{DateTime, Utc};
use sqlite::Value;

use mithril_common::{
    entities::{Epoch, SignedEntityType},
    StdResult,
};
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::OpenMessageRecord;

/// Simple queries to retrieve [OpenMessageRecord] from the sqlite database.
pub struct GetOpenMessageQuery {
    condition: WhereCondition,
}

impl GetOpenMessageQuery {
    pub fn by_epoch_and_signed_entity_type(
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Self> {
        let condition = Self::get_epoch_condition(epoch)
            .and_where(Self::get_signed_entity_type_condition(signed_entity_type)?);

        Ok(Self { condition })
    }

    pub fn by_expired_entity_type(
        now: DateTime<Utc>,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Self> {
        let condition = Self::get_expired_condition(now)
            .and_where(Self::get_signed_entity_type_condition(signed_entity_type)?);

        Ok(Self { condition })
    }

    fn get_epoch_condition(epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(*epoch as i64)])
    }

    fn get_signed_entity_type_condition(
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<WhereCondition> {
        Ok(WhereCondition::new(
            "signed_entity_type_id = ?* and beacon = ?*",
            vec![
                Value::Integer(signed_entity_type.index() as i64),
                Value::String(signed_entity_type.get_json_beacon()?),
            ],
        ))
    }

    fn get_expired_condition(expires_at: DateTime<Utc>) -> WhereCondition {
        WhereCondition::new(
            "expires_at < ?*",
            vec![Value::String(expires_at.to_rfc3339())],
        )
    }

    #[cfg(test)]
    pub fn by_id(open_message_id: &uuid::Uuid) -> Self {
        Self {
            condition: WhereCondition::new(
                "open_message_id = ?*",
                vec![Value::String(open_message_id.to_string())],
            ),
        }
    }
}

impl Query for GetOpenMessageQuery {
    type Entity = OpenMessageRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("select {projection} from open_message where {condition} order by created_at desc")
    }
}
