use sqlite::Value;

use mithril_common::entities::{Epoch, SignedEntityType};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::OpenMessageWithSingleSignaturesRecord;

/// Simple queries to retrieve [OpenMessageWithSingleSignaturesRecord] from the sqlite database.
pub struct GetOpenMessageWithSingleSignaturesQuery {
    condition: WhereCondition,
}

impl GetOpenMessageWithSingleSignaturesQuery {
    pub fn by_epoch_and_signed_entity_type(
        epoch: Epoch,
        signed_entity_type: &SignedEntityType,
    ) -> StdResult<Self> {
        let condition = Self::get_epoch_condition(epoch)
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
}

impl Query for GetOpenMessageWithSingleSignaturesQuery {
    type Entity = OpenMessageWithSingleSignaturesRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[
            ("{:open_message:}", "open_message"),
            ("{:single_signature:}", "single_signature"),
        ]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!(
            r#"
select {projection}
from open_message
    left outer join single_signature
        on open_message.open_message_id = single_signature.open_message_id
where {condition}
group by open_message.open_message_id
order by open_message.created_at desc, open_message.rowid desc
"#
        )
    }
}
