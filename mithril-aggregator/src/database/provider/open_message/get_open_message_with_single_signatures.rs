use sqlite::Value;

use mithril_common::entities::{Epoch, SignedEntityType};
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::OpenMessageWithSingleSignaturesRecord;

/// Simple queries to retrieve [OpenMessageWithSingleSignaturesRecord] from the sqlite database.
pub struct GetOpenMessageWithSingleSignaturesProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetOpenMessageWithSingleSignaturesProvider<'client> {
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(*epoch as i64)])
    }

    pub fn get_signed_entity_type_condition(
        &self,
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

impl<'client> Provider<'client> for GetOpenMessageWithSingleSignaturesProvider<'client> {
    type Entity = OpenMessageWithSingleSignaturesRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
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
