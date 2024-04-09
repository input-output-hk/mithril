use sqlite::Value;

use mithril_common::{
    entities::{Epoch, SignedEntityType},
    StdResult,
};
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::OpenMessageRecord;

/// Simple queries to retrieve [OpenMessageRecord] from the sqlite database.
pub(crate) struct GetOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> GetOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id = ?*", vec![Value::Integer(*epoch as i64)])
    }

    pub(crate) fn get_signed_entity_type_condition(
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

    pub(crate) fn get_expired_entity_type_condition(&self, now: &str) -> WhereCondition {
        WhereCondition::new("expires_at < ?*", vec![Value::String(now.to_string())])
    }

    // Useful in test and probably in the future.
    #[allow(dead_code)]
    fn get_open_message_id_condition(&self, open_message_id: &str) -> WhereCondition {
        WhereCondition::new(
            "open_message_id = ?*",
            vec![Value::String(open_message_id.to_owned())],
        )
    }
}

impl<'client> Provider<'client> for GetOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
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
