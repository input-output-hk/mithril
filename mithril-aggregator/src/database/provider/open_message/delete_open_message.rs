use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::OpenMessageRecord;

/// Query to delete old [OpenMessageRecord] from the sqlite database
pub struct DeleteOpenMessageProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> DeleteOpenMessageProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    pub(crate) fn get_epoch_condition(&self, epoch: Epoch) -> WhereCondition {
        WhereCondition::new("epoch_setting_id < ?*", vec![Value::Integer(*epoch as i64)])
    }
}

impl<'client> Provider<'client> for DeleteOpenMessageProvider<'client> {
    type Entity = OpenMessageRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:open_message:}", "open_message")]);
        let projection = Self::Entity::get_projection().expand(aliases);

        format!("delete from open_message where {condition} returning {projection}")
    }
}
