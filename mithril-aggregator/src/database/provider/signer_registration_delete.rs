use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignerRegistrationRecord;

/// Query to delete old [SignerRegistrationRecord] from the sqlite database
pub(crate) struct DeleteSignerRegistrationRecordProvider<'conn> {
    connection: &'conn SqliteConnection,
}

impl<'conn> Provider<'conn> for DeleteSignerRegistrationRecordProvider<'conn> {
    type Entity = SignerRegistrationRecord;

    fn get_connection(&'conn self) -> &'conn SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
            "{:signer_registration:}",
            "signer_registration",
        )]));

        format!("delete from signer_registration where {condition} returning {projection}")
    }
}

impl<'conn> DeleteSignerRegistrationRecordProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn SqliteConnection) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to prune data older than the given Epoch.
    fn get_prune_condition(&self, epoch_threshold: Epoch) -> WhereCondition {
        let epoch_threshold = Value::Integer(epoch_threshold.try_into().unwrap());

        WhereCondition::new("epoch_setting_id < ?*", vec![epoch_threshold])
    }

    /// Prune the epoch setting data older than the given epoch.
    pub fn prune(
        &self,
        epoch_threshold: Epoch,
    ) -> StdResult<EntityCursor<SignerRegistrationRecord>> {
        let filters = self.get_prune_condition(epoch_threshold);

        self.find(filters)
    }
}
