use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignerRegistrationRecord;

/// Query to delete old [SignerRegistrationRecord] from the sqlite database
pub struct DeleteSignerRegistrationRecordQuery {
    condition: WhereCondition,
}

impl Query for DeleteSignerRegistrationRecordQuery {
    type Entity = SignerRegistrationRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
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

impl DeleteSignerRegistrationRecordQuery {
    /// Create the SQL query to prune data older than the given Epoch.
    pub fn below_epoch_threshold(epoch_threshold: Epoch) -> Self {
        let epoch_threshold = Value::Integer(epoch_threshold.try_into().unwrap());

        Self {
            condition: WhereCondition::new("epoch_setting_id < ?*", vec![epoch_threshold]),
        }
    }
}
