use sqlite::{ConnectionThreadSafe, Value};

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    EntityCursor, Provider, SourceAlias, SqLiteEntity, WhereCondition,
};

use crate::database::record::CertificateRecord;

/// Query to delete old [CertificateRecord] from the sqlite database
pub struct DeleteCertificateProvider<'conn> {
    connection: &'conn ConnectionThreadSafe,
}

impl<'conn> Provider<'conn> for DeleteCertificateProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn ConnectionThreadSafe {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:certificate:}", "certificate")]));

        format!("delete from certificate where {condition} returning {projection}")
    }
}

impl<'conn> DeleteCertificateProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn ConnectionThreadSafe) -> Self {
        Self { connection }
    }

    /// Create the SQL condition to delete certificates with the given ids.
    pub fn get_delete_by_ids_condition(&self, ids: &[&str]) -> WhereCondition {
        let ids_values = ids.iter().map(|id| Value::String(id.to_string())).collect();

        WhereCondition::where_in("certificate_id", ids_values)
    }

    /// Delete the certificates with the given ids.
    pub fn delete_by_ids(&self, ids: &[&str]) -> StdResult<EntityCursor<CertificateRecord>> {
        let filters = self.get_delete_by_ids_condition(ids);

        self.find(filters)
    }
}
