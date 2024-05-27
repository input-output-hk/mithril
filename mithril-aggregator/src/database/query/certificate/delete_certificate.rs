use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Query to delete old [CertificateRecord] from the sqlite database
pub struct DeleteCertificateQuery {
    condition: WhereCondition,
}

impl Query for DeleteCertificateQuery {
    type Entity = CertificateRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:certificate:}", "certificate")]));

        format!("delete from certificate where {condition} returning {projection}")
    }
}

impl DeleteCertificateQuery {
    /// Create the SQL condition to delete certificates with the given ids.
    pub fn by_ids(ids: &[&str]) -> Self {
        let ids_values = ids.iter().map(|id| Value::String(id.to_string())).collect();

        Self {
            condition: WhereCondition::where_in("certificate_id", ids_values),
        }
    }
}
