use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificatePendingRecord;

/// Query to delete old [CertificatePendingRecord] from the sqlite database
pub struct DeletePendingCertificateRecordQuery {
    condition: WhereCondition,
}

impl DeletePendingCertificateRecordQuery {
    pub fn get() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

impl Query for DeletePendingCertificateRecordQuery {
    type Entity = CertificatePendingRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.

        // let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[(
        //     "{:pending_certificate:}",
        //     "new_pending_certificate",
        // )]));
        let projection = Self::Entity::get_projection_with_table("new_pending_certificate")
            .expand(SourceAlias::new(&[]));

        format!("delete from new_pending_certificate where {condition} returning {projection}")
    }
}
