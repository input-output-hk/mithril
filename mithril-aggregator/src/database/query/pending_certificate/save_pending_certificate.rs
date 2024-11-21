use chrono::Utc;
use sqlite::Value;

use mithril_persistence::sqlite::{Query, SourceAlias, WhereCondition};

use crate::database::record::CertificatePendingRecord;

/// Query to update [CertificatePendingRecord] in the sqlite database
pub struct SavePendingCertificateRecordQuery {
    condition: WhereCondition,
}

impl SavePendingCertificateRecordQuery {
    pub fn save(pending_certificate_record: CertificatePendingRecord) -> Self {
        let condition = WhereCondition::new(
            "(epoch, certificate, created_at) values (?*, ?*, ?*)",
            vec![
                Value::Integer(*pending_certificate_record.epoch as i64),
                Value::String(pending_certificate_record.certificate),
                Value::String(Utc::now().to_rfc3339()),
            ],
        );

        Self { condition }
    }
}

impl Query for SavePendingCertificateRecordQuery {
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

        format!("insert or replace into new_pending_certificate {condition} returning {projection}")
    }
}
