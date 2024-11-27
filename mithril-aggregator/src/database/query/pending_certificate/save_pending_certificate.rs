use chrono::Utc;
use sqlite::Value;

use mithril_persistence::sqlite::{Query, WhereCondition};

use crate::database::record::CertificatePendingRecord;

/// Query to save [CertificatePendingRecord] in the sqlite database
pub struct SavePendingCertificateRecordQuery {
    condition: WhereCondition,
}

impl SavePendingCertificateRecordQuery {
    pub fn save(pending_certificate_record: CertificatePendingRecord) -> Self {
        let condition = WhereCondition::new(
            "(epoch, pending_certificate, created_at) values (?*, ?*, ?*)",
            vec![
                Value::Integer(*pending_certificate_record.epoch as i64),
                Value::String(pending_certificate_record.pending_certificate),
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

        let projection = Self::Entity::expand_projection("pending_certificate");
        format!("insert or replace into pending_certificate {condition} returning {projection}")
    }
}
