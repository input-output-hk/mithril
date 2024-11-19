use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificatePendingRecord;

/// Simple queries to retrieve [CertificatePendingRecord] from the sqlite database.
pub struct GetPendingCertificateRecordQuery {
    condition: WhereCondition,
}

impl GetPendingCertificateRecordQuery {
    pub fn get() -> Self {
        Self {
            condition: WhereCondition::default(),
        }
    }
}

#[cfg(test)]
mod test_extensions {
    use mithril_persistence::sqlite::WhereCondition;

    use super::*;

    impl GetPendingCertificateRecordQuery {
        /// Query to get PendingCertificateRecords for a given PendingCertificate id.
        pub fn by_pending_certificate_id(pending_certificate_id: String) -> Self {
            Self {
                condition: WhereCondition::new(
                    "PendingCertificate_id = ?*",
                    vec![sqlite::Value::String(pending_certificate_id)],
                ),
            }
        }
    }
}

impl Query for GetPendingCertificateRecordQuery {
    type Entity = CertificatePendingRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let aliases = SourceAlias::new(&[("{:pending_certificate:}", "new_pending_certificate")]);
        let projection = Self::Entity::get_projection().expand(aliases);
        // let projection = Self::Entity::get_projection_with_table("new_pending_certificate")
        //     .expand(SourceAlias::new(&[]));
        format!(
            // TODO XXX check the order to keep
            "select {projection} from new_pending_certificate where {condition} order by ROWID desc"
        )
    }
}
