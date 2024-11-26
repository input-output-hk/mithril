use mithril_persistence::sqlite::{Query, WhereCondition};

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

impl Query for GetPendingCertificateRecordQuery {
    type Entity = CertificatePendingRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::expand_projection("pending_certificate");
        format!(
            "select {projection} from pending_certificate where {condition} order by ROWID desc"
        )
    }
}
