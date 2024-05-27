use sqlite::Value;

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Query to obtains the master [CertificateRecord] of an epoch
pub struct MasterCertificateQuery {
    condition: WhereCondition,
}

impl MasterCertificateQuery {
    pub fn for_epoch(epoch: Epoch) -> Self {
        let epoch_i64: i64 = epoch.try_into().unwrap();
        let condition = WhereCondition::new(
            "certificate.epoch between ?* and ?*",
            vec![Value::Integer(epoch_i64 - 1), Value::Integer(epoch_i64)],
        )
        .and_where(
            WhereCondition::new("certificate.parent_certificate_id is null", vec![]).or_where(
                WhereCondition::new("certificate.epoch != parent_certificate.epoch", vec![]),
            ),
        );

        Self { condition }
    }
}

impl Query for MasterCertificateQuery {
    type Entity = CertificateRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        // it is important to alias the fields with the same name as the table
        // since the table cannot be aliased in a RETURNING statement in SQLite.
        let projection = Self::Entity::get_projection().expand(SourceAlias::new(&[
            ("{:certificate:}", "certificate"),
            ("{:parent_certificate:}", "parent_certificate"),
        ]));

        format!(
            r#"
select {projection}
from certificate
    left join certificate as parent_certificate
        on parent_certificate.certificate_id = certificate.parent_certificate_id
where {condition}
order by certificate.ROWID desc"#
        )
    }
}
