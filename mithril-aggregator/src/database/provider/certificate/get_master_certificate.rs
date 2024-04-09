use sqlite::{ConnectionThreadSafe, Value};

use mithril_common::entities::Epoch;
use mithril_persistence::sqlite::{Provider, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::CertificateRecord;

/// Query to obtains the master [CertificateRecord] of an epoch
pub struct MasterCertificateProvider<'conn> {
    connection: &'conn ConnectionThreadSafe,
}

impl<'conn> MasterCertificateProvider<'conn> {
    /// Create a new instance
    pub fn new(connection: &'conn ConnectionThreadSafe) -> Self {
        Self { connection }
    }

    pub fn get_master_certificate_condition(&self, epoch: Epoch) -> WhereCondition {
        let epoch_i64: i64 = epoch.try_into().unwrap();
        WhereCondition::new(
            "certificate.epoch between ?* and ?*",
            vec![Value::Integer(epoch_i64 - 1), Value::Integer(epoch_i64)],
        )
        .and_where(
            WhereCondition::new("certificate.parent_certificate_id is null", vec![]).or_where(
                WhereCondition::new("certificate.epoch != parent_certificate.epoch", vec![]),
            ),
        )
    }
}

impl<'conn> Provider<'conn> for MasterCertificateProvider<'conn> {
    type Entity = CertificateRecord;

    fn get_connection(&'conn self) -> &'conn ConnectionThreadSafe {
        self.connection
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
