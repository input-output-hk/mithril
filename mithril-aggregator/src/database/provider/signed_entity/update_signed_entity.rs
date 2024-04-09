use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{
    Provider, SourceAlias, SqLiteEntity, SqliteConnection, WhereCondition,
};

use crate::database::record::SignedEntityRecord;

/// Query to update [SignedEntityRecord] in the sqlite database
pub struct UpdateSignedEntityProvider<'client> {
    connection: &'client SqliteConnection,
}

impl<'client> UpdateSignedEntityProvider<'client> {
    /// Create a new instance
    pub fn new(connection: &'client SqliteConnection) -> Self {
        Self { connection }
    }

    fn get_update_condition(
        &self,
        signed_entity_record: &SignedEntityRecord,
    ) -> StdResult<WhereCondition> {
        let expression =
            "signed_entity_type_id = ?*, certificate_id = ?*, beacon = ?*, artifact = ?*, \
created_at = ?* \
where signed_entity_id = ?*";
        let parameters = vec![
            Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
            Value::String(signed_entity_record.certificate_id.to_owned()),
            Value::String(signed_entity_record.signed_entity_type.get_json_beacon()?),
            Value::String(signed_entity_record.artifact.to_owned()),
            Value::String(signed_entity_record.created_at.to_rfc3339()),
            Value::String(signed_entity_record.signed_entity_id.to_owned()),
        ];

        Ok(WhereCondition::new(expression, parameters))
    }

    pub(crate) fn persist(
        &self,
        signed_entity_record: &SignedEntityRecord,
    ) -> StdResult<SignedEntityRecord> {
        let filters = self.get_update_condition(signed_entity_record)?;
        let mut cursor = self.find(filters)?;

        cursor.next().ok_or_else(|| {
            panic!(
                "Updating a signed_entity should not return nothing, id = {:?}",
                signed_entity_record.signed_entity_id
            )
        })
    }
}

impl<'client> Provider<'client> for UpdateSignedEntityProvider<'client> {
    type Entity = SignedEntityRecord;

    fn get_connection(&'client self) -> &'client SqliteConnection {
        self.connection
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_entity:}", "signed_entity")]));

        format!("update signed_entity set {condition} returning {projection}")
    }
}
