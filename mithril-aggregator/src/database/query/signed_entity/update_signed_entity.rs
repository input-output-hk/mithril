use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

use crate::database::record::SignedEntityRecord;

/// Query to update [SignedEntityRecord] in the sqlite database
pub struct UpdateSignedEntityQuery {
    condition: WhereCondition,
}

impl UpdateSignedEntityQuery {
    pub fn one(signed_entity_record: SignedEntityRecord) -> StdResult<Self> {
        let expression =
            "signed_entity_type_id = ?*, certificate_id = ?*, beacon = ?*, artifact = ?*, \
created_at = ?* \
where signed_entity_id = ?*";
        let parameters = vec![
            Value::Integer(signed_entity_record.signed_entity_type.index() as i64),
            Value::String(signed_entity_record.certificate_id),
            Value::String(signed_entity_record.signed_entity_type.get_json_beacon()?),
            Value::String(signed_entity_record.artifact),
            Value::String(signed_entity_record.created_at.to_rfc3339()),
            Value::String(signed_entity_record.signed_entity_id),
        ];

        Ok(Self {
            condition: WhereCondition::new(expression, parameters),
        })
    }
}

impl Query for UpdateSignedEntityQuery {
    type Entity = SignedEntityRecord;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, condition: &str) -> String {
        let projection = Self::Entity::get_projection()
            .expand(SourceAlias::new(&[("{:signed_entity:}", "signed_entity")]));

        format!("update signed_entity set {condition} returning {projection}")
    }
}
