use crate::event_store::{Event, EventMessage};
use chrono::Utc;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};
use serde_json::json;

/// Queries to insert [EventMessage] to the sqlite database.
pub struct InsertEventQuery {
    condition: WhereCondition,
}

impl InsertEventQuery {
    pub fn one(message: EventMessage) -> StdResult<Self> {
        let condition = WhereCondition::new(
            "(source, action, content, created_at) values (?*, ?*, ?*, ?*)",
            vec![
                sqlite::Value::String(message.source),
                sqlite::Value::String(message.action),
                sqlite::Value::String(serde_json::to_string(&json!({
                    "content": message.content,
                    "headers": message.headers,
                }))?),
                sqlite::Value::String(Utc::now().to_rfc3339()),
            ],
        );

        Ok(Self { condition })
    }
}

impl Query for InsertEventQuery {
    type Entity = Event;

    fn filters(&self) -> WhereCondition {
        self.condition.clone()
    }

    fn get_definition(&self, data: &str) -> String {
        let projection = Self::Entity::get_projection().expand(SourceAlias::default());

        format!(r#"insert into event {data} returning {projection}"#)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn provider_sql() {
        let message = EventMessage::new("source", "action", serde_json::json!("content"));

        let (final_expression, parameters) =
            InsertEventQuery::one(message).unwrap().filters().expand();

        assert_eq!(
            "(source, action, content, created_at) values (?1, ?2, ?3, ?4)".to_string(),
            final_expression
        );
        assert_eq!(4, parameters.len());
    }

    #[test]
    fn build_a_json_for_content_field_with_content_and_headers() {
        let content = serde_json::json!({
            "attr1": "content".to_string(),
            "attr2": 123,
        });

        let mut message = EventMessage::new("source", "action", content);
        message
            .headers
            .insert("key".to_string(), "value".to_string());

        let (_, parameters) = InsertEventQuery::one(message).unwrap().filters().expand();

        assert_eq!(
            sqlite::Value::String(
                r#"{"content":{"attr1":"content","attr2":123},"headers":{"key":"value"}}"#
                    .to_string()
            ),
            parameters[2]
        );
    }
}
