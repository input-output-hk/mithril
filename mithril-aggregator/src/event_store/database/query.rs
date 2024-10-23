use crate::event_store::{Event, EventMessage};
use chrono::Utc;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{Query, SourceAlias, SqLiteEntity, WhereCondition};

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
                sqlite::Value::String(format!(
                    r#"{{"headers": {}, "content": {}}}"#,
                    serde_json::to_string(&message.headers)?,
                    message.content
                )),
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
        let message = EventMessage::new("source", "action", "content");
        let (parameters, values) = InsertEventQuery::one(message).unwrap().filters().expand();

        assert_eq!(
            "(source, action, content, created_at) values (?1, ?2, ?3, ?4)".to_string(),
            parameters
        );
        assert_eq!(4, values.len());
    }
}
