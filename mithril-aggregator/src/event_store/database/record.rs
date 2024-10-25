use chrono::{DateTime, Utc};
use mithril_persistence::sqlite::{HydrationError, Projection, SqLiteEntity};

use crate::event_store::Event;

impl SqLiteEntity for Event {
    fn hydrate(row: sqlite::Row) -> Result<Self, HydrationError>
    where
        Self: Sized,
    {
        let created_at = &row.read::<&str, _>("created_at");

        let myself = Self {
            event_id: row.read::<i64, _>("event_id"),
            created_at: DateTime::parse_from_rfc3339(created_at)
                .map_err(|e| {
                    HydrationError::InvalidData(format!(
                        "Could not turn string '{created_at}' to rfc3339 Datetime. Error: {e}"
                    ))
                })?
                .with_timezone(&Utc),
            source: row.read::<&str, _>("source").to_string(),
            action: row.read::<&str, _>("action").to_string(),
            content: row.read::<&str, _>("content").to_string(),
        };

        Ok(myself)
    }

    fn get_projection() -> Projection {
        let mut projection = Projection::default();
        projection.add_field("event_id", "event_id", "int");
        projection.add_field("created_at", "created_at", "string");
        projection.add_field("source", "source", "string");
        projection.add_field("action", "action", "string");
        projection.add_field("content", "content", "string");

        projection
    }
}

#[cfg(test)]
mod tests {
    use mithril_persistence::sqlite::SourceAlias;

    use super::*;

    #[test]
    fn event_projection() {
        let projection = Event::get_projection();

        assert_eq!(
            "event_id as event_id, created_at as created_at, source as source, action as action, content as content".to_string(),
            projection.expand(SourceAlias::default())
        )
    }
}
