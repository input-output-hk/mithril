//! Shared `WhereCondition` across open message queries

use sqlite::Value;

use mithril_common::StdResult;
use mithril_persistence::sqlite::WhereCondition;

use crate::database::record::OpenMessageRecord;

pub(crate) fn insert_one(record: OpenMessageRecord) -> StdResult<WhereCondition> {
    let expression = "(\
        open_message_id, epoch_setting_id, beacon, signed_entity_type_id, protocol_message, \
        expires_at, created_at, is_certified, is_expired\
    ) values (?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)";
    let beacon_str = record.signed_entity_type.get_json_beacon()?;
    let parameters = vec![
        Value::String(record.open_message_id.to_string()),
        Value::Integer(record.epoch.try_into()?),
        Value::String(beacon_str),
        Value::Integer(record.signed_entity_type.index() as i64),
        Value::String(serde_json::to_string(&record.protocol_message)?),
        record
            .expires_at
            .map(|t| Value::String(t.to_rfc3339()))
            .unwrap_or(Value::Null),
        Value::String(record.created_at.to_rfc3339()),
        Value::Integer(record.is_certified as i64),
        Value::Integer(record.is_expired as i64),
    ];

    Ok(WhereCondition::new(expression, parameters))
}
