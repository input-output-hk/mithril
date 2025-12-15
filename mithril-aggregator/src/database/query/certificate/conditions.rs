//! Shared `WhereCondition` across certificates queries

use std::iter::repeat_n;

use mithril_persistence::sqlite::WhereCondition;
use sqlite::Value;

use crate::database::record::CertificateRecord;

pub(super) fn insert_many(certificates_records: Vec<CertificateRecord>) -> WhereCondition {
    let columns = "(\
        certificate_id, \
        parent_certificate_id, \
        message, \
        signature, \
        aggregate_verification_key, \
        epoch, \
        network, \
        signed_entity_type_id, \
        signed_entity_beacon, \
        protocol_version, \
        protocol_parameters, \
        protocol_message, \
        signers, \
        initiated_at, \
        sealed_at)";
    let values_columns: Vec<&str> = repeat_n(
        "(?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*, ?*)",
        certificates_records.len(),
    )
    .collect();

    let values: Vec<Value> = certificates_records
        .into_iter()
        .flat_map(|certificate_record| {
            vec![
                Value::String(certificate_record.certificate_id),
                match certificate_record.parent_certificate_id {
                    Some(parent_certificate_id) => Value::String(parent_certificate_id),
                    None => Value::Null,
                },
                Value::String(certificate_record.message),
                Value::String(certificate_record.signature),
                Value::String(certificate_record.aggregate_verification_key),
                Value::Integer(certificate_record.epoch.try_into().unwrap()),
                Value::String(certificate_record.network),
                Value::Integer(certificate_record.signed_entity_type.index() as i64),
                Value::String(certificate_record.signed_entity_type.get_json_beacon().unwrap()),
                Value::String(certificate_record.protocol_version),
                Value::String(
                    serde_json::to_string(&certificate_record.protocol_parameters).unwrap(),
                ),
                Value::String(serde_json::to_string(&certificate_record.protocol_message).unwrap()),
                Value::String(serde_json::to_string(&certificate_record.signers).unwrap()),
                Value::String(certificate_record.initiated_at.to_rfc3339()),
                Value::String(certificate_record.sealed_at.to_rfc3339()),
            ]
        })
        .collect();

    WhereCondition::new(
        format!("{columns} values {}", values_columns.join(", ")).as_str(),
        values,
    )
}
