use chrono::Utc;
use mithril_common::{entities::Epoch, StdResult};
use sqlite::Connection;
use uuid::Uuid;

use crate::database::{migration::get_migrations, provider::UpdateSingleSignatureRecordProvider};

use super::SingleSignatureRecord;

pub fn setup_single_signature_records(
    total_epoch: u64,
    total_open_message: u64,
    total_signer: u64,
) -> Vec<SingleSignatureRecord> {
    let mut single_signature_records = Vec::new();
    for epoch in 1..=total_epoch {
        for open_message_idx in 1..=total_open_message {
            let open_message_id = Uuid::new_v4();
            for signer_idx in 1..=total_signer {
                let single_signature_id = epoch
                    + (epoch + 1) * open_message_idx
                    + (epoch + 1) * (open_message_idx + 1) * signer_idx;
                single_signature_records.push(SingleSignatureRecord {
                    open_message_id,
                    signer_id: format!("signer-{signer_idx}"),
                    registration_epoch_setting_id: Epoch(epoch),
                    lottery_indexes: (1..=single_signature_id).collect(),
                    signature: format!("signature-{single_signature_id}"),
                    created_at: Utc::now(),
                });
            }
        }
    }
    single_signature_records
}

pub fn disable_foreign_key_support(connection: &Connection) -> StdResult<()> {
    connection
        .execute("pragma foreign_keys=false")
        .map_err(|e| {
            format!("SQLite initialization: could not enable FOREIGN KEY support. err: {e}")
        })?;
    Ok(())
}

pub fn apply_all_migrations_to_db(connection: &Connection) -> StdResult<()> {
    for migration in get_migrations() {
        connection.execute(&migration.alterations)?;
    }

    Ok(())
}

pub fn insert_single_signatures_in_db(
    connection: &Connection,
    single_signature_records: Vec<SingleSignatureRecord>,
) -> StdResult<()> {
    if single_signature_records.is_empty() {
        return Ok(());
    }

    let query = {
        // leverage the expanded parameter from this provider which is unit
        // tested on its own above.
        let update_provider = UpdateSingleSignatureRecordProvider::new(connection);
        let (sql_values, _) = update_provider
            .get_update_condition(single_signature_records.first().unwrap())
            .expand();
        format!("insert into single_signature {sql_values}")
    };

    for single_signature_record in single_signature_records {
        let mut statement = connection.prepare(&query)?;

        statement
            .bind(
                1,
                single_signature_record.open_message_id.to_string().as_str(),
            )
            .unwrap();
        statement
            .bind(2, single_signature_record.signer_id.as_str())
            .unwrap();
        statement
            .bind(
                3,
                single_signature_record.registration_epoch_setting_id.0 as i64,
            )
            .unwrap();
        statement
            .bind(
                4,
                serde_json::to_string(&single_signature_record.lottery_indexes)
                    .unwrap()
                    .as_str(),
            )
            .unwrap();
        statement
            .bind(5, single_signature_record.signature.as_str())
            .unwrap();
        statement
            .bind(6, single_signature_record.created_at.to_rfc3339().as_str())
            .unwrap();
        statement.next().unwrap();
    }

    Ok(())
}
