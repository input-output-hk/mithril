use chrono::Utc;
use mithril_common::{entities::Epoch, StdResult};
use sqlite::{Connection, Value};
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
                    signature: "7b227369676d61223a5b3133302c32382c3134332c31372c38302c31302c3231352c3138382c3230352c3132322c31312c3233392c34362c3234352c32312c3139332c32382c3232312c3133302c34302c3131362c39322c3139362c33352c3235342c34332c3138382c362c38372c3136392c37312c3134352c3130342c3137382c392c3136362c39342c31332c3234372c3139302c3130322c37312c3232362c3230392c312c3230392c3235312c3137305d2c22696e6465786573223a5b302c312c382c31322c31332c31342c31382c31392c32332c32352c32362c32372c32382c33322c33332c33342c33352c33372c33382c33392c34312c34322c34332c34342c34352c34362c34372c34382c34392c35302c35312c35322c35332c35342c35352c35362c35372c35382c35392c36302c36312c36332c36342c36352c36372c36382c37302c37312c37352c37362c37372c37392c38302c38312c38322c38342c38352c38392c39302c39312c39332c39352c39372c39382c39395d2c227369676e65725f696e646578223a337d".to_string(),
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
            .bind::<&[(_, Value)]>(&[
                (
                    1,
                    single_signature_record.open_message_id.to_string().into(),
                ),
                (2, single_signature_record.signer_id.into()),
                (
                    3,
                    i64::try_from(single_signature_record.registration_epoch_setting_id.0)
                        .unwrap()
                        .into(),
                ),
                (
                    4,
                    serde_json::to_string(&single_signature_record.lottery_indexes)
                        .unwrap()
                        .into(),
                ),
                (5, single_signature_record.signature.into()),
                (6, single_signature_record.created_at.to_rfc3339().into()),
            ])
            .unwrap();
        statement.next().unwrap();
    }

    Ok(())
}
