use chrono::Utc;
use sqlite::{ConnectionThreadSafe, Value};
use uuid::Uuid;

use mithril_common::entities::{ProtocolParameters, SignerWithStake};
use mithril_common::{entities::Epoch, test_utils::fake_keys, StdError, StdResult};
use mithril_persistence::sqlite::{
    ConnectionBuilder, ConnectionExtensions, ConnectionOptions, Query, SqliteConnection,
};

use crate::database::query::{
    ImportSignerRecordQuery, InsertCertificateRecordQuery,
    InsertOrReplaceSignerRegistrationRecordQuery, InsertOrReplaceStakePoolQuery,
    InsertSignedEntityRecordQuery, UpdateEpochSettingQuery, UpdateSingleSignatureRecordQuery,
};
use crate::database::record::{
    CertificateRecord, SignedEntityRecord, SignerRecord, SignerRegistrationRecord,
    SingleSignatureRecord,
};

/// In-memory sqlite database without foreign key support with migrations applied
pub fn main_db_connection() -> StdResult<ConnectionThreadSafe> {
    let connection = ConnectionBuilder::open_memory()
        .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        .with_migrations(crate::database::migration::get_migrations())
        .build()?;
    Ok(connection)
}

/// In-memory sqlite database without foreign key support with cardano db migrations applied
pub fn cardano_tx_db_connection() -> StdResult<ConnectionThreadSafe> {
    let connection = ConnectionBuilder::open_memory()
        .with_options(&[ConnectionOptions::ForceDisableForeignKeys])
        .with_migrations(
            mithril_persistence::database::cardano_transaction_migration::get_migrations(),
        )
        .build()?;
    Ok(connection)
}

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
                    signature: fake_keys::single_signature()[3].to_string(),
                    created_at: Utc::now(),
                });
            }
        }
    }
    single_signature_records
}

pub fn insert_single_signatures_in_db(
    connection: &SqliteConnection,
    single_signature_records: Vec<SingleSignatureRecord>,
) -> StdResult<()> {
    if single_signature_records.is_empty() {
        return Ok(());
    }

    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) = UpdateSingleSignatureRecordQuery::one(
            single_signature_records.first().unwrap().clone(),
        )
        .filters()
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
                    Value::Integer(*single_signature_record.registration_epoch_setting_id as i64),
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

pub fn insert_certificate_records<T: Into<CertificateRecord>>(
    connection: &ConnectionThreadSafe,
    records: Vec<T>,
) {
    let _ = connection
        .fetch_first(InsertCertificateRecordQuery::many(
            records.into_iter().map(Into::into).collect(),
        ))
        .unwrap();
}

pub fn insert_epoch_settings(
    connection: &SqliteConnection,
    epoch_to_insert_settings: &[u64],
) -> StdResult<()> {
    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) =
            UpdateEpochSettingQuery::one(Epoch(1), ProtocolParameters::new(1, 2, 1.0))
                .filters()
                .expand();

        format!("insert into epoch_setting {sql_values}")
    };

    for (epoch, protocol_parameters) in epoch_to_insert_settings
        .iter()
        .map(|epoch| (epoch, ProtocolParameters::new(*epoch, epoch + 1, 1.0)))
    {
        let mut statement = connection.prepare(&query)?;
        statement
            .bind::<&[(_, Value)]>(&[
                (1, Value::Integer(*epoch as i64)),
                (
                    2,
                    serde_json::to_string(&protocol_parameters).unwrap().into(),
                ),
            ])
            .unwrap();

        statement.next().unwrap();
    }
    Ok(())
}

pub fn insert_signed_entities(
    connection: &SqliteConnection,
    signed_entity_records: Vec<SignedEntityRecord>,
) -> StdResult<()> {
    if signed_entity_records.is_empty() {
        return Ok(());
    }

    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) =
            InsertSignedEntityRecordQuery::one(signed_entity_records.first().unwrap().to_owned())
                .filters()
                .expand();
        format!("insert into signed_entity {sql_values}")
    };

    for signed_entity_record in signed_entity_records {
        let mut statement = connection.prepare(&query)?;
        statement
            .bind::<&[(_, Value)]>(&[
                (1, signed_entity_record.signed_entity_id.into()),
                (
                    2,
                    i64::try_from(signed_entity_record.signed_entity_type.index())
                        .unwrap()
                        .into(),
                ),
                (3, signed_entity_record.certificate_id.into()),
                (
                    4,
                    signed_entity_record
                        .signed_entity_type
                        .get_json_beacon()
                        .unwrap()
                        .into(),
                ),
                (5, signed_entity_record.artifact.into()),
                (6, signed_entity_record.created_at.to_rfc3339().into()),
            ])
            .unwrap();

        statement.next().unwrap();
    }

    Ok(())
}

pub fn insert_signers(
    connection: &SqliteConnection,
    signer_records: Vec<SignerRecord>,
) -> Result<(), StdError> {
    if signer_records.is_empty() {
        return Ok(());
    }

    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) =
            ImportSignerRecordQuery::one(signer_records.first().unwrap().to_owned())
                .filters()
                .expand();
        format!("insert into signer {sql_values}")
    };

    for signer_record in signer_records {
        let mut statement = connection.prepare(&query)?;
        statement
            .bind::<&[(_, Value)]>(&[
                (1, signer_record.signer_id.into()),
                (
                    2,
                    signer_record
                        .pool_ticker
                        .map(Value::String)
                        .unwrap_or(Value::Null),
                ),
                (3, signer_record.created_at.to_rfc3339().into()),
                (4, signer_record.updated_at.to_rfc3339().into()),
                (
                    5,
                    signer_record
                        .last_registered_at
                        .map(|d| Value::String(d.to_rfc3339()))
                        .unwrap_or(Value::Null),
                ),
            ])
            .unwrap();
        statement.next().unwrap();
    }

    Ok(())
}

pub fn insert_signer_registrations(
    connection: &SqliteConnection,
    signer_with_stakes_by_epoch: Vec<(Epoch, Vec<SignerWithStake>)>,
) -> StdResult<()> {
    if signer_with_stakes_by_epoch.is_empty() {
        return Ok(());
    }

    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) = InsertOrReplaceSignerRegistrationRecordQuery::one(
            SignerRegistrationRecord::from_signer_with_stake(
                signer_with_stakes_by_epoch
                    .first()
                    .unwrap()
                    .1
                    .first()
                    .unwrap()
                    .to_owned(),
                Epoch(1),
            ),
        )
        .filters()
        .expand();
        format!("insert into signer_registration {sql_values}")
    };

    for (epoch, signer_with_stakes) in signer_with_stakes_by_epoch {
        for signer_with_stake in signer_with_stakes {
            let signer_registration_record =
                SignerRegistrationRecord::from_signer_with_stake(signer_with_stake, epoch);
            let mut statement = connection.prepare(&query)?;
            statement
                .bind::<&[(_, Value)]>(&[
                    (1, signer_registration_record.signer_id.into()),
                    (
                        2,
                        Value::Integer(*signer_registration_record.epoch_setting_id as i64),
                    ),
                    (3, signer_registration_record.verification_key.into()),
                    (
                        4,
                        signer_registration_record
                            .verification_key_signature
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                    ),
                    (
                        5,
                        signer_registration_record
                            .operational_certificate
                            .map(Value::String)
                            .unwrap_or(Value::Null),
                    ),
                    (
                        6,
                        signer_registration_record
                            .kes_period
                            .map(|k| Value::Integer(k as i64))
                            .unwrap_or(Value::Null),
                    ),
                    (
                        7,
                        signer_registration_record
                            .stake
                            .map(|s| Value::Integer(s as i64))
                            .unwrap_or(Value::Null),
                    ),
                    (8, signer_registration_record.created_at.to_rfc3339().into()),
                ])
                .unwrap();

            statement.next().unwrap();
        }
    }

    Ok(())
}

pub fn insert_stake_pool(
    connection: &SqliteConnection,
    epoch_to_insert_stake_pools: &[i64],
) -> StdResult<()> {
    let query = {
        // leverage the expanded parameter from this query which is unit
        // tested on its own above.
        let (sql_values, _) =
            InsertOrReplaceStakePoolQuery::many(vec![("pool_id".to_string(), Epoch(1), 1000)])
                .filters()
                .expand();

        format!("insert into stake_pool {sql_values}")
    };

    // Note: decreasing stakes for pool3 so we can test that the order has changed
    for (pool_id, epoch, stake) in epoch_to_insert_stake_pools.iter().flat_map(|epoch| {
        [
            ("pool1", *epoch, 1000 + (epoch - 1) * 40),
            ("pool2", *epoch, 1100 + (epoch - 1) * 45),
            ("pool3", *epoch, 1200 - (epoch - 1) * 50),
        ]
    }) {
        let mut statement = connection.prepare(&query)?;
        statement
            .bind::<&[(_, Value)]>(&[
                (1, pool_id.to_string().into()),
                (2, Value::Integer(epoch)),
                (3, Value::Integer(stake)),
                (4, Utc::now().to_rfc3339().into()),
            ])
            .unwrap();
        statement.next().unwrap();
    }

    Ok(())
}
