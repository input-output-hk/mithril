use std::sync::Arc;

use anyhow::Context;
use async_trait::async_trait;

use mithril_common::entities::SignedEntityTypeDiscriminants;
use mithril_common::StdResult;
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    GetSignedEntityRecordQuery, InsertSignedEntityRecordQuery, UpdateSignedEntityQuery,
};
use crate::database::record::SignedEntityRecord;

/// Signed entity storer trait
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignedEntityStorer: Sync + Send {
    /// Store a signed entity
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()>;

    /// Get signed entity type
    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get signed entity type by certificate id
    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_hash: &str,
    ) -> StdResult<Option<SignedEntityRecord>>;

    /// Get signed entities type by certificates ids
    async fn get_signed_entities_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>>;

    /// Get last signed entities by signed entity type
    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>>;

    /// Perform an update for all the given signed entities.
    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>>;
}

/// Service to deal with signed_entity (read & write).
pub struct SignedEntityStore {
    connection: Arc<SqliteConnection>,
}

impl SignedEntityStore {
    /// Create a new SignedEntityStoreAdapter service
    pub fn new(connection: Arc<SqliteConnection>) -> Self {
        Self { connection }
    }
}

#[async_trait]
impl SignedEntityStorer for SignedEntityStore {
    async fn store_signed_entity(&self, signed_entity: &SignedEntityRecord) -> StdResult<()> {
        let _ = self
            .connection
            .fetch_first(InsertSignedEntityRecordQuery::one(signed_entity.clone()));

        Ok(())
    }

    async fn get_signed_entity(
        &self,
        signed_entity_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        self.connection
            .fetch_first(GetSignedEntityRecordQuery::by_signed_entity_id(
                signed_entity_id,
            ))
            .with_context(|| format!("get signed entity by id failure, id: {signed_entity_id}"))
    }

    async fn get_signed_entity_by_certificate_id(
        &self,
        certificate_id: &str,
    ) -> StdResult<Option<SignedEntityRecord>> {
        self.connection
            .fetch_first(GetSignedEntityRecordQuery::by_certificate_id(
                certificate_id,
            ))
            .with_context(|| {
                format!(
                    "get signed entity by certificate id failure, certificate_id: {certificate_id}"
                )
            })
    }

    async fn get_signed_entities_by_certificates_ids<'a>(
        &self,
        certificates_ids: &[&'a str],
    ) -> StdResult<Vec<SignedEntityRecord>> {
        self.connection
            .fetch_collect(GetSignedEntityRecordQuery::by_certificates_ids(
                certificates_ids,
            ))
    }

    async fn get_last_signed_entities_by_type(
        &self,
        signed_entity_type_id: &SignedEntityTypeDiscriminants,
        total: usize,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let cursor = self
            .connection
            .fetch(GetSignedEntityRecordQuery::by_signed_entity_type(
                signed_entity_type_id,
            )?)
            .with_context(|| {
                format!("get last signed entity by type failure, type: {signed_entity_type_id:?}")
            })?;
        let signed_entities: Vec<SignedEntityRecord> = cursor.take(total).collect();

        Ok(signed_entities)
    }

    async fn update_signed_entities(
        &self,
        signed_entities: Vec<SignedEntityRecord>,
    ) -> StdResult<Vec<SignedEntityRecord>> {
        let mut updated_records = vec![];

        for record in signed_entities {
            let id = record.signed_entity_id.clone();
            let updated_record = self
                .connection
                .fetch_first(UpdateSignedEntityQuery::one(record)?)?;

            updated_records.push(updated_record.unwrap_or_else(|| {
                panic!("Updating a signed_entity should not return nothing, id = {id:?}",)
            }));
        }

        Ok(updated_records)
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{MithrilStakeDistribution, SignedEntity, Snapshot};

    use crate::database::test_helper::{insert_signed_entities, main_db_connection};

    use super::*;

    fn insert_golden_signed_entities(connection: &SqliteConnection) {
        connection
            .execute(r#"
            -- Cardano immutable file full
            insert into signed_entity values(
                'bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1',
                2,
                '258edf0f1238c60985d0229869a6d4c4c635c118915b4d524d2686515be99946',
                '{"network":"preview","epoch":142,"immutable_file_number":2847}',
                '2023-05-09T13:11:15Z',
                '{
                    "digest":"bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1",
                    "beacon":{"network":"preview","epoch":142,"immutable_file_number":2847},
                    "size":1689696245,
                    "locations":["https://storage.googleapis.com/mithril-testing-preview-cs/preview-e142-i2847.bfcd77e372a25e13353bb77697d0d08785ba98b703e22640a317c5054dc05fb1.tar.gz"],
                    "compression_algorithm":"gzip",
                    "cardano_node_version": "0.0.1"
                }'
            );

            -- Mithril stake distribution
            insert into signed_entity
            values(
                '2da62e3ffee5e284ffd1e29ee52ee5547c5ff5ef34bee0a49dc54ea5e375f77e',
                0,
                'ad2d3705693dfaae8baac099b6976a5cc3e0f708245d0fa79d149a3fcbc79f00',
                203,
                '2023-05-16T02:17:16.203859116Z',
                '{
                    "type":"MithrilStakeDistribution",
                    "epoch":203,
                    "signers_with_stake":[{
                        "party_id":"pool1r0tln8nct3mpyvehgy6uu3cdlmjnmtr2fxjcqnfl6v0qg0we42e",
                        "verification_key":"7b22766b223a5b3138352c3132342c3231382c31362c3133312c3137382c3136302c37312c35382c3235312c31382c36382c37372c3135342c35382c3131352c3133392c3139392c38392c3230382c3139312c3235332c3138362c3232302c3133372c3135382c34312c3230332c382c3136352c3232362c3139342c3133382c3135322c35382c3131352c35342c3136322c3230332c32322c32332c3232382c3139342c34382c3137322c3139342c3130352c36382c302c3138302c3131332c3230312c3130392c3234372c39362c39342c3232372c3135372c36322c3139352c3134382c33352c3230352c3133372c3132312c3135322c3130302c3138342c3136372c3230362c3133322c34352c3133382c3131312c38392c3138322c3230352c3138372c3135382c32322c32332c3231382c36342c3137332c35392c3134312c3133332c3138302c3131392c36302c3134392c3134382c38332c3234312c3230312c33375d2c22706f70223a5b3137352c3135382c3130322c34352c3133322c38352c33312c3130342c36352c3230342c38352c312c34332c3137382c3138372c3233382c3135372c32372c39312c3230332c35342c37332c36322c35352c3131322c3131352c302c32312c3130302c3230382c37392c3135382c3233332c3132372c38332c3234352c3134362c3231382c3131382c3139332c38322c3139352c3137302c32312c36382c3231342c3138352c38372c3136382c3135372c3230322c3136382c3132392c3233332c38372c3230332c37372c36332c3232312c31362c3130392c33302c3235312c35312c38342c36392c3233372c39382c3133372c36302c39312c37362c38302c3232322c302c3130342c3231332c3132352c31332c3135312c3133312c3130312c3230312c33322c3138392c3137362c3139392c3131342c3234302c31352c33312c3136302c31332c3136352c32372c3134335d7d",
                        "verification_key_signature":"7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a7b227369676d61223a5b36322c33302c32362c3131302c3134382c3137342c36302c32392c3139302c34362c37342c3135392c3137312c3134362c31342c3231362c37352c32302c38352c3134342c32332c3134352c3132352c39382c36362c3132342c3139332c32352c3233352c3234372c3130342c382c352c3133342c302c3134302c3131352c33362c31342c33382c31322c34392c39372c33392c3232312c3234352c39322c362c35302c3134362c3135372c3136362c32382c38352c33312c33352c3232392c3233312c34332c3230322c34372c3235352c3138322c375d2c226c68735f706b223a5b3234322c3132322c34322c39332c39352c31322c36352c3131332c31342c35322c3135352c3133372c3130312c3137382c3232362c3133332c31372c39302c3138332c3132312c3136362c35322c342c39312c3135332c3232332c32352c3133372c37332c3137332c3235332c3233315d2c227268735f706b223a5b322c3231312c3230372c3234392c3234322c362c3131322c362c3235322c31322c3135362c3139332c38362c3133362c3138352c36342c3132342c35302c3230392c38382c3138322c3133352c32392c3138372c3133302c3138392c34312c3134302c34322c33342c3135392c3234365d7d2c226c68735f706b223a5b37352c3232312c3235302c3235322c3135382c3134362c35362c34312c39382c3137362c3139382c3231392c33352c3130392c3136332c36312c3139362c3139342c3137382c3130392c3132382c3131352c3130302c3135322c33392c3231392c35382c34392c3235302c33312c3138342c395d2c227268735f706b223a5b3135372c3232312c38372c3139342c3235322c3234382c3132372c33312c3136362c3235322c3233342c3232362c33362c3139352c3230312c33312c34372c3232302c3233372c3137342c3130372c3134342c33382c3234372c3135352c3135382c34372c3139302c3235322c3134302c3235342c3131375d7d2c226c68735f706b223a5b34322c37372c37392c36302c3137312c3234372c31392c3230332c3232302c36332c3231352c3135372c3132392c3230382c3135382c35352c3131302c3232312c3139372c3233322c38372c33312c31312c3235342c3133352c32372c3234352c3137352c3135342c3231382c3232312c3138345d2c227268735f706b223a5b36382c3230332c38362c31352c37352c33352c3232332c342c3130392c3234392c3231372c3132352c34372c3231372c3130342c36352c3131332c3234312c3235332c3138332c3138362c36362c37352c3135302c3233342c3138362c3137332c3233302c3130332c3139342c3135322c3132375d7d2c226c68735f706b223a5b3133312c38372c3135382c3233352c34312c3233372c33332c34362c3235342c38302c3235322c3132392c37332c3234382c3135332c33382c3138332c33342c3231362c3135362c3131302c36392c37322c39302c32382c36382c3131342c33352c33352c3134332c3234312c3231305d2c227268735f706b223a5b3232382c3135352c3133332c34312c3137392c39342c3233362c3133392c3231362c3136302c3130382c3137362c3134362c3232352c3134302c3231352c35392c3130372c32302c3133372c3139372c33392c3135332c3132362c3233372c3135382c3132332c392c3133322c3139342c3132312c3232355d7d2c226c68735f706b223a5b3232322c39362c35332c34312c32342c38382c3136342c39382c3133312c33342c3132362c3133392c32382c37342c34362c3137332c35302c3133362c39372c3137312c3130312c3136322c33312c3137352c32332c3130352c3231352c36332c37362c3132342c31322c3131365d2c227268735f706b223a5b3133382c31382c31302c352c3231382c3134372c35332c35322c33362c3234342c33362c3131302c31302c33382c3134382c3132332c3235302c3131352c36342c36372c3137332c3130352c3137392c3235342c3130352c332c3132372c32302c31322c3230352c37372c3230365d7d2c226c68735f706b223a5b3132342c382c37312c3135302c34352c3130362c3232322c3234372c3130302c3137342c34352c3135322c3136312c3130382c3135382c32372c3234342c35362c3131352c3233322c3136332c3234342c38372c3138332c3232372c3235302c3232372c3234382c3137352c3136332c3230392c37345d2c227268735f706b223a5b3134302c33372c3131392c36332c39302c3132302c3131332c3135372c3130352c34362c31342c33332c3230372c3131322c3131332c3235342c38342c37332c3131302c33392c392c3230372c3133312c342c3232352c39302c3135312c32302c31352c36342c39372c39385d7d",
                        "operational_certificate":"5b5b5b3138332c33342c3231362c34362c3232372c3235312c37342c3130312c31352c3233332c3234392c34322c312c37372c37322c3234382c3137392c32312c3137332c3131332c3131382c3139382c36322c3133352c34352c38382c3138372c3233332c34302c37322c31362c36365d2c312c3132332c5b31362c3136392c3134312c3138332c32322c3137342c3131312c33322c36342c35322c3234392c36382c3230322c33352c3130362c332c38362c3230352c37382c3230302c3138362c39342c3139372c3232382c37392c3137352c32392c31342c3132382c36332c35392c3139382c36322c3233302c34362c34312c38342c39382c3131392c3134352c32392c3132312c33352c3139372c3132382c3137322c302c3135342c392c31332c32362c3138332c3138362c3138362c33312c3234392c3133322c3232392c3235332c3134332c3130322c3235342c3231322c315d5d2c5b3234312c32372c31332c34342c3131342c37382c3138392c3234392c3135302c3135302c35332c3134342c3233362c3135312c38382c3134302c3132382c3136322c36302c3232382c38382c3131312c392c3134342c3233322c38332c39342c3231302c3135362c3136382c33352c3234325d5d",
                        "kes_period":12,
                        "stake":9497629046
                    }],
                    "hash":"2da62e3ffee5e284ffd1e29ee52ee5547c5ff5ef34bee0a49dc54ea5e375f77e",
                    "protocol_parameters":{"k":2422,"m":20973,"phi_f":0.2}}'
            );

            "#,
            )
            .unwrap();
    }

    #[tokio::test]
    async fn test_golden_master() {
        let connection = main_db_connection().unwrap();
        insert_golden_signed_entities(&connection);

        let store = SignedEntityStore::new(Arc::new(connection));
        let cardano_immutable_files_fulls: Vec<SignedEntity<Snapshot>> = store
            .get_last_signed_entities_by_type(
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                usize::MAX,
            )
            .await
            .expect("Getting Golden snapshot signed entities should not fail")
            .into_iter()
            .map(|r| r.try_into().unwrap())
            .collect();
        let mithril_stake_distributions: Vec<SignedEntity<MithrilStakeDistribution>> = store
            .get_last_signed_entities_by_type(
                &SignedEntityTypeDiscriminants::MithrilStakeDistribution,
                usize::MAX,
            )
            .await
            .expect("Getting Golden mithril stake distribution signed entities should not fail")
            .into_iter()
            .map(|r| r.try_into().unwrap())
            .collect();

        assert_eq!(cardano_immutable_files_fulls.len(), 1);
        assert_eq!(mithril_stake_distributions.len(), 1);
    }

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificate_id() {
        let expected_record = SignedEntityRecord::fake_records(1).remove(0);
        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, vec![expected_record.clone()]).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));

        let record = store
            .get_signed_entity_by_certificate_id(&expected_record.certificate_id)
            .await
            .expect("querying signed entity record by certificate id should not fail");

        assert_eq!(Some(expected_record), record);
    }

    #[tokio::test]
    async fn test_get_signed_entity_record_by_certificates_ids() {
        let expected_records = SignedEntityRecord::fake_records(3);
        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, expected_records.clone()).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));
        let certificates_ids: Vec<&str> = expected_records
            .iter()
            .map(|r| r.certificate_id.as_str())
            .collect();

        let queried_records = store
            .get_signed_entities_by_certificates_ids(&certificates_ids)
            .await
            .expect("querying signed entity record by certificates ids should not fail");

        assert_eq!(
            // Records are inserted older to earlier and queried the other way round
            expected_records.into_iter().rev().collect::<Vec<_>>(),
            queried_records
        );
    }

    #[tokio::test]
    async fn update_only_given_entities() {
        let mut signed_entity_records = SignedEntityRecord::fake_records(5);

        let connection = main_db_connection().unwrap();
        insert_signed_entities(&connection, signed_entity_records.clone()).unwrap();
        let store = SignedEntityStore::new(Arc::new(connection));

        let records_to_update: Vec<SignedEntityRecord> = signed_entity_records
            .drain(2..)
            .map(|mut r| {
                r.certificate_id = format!("updated-{}", r.certificate_id);
                r
            })
            .collect();
        let expected_records: Vec<SignedEntityRecord> = signed_entity_records
            .into_iter()
            .chain(records_to_update.clone())
            .rev() // Records are returned from latest to oldest
            .collect();

        let updated_records = store
            .update_signed_entities(records_to_update.clone())
            .await
            .expect("updating signed entities should not fail");

        let stored_records = store
            .get_last_signed_entities_by_type(
                &SignedEntityTypeDiscriminants::CardanoImmutableFilesFull,
                usize::MAX,
            )
            .await
            .expect("getting signed entities should not fail");

        assert_eq!(records_to_update, updated_records);
        assert_eq!(expected_records, stored_records);
    }
}
