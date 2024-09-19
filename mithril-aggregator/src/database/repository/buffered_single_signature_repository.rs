use anyhow::Context;
use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::entities::{SignedEntityTypeDiscriminants, SingleSignatures};
use mithril_common::{StdError, StdResult};
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::{
    DeleteBufferedSingleSignatureQuery, GetBufferedSingleSignatureQuery,
    InsertOrReplaceBufferedSingleSignatureRecordQuery,
};
use crate::database::record::BufferedSingleSignatureRecord;
use crate::services::BufferedSingleSignatureStore;

/// An implementation of [BufferedSingleSignatureStore] that uses a SQLite database.
pub struct BufferedSingleSignatureRepository {
    connection: Arc<SqliteConnection>,
}

impl BufferedSingleSignatureRepository {
    /// Creates a new [BufferedSingleSignatureRepository] instance.
    pub fn new(connection_pool: Arc<SqliteConnection>) -> Self {
        Self {
            connection: connection_pool,
        }
    }

    #[cfg(test)]
    fn get_all(&self) -> StdResult<Vec<BufferedSingleSignatureRecord>> {
        self.connection
            .fetch_collect(GetBufferedSingleSignatureQuery::all())
    }

    fn get_by_discriminant<T>(
        &self,
        signed_entity_type_discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<T>>
    where
        T: TryFrom<BufferedSingleSignatureRecord>,
        StdError: From<T::Error>,
    {
        let records: Vec<BufferedSingleSignatureRecord> =
            self.connection
                .fetch_collect(GetBufferedSingleSignatureQuery::by_discriminant(
                    signed_entity_type_discriminant,
                ))?;

        let mut entities: Vec<T> = Vec::with_capacity(records.len());
        for record in records {
            entities.push(record.try_into()?);
        }

        Ok(entities)
    }
}

#[async_trait]
impl BufferedSingleSignatureStore for BufferedSingleSignatureRepository {
    async fn buffer_signature(
        &self,
        signed_entity_type_discriminant: SignedEntityTypeDiscriminants,
        signature: &SingleSignatures,
    ) -> StdResult<()> {
        let record = BufferedSingleSignatureRecord::try_from_single_signatures(
            signature,
            signed_entity_type_discriminant,
        )
        .with_context(|| "Failed to convert SingleSignatures to BufferedSingleSignatureRecord")?;

        self.connection
            .fetch_first(InsertOrReplaceBufferedSingleSignatureRecordQuery::one(
                record,
            ))?;

        Ok(())
    }

    async fn get_buffered_signatures(
        &self,
        signed_entity_type_discriminant: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<SingleSignatures>> {
        self.get_by_discriminant(signed_entity_type_discriminant)
    }

    async fn remove_buffered_signatures(
        &self,
        signed_entity_type_discriminant: SignedEntityTypeDiscriminants,
        single_signatures: Vec<SingleSignatures>,
    ) -> StdResult<()> {
        let signatures_party_ids = single_signatures.into_iter().map(|s| s.party_id).collect();
        self.connection.fetch_first(
            DeleteBufferedSingleSignatureQuery::by_discriminant_and_party_ids(
                signed_entity_type_discriminant,
                signatures_party_ids,
            ),
        )?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::SignedEntityTypeDiscriminants::{
        CardanoTransactions, MithrilStakeDistribution,
    };
    use mithril_common::test_utils::fake_keys;

    use crate::database::record::{strip_buffered_sigs_date, BufferedSingleSignatureRecord};
    use crate::database::test_helper::{insert_buffered_single_signatures, main_db_connection};

    use super::*;

    #[test]
    fn retrieve_all() {
        let connection = main_db_connection().unwrap();
        insert_buffered_single_signatures(
            &connection,
            BufferedSingleSignatureRecord::fakes(&[
                ("party1", CardanoTransactions),
                ("party2", CardanoTransactions),
                ("party3", MithrilStakeDistribution),
            ]),
        )
        .unwrap();

        let store = BufferedSingleSignatureRepository::new(Arc::new(connection));

        let buffered_signatures_ctx = store.get_all().unwrap();
        assert_eq!(
            strip_buffered_sigs_date(&BufferedSingleSignatureRecord::fakes(&[
                ("party3", MithrilStakeDistribution),
                ("party2", CardanoTransactions),
                ("party1", CardanoTransactions),
            ])),
            strip_buffered_sigs_date(&buffered_signatures_ctx)
        );
    }

    #[tokio::test]
    async fn retrieve_signatures_by_discriminant() {
        let connection = main_db_connection().unwrap();
        insert_buffered_single_signatures(
            &connection,
            BufferedSingleSignatureRecord::fakes(&[
                ("party1", CardanoTransactions),
                ("party2", CardanoTransactions),
                ("party3", MithrilStakeDistribution),
            ]),
        )
        .unwrap();

        let store = BufferedSingleSignatureRepository::new(Arc::new(connection));

        let buffered_signatures_ctx = store
            .get_by_discriminant::<BufferedSingleSignatureRecord>(CardanoTransactions)
            .unwrap();
        assert_eq!(
            strip_buffered_sigs_date(&BufferedSingleSignatureRecord::fakes(&[
                ("party2", CardanoTransactions),
                ("party1", CardanoTransactions),
            ])),
            strip_buffered_sigs_date(&buffered_signatures_ctx)
        );

        let buffered_signatures_msd = store
            .get_by_discriminant::<BufferedSingleSignatureRecord>(MithrilStakeDistribution)
            .unwrap();
        assert_eq!(
            strip_buffered_sigs_date(&BufferedSingleSignatureRecord::fakes(&[(
                "party3",
                MithrilStakeDistribution
            ),])),
            strip_buffered_sigs_date(&buffered_signatures_msd)
        );
    }

    #[tokio::test]
    async fn store_signatures() {
        let connection = main_db_connection().unwrap();
        let store = BufferedSingleSignatureRepository::new(Arc::new(connection));

        // Multiple signatures of the same signed entity type
        {
            store
                .buffer_signature(
                    CardanoTransactions,
                    &SingleSignatures::new(
                        "party1",
                        fake_keys::single_signature()[0].try_into().unwrap(),
                        vec![1],
                    ),
                )
                .await
                .unwrap();
            store
                .buffer_signature(
                    CardanoTransactions,
                    &SingleSignatures::new(
                        "party2",
                        fake_keys::single_signature()[1].try_into().unwrap(),
                        vec![2],
                    ),
                )
                .await
                .unwrap();

            let buffered_signatures = store
                .get_buffered_signatures(CardanoTransactions)
                .await
                .unwrap();
            assert_eq!(
                vec![
                    SingleSignatures::new(
                        "party2",
                        fake_keys::single_signature()[1].try_into().unwrap(),
                        vec![2],
                    ),
                    SingleSignatures::new(
                        "party1",
                        fake_keys::single_signature()[0].try_into().unwrap(),
                        vec![1],
                    ),
                ],
                buffered_signatures
            );
        }
        // Another signed entity type to test that the store is able to differentiate between them
        {
            store
                .buffer_signature(
                    MithrilStakeDistribution,
                    &SingleSignatures::new(
                        "party3",
                        fake_keys::single_signature()[2].try_into().unwrap(),
                        vec![3],
                    ),
                )
                .await
                .unwrap();

            let buffered_signatures = store
                .get_buffered_signatures(MithrilStakeDistribution)
                .await
                .unwrap();
            assert_eq!(
                vec![SingleSignatures::new(
                    "party3",
                    fake_keys::single_signature()[2].try_into().unwrap(),
                    vec![3],
                )],
                buffered_signatures
            );
        }
    }

    #[tokio::test]
    async fn remove_buffered_signatures() {
        let connection = main_db_connection().unwrap();
        insert_buffered_single_signatures(
            &connection,
            BufferedSingleSignatureRecord::fakes(&[
                ("party1", MithrilStakeDistribution),
                ("party2", MithrilStakeDistribution),
                ("party3", MithrilStakeDistribution),
                ("party4", CardanoTransactions),
            ]),
        )
        .unwrap();

        let store = BufferedSingleSignatureRepository::new(Arc::new(connection));

        store
            .remove_buffered_signatures(
                MithrilStakeDistribution,
                vec![
                    BufferedSingleSignatureRecord::fake("party1", MithrilStakeDistribution)
                        .try_into()
                        .unwrap(),
                    BufferedSingleSignatureRecord::fake("party3", MithrilStakeDistribution)
                        .try_into()
                        .unwrap(),
                ],
            )
            .await
            .unwrap();

        let remaining_msd_sigs = store.get_all().unwrap();
        assert_eq!(
            strip_buffered_sigs_date(&BufferedSingleSignatureRecord::fakes(&[
                ("party4", CardanoTransactions),
                ("party2", MithrilStakeDistribution),
            ])),
            strip_buffered_sigs_date(&remaining_msd_sigs)
        );
    }
}
