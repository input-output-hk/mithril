use async_trait::async_trait;
use std::sync::Arc;

use mithril_common::entities::{SignedEntityTypeDiscriminants, SingleSignatures};
use mithril_common::{StdError, StdResult};
use mithril_persistence::sqlite::{ConnectionExtensions, SqliteConnection};

use crate::database::query::GetBufferedSingleSignatureQuery;
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
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<T>>
    where
        T: TryFrom<BufferedSingleSignatureRecord>,
        StdError: From<T::Error>,
    {
        let records: Vec<BufferedSingleSignatureRecord> =
            self.connection
                .fetch_collect(GetBufferedSingleSignatureQuery::by_discriminant(
                    signed_entity_type_discriminants,
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
        _signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        _signature: &SingleSignatures,
    ) -> StdResult<()> {
        todo!()
    }

    async fn get_buffered_signatures(
        &self,
        signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
    ) -> StdResult<Vec<SingleSignatures>> {
        self.get_by_discriminant(signed_entity_type_discriminants)
    }

    async fn remove_buffered_signatures(
        &self,
        _signed_entity_type_discriminants: SignedEntityTypeDiscriminants,
        _single_signatures: Vec<SingleSignatures>,
    ) -> StdResult<()> {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::entities::{
        Epoch, LotteryIndex,
        SignedEntityTypeDiscriminants::{CardanoTransactions, MithrilStakeDistribution},
    };
    use mithril_common::test_utils::fake_data;

    use crate::database::record::BufferedSingleSignatureRecord;
    use crate::database::test_helper::{insert_buffered_single_signatures, main_db_connection};

    use super::*;

    fn strip_date(records: &[BufferedSingleSignatureRecord]) -> Vec<BufferedSingleSignatureRecord> {
        records
            .iter()
            .map(BufferedSingleSignatureRecord::with_stripped_date)
            .collect::<Vec<_>>()
    }

    fn fake_record(
        discriminants: SignedEntityTypeDiscriminants,
        signature_won_indexes: Vec<LotteryIndex>,
    ) -> BufferedSingleSignatureRecord {
        BufferedSingleSignatureRecord::try_from_single_signatures(
            &fake_data::single_signatures(signature_won_indexes),
            discriminants,
            Epoch(12),
        )
        .unwrap()
    }

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
            strip_date(&BufferedSingleSignatureRecord::fakes(&[
                ("party3", MithrilStakeDistribution),
                ("party2", CardanoTransactions),
                ("party1", CardanoTransactions),
            ])),
            strip_date(&buffered_signatures_ctx)
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
            strip_date(&BufferedSingleSignatureRecord::fakes(&[
                ("party2", CardanoTransactions),
                ("party1", CardanoTransactions),
            ])),
            strip_date(&buffered_signatures_ctx)
        );

        let buffered_signatures_msd = store
            .get_by_discriminant::<BufferedSingleSignatureRecord>(MithrilStakeDistribution)
            .unwrap();
        assert_eq!(
            strip_date(&BufferedSingleSignatureRecord::fakes(&[(
                "party3",
                MithrilStakeDistribution
            ),])),
            strip_date(&buffered_signatures_msd)
        );
    }
}
