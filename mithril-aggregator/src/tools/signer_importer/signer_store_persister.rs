use async_trait::async_trait;
use std::collections::HashMap;

use mithril_common::StdResult;
use mithril_common::entities::PartyId;

use crate::database::repository::SignerStore;

use super::{PoolTicker, SignersImporterPersister};

#[async_trait]
impl SignersImporterPersister for SignerStore {
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()> {
        self.import_many_signers(signers).await?;

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::sync::Arc;

    use mithril_common::StdResult;
    use mithril_persistence::sqlite::SqliteConnection;

    use crate::SignersImporter;
    use crate::database::repository::{SignerGetter, SignerStore};
    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;
    use crate::tools::signer_importer::MockSignersImporterRetriever;

    use super::*;

    #[derive(Debug, Clone, Ord, PartialOrd, Eq, PartialEq)]
    struct TestSigner {
        pool_id: String,
        ticker: Option<String>,
    }

    impl TestSigner {
        fn with_ticker(pool_id: &str, ticker: &str) -> Self {
            Self {
                pool_id: pool_id.to_string(),
                ticker: Some(ticker.to_string()),
            }
        }

        fn without_ticker(pool_id: &str) -> Self {
            Self {
                pool_id: pool_id.to_string(),
                ticker: None,
            }
        }
    }

    async fn fill_signer_db(
        connection: Arc<SqliteConnection>,
        test_signers: &[TestSigner],
    ) -> StdResult<()> {
        let store = SignerStore::new(connection);

        for signer in test_signers {
            store
                .import_signer(signer.pool_id.clone(), signer.ticker.clone())
                .await?;
        }

        Ok(())
    }

    async fn get_all_signers(connection: Arc<SqliteConnection>) -> StdResult<BTreeSet<TestSigner>> {
        let store = SignerStore::new(connection);

        let signers = store
            .get_all()
            .await?
            .into_iter()
            .map(|s| TestSigner {
                pool_id: s.signer_id,
                ticker: s.pool_ticker,
            })
            .collect();
        Ok(signers)
    }

    #[tokio::test]
    async fn persist_list_of_two_signers_one_with_ticker_the_other_without() {
        let connection = Arc::new(main_db_connection().unwrap());
        let mut retriever = MockSignersImporterRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Pool name test]".to_string())),
                ("pool2".to_string(), None),
            ]))
        });

        let importer = SignersImporter::new(
            Arc::new(retriever),
            Arc::new(SignerStore::new(connection.clone())),
            TestLogger::stdout(),
        );
        importer.run().await.expect("running importer should not fail");

        let result = get_all_signers(connection).await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::with_ticker("pool1", "[Pool name test]",),
                TestSigner::without_ticker("pool2"),
            ])
        );
    }

    #[tokio::test]
    async fn persist_update_existing_data() {
        let connection = Arc::new(main_db_connection().unwrap());
        fill_signer_db(
            connection.clone(),
            &[
                TestSigner::with_ticker("pool1", "[Pool name test]"),
                TestSigner::without_ticker("pool2"),
                TestSigner::with_ticker("pool3", "[Not updated]"),
                TestSigner::with_ticker("pool4", "[Ticker will be removed]"),
            ],
        )
        .await
        .unwrap();
        let mut retriever = MockSignersImporterRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Updated Pool name]".to_string())),
                ("pool2".to_string(), Some("[Added Pool name]".to_string())),
                ("pool3".to_string(), Some("[Not updated]".to_string())),
                ("pool4".to_string(), None),
                ("pool5".to_string(), Some("[New Pool]".to_string())),
            ]))
        });

        let importer = SignersImporter::new(
            Arc::new(retriever),
            Arc::new(SignerStore::new(connection.clone())),
            TestLogger::stdout(),
        );
        importer.run().await.expect("running importer should not fail");

        let result = get_all_signers(connection).await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::with_ticker("pool1", "[Updated Pool name]"),
                TestSigner::with_ticker("pool2", "[Added Pool name]"),
                TestSigner::with_ticker("pool3", "[Not updated]"),
                TestSigner::without_ticker("pool4"),
                TestSigner::with_ticker("pool5", "[New Pool]"),
            ])
        );
    }
}
