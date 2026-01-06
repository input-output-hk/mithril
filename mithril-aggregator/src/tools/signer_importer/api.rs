use std::collections::HashMap;
use std::sync::Arc;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use slog::{Logger, info, warn};

use mithril_common::StdResult;
use mithril_common::entities::PartyId;
use mithril_common::logging::LoggerExtensions;

pub type PoolTicker = String;

/// Tool that can import a list of signers
pub struct SignersImporter {
    retriever: Arc<dyn SignersImporterRetriever>,
    persister: Arc<dyn SignersImporterPersister>,
    logger: Logger,
}

impl SignersImporter {
    /// [SignersImporter] factory
    pub fn new(
        retriever: Arc<dyn SignersImporterRetriever>,
        persister: Arc<dyn SignersImporterPersister>,
        logger: Logger,
    ) -> Self {
        Self {
            retriever,
            persister,
            logger: logger.new_with_component_name::<Self>(),
        }
    }

    /// Import and persist the signers
    pub async fn run(&self) -> StdResult<()> {
        info!(self.logger, "Starting import");
        let items = self
            .retriever
            .retrieve()
            .await
            .with_context(|| "Failed to retrieve signers from remote service")?;

        info!(
            self.logger, "Persisting retrieved data in the database";
            "number_of_signer_to_insert" => items.len()
        );
        self.persister
            .persist(items)
            .await
            .with_context(|| "Failed to persist retrieved data into the database")
    }

    /// Start a loop that call [run][Self::run] at the given time interval.
    pub async fn run_forever(&self, run_interval: Duration) {
        let mut interval = tokio::time::interval(run_interval);

        loop {
            interval.tick().await;
            if let Err(error) = self.run().await {
                warn!(self.logger, "Signer retriever failed"; "error" => ?error);
            }
            info!(
                self.logger,
                "Cycle finished, Sleeping for {} min",
                run_interval.as_secs() / 60
            );
        }
    }
}

/// Trait that define how a [SignersImporter] retrieve the signers to import.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignersImporterRetriever: Sync + Send {
    /// Retrieve the signers list.
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>>;
}

/// Trait that define how a [SignersImporter] persist the retrieved signers.
#[cfg_attr(test, mockall::automock)]
#[async_trait]
pub trait SignersImporterPersister: Sync + Send {
    /// Persist the given list of signers.
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()>;
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;
    use std::sync::Arc;
    use warp::Filter;

    use mithril_common::StdResult;
    use mithril_persistence::sqlite::SqliteConnection;
    use mithril_test_http_server::test_http_server;

    use crate::CExplorerSignerRetriever;
    use crate::database::repository::{SignerGetter, SignerStore};
    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;

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
    async fn cexplorer_importer_integration_test() {
        let connection = Arc::new(main_db_connection().unwrap());
        let store = Arc::new(SignerStore::new(connection.clone()));
        fill_signer_db(
            connection.clone(),
            &[
                TestSigner::with_ticker("pool4", "[Pool4 dont change]"),
                TestSigner::without_ticker("pool5"),
                TestSigner::with_ticker("pool6", "[Pool6 not returned by server]"),
                TestSigner::with_ticker("pool7", "[Pool7 ticker will be removed]"),
            ],
        )
        .await
        .unwrap();
        let server = test_http_server(warp::path("list").map(|| {
            r#"{
            "data": [
                {"pool_id": "pool1", "name": ""},
                {"pool_id": "pool2", "name": "[] "},
                {"pool_id": "pool3", "name": "[Pool3 added]"},
                {"pool_id": "pool4", "name": "[Pool4 dont change]"},
                {"pool_id": "pool5", "name": "[Pool5 add ticker]"},
                {"pool_id": "pool7", "name": "[] "}
            ]
        }"#
        }));

        let importer = SignersImporter::new(
            Arc::new(
                CExplorerSignerRetriever::new(
                    format!("{}/list", server.url()),
                    None,
                    TestLogger::stdout(),
                )
                .unwrap(),
            ),
            store.clone(),
            TestLogger::stdout(),
        );
        importer.run().await.expect("running importer should not fail");

        let result = get_all_signers(connection).await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::without_ticker("pool1"),
                TestSigner::without_ticker("pool2"),
                TestSigner::with_ticker("pool3", "[Pool3 added]",),
                TestSigner::with_ticker("pool4", "[Pool4 dont change]",),
                TestSigner::with_ticker("pool5", "[Pool5 add ticker]",),
                TestSigner::with_ticker("pool6", "[Pool6 not returned by server]",),
                TestSigner::without_ticker("pool7"),
            ])
        );
    }
}
