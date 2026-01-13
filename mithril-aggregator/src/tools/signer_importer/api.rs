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

    use mithril_common::test::mock_extensions::MockBuilder;

    use crate::database::repository::SignerStore;
    use crate::database::test_helper::main_db_connection;
    use crate::test::TestLogger;
    use crate::tools::signer_importer::MockSignersImporterRetriever;

    use super::{super::test_tools::*, *};

    #[tokio::test]
    async fn importer_integration_test() {
        let connection = Arc::new(main_db_connection().unwrap());
        let store = Arc::new(SignerStore::new(connection.clone()));
        store
            .fill_with_test_signers(&[
                TestSigner::with_ticker("pool3", "[Pool3 dont change]"),
                TestSigner::without_ticker("pool4"),
                TestSigner::with_ticker("pool5", "[Pool5 not returned by server]"),
                TestSigner::with_ticker("pool6", "[Pool6 ticker will be removed]"),
            ])
            .await
            .unwrap();
        let retriever = MockBuilder::<MockSignersImporterRetriever>::configure(|mock| {
            mock.expect_retrieve().returning(|| {
                Ok(HashMap::from([
                    ("pool1".to_string(), None),
                    ("pool2".to_string(), Some("[Pool2 added]".to_string())),
                    ("pool3".to_string(), Some("[Pool3 dont change]".to_string())),
                    ("pool4".to_string(), Some("[Pool4 add ticker]".to_string())),
                    ("pool6".to_string(), None),
                ]))
            });
        });

        let importer = SignersImporter::new(retriever, store.clone(), TestLogger::stdout());
        importer.run().await.expect("running importer should not fail");

        let result = store.get_all_test_signers().await.unwrap();
        assert_eq!(
            result,
            BTreeSet::from([
                TestSigner::without_ticker("pool1"),
                TestSigner::with_ticker("pool2", "[Pool2 added]",),
                TestSigner::with_ticker("pool3", "[Pool3 dont change]",),
                TestSigner::with_ticker("pool4", "[Pool4 add ticker]",),
                TestSigner::with_ticker("pool5", "[Pool5 not returned by server]",),
                TestSigner::without_ticker("pool6"),
            ])
        );
    }
}
