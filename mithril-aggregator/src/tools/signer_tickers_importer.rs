use crate::database::provider::SignerStore;
use anyhow::Context;
use async_trait::async_trait;
use mithril_common::StdResult;
use std::collections::HashMap;
use std::sync::Arc;

use crate::SignerRecorder;
use mithril_common::entities::PartyId;
#[cfg(test)]
use mockall::automock;

pub type PoolTicker = String;

/// Tool that can import a list of Signer including their Pool Tickers
pub struct SignerTickersImporter {
    retriever: Arc<dyn SignerTickersRetriever>,
    persister: Arc<dyn SignerTickersPersister>,
}

impl SignerTickersImporter {
    /// [SignerTickersImporter] factory
    pub fn new(
        retriever: Arc<dyn SignerTickersRetriever>,
        persister: Arc<dyn SignerTickersPersister>,
    ) -> Self {
        Self {
            retriever,
            persister,
        }
    }

    /// Import and persist the signers
    pub async fn run(&self) -> StdResult<()> {
        let items = self.retriever.retrieve().await.with_context(|| "todo")?;
        self.persister.persist(items).await.with_context(|| "todo")
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersRetriever {
    /// Retrieve the signers list.
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, PoolTicker>>;
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersPersister {
    /// Persist the given list of signers.
    async fn persist(&self, signers: HashMap<PartyId, PoolTicker>) -> StdResult<()>;
}

#[async_trait]
impl SignerTickersPersister for SignerStore {
    async fn persist(&self, signers: HashMap<PartyId, PoolTicker>) -> StdResult<()> {
        for (party_id, ticker) in signers {
            self.record_signer_pool_ticker(party_id, Some(ticker))
                .await?;
        }

        Ok(())
    }
}

#[derive(Debug, Copy, Clone)]
struct SPOItem {
    pool_id: &'static str,
    name: &'static str,
}

#[derive(Debug, Clone)]
struct SPOList {
    data: Vec<SPOItem>,
}

#[cfg(test)]
mod tests {
    use mithril_common::StdResult;
    use sqlite::Connection;
    use std::sync::Arc;
    use tokio::sync::Mutex;

    use crate::database::provider::{
        apply_all_migrations_to_db, disable_foreign_key_support, SignerStore,
    };
    use crate::SignerRecorder;

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

    fn connection_without_foreign_key_support() -> Connection {
        let connection = Connection::open(":memory:").unwrap();
        apply_all_migrations_to_db(&connection).unwrap();
        disable_foreign_key_support(&connection).unwrap();

        connection
    }

    async fn fill_signer_db(
        connection: Arc<Mutex<Connection>>,
        test_signers: &[TestSigner],
    ) -> StdResult<()> {
        let store = SignerStore::new(connection);

        for signer in test_signers {
            store
                .record_signer_pool_ticker(signer.pool_id.clone(), signer.ticker.clone())
                .await?;
        }

        Ok(())
    }

    async fn get_all_signers(connection: Arc<Mutex<Connection>>) -> StdResult<Vec<TestSigner>> {
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
    async fn imported_list_of_one_signer_with_ticker() {
        let connection = Arc::new(Mutex::new(connection_without_foreign_key_support()));
        let expected = vec![TestSigner::with_ticker(
            "pool7809lhh2dhf48ezp7dy8j7450z026hch1w7c2j0px43jmu9l3wc",
            "[Pool name test]",
        )];
        let mut retriever = MockSignerTickersRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([(
                "pool7809lhh2dhf48ezp7dy8j7450z026hch1w7c2j0px43jmu9l3wc".to_string(),
                "[Pool name test]".to_string(),
            )]))
        });

        let importer = SignerTickersImporter::new(
            Arc::new(retriever),
            Arc::new(SignerStore::new(connection.clone())),
        );
        importer
            .run()
            .await
            .expect("running importer should not fail");

        let result = get_all_signers(connection).await.unwrap();
        assert_eq!(result, expected);
    }
}
