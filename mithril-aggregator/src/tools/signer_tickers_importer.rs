use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{entities::PartyId, StdResult};
use reqwest::Url;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::Not;
use std::sync::Arc;

use crate::database::provider::SignerStore;
use crate::SignerRecorder;

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
        let items = self
            .retriever
            .retrieve()
            .await
            .with_context(|| "Failed to retrieve signers from remote service")?;
        self.persister
            .persist(items)
            .await
            .with_context(|| "Failed to persist retrieved data into the database")
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersRetriever {
    /// Retrieve the signers list.
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>>;
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersPersister {
    /// Persist the given list of signers.
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()>;
}

#[async_trait]
impl SignerTickersPersister for SignerStore {
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()> {
        for (party_id, ticker) in signers {
            self.record_signer_pool_ticker(party_id, ticker).await?;
        }

        Ok(())
    }
}

/// A [SignerTickersRetriever] fetching signers data from CExplorer.
pub struct CExplorerSignerTickerRetriever {
    /// Url from which a SPO list using the CExplorer format will be fetch.
    source_url: Url,
}

impl CExplorerSignerTickerRetriever {
    /// Create a new [CExplorerSignerTickerRetriever] that will fetch data from the given url.
    pub(crate) fn new(source_url: Url) -> Self {
        Self { source_url }
    }
}

#[async_trait]
impl SignerTickersRetriever for CExplorerSignerTickerRetriever {
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>> {
        let spo_list = reqwest::get(self.source_url.to_owned())
            .await
            .with_context(|| "Retrieving of CExplorer SPO list failed")?
            .json::<SPOList>()
            .await
            .with_context(|| "Failed to deserialize retrieved SPO list from CExplorer")?;

        Ok(spo_list
            .data
            .into_iter()
            .map(|item| item.extract())
            .collect())
    }
}

/// *Internal type* that map a CExplorer SPO list.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SPOList {
    data: Vec<SPOItem>,
}

/// *Internal type* that map a CExplorer SPO item inside its data list.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SPOItem {
    pool_id: String,
    name: String,
}

impl SPOItem {
    const EMPTY_NAME: &'static str = "[] ";

    fn is_name_empty(&self) -> bool {
        self.name.is_empty() || self.name == Self::EMPTY_NAME
    }

    /// Consume this item to convert it to a result ready to be yield by a
    /// [SignerTickersRetriever::retrieve] implementation.
    fn extract(self) -> (PartyId, Option<PoolTicker>) {
        let is_name_empty = self.is_name_empty();
        let (pool_id, name) = (self.pool_id, self.name);

        (pool_id, is_name_empty.not().then_some(name))
    }
}

#[cfg(test)]
mod tests {
    use mithril_common::test_utils::test_http_server::test_http_server;
    use mithril_common::StdResult;
    use sqlite::Connection;
    use std::collections::BTreeSet;
    use std::sync::Arc;
    use tokio::sync::Mutex;
    use warp::Filter;

    use crate::database::provider::{
        apply_all_migrations_to_db, disable_foreign_key_support, SignerStore,
    };
    use crate::http_server::routes::reply;
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

    async fn get_all_signers(
        connection: Arc<Mutex<Connection>>,
    ) -> StdResult<BTreeSet<TestSigner>> {
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

    #[test]
    fn item_with_empty_name_yield_empty_pool_ticker() {
        for name in ["", SPOItem::EMPTY_NAME] {
            let item = SPOItem {
                pool_id: "whatever".to_string(),
                name: name.to_string(),
            };
            assert!(item.is_name_empty());
            assert_eq!(("whatever".to_string(), None), item.extract());
        }
    }

    #[tokio::test]
    async fn retriever_should_return_deduplicated_data_and_handle_empty_name() {
        let server = test_http_server(warp::path("list").map(|| {
            r#"{
            "data": [
                {"pool_id": "pool1", "name": ""},
                {"pool_id": "pool2", "name": "[] "},
                {"pool_id": "pool3", "name": "whatever"},
                {"pool_id": "pool3", "name": "whatever2"}
            ]
        }"#
        }));

        let retriever = CExplorerSignerTickerRetriever::new(
            Url::parse(&format!("{}/list", server.url())).unwrap(),
        );
        let result = retriever
            .retrieve()
            .await
            .expect("Retriever should not fail");

        assert_eq!(
            result,
            HashMap::from([
                ("pool1".to_string(), None),
                ("pool2".to_string(), None),
                ("pool3".to_string(), Some("whatever2".to_string())),
            ])
        );
    }

    #[tokio::test]
    async fn retriever_doesnt_crash_if_remote_url_is_not_responding() {
        let server = test_http_server(
            warp::path("list").map(|| reply::internal_server_error("whatever".to_string())),
        );

        let retriever = CExplorerSignerTickerRetriever::new(
            Url::parse(&format!("{}/list", server.url())).unwrap(),
        );
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }

    #[tokio::test]
    async fn retriever_yield_error_when_json_is_malformed() {
        let server = test_http_server(warp::path("list").map(|| r#"{ "data": [ {"pool_" ] }"#));

        let retriever = CExplorerSignerTickerRetriever::new(
            Url::parse(&format!("{}/list", server.url())).unwrap(),
        );
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }

    #[tokio::test]
    async fn persist_list_of_two_signers_one_with_ticker_the_other_without() {
        let connection = Arc::new(Mutex::new(connection_without_foreign_key_support()));
        let mut retriever = MockSignerTickersRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Pool name test]".to_string())),
                ("pool2".to_string(), None),
            ]))
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
        let connection = Arc::new(Mutex::new(connection_without_foreign_key_support()));
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
        let mut retriever = MockSignerTickersRetriever::new();
        retriever.expect_retrieve().returning(|| {
            Ok(HashMap::from([
                ("pool1".to_string(), Some("[Updated Pool name]".to_string())),
                ("pool2".to_string(), Some("[Added Pool name]".to_string())),
                ("pool3".to_string(), Some("[Not updated]".to_string())),
                ("pool4".to_string(), None),
                ("pool5".to_string(), Some("[New Pool]".to_string())),
            ]))
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
