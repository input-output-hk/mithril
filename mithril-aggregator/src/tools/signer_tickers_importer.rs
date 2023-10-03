use anyhow::Context;
use async_trait::async_trait;
use mithril_common::{entities::PartyId, StdResult};
use reqwest::{IntoUrl, Url};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::ops::Not;
use std::sync::Arc;
use std::time::Duration;

use crate::database::provider::SignerStore;

#[cfg(test)]
use mockall::automock;
use slog_scope::{info, warn};

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
        info!("🔧 Signer Ticker Importer: starting");
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

    /// Start a loop that call [run][Self::run] at the given time interval.
    pub async fn run_forever(&self, run_interval: Duration) {
        let mut interval = tokio::time::interval(run_interval);

        loop {
            interval.tick().await;
            if let Err(error) = self.run().await {
                warn!("Signer ticker retriever failed: Error: «{:?}».", error);
            }
            info!(
                "🔧 Signer Ticker Importer: Cycle finished, Sleeping for {} min",
                run_interval.as_secs() / 60
            );
        }
    }
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersRetriever: Sync + Send {
    /// Retrieve the signers list.
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>>;
}

#[cfg_attr(test, automock)]
#[async_trait]
pub trait SignerTickersPersister: Sync + Send {
    /// Persist the given list of signers.
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()>;
}

#[async_trait]
impl SignerTickersPersister for SignerStore {
    async fn persist(&self, signers: HashMap<PartyId, Option<PoolTicker>>) -> StdResult<()> {
        info!(
            "🔧 Signer Ticker Importer: persisting retrieved data in the database";
            "number_of_signer_to_insert" => signers.len()
        );
        self.import_many_signers(signers).await?;

        Ok(())
    }
}

/// A [SignerTickersRetriever] fetching signers data from CExplorer.
pub struct CExplorerSignerTickerRetriever {
    /// Url from which a SPO list using the CExplorer format will be fetch.
    source_url: Url,
    client: reqwest::Client,
}

impl CExplorerSignerTickerRetriever {
    /// Create a new [CExplorerSignerTickerRetriever] that will fetch data from the given url.
    pub(crate) fn new<T: IntoUrl>(source_url: T, timeout: Option<Duration>) -> StdResult<Self> {
        let source_url = source_url
            .into_url()
            .with_context(|| "Given `source_url` is not a valid Url")?;
        let client_builder = reqwest::Client::builder();
        let client = match timeout {
            None => client_builder,
            Some(timeout) => client_builder.timeout(timeout),
        }
        .build()
        .with_context(|| "Http Client build failed")?;

        Ok(Self { source_url, client })
    }
}

#[async_trait]
impl SignerTickersRetriever for CExplorerSignerTickerRetriever {
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>> {
        info!(
            "🔧 Signer Ticker Importer: retrieving data from source";
            "source_url" => &self.source_url.as_str()
        );
        let response = self
            .client
            .get(self.source_url.to_owned())
            .send()
            .await
            .with_context(|| "Retrieving of CExplorer SPO list failed")?;

        let spo_list = response
            .error_for_status()
            .with_context(|| "Data fetching failed")?
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

/// *Internal type* Map a CExplorer SPO list.
#[derive(Debug, Clone, Serialize, Deserialize)]
struct SPOList {
    data: Vec<SPOItem>,
}

/// *Internal type* Map a CExplorer SPO item inside its data list.
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
    use std::convert::Infallible;
    use std::sync::Arc;
    use tokio::sync::Mutex;
    use warp::Filter;

    use crate::database::provider::{
        apply_all_migrations_to_db, disable_foreign_key_support, SignerGetter, SignerStore,
    };
    use crate::http_server::routes::reply;

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
                .import_signer(signer.pool_id.clone(), signer.ticker.clone())
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

        let retriever =
            CExplorerSignerTickerRetriever::new(format!("{}/list", server.url()), None).unwrap();
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
    async fn retriever_handle_http_data_fetching_error() {
        let server = test_http_server(
            warp::path("list").map(|| reply::internal_server_error("whatever".to_string())),
        );

        let retriever =
            CExplorerSignerTickerRetriever::new(format!("{}/list", server.url()), None).unwrap();
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }

    #[tokio::test]
    async fn retriever_yield_error_when_json_is_malformed() {
        let server = test_http_server(warp::path("list").map(|| r#"{ "data": [ {"pool_" ] }"#));

        let retriever =
            CExplorerSignerTickerRetriever::new(format!("{}/list", server.url()), None).unwrap();
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }

    #[tokio::test]
    async fn retriever_can_timeout() {
        let server = test_http_server(warp::path("list").and_then(|| async {
            tokio::time::sleep(Duration::from_millis(70)).await;
            Ok::<&str, Infallible>(r#"{"data":[]}"#)
        }));

        let retriever = CExplorerSignerTickerRetriever::new(
            format!("{}/list", server.url()),
            Some(Duration::from_millis(10)),
        )
        .unwrap();
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

    #[tokio::test]
    async fn importer_integration_test() {
        let connection = Arc::new(Mutex::new(connection_without_foreign_key_support()));
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

        let importer = SignerTickersImporter::new(
            Arc::new(
                CExplorerSignerTickerRetriever::new(format!("{}/list", server.url()), None)
                    .unwrap(),
            ),
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
