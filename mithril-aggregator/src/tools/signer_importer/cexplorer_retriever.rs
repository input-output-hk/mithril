use std::collections::HashMap;
use std::ops::Not;
use std::time::Duration;

use anyhow::Context;
use async_trait::async_trait;
use reqwest::{IntoUrl, Url};
use serde::{Deserialize, Serialize};
use slog::{Logger, info};

use mithril_common::StdResult;
use mithril_common::entities::PartyId;

use super::{PoolTicker, SignersImporterRetriever};

/// A [SignersImporterRetriever] fetching signers data from CExplorer.
pub struct CExplorerSignerRetriever {
    /// Url from which a SPO list using the CExplorer format will be fetch.
    source_url: Url,
    client: reqwest::Client,
    logger: Logger,
}

impl CExplorerSignerRetriever {
    /// Create a new [CExplorerSignerRetriever] that will fetch data from the given url.
    pub(crate) fn new<T: IntoUrl>(
        source_url: T,
        timeout: Option<Duration>,
        logger: Logger,
    ) -> StdResult<Self> {
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

        Ok(Self {
            source_url,
            client,
            logger,
        })
    }
}

#[async_trait]
impl SignersImporterRetriever for CExplorerSignerRetriever {
    async fn retrieve(&self) -> StdResult<HashMap<PartyId, Option<PoolTicker>>> {
        info!(
            self.logger, "Retrieving data from source";
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

        Ok(spo_list.data.into_iter().map(|item| item.extract()).collect())
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
    /// [SignersImporterRetriever::retrieve] implementation.
    fn extract(self) -> (PartyId, Option<PoolTicker>) {
        let is_name_empty = self.is_name_empty();
        let (pool_id, name) = (self.pool_id, self.name);

        (pool_id, is_name_empty.not().then_some(name))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;
    use std::convert::Infallible;

    use mithril_test_http_server::test_http_server;
    use warp::Filter;

    use crate::http_server::routes::reply;
    use crate::test::TestLogger;

    use super::*;

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

        let retriever = CExplorerSignerRetriever::new(
            format!("{}/list", server.url()),
            None,
            TestLogger::stdout(),
        )
        .unwrap();
        let result = retriever.retrieve().await.expect("Retriever should not fail");

        assert_eq!(
            result.into_iter().collect::<BTreeMap<_, _>>(),
            BTreeMap::from([
                ("pool1".to_string(), None),
                ("pool2".to_string(), None),
                ("pool3".to_string(), Some("whatever2".to_string())),
            ])
        );
    }

    #[tokio::test]
    async fn retriever_handle_http_data_fetching_error() {
        let server =
            test_http_server(warp::path("list").map(|| reply::internal_server_error("whatever")));

        let retriever = CExplorerSignerRetriever::new(
            format!("{}/list", server.url()),
            None,
            TestLogger::stdout(),
        )
        .unwrap();
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }

    #[tokio::test]
    async fn retriever_yield_error_when_json_is_malformed() {
        let server = test_http_server(warp::path("list").map(|| r#"{ "data": [ {"pool_" ] }"#));

        let retriever = CExplorerSignerRetriever::new(
            format!("{}/list", server.url()),
            None,
            TestLogger::stdout(),
        )
        .unwrap();
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

        let retriever = CExplorerSignerRetriever::new(
            format!("{}/list", server.url()),
            Some(Duration::from_millis(10)),
            TestLogger::stdout(),
        )
        .unwrap();
        retriever
            .retrieve()
            .await
            .expect_err("An error should have been raised");
    }
}
