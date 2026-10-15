use std::time::Duration;

use reqwest::{RequestBuilder, Response};

use mithril_common::StdResult;

use crate::tools::kubo_rpc_client::{KuboRpcQuery, TIMEOUT_FOR_SLOW_QUERY_SECS};

/// Query to publish an IPFS object to IPNS
///
/// see: https://docs.ipfs.tech/reference/kubo/rpc/#api-v0-name-publish
#[derive(Debug)]
pub struct IpfsNamePublishQuery {
    ipfs_object_path: String,
    ipfs_key: String,
    lifetime: Duration,
    ttl: Duration,
}

impl IpfsNamePublishQuery {
    /// Default lifetime of the IPNS record in the DHT (using kubo default: 48 hours).
    const DEFAULT_LIFETIME: Duration = Duration::from_hours(48);
    /// Default cache TTL of the IPNS record (using kubo default: 5 minutes).
    const DEFAULT_TTL: Duration = Duration::from_mins(5);

    /// Create a query that will publish the given IPFS object to IPNS.
    pub fn publish_to_ipns<P: Into<String>, K: Into<String>>(
        ipfs_object_path: P,
        ipfs_key: K,
    ) -> Self {
        Self {
            ipfs_object_path: ipfs_object_path.into(),
            ipfs_key: ipfs_key.into(),
            lifetime: Self::DEFAULT_LIFETIME,
            ttl: Self::DEFAULT_TTL,
        }
    }

    /// Set the lifetime of the IPNS record in the DHT.
    ///
    /// Sent to kubo with a seconds precision, sub-second parts are truncated.
    pub fn with_lifetime(mut self, lifetime: Duration) -> Self {
        self.lifetime = lifetime;
        self
    }

    /// Set the cache TTL of the IPNS record.
    ///
    /// Sent to kubo with a seconds precision, sub-second parts are truncated.
    pub fn with_ttl(mut self, ttl: Duration) -> Self {
        self.ttl = ttl;
        self
    }

    /// Format a duration to a format accepted by kubo (Go `time.ParseDuration`).
    fn format_duration(duration: Duration) -> String {
        format!("{}s", duration.as_secs())
    }
}

#[async_trait::async_trait]
impl KuboRpcQuery for IpfsNamePublishQuery {
    type Response = ();

    fn route(&self) -> String {
        "api/v0/name/publish".to_string()
    }

    async fn configure_request(
        &self,
        request_builder: RequestBuilder,
    ) -> StdResult<RequestBuilder> {
        Ok(request_builder.query(&[
            ("arg", &self.ipfs_object_path),
            ("key", &self.ipfs_key),
            ("lifetime", &Self::format_duration(self.lifetime)),
            ("ttl", &Self::format_duration(self.ttl)),
        ]))
    }

    fn timeout(&self) -> Duration {
        Duration::from_secs(TIMEOUT_FOR_SLOW_QUERY_SECS)
    }

    async fn handle_success(&self, _response: Response) -> StdResult<Self::Response> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use httpmock::Method::POST;

    use crate::tools::kubo_rpc_client::test_tools::setup_server_and_client;

    use super::*;

    #[tokio::test]
    async fn succeeds_when_server_returns_200() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST)
                .path("/api/v0/name/publish")
                .query_param(
                    "arg",
                    "/ipfs/QmYi7wrRFKVCcTB56A6Pep2j31Q5mHfmmu21RzHXu25RVR",
                )
                .query_param("key", "my-key-name")
                .query_param("lifetime", "172800s")
                .query_param("ttl", "300s");
            then.status(200);
        });

        client
            .send(IpfsNamePublishQuery::publish_to_ipns(
                "/ipfs/QmYi7wrRFKVCcTB56A6Pep2j31Q5mHfmmu21RzHXu25RVR",
                "my-key-name",
            ))
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn send_configured_lifetime_and_ttl_in_seconds() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST)
                .path("/api/v0/name/publish")
                .query_param("lifetime", "120s")
                .query_param("ttl", "1s");
            then.status(200);
        });

        client
            .send(
                IpfsNamePublishQuery::publish_to_ipns(
                    "/ipfs/QmYi7wrRFKVCcTB56A6Pep2j31Q5mHfmmu21RzHXu25RVR",
                    "my-key-name",
                )
                .with_lifetime(Duration::from_mins(2))
                .with_ttl(Duration::from_millis(1500)),
            )
            .await
            .unwrap();
    }

    #[tokio::test]
    async fn return_error_if_request_fails_with_other_message() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST).path("/api/v0/name/publish");
            then.status(500).json_body(
                serde_json::json!({"Message":"no key by the given name was found","Code":0,"Type":"error"}),
            );
        });

        let err = client
            .send(IpfsNamePublishQuery::publish_to_ipns(
                "/ipfs/QmYi7wrRFKVCcTB56A6Pep2j31Q5mHfmmu21RzHXu25RVR",
                "my-key-name",
            ))
            .await
            .unwrap_err();
        assert!(err.to_string().contains("no key by the given name was found"));
    }
}
