use anyhow::Context;
use reqwest::Response;
use serde::Deserialize;

use mithril_common::StdResult;

use crate::tools::kubo_rpc_client::KuboRpcQuery;

/// Query to list the keys stored in the IPFS node keystore (used to sign IPNS records).
///
/// see: https://docs.ipfs.tech/reference/kubo/rpc/#api-v0-key-ls
#[derive(Debug, Default)]
pub struct IpfsKeyLsQuery;

#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "PascalCase")]
struct IpfsKeyLsResponse {
    keys: Option<Vec<IpfsKey>>,
}

/// A key stored in the IPFS node keystore
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "PascalCase")]
pub struct IpfsKey {
    /// Name of the key, local to the IPFS node
    pub name: String,
    /// Id of the key, usable to resolve the IPNS records signed with this key
    pub id: String,
}

impl IpfsKeyLsQuery {
    /// Create a query that will list the keys of the IPFS node keystore.
    pub fn new() -> Self {
        Self
    }
}

#[async_trait::async_trait]
impl KuboRpcQuery for IpfsKeyLsQuery {
    type Response = Vec<IpfsKey>;

    fn route(&self) -> String {
        "api/v0/key/ls".to_string()
    }

    async fn handle_success(&self, response: Response) -> StdResult<Self::Response> {
        let response: IpfsKeyLsResponse = response
            .json()
            .await
            .with_context(|| "Failed to deserialize IPFS key ls response")?;

        Ok(response.keys.unwrap_or_default())
    }
}

#[cfg(test)]
mod tests {
    use httpmock::Method::POST;

    use crate::tools::kubo_rpc_client::test_tools::setup_server_and_client;

    use super::*;

    #[tokio::test]
    async fn return_keys_list_if_request_succeeds() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST).path("/api/v0/key/ls");
            then.status(200).json_body(serde_json::json!({
                "Keys": [
                    { "Name": "self", "Id": "k51qzi5uqu5djd98ozajs6h4ljp0p53lqf4fn4p3904t20yqjbc2nksxotpz66" },
                    { "Name": "mithril-snapshot", "Id": "k51qzi5uqu5di91xjrh1a4hs0gd7l22tdoyh1odjjfu40vllpi1p9i2jert6sp" }
                ]
            }));
        });

        let response = client.send(IpfsKeyLsQuery::new()).await.unwrap();

        assert_eq!(
            vec![
                IpfsKey {
                    name: "self".to_string(),
                    id: "k51qzi5uqu5djd98ozajs6h4ljp0p53lqf4fn4p3904t20yqjbc2nksxotpz66"
                        .to_string(),
                },
                IpfsKey {
                    name: "mithril-snapshot".to_string(),
                    id: "k51qzi5uqu5di91xjrh1a4hs0gd7l22tdoyh1odjjfu40vllpi1p9i2jert6sp"
                        .to_string(),
                },
            ],
            response
        );
    }

    #[tokio::test]
    async fn return_empty_list_when_keys_are_null() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST).path("/api/v0/key/ls");
            then.status(200).json_body(serde_json::json!({ "Keys": null }));
        });

        let response = client.send(IpfsKeyLsQuery::new()).await.unwrap();

        assert_eq!(Vec::<IpfsKey>::new(), response);
    }

    #[tokio::test]
    async fn return_error_if_response_body_is_malformed() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST).path("/api/v0/key/ls");
            then.status(200).json_body(serde_json::json!({ "Keys": "invalid" }));
        });

        let err = client.send(IpfsKeyLsQuery::new()).await.unwrap_err();

        assert!(
            err.to_string().contains("Failed to deserialize IPFS key ls response"),
            "unexpected error: {err:?}"
        );
    }

    #[tokio::test]
    async fn return_error_if_request_fails() {
        let (server, client) = setup_server_and_client();
        server.mock(|when, then| {
            when.method(POST).path("/api/v0/key/ls");
            then.status(500).json_body(
                serde_json::json!({"Message":"keystore unavailable","Code":0,"Type":"error"}),
            );
        });

        let err = client.send(IpfsKeyLsQuery::new()).await.unwrap_err();

        assert!(err.to_string().contains("keystore unavailable"));
    }
}
