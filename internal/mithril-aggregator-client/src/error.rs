use anyhow::anyhow;
use reqwest::{Response, StatusCode, header};
use thiserror::Error;

use mithril_common::StdError;
use mithril_common::entities::{ClientError, ServerError};

use crate::JSON_CONTENT_TYPE;

/// Error structure for the Aggregator Client.
#[derive(Error, Debug)]
pub enum AggregatorClientError {
    /// The aggregator host has returned a technical error.
    #[error("remote server technical error")]
    RemoteServerTechnical(#[source] StdError),

    /// The aggregator host responded it cannot fulfill our request.
    #[error("remote server logical error")]
    RemoteServerLogical(#[source] StdError),

    /// Could not reach aggregator.
    #[error("Remote server unreachable")]
    RemoteServerUnreachable(#[source] StdError),

    /// Unhandled status code
    #[error("Unhandled status code: {0}, response text: {1}")]
    UnhandledStatusCode(StatusCode, String),

    /// Could not parse response.
    #[error("Json parsing failed")]
    JsonParseFailed(#[source] StdError),

    /// Failed to join the query endpoint to the aggregator url
    #[error("Invalid endpoint")]
    InvalidEndpoint(#[source] StdError),
}

impl AggregatorClientError {
    /// Create an `AggregatorClientError` from a response.
    ///
    /// This method is meant to be used after handling domain-specific cases leaving only
    /// 4xx or 5xx status codes.
    /// Otherwise, it will return an `UnhandledStatusCode` error.
    pub async fn from_response(response: Response) -> Self {
        let error_code = response.status();

        if error_code.is_client_error() {
            let root_cause = Self::get_root_cause(response).await;
            Self::RemoteServerLogical(anyhow!(root_cause))
        } else if error_code.is_server_error() {
            let root_cause = Self::get_root_cause(response).await;
            Self::RemoteServerTechnical(anyhow!(root_cause))
        } else {
            let response_text = response.text().await.unwrap_or_default();
            Self::UnhandledStatusCode(error_code, response_text)
        }
    }

    async fn get_root_cause(response: Response) -> String {
        let error_code = response.status();
        let canonical_reason = error_code.canonical_reason().unwrap_or_default().to_lowercase();
        let is_json = response
            .headers()
            .get(header::CONTENT_TYPE)
            .is_some_and(|ct| JSON_CONTENT_TYPE == ct);

        if is_json {
            let json_value: serde_json::Value = response.json().await.unwrap_or_default();

            if let Ok(client_error) = serde_json::from_value::<ClientError>(json_value.clone()) {
                format!(
                    "{}: {}: {}",
                    canonical_reason, client_error.label, client_error.message
                )
            } else if let Ok(server_error) =
                serde_json::from_value::<ServerError>(json_value.clone())
            {
                format!("{}: {}", canonical_reason, server_error.message)
            } else if json_value.is_null() {
                canonical_reason.to_string()
            } else {
                format!("{canonical_reason}: {json_value}")
            }
        } else {
            let response_text = response.text().await.unwrap_or_default();
            format!("{canonical_reason}: {response_text}")
        }
    }
}
