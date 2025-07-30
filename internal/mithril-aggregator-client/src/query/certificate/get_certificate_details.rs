use anyhow::anyhow;
use reqwest::StatusCode;
use slog::debug;

use mithril_common::messages::CertificateMessage;

use crate::AggregatorClientResult;
use crate::error::AggregatorClientError;
use crate::query::{AggregatorQuery, QueryContext, QueryMethod};

/// Get the details of a certificate
pub struct CertificateDetailsQuery {
    hash: String,
}

impl CertificateDetailsQuery {
    /// Instantiate a query to get a certificate by hash
    pub fn by_hash(hash: String) -> Self {
        Self { hash }
    }
}

#[async_trait::async_trait]
impl AggregatorQuery for CertificateDetailsQuery {
    type Response = Option<CertificateMessage>;
    type Body = ();

    fn method() -> QueryMethod {
        QueryMethod::Get
    }

    fn route(&self) -> String {
        format!("certificate/{}", self.hash)
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorClientResult<Self::Response> {
        debug!(context.logger, "Retrieve certificate details"; "certificate_hash" => %self.hash);

        match context.response.status() {
            StatusCode::OK => match context.response.json::<CertificateMessage>().await {
                Ok(message) => Ok(Some(message)),
                Err(err) => Err(AggregatorClientError::JsonParseFailed(anyhow!(err))),
            },
            StatusCode::NOT_FOUND => Ok(None),
            _ => Err(context.unhandled_status_code().await),
        }
    }
}
