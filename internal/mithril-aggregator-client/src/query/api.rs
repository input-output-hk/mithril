use anyhow::anyhow;
use reqwest::Response;
use serde::de::DeserializeOwned;
use slog::{Logger, Record, Serializer};
use std::collections::BTreeSet;
use std::fmt::{Display, Formatter};

use crate::AggregatorHttpClientResult;
use crate::error::AggregatorHttpClientError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryMethod {
    Get,
    Post,
}

impl Display for QueryMethod {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            QueryMethod::Get => write!(f, "GET"),
            QueryMethod::Post => write!(f, "POST"),
        }
    }
}

#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait AggregatorQuery {
    type Response: DeserializeOwned;
    type Body: serde::Serialize + Sized;

    fn method() -> QueryMethod;

    fn route(&self) -> String;

    fn body(&self) -> Option<Self::Body> {
        None
    }

    fn entry_log_additional_fields(&self) -> QueryLogFields {
        QueryLogFields::default()
    }

    async fn handle_response(
        &self,
        context: QueryContext,
    ) -> AggregatorHttpClientResult<Self::Response>;
}

pub struct QueryContext {
    pub(crate) response: Response,
    pub(crate) logger: Logger,
}

impl QueryContext {
    pub async fn unhandled_status_code(self) -> AggregatorHttpClientError {
        AggregatorHttpClientError::from_response(self.response).await
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct QueryLogFields {
    kv: BTreeSet<(&'static str, String)>,
}

impl<const N: usize> From<[(&'static str, String); N]> for QueryLogFields {
    fn from(value: [(&'static str, String); N]) -> Self {
        Self {
            kv: BTreeSet::from(value),
        }
    }
}

impl slog::KV for QueryLogFields {
    fn serialize(&self, _record: &Record, serializer: &mut dyn Serializer) -> slog::Result {
        for (k, v) in &self.kv {
            serializer.emit_arguments(k, &format_args!("{v}"))?;
        }
        Ok(())
    }
}

/// Extension trait for [reqwest::Response] to reduce boilerplate with our library.
#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
pub trait ResponseExt {
    /// Try to deserialize the response body as JSON, wrapping the error in
    /// [AggregatorHttpClientError::JsonParseFailed].
    async fn parse_json<T: DeserializeOwned>(self) -> AggregatorHttpClientResult<T>;

    /// Try to deserialize the response body as JSON, wrapping a successful result in `Some` and
    /// an error in [AggregatorHttpClientError::JsonParseFailed].
    async fn parse_json_option<T: DeserializeOwned>(self) -> AggregatorHttpClientResult<Option<T>>;
}

#[cfg_attr(target_family = "wasm", async_trait::async_trait(?Send))]
#[cfg_attr(not(target_family = "wasm"), async_trait::async_trait)]
impl ResponseExt for Response {
    async fn parse_json<T: DeserializeOwned>(self) -> AggregatorHttpClientResult<T> {
        let json = self
            .json()
            .await
            .map_err(|err| AggregatorHttpClientError::JsonParseFailed(anyhow!(err)))?;
        Ok(json)
    }

    async fn parse_json_option<T: DeserializeOwned>(self) -> AggregatorHttpClientResult<Option<T>> {
        self.parse_json().await.map(Some)
    }
}
