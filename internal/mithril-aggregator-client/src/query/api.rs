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
