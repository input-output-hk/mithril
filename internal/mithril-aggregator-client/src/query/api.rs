use reqwest::Response;
use serde::de::DeserializeOwned;
use slog::Logger;

use crate::AggregatorHttpClientResult;
use crate::error::AggregatorHttpClientError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryMethod {
    Get,
    Post,
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
