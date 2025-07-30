use reqwest::Response;
use serde::de::DeserializeOwned;
use slog::Logger;

use crate::AggregatorClientResult;
use crate::error::AggregatorClientError;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum QueryMethod {
    Get,
    Post,
}

// Todo: wasm compatibility
#[async_trait::async_trait]
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
    ) -> AggregatorClientResult<Self::Response>;
}

pub struct QueryContext {
    pub(crate) response: Response,
    pub(crate) logger: Logger,
}

impl QueryContext {
    pub async fn unhandled_status_code(self) -> AggregatorClientError {
        AggregatorClientError::from_response(self.response).await
    }
}
