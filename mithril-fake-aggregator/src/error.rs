use axum::{
    body::Body,
    http::{Response, StatusCode},
    response::IntoResponse,
};
use tracing::{debug, error};

/// error that wraps `anyhow::Error`.
#[derive(Debug)]
pub enum AppError {
    /// Catching anyhow errors
    Internal(anyhow::Error),

    /// Resource not found (specify what)
    NotFound(String),
}

/// Tell axum how to convert `AppError` into a response.
impl IntoResponse for AppError {
    fn into_response(self) -> Response<Body> {
        match self {
            Self::Internal(e) => {
                error!("{}", e);

                (StatusCode::INTERNAL_SERVER_ERROR, format!("Error: {:?}", e)).into_response()
            }
            Self::NotFound(resource) => {
                debug!("{resource} NOT FOUND.");

                (
                    StatusCode::NOT_FOUND,
                    format!("resource '{resource}' not found"),
                )
                    .into_response()
            }
        }
    }
}

/// This enables using `?` on functions that return `Result<_, anyhow::Error>` to turn them into
/// `Result<_, AppError>`. That way you don't need to do that manually.
impl<E> From<E> for AppError
where
    E: Into<anyhow::Error>,
{
    fn from(err: E) -> Self {
        Self::Internal(err.into())
    }
}
