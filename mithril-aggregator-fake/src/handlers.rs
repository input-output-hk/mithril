//! HTTP handlers module
//! This module contains the controllers for the different routes and middlewares.

use axum::{
    body::Body,
    extract::{Path, Request, State},
    http::{HeaderValue, Response, StatusCode},
    middleware::Next,
    response::IntoResponse,
};

use crate::shared_state::SharedState;
use crate::AppError;

/// HTTP: Return the Epoch Settings.
pub async fn epoch_settings(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let epoch_settings = app_state.get_epoch_settings().await?;

    Ok(epoch_settings)
}

/// HTTP: Return a snapshot identified by its digest.
pub async fn snapshot(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_snapshot(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("snapshot digest={key}")))
}

/// HTTP: return the list of snapshots.
pub async fn snapshots(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let snapshots = app_state.get_snapshots().await?;

    Ok(snapshots)
}

/// HTTP: return the list of mithril stake distributions.
pub async fn msds(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let msds = app_state.get_msds().await?;

    Ok(msds)
}

/// HTTP: return a mithril stake distribution identified by its hash.
pub async fn msd(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_msd(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("mithril stake distribution epoch={key}")))
}

/// HTTP: return the list of certificates
pub async fn certificates(State(state): State<SharedState>) -> Result<String, AppError> {
    let app_state = state.read().await;
    let certificates = app_state.get_certificates().await?;

    Ok(certificates)
}

/// HTTP: return a certificate identified by its hash.
pub async fn certificate(
    Path(key): Path<String>,
    State(state): State<SharedState>,
) -> Result<Response<Body>, AppError> {
    let app_state = state.read().await;

    app_state
        .get_certificate(&key)
        .await?
        .map(|s| s.into_response())
        .ok_or_else(|| AppError::NotFound(format!("certificate hash={key}")))
}

/// HTTP: return OK when the client registers download statistics
pub async fn statistics() -> Result<Response<Body>, AppError> {
    let response = Response::builder().status(StatusCode::CREATED);

    response.body(String::new().into()).map_err(|e| e.into())
}

/// MIDDLEWARE: set JSON application type in HTTP headers
pub async fn set_json_app_header(
    req: Request,
    next: Next,
) -> Result<impl IntoResponse, (StatusCode, String)> {
    let mut res = next.run(req).await;

    if res.status() == StatusCode::OK {
        let headers = res.headers_mut();
        headers.insert(
            "Content-Type",
            HeaderValue::from_static("application/json; charset=utf-8"),
        );
    }

    Ok(res)
}

#[cfg(test)]
mod tests {
    use crate::shared_state::AppState;

    pub use super::*;

    #[tokio::test]
    async fn invalid_snapshot_digest() {
        let state: State<SharedState> = State(AppState::default().into());
        let digest = Path("whatever".to_string());

        let error = snapshot(digest, state).await.expect_err(
            "The handler was expected to fail since the snapshot's digest does not exist.",
        );

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_snapshot_digest() {
        let state: State<SharedState> = State(AppState::default().into());
        let digest =
            Path("b65e89b4b504d71cdb035960d7300449f8d8602dc80dd805bf222a6100d66dbd".to_string());

        let response = snapshot(digest, state)
            .await
            .expect("The handler was expected to succeed since the snapshot's digest does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_msd_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = msd(hash, state)
            .await
            .expect_err("The handler was expected to fail since the msd's hash does not exist.");

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_certificate_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash =
            Path("74ad5da3825aea1c9c2323a42cdcb4abebeee0424fec41885973f57f6520a164".to_string());

        let response = certificate(hash, state)
            .await
            .expect("The handler was expected to succeed since the certificate's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }

    #[tokio::test]
    async fn invalid_certificate_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash = Path("whatever".to_string());

        let error = certificate(hash, state).await.expect_err(
            "The handler was expected to fail since the certificate's hash does not exist.",
        );

        assert!(matches!(error, AppError::NotFound(_)));
    }

    #[tokio::test]
    async fn existing_msd_hash() {
        let state: State<SharedState> = State(AppState::default().into());
        let hash =
            Path("11b6f0165d431ba6a9906c8f8ffab317e10104a06c986517165fc7766cc22dbe".to_string());

        let response = msd(hash, state)
            .await
            .expect("The handler was expected to succeed since the msd's hash does exist.");

        assert_eq!(StatusCode::OK, response.status());
    }
}
